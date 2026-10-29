import { drawGraph, initializeExpandedGraph } from './graph-viewer.mjs';
// Presentation only: R supplies relations, choices, graph nodes/edges and positions.
export function initializeStructureUI({ callJson, renderTable, busy, onChanged, refreshMasters, closeEditors, onCsvTarget = () => {} }) {
  const el = id => document.getElementById(id);
  let kind = 'bom', view = 'by', editing = null, working = false, active = false;
  const parent = () => Number(el('structure-parent').value || 0);
  const number = id => el(id).valueAsNumber ?? Number(el(id).value);
  function options(id, rows, selected = '') {
    const select = el(id); select.replaceChildren();
    const placeholder = document.createElement('option'); placeholder.value = id === 'structure-parent' ? '0' : ''; placeholder.textContent = 'Select…'; select.append(placeholder);
    for (const row of rows) { const option = document.createElement('option'); option.value = row.value; option.textContent = row.label; select.append(option); }
    select.value = String(selected);
    if (select.selectedIndex < 0) select.selectedIndex = 0;
  }
  const unitOptions = values => [].concat(values).map(value => ({ value, label: value }));
  const draw = graph => drawGraph(el('structure-svg'),graph,'Production Graph',
    graph.context > 0 ? `${kind}:fp:${graph.context}` : `${kind}:item:${parent()}`);
  async function reload() {
    let data;
    const tableKind = kind === 'production' ? 'bom' : kind;
    try { data = await callJson('get_structure_json', tableKind, parent(), view); }
    catch (error) { if (!parent()) throw error; options('structure-parent', [], 0); data = await callJson('get_structure_json', tableKind, 0, view); }
    const selected = parent(); options('structure-parent', data.parents, selected);
    const context = Number(el('graph-context').value || 0);
    options('graph-context', [{value:0,label:kind !== 'bom' ? 'Select finished product' : 'All configurations'},...data.contexts], context);
    el('structure-parent').parentElement.hidden = kind === 'production' || (kind === 'route' && view === 'graph');
    const graphical = view === 'graph' || kind === 'production';
    renderTable(data, 'structure', view === 'by' && !parent() ? 'Select a produced item.' : 'No relations yet.', kind === 'bom' ? 'bomID' : 'routeID');
    el('structure-table').hidden = graphical; el('structure-svg').hidden = !graphical;
    el('structure-expand').hidden = !graphical; el('graph-context-controls').hidden = !graphical;
    el('graph-details').textContent = '';
    if (graphical) {
      const graph = await callJson('get_definition_graph_json',kind,parent(),Number(el('graph-context').value || 0));
      draw(graph);
      if (graph.context > 0) el('graph-context').value = String(graph.context);
    }
    for (const mode of ['by','all','graph']) el(`structure-${mode}`).classList.toggle('active', mode === view);
    return data;
  }
  async function run(action) {
    if (working || busy()) return;
    working = true; el('structure-fields').disabled = true; el('structure-error').hidden = true;
    el('structure-workspace').inert = true; el('master-workspace').inert = true;
    try { await action(); } catch (error) { el('structure-error').textContent = error.message; el('structure-error').hidden = false; }
    finally { working = false; el('structure-fields').disabled = false; el('structure-workspace').inert = false; el('master-workspace').inert = false; }
  }
  function close() { editing = null; el('structure-panel').hidden = true; }
  async function editor(row = null) {
    const selected = row ? (kind === 'bom' ? row.parentItemID : row.itemID) : parent();
    const data = await callJson('get_structure_json', kind, selected, 'all');
    el('structure-form').reset(); editing = row ? row[kind === 'bom' ? 'bomID' : 'routeID'] : null;
    options('design-parent', data.parents, selected);
    options('design-scope', [{value:0,label:'Shared configuration'},...data.contexts], row?.fpID ?? 0);
    options('design-child', data.children, row?.childItemID);
    for (const [id,rows,value] of [['design-activity',data.activities,row?.actyID],['design-equipment',data.equipment,row?.eqipID],['design-energy',data.energy,row?.enyID]]) options(id,rows,value);
    options('design-material-unit', unitOptions(data.units.matUOM), row?.matUOM);
    options('design-time-unit', unitOptions(data.units.eqipUOM), row?.eqipUOM ?? 'min/unit');
    options('design-power-unit', unitOptions(data.units.powUOM), row?.powUOM ?? 'kW');
    el('design-quantity').value = row?.rMstd ?? ''; el('design-time').value = row?.rEstd ?? ''; el('design-power').value = row?.powEstd ?? '';
    el('bom-fields').hidden = el('bom-fields').disabled = kind !== 'bom'; el('route-fields').hidden = el('route-fields').disabled = kind === 'bom';
    el('structure-save').textContent = editing === null ? 'Create' : 'Update';
    el('structure-editor-title').textContent = `${editing === null ? 'New' : 'Edit'} ${kind === 'bom' ? 'Component' : 'Route'}`;
    el('structure-panel').hidden = false; el('design-parent').focus();
  }
  el('nav-structure').disabled = false;
  for (const [id,isStructure] of [['nav-master',false],['nav-structure',true]]) el(id).addEventListener('click', () => run(async () => {
    if (isStructure) await reload(); else await refreshMasters();
    el('production-workspace').hidden = true;
    el('actual-workspace').hidden = true;
    el('nav-actual').classList.toggle('active',false); el('nav-actual').removeAttribute('aria-current');
    el('nav-production').classList.toggle('active',false); el('nav-production').removeAttribute('aria-current');
    closeEditors(); close(); active = isStructure;
    el('master-workspace').hidden = active; el('structure-workspace').hidden = !active;
    for (const [button,on] of [['nav-master',!active],['nav-structure',active]]) { el(button).classList.toggle('active',on); if(on) el(button).setAttribute('aria-current','page'); else el(button).removeAttribute('aria-current'); }
  }));
  document.querySelectorAll('[data-structure-kind]').forEach(button => button.addEventListener('click', () => run(async () => {
    kind = button.dataset.structureKind; close();
    onCsvTarget(kind === 'production' ? [] : [kind === 'bom' ? 'BOM.ECSI' : 'Routing.ECSI']);
    view = kind === 'production' ? 'graph' : 'by';
    document.querySelectorAll('[data-structure-kind]').forEach(b => b.classList.toggle('active', b === button));
    el('structure-new').textContent = kind === 'bom' ? '+ Add Component' : '+ Add Route';
    el('structure-all').textContent = kind === 'bom' ? 'All Relations' : 'All Routes';
    el('structure-graph').textContent = kind === 'bom' ? 'Graph' : 'Process Flow';
    el('structure-caption').textContent = kind === 'bom' ? 'BOM.ECSI' : 'Routing.ECSI'; await reload();
    for (const id of ['structure-new','structure-by','structure-all','structure-graph']) el(id).hidden = kind === 'production';
  })));
  for (const mode of ['by','all','graph']) el(`structure-${mode}`).addEventListener('click', () => run(async () => { view = mode; close(); await reload(); }));
  el('structure-parent').addEventListener('change', () => run(async () => { close(); el('graph-context').value = '0'; await reload(); }));
  el('structure-new').addEventListener('click', () => run(() => editor()));
  for (const id of ['structure-close','structure-cancel']) el(id).addEventListener('click', () => { if(!working) close(); });
  el('structure-panel').addEventListener('keydown', event => { if(event.key === 'Escape' && !working) close(); });
  el('design-parent').addEventListener('change', () => run(async () => { const data = await callJson('get_structure_json',kind,Number(el('design-parent').value || 0),'all'); options('design-child',data.children); }));
  el('structure-rows').addEventListener('click', event => {
    const button = event.target.closest('button[data-action]'); if(!button) return;
    run(async () => { const id = Number(button.dataset.id);
      if(button.dataset.action === 'edit') await editor((await callJson('get_structure_row_json',kind,id))[0]);
      else if(window.confirm('Delete this design relation?')) { await callJson(`delete_${kind}`,id); onChanged(); close(); await reload(); }
    });
  });
  el('structure-form').addEventListener('submit', event => { event.preventDefault(); if(!el('structure-form').reportValidity()) return;
    run(async () => { const args = kind === 'bom' ? [Number(el('design-parent').value),Number(el('design-child').value),number('design-quantity'),el('design-material-unit').value]
      : [Number(el('design-parent').value),Number(el('design-activity').value),Number(el('design-equipment').value),Number(el('design-energy').value),number('design-time'),el('design-time-unit').value,number('design-power'),el('design-power-unit').value];
      args.push(Number(el('design-scope').value || 0));
      await callJson(`${editing === null ? 'create' : 'update'}_scoped_${kind}`, ...(editing === null ? args : [editing,...args])); onChanged(); close(); await reload();
    });
  });
  el('graph-context').addEventListener('change', () => run(reload));
  const viewer = initializeExpandedGraph();
  const expanded = el('expanded-graph');
  el('structure-expand').addEventListener('click', () => {
    if (!working && !busy()) viewer.open(el('structure-svg'), 'Production Graph', el('structure-expand'));
  });
  for(const [canvas,details] of [['structure-svg','graph-details']]) {
    const inspect = event => { const node = event.target.closest('[data-details]'); if(node) el(details).textContent = node.dataset.details; };
    el(canvas).addEventListener('click', inspect);
    el(canvas).addEventListener('focusin', inspect);
  }
  return { deactivate: () => { active=false; close(); closeEditors(); }, isBusy: () => working, refresh: async (showAllRelations = false) => { close(); if(expanded.open) expanded.close(); options('structure-parent',[],0); options('graph-context',[],0); if(showAllRelations && kind !== 'production') view = 'all'; if(active) await reload(); } };
}
