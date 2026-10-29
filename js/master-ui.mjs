// UI labels and form bindings only. All data, validation, IDs and mutations are R-owned.
const entities = {
  equipment: { title: 'Equipment', singular: 'Equipment', id: 'eqipID', name: 'eqipName', description: 'eqipDescription', table: 'EQIP.TBL', list: 'get_equipments_json' },
  energy: { title: 'Energy', singular: 'Energy', id: 'enyID', name: 'enyName', description: 'enyDescription', table: 'ENY.TBL', list: 'get_energies_json' },
  wip: { title: 'WIP', singular: 'WIP', id: 'wipID', name: 'wipName', description: 'wipDescription', table: 'WIP.ECSI', list: 'get_wips_json' },
  fp: { title: 'Finished Products', singular: 'Finished Product', id: 'fpID', name: 'fpName', description: 'fpDescription', table: 'FP.ECSI', list: 'get_fps_json' },
  activity: { title: 'Activities', singular: 'Activity', id: 'actyID', name: 'actyName', description: 'actyDescription', table: 'ACTY.ECSI', list: 'get_activities_json' },
};

// Presentation labels only; resource field names and R validation remain unchanged.
const resourceLabels = {
  eqipCat: 'Equipment category (eqipCat)',
  ltCapacity: 'Lifetime capacity (ltCapacity)',
  pEstd: 'Full cost rate (pEstd)', uOMpE: 'UOM (uOMpE)',
  qEstd: 'Variable cost rate (qEstd)', uOMqE: 'UOM (uOMqE)',
  uEELstd: 'E-Liability (uEELstd)', uOMuEEL: 'UOM (uOMuEEL)',
  emEstd: 'Emission coefficient (emEstd)', uOMemE: 'UOM (uOMemE)',
};

export function initializeMasterTabs({ callJson, renderTable, materialBusy, closeMaterial, onChanged, activityCategories, resourceFields, onCsvTarget = () => {} }) {
  const el = id => document.getElementById(id);
  const tabs = [...document.querySelectorAll('[data-master-tab]')];
  const workspace = el('other-masters');
  const panel = el('master-panel');
  const form = el('master-form');
  const fields = el('master-fields');
  for (const { value, label } of activityCategories) {
    const option = document.createElement('option');
    option.value = value;
    option.textContent = label;
    el('master-category').append(option);
  }
  let selected = 'material';
  let editingID = null;
  let busy = false;
  function resourceInputs() { return [...el('resource-fields').querySelectorAll('input, select')]; }
  function configureResourceFields(kind) {
    const container = el('resource-fields');
    container.replaceChildren();
    container.hidden = !resourceFields[kind];
    for (const field of resourceFields[kind] ?? []) {
      const label = document.createElement('label');
      const input = document.createElement(field.options ? 'select' : 'input');
      input.id = `resource-${field.name}`;
      input.name = field.name;
      input.required = true;
      label.htmlFor = input.id;
      label.textContent = resourceLabels[field.name] ?? field.label;
      if (field.options) {
        for (const value of ['', ...[].concat(field.options)]) {
          const option = document.createElement('option');
          option.value = value; option.textContent = value || 'Select value'; input.append(option);
        }
      } else { input.type = field.type; input.step = 'any'; }
      container.append(label, input);
    }
  }

  function showPanel(open) {
    panel.hidden = !open;
    el('new-master').setAttribute('aria-expanded', String(open));
    if (open) el('master-name').focus();
    else el('new-master').focus();
  }
  function resetEditor() {
    editingID = null;
    form.reset();
    el('master-error').hidden = true;
    el('master-status').textContent = '';
    el('master-panel-title').textContent = `New ${entities[selected].singular}`;
    el('save-master').textContent = `Create ${entities[selected].singular}`;
  }
  function render(response) {
    renderTable(response, 'master', 'No records created yet.', entities[selected].id);
    renderTable(response.items, 'item', 'No items registered yet.');
    el('master-count').textContent = response.rows.length;
  }
  async function run(action) {
    if (busy || materialBusy()) return;
    busy = true;
    el('error').hidden = true;
    fields.disabled = true;
    el('master-error').hidden = true;
    try { await action(); }
    catch (error) {
      el('master-status').textContent = '';
      el('master-error').textContent = `Operation failed: ${error?.message ?? String(error)}`;
      el('master-error').hidden = false;
      if (selected !== 'material' && selected !== 'items') showPanel(true);
      else {
        el('error').textContent = el('master-error').textContent;
        el('error').hidden = false;
      }
    } finally { busy = false; fields.disabled = false; }
  }
  tabs.forEach(tab => {
    tab.disabled = false;
    tab.addEventListener('click', () => run(async () => {
      const next = tab.dataset.masterTab;
      if (next === selected) return;
      // Fetch before changing the visible selection so a failure retains the current table.
      const response = await callJson(next === 'material' ? 'get_materials_json' : next === 'items' ? 'get_item_registry_json' : entities[next].list);
      closeMaterial();
      selected = next;
      onCsvTarget({ wip: ['WIP.ECSI'], fp: ['FP.ECSI'], activity: ['ACTY.ECSI'], equipment: ['EQIP.TBL'], energy: ['ENY.TBL'] }[next] ?? []);
      panel.hidden = true;
      el('new-master').setAttribute('aria-expanded', 'false');
      tabs.forEach(button => {
        const active = button === tab;
        button.classList.toggle('active', active);
        if (active) button.setAttribute('aria-current', 'page');
        else button.removeAttribute('aria-current');
      });
      el('materials').hidden = next !== 'material';
      workspace.hidden = next === 'material' || next === 'items';
      el('items-workspace').hidden = next !== 'items';
      if (next === 'material') {
        renderTable(response, 'material', 'No materials created yet.');
        renderTable(response.items, 'item', 'No items registered yet.');
        el('material-count').textContent = response.rows.length;
      } else if (next === 'items') {
        renderTable(response, 'item', 'No items registered yet.');
      } else {
        const config = entities[next];
        el('master-title').textContent = config.title;
        el('master-table-name').textContent = config.table;
        el('master-caption').textContent = config.table;
        el('new-master').textContent = `+ New ${config.singular}`;
        el('master-category-field').hidden = next !== 'activity';
        el('master-category').disabled = next !== 'activity';
        configureResourceFields(next);
        resetEditor();
        render(response);
      }
    }));
  });
  el('new-master').addEventListener('click', () => {
    if (busy) return;
    resetEditor(); showPanel(true);
  });
  for (const id of ['close-master', 'cancel-master']) el(id).addEventListener('click', () => {
    if (busy) return;
    resetEditor(); showPanel(false);
  });
  panel.addEventListener('keydown', event => {
    if (event.key === 'Escape' && !busy) { resetEditor(); showPanel(false); }
  });
  form.addEventListener('submit', event => {
    event.preventDefault();
    if (!form.reportValidity()) return;
    run(async () => {
      const args = [el('master-name').value, el('master-description').value];
      if (selected === 'activity') args.push(el('master-category').value);
      for (const input of resourceInputs()) args.push(input.type === 'number' ? input.valueAsNumber : input.value);
      const isEdit = editingID !== null;
      const response = isEdit
        ? await callJson(`update_${selected}`, editingID, ...args)
        : await callJson(`create_${selected}`, ...args);
      onChanged();
      render(response);
      resetEditor();
      el('master-status').textContent = isEdit ? 'Record updated.' : 'Record created.';
    });
  });
  el('master-rows').addEventListener('click', event => {
    const button = event.target.closest('button[data-action]');
    if (!button) return;
    run(async () => {
      const config = entities[selected];
      const [row] = await callJson(`get_${selected}_json`, Number(button.dataset.id));
      if (button.dataset.action === 'edit') {
        resetEditor();
        editingID = row[config.id];
        el('master-name').value = row[config.name];
        el('master-description').value = row[config.description];
        if (selected === 'activity') el('master-category').value = row.actyCat;
        for (const input of resourceInputs()) input.value = row[input.name];
        el('master-panel-title').textContent = `Edit ${config.singular} · ${editingID}`;
        el('save-master').textContent = `Update ${config.singular}`;
        showPanel(true);
      } else if (window.confirm(`Delete ${config.singular} "${row[config.name]}"? This cannot be undone.`)) {
        render(await callJson(`delete_${selected}`, row[config.id]));
        onChanged();
        resetEditor();
        showPanel(false);
      }
    });
  });
  return {
    isBusy: () => busy,
    refresh: async () => {
      panel.hidden = true;
      el('new-master').setAttribute('aria-expanded', 'false');
      if (selected === 'items') {
        renderTable(await callJson('get_item_registry_json'), 'item', 'No items registered yet.');
      } else if (selected !== 'material') {
        resetEditor();
        render(await callJson(entities[selected].list));
      }
    },
  };
}
