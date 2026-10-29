import { initializeSimulationUI } from './simulation-ui.mjs';
import { initializeVarianceUI } from './variance-ui.mjs';
import { initializeHelpUI } from './help-ui.mjs';
import { initializeDashboardUI } from './dashboard-ui.mjs';
import { initializeAccountingUI } from './accounting-ui.mjs';
import { visualLayouts } from './graph-viewer.mjs';
import { initializeProductionUI } from './production-ui.mjs';
import { createRJsonBridge } from './r-bridge.mjs';
import { initializeMasterTabs } from './master-ui.mjs';
import { initializeStructureUI } from './structure-ui.mjs';
import { initializeProjectControls } from './project-ui.mjs';
import { initializeCsvControls } from './csv-ui.mjs';

initializeHelpUI();
const status = document.getElementById('status');
const errorOutput = document.getElementById('error');
let stage = 'loading the webR runtime';
let webR;
let callJson;
let editingID = null; // UI selection only; row contents are fetched from R.
let creating = false;
let csvUI = { isBusy: () => false, update: () => {} };
let otherMasterBusy = () => false;
let projectControls = { isBusy: () => false, markModified: () => {} };
const form = document.getElementById('material-form');
const fields = document.getElementById('material-fields');
const nameInput = document.getElementById('material-name');
const descriptionInput = document.getElementById('material-description');
const priceInput = document.getElementById('material-price');
const priceUomInput = document.getElementById('price-uom');
const ghgInput = document.getElementById('material-ghg');
const ghgUomInput = document.getElementById('ghg-uom');
const creationError = document.getElementById('creation-error');
const creationStatus = document.getElementById('creation-status');
const materialPanel = document.getElementById('material-panel');
const newMaterialButton = document.getElementById('new-material');

// Shell state is presentation only. All material and item state stays in R.
function showMaterialPanel(open) {
  if (creating) return;
  if (!open) resetEditor();
  materialPanel.hidden = !open;
  newMaterialButton.setAttribute('aria-expanded', String(open));
  if (open) nameInput.focus();
  else newMaterialButton.focus();
}
newMaterialButton.addEventListener('click', () => { if (creating || otherMasterBusy()) return; resetEditor(); showMaterialPanel(true); });
document.getElementById('close-material').addEventListener('click', () => showMaterialPanel(false));
document.getElementById('cancel-material').addEventListener('click', () => {
  if (creating) return;
  form.reset();
  creationStatus.textContent = '';
  creationError.hidden = true;
  showMaterialPanel(false);
});
materialPanel.addEventListener('keydown', (event) => {
  if (event.key === 'Escape' && !creating) showMaterialPanel(false);
});
const logo = document.getElementById('tu-logo');
logo.addEventListener('load', () => {
  logo.hidden = false;
  document.getElementById('logo-fallback').hidden = true;
});
logo.addEventListener('error', () => {
  logo.hidden = true;
  document.getElementById('logo-fallback').hidden = false;
});
logo.src = 'assets/tu-wien-logo.png';

function renderTable(materials, prefix, emptyMessage, idColumn = prefix === 'material' ? 'matID' : null) {
  if (!Array.isArray(materials.columns) || !Array.isArray(materials.rows)) {
    throw new Error('R returned an invalid material table response.');
  }
  const headings = document.getElementById(`${prefix}-columns`);
  const body = document.getElementById(`${prefix}-rows`);
  headings.replaceChildren();
  body.replaceChildren();
  for (const column of materials.columns) {
    const heading = document.createElement('th');
    heading.scope = 'col';
    heading.textContent = column;
    headings.append(heading);
  }
  if (idColumn) {
    const heading = document.createElement('th');
    heading.scope = 'col';
    heading.textContent = 'Actions';

    headings.append(heading);
  }
  for (const material of materials.rows) {
    const row = document.createElement('tr');
    for (const column of materials.columns) {
      const cell = document.createElement('td');
      cell.textContent = material[column] ?? '';
      if (column.endsWith('Description')) {
        cell.className = 'description';
        cell.title = material[column] ?? '';
      } else if (typeof material[column] === 'number') cell.className = 'numeric';
      row.append(cell);
    }
    if (idColumn) {
      const cell = document.createElement('td');
      cell.className = 'row-actions';
      for (const action of ['Edit', 'Delete']) {
        const button = document.createElement('button');
        button.textContent = action;
        button.dataset.action = action.toLowerCase();
        button.dataset.id = material[idColumn];
        if (action === 'Delete') button.className = 'danger';
        cell.append(button);
      }
      row.append(cell);
    }
    body.append(row);
  }
  if (materials.rows.length === 0) {
    const row = document.createElement('tr');
    const cell = document.createElement('td');
    cell.colSpan = materials.columns.length + (idColumn ? 1 : 0);
    cell.className = 'empty';
    cell.textContent = emptyMessage;
    row.append(cell);
    body.append(row);
  }
}

function renderMaterials(response) {
  renderTable(response, 'material', 'No materials created yet.');
  renderTable(response.items, 'item', 'No items registered yet.');
  document.getElementById('material-count').textContent = response.rows.length;
}

function populateUoms(select, values) {
  // Controlled values come from R; the UI only presents the choices.
  for (const value of values) {
    const option = document.createElement('option');
    option.value = value;
    option.textContent = value;
    select.append(option);
  }
}

form.addEventListener('submit', async (event) => {
  event.preventDefault();
  if (creating || otherMasterBusy() || !callJson) return;
  creationError.hidden = true;
  creationStatus.textContent = '';
  if (!form.reportValidity()) return;
  if (!nameInput.value.trim()) {
    creationError.textContent = 'Material name must not be empty.';
    creationError.hidden = false;
    nameInput.focus();
    return;
  }
  for (const [input, label] of [[priceInput, 'Material price qMstd'], [ghgInput, 'Material emission liability uMELstd']]) {
    if (!Number.isFinite(input.valueAsNumber) || input.valueAsNumber < 0) {
      creationError.textContent = `${label} must be a finite, non-negative number.`;
      creationError.hidden = false;
      input.focus();
      return;
    }
  }
  for (const [input, label] of [[priceUomInput, 'Price UOM uOMqM'], [ghgUomInput, 'GHG UOM uOMuMEL']]) {
    if (!input.value.trim()) {
      creationError.textContent = `${label} is required.`;
      creationError.hidden = false;
      input.focus();
      return;
    }
  }
  creating = true;
  fields.disabled = true;
  creationStatus.textContent = editingID === null ? 'Creating material...' : 'Updating material...';
  try {
    // Pass text as data through webR; never interpolate user input into R code.
    const args = [
      nameInput.value, descriptionInput.value,
      priceInput.valueAsNumber, priceUomInput.value,
      ghgInput.valueAsNumber, ghgUomInput.value,
    ];
    const isEdit = editingID !== null;
    const result = isEdit
      ? await callJson('update_material', editingID, ...args)
      : await callJson('create_material', ...args);
    projectControls.markModified();
    renderMaterials(result);
    resetEditor();
    creationStatus.textContent = isEdit ? 'Material updated.' : 'Material created.';
  } catch (error) {
    creationStatus.textContent = '';
    creationError.textContent = `Could not save material: ${error?.message ?? String(error)}`;
    creationError.hidden = false;
    console.error('Material creation failed:', error);
  } finally {
    creating = false;
    fields.disabled = false;
    nameInput.focus();
  }
});

async function startR() {
  try {
    // Pinned local runtime and package assets allow startup without internet.
    const { WebR, ChannelType } = await import('../vendor/webr/webr.mjs');
    webR = new WebR({
      baseUrl: new URL('../vendor/webr/', import.meta.url).href,
      // Works with a plain Python static server without special HTTP headers.
      channelType: ChannelType.PostMessage,
    });
    stage = 'initializing R';
    await webR.init();
    stage = 'installing jsonlite';
    const packageResponse = await fetch('vendor/packages/jsonlite.tgz');
    if (!packageResponse.ok) throw new Error('Local jsonlite package could not be loaded.');
    await webR.FS.writeFile('/tmp/jsonlite.tgz',new Uint8Array(await packageResponse.arrayBuffer()));
    await webR.evalRVoid('untar("/tmp/jsonlite.tgz",exdir=.libPaths()[1],tar="internal")');

    stage = 'loading local dplyr packages';
    const manifestResponse = await fetch('vendor/packages/dplyr-manifest.json');
    if (!manifestResponse.ok) throw new Error('Local dplyr manifest could not be loaded.');
    for (const pkg of await manifestResponse.json()) {
      const packageResponse = await fetch(`vendor/packages/${pkg.name}.tgz`);
      if (!packageResponse.ok) throw new Error(`Local R package could not be loaded: ${pkg.name}`);
      await webR.FS.writeFile(`/tmp/${pkg.name}.tgz`, new Uint8Array(await packageResponse.arrayBuffer()));
      await webR.evalRVoid(`untar("/tmp/${pkg.name}.tgz",exdir=.libPaths()[1],tar="internal")`);
    }

    stage = 'loading R/00_project_init.R';
    const response = await fetch('R/00_project_init.R', { cache: 'no-store' });
    if (!response.ok) throw new Error(`R script request failed: HTTP ${response.status}`);
    const source = await response.text();
    await webR.FS.writeFile('/home/web_user/00_project_init.R', new TextEncoder().encode(source));
    stage = 'sourcing the project initialization script';
    await webR.evalRVoid('source("/home/web_user/00_project_init.R")');
    stage = 'loading R/itemization.R';
    const itemizationResponse = await fetch('R/itemization.R', { cache: 'no-store' });
    if (!itemizationResponse.ok) throw new Error('Itemization R script request failed.');
    await webR.FS.writeFile('/home/web_user/itemization.R', new TextEncoder().encode(await itemizationResponse.text()));
    await webR.evalRVoid('source("/home/web_user/itemization.R")');
    stage = 'loading R/structure_mapping.R';
    const mappingResponse = await fetch('R/structure_mapping.R', { cache: 'no-store' });
    if (!mappingResponse.ok) throw new Error('Structure mapping R script request failed.');
    await webR.FS.writeFile('/home/web_user/structure_mapping.R', new TextEncoder().encode(await mappingResponse.text()));
    await webR.evalRVoid('source("/home/web_user/structure_mapping.R")');
    stage = 'loading R/01_master_data.R';
    const masterResponse = await fetch('R/01_master_data.R', { cache: 'no-store' });
    if (!masterResponse.ok) throw new Error(`R script request failed: HTTP ${masterResponse.status}`);
    await webR.FS.writeFile('/home/web_user/01_master_data.R', new TextEncoder().encode(await masterResponse.text()));
    await webR.evalRVoid('source("/home/web_user/01_master_data.R")');
    stage = 'loading R/01_resources.R';
    const resourceResponse = await fetch('R/01_resources.R', { cache: 'no-store' });
    if (!resourceResponse.ok) throw new Error(`R script request failed: HTTP ${resourceResponse.status}`);
    await webR.FS.writeFile('/home/web_user/01_resources.R', new TextEncoder().encode(await resourceResponse.text()));
    await webR.evalRVoid('source("/home/web_user/01_resources.R")');
    stage = 'loading R/01_structure.R';
    const structureResponse = await fetch('R/01_structure.R', { cache: 'no-store' });
    if (!structureResponse.ok) throw new Error(`R script request failed: HTTP ${structureResponse.status}`);
    await webR.FS.writeFile('/home/web_user/01_structure.R', new TextEncoder().encode(await structureResponse.text()));
    await webR.evalRVoid('source("/home/web_user/01_structure.R")');
    stage = 'loading R/03_production.R';
    const productionResponse = await fetch('R/03_production.R', { cache: 'no-store' });
    if (!productionResponse.ok) throw new Error('Production R script request failed.');
    await webR.FS.writeFile('/home/web_user/03_production.R', new TextEncoder().encode(await productionResponse.text()));
    await webR.evalRVoid('source("/home/web_user/03_production.R")');
    stage = 'loading R/04_production_persistence.R';
    const productionPersistenceResponse = await fetch('R/04_production_persistence.R', { cache: 'no-store' });
    if (!productionPersistenceResponse.ok) throw new Error('Production persistence R script request failed.');
    await webR.FS.writeFile('/home/web_user/04_production_persistence.R', new TextEncoder().encode(await productionPersistenceResponse.text()));
    await webR.evalRVoid('source("/home/web_user/04_production_persistence.R")');
    stage = 'loading R/05_activity_response.R';
    const actualResponse = await fetch('R/05_activity_response.R', { cache: 'no-store' });
    if (!actualResponse.ok) throw new Error('Activity Response R script request failed.');
    await webR.FS.writeFile('/home/web_user/05_activity_response.R', new TextEncoder().encode(await actualResponse.text()));
    await webR.evalRVoid('source("/home/web_user/05_activity_response.R")');
    stage = 'loading R/02_project.R';
    const projectResponse = await fetch('R/02_project.R', { cache: 'no-store' });
    if (!projectResponse.ok) throw new Error(`R script request failed: HTTP ${projectResponse.status}`);
    await webR.FS.writeFile('/home/web_user/02_project.R', new TextEncoder().encode(await projectResponse.text()));
    await webR.evalRVoid('source("/home/web_user/02_project.R")');
    callJson = createRJsonBridge(webR);

    stage = 'requesting MAT.TBL from R';
    const materials = await callJson('get_materials_json');
    populateUoms(priceUomInput, materials.uoms.financial);
    populateUoms(ghgUomInput, materials.uoms.ghg);
    renderMaterials(materials);
    const activityCategories = await callJson('get_activity_categories_json');
    for (const file of ['06_standard_core.R','07_standard_accounting.R','08_actual_core.R','09_actual_accounting.R','10_accounting_state.R','11_variance_core.R','12_variance.R','13_reports.R','14_dashboard.R','15_csv.R','16_node_variance.R','17_comparison_reports.R']) {
      stage = `loading R/${file}`;
      const response = await fetch(`R/${file}`, {cache:'no-store'});
      if(!response.ok) throw new Error(`Cannot load ${file}`);
      await webR.FS.writeFile(`/home/web_user/${file}`,new TextEncoder().encode(await response.text()));
      await webR.evalRVoid(`source("/home/web_user/${file}")`);
    }
    stage = 'initializing application controls';
    const resourceFields = await callJson('get_resource_fields_json');
    let structureUI = { isBusy: () => false };
    let dashboardUI = {refresh:async()=>{}};
    let accountingUI = {refresh:async()=>{}};
    let simulationUI = {refresh:async()=>{}};
    let varianceUI = {refresh:async()=>{}};
    let productionUI = { isBusy: () => false };
    stage = "initializing JavaScript controls (master-ui.mjs)";
    const masterUI = initializeMasterTabs({ callJson, renderTable, activityCategories, resourceFields,
      onCsvTarget: tables => csvUI.update('master', tables),
      materialBusy: () => csvUI.isBusy() || creating || projectControls.isBusy() || structureUI.isBusy() || productionUI.isBusy(),
      closeMaterial: () => showMaterialPanel(false), onChanged: () => projectControls.markModified() });
    stage = "initializing JavaScript controls (structure-ui.mjs)";
    structureUI = initializeStructureUI({ callJson, renderTable,
      onCsvTarget: tables => csvUI.update('structure', tables),
      busy: () => csvUI.isBusy() || creating || masterUI.isBusy() || projectControls.isBusy() || productionUI.isBusy(),
      onChanged: () => projectControls.markModified(),
      refreshMasters: async () => { renderMaterials(await callJson('get_materials_json')); await masterUI.refresh(); },
      closeEditors: () => showMaterialPanel(false) });
    stage = "initializing JavaScript controls (production-ui.mjs)";
    productionUI = initializeProductionUI({ callJson,
      busy: () => csvUI.isBusy() || creating || masterUI.isBusy() || projectControls.isBusy() || structureUI.isBusy(),
      onChanged: () => projectControls.markModified(),
      leave: () => structureUI.deactivate() });
    otherMasterBusy = () => csvUI.isBusy() || masterUI.isBusy() || projectControls.isBusy() || structureUI.isBusy() || productionUI.isBusy();
    const refreshProject = async (showAllRelations = false) => {
        resetEditor();
        showMaterialPanel(false);
        renderMaterials(await callJson('get_materials_json'));
        await masterUI.refresh();
        await structureUI.refresh(showAllRelations);
        await productionUI.refresh();
        await accountingUI.refresh();
        await varianceUI.refresh();
        await simulationUI.refresh();
        await dashboardUI.refresh();
      };
    stage = "initializing JavaScript controls (project-ui.mjs)";
    projectControls = initializeProjectControls({ callJson, layouts: visualLayouts,
      busy: () => csvUI.isBusy() || creating || masterUI.isBusy() || structureUI.isBusy() || productionUI.isBusy(),
      refresh: refreshProject });
    stage = "initializing JavaScript controls (csv-ui.mjs)";
    csvUI = initializeCsvControls({ callJson, refresh: () => refreshProject(true),
      busy: () => creating || masterUI.isBusy() || projectControls.isBusy() || structureUI.isBusy() || productionUI.isBusy(),
      onChanged: () => projectControls.markModified() });
    stage = "initializing JavaScript controls (accounting-ui.mjs)";
    accountingUI = initializeAccountingUI({callJson,onChanged:()=>projectControls.markModified(), busy:()=>otherMasterBusy()||creating,
      leave:()=>structureUI.deactivate()});
    stage = "initializing JavaScript controls (variance-ui.mjs)";
    varianceUI = initializeVarianceUI({callJson,onChanged:()=>projectControls.markModified(),busy:()=>otherMasterBusy()||creating,leave:()=>structureUI.deactivate()});
    stage = "initializing JavaScript controls (simulation-ui.mjs)";
    simulationUI = initializeSimulationUI({enabled:false,callJson,busy:()=>otherMasterBusy()||creating,leave:()=>structureUI.deactivate()});
    stage = "initializing JavaScript controls (dashboard-ui.mjs)";
    dashboardUI = initializeDashboardUI({callJson,busy:()=>otherMasterBusy()||creating,leave:()=>structureUI.deactivate(),openAccounting:order=>accountingUI.open(order),openVariance:order=>varianceUI.open(order)});
    stage = 'opening the initial Dashboard';
    await dashboardUI.open();
    visualLayouts.onChanged(() => projectControls.markModified());
    document.getElementById('materials').hidden = false;
    fields.disabled = false;
    status.textContent = 'R ready';
    newMaterialButton.disabled = false;
    status.dataset.state = 'ready';
  } catch (error) {
    const uiFailure = stage.includes('JavaScript') || stage === 'opening the initial Dashboard';
    status.textContent = uiFailure ? 'Application UI initialization failed' : 'R initialization failed';
    status.dataset.state = 'error';
    errorOutput.textContent = `Failed while ${stage}.\n${error?.stack ?? error?.message ?? String(error)}`;
    errorOutput.hidden = false;
    console.error(`Startup failed while ${stage}:`, error);
  }
}

startR();

// A reusable form binding: inputs are drafts, never an authoritative row cache.
function resetEditor() {
  editingID = null;
  form.reset();
  document.getElementById('material-panel-title').textContent = 'New Material';
  document.getElementById('save-material').textContent = 'Create Material';
  document.getElementById('cancel-material').textContent = 'Cancel';
  creationError.hidden = true;
  creationStatus.textContent = '';
}

document.getElementById('material-rows').addEventListener('click', async (event) => {
  const button = event.target.closest('button[data-action]');
  if (!button || creating || otherMasterBusy()) return;
  const id = Number(button.dataset.id);
  showMaterialPanel(true);
  creating = true;
  fields.disabled = true;
  creationError.hidden = true;
  creationStatus.textContent = '';
  try {
    // Read the latest row from R for both editing and confirmation text.
    const [material] = await callJson('get_material_json', id);
    if (button.dataset.action === 'edit') {
      resetEditor();
      editingID = material.matID;
      for (const input of form.elements) {
        if (input.name && Object.hasOwn(material, input.name)) input.value = material[input.name];
      }
      document.getElementById('material-panel-title').textContent = `Edit Material ? ${material.matID}`;
      document.getElementById('save-material').textContent = 'Update Material';
      document.getElementById('cancel-material').textContent = 'Cancel edit';
    } else {
      if (!window.confirm(`Delete material "${material.matName}" (matID ${material.matID})? Its item registry entry will also be removed. This cannot be undone.`)) return;
      renderMaterials(await callJson('delete_material', id));
      projectControls.markModified();
      if (editingID === id) resetEditor();
      creationStatus.textContent = 'Material deleted.';
    }
  } catch (error) {
    creationError.textContent = `Material operation failed: ${error?.message ?? String(error)}`;
    creationError.hidden = false;
  } finally {
    creating = false;
    fields.disabled = false;
    nameInput.focus();
  }
});
