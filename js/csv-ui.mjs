// File I/O and preview only. Parsing, types, references and commits belong to R.
export function initializeCsvControls({ callJson, busy, refresh, onChanged }) {
  const el = id => document.getElementById(id);
  const dialog = el('csv-dialog');
  let working = false, target = '', contents = '', companion = '';
  const groups = {};
  function message(text) { el('csv-message').textContent = text; }
  function invalidate() {
    el('csv-apply').disabled = true;
    el('csv-preview').replaceChildren();
  }
  async function run(action) {
    if (working || busy()) return;
    working = true;
    for (const id of ['csv-file', 'csv-companion', 'csv-validate', 'csv-apply', 'csv-close']) el(id).disabled = true;
    try { await action(); }
    catch (error) {
      invalidate(); message(error?.message ?? String(error));
      if (!dialog.open) el('project-error').textContent = `CSV export failed: ${error?.message ?? String(error)}`;
    }
    finally {
      working = false;
      for (const id of ['csv-file', 'csv-companion', 'csv-validate', 'csv-close']) el(id).disabled = false;
    }
  }
  function update(group, tables) {
    const { select, container } = groups[group];
    container.hidden = !tables.length;
    select.replaceChildren();
    for (const table of tables) {
      const option = document.createElement('option'); option.value = table; option.textContent = table; select.append(option);
    }
    select.hidden = tables.length < 2;
  }
  for (const [group, anchor, tables] of [
    ['material', 'new-material', ['MAT.TBL']], ['master', 'new-master', []],
    ['structure', 'structure-new', ['BOM.ECSI']],
  ]) {
    const container = document.createElement('span'); container.className = 'csv-controls';
    const select = document.createElement('select'); select.setAttribute('aria-label', 'CSV table'); container.append(select);
    for (const action of ['Import', 'Export']) {
      const button = document.createElement('button'); button.type = 'button'; button.textContent = `${action} CSV`; container.append(button);
      button.addEventListener('click', () => {
        if (working || busy()) return;
        const table = select.value;
        if (action === 'Import') {
          target = table; contents = ''; companion = ''; invalidate();
          el('csv-file').value = ''; el('csv-companion').value = '';
          el('csv-title').textContent = `Import CSV — ${table}`;
          el('csv-companion-field').hidden = true;
          el('csv-companion-label').textContent = '';
          const sourceHelp = table === 'BOM.ECSI'
            ? ' Product Structure Import: source ERP/manufacturing parCat + parID and childCat + childID map through ITEM.TBL to canonical BOM.ECSI item references; fpID is optional for product-specific WIP definitions.'
            : table === 'Routing.ECSI'
              ? ' Production Structure Import: source ERP/manufacturing wipFpCat (FP/WIP) + wipFpID maps through ITEM.TBL to canonical Routing.ECSI.itemID; fpID is optional for product-specific WIP definitions.' : '';
          message(`Choose a UTF-8 CSV, then validate and preview. Existing records in the selected table will be replaced.${sourceHelp}`);
          dialog.showModal(); el('csv-file').click();
        } else run(async () => {
          const file = await callJson('export_csv_json', table);
          const href = URL.createObjectURL(new Blob([file.contents], { type: 'text/csv;charset=utf-8' }));
          const link = document.createElement('a'); link.href = href; link.download = file.name; document.body.append(link);
          try { link.click(); } finally { link.remove(); setTimeout(() => URL.revokeObjectURL(href), 60000); }
        }).catch(error => { message(error.message); });
      });
    }
    el(anchor).before(container); groups[group] = { container, select }; update(group, tables);
  }
  for (const id of ['csv-file', 'csv-companion']) el(id).addEventListener('change', () => { invalidate(); message('File selection changed. Validate and preview before replacing tables.'); });
  el('csv-validate').addEventListener('click', () => run(async () => {
    invalidate();
    if (!el('csv-file').files[0]) throw new Error('Choose a CSV file.');
    // Fatal UTF-8 decoding rejects damaged input rather than replacing characters.
    const read = async file => new TextDecoder('utf-8', { fatal: true }).decode(await file.arrayBuffer());
    contents = await read(el('csv-file').files[0]);
    companion = el('csv-companion').files[0] ? await read(el('csv-companion').files[0]) : '';
    const result = await callJson('preview_csv_json', target, contents, companion);
    for (const preview of result.previews) {
      const heading = document.createElement('h4'); heading.textContent = `${preview.table}: ${preview.count} records — replace entire table`;
      const wrapper = document.createElement('div'); wrapper.className = 'table-scroll';
      const table = document.createElement('table'); const thead = document.createElement('thead'); const tr = document.createElement('tr');
      for (const column of preview.columns) { const th = document.createElement('th'); th.textContent = column; tr.append(th); }
      thead.append(tr); table.append(thead);
      const tbody = document.createElement('tbody');
      for (const row of preview.rows) {
        const tr = document.createElement('tr');
        for (const column of preview.columns) { const td = document.createElement('td'); td.textContent = row[column] ?? ''; tr.append(td); }
        tbody.append(tr);
      }
      table.append(tbody); wrapper.append(table); el('csv-preview').append(heading, wrapper);
    }
    message(`Validation passed. ${result.message}`); el('csv-apply').disabled = false;
  }));
  el('csv-apply').addEventListener('click', () => {
    if (el('csv-apply').disabled) return;
    return run(async () => {
    await callJson('import_csv_json', target, contents, companion);
    onChanged(); invalidate();
    message('CSV import applied successfully.');
    await refresh();
    dialog.close();
    });
  });
  el('csv-close').addEventListener('click', () => { if (!working) dialog.close(); });
  dialog.addEventListener('cancel', event => { if (working) event.preventDefault(); });
  return { isBusy: () => working || dialog.open, update };
}
