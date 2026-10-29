// Browser file I/O and presentation state only. Project contents are opaque R JSON.
export function initializeProjectControls({ callJson, busy, refresh, layouts }) {
  const el = id => document.getElementById(id);
  const buttons = ['new-project', 'load-project', 'save-project'].map(el);
  const picker = el('project-file');
  let messageTimer;
  function message(text, state = 'success') {
    clearTimeout(messageTimer);
    el('project-message').textContent = state === 'error' ? '' : text;
    el('project-error').textContent = state === 'error' ? text : '';
    if (text && state !== 'error') {
      messageTimer = setTimeout(() => { el('project-message').textContent = ''; }, 6000);
      messageTimer?.unref?.();
    }
  }
  let working = false;
  let hasSaved = false;
  function indicator(state) { el('project-state').textContent = state; }
  function markModified() { indicator(hasSaved ? 'Modified' : 'Unsaved'); }
  async function run(action) {
    if (working || busy()) return;
    working = true;
    buttons.forEach(button => { button.disabled = true; });
    document.querySelector('main').inert = true;
    message('');
    try { await action(); }
    catch (error) { message(`Project operation failed: ${error?.message ?? String(error)}`, 'error'); }
    finally {
      working = false;
      buttons.forEach(button => { button.disabled = false; });
      document.querySelector('main').inert = false;
    }
  }
  buttons.forEach(button => { button.disabled = false; });
  el('save-project').addEventListener('click', () => run(async () => {
    const current = await callJson('get_project_status_json');
    const requestedName = window.prompt('Project name', current.name === 'Untitled project' ? 'cost-ghg-project' : current.name);
    if (requestedName === null) return;
    const { contents, name } = layouts
      ? await callJson('export_visual_project_json', requestedName, JSON.stringify(layouts.snapshot()))
      : await callJson('export_named_project_json', requestedName);
    const href = URL.createObjectURL(new Blob([contents], { type: 'application/json' }));
    const link = document.createElement('a');
    link.href = href;
    // Filename safety is browser file I/O; the project metadata comes from R.
    let filename = name.replace(/[<>:"/\\|?*\u0000-\u001f]/g, '_').replace(/[. ]+$/, '').replace(/\.json$/i, '');
    if (!filename) filename = 'cost-ghg-project';
    if (/^(con|prn|aux|nul|com[1-9]|lpt[1-9])(?:\.|$)/i.test(filename)) filename = `_${filename}`;
    link.download = `${filename}.json`;
    document.body.append(link);
    try { link.click(); } finally { link.remove(); setTimeout(() => URL.revokeObjectURL(href), 60000); }
    hasSaved = true;
    el('project-name').textContent = name;
    indicator('Exported');
    message('Download requested. Keep the project file to load it later.');
  }));
  el('load-project').addEventListener('click', () => {
    if (working || busy()) return;
    picker.value = '';
    picker.click();
  });
  picker.addEventListener('change', () => {
    const file = picker.files[0];
    if (!file) return;
    run(async () => {
      const current = await callJson('get_project_status_json');
      if (current.hasData && !window.confirm('Loading replaces the current project. Save it first if you want to keep it. Continue?')) return;
      const status = await callJson('restore_project', await file.text());
      layouts?.restore(status.visualization);
      el('project-name').textContent = status.name;
      hasSaved = true;
      indicator('Loaded');
      await refresh();
      message('Project loaded.');
    });
  });
  el('new-project').addEventListener('click', () => run(async () => {
    const current = await callJson('get_project_status_json');
    if (current.hasData && !window.confirm('Start a new empty project? Save the current project first if you want to keep it.')) return;
    const status = await callJson('reset_project');
    layouts?.restore();
    el('project-name').textContent = status.name;
    hasSaved = false;
    indicator('Unsaved');
    await refresh();
    message('New project started.');
  }));
  return { isBusy: () => working, markModified };
}
