// Documentation interaction only; independent of R and project state.
import { APP_VERSION } from './version.mjs';
export function initializeHelpUI() {
  const el = id => document.getElementById(id);
  el('about-version').textContent = `Version ${APP_VERSION}`;
  const dialog = el('help-dialog');
  const topics = ['start', 'terms'];
  let opener = 'help-open';
  const tabs = topics.map(topic => el(`help-${topic}-tab`));
  function select(index, focus = false) {
    topics.forEach((topic, i) => {
      const active = i === index;
      tabs[i].setAttribute('aria-selected', String(active));
      tabs[i].tabIndex = active ? 0 : -1;
      tabs[i].classList.toggle('active', active);
      el(`help-${topic}`).hidden = !active;
    });
    if (focus) tabs[index].focus();
  }
  tabs.forEach((tab, index) => {
    tab.addEventListener('click', () => select(index));
    tab.addEventListener('keydown', event => {
      const next = {ArrowRight: (index + 1) % tabs.length,
        ArrowLeft: (index + tabs.length - 1) % tabs.length, Home: 0, End: tabs.length - 1}[event.key];
      if (next === undefined) return;
      event.preventDefault();
      select(next, true);
    });
  });
  function open(about) {
    opener = about ? 'about-open' : 'help-open';
    el('help-title').textContent = about ? 'About Cost & GHG Manager' : 'Help';
    el('help-tabs').hidden = about;
    select(0);
    if (about) topics.forEach(topic => { el(`help-${topic}`).hidden = true; });
    el('help-about').hidden = !about;
    dialog.showModal();
  }
  el('help-open').addEventListener('click', () => open(false));
  el('about-open').addEventListener('click', () => open(true));
  el('help-close').addEventListener('click', () => dialog.close());
  dialog.addEventListener('close', () => el(opener).focus());
}
