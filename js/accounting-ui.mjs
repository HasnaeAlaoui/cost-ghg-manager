// Bar widths are visual scaling only; all Cost/GHG values are calculated by R.
export function renderOrderComparison(host,summary) {
 host.replaceChildren();
 for(const [title,std,act,unit] of [['Order Cost','PCstd','PCact','EUR'],['Order PCF','PCFstd','PCFact','kgCO2e']]) {
  const section=document.createElement('section'),heading=document.createElement('h3');heading.textContent=title;section.append(heading);
  const scale=Math.max(summary[std],summary[act],0);
  for(const [label,key,color] of [['Standard',std,'#006e9e'],['Actual',act,'#00858f']]) {
   const value=summary[key],line=document.createElement('p');line.textContent=`${label}: ${Number(value).toLocaleString(undefined,{maximumFractionDigits:6})} ${unit}`;
   const bar=document.createElement('div');bar.style.width=`${scale>0?100*value/scale:0}%`;bar.style.height='14px';bar.style.backgroundColor=color;
   section.append(line,bar);
  }
  host.append(section);
 }
}
// Accounting transport and table rendering only; all metrics come from R.
export function summaryDisplay(data,mode) {
 const metrics=mode==='actual'?['PCfact','PCvact','PCact']:['PCfstd','PCvstd','PCstd'];
 const columns=['orderID','fpID','fpName','orderQuantity',...metrics,'costUOM',mode==='actual'?'PCFact':'PCFstd','ghgUOM'];
 return {columns:columns.filter(column=>data.columns.includes(column)),rows:data.rows};
}
export function formatCell(column,value,id) {
 if(Array.isArray(value))return JSON.stringify(value);
 if(typeof value==='number'&&Number.isFinite(value)&&id.startsWith('accounting-')) {
  if(/^(PACF|PECF|PMCF|PCF)/.test(column))return value.toLocaleString(undefined,{maximumFractionDigits:6,useGrouping:false});
  if(/^(PAC|PDC|PC)(v|f)?(std|act)(Unit)?$/.test(column))return value.toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2,useGrouping:false});
 }
 return String(value??'');
}
export function initializeAccountingUI({callJson,busy,leave,onChanged=()=>{}}) {
 const el=id=>document.getElementById(id);let working=false;
 const clear=()=>{el('accounting-results').replaceChildren();el('accounting-summary').replaceChildren();el('accounting-status').textContent='';el('accounting-mode').textContent='';el('accounting-comparison').replaceChildren();el('accounting-error').hidden=true;};
 async function run(action){if(working||busy())return;working=true;
  const surfaces=[document.querySelector('main'),document.querySelector('.sidebar'),document.querySelector('.global-header')];
  surfaces.forEach(x=>x.inert=true);
  try{await action();}catch(error){el('accounting-error').textContent=error.message;el('accounting-error').hidden=false;}
  finally{surfaces.forEach(x=>x.inert=false);working=false;}
 }
 async function loadResult(){
  clear();const data=await callJson('get_accounting_view_json',Number(el('accounting-order').value||0));
  renderTable('accounting-results',data.table);renderTable('accounting-summary',summaryDisplay(data.summary,data.mode));
  if(data.mode){el('accounting-mode').textContent=`Mode: ${data.mode==='actual'?'Actual':'Standard'}`;el('accounting-status').textContent=data.stale?'Results require recalculation.':'';}
 }
 async function loadChoices(){
  const data=await callJson('get_standard_context_json');el('accounting-order').replaceChildren();
  const blank=document.createElement('option');blank.value='';blank.textContent='Select...';el('accounting-order').append(blank);
  for(const row of data.requests){const option=document.createElement('option');option.value=row.value;option.textContent=row.label;el('accounting-order').append(option);}
  el('accounting-order').value=String(data.resultOrder||'');el('calculate-standard').disabled=!el('accounting-order').value;el('calculate-actual').disabled=!el('accounting-order').value;await loadResult();
 }
 el('nav-accounting').disabled=false;el('nav-accounting').title='Standard Accounting';
 const open=(orderID)=>run(async()=>{
  clear();el('accounting-detail-view').hidden=true;el('accounting-summary-view').hidden=false;el('accounting-detail-tab').classList.toggle('active',false);el('accounting-summary-tab').classList.toggle('active',true);await loadChoices();
  if(orderID){el('accounting-order').value=String(orderID);await loadResult();}
  leave();for(const name of ['master','structure','production','actual','variance']){el(`${name}-workspace`).hidden=true;el(`nav-${name}`).classList.toggle('active',false);el(`nav-${name}`).removeAttribute('aria-current');}
  el('accounting-workspace').hidden=false;el('nav-accounting').classList.toggle('active',true);el('nav-accounting').setAttribute('aria-current','page');
 });
 el('nav-accounting').addEventListener('click',()=>open());
 for(const name of ['master','structure','production','actual','variance'])el(`nav-${name}`).addEventListener('click',()=>{el('accounting-workspace').hidden=true;el('nav-accounting').classList.toggle('active',false);el('nav-accounting').removeAttribute('aria-current');clear();});
 for(const name of ['new-project','load-project'])el(name).addEventListener('click',()=>{clear();el('accounting-order').replaceChildren();el('calculate-standard').disabled=true;el('calculate-actual').disabled=true;});
 el('accounting-order').addEventListener('change',()=>run(async()=>{clear();el('calculate-standard').disabled=!el('accounting-order').value;el('calculate-actual').disabled=!el('accounting-order').value;await loadResult();}));
 function renderTable(id,data,labels={},selectNode=null){
  const table=document.createElement('table'),head=document.createElement('thead'),body=document.createElement('tbody'),tr=document.createElement('tr');
  for(const column of data.columns){const th=document.createElement('th');th.textContent=labels[column]??(column);tr.append(th);}head.append(tr);table.append(head);
  for(const row of data.rows){const tr=document.createElement('tr');for(const column of data.columns){const td=document.createElement('td');if(selectNode&&column==='nodeID'){const button=document.createElement('button');button.type='button';button.textContent=String(row.nodeID);button.setAttribute('aria-label',`Inspect node ${row.nodeID}: ${row.itemName}`);button.addEventListener('click',()=>selectNode(row.nodeID));td.append(button);}else td.textContent=formatCell(column,row[column],id);tr.append(td);}if(selectNode)tr.addEventListener('click',event=>{if(!event.target.closest('button'))selectNode(row.nodeID);});body.append(tr);}table.append(body);el(id).append(table);
 }
 el('compare-order').addEventListener('click',()=>run(async()=>{
  const data=await callJson('get_order_comparison_json',Number(el('accounting-order').value));
  onChanged();await loadResult();renderOrderComparison(el('accounting-comparison'),data.summary);
 }));
 for(const mode of ['standard','actual'])el(`calculate-${mode}`).addEventListener('click',()=>run(async()=>{
  clear();await callJson(mode==='actual'?'calculate_actual_json':'calculate_standard_view_json',Number(el('accounting-order').value));
  onChanged();await loadResult();
 }));
 for(const summary of [false,true])el(summary?'accounting-summary-tab':'accounting-detail-tab').addEventListener('click',()=>{
  el('accounting-detail-view').hidden=summary;el('accounting-summary-view').hidden=!summary;
  el('accounting-detail-tab').classList.toggle('active',!summary);el('accounting-summary-tab').classList.toggle('active',summary);
 });
 return {open,refresh:async()=>{clear();if(!el('accounting-workspace').hidden)await loadChoices();}};
}
