// Independent variance workspace. R supplies every accounting result.
export function initializeVarianceUI({callJson,busy,leave,onChanged=()=>{}}) {
 const el=id=>document.getElementById(id);
 let working=false,result=null,detail=false;
 const otherModules=['dashboard','master','structure','production','actual','accounting'];
 function clear(){result=null;for(const name of ['cost','ghg'])el(`variance-${name}`).replaceChildren();el('variance-error').hidden=true;}
 function render(){
  el('variance-caption').textContent=detail?'AccountingDetail.MCS \u2014 Recursive Node-Level Analytical Variance Trace':'Accounting.MCS \u2014 Analytical Cost & GHG Variance Results';
  el('variance-summary-tab').classList.toggle('active',!detail);el('variance-detail-tab').classList.toggle('active',detail);
  for(const group of ['cost','ghg']) {
   const host=el(`variance-${group}`);host.replaceChildren();if(!result)continue;
   const data=result[group][detail?'detail':'summary'],columns=data.columns.filter(column=>column!=='rootNodeID'&&!column.endsWith('adj'));
   const table=document.createElement('table'),head=document.createElement('thead'),body=document.createElement('tbody'),heading=document.createElement('tr');
   for(const column of columns){const th=document.createElement('th');th.textContent=column;heading.append(th);}head.append(heading);table.append(head);
   for(const row of data.rows){const tr=document.createElement('tr');for(const column of columns){const td=document.createElement('td');td.textContent=Array.isArray(row[column])?JSON.stringify(row[column]):String(row[column]??'');tr.append(td);}body.append(tr);}
   table.append(body);host.append(table);
  }
 }
 async function run(action){
  if(working||busy())return;working=true;
  const surfaces=['main','.sidebar','.global-header'].map(selector=>document.querySelector(selector));surfaces.forEach(x=>x.inert=true);
  try{await action();}catch(error){el('variance-error').textContent=error.message;el('variance-error').hidden=false;}
  finally{surfaces.forEach(x=>x.inert=false);working=false;}
 }
 async function choices(orderID){
  const selected=orderID??el('variance-order').value;
  const data=await callJson('get_standard_context_json');el('variance-order').replaceChildren();
  const blank=document.createElement('option');blank.value='';blank.textContent='Select...';el('variance-order').append(blank);
  for(const row of data.requests){const option=document.createElement('option');option.value=row.value;option.textContent=row.label;el('variance-order').append(option);}
  el('variance-order').value=String(selected||data.resultOrder||'');el('calculate-variance').disabled=!el('variance-order').value;
 }
 function analytical(){el('analytical-variance-panel').hidden=false;el('simulation-workspace').hidden=true;for(const [id,on] of [['variance-analytical-tab',true],['variance-simulation-tab',false]]){el(id).classList.toggle('active',on);if(on)el(id).setAttribute('aria-current','page');else el(id).removeAttribute('aria-current');}}
 analytical();
 el('variance-analytical-tab').disabled=false;el('variance-analytical-tab').addEventListener('click',analytical);
 const open=orderID=>run(async()=>{
  analytical();
  clear();detail=false;render();await choices(orderID);leave();
  for(const name of otherModules){el(`${name}-workspace`).hidden=true;el(`nav-${name}`).classList.toggle('active',false);el(`nav-${name}`).removeAttribute('aria-current');}
  el('variance-workspace').hidden=false;el('nav-variance').classList.toggle('active',true);el('nav-variance').setAttribute('aria-current','page');
 });
 el('nav-variance').disabled=false;el('nav-variance').addEventListener('click',()=>open());
 for(const name of otherModules)el(`nav-${name}`).addEventListener('click',()=>{el('variance-workspace').hidden=true;el('nav-variance').classList.toggle('active',false);el('nav-variance').removeAttribute('aria-current');});
 el('variance-order').addEventListener('change',()=>run(async()=>{clear();el('calculate-variance').disabled=!el('variance-order').value;}));
 el('calculate-variance').addEventListener('click',()=>run(async()=>{
  clear();if(!el('variance-order').value)return;
  result=await callJson('calculate_variance_json',Number(el('variance-order').value));render();
 }));
 for(const value of [false,true])el(value?'variance-detail-tab':'variance-summary-tab').addEventListener('click',()=>{detail=value;render();});
 for(const name of ['new-project','load-project'])el(name).addEventListener('click',()=>{analytical();clear();el('variance-order').replaceChildren();el('calculate-variance').disabled=true;});
 return {open,refresh:async()=>{analytical();clear();if(!el('variance-workspace').hidden)await choices();}};
}
