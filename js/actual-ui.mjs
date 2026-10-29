// Presentation/input transport only. R supplies responses, identities, units and references.
export function initializeActualUI({callJson,run,onChanged,renderTable}) {
 const el=id=>document.getElementById(id);
 function options(id,rows){const select=el(id),selected=select.value;select.replaceChildren();const blank=document.createElement('option');blank.value='';blank.textContent='Select...';select.append(blank);for(const row of rows){const option=document.createElement('option');option.value=row.value;option.textContent=row.label;select.append(option);}select.value=selected;}
 async function reload(){
  const data=await callJson('get_activity_response_json',Number(el('actual-order').value||0),Number(el('actual-node').value||0));
  options('actual-order',data.requests);options('actual-node',data.nodes);renderTable('actual-table',data.table);
  el('actual-execute').disabled=!el('actual-order').value||data.orderStatus!=='Planned'||data.initialized;
  el('actual-entry-tab').disabled=!data.initialized;
  el('actual-status').textContent=data.orderStatus||'';
  el('actual-order-context').textContent=data.context?`Order ${data.context.orderID} | ${data.context.fpName} | Quantity ${data.context.orderQuantity} | ${data.context.orderStart} to ${data.context.orderEnd} | ${data.context.orderStatus}`:'';
  const detail=data.detail?.response?data.detail:null;el('actual-detail').hidden=!detail;
  el('actual-materials').replaceChildren();if(!detail)return;
  const row=detail.response,route=detail.route;
  el('actual-identity').textContent=`Activity Response ${row.jobID} | orderID ${row.orderID} | nodeID ${row.nodeID} | ${detail.itemName} | itemID ${row.itemID} | routeID ${row.routeID}`;
  el('actual-route-reference').textContent=`${route.actyName} | ${route.eqipName} | ${route.enyName} | rEstd ${route.rEstd} ${route.eqipUOM} | powEstd ${route.powEstd} ${route.powUOM}`;
  el('actual-time-label').textContent=`Actual execution time / rEact (${route.eqipUOM})`;el('actual-power-label').textContent=`Actual power / powEact (${route.powUOM})`;
  el('actual-time').value=row.rEact;el('actual-power').value=row.powEact;
  el('actual-route-reference').textContent += ` | emEstd ${detail.emEstd} ${detail.uOMemE}`;
  el('actual-emission-label').textContent=`Actual energy emission coefficient / emEact (${detail.uOMemE})`;
  el('actual-emission').value=row.emEact;
  for(const material of detail.materials){
   const label=document.createElement('label');label.textContent=`${material.name} (${material.type}), bomID ${material.bomID} | Standard rMstd: ${material.rMstd} ${material.matUOM} | Actual rMact:`;
   const input=document.createElement('input');input.type='number';input.min='0';input.step='any';input.required=true;input.dataset.bomId=material.bomID;input.value=material.rMact;
   label.append(input);el('actual-materials').append(label);
  }
 }
 el('actual-order').addEventListener('change',()=>run(async()=>{el('actual-node').value='';await reload();}));
 el('actual-node').addEventListener('change',()=>run(reload));

 const number=input=>input.value===''?null:Number(input.value);
 el('actual-form').addEventListener('submit',event=>{event.preventDefault();if(!el('actual-form').reportValidity())return;run(async()=>{
  const values={rEact:number(el('actual-time')),powEact:number(el('actual-power')),emEact:number(el('actual-emission')),materials:[...el('actual-materials').querySelectorAll('input')].map(input=>({bomID:Number(input.dataset.bomId),rMact:number(input)}))};
  await callJson('update_activity_response',Number(el('actual-order').value),Number(el('actual-node').value),JSON.stringify(values));onChanged();await reload();el('actual-status').textContent='Actual observations saved for this production node.';
 });});
 el('actual-execute').addEventListener('click',()=>run(async()=>{
  if(!el('actual-order').value)return;
  const result=await callJson('execute_production_order',Number(el('actual-order').value));
  if(!result.existing)onChanged();
  await reload();
 }));
 for(const table of [false,true])el(table?'actual-table-tab':'actual-entry-tab').addEventListener('click',()=>run(async()=>{
  el('actual-entry').hidden=table;el('actual-entry-meta').hidden=table;el('actual-table').hidden=!table;el('actual-table-meta').hidden=!table;
  el('actual-table-tab').classList.toggle('active',table);el('actual-entry-tab').classList.toggle('active',!table);
  await reload();
 }));
 return {reload,refresh:async()=>{el('actual-order').value='';el('actual-node').value='';if(!el('actual-workspace').hidden)await reload();}};
}
