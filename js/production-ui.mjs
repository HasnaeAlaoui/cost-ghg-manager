import { initializeActualUI } from './actual-ui.mjs';
﻿import { drawGraph, initializeExpandedGraph } from './graph-viewer.mjs';
// Selection/input transport and rendering only. R owns production orders and occurrence data.
export function initializeProductionUI({callJson,busy,onChanged,leave}) {
  const el=id=>document.getElementById(id),viewer=initializeExpandedGraph();
  let working=false,active=false,table=false,editing=null,execution=false;
  function options(id,rows,selected){const select=el(id);select.replaceChildren();const blank=document.createElement('option');blank.value='';blank.textContent='Select...';select.append(blank);for(const row of rows){const option=document.createElement('option');option.value=row.value;option.textContent=row.label;select.append(option);}select.value=String(selected||'');}
  function renderTable(id,data){const host=el(id);host.replaceChildren();const tbl=document.createElement('table'),head=document.createElement('thead'),tr=document.createElement('tr');for(const column of data.columns){const th=document.createElement('th');th.textContent=column;tr.append(th);}if(id==='production-orders'){const th=document.createElement('th');th.textContent='Actions';tr.append(th);}head.append(tr);tbl.append(head);const body=document.createElement('tbody');for(const row of data.rows){const tr=document.createElement('tr');for(const column of data.columns){const td=document.createElement('td');td.textContent=Array.isArray(row[column])?JSON.stringify(row[column]):String(row[column]??'');tr.append(td);}if(id==='production-orders'){const td=document.createElement('td');for(const action of ['edit','delete']){const button=document.createElement('button');button.type='button';button.dataset.action=action;button.dataset.orderId=row.orderID;button.textContent=action==='edit'?'Edit':'Delete';button.disabled=row.orderStatus==='Completed';td.append(button);}tr.append(td);}body.append(tr);}tbl.append(body);host.append(tbl);}
  async function reload(){const data=await callJson('get_order_production_json',Number(el('production-order').value||0));options('production-fp',data.products,el('production-fp').value);options('production-order',data.requests,el('production-order').value||data.generatedOrder);renderTable('production-orders',data.orders);
    const matches=Number(el('production-order').value)===data.generatedOrder && data.generatedOrder>0 && data.tree.rows.length>0 && data.graph.nodes.length>0;
    drawGraph(el('production-tree'),matches?data.graph:{nodes:[],edges:[]},'Production node tree');
    renderTable('production-table',matches?data.tree:{columns:data.tree.columns,rows:[]});
    el('production-expand').disabled=!matches;
    el('production-generate').textContent='Generate ProNodeTree';
    const selected=data.orders.rows.find(row=>row.orderID===Number(el('production-order').value));
    el('production-generate').disabled=!selected||(data.generatedOrder>0&&Number(el('production-order').value)===data.generatedOrder)||selected.orderStatus==='Completed';
    el('production-tree-note').textContent=matches?`Order ${data.generatedOrder} | ${data.rootCount} FP roots`:'';
    el('production-details').textContent='';
  }
  async function run(action){if(working||busy())return;working=true;el('production-workspace').inert=true;el('actual-workspace').inert=true;const error=el(execution?'actual-error':'production-error');error.hidden=true;try{await action();}catch(problem){error.textContent=problem.message;error.hidden=false;}finally{working=false;el('production-workspace').inert=false;el('actual-workspace').inert=false;}}
  const actualUI=initializeActualUI({callJson,run,onChanged,renderTable});
  for(const [name,isExecution] of [['production',false],['actual',true]]){
    el(`nav-${name}`).disabled=false;
    el(`nav-${name}`).addEventListener('click',()=>{execution=isExecution;return run(async()=>{
      if(execution)await actualUI.reload();else await reload();leave();active=true;
      for(const other of ['dashboard','master','structure','production','actual','accounting','variance']){
        el(`${other}-workspace`).hidden=other!==name;el(`nav-${other}`).classList.toggle('active',other===name);el(`nav-${other}`).removeAttribute('aria-current');
      }
      el(`nav-${name}`).setAttribute('aria-current','page');
    });});
  }
  for(const name of ['dashboard','master','structure','accounting','variance'])el(`nav-${name}`).addEventListener('click',()=>{active=false;el('production-workspace').hidden=true;el('actual-workspace').hidden=true;for(const own of ['production','actual']){el(`nav-${own}`).classList.toggle('active',false);el(`nav-${own}`).removeAttribute('aria-current');}});
  for(const mode of ['requests','generated'])el(`production-${mode}`).addEventListener('click',()=>run(async()=>{
    el('production-request-view').hidden=mode!=='requests';el('production-generated-view').hidden=mode!=='generated';
    for(const tab of ['requests','generated'])el(`production-${tab}`).classList.toggle('active',mode===tab);
    await reload();
  }));
  function cancelEdit(){editing=null;el('production-editor-title').textContent='Create Production Order';el('production-save').textContent='Create Production Order';el('production-cancel').hidden=true;el('production-fp').value='';el('production-quantity').value='1';el('production-start').value='';el('production-end').value='';}
  el('production-cancel').addEventListener('click',()=>{if(!working)cancelEdit();});
  el('production-orders').addEventListener('click',event=>{
    const button=event.target.closest('button[data-order-id]');if(!button)return;
    run(async()=>{
      const id=Number(button.dataset.orderId);
      const [row]=await callJson('get_production_order_json',id);
      if(button.dataset.action==='edit'){
        editing=id;el('production-fp').value=String(row.fpID);el('production-quantity').value=String(row.orderQuantity);el('production-start').value=row.orderStart;el('production-end').value=row.orderEnd;
        el('production-editor-title').textContent=`Edit Order ${id}`;el('production-save').textContent='Update Production Order';el('production-cancel').hidden=false;
        el('production-fp').focus();
      }else if(window.confirm(`Delete Order ${id} and only its generated production nodes? This cannot be undone.`)){
        await callJson('delete_production_order',id);if(editing===id)cancelEdit();
        if(Number(el('production-order').value)===id)el('production-order').value='';
        onChanged();await reload();el('production-maintenance-status').textContent=`Order ${id} deleted. Other orders are unchanged.`;
      }
    });
  });
  el('production-request-form').addEventListener('submit',event=>{event.preventDefault();if(!el('production-request-form').reportValidity())return;
    run(async()=>{
      const fp=Number(el('production-fp').value),quantity=Number(el('production-quantity').value);
      if(editing===null){await callJson('create_production_order',fp,quantity,el('production-start').value,el('production-end').value);el('production-maintenance-status').textContent='Production order created.';}
      else {
        const id=editing,result=await callJson('update_production_order',id,fp,quantity,el('production-start').value,el('production-end').value);
        el('production-order').value=String(id);cancelEdit();
        el('production-maintenance-status').textContent=result.needsRegeneration?`Order ${id} updated. Generate ProNodeTree for the updated order.`:`Order ${id} unchanged; generated production nodes retained.`;
      }
      onChanged();await reload();
    });
  });
  el('production-order').addEventListener('change',()=>run(reload));
  el('production-generate').addEventListener('click',()=>run(async()=>{await callJson('generate_pro_node_tree',Number(el('production-order').value));onChanged();await reload();}));
  for(const showTable of [false,true])el(showTable?'production-table-mode':'production-tree-mode').addEventListener('click',()=>{table=showTable;el('production-table-meta').hidden=!table;el('production-tree-meta').hidden=table;el('production-table').hidden=!table;el('production-tree').hidden=table;el('production-table-mode').classList.toggle('active',table);el('production-tree-mode').classList.toggle('active',!table);});
  el('production-expand').addEventListener('click',()=>{if(!working&&!busy()&&!el('production-expand').disabled)viewer.open(el('production-tree'),'Production Node Tree',el('production-expand'));});
  for(const name of ['click','focusin'])el('production-tree').addEventListener(name,event=>{const node=event.target.closest('[data-details]');if(node)el('production-details').textContent=node.dataset.details;});
  return {isBusy:()=>working,refresh:async()=>{cancelEdit();el('production-maintenance-status').textContent='';options('production-fp',[]);options('production-order',[]);await actualUI.refresh();if(active)await reload();}};
}
