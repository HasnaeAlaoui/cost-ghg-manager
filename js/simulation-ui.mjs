// Input drafts are temporary; R owns validation and every calculated result.
export function initializeSimulationUI({callJson,busy,leave,enabled=false}) {
 const el=id=>document.getElementById(id);
 const modules=['dashboard','master','structure','production','actual','accounting','variance'];
 let timer,revision=0,loading=false;
 const options=(id,rows,selected)=>{const select=el(id);select.replaceChildren();for(const row of rows){const option=document.createElement('option');option.value=row.value;option.textContent=row.label;select.append(option);}select.value=String(selected||rows[0]?.value||'');};
 const clear=()=>{clearTimeout(timer);loading=false;revision++;el('simulation-form').hidden=true;el('simulation-results').replaceChildren();el('simulation-error').hidden=true;};
 function values(){return {rEact:Number(el('simulation-time').value),powEact:Number(el('simulation-power').value),emEact:Number(el('simulation-emission').value),materials:[...el('simulation-materials').querySelectorAll('input')].map(input=>({bomID:Number(input.dataset.bomId),rMact:Number(input.value)}))};}
 function render(data){
  const host=el('simulation-results'),sections=[];
  for(const [title,unit,metrics] of [['Cost','EUR',[['PCact',data.accounting.PCact],['PCstd',data.accounting.PCstd],['PCTV',data.cost.PCTV]]],['GHG','kgCO2e',[['PCFact',data.accounting.PCFact],['PCFstd',data.accounting.PCFstd],['PCFTV',data.ghg.PCFTV]]]]){
   const section=document.createElement('section'),heading=document.createElement('h3');heading.textContent=title;section.append(heading);
   for(const [name,value] of metrics){const line=document.createElement('p');line.dataset.metric=name;line.dataset.value=String(value);line.textContent=`${{"PCact": "Actual Production Cost (PCact)", "PCstd": "Standard Production Cost (PCstd)", "PCTV": "Production Cost Variance (PCTV)", "PCFact": "Actual Production Carbon Footprint (PCFact)", "PCFstd": "Standard Production Carbon Footprint (PCFstd)", "PCFTV": "Production Carbon Footprint Total Variance (PCFTV)"}[name]}: ${Number(value).toLocaleString(undefined,{maximumFractionDigits:6})} ${unit}`;section.append(line);}sections.push(section);
  }
  host.replaceChildren(...sections);
 }
 async function calculate(token=++revision){
  if(!enabled)return;
  if(token!==revision||el('simulation-form').hidden||!el('simulation-form').checkValidity())return;
  if(busy()||loading){timer=setTimeout(()=>calculate(token),200);return;}
  try{const result=await callJson('simulate_activity_response_json',Number(el('simulation-order').value),Number(el('simulation-node').value),JSON.stringify(values()));if(token!==revision)return;el('simulation-error').hidden=true;render(result);}
  catch(error){if(token===revision){el('simulation-error').textContent=error.message;el('simulation-error').hidden=false;}}
 }
 async function response(resetNode=false,keepResults=false){
  if(!enabled)return;
  const selectedOrder=Number(el('simulation-order').value||0),selectedNode=resetNode?0:Number(el('simulation-node').value||0);
  if(keepResults){clearTimeout(timer);revision++;el('simulation-error').hidden=true;}else clear();
  const token=revision;loading=true;
  try{
   let data=await callJson('get_activity_response_json',selectedOrder,selectedNode);
   if(token!==revision)return;
   options('simulation-node',data.nodes??[],selectedNode);
   if(data.initialized&&!data.detail?.response&&el('simulation-node').value)data=await callJson('get_activity_response_json',selectedOrder,Number(el('simulation-node').value));
   if(token!==revision)return;
   const detail=data.detail?.response?data.detail:null;if(!detail)return;
   const {response:row,materials}=detail;
   for(const [id,key] of [['time','rEact'],['power','powEact'],['emission','emEact']])el(`simulation-${id}`).value=row[key];
   el('simulation-materials').replaceChildren();for(const material of materials){const label=document.createElement('label');label.textContent=`${material.name} (${material.type}) | bomID ${material.bomID} | rMact (${material.matUOM})`;const input=document.createElement('input');input.type='number';input.min='0';input.step='any';input.required=true;input.value=material.rMact;input.dataset.bomId=material.bomID;label.append(input);el('simulation-materials').append(label);}
   el('simulation-form').hidden=false;
  }catch(error){if(token===revision){el('simulation-error').textContent=error.message;el('simulation-error').hidden=false;}}
  finally{if(token===revision)loading=false;}
  if(token===revision)await calculate();
 }
 async function open(){
  if(!enabled)return;
  if(busy()||loading)return;
  el('analytical-variance-panel').hidden=true;el('simulation-workspace').hidden=false;
  for(const [id,on] of [['variance-analytical-tab',false],['variance-simulation-tab',true]]){el(id).classList.toggle('active',on);if(on)el(id).setAttribute('aria-current','page');else el(id).removeAttribute('aria-current');}
  try{const context=await callJson('get_standard_context_json');options('simulation-order',context.requests,el('simulation-order').value);await response(true);}catch(error){el('simulation-error').textContent=error.message;el('simulation-error').hidden=false;}
 }
 // Disable simulation controls when enabled is false.
 function availability(){
  el('variance-simulation-tab').disabled=!enabled;
  el('variance-simulation-tab').setAttribute('aria-disabled',String(!enabled));
  el('simulation-workspace').inert=!enabled;
  el('simulation-form').querySelector('fieldset').disabled=!enabled;
  if(!enabled){el('simulation-workspace').hidden=true;el('variance-simulation-tab').classList.toggle('active',false);el('variance-simulation-tab').removeAttribute('aria-current');}
 }
 availability();el('variance-simulation-tab').addEventListener('click',open);
 for(const id of [...modules.map(name=>`nav-${name}`),'variance-analytical-tab'])el(id).addEventListener('click',()=>{clear();el('simulation-workspace').hidden=true;});
 el('simulation-order').addEventListener('change',()=>response(true));el('simulation-node').addEventListener('change',()=>response());el('simulation-reset').addEventListener('click',()=>response(false,true));
 el('simulation-form').addEventListener('submit',event=>event.preventDefault());
 el('simulation-form').addEventListener('input',()=>{if(!enabled)return;clearTimeout(timer);loading=false;el('simulation-error').hidden=true;const token=++revision;timer=setTimeout(()=>calculate(token),250);});
 for(const id of ['new-project','load-project'])el(id).addEventListener('click',()=>{clear();availability();el('simulation-order').replaceChildren();el('simulation-node').replaceChildren();});
 return {refresh:async()=>{clear();availability();if(enabled&&!el('simulation-workspace').hidden)await open();}};
}
