// Presentation only. All differences, per-unit values and filtering come from R.
const node=(tag,text,cls)=>{const el=document.createElement(tag);if(text!==undefined)el.textContent=text;if(cls)el.className=cls;return el;};
export const comparisonLabel=row=>({costDifference:'Production Cost Variance',PCFBV:'Production Cost Flexible-Budget Variance (PCFBV)',PCFTV:'Total Production Carbon Footprint Variance (PCFTV)',PCFintTV:'Internal Production Carbon Footprint Variance (PCFintTV)',PCFextTV:'External Production Carbon Footprint Variance (PCFextTV)'}[row.metric]??row.label);
const format=(value,unit='')=>typeof value==='number'?value.toLocaleString(undefined,{maximumFractionDigits:unit==='EUR'?2:6}):String(value??'');
function table(headers,rows){
 const wrap=node('div',undefined,'report-table'),table=node('table'),head=node('thead'),tr=node('tr');
 for(const header of headers)tr.append(node('th',header));head.append(tr);table.append(head);
 const body=node('tbody');for(const values of rows){const tr=node('tr');for(const value of values){const td=node('td');if(typeof value==='object')td.append(value);else td.textContent=value;tr.append(td);}body.append(tr);}table.append(body);wrap.append(table);return wrap;
}
const metrics=rows=>table(['Analytical component','Value'],rows.map(row=>[row.metric,`${format(row.value,row.unit)} ${row.unit}`]));
export function renderProductionDeviation(target,detail){
 target.replaceChildren();target.append(node('h3',`Order ${detail.orderID} · Node ${detail.nodeID} · ${detail.itemName}`),
  node('p',`itemID ${detail.itemID} · routeID ${detail.routeID} · jobID ${detail.jobID} · itemQuantity ${format(detail.itemQuantity)} · order quantity ${format(detail.orderQuantity)}`,'report-subtitle'));
 target.append(table(['Scope / metric','Standard','Actual','Actual vs. Standard'],detail.results.rows.flatMap(row=>[
  [`${row.basis} cost (EUR)`,format(row.PCstd,'EUR'),format(row.PCact,'EUR'),format(row.costDifference,'EUR')],
  [`${row.basis} PCF (kgCO2e)`,format(row.PCFstd),format(row.PCFact),format(row.pcfDifference)]
 ])));
 target.append(node('p','Values are recursively aggregated through the production structure, including parent WIP quantity effects. Direct local results exclude child-derived WIP contributions. Zero direct local deviation does not exclude WIP-consumption effects.','report-subtitle'),node('h3','Observed input differences'));
 target.append(table(['Input / unit','Standard','Actual','Difference'],detail.inputs.rows.map(row=>[`${row.input} (${row.unit})`,format(row.standard),format(row.actual),format(row.difference)])));
 target.append(node('h3','Direct BOM inputs, including WIP'),table(['BOM input / unit','Standard','Actual','Difference'],detail.materials.rows.map(row=>[`${row.itemName} (${row.itemType}), bomID ${row.bomID} · ${row.unit}`,format(row.rMstd),format(row.rMact),format(row.difference)])));
 const analytics=node('div',undefined,'report-two-column');analytics.append(metrics(detail.analyticalCost.rows),metrics(detail.analyticalGHG.rows));
 target.append(node('h3','Recursive analytical variance components'),node('p','PDCEV and PMCFEV include WIP-consumption efficiency effects. PCFBV is flexible-budget variance, not the Actual vs. Standard full-cost difference. Edit observations in Production Execution / Edit Activity Response.','report-subtitle'),analytics);
}
export function renderComparison(target,data,{loadNodes}){
 target.replaceChildren();
 const labels=data.orders.map(order=>`${order.fpName} · Order ${order.orderID}`);
 const page=(title,branded=false)=>{const section=node('section',undefined,'report-page comparison-page');const titleBlock=branded?node('div',undefined,'report-heading-copy'):section;if(branded){const heading=node('header',undefined,'report-heading'),logo=node('img');logo.src='assets/tu-wien-logo.png';logo.alt='TU Wien';heading.append(titleBlock,logo);section.append(heading);}titleBlock.append(node('p','MANUFACTURING ANALYTICS / COMPARISON','report-eyebrow'),node('h2',title));target.append(section);return section;};
 const overview=page('Production Cost & GHG Comparison',true);
 overview.children[0].children[0].append(node('p',data.projectName,'report-subtitle'),node('p','Manufacturing Cost & GHG Manager','report-brand'));
 overview.append(table(['Production overview',...labels],[['Product',...data.orders.map(row=>row.fpName)],['Order ID',...data.orders.map(row=>String(row.orderID))],['Production quantity',...data.orders.map(row=>format(row.orderQuantity))]]));
 const comparison=(rows)=>table(['Metric',...labels],rows.map(row=>[`${comparisonLabel(row)} (${row.unit})`,...['left','right'].map(side=>{const cell=node('div');cell.append(node('strong',format(row[side],row.unit)),node('small',`${format(row[`${side}Unit`],row.unit)} ${row.unit} / produced unit`,'comparison-unit'));return cell;})]));
 overview.append(node('p','Each value is an order total; the smaller value is per produced unit. Use per-unit values when quantities differ. Totals use finished-product roots only.','report-subtitle'),node('h3','Cost comparison'),comparison(data.cost),node('p','Production Cost Variance: Actual vs. Standard. Production Cost Flexible-Budget Variance (PCFBV) is a separate analytical metric.','report-subtitle'),node('h3','GHG comparison'),comparison(data.ghg));
 const drivers=page('Analytical Variance Drivers');drivers.append(node('p',`${labels.join(' versus ')}. Existing analytical components retain their definitions; these are potential drivers, not proven operational causes.`,'report-subtitle'),node('h3','Cost components'),comparison(data.costDrivers),node('h3','GHG components'),comparison(data.ghgDrivers));
 const explore=node('details',undefined,'report-details comparison-explore');explore.append(node('summary','Explore Production Deviations'),node('p','Optional: inspect either order separately. Nodes are not paired across products. Only production nodes with nonzero results, analytical drivers or input differences are listed (numerical tolerance 1e−10). Expand and load a node to include its details in PDF.','report-subtitle'));
 const controls=node('div',undefined,'comparison-explore-controls'),status=node('p','Choose an order to load its production deviations.','report-subtitle'),selection=node('div',undefined,'comparison-explore-controls'),detail=node('div');
 let loading=false;
 for(const order of data.orders){const button=node('button',`Inspect ${order.fpName} · Order ${order.orderID}`);button.type='button';button.addEventListener('click',async()=>{
  if(loading)return;loading=true;for(const child of controls.children)child.disabled=true;selection.replaceChildren();detail.replaceChildren();status.textContent='Loading production deviations…';
  try{
   const result=await loadNodes(order.orderID);status.textContent=`${order.fpName} · Order ${order.orderID}: ${result.deviationCount} of ${result.totalCount} production nodes have deviations or changed inputs. Repeated items remain distinct.`;
   if(!result.details.length)return;
   const label=node('label','Production node'),select=node('select');select.setAttribute('aria-label',`Production node for order ${order.orderID}`);
   for(const row of result.details){const option=node('option',`Node ${row.nodeID} · ${row.itemName}`);option.value=String(row.nodeID);select.append(option);}
   const show=()=>{const row=result.details.find(row=>String(row.nodeID)===select.value);if(row)renderProductionDeviation(detail,row);};
   select.addEventListener('change',show);label.append(select);selection.append(label);select.value=String(result.details[0].nodeID);show();
  }catch(error){status.textContent=`Could not load deviations: ${error.message}`;}
  finally{loading=false;for(const child of controls.children)child.disabled=false;}
 });controls.append(button);}
 explore.append(controls,status,selection,detail);target.append(explore,node('p','Displayed values are rounded; small differences may round to zero. Refresh the comparison after editing observations.','report-footnote'));target.hidden=false;
}
