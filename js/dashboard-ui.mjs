// Live read-only presentation. Values and calculation availability come from R.

const make=(tag,text,cls)=>{const x=document.createElement(tag);if(text!==undefined)x.textContent=text;if(cls)x.className=cls;return x;};
const format=value=>typeof value==='number'?value.toLocaleString(undefined,{maximumFractionDigits:2}):'Not available';
// Local layered, decorative icons; labels always carry the meaning.
function cardIcon(kind){
 const paths={
  coins:[['M3 6v10c0 4 12 4 12 0V6Z','#008c85'],['M3 6c0-4 12-4 12 0s-12 4-12 0Z','#28be9b'],['M3 11c2 3 10 3 12 0M3 15c2 3 10 3 12 0','none','#dbf8ef'],['M14 13v7c0 3 8 3 8 0v-7Z','#007a83'],['M14 13c0-3 8-3 8 0s-8 3-8 0Z','#36c6a2'],['M14 17c2 2 6 2 8 0','none','#dbf8ef']],
  chart:[['M3 21h19','none','#07576e'],['M4 12h4v7H4Z','#24b7a1'],['M10 7h4v12h-4Z','#008d98'],['M16 2h4v17h-4Z','#08607c']],
  leaf:[['M4 19C0 9 10 4 21 3c0 12-7 19-17 16Z','#18b893','#00747c'],['M6 15c3-6 7-7 11-8','none','#68d6af'],['M3 22C7 14 11 11 17 8','none','#075a68']],
  gear:[['M9 2h6l1 4 3-1 3 5-3 3 2 3-4 4-3-2-3 4-5-3 1-4-4-1V9l4-1Z','#00899a'],['M16 12a4 4 0 1 1-8 0 4 4 0 0 1 8 0','#effafb','#086679']],
  box:[['m3 7 9-5 9 5-9 5Z','#29b5bb','#006c80'],['m3 7 9 5v10l-9-5Z','#b3e7e9','#006c80'],['m12 12 9-5v10l-9 5Z','#008ba0','#006c80'],['m7 5 10 5','none','#effafb']],
  tag:[['M3 3h9l9 9-9 9-9-9Z','#1aabb0','#006d80'],['M9 7a2 2 0 1 1-4 0 2 2 0 0 1 4 0','#e8faf8'],['m8 13 4 4 5-5','none','#b9eee7']],
  cloud:[['M6 20a5 5 0 0 1-1-10 7 7 0 0 1 13-1 5.5 5.5 0 0 1 0 11Z','#087e9b','#075d79'],['M6 12c0-5 7-8 10-3','none','#55c7cf']]
 };
 const svg=document.createElementNS('http://www.w3.org/2000/svg','svg');
 for(const [key,value] of Object.entries({viewBox:'0 0 24 24',class:'dashboard-card-icon','aria-hidden':'true',focusable:'false',fill:'none',stroke:'currentColor','stroke-width':1.7,'stroke-linecap':'round','stroke-linejoin':'round'}))svg.setAttribute(key,value);
 svg.dataset.icon=kind;
 for(const [d,fill,stroke='none'] of paths[kind]){const path=document.createElementNS('http://www.w3.org/2000/svg','path');path.setAttribute('d',d);path.setAttribute('fill',fill);path.setAttribute('stroke',stroke);svg.append(path);}return svg;
}
const percent=value=>typeof value==='number'?`${value.toFixed(1)}%`:'Not available';
function donut(title,total,unit,parts){
 const card=make('section',undefined,'dashboard-composition');card.append(make('h3',`${title} (${format(total)} ${unit})`));const layout=make('div',undefined,'dashboard-donut-layout');
 const svg=document.createElementNS('http://www.w3.org/2000/svg','svg');svg.setAttribute('viewBox','0 0 200 200');svg.setAttribute('role','img');svg.setAttribute('aria-label',`${title}: ${format(total)} ${unit}`);
 const draw=(tag,attrs,text)=>{const x=document.createElementNS('http://www.w3.org/2000/svg',tag);for(const [key,value] of Object.entries(attrs))x.setAttribute(key,value);if(text!==undefined)x.textContent=text;svg.append(x);};
 draw('circle',{cx:100,cy:100,r:72,fill:'none',stroke:'#e8f0f4','stroke-width':32});
 let offset=0;const colors=['#17324d','#0d7c86','#78b89b'];
 // Percentages come from R. Only SVG geometry is calculated here.
 const drawable=parts.every(part=>typeof part.percent==='number'&&part.percent>=0&&part.percent<=100);
 if(drawable)parts.forEach((part,i)=>{draw('circle',{cx:100,cy:100,r:72,fill:'none',stroke:colors[i%colors.length],'stroke-width':32,pathLength:100,'stroke-dasharray':`${part.percent} ${100-part.percent}`,'stroke-dashoffset':-offset,transform:'rotate(-90 100 100)'});offset+=part.percent;});
 draw('text',{x:100,y:98,'text-anchor':'middle',fill:'#17324d','font-size':17,'font-weight':700},format(total));draw('text',{x:100,y:119,'text-anchor':'middle',fill:'#17324d','font-size':12},unit);
 const legend=make('div',undefined,'dashboard-donut-legend');parts.forEach((part,i)=>{const row=make('div');const swatch=make('span',undefined,'dashboard-swatch');swatch.style.backgroundColor=colors[i%colors.length];row.append(swatch,make('span',part.label),make('strong',`${format(part.value)} ${unit}`),make('span',percent(part.percent)));legend.append(row);});layout.append(svg,legend);card.append(layout);return card;
}
function ranking(title,unit,rows){const area=make('section',undefined,'dashboard-ranking');area.append(make('h3',`${title} (Top 8, actual ${unit})`));for(const row of rows){const line=make('div',undefined,'dashboard-ranking-row');const label=make('span',row.label);label.title=row.label;const track=make('div',undefined,'dashboard-rank-track'),bar=make('div');bar.style.width=`${typeof row.percent==='number'?Math.min(100,Math.max(0,row.percent)):0}%`;track.append(bar);line.append(label,track,make('span',format(row.value)),make('span',percent(row.percent)));area.append(line);}return area;}
export function initializeDashboardUI({callJson,busy,leave,openAccounting,openVariance}){
 const el=id=>document.getElementById(id);let working=false,selected=0;
 const modules=['master','structure','production','actual','accounting','variance'];
 const hide=()=>{el('dashboard-workspace').hidden=true;el('nav-dashboard').classList.toggle('active',false);el('nav-dashboard').removeAttribute('aria-current');};
 const button=(label,action)=>{const x=make('button',label);x.addEventListener('click',action);return x;};
 const navigate=(name,order=selected)=>{hide();if(name==='accounting')return openAccounting(order);if(name==='variance')return openVariance(order);el(`nav-${name}`).click();};
 for(const [label,name] of [['Master Data','master'],['Production Information','structure'],['Production Order','production'],['Production Execution','actual'],['Standard Accounting','accounting'],['Variance Analysis','variance']])el('dashboard-workflow').append(button(label,()=>navigate(name)));
 function render(data){
  selected=data.selectedOrder;const context=el('dashboard-context');context.replaceChildren();context.append(make('h3',data.projectName));const counts=make('div',undefined,'dashboard-counts');for(const [label,key] of [['Finished Products','finishedProducts'],['Production Orders','orders'],['Production Nodes','nodes']])counts.append(make('span',`${data.counts[key]} ${label}`));context.append(counts);
  el('dashboard-order').replaceChildren();for(const order of data.orders){const option=make('option',`Order ${order.orderID} - ${order.fpName} x ${order.quantity}`);option.value=order.orderID;el('dashboard-order').append(option);}el('dashboard-order').value=String(selected);el('dashboard-order').disabled=!data.orders.length;
  const content=el('dashboard-content');content.replaceChildren();const order=data.orders.find(x=>x.orderID===selected);
  if(!order){const empty=make('section',undefined,'dashboard-empty');empty.append(make('h3',data.empty?'Start your manufacturing project':'Prepare your first production order'),button(data.empty?'Start with Master Data':'Open Production Order',()=>navigate(data.empty?'master':'production')));content.append(empty);}
  else if(!order.ready){const notice=make('section',undefined,'dashboard-empty');notice.append(make('h3',`${order.fpName}: accounting results unavailable`),make('p',order.status),button('Open Production Order',()=>navigate('production')));content.append(notice);}
  else {
   const a=order.accounting,c=order.cost,g=order.ghg;const cards=make('div',undefined,'report-kpis');
   for(const [label,value,unit,benchmark] of [['Actual Production Cost',a.PCact,'EUR',`Standard: ${format(a.PCstd)} EUR`],['Production Cost Variance',order.analytics.costDifference,'EUR','Actual vs. Standard'],['Actual Production Carbon Footprint',a.PCFact,'kgCO2e',`Standard: ${format(a.PCFstd)} kgCO2e`],['Production Carbon Footprint Variance',order.analytics.ghgDifference,'kgCO2e','Actual vs. Standard']]){const card=make('section',undefined,'report-kpi');card.append(cardIcon(label==='Actual Production Cost'?'coins':label==='Production Cost Variance'?'chart':label==='Production Carbon Footprint Variance'?'cloud':'leaf'),make('h3',label),make('strong',format(value)),make('span',unit),make('p',benchmark));const change=label==='Actual Production Cost'?order.analytics.costChange:label==='Actual Production Carbon Footprint'?order.analytics.ghgChange:null;if(typeof change==='number'){const badge=make('span',`${change>0?'+':''}${percent(change)}`,'dashboard-change');badge.title=label==='Actual Production Cost'?'Difference from Standard Cost':'Difference from Standard PCF';badge.setAttribute('aria-label',`${badge.textContent} ${badge.title.toLowerCase()}`);badge.dataset.direction=change>0?'up':change<0?'down':'flat';if(change!==0){const arrow=make('span',undefined,'dashboard-badge-arrow');arrow.setAttribute('aria-hidden','true');badge.append(arrow);}card.append(badge);}cards.append(card);}content.append(cards);
   const performance=make('section',undefined,'dashboard-panel dashboard-performance');performance.append(make('h3',`Cost & GHG Performance - Order ${order.orderID} (${order.fpName} x ${order.quantity})`));const splits=make('div',undefined,'dashboard-performance-grid');
   const left=make('div'),right=make('div');left.append(donut('Production Cost',a.PCact,'EUR',order.analytics.costComposition),ranking('Cost Contributors','EUR',order.analytics.costContributors));right.append(donut('Carbon Footprint',a.PCFact,'kgCO2e',order.analytics.ghgComposition),ranking('Emission Contributors','kgCO2e',order.analytics.ghgContributors));splits.append(left,right);performance.append(splits);content.append(performance);
   const overview=make('section',undefined,'dashboard-panel');overview.append(make('h3','Variance Overview'));
   const groups=make('div',undefined,'dashboard-variance-groups');
   for(const [title,rows] of [
    ['Cost',[['Production Cost Flexible-Budget Variance (PCFBV)',c.PCFBV,'EUR'],['Production Cost Efficiency Variance (PCEV)',c.PCEV,'EUR'],['Production Cost Price Variance (PCPV)',c.PCPV,'EUR']]],
    ['GHG',[['Total Production Carbon Footprint Variance (PCFTV)',g.PCFTV,'kgCO2e'],['Internal Production Carbon Footprint Variance (PCFintTV)',g.PCFintTV,'kgCO2e'],['External Production Carbon Footprint Variance (PCFextTV)',g.PCFextTV,'kgCO2e']]]
   ]){const group=make('section');group.append(make('h4',title));const list=make('dl');for(const [label,value,unit] of rows)list.append(make('dt',label),make('dd',format(value)+' '+unit));group.append(list);groups.append(group);}overview.append(groups);content.append(overview);
   const actions=make('div',undefined,'dashboard-actions');actions.append(button('View Standard Accounting',()=>navigate('accounting')));content.append(actions);
  }
 }
 async function refresh(force=false){if(working||(!force&&busy()))return;working=true;el('dashboard-error').hidden=true;el('dashboard-content').replaceChildren();const surfaces=['main','.sidebar','.global-header'].map(x=>document.querySelector(x));surfaces.forEach(x=>x.inert=true);try{render(await callJson('get_dashboard_json',selected));}catch(error){el('dashboard-error').textContent=error.message;el('dashboard-error').hidden=false;}finally{surfaces.forEach(x=>x.inert=false);working=false;}}
 async function open(){if(working||busy())return;leave();for(const name of modules){el(`${name}-workspace`).hidden=true;el(`nav-${name}`).classList.toggle('active',false);el(`nav-${name}`).removeAttribute('aria-current');}el('dashboard-workspace').hidden=false;el('nav-dashboard').classList.toggle('active',true);el('nav-dashboard').setAttribute('aria-current','page');await refresh();}
 el('nav-dashboard').disabled=false;el('dashboard-refresh').disabled=false;el('nav-dashboard').addEventListener('click',open);el('dashboard-refresh').addEventListener('click',()=>refresh());el('dashboard-order').addEventListener('change',()=>{selected=Number(el('dashboard-order').value);return refresh();});for(const name of modules)el(`nav-${name}`).addEventListener('click',hide);
 return {open,refresh:async()=>{selected=0;if(!el('dashboard-workspace').hidden)await refresh(true);}};
}
