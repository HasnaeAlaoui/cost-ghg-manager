import { renderComparison } from './comparison-ui.mjs';
// Presentation only: every accounting value and aggregate is supplied by R.
const format=value=>typeof value==='number'?value.toLocaleString(undefined,{maximumFractionDigits:6}):Array.isArray(value)?value.join(', '):String(value??'');
const node=(tag,text,cls)=>{const x=document.createElement(tag);if(text!==undefined)x.textContent=text;if(cls)x.className=cls;return x;};
export function reportChart(title,unit,series){
 const figure=node('figure',undefined,'report-chart');figure.append(node('figcaption',`${title} (${unit})`));
 const svg=document.createElementNS('http://www.w3.org/2000/svg','svg');
 const height=series.length*42+18;svg.setAttribute('viewBox',`0 0 620 ${height}`);svg.setAttribute('role','img');svg.setAttribute('aria-label',`${title}, ${unit}`);
 const draw=(tag,attrs,text)=>{const x=document.createElementNS('http://www.w3.org/2000/svg',tag);for(const [k,v] of Object.entries(attrs))x.setAttribute(k,v);if(text!==undefined)x.textContent=text;svg.append(x);};
 // Scaling and coordinates are chart geometry, not accounting calculations.
 const scale=Math.max(...series.map(x=>Math.abs(x.value)),1e-12);
 draw('line',{x1:365,x2:365,y1:4,y2:height-4,stroke:'#68777b'});
 series.forEach((entry,i)=>{const y=i*42+12;const width=Math.abs(entry.value)/scale*130;
  draw('text',{x:4,y:y+16,'font-size':12,fill:'#243b44'},entry.label);
  draw('rect',{x:entry.value<0?365-width:365,y,width,height:22,fill:i%2?'#587782':'#087f82',stroke:'#243b44'});
  draw('text',{x:510,y:y+16,'font-size':12,fill:'#243b44'},format(entry.value));
 });figure.append(svg);return figure;
}
function resultTable(data){const table=node('table'),head=node('tr');for(const col of data.columns)head.append(node('th',col));const thead=node('thead');thead.append(head);table.append(thead);const body=node('tbody');for(const row of data.rows){const tr=node('tr');for(const col of data.columns)tr.append(node('td',format(row[col])));body.append(tr);}table.append(body);const wrap=node('div',undefined,'report-table');wrap.append(table);return wrap;}
// Percentages below are display shares of R-provided totals; no accounting totals are recalculated.
export function composition(title,unit,total,parts,externalParts=[]){
 const card=node('figure',undefined,'report-composition');card.append(node('figcaption',title),node('strong',`${format(total)} ${unit}`));
 const valid=total>0&&parts.every(([,value])=>Number.isFinite(value)&&value>=0);
 const svg=document.createElementNS('http://www.w3.org/2000/svg','svg');svg.setAttribute('viewBox','0 0 400 24');svg.setAttribute('role','img');svg.setAttribute('aria-label',`${title} composition`);let offset=0;
 parts.forEach(([label,value],index)=>{const share=valid?value/total:0;const rect=document.createElementNS('http://www.w3.org/2000/svg','rect');rect.setAttribute('x',offset);rect.setAttribute('y',0);rect.setAttribute('width',share*400);rect.setAttribute('height',24);rect.setAttribute('fill',index?'#809aa6':'#086f73');rect.setAttribute('stroke','white');svg.append(rect);offset+=share*400;
 const row=node('p',undefined,'report-legend');row.append(node('span',`${index+1}. ${label}`),node('span',`${format(value)} ${unit} | ${valid?(share*100).toFixed(1)+'%':'share unavailable'}`));card.append(row);if(label==='External')for(const [name,amount] of externalParts){const detail=node('p',undefined,'report-legend report-legend-detail');detail.append(node('span',name),node('span',format(amount)+' '+unit));card.append(detail);}});card.append(svg);return card;
}
export function managementInsights(cost,ghg,costDifference){
 const costZero=costDifference===0&&cost.PCEV===0&&cost.PCPV===0;
 const ghgZero=ghg.PCFTV===0&&ghg.PCFintTV===0&&ghg.PMCFTV===0&&ghg.PECFTV===0;
 if(costZero&&ghgZero)return ['No Production Cost or GHG variances were recorded.'];
 const facts=[];
 if(costZero)facts.push('No Production Cost variances were recorded.');
 else {
  if(typeof costDifference==='number')facts.push(`Production Cost Variance (Actual vs. Standard) is ${format(costDifference)} EUR.`);
  if(cost.PCEV!==0&&cost.PCPV===0)facts.push(`Production Cost Flexible-Budget Variance (PCFBV) is driven by efficiency variance (${format(cost.PCEV)} EUR); price variance is zero.`);
  else facts.push(`Production Cost Efficiency Variance is ${format(cost.PCEV)} EUR; Production Cost Price Variance is ${format(cost.PCPV)} EUR.`);
 }
 if(ghgZero)facts.push('No GHG variances were recorded.');
 else facts.push(`Material PCF Variance is ${format(ghg.PMCFTV)} kgCO2e; Internal PCF Variance is ${format(ghg.PCFintTV)} kgCO2e; Equipment PCF Variance is ${format(ghg.PECFTV)} kgCO2e.`);
 return facts;
}
function varianceBridge(c){
 const card=node('figure',undefined,'report-bridge');card.append(node('figcaption','Analytical Cost Variance Driver Bridge (EUR)'));
 for(const steps of [
  [['Activity efficiency','PACEV','+'],['Material efficiency','PDCEV','='],['Efficiency subtotal','PCEV','']],
  [['Efficiency subtotal','PCEV','+'],['Price variance','PCPV','='],['Production Cost Flexible-Budget Variance (PCFBV)','PCFBV','']]
 ]){const track=node('div',undefined,'report-bridge-track');
 for(const [label,key,connector] of steps){const step=node('div',undefined,'report-bridge-step');step.append(node('span',label),node('strong',format(c[key])),node('small',key));track.append(step);if(connector)track.append(node('span',connector,'report-bridge-connector'));}card.append(track);}return card;
}
export function renderReport(target,data){
 target.replaceChildren();const a=data.accounting,c=data.variance.cost.summary.rows[0],g=data.variance.ghg.summary.rows[0];
 const page=(number,title,subtitle)=>{const section=node('section',undefined,'report-page');const titleBlock=number==='01'?node('div',undefined,'report-heading-copy'):section;if(number==='01'){const header=node('header',undefined,'report-heading'),logo=node('img');logo.src='assets/tu-wien-logo.png';logo.alt='TU Wien';header.append(titleBlock,logo);section.append(header);}titleBlock.append(node('p',`MANUFACTURING ANALYTICS / ${number}`,'report-eyebrow'),node('h2',title),node('p',subtitle,'report-subtitle'));target.append(section);return section;};
 const executive=page('01','Executive Summary',`${data.projectName} | Order ${a.orderID} | ${a.fpName} | Quantity ${a.orderQuantity}`);
 executive.children[0].children[0].append(node('p','Cost & GHG Manager','report-brand'),node('h1','Production Cost & GHG Accounting Report'));
 const cards=node('div',undefined,'report-kpis');
 for(const [label,value,unit,benchmark] of [['Actual Production Cost',a.PCact,'EUR',`Standard Cost: ${format(a.PCstd)} EUR`],['Production Cost Variance',data.costDifference,'EUR','Actual vs. Standard'],['Actual Production Carbon Footprint',a.PCFact,'kgCO2e',`Standard PCF: ${format(a.PCFstd)} kgCO2e`],['GHG Variance',g.PCFTV,'kgCO2e','Total carbon-footprint variance']]){const card=node('section',undefined,'report-kpi');card.append(node('h3',label),node('strong',format(value)),node('span',unit),node('p',benchmark));cards.append(card);}executive.append(cards);
 const performance=node('section',undefined,'report-performance');performance.append(node('h3','Cost & GHG Performance'),node('p','Actual composition | share of the corresponding actual total','report-subtitle'));const splits=node('div',undefined,'report-two-column');
 splits.append(composition('Production Cost','EUR',a.PCact,[['Activity',a.PACact],['Direct / material',a.PDCact]]),composition('Carbon Footprint','kgCO2e',a.PCFact,[['Internal',a.PCFintact],['External',a.PCFextact]],[['Material',a.PMCFact],['Equipment',a.PECFact]]));performance.append(splits);executive.append(performance);
 const drivers=node('section',undefined,'report-drivers');drivers.append(node('h3','Key Variance Drivers'));const list=node('div',undefined,'report-driver-grid');for(const [label,value,unit] of [['Activity Efficiency',c.PACEV,'EUR'],['Direct Material Efficiency',c.PDCEV,'EUR'],['Price Variance',c.PCPV,'EUR'],['Internal PCF Variance',g.PCFintTV,'kgCO2e'],['Material PCF Variance',g.PMCFTV,'kgCO2e'],['Equipment PCF Variance',g.PECFTV,'kgCO2e']]){const item=node('div');item.append(node('span',label),node('strong',`${format(value)} ${unit}`));list.append(item);}drivers.append(list);executive.append(drivers);
 const insights=node('section',undefined,'report-insights');insights.append(node('h3','Management Insights'));for(const fact of managementInsights(c,g,data.costDifference))insights.append(node('p',fact));executive.append(insights);
 const chart=(parent,title,unit,source,fields)=>parent.append(reportChart(title,unit,fields.map(([label,key])=>({label,value:source[key]}))));
 const metrics=(parent,source,fields,unit)=>parent.append(resultTable({columns:['Analytical metric',unit],rows:fields.map(key=>({'Analytical metric':key,[unit]:source[key]}))}));
 const cost=page('02','Cost Analysis','Standard benchmarks, actual composition and analytical variance drivers | EUR');
 chart(cost,'Standard vs Actual Production Cost','EUR',a,[['Standard','PCstd'],['Actual','PCact']]);
 const costSplit=node('div',undefined,'report-two-column');costSplit.append(composition('Standard Cost','EUR',a.PCstd,[['Activity',a.PACstd],['Direct / material',a.PDCstd]]),composition('Actual Cost','EUR',a.PCact,[['Activity',a.PACact],['Direct / material',a.PDCact]]));cost.append(costSplit);
 cost.append(varianceBridge(c),node('p','Efficiency components form the efficiency subtotal; efficiency and price form the flexible-budget variance. This is not the difference between full actual and standard cost.','report-subtitle'));
 metrics(cost,c,['PACEV','PACPV','PACFBV','PDCEV','PDCPV','PDCFBV','PCEV','PCPV','PCFBV'],'EUR');
 const ghg=page('03','GHG Analysis','Internal and external carbon footprint | kgCO2e');
 chart(ghg,'Standard vs Actual PCF','kgCO2e',a,[['Standard','PCFstd'],['Actual','PCFact']]);
 const ghgSplit=node('div',undefined,'report-two-column');ghgSplit.append(composition('Standard PCF','kgCO2e',a.PCFstd,[['Internal',a.PCFintstd],['External',a.PCFextstd]]),composition('Actual PCF','kgCO2e',a.PCFact,[['Internal',a.PCFintact],['External',a.PCFextact]]));ghg.append(ghgSplit);
 chart(ghg,'GHG Variance Drivers','kgCO2e',g,[['Internal variance','PCFintTV'],['External variance','PCFextTV'],['Total variance','PCFTV']]);metrics(ghg,g,['PACFEmV','PACFEnyV','PACFTV','PECFEV','PECFELV','PECFTV','PMCFEV','PMCFELV','PMCFTV','PCFintTV','PCFextTV','PCFTV'],'kgCO2e');
 const details=node('details',undefined,'report-details');details.append(node('summary','Detailed Accounting Results'),node('h3','AccountingDetail.MCS \u00b7 Standard and Actual'),resultTable(data.detail));
 for(const [title,group,fields] of [['Cost Variance','cost',['orderID','nodeID','itemID','PCEV','PCPV','PCFBV']],['GHG Variance','ghg',['orderID','nodeID','itemID','PCFintTV','PCFextTV','PCFTV']]]){const source=data.variance[group].detail;details.append(node('h3',title),resultTable({columns:fields,rows:source.rows}));}details.append(node('p','Values are recursively aggregated through the production structure.'));target.append(details,node('p','Displayed values are rounded. Cost: EUR \u00b7 Carbon footprint: kgCO2e.','report-footnote'));target.hidden=false;
}
