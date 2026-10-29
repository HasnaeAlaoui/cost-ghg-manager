// Only visual coordinates are retained; no graph relationships or R domain rows.
const layouts = new Map();
let layoutChanged = () => {};
export const visualLayouts = {
  onChanged(callback) { layoutChanged = callback; },
  snapshot() { return {version:1,graphs:[...layouts].map(([key,positions])=>({key,positions:[...positions].map(([id,p])=>({id,...p}))}))}; },
  restore(value) {
    const candidate = new Map();
    if(value !== undefined) {
      if(value?.version!==1 || !Array.isArray(value.graphs)) throw Error('Invalid visual layout metadata.');
      for(const graph of value.graphs) {
        if(typeof graph.key!=='string' || !Array.isArray(graph.positions) || candidate.has(graph.key)) throw Error('Invalid graph layout.');
        const positions=new Map();
        for(const p of graph.positions) {
          if(typeof p.id!=='string' || !Number.isFinite(p.x) || !Number.isFinite(p.y) || positions.has(p.id)) throw Error('Invalid node position.');
          positions.set(p.id,{x:p.x,y:p.y});
        }
        candidate.set(graph.key,positions);
      }
    }
    layouts.clear();for(const [key,positions] of candidate)layouts.set(key,positions);
  }
};
function pruneLayout(key,ids) {
  const saved=layouts.get(key);if(!saved)return;
  let changed=false;for(const id of saved.keys())if(!ids.has(id)){saved.delete(id);changed=true;}
  if(!saved.size)layouts.delete(key);
  if(changed)layoutChanged();
}
export function drawGraph(container, graph, label = "Manufacturing graph", layoutKey = "") {
    if(layoutKey)pruneLayout(layoutKey,new Set(graph.nodes.map(n=>n.id)));
    container.replaceChildren();
    if (!graph.nodes.length) { container.textContent = 'No design relations yet.'; return; }
    const ns = 'http://www.w3.org/2000/svg';
    const svg = document.createElementNS(ns, 'svg');
    svg.dataset.layoutKey = layoutKey;
    const width = Math.max(...graph.nodes.map(n => n.x)) + 120, height = Math.max(...graph.nodes.map(n => n.y)) + 70;
    svg.setAttribute('viewBox', `0 0 ${width} ${height}`); svg.setAttribute('width', width); svg.setAttribute('height', height);
    svg.setAttribute('role', 'group'); svg.setAttribute('aria-label', label);
    const byID = new Map(graph.nodes.map(node => [node.id, node]));
    for (const edge of graph.edges) {
      const a = byID.get(edge.from), b = byID.get(edge.to);
      const line = document.createElementNS(ns, 'path');
      const vertical = graph.direction === 'TB';
      const dy = b.y >= a.y ? 1 : -1;
      line.setAttribute('d', vertical
        ? `M${a.x},${a.y+dy*23} C${a.x},${a.y+dy*65} ${b.x},${b.y-dy*65} ${b.x},${b.y-dy*29} M${b.x-4},${b.y-dy*35} L${b.x},${b.y-dy*29} L${b.x+4},${b.y-dy*35}`
        : `M${a.x+80},${a.y} L${b.x-86},${b.y} M${b.x-92},${b.y-4} L${b.x-86},${b.y} L${b.x-92},${b.y+4}`);
      const edgeTitle = document.createElementNS(ns,'title'); edgeTitle.textContent = edge.details || edge.label; line.append(edgeTitle);
      line.dataset.from = edge.from; line.dataset.to = edge.to; line.dataset.direction = graph.direction; line.setAttribute('class', 'design-edge'); svg.append(line);
      const text = document.createElementNS(ns, 'text'); text.setAttribute('x', (a.x+b.x)/2+7); text.setAttribute('y', (a.y+b.y)/2); text.textContent = edge.label; text.dataset.from = edge.from; text.dataset.to = edge.to; text.setAttribute('class', 'design-edge-label'); svg.append(text);
    }
    for (const node of graph.nodes) {
      const group = document.createElementNS(ns, 'g'); group.setAttribute('class', `design-node design-${node.type}`);
      group.dataset.nodeId = node.id; group.dataset.x = node.x; group.dataset.y = node.y; group.setAttribute('tabindex','0'); group.setAttribute('role','button');
      group.dataset.details = node.details || `${node.label} (${node.type})`;
      group.setAttribute('aria-label', group.dataset.details);
      const rect = document.createElementNS(ns, 'rect'); for (const [key,value] of Object.entries({x:node.x-80,y:node.y-23,width:160,height:46,rx:5})) rect.setAttribute(key,value);
      const title = document.createElementNS(ns, 'title'); title.textContent = group.dataset.details;
      const text = document.createElementNS(ns, 'text'); text.setAttribute('x',node.x); text.setAttribute('y',node.y); text.setAttribute('text-anchor','middle'); text.textContent = node.label.length > 21 ? node.label.slice(0,20)+'…' : node.label;
      const type = document.createElementNS(ns, 'text'); type.setAttribute('x',node.x); type.setAttribute('y',node.y+15); type.setAttribute('text-anchor','middle'); type.setAttribute('class','design-type'); type.textContent=node.type;
      group.append(rect,title,text,type); svg.append(group);
    }
    container.append(svg);
  }
let sharedViewer;
export function initializeExpandedGraph() {
  if (sharedViewer) return sharedViewer;
  const el = id => document.getElementById(id);
  const dialog=el('expanded-graph'), canvas=el('expanded-graph-canvas');
  let original, trigger, drag, key;
  function fit(fitted) {
    const svg=canvas.querySelector('svg');
    if(svg){
      if(fitted){
        const nodes=[...svg.querySelectorAll('[data-node-id]')];
        if(nodes.length){
          const xs=nodes.map(n=>Number(n.dataset.x)),ys=nodes.map(n=>Number(n.dataset.y));
          const left=Math.min(0,Math.min(...xs)-110),top=Math.min(0,Math.min(...ys)-70);
          const width=Math.max(...xs)+120-left,height=Math.max(...ys)+70-top;
          svg.setAttribute('viewBox',`${left} ${top} ${width} ${height}`);
          svg.setAttribute('width',width);svg.setAttribute('height',height);
        }
      }
      svg.style.width=fitted?'100%':'';svg.style.height=fitted?'100%':'';
    }
  }
  function restoreView(){
    canvas.replaceChildren();if(original){const svg=original.cloneNode(true);canvas.append(svg);
      const saved=layouts.get(key);for(const node of svg.querySelectorAll('[data-node-id]')) {
        const p=saved?.get(node.dataset.nodeId);if(p)moveNode(node,p.x,p.y);
      }
      updateEdges(svg);
    }fit(true);
  }
  function reset(){if(key && layouts.delete(key))layoutChanged();restoreView();}
  function moveNode(node,x,y){
    if(node.dataset.baseX===undefined){node.dataset.baseX=node.dataset.x;node.dataset.baseY=node.dataset.y;}
    node.dataset.x=x;node.dataset.y=y;
    node.setAttribute('transform',`translate(${x-Number(node.dataset.baseX)},${y-Number(node.dataset.baseY)})`);
  }
  function finishDrag(){if(drag?.moved && key){
    if(!layouts.has(key))layouts.set(key,new Map());
    layouts.get(key).set(drag.node.dataset.nodeId,{x:Number(drag.node.dataset.x),y:Number(drag.node.dataset.y)});
    layoutChanged();
  }drag=null;}
  el('graph-fit').addEventListener('click',()=>fit(true));
  el('graph-actual').addEventListener('click',()=>fit(false));
  el('graph-reset').addEventListener('click',reset);
  el('graph-close').addEventListener('click',()=>dialog.close());
  dialog.addEventListener('close',()=>{finishDrag();trigger?.focus();});
  const inspect=event=>{const node=event.target.closest('[data-details]');if(node)el('expanded-graph-details').textContent=node.dataset.details;};
  canvas.addEventListener('click',inspect);canvas.addEventListener('focusin',inspect);
  canvas.addEventListener('pointerdown',event=>{
    const node=event.target.closest('[data-node-id]');if(!node || event.button!==0)return;
    const svg=canvas.querySelector('svg');
    const point=new DOMPoint(event.clientX,event.clientY).matrixTransform(svg.getScreenCTM().inverse());
    drag={node,svg,x:point.x,y:point.y,startX:Number(node.dataset.x),startY:Number(node.dataset.y)};
    canvas.setPointerCapture(event.pointerId);event.preventDefault();
  });
  canvas.addEventListener('pointermove',event=>{
    if(!drag)return;
    const {node,svg}=drag;
    const point=new DOMPoint(event.clientX,event.clientY).matrixTransform(svg.getScreenCTM().inverse());
    const x=drag.startX+point.x-drag.x,y=drag.startY+point.y-drag.y;
    moveNode(node,x,y);drag.moved=true;updateEdges(svg);
  });
  function updateEdges(svg){
    const positions=new Map([...svg.querySelectorAll('[data-node-id]')].map(n=>[n.dataset.nodeId,{x:Number(n.dataset.x),y:Number(n.dataset.y)}]));
    for(const edge of svg.querySelectorAll('[data-from]')){
      const a=positions.get(edge.dataset.from),b=positions.get(edge.dataset.to);
      if(edge.tagName.toLowerCase()==='text'){edge.setAttribute('x',(a.x+b.x)/2+7);edge.setAttribute('y',(a.y+b.y)/2);continue;}
      const vertical=edge.dataset.direction==='TB',sign=vertical?(b.y>=a.y?1:-1):(b.x>=a.x?1:-1);
      edge.setAttribute('d',vertical
        ?`M${a.x},${a.y+sign*23} C${a.x},${a.y+sign*65} ${b.x},${b.y-sign*65} ${b.x},${b.y-sign*29} M${b.x-4},${b.y-sign*35} L${b.x},${b.y-sign*29} L${b.x+4},${b.y-sign*35}`
        :`M${a.x+sign*80},${a.y} L${b.x-sign*86},${b.y} M${b.x-sign*92},${b.y-4} L${b.x-sign*86},${b.y} L${b.x-sign*92},${b.y+4}`);
    }
  }
  for(const event of ['pointerup','pointercancel','lostpointercapture'])canvas.addEventListener(event,finishDrag);
  sharedViewer={open(source,title,button){original=source.querySelector('svg')?.cloneNode(true);key=original?.dataset.layoutKey;trigger=button;el('expanded-graph-title').textContent=title;el('expanded-graph-details').textContent='';restoreView();dialog.showModal();}};
  return sharedViewer;
}
