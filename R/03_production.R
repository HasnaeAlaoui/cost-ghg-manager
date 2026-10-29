# Adapted from baseline/1. AIS_SCM_ExistingData.Rmd:505-571.
# Same nested recursion/order/context/quantity rules; base-R collection primitives.
empty_pro_tree <- function() data.frame(nodeID=integer(),chilNodeID=I(list()),routeID=integer(),
  bomID=I(list()),fpID=integer(),itemID=integer(),itemQuantity=double())
createProNodeTree <- function(BOM.ECSI, Routing.ECSI, FP.ECSI) {
  nodes <- list()
  nodeID <- 0
  producedItems <- unique(Routing.ECSI$itemID)
  buildNode <- function(curItem, curFP, curQty = 1) {
    nodeID <<- nodeID + 1
    curNode <- nodeID
    route <- Routing.ECSI[Routing.ECSI$itemID == curItem &
      (is.na(Routing.ECSI$fpID) | Routing.ECSI$fpID == curFP), , drop=FALSE]
    if (nrow(route) != 1) stop(paste("Expected one routing for itemID", curItem))
    bom <- BOM.ECSI[BOM.ECSI$parentItemID == curItem &
      (is.na(BOM.ECSI$fpID) | BOM.ECSI$fpID == curFP), , drop=FALSE]
    bom <- bom[order(bom$bomID), , drop=FALSE]
    childBOM <- bom[bom$childItemID %in% producedItems, , drop=FALSE]
    pos <- length(nodes) + 1
    nodes[[pos]] <<- data.frame(nodeID=curNode,chilNodeID=I(list(integer(0))),
      routeID=route$routeID,bomID=I(list(bom$bomID)),fpID=curFP,itemID=curItem,itemQuantity=curQty)
    childNodes <- vapply(seq_len(nrow(childBOM)),function(i)
      as.integer(buildNode(childBOM$childItemID[i],curFP,curQty * childBOM$rMstd[i])),integer(1))
    nodes[[pos]]$chilNodeID <<- I(list(childNodes))
    curNode
  }
  for (i in seq_len(nrow(FP.ECSI))) buildNode(FP.ECSI$itemID[i],FP.ECSI$fpID[i],1)
  if (!length(nodes)) empty_pro_tree() else do.call(rbind,nodes)
}
# Baseline initial ProductionOrder schema (line 602), plus its generated enrichment (614).
ProductionOrder.ECSI <- data.frame(orderID=integer(),fpID=integer(),orderQuantity=double(),orderStart=character(),orderEnd=character(),orderStatus=character(),
  nodeID=I(list()),chilNodeID=I(list()),routeID=I(list()),bomID=I(list()),itemID=I(list()))
.last_order_id <- 0L
.last_node_id <- 0L
.production_generations <- list()
ProNodeTree.ECSI <- empty_pro_tree()
# Save the manufacturing definition used to generate production nodes.
production_definition <- function() list(BOM.ECSI=BOM.ECSI,Routing.ECSI=Routing.ECSI,
  ITEM.TBL=ITEM.TBL,FP.ECSI=source_master_rows(FP.ECSI),ACTY.ECSI=ACTY.ECSI,
  EQIP.TBL=EQIP.TBL[names(.eqip_identity_schema)],ENY.TBL=ENY.TBL[names(.eny_identity_schema)])
clear_production <- function() {
  ProNodeTree.ECSI <<- empty_pro_tree(); .last_node_id <<- 0L
  .production_generations <<- list()
  for(column in c("nodeID","chilNodeID","routeID","bomID","itemID")) ProductionOrder.ECSI[[column]] <<- I(rep(list(list()),nrow(ProductionOrder.ECSI)))
}
validate_order_quantity <- function(quantity) {
  structure_positive(quantity,"orderQuantity")
  if(quantity!=floor(quantity) || quantity>=.Machine$integer.max) stop("orderQuantity must be a positive whole number of FP units.")
}
validate_order_dates <- function(orderStart,orderEnd) {
  dates <- c(orderStart,orderEnd)
  if(length(dates)!=2L||anyNA(dates)||any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",dates))) stop("Order Start and Order End require YYYY-MM-DD dates.")
  parsed <- as.Date(dates,format="%Y-%m-%d")
  if(anyNA(parsed)||any(format(parsed,"%Y-%m-%d")!=dates))stop("Invalid order date.")
  if(parsed[2]<parsed[1])stop("Order End cannot precede Order Start.")
}
validate_orders <- function(orders, fps=FP.ECSI) {
  for(i in seq_len(nrow(orders))) {
    master_position(fps,"fpID",orders$fpID[i])
    validate_order_quantity(orders$orderQuantity[i])
    validate_order_dates(orders$orderStart[i],orders$orderEnd[i])
    if(!orders$orderStatus[i] %in% c("Planned","Completed")) stop("orderStatus must be Planned or Completed.")
  }
}
create_production_order <- function(fpID,orderQuantity,orderStart,orderEnd) {
  master_position(FP.ECSI,"fpID",fpID); validate_order_quantity(orderQuantity); validate_order_dates(orderStart,orderEnd)
  row <- data.frame(orderID=.last_order_id+1L,fpID=fpID,orderQuantity=orderQuantity,orderStart=orderStart,orderEnd=orderEnd,orderStatus="Planned",
    nodeID=I(list(list())),chilNodeID=I(list(list())),routeID=I(list(list())),bomID=I(list(list())),itemID=I(list(list())))
  ProductionOrder.ECSI <<- rbind(ProductionOrder.ECSI,row); .last_order_id <<- row$orderID
  get_production_json()
}
# Read and update production orders.
get_production_order_json <- function(orderID) {
  pos <- master_position(ProductionOrder.ECSI,"orderID",orderID)
  project_json(production_rows(ProductionOrder.ECSI[pos,,drop=FALSE]))
}
update_production_order <- function(orderID,fpID,orderQuantity,orderStart,orderEnd) {
  if(exists("assert_no_activity_response",mode="function")) assert_no_activity_response(orderID)
  pos <- master_position(ProductionOrder.ECSI,"orderID",orderID)
  master_position(FP.ECSI,"fpID",fpID); validate_order_quantity(orderQuantity); validate_order_dates(orderStart,orderEnd)
  changed <- ProductionOrder.ECSI$fpID[pos]!=fpID || ProductionOrder.ECSI$orderQuantity[pos]!=orderQuantity
  orders <- ProductionOrder.ECSI; tree <- ProNodeTree.ECSI; records <- .production_generations
  if(changed && length(ProductionOrder.ECSI$nodeID[[pos]])) stop("A generated order cannot change its finished product or quantity. Create a new production order.")
  if(changed) {
    tree <- tree[!tree$nodeID %in% orders$nodeID[[pos]],,drop=FALSE]
    orders <- order_references(orders,pos,empty_pro_tree())
    records <- Filter(function(record)record$orderID!=orderID,records)
    orders$fpID[pos] <- fpID; orders$orderQuantity[pos] <- orderQuantity
  }
  orders$orderStart[pos] <- orderStart;orders$orderEnd[pos] <- orderEnd
  response <- project_json(list(ok=TRUE,needsRegeneration=changed))
  ProductionOrder.ECSI <<- orders; ProNodeTree.ECSI <<- tree; .production_generations <<- records
  response
}
delete_production_order <- function(orderID) {
  if(exists("assert_no_activity_response",mode="function")) assert_no_activity_response(orderID)
  pos <- master_position(ProductionOrder.ECSI,"orderID",orderID)
  tree <- ProNodeTree.ECSI[!ProNodeTree.ECSI$nodeID %in% ProductionOrder.ECSI$nodeID[[pos]],,drop=FALSE]
  orders <- ProductionOrder.ECSI[-pos,,drop=FALSE]
  records <- Filter(function(record)record$orderID!=orderID,.production_generations)
  response <- project_json(list(ok=TRUE))
  ProductionOrder.ECSI <<- orders; ProNodeTree.ECSI <<- tree; .production_generations <<- records
  response
}
# Generate one tree per FP unit and offset node IDs to keep them unique.
# itemQuantity remains the quantity per FP unit.
order_occurrences <- function(order,definition,offset) {
  validate_order_quantity(order$orderQuantity)
  fp <- definition$FP.ECSI[definition$FP.ECSI$fpID==order$fpID,,drop=FALSE]
  if(nrow(fp)!=1L) stop("Order finished product is missing.")
  bom <- definition$BOM.ECSI; routes <- definition$Routing.ECSI
  items <- definition$ITEM.TBL
  fp$itemID <- item_for_entity("FP",order$fpID,items)$itemID # private recursion adapter only
  validate_structure(bom,routes,items,definition$ACTY.ECSI,definition$EQIP.TBL,definition$ENY.TBL)
  scoped <- bom[is.na(bom$fpID)|bom$fpID==order$fpID,,drop=FALSE]
  reached <- fp$itemID
  repeat {
    expanded <- union(reached,scoped$childItemID[scoped$parentItemID %in% reached])
    if(length(expanded)==length(reached)) break
    reached <- expanded
  }
  for(item in intersect(reached,items$itemID[source_category(items$erpCat) %in% c("WIP","FP")])) {
    effective <- routes[routes$itemID==item & (is.na(routes$fpID)|routes$fpID==order$fpID),,drop=FALSE]
    if(nrow(effective)!=1L) stop(paste("Expected one routing for itemID",item))
  }
  units <- vector("list",order$orderQuantity)
  for(unit in seq_len(order$orderQuantity)) {
    tree <- createProNodeTree(bom,routes,fp)
    if(any(!is.finite(tree$itemQuantity))) stop("Non-finite propagated quantity.")
    if(max(tree$nodeID)+offset>=.Machine$integer.max) stop("Production node ID limit reached.")
    tree$nodeID <- tree$nodeID+offset
    tree$chilNodeID <- I(lapply(tree$chilNodeID,function(ids)ids+offset))
    units[[unit]] <- tree; offset <- max(tree$nodeID)
  }
  do.call(rbind,units)
}
order_references <- function(orders,pos,tree) {
  for(column in c("nodeID","chilNodeID","routeID","bomID","itemID"))
    orders[[column]][pos] <- list(if(column %in% c("chilNodeID","bomID")) unclass(tree[[column]]) else tree[[column]])
  orders
}
generate_pro_node_tree <- function(orderID) {
  if(exists("assert_no_activity_response",mode="function")) assert_no_activity_response(orderID)
  pos <- master_position(ProductionOrder.ECSI,"orderID",orderID)
  if(length(ProductionOrder.ECSI$nodeID[[pos]]) || any(vapply(.production_generations,function(record)record$orderID==orderID,logical(1))))
    stop("ProNodeTree has already been generated for this production order.")
  definition <- production_definition()
  added <- order_occurrences(ProductionOrder.ECSI[pos,,drop=FALSE],definition,.last_node_id)
  retained <- ProNodeTree.ECSI[!ProNodeTree.ECSI$nodeID %in% ProductionOrder.ECSI$nodeID[[pos]],,drop=FALSE]
  candidate <- rbind(retained,added)
  orders <- order_references(ProductionOrder.ECSI,pos,added)
  record <- list(orderID=orderID,offset=.last_node_id,definition=definition)
  records <- Filter(function(record)record$orderID!=orderID,.production_generations)
  records[[length(records)+1L]] <- record
  # Prepare the graph before replacing this order's production nodes.
  graph <- production_tree_graph(added,definition)
  response <- project_json(list(ok=TRUE))
  ProNodeTree.ECSI <<- candidate; ProductionOrder.ECSI <<- orders
  .last_node_id <<- max(added$nodeID); .production_generations <<- records
  response
}
# Draw recorded production nodes and their material inputs.
production_tree_graph <- function(tree,definition=production_definition()) {
  BOM.ECSI <- definition$BOM.ECSI; Routing.ECSI <- definition$Routing.ECSI; ITEM.TBL <- definition$ITEM.TBL
  nodes <- list(); edges <- list(); levels <- setNames(rep(0L,nrow(tree)),tree$nodeID)
  add <- function(id,label,type,details,level) {
    nodes[[length(nodes)+1L]] <<- list(id=id,label=label,type=type,details=details,level=level,group=group)
  }
  edge <- function(from,to,label="") edges[[length(edges)+1L]] <<- list(from=from,to=to,label=label)
  groups <- setNames(rep(0L,nrow(tree)),tree$nodeID); group <- 0L
  for(i in seq_len(nrow(tree))) {
    if(groups[[as.character(tree$nodeID[i])]]==0L){group <- group+1L;groups[[as.character(tree$nodeID[i])]] <- group}
    group <- groups[[as.character(tree$nodeID[i])]]
    row <- tree[i, ]; id <- paste0("node:",row$nodeID); level <- levels[[as.character(row$nodeID)]]
    item <- ITEM.TBL[match(row$itemID,ITEM.TBL$itemID), ]; route <- Routing.ECSI[match(row$routeID,Routing.ECSI$routeID), ]
    add(id,paste0("#",row$nodeID," ",item$itemName),source_category(item$erpCat),
      paste("nodeID",row$nodeID,"| itemID",row$itemID,"| fpID",row$fpID,"| itemQuantity",row$itemQuantity,"per FP unit"),level)
    op <- paste0("node-route:",row$nodeID)
    add(op,route$actyName,"ACTY",paste("routeID",row$routeID,"|",route$eqipName,"|",route$enyName,
      "| rEstd",route$rEstd,route$eqipUOM,"| powEstd",route$powEstd,route$powUOM),level+1L)
    edge(op,id,paste(route$rEstd,route$eqipUOM))
    for(child in row$chilNodeID[[1]]) {
      levels[[as.character(child)]] <- level+2L
      groups[[as.character(child)]] <- group
      edge(paste0("node:",child),op)
    }
    for(bomID in row$bomID[[1]]) {
      bom <- BOM.ECSI[match(bomID,BOM.ECSI$bomID), ]; mat <- ITEM.TBL[match(bom$childItemID,ITEM.TBL$itemID), ]
      if(source_category(mat$erpCat)!="MAT") next
      leaf <- paste0("material:",row$nodeID,":",bomID)
      add(leaf,paste0(mat$itemName," [",row$nodeID,"/",bomID,"]"),"MAT",
        paste("Material display-only node | parent nodeID",row$nodeID,"| bomID",bomID,"| itemID",mat$itemID,
          "| rMstd",bom$rMstd,bom$matUOM,"| parent itemQuantity",row$itemQuantity),level+2L)
      edge(leaf,op,paste(bom$rMstd,bom$matUOM))
    }
  }
  lv <- vapply(nodes,function(n)n$level,integer(1)); gr <- vapply(nodes,function(n)n$group,integer(1))
  left <- 0
  for(group in unique(gr)) {
    indices <- which(gr==group); widest <- max(table(lv[indices]))
    for(i in indices) {
      nodes[[i]]$x <- left+110+((widest-sum(lv[indices]==lv[i]))/2+sum(lv[indices[indices<=i]]==lv[i])-1)*240
      nodes[[i]]$y <- 70+lv[i]*140; nodes[[i]]$level <- NULL; nodes[[i]]$group <- NULL
    }
    left <- left+widest*240+120
  }
  list(nodes=nodes,edges=edges,direction="TB")
}
production_rows <- function(table) {
  arrays <- function(value) if(is.list(value)) lapply(value,arrays) else I(value)
  lapply(seq_len(nrow(table)),function(i) {
    row <- lapply(table,function(column) if(is.list(column)) arrays(column[[i]]) else column[i])
    row
  })
}
get_production_json <- function(orderID=0) {
  tree <- empty_pro_tree(); graph <- list(nodes=list(),edges=list(),direction="TB"); roots <- 0L
  record <- Filter(function(record)record$orderID==orderID,.production_generations)
  if(length(record)) {
    pos <- master_position(ProductionOrder.ECSI,"orderID",orderID)
    tree <- ProNodeTree.ECSI[ProNodeTree.ECSI$nodeID %in% ProductionOrder.ECSI$nodeID[[pos]],,drop=FALSE]
    graph <- production_tree_graph(tree,record[[1]]$definition)
    roots <- length(setdiff(tree$nodeID,unlist(tree$chilNodeID)))
  }
  orderTable <- ProductionOrder.ECSI[c("orderID","fpID","orderQuantity","orderStart","orderEnd","orderStatus","routeID","bomID")]
  project_json(list(orders=list(columns=names(orderTable),rows=production_rows(orderTable)),
    products=lapply(seq_len(nrow(FP.ECSI)),function(i)list(value=FP.ECSI$fpID[i],label=FP.ECSI$fpName[i])),
    requests=lapply(seq_len(nrow(ProductionOrder.ECSI)),function(i)list(value=ProductionOrder.ECSI$orderID[i],
      label=paste("Order",ProductionOrder.ECSI$orderID[i],"-",FP.ECSI$fpName[match(ProductionOrder.ECSI$fpID[i],FP.ECSI$fpID)],"x",ProductionOrder.ECSI$orderQuantity[i],"—",ProductionOrder.ECSI$orderStatus[i]))),
    generatedOrder=if(length(record)) orderID else 0L,rootCount=roots,
    tree=list(columns=names(tree),rows=production_rows(tree)),graph=graph,
    globalTree=list(columns=names(ProNodeTree.ECSI),rows=production_rows(ProNodeTree.ECSI))))
}
