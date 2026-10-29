# Baseline 1. AIS_SCM_ExistingData.Rmd:641-735, adapted from its pipelines.
# Expand order references and initialize actual quantities from routing and BOM standards.
empty_activity_response <- function() data.frame(jobID=integer(),orderID=integer(),fpID=integer(),
  nodeID=integer(),chilNodeID=I(list()),itemID=integer(),routeID=integer(),bomID=I(list()),
  rEact=double(),powEact=double(),rMact=I(list()),emEact=double())
ActyResponse.ECSI <- empty_activity_response()
.last_activity_response_id <- 0L
createActyResponse <- function(order,routes,bom,startID=0) {
  rows <- lapply(seq_along(order$nodeID[[1]]),function(pos) {
    routeID <- order$routeID[[1]][pos]
    route <- routes[master_position(routes,"routeID",routeID),,drop=FALSE]
    bomIDs <- order$bomID[[1]][[pos]]
    if(any(!bomIDs %in% bom$bomID)) stop("Response BOM references are missing.")
    data.frame(jobID=startID+pos,orderID=order$orderID,fpID=order$fpID,
      nodeID=order$nodeID[[1]][pos],chilNodeID=I(list(order$chilNodeID[[1]][[pos]])),
      itemID=order$itemID[[1]][pos],routeID=routeID,bomID=I(list(bomIDs)),
      rEact=route$rEstd,powEact=route$powEstd,
      rMact=I(list(bom$rMstd[match(bomIDs,bom$bomID)])))
  })
  if(length(rows))do.call(rbind,rows) else empty_activity_response()
}
# Keep orders with actual observations from being changed or regenerated.
assert_no_activity_response <- function(orderID) {
  pos <- master_position(ProductionOrder.ECSI,"orderID",orderID)
  if(ProductionOrder.ECSI$orderStatus[pos]=="Completed") stop("Completed orders cannot be edited, deleted or regenerated.")
  if(any(ActyResponse.ECSI$orderID==orderID)) stop("Actual Activity Response records exist for this order. Editing, deletion and regeneration are blocked to preserve observations.")
}
response_generation <- function(orderID,records=.production_generations) {
  record <- Filter(function(x)x$orderID==orderID,records)
  if(length(record)!=1L) stop("Generate ProNodeTree for this order first.")
  record[[1]]
}
# Baseline 3:254-274 extends the baseline 1 response with emEact. Energy
# identity comes from the occurrence's saved route, never from row position.
response_energy_standard <- function(orderID,routeID,records=.production_generations,energy=ENY.TBL) {
  routes <- response_generation(orderID,records)$definition$Routing.ECSI
  route <- routes[master_position(routes,"routeID",routeID),,drop=FALSE]
  row <- energy[master_position(energy,"enyID",route$enyID),,drop=FALSE]
  validate_actual(row$emEstd,"Standard emEstd")
  row
}
execute_production_order <- function(orderID) {
  pos <- master_position(ProductionOrder.ECSI,"orderID",orderID)
  if(ProductionOrder.ECSI$orderStatus[pos]=="Completed") stop("Completed orders cannot be executed again.")
  if(ProductionOrder.ECSI$orderStatus[pos]!="Planned") stop("Only Planned orders can be executed.")
  record <- response_generation(orderID)
  if(any(ActyResponse.ECSI$orderID==orderID)) stop("Activity Response records already exist; execution cannot be repeated.")
  added <- createActyResponse(ProductionOrder.ECSI[pos,,drop=FALSE],record$definition$Routing.ECSI,record$definition$BOM.ECSI,.last_activity_response_id)
  if(!nrow(added)) stop("Generate ProNodeTree first.")
  added$emEact <- vapply(added$routeID,function(id)response_energy_standard(orderID,id)$emEstd,numeric(1))
  if(max(added$jobID)>=.Machine$integer.max) stop("Activity Response ID limit reached.")
  candidate <- rbind(ActyResponse.ECSI,added)
  validate_activity_responses(candidate,ProductionOrder.ECSI,ProNodeTree.ECSI,.production_generations)
  orders <- ProductionOrder.ECSI; orders$orderStatus[pos] <- "Completed"
  result <- project_json(list(ok=TRUE,existing=FALSE,orderStatus="Completed"))
  ActyResponse.ECSI <<- candidate; .last_activity_response_id <<- max(added$jobID)
  ProductionOrder.ECSI <<- orders
  result
}
validate_actual <- function(value,label) {
  if(!is.numeric(value)||length(value)!=1L||!is.finite(value)||value<0) stop(paste(label,"must be a finite non-negative observation."))
}
update_activity_response <- function(orderID,nodeID,serialized_observations) {
  pos <- which(ActyResponse.ECSI$orderID==orderID & ActyResponse.ECSI$nodeID==nodeID)
  if(length(pos)!=1L) stop("Initialize an actual response for this order/node first.")
  values <- jsonlite::fromJSON(serialized_observations,simplifyVector=FALSE)
  project_keys(values,c("rEact","powEact","materials","emEact"),"actual observations")
  validate_actual(values$rEact,"rEact"); validate_actual(values$powEact,"powEact")
  validate_actual(values$emEact,"emEact")
  if(!is.list(values$materials)||!is.null(names(values$materials))) stop("Invalid material observations.")
  ids <- numeric(); actual <- numeric()
  for(material in values$materials) {
    project_keys(material,c("bomID","rMact"),"actual BOM observation")
    if(!project_integer(material$bomID,1)) stop("Invalid BOM observation ID.")
    validate_actual(material$rMact,"rMact");ids <- c(ids,material$bomID);actual <- c(actual,material$rMact)
  }
  expected <- ActyResponse.ECSI$bomID[[pos]]
  if(anyDuplicated(ids)||length(ids)!=length(expected)||!setequal(ids,expected)) stop("Actual BOM inputs must match this production node's BOM references exactly.")
  candidate <- ActyResponse.ECSI
  candidate$rEact[pos] <- values$rEact;candidate$powEact[pos] <- values$powEact
  candidate$rMact[pos] <- I(list(actual[match(expected,ids)]))
  candidate$emEact[pos] <- values$emEact
  validate_activity_responses(candidate,ProductionOrder.ECSI,ProNodeTree.ECSI,.production_generations)
  result <- project_json(list(ok=TRUE));ActyResponse.ECSI <<- candidate;result
}
validate_activity_responses <- function(responses,orders,tree,records) {
  if(anyDuplicated(responses$jobID)||anyDuplicated(paste(responses$orderID,responses$nodeID))) stop("Duplicate Activity Response production node.")
  for(orderID in unique(responses$orderID)) {
    pos <- master_position(orders,"orderID",orderID);record <- response_generation(orderID,records)
    own <- responses[responses$orderID==orderID,,drop=FALSE]
    expected <- createActyResponse(orders[pos,,drop=FALSE],record$definition$Routing.ECSI,record$definition$BOM.ECSI)
    if(!setequal(own$nodeID,expected$nodeID)||nrow(own)!=nrow(expected)) stop("Activity Response must cover exactly the generated order's nodes.")
    expected <- expected[match(own$nodeID,expected$nodeID),,drop=FALSE]
    references <- c("orderID","fpID","nodeID","chilNodeID","itemID","routeID","bomID")
    if(!identical(project_json(production_rows(own[references])),project_json(production_rows(expected[references])))) stop("Activity Response identities do not match generated production nodes.")
    if(any(!own$nodeID %in% tree$nodeID)) stop("Unknown production node.")
    for(i in seq_len(nrow(own))) {
      validate_actual(own$rEact[i],"rEact");validate_actual(own$powEact[i],"powEact")
      validate_actual(own$emEact[i],"emEact")
      if(length(own$rMact[[i]])!=length(own$bomID[[i]])) stop("Actual material vector length mismatch.")
      for(value in own$rMact[[i]])validate_actual(value,"rMact")
    }
  }
  invisible(TRUE)
}
get_activity_response_json <- function(orderID=0,nodeID=0) {
  available <- ProductionOrder.ECSI[ProductionOrder.ECSI$orderID %in% vapply(.production_generations,function(x)x$orderID,numeric(1)),,drop=FALSE]
  requests <- lapply(seq_len(nrow(available)),function(i)list(value=available$orderID[i],label=paste("Order",available$orderID[i],"-",FP.ECSI$fpName[match(available$fpID[i],FP.ECSI$fpID)],"x",available$orderQuantity[i],"—",available$orderStatus[i])))
  rows <- ActyResponse.ECSI[ActyResponse.ECSI$orderID==orderID,,drop=FALSE]
  nodes <- list();detail <- NULL
  if(orderID %in% available$orderID) {
    record <- response_generation(orderID);definition <- record$definition
    own <- ProNodeTree.ECSI[ProNodeTree.ECSI$nodeID %in% available$nodeID[[match(orderID,available$orderID)]],,drop=FALSE]
    nodes <- lapply(seq_len(nrow(own)),function(i)list(value=own$nodeID[i],label=paste0("#",own$nodeID[i]," ",definition$ITEM.TBL$itemName[match(own$itemID[i],definition$ITEM.TBL$itemID)])))
    if(nodeID %in% rows$nodeID) {
      row <- rows[rows$nodeID==nodeID,,drop=FALSE]
      route <- definition$Routing.ECSI[match(row$routeID,definition$Routing.ECSI$routeID),,drop=FALSE]
      bom <- definition$BOM.ECSI[match(row$bomID[[1]],definition$BOM.ECSI$bomID),,drop=FALSE]
      energy <- response_energy_standard(orderID,row$routeID)
      detail <- list(response=production_rows(row)[[1]],itemName=definition$ITEM.TBL$itemName[match(row$itemID,definition$ITEM.TBL$itemID)],
        emEstd=energy$emEstd,uOMemE=energy$uOMemE,
        route=as.list(route[1,]),materials=lapply(seq_len(nrow(bom)),function(i)list(bomID=bom$bomID[i],name=bom$childItemName[i],
          type=source_category(definition$ITEM.TBL$erpCat)[match(bom$childItemID[i],definition$ITEM.TBL$itemID)],rMstd=bom$rMstd[i],matUOM=bom$matUOM[i],rMact=row$rMact[[1]][i])))
    }
  }
  context <- if(orderID %in% available$orderID) {
    order <- available[match(orderID,available$orderID),,drop=FALSE]
    list(orderID=order$orderID,fpName=FP.ECSI$fpName[match(order$fpID,FP.ECSI$fpID)],orderQuantity=order$orderQuantity,orderStart=order$orderStart,orderEnd=order$orderEnd,orderStatus=order$orderStatus)
  } else NULL
  project_json(list(context=context,requests=requests,nodes=nodes,orderStatus=if(orderID %in% available$orderID) available$orderStatus[match(orderID,available$orderID)] else "",initialized=nrow(rows)>0,detail=detail,table=list(columns=names(ActyResponse.ECSI),rows=production_rows(rows))))
}
restore_activity_response_state <- function(value,orders,production) {
  project_keys(value,c("lastJobID","rows"),"actual production state")
  if(!project_integer(value$lastJobID))stop("Invalid Activity Response counter.")
  if(!is.list(value$rows)||!is.null(names(value$rows)))stop("Invalid Activity Response rows.")
  table <- empty_activity_response()
  for(row in value$rows) {
    project_keys(row,names(table),"Activity Response row")
    for(column in c("jobID","orderID","fpID","nodeID","itemID","routeID"))if(!project_integer(row[[column]],1))stop("Invalid Activity Response ID.")
    validate_actual(row$emEact,"emEact")
    row$chilNodeID <- production_id_list(row$chilNodeID);row$bomID <- production_id_list(row$bomID)
    validate_actual(row$rEact,"rEact");validate_actual(row$powEact,"powEact")
    if(!is.list(row$rMact)||!is.null(names(row$rMact)))stop("Invalid actual material array.")
    for(value_actual in row$rMact)validate_actual(value_actual,"rMact")
    row$rMact <- if(length(row$rMact))unlist(row$rMact,use.names=FALSE) else numeric()
    parsed <- as.data.frame(row[c("jobID","orderID","fpID","nodeID","itemID","routeID","rEact","powEact","emEact")])
    for(column in c("chilNodeID","bomID","rMact"))parsed[[column]] <- I(list(row[[column]]))
    table <- rbind(table,parsed[names(table)])
  }
  validate_activity_responses(table,orders,production$tree,production$records)
  if(value$lastJobID<max(c(0,table$jobID)))stop("Activity Response counter below allocated IDs.")
  list(table=table,lastJobID=value$lastJobID)
}
assert_actual_master_unreferenced <- function(kind,id) {
  for(orderID in unique(ActyResponse.ECSI$orderID)) {
    definition <- response_generation(orderID)$definition
    rows <- ActyResponse.ECSI[ActyResponse.ECSI$orderID==orderID,,drop=FALSE]
    routes <- definition$Routing.ECSI[definition$Routing.ECSI$routeID %in% rows$routeID,,drop=FALSE]
    if(kind %in% c("MAT","WIP","FP")) {
      ids <- definition$ITEM.TBL$itemID[source_category(definition$ITEM.TBL$erpCat)==kind & definition$ITEM.TBL$erpID==id]
      bom <- definition$BOM.ECSI[definition$BOM.ECSI$bomID %in% unlist(rows$bomID),,drop=FALSE]
      used <- any(c(rows$itemID,bom$childItemID) %in% ids)
    } else {
      column <- switch(kind,ACTY="actyID",EQIP="eqipID",ENY="enyID")
      used <- !is.null(column) && any(routes[[column]]==id)
    }
    if(used)stop("Master record is referenced by actual production observations.")
  }
}
