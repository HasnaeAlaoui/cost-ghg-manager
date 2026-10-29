# Compare direct local and recursive cost and GHG results for each node.
get_node_variance_json <- function(orderID) {
  scope <- new.env(parent=environment(calculate_accounting_json))
  scope$Accounting.TBL <- NULL; scope$AccountingDetail.MCS <- NULL
  scope$Accounting.MCS <- NULL; scope$.accounting_cache <- NULL
  calculate <- calculate_accounting_json; environment(calculate) <- scope
  calculate(orderID,TRUE)
  input <- scope$Accounting.TBL; nodes <- scope$AccountingDetail.MCS
  summary <- scope$Accounting.MCS
  definition <- response_generation(orderID)$definition
  observations <- ActyResponse.ECSI[ActyResponse.ECSI$orderID==orderID,,drop=FALSE]
  key <- function(rows) paste(rows$orderID,rows$nodeID,sep=":")
  observations <- observations[match(key(nodes),key(observations)),,drop=FALSE]
  analytics <- jsonlite::fromJSON(calculate_variance_json(orderID),simplifyVector=FALSE)

  # Calculate direct contributions with child links removed from a copy of the inputs.
  # Keep own activity, equipment and purchased materials; child-derived WIP is zero.
  # Subtracting child totals would miss the parent BOM quantity weights.
  direct <- input
  direct$chilNodeID <- list(lapply(nodes$nodeID,function(id)integer()))
  local <- data.frame(nodeID=nodes$nodeID)
  for(mode in c("std","act")) {
    metric <- function(name) vapply(nodes$nodeID,function(id)
      get(paste0("sum",name,mode),mode="function")(direct,id),numeric(1))
    local[[paste0("PC",mode)]] <- metric("PAC") + metric("PDC")
    local[[paste0("PCF",mode)]] <- metric("PACF") + metric("PECF") + metric("PMCF")
  }
  difference <- function(rows) {
    rows$costDifference <- rows$PCact-rows$PCstd
    rows$pcfDifference <- rows$PCFact-rows$PCFstd
    rows
  }
  values <- c("PCstd","PCact","costDifference","PCFstd","PCFact","pcfDifference")
  recursive <- difference(nodes); local <- difference(local); summary <- difference(summary)
  recursive$itemName <- definition$ITEM.TBL$itemName[match(nodes$itemID,definition$ITEM.TBL$itemID)]
  table <- function(rows)list(columns=names(rows),rows=production_rows(rows))
  analytical_row <- function(group,nodeID) {
    rows <- Filter(function(row)row$orderID==orderID && row$nodeID==nodeID,analytics[[group]]$detail$rows)
    if(length(rows)!=1L)stop("Missing analytical variance production node.")
    row <- rows[[1]]
    metrics <- setdiff(names(row),c("orderID","nodeID","fpID","itemID","routeID","bomID","chilNodeID","itemQuantity","rEstd","rEadj","rEact","powEstd","powEact","emEstd","emEact","ECadj","ECact"))
    data.frame(metric=metrics,value=vapply(row[metrics],identity,numeric(1)),
      unit=if(group=="cost")"EUR" else "kgCO2e",row.names=NULL)
  }
  details <- lapply(seq_len(nrow(nodes)),function(i) {
    node <- nodes[i,,drop=FALSE]; response <- observations[i,,drop=FALSE]
    route <- definition$Routing.ECSI[master_position(definition$Routing.ECSI,"routeID",node$routeID),,drop=FALSE]
    bom <- definition$BOM.ECSI[match(node$bomID[[1]],definition$BOM.ECSI$bomID),,drop=FALSE]
    # Match actual material quantities by bomID, not row order.
    actual_materials <- response$rMact[[1]][match(bom$bomID,response$bomID[[1]])]
    materials <- data.frame(bomID=bom$bomID,childItemID=bom$childItemID,
      itemName=bom$childItemName,
      itemType=source_category(definition$ITEM.TBL$erpCat)[match(bom$childItemID,definition$ITEM.TBL$itemID)],
      rMstd=bom$rMstd,rMact=actual_materials,difference=actual_materials-bom$rMstd,unit=bom$matUOM)
    inputs <- data.frame(input=c("rEstd / rEact","powEstd / powEact","emEstd / emEact","Accounting execution time (rE * itemQuantity)"),
      standard=c(route$rEstd,route$powEstd,input$emEstd[[1]][i],input$rEstd[[1]][i]),
      actual=c(response$rEact,response$powEact,response$emEact,input$rEact[[1]][i]),
      unit=c(route$eqipUOM,route$powUOM,"kgCO2e/kWh","min"))
    inputs$difference <- inputs$actual-inputs$standard
    inputs <- inputs[c("input","standard","actual","difference","unit")]
    comparison <- rbind(data.frame(basis="Recursive",recursive[i,values,drop=FALSE]),
      data.frame(basis="Direct local",local[i,values,drop=FALSE]))
    list(orderID=orderID,nodeID=node$nodeID,jobID=response$jobID,fpID=node$fpID,
      itemID=node$itemID,itemName=recursive$itemName[i],routeID=node$routeID,
      itemQuantity=node$itemQuantity,orderQuantity=summary$orderQuantity,
      chilNodeID=as.list(node$chilNodeID[[1]]),
      results=table(comparison),inputs=table(inputs),materials=table(materials),
      analyticalCost=table(analytical_row("cost",node$nodeID)),
      analyticalGHG=table(analytical_row("ghg",node$nodeID)))
  })
  project_json(list(order=table(summary[c("orderID","fpID","orderQuantity","rootNodeID",values)]),
    nodes=table(recursive[c("nodeID","itemName",values)]),details=details))
}

# What-if observations and calculated tables exist only in this private R copy.
simulate_activity_response_json <- function(orderID,nodeID,serialized_observations) {
  engine <- environment(simulate_activity_response_json)
  simulation <- new.env(parent=engine)
  for(name in ls(engine,all.names=TRUE)) {
    value <- get(name,envir=engine,inherits=FALSE)
    if(is.function(value) && identical(environment(value),engine)) environment(value) <- simulation
    assign(name,value,envir=simulation)
  }
  simulation$Accounting.TBL <- NULL; simulation$AccountingDetail.MCS <- NULL
  simulation$Accounting.MCS <- NULL; simulation$.accounting_cache <- NULL
  simulation$update_activity_response(orderID,nodeID,serialized_observations)
  report <- jsonlite::fromJSON(simulation$get_report_json(orderID),simplifyVector=FALSE)
  cost <- report$variance$cost$summary$rows[[1]]
  # Reuse the authoritative accounting difference; FBV columns already total their components.
  cost$PCTV <- report$costDifference
  project_json(list(accounting=report$accounting,costDifference=report$costDifference,
    cost=cost,ghg=report$variance$ghg$summary$rows[[1]]))
}
