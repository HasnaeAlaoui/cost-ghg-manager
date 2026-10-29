# Prepare standard inputs for the recursive functions in 06_standard_core.R.
# Input preparation: baseline 2:234-390,806-864; baseline 3:140-182.
get_standard_context_json <- function() {
  data <- jsonlite::fromJSON(get_activity_response_json(),simplifyVector=FALSE)
  project_json(list(requests=data$requests,resultOrder=if(exists(".accounting_cache")&&!is.null(.accounting_cache)) .accounting_cache$orderID else 0))
}
calculate_standard_json <- function(orderID) {
  order <- ProductionOrder.ECSI[master_position(ProductionOrder.ECSI,"orderID",orderID),,drop=FALSE]
  definition <- response_generation(orderID)$definition
  nodes <- ProNodeTree.ECSI[match(order$nodeID[[1]],ProNodeTree.ECSI$nodeID),,drop=FALSE]
  if(!nrow(nodes)||anyNA(nodes$nodeID))stop("Generate the selected production order first.")
  routes <- definition$Routing.ECSI[match(nodes$routeID,definition$Routing.ECSI$routeID),,drop=FALSE]
  equipment <- EQIP.TBL[match(routes$eqipID,EQIP.TBL$eqipID),,drop=FALSE]
  energy <- ENY.TBL[match(routes$enyID,ENY.TBL$enyID),,drop=FALSE]
  if(anyNA(equipment$eqipID)||anyNA(energy$enyID))stop("Referenced accounting master data is missing.")
  boms <- lapply(nodes$bomID,function(ids)definition$BOM.ECSI[match(ids,definition$BOM.ECSI$bomID),,drop=FALSE])
  # Material prices and emission parameters belong to the existing MAT.TBL master.
  materials <- lapply(boms,function(b){
    ids <- definition$ITEM.TBL[match(b$childItemID,definition$ITEM.TBL$itemID),,drop=FALSE]
    idx <- match(ids$erpID,MAT.TBL$matID);idx[source_category(ids$erpCat)!="MAT"] <- NA_integer_
    if(any(source_category(ids$erpCat)=="MAT" & is.na(idx)))stop("Referenced material accounting data is missing.")
    m <- MAT.TBL[idx,,drop=FALSE]
    purchased <- source_category(ids$erpCat)=="MAT"
    unit <- sub("/unit$","",b$matUOM)
    if(any(m$uOMqM[purchased]!=paste0("EUR/",unit[purchased])) ||
       any(m$uOMuMEL[purchased]!=paste0("kgCO2e/",unit[purchased])))stop("Material reference units do not match BOM units; no conversion is defined in the baseline.")
    m
  })
  # Standard activity time for the production-node quantity: rEstd * itemQuantity.
  input <- list(nodeID=list(nodes$nodeID),chilNodeID=list(nodes$chilNodeID),itemID=list(nodes$itemID),
    rEstd=list(routes$rEstd * nodes$itemQuantity),powEstd=list(routes$powEstd),
    pEstd=list(equipment$pEstd),qEstd=list(equipment$qEstd),
    childItemID=list(lapply(boms,function(b)b$childItemID)),rMstd=list(lapply(boms,function(b)b$rMstd)),
    qMstd=list(lapply(materials,function(m)m$qMstd)),uMELstd=list(lapply(materials,function(m)m$uMELstd)),
    emEstd=list(energy$emEstd),cuEELstd=list(equipment$cuEELstd))
  # Calculate recursive cost and GHG results from each production node.
  result <- data.frame(orderID=orderID,nodeID=nodes$nodeID,fpID=order$fpID,itemID=nodes$itemID,routeID=nodes$routeID,
    bomID=I(nodes$bomID),chilNodeID=I(nodes$chilNodeID),itemQuantity=nodes$itemQuantity)
  functions <- list(PACstd=sumPACstd,PACvstd=sumPACvstd,PDCstd=sumPDCstd,
    PACFstd=sumPACFstd,PECFstd=sumPECFstd,PMCFstd=sumPMCFstd)
  for(metric in names(functions))result[[metric]] <- vapply(nodes$nodeID,function(root)functions[[metric]](input,root),numeric(1))
  # Standard cost and GHG totals (baseline 4).
  result <- transform(result,PACfstd=PACstd-PACvstd,PCstd=PACstd+PDCstd,PCvstd=PACvstd+PDCstd,
    PCFintstd=PACFstd,PCFextstd=PECFstd+PMCFstd)
  result <- transform(result,PCfstd=PACfstd,PCFstd=PCFintstd+PCFextstd,costUOM="EUR",ghgUOM="kgCO2e")
  if(any(!is.finite(as.matrix(result[names(functions)]))))stop("Incomplete standard accounting inputs.")
  payload <- project_json(list(table=list(columns=names(result),rows=production_rows(result))))
  # Store order totals, calculation inputs and node-level results.
  Accounting.MCS <<- accounting_order_summary(result,order,definition)
  Accounting.TBL <<- input
  AccountingDetail.MCS <<- result
  payload
}
