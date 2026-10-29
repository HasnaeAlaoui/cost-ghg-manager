# Order totals from finished-product roots; per-unit values use orderQuantity.
accounting_order_summary <- function(result, order, definition) {
  roots <- setdiff(result$nodeID,unlist(result$chilNodeID,use.names=FALSE))
  items <- definition$ITEM.TBL[match(result$itemID,definition$ITEM.TBL$itemID),,drop=FALSE]
  root_rows <- result[result$nodeID %in% roots & source_category(items$erpCat)=="FP" & items$erpID==order$fpID,,drop=FALSE]
  if(nrow(root_rows)!=order$orderQuantity)stop("Finished-product root count does not match the order quantity.")
  summary <- data.frame(orderID=order$orderID,fpID=order$fpID,
    fpName=FP.ECSI$fpName[match(order$fpID,FP.ECSI$fpID)],orderQuantity=order$orderQuantity,
    rootNodeID=I(list(root_rows$nodeID)))
  metrics <- intersect(c("PCstd","PCvstd","PCfstd","PCact","PCvact","PCfact","PCFstd","PCFact"),names(result))
  for(metric in metrics)summary[[metric]] <- sum(root_rows[[metric]])
  for(metric in intersect(c("PCstd","PCact","PCFstd","PCFact"),names(summary)))
    summary[[paste0(metric,"Unit")]] <- summary[[metric]] / order$orderQuantity
  summary$costUOM <- "EUR";summary$ghgUOM <- "kgCO2e"
  summary
}

# Compare the existing standard and actual order totals for a completed order.
get_order_comparison_json <- function(orderID) {
  order <- ProductionOrder.ECSI[master_position(ProductionOrder.ECSI,"orderID",orderID),,drop=FALSE]
  if(order$orderStatus!="Completed")stop("Execute the Production Order before comparing standard and actual results.")
  calculate_accounting_json(orderID,TRUE)
  project_json(list(orderID=orderID,summary=production_rows(Accounting.MCS)[[1]]))
}
calculate_accounting_json <- function(orderID,actual=FALSE) {
  previous <- if(!is.null(.accounting_cache) && .accounting_cache$orderID==orderID && identical(.accounting_cache$stamp,accounting_stamp()))
    list(detail=AccountingDetail.MCS,summary=Accounting.MCS) else NULL
  order <- ProductionOrder.ECSI[master_position(ProductionOrder.ECSI,"orderID",orderID),,drop=FALSE]
  definition <- response_generation(orderID)$definition
  # Calculate standard results separately before updating the accounting tables.
  scope <- new.env(parent=environment(calculate_standard_json))
  scope$Accounting.TBL <- NULL;scope$Accounting.MCS <- NULL;scope$AccountingDetail.MCS <- NULL
  standard <- calculate_standard_json;environment(standard) <- scope
  standard(orderID)
  result <- scope$AccountingDetail.MCS;input <- scope$Accounting.TBL
  if(actual) {
    responses <- ActyResponse.ECSI[ActyResponse.ECSI$orderID==orderID,,drop=FALSE]
    if(!nrow(responses))stop("Open Edit Activity Response and record Activity Response observations for this order first.")
    validate_activity_responses(responses,ProductionOrder.ECSI,ProNodeTree.ECSI,.production_generations)
    occurrence_key <- function(rows) paste(rows$orderID,rows$nodeID,sep=":")
    responses <- responses[match(occurrence_key(result),occurrence_key(responses)),,drop=FALSE]
    # Baseline 3:294-318: occurrence observations and rEact * itemQuantity.
    input$rEact <- list(responses$rEact * result$itemQuantity)
    input$powEact <- list(responses$powEact)
    input$rMact <- list(responses$rMact)
    # Baseline 3:344-348 actual reference parameters equal standard parameters.
    for(parameter in c("pE","qE","qM","cuEEL","uMEL"))input[[paste0(parameter,"act")]] <- input[[paste0(parameter,"std")]]
    # Baseline 3:254-300: the captured coefficient belongs to each occurrence.
    input$emEact <- list(responses$emEact)
    functions <- list(PACact=sumPACact,PACvact=sumPACvact,PDCact=sumPDCact,
      PACFact=sumPACFact,PECFact=sumPECFact,PMCFact=sumPMCFact)
    for(metric in names(functions))result[[metric]] <- vapply(result$nodeID,function(root)functions[[metric]](input,root),numeric(1))
    # Actual cost and GHG totals (baseline 4).
    result <- transform(result,PACfact=PACact-PACvact,PCact=PACact+PDCact,PCvact=PACvact+PDCact,
      PCFintact=PACFact,PCFextact=PECFact+PMCFact)
    result <- transform(result,PCfact=PACfact,PCFact=PCFintact+PCFextact)
    if(any(!is.finite(as.matrix(result[names(functions)]))))stop("Incomplete actual accounting inputs.")
  }
  if(!actual && !is.null(previous)) {
    for(column in grep("act$",names(previous$detail),value=TRUE))result[[column]] <- previous$detail[[column]][match(result$nodeID,previous$detail$nodeID)]
  }
  summary <- accounting_order_summary(result,order,definition)
  payload <- project_json(list(table=list(columns=names(result),rows=production_rows(result)),
    summary=list(columns=names(summary),rows=production_rows(summary))))
  Accounting.TBL <<- input;AccountingDetail.MCS <<- result;Accounting.MCS <<- summary
  .accounting_cache <<- list(orderID=orderID,mode=if(actual)"actual" else "standard",stamp=accounting_stamp())
  payload
}
