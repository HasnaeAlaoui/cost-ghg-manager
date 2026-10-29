# Prepare inputs for the recursive variances in 11_variance_core.R.
# Variance results are recalculated from the production observations.
calculate_variance_json <- function(orderID) {
  scope <- new.env(parent=environment(calculate_accounting_json))
  scope$Accounting.TBL <- NULL; scope$AccountingDetail.MCS <- NULL
  scope$Accounting.MCS <- NULL; scope$.accounting_cache <- NULL
  calculate <- calculate_accounting_json; environment(calculate) <- scope
  calculate(orderID, TRUE)
  input <- scope$Accounting.TBL; nodes <- scope$AccountingDetail.MCS
  input$aEstd <- list(input$rEstd[[1]] / nodes$itemQuantity)
  input$rEadj <- list(input$aEstd[[1]] * nodes$itemQuantity)
  result <- nodes[c("orderID","nodeID","fpID","itemID","routeID","bomID","chilNodeID","itemQuantity")]
  # Display the route standard beside the quantity-adjusted accounting inputs.
  result$rEstd <- input$aEstd[[1]]; result$rEadj <- input$rEadj[[1]]
  result$rEact <- input$rEact[[1]]
  for(parameter in c("powEstd","powEact","emEstd","emEact"))result[[parameter]] <- input[[parameter]][[1]]
  result$ECadj <- (result$rEadj / 60) * result$powEstd
  result$ECact <- (result$rEact / 60) * result$powEact
  functions <- list(PACEV=sumPACEV,PACPV=sumPACPV,PDCEV=sumPDCEV,PDCPV=sumPDCPV,
    PACFEmV=sumPACFEmV,PACFEnyV=sumPACFEnyV,PECFEV=sumPECFEV,
    PECFELV=sumPECFELV,PMCFEV=sumPMCFEV,PMCFELV=sumPMCFELV)
  for(metric in names(functions))result[[metric]] <- vapply(result$nodeID,function(root)functions[[metric]](input,root),numeric(1))
  # Combine variance components as in baseline 4.
  result <- transform(result,PACFBV=PACEV+PACPV,PDCFBV=PDCEV+PDCPV,
    PCEV=PACEV+PDCEV,PCPV=PACPV+PDCPV,PACFTV=PACFEmV+PACFEnyV,
    PECFTV=PECFEV+PECFELV,PMCFTV=PMCFEV+PMCFELV)
  result <- transform(result,PCFBV=PCEV+PCPV,PCFintTV=PACFTV,PCFextTV=PECFTV+PMCFTV)
  result <- transform(result,PCFTV=PCFintTV+PCFextTV)
  cost <- c("PACEV","PACPV","PACFBV","PDCEV","PDCPV","PDCFBV","PCEV","PCPV","PCFBV")
  ghg <- c("PACFEmV","PACFEnyV","PACFTV","PECFEV","PECFELV","PECFTV","PMCFEV","PMCFELV","PMCFTV","PCFintTV","PCFextTV","PCFTV")
  if(any(!is.finite(as.matrix(result[c(cost,ghg)]))))stop("Incomplete variance inputs.")
  summary <- scope$Accounting.MCS[c("orderID","fpID","fpName","orderQuantity","rootNodeID")]
  roots <- result$nodeID %in% summary$rootNodeID[[1]]
  for(metric in c(cost,ghg))summary[[metric]] <- sum(result[[metric]][roots])
  table <- function(x)list(columns=names(x),rows=production_rows(x))
  projection <- function(metrics, inputs)list(summary=table(summary[c("orderID","fpID","orderQuantity","rootNodeID",metrics)]),
    detail=table(result[c("orderID","nodeID","fpID","itemID","routeID","bomID","chilNodeID","itemQuantity",inputs,metrics)]))
  # Return the calculated cost and GHG variances.
  project_json(list(cost=projection(cost,c("rEstd","rEact")),
    ghg=projection(ghg,c("rEstd","rEact","powEstd","powEact","emEstd","emEact","ECact"))))
}
