# Prepare report results using the accounting and variance functions.
get_report_json <- function(orderID) {
  scope <- new.env(parent=environment(calculate_accounting_json))
  scope$Accounting.TBL <- NULL; scope$AccountingDetail.MCS <- NULL
  scope$Accounting.MCS <- NULL; scope$.accounting_cache <- NULL
  calculate <- calculate_accounting_json; environment(calculate) <- scope
  calculate(orderID,TRUE)
  summary <- scope$Accounting.MCS; detail <- scope$AccountingDetail.MCS
  roots <- detail$nodeID %in% summary$rootNodeID[[1]]
  # Sum finished-product roots for order totals.
  fields <- c("PACstd","PACact","PDCstd","PDCact","PCFintstd","PCFintact","PCFextstd","PCFextact","PMCFact","PECFact")
  for(field in fields)summary[[field]] <- sum(detail[[field]][roots])
  variance <- jsonlite::fromJSON(calculate_variance_json(orderID),simplifyVector=FALSE)
  selected <- c("orderID","nodeID","itemID","routeID","PCstd","PCact","PCFstd","PCFact")
  project_json(list(projectName=.project_name,costDifference=summary$PCact-summary$PCstd,accounting=production_rows(summary)[[1]],
    detail=list(columns=selected,rows=production_rows(detail[selected])),variance=variance))
}
