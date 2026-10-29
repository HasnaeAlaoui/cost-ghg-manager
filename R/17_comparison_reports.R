# Compare two orders using their accounting and variance results.
get_comparison_report_json <- function(leftOrderID,rightOrderID) {
  if(!project_integer(leftOrderID,1)||!project_integer(rightOrderID,1)||leftOrderID==rightOrderID)
    stop("Select two different production orders.")
  reports <- lapply(c(leftOrderID,rightOrderID),function(id)
    jsonlite::fromJSON(get_report_json(id),simplifyVector=FALSE))
  summaries <- lapply(reports,function(report)report$accounting)
  rows <- function(spec,group=NULL) lapply(spec,function(s) {
    values <- vapply(seq_along(reports),function(i) {
      if(!is.null(group))return(reports[[i]]$variance[[group]]$summary$rows[[1]][[s[1]]])
      a <- summaries[[i]]
      if(s[1]=="costDifference")return(a$PCact-a$PCstd)
      if(s[1]=="pcfDifference")return(a$PCFact-a$PCFstd)
      a[[s[1]]]
    },numeric(1))
    list(metric=s[1],label=s[2],unit=s[3],left=values[1],right=values[2],
      leftUnit=values[1]/summaries[[1]]$orderQuantity,rightUnit=values[2]/summaries[[2]]$orderQuantity)
  })
  cost <- rows(list(c("PCstd","Standard production cost","EUR"),c("PCact","Actual production cost","EUR"),c("costDifference","Actual vs. Standard full cost","EUR")))
  cost <- c(cost,rows(list(c("PCFBV","Analytical flexible-budget variance (PCFBV)","EUR")),"cost"))
  ghg <- rows(list(c("PCFstd","Standard PCF","kgCO2e"),c("PCFact","Actual PCF","kgCO2e"),c("pcfDifference","Actual vs. Standard PCF","kgCO2e")))
  ghg <- c(ghg,rows(list(c("PCFTV","Analytical total GHG variance (PCFTV)","kgCO2e")),"ghg"))
  costDrivers <- rows(list(c("PACEV","Activity efficiency","EUR"),c("PACPV","Activity price","EUR"),
    c("PDCEV","Material efficiency (including WIP quantity effects)","EUR"),c("PDCPV","Material price","EUR")),"cost")
  ghgDrivers <- rows(list(c("PACFEmV","Energy emission-factor effect","kgCO2e"),c("PACFEnyV","Energy consumption effect","kgCO2e"),
    c("PECFEV","Equipment efficiency","kgCO2e"),c("PECFELV","Equipment emission-liability effect","kgCO2e"),
    c("PMCFEV","Material efficiency (including WIP quantity effects)","kgCO2e"),c("PMCFELV","Material emission-liability effect","kgCO2e")),"ghg")
  project_json(list(projectName=.project_name,orders=lapply(summaries,function(a)a[c("orderID","fpID","fpName","orderQuantity","rootNodeID")]),
    cost=cost,ghg=ghg,costDrivers=costDrivers,ghgDrivers=ghgDrivers))
}

get_comparison_nodes_json <- function(orderID) {
  data <- jsonlite::fromJSON(get_node_variance_json(orderID),simplifyVector=FALSE)
  # Suppress floating-point noise only; input changes are retained even if their
  # net cost/GHG effect cancels. Never pair nodes between different orders.
  nonzero <- function(value)is.numeric(value)&&length(value)==1L&&is.finite(value)&&abs(value)>1e-10
  changed <- function(detail) {
    any(vapply(detail$results$rows,function(row)nonzero(row$costDifference)||nonzero(row$pcfDifference),logical(1))) ||
      any(vapply(c(detail$inputs$rows,detail$materials$rows),function(row)nonzero(row$difference),logical(1))) ||
      any(vapply(c(detail$analyticalCost$rows,detail$analyticalGHG$rows),function(row)nonzero(row$value),logical(1)))
  }
  total <- length(data$details)
  data$details <- Filter(changed,data$details)
  ids <- vapply(data$details,function(row)row$nodeID,numeric(1))
  data$nodes$rows <- Filter(function(row)row$nodeID %in% ids,data$nodes$rows)
  data$totalCount <- total;data$deviationCount <- length(ids)
  project_json(data)
}
