# Cost and GHG shares for the dashboard.

dashboard_percent <- function(value,total) if(is.finite(total)&&total>0)100*value/total else NA_real_

dashboard_rank <- function(values,labels,total) {

  keep <- order(-values,seq_along(values)); values <- values[keep];labels <- labels[keep]

  if(length(values)>8){labels <- c(labels[1:7],"Other");values <- c(values[1:7],sum(values[-(1:7)]))}

  lapply(seq_along(values),function(i)list(label=labels[i],value=values[i],percent=dashboard_percent(values[i],total)))

}

dashboard_analytics <- function(orderID,a) {

  scope <- new.env(parent=environment(calculate_accounting_json))

  scope$Accounting.TBL <- NULL;scope$Accounting.MCS <- NULL;scope$AccountingDetail.MCS <- NULL;scope$.accounting_cache <- NULL

  calculate <- calculate_accounting_json;environment(calculate) <- scope;calculate(orderID,TRUE)

  input <- scope$Accounting.TBL;nodes <- scope$AccountingDetail.MCS

  roots <- scope$Accounting.MCS$rootNodeID[[1]]

  # Keep one node's coefficients at a time to calculate its contribution.

  # Retain NA for WIP and all BOM quantities so recursion keeps the material weights.

  cost <- ghg <- numeric(nrow(nodes))

  for(i in seq_len(nrow(nodes))) {

    isolated <- input

    for(field in c("pEact","emEact","cuEELact"))isolated[[field]][[1]][-i] <- 0

    for(field in c("qMact","uMELact"))for(j in setdiff(seq_len(nrow(nodes)),i)) {

      x <- isolated[[field]][[1]][[j]];x[!is.na(x)] <- 0;isolated[[field]][[1]][[j]] <- x

    }

    cost[i] <- sum(vapply(roots,function(root)sumPACact(isolated,root)+sumPDCact(isolated,root),numeric(1)))

    ghg[i] <- sum(vapply(roots,function(root)sumPACFact(isolated,root)+sumPECFact(isolated,root)+sumPMCFact(isolated,root),numeric(1)))

  }

  # Check that contributions add up to the order totals.

  stopifnot(isTRUE(all.equal(sum(cost),a$PCact,tolerance=1e-9)),isTRUE(all.equal(sum(ghg),a$PCFact,tolerance=1e-9)))

  definition <- response_generation(orderID)$definition

  names <- definition$ITEM.TBL$itemName[match(nodes$itemID,definition$ITEM.TBL$itemID)]

  names[is.na(names)] <- paste("Item",nodes$itemID[is.na(names)])

  # Group contributions by itemID before ranking.
  items <- unique(nodes$itemID)
  labels <- names[match(items,nodes$itemID)]
  cost <- vapply(items,function(id)sum(cost[nodes$itemID==id]),numeric(1))
  ghg <- vapply(items,function(id)sum(ghg[nodes$itemID==id]),numeric(1))
  material <- sum(nodes$PMCFact[nodes$nodeID %in% roots])
  equipment <- sum(nodes$PECFact[nodes$nodeID %in% roots])
  stopifnot(isTRUE(all.equal(a$PCFintact+material+equipment,a$PCFact,tolerance=1e-9)))
  parts <- function(labels,values,total)lapply(seq_along(values),function(i)list(label=labels[i],value=values[i],percent=dashboard_percent(values[i],total)))

  list(costDifference=a$PCact-a$PCstd,ghgDifference=a$PCFact-a$PCFstd,
    costChange=dashboard_percent(a$PCact-a$PCstd,a$PCstd),ghgChange=dashboard_percent(a$PCFact-a$PCFstd,a$PCFstd),
    costComposition=parts(c("Activity Cost","Direct / Material Cost"),c(a$PACact,a$PDCact),a$PCact),

    ghgComposition=parts(c("Internal PCF","Material PCF","Equipment PCF"),c(a$PCFintact,material,equipment),a$PCFact),
    costContributors=dashboard_rank(cost,labels,a$PCact),ghgContributors=dashboard_rank(ghg,labels,a$PCFact))

}

# Collect current accounting and variance results for each order.

get_dashboard_json <- function(orderID=0) {

  orders <- lapply(seq_len(nrow(ProductionOrder.ECSI)),function(i) {

    order <- ProductionOrder.ECSI[i,,drop=FALSE]

    info <- list(orderID=order$orderID,fpID=order$fpID,

      fpName=FP.ECSI$fpName[match(order$fpID,FP.ECSI$fpID)],quantity=order$orderQuantity,

      generated=length(unlist(order$nodeID,use.names=FALSE))>0)

    if(!info$generated)return(c(info,list(ready=FALSE,status="Generate the production tree in Production Order.")))
    tryCatch({

      report <- jsonlite::fromJSON(get_report_json(order$orderID),simplifyVector=FALSE)

      g <- report$variance$ghg$summary$rows[[1]]
      stopifnot(isTRUE(all.equal(g$PCFintTV+g$PMCFTV+g$PECFTV,g$PCFTV,tolerance=1e-9)))
      c(info,list(ready=TRUE,status="",
        accounting=report$accounting,cost=report$variance$cost$summary$rows[[1]],

        ghg=report$variance$ghg$summary$rows[[1]],analytics=dashboard_analytics(order$orderID,report$accounting)))

    },error=function(e)c(info,list(ready=FALSE,status=conditionMessage(e))))

  })

  ids <- vapply(orders,function(x)x$orderID,numeric(1))

  if(!orderID %in% ids) {

    ready <- vapply(orders,function(x)isTRUE(x$ready),logical(1))

    orderID <- if(any(ready))tail(ids[ready],1) else if(length(ids))tail(ids,1) else 0

  }

  project_json(list(projectName=.project_name,counts=list(finishedProducts=nrow(FP.ECSI),

    orders=nrow(ProductionOrder.ECSI),nodes=nrow(ProNodeTree.ECSI)),

    empty=nrow(ITEM.TBL)==0 && nrow(ProductionOrder.ECSI)==0,

    selectedOrder=orderID,orders=orders))

}

