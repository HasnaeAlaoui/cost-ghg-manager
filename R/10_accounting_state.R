# Store and display calculated accounting results.
.accounting_cache <- NULL
accounting_stamp <- function() project_json(list(tables=mget(names(.project_templates),.GlobalEnv),
  tree=production_rows(ProNodeTree.ECSI),actuals=production_rows(ActyResponse.ECSI),definitions=.production_generations))
clear_accounting_state <- function() {
  for(n in c("AccountingDetail.MCS","Accounting.MCS","Accounting.TBL"))if(exists(n,.GlobalEnv,inherits=FALSE))rm(list=n,envir=.GlobalEnv)
  .accounting_cache <<- NULL
}
accounting_view_json <- function(orderID=0) {
  empty <- list(columns=list(),rows=list())
  if(is.null(.accounting_cache)||.accounting_cache$orderID!=orderID)return(project_json(list(table=empty,summary=empty,mode="",stale=FALSE)))
  mode <- .accounting_cache$mode
  project_table <- function(table) {
    excluded <- if(mode=="actual")"std(Unit)?$" else "act(Unit)?$"
    columns <- names(table)[!grepl(excluded,names(table))]
    list(columns=columns,rows=production_rows(table[columns]))
  }
  project_json(list(table=project_table(AccountingDetail.MCS),summary=project_table(Accounting.MCS),mode=mode,
    stale=isTRUE(.accounting_cache$loaded)||!identical(.accounting_cache$stamp,accounting_stamp())))
}
save_accounting_state <- function() {
  if(is.null(.accounting_cache))return(NULL)
  list(version=1,orderID=.accounting_cache$orderID,mode=.accounting_cache$mode,
    AccountingDetail.MCS=list(columns=names(AccountingDetail.MCS),rows=production_rows(AccountingDetail.MCS)),
    Accounting.MCS=list(columns=names(Accounting.MCS),rows=production_rows(Accounting.MCS)))
}
parse_accounting_state <- function(value) {
  if(is.null(value))return(NULL)
  project_keys(value,c("version","orderID","mode","AccountingDetail.MCS","Accounting.MCS"),"derived accounting")
  if(value$version!=1||!project_integer(value$orderID,1)||!value$mode %in% c("standard","actual"))stop("Invalid derived accounting metadata.")
  metrics <- c("PAC","PACv","PDC","PACF","PECF","PMCF","PACf","PC","PCv","PCFint","PCFext","PCf","PCF")
  allowed_metrics <- c(paste0(metrics,"std"),paste0(metrics,"act"),"PCstdUnit","PCactUnit","PCFstdUnit","PCFactUnit")
  parse <- function(v,detail) {
    project_keys(v,c("columns","rows"),"derived accounting table")
    columns <- unlist(v$columns,use.names=FALSE)
    ids <- if(detail)c("orderID","nodeID","fpID","itemID","routeID","bomID","chilNodeID","itemQuantity") else c("orderID","fpID","fpName","orderQuantity","rootNodeID")
    if(!is.character(columns)||anyDuplicated(columns)||!all(c(ids,"costUOM","ghgUOM") %in% columns)||any(!columns %in% c(ids,allowed_metrics,"costUOM","ghgUOM")))stop("Invalid derived accounting columns.")
    if(!is.list(v$rows)||!is.null(names(v$rows))||!length(v$rows))stop("Invalid derived accounting rows.")
    rows <- lapply(v$rows,function(row){
      project_keys(row,columns,"derived accounting row")
      lists <- intersect(columns,c("bomID","chilNodeID","rootNodeID"))
      text <- intersect(columns,c("fpName","costUOM","ghgUOM"))
      for(n in text)if(!is.character(row[[n]])||length(row[[n]])!=1)stop("Invalid derived text.")
      for(n in setdiff(columns,c(lists,text)))if(!is.numeric(row[[n]])||length(row[[n]])!=1||!is.finite(row[[n]]))stop("Invalid derived number.")
      if(row$orderID!=value$orderID)stop("Mixed derived order identities.")
      out <- as.data.frame(row[setdiff(columns,lists)],stringsAsFactors=FALSE)
      for(n in lists)out[[n]] <- I(list(production_id_list(row[[n]])))
      out[columns]
    })
    result <- do.call(rbind,rows)
    if((!detail&&nrow(result)!=1)||(detail&&anyDuplicated(result$nodeID)))stop("Invalid derived accounting granularity.")
    result
  }
  list(orderID=value$orderID,mode=value$mode,detail=parse(value$AccountingDetail.MCS,TRUE),summary=parse(value$Accounting.MCS,FALSE))
}
restore_accounting_state <- function(value) {
  clear_accounting_state()
  if(is.null(value))return(invisible(NULL))
  AccountingDetail.MCS <<- value$detail;Accounting.MCS <<- value$summary
  # Loaded results require recalculation.
  .accounting_cache <<- list(orderID=value$orderID,mode=value$mode,stamp=accounting_stamp(),loaded=TRUE)
}
