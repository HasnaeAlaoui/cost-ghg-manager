# Read saved production nodes and generation definitions.
production_id_list <- function(value,nested=FALSE) {
  if(!is.list(value) || !is.null(names(value))) stop("Production references must be arrays.")
  if(nested) return(lapply(value,production_id_list))
  if(!all(vapply(value,project_integer,logical(1),minimum=1))) stop("Invalid production reference ID.")
  if(!length(value)) numeric() else vapply(value,as.numeric,numeric(1))
}
read_production_table <- function(rows,template,label) {
  if(!is.list(rows) || !is.null(names(rows))) stop("Invalid production table rows.")
  result <- template[0,,drop=FALSE]
  for(row in rows) {
    project_keys(row,names(template),label)
    parsed <- template[NA_integer_,,drop=FALSE]
    for(column in names(template)) {
      value <- row[[column]]
      if(is.list(template[[column]])) parsed[[column]] <- I(list(production_id_list(value)))
      else if(is.null(value) && column=="fpID" && label %in% c("BOM.ECSI","Routing.ECSI")) parsed[[column]] <- NA_integer_
      else {
        if(is.character(template[[column]])) validate_master_text(value,column,!grepl("Description$",column))
        else if(grepl("ID$",column)) {if(!project_integer(value,1)) stop("Invalid snapshot ID.")}
        else if(!is.numeric(value)||length(value)!=1L||!is.finite(value)) stop("Invalid snapshot number.")
        parsed[[column]] <- value
      }
    }
    result <- rbind(result,parsed)
  }
  if(anyDuplicated(result[[names(template)[1]]])) stop("Duplicate production/snapshot ID.")
  rownames(result) <- NULL
  result
}
restore_production_state <- function(value,orders) {
  project_keys(value,c("lastNodeID","tree","generations"),"production state")
  if(!project_integer(value$lastNodeID)) stop("Invalid production node counter.")
  tree <- read_production_table(value$tree,empty_pro_tree(),"ProNodeTree.ECSI")
  if(!is.list(value$generations)||!is.null(names(value$generations))) stop("Invalid generation records.")
  records <- list(); expected <- empty_pro_tree(); expected_orders <- orders
  for(column in c("nodeID","chilNodeID","routeID","bomID","itemID")) expected_orders[[column]] <- I(rep(list(list()),nrow(orders)))
  for(record in value$generations) {
    project_keys(record,c("orderID","offset","definition"),"generation record")
    if(!project_integer(record$orderID,1)||!project_integer(record$offset)) stop("Invalid generation ID/offset.")
    pos <- master_position(orders,"orderID",record$orderID)
    if(record$orderID %in% vapply(records,function(x)x$orderID,numeric(1))) stop("Duplicate generated order.")
    templates <- production_definition()
    names_required <- names(templates)
    project_keys(record$definition,names_required,"generation definition")
    definition <- lapply(names_required,function(name)read_production_table(record$definition[[name]],templates[[name]],name))
    names(definition) <- names_required
    validate_item_identities(definition$ITEM.TBL)
    # Check saved nodes by regenerating them from the saved manufacturing definition,
    # not the current BOM and routing. Keep the open project unchanged during checks.
    own <- order_occurrences(orders[pos,,drop=FALSE],definition,record$offset)
    expected <- rbind(expected,own)
    expected_orders <- order_references(expected_orders,pos,own)
    records[[length(records)+1L]] <- list(orderID=record$orderID,offset=record$offset,definition=definition)
  }
  if(anyDuplicated(expected$nodeID)) stop("Production node IDs overlap between orders.")
  if(value$lastNodeID < max(c(0,tree$nodeID))) stop("Production node counter below allocated IDs.")
  if(!identical(project_json(production_rows(expected)),project_json(production_rows(tree)))) stop("Saved production nodes do not match generation records.")
  if(!identical(project_json(production_rows(expected_orders)),project_json(production_rows(orders)))) stop("ProductionOrder production node references do not match production state.")
  list(tree=tree,records=records,lastNodeID=value$lastNodeID)
}
