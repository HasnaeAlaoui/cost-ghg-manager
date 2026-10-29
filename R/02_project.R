# v0.1 format 12: generated ITEM.TBL and current model schemas.
.project_templates <- lapply(list(MAT.TBL = MAT.TBL, WIP.ECSI = WIP.ECSI,
  FP.ECSI = FP.ECSI, ACTY.ECSI = ACTY.ECSI, ITEM.TBL = ITEM.TBL,
  EQIP.TBL = EQIP.TBL, ENY.TBL = ENY.TBL, BOM.ECSI = BOM.ECSI, Routing.ECSI = Routing.ECSI, ProductionOrder.ECSI = ProductionOrder.ECSI), function(x) x[0, , drop = FALSE])
for(table in itemized_master_names).project_templates[[table]] <- source_master_rows(.project_templates[[table]])
.project_name <- "Untitled project"

project_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE,
  dataframe = "rows", digits = NA, na = "null"))

get_project_status_json <- function() project_json(list(name = .project_name,
  hasData = any(vapply(names(.project_templates), function(n) nrow(get(n, .GlobalEnv)) > 0, logical(1)))))

serialize_project <- function(visualization = NULL) {
  saved_tables <- mget(names(.project_templates), envir=.GlobalEnv)
  for(table in itemized_master_names)saved_tables[[table]] <- source_master_rows(saved_tables[[table]])
  saved_tables$ProductionOrder.ECSI <- production_rows(ProductionOrder.ECSI)
  document <- list(projectFormat = "manufacturing-cost-ghg", projectFormatVersion = 12L,
    metadata = list(name = .project_name),
    tables = saved_tables,
    counters = list(MAT = .last_mat_id, WIP = unname(.master_last_ids[["WIP"]]),
      FP = unname(.master_last_ids[["FP"]]), ACTY = unname(.master_last_ids[["ACTY"]]), ITEM = nrow(ITEM.TBL),
      EQIP = unname(.resource_last_ids[["EQIP"]]), ENY = unname(.resource_last_ids[["ENY"]]), BOM = unname(.structure_last_ids[["BOM"]]), Routing = unname(.structure_last_ids[["Routing"]]), ProductionOrder = .last_order_id))
  document$actualProduction <- list(lastJobID=.last_activity_response_id,rows=production_rows(ActyResponse.ECSI))
  document$production <- list(lastNodeID=.last_node_id,tree=production_rows(ProNodeTree.ECSI),generations=.production_generations)
  if(exists("save_accounting_state",mode="function"))document$derivedAccounting <- save_accounting_state()
  if(!is.null(visualization)) document$visualization <- visualization
  project_json(document)
}

# Return the project name and serialized contents for download.
export_project_json <- function(project_name = .project_name, visualization = NULL) {
  validate_master_text(project_name, "Project name", FALSE)
  project_name <- trimws(project_name)
  if (!nzchar(project_name)) project_name <- "cost-ghg-project"
  previous_name <- .project_name
  .project_name <<- project_name
  # Restore metadata if serialization fails before an export can be returned.
  tryCatch(project_json(list(name = .project_name, contents = serialize_project(visualization))),
    error = function(e) { .project_name <<- previous_name; stop(e) })
}

project_keys <- function(x, required, label) {
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x)) ||
      !setequal(names(x), required)) stop("Invalid ", label, " components.")
}

# Graph positions are stored separately from manufacturing data and ID counters.
validate_visualization <- function(value) {
  project_keys(value,c("version","graphs"),"visualization")
  if(!identical(value$version,1L) && !identical(value$version,1)) stop("Unsupported visualization version.")
  if(!is.list(value$graphs) || !is.null(names(value$graphs))) stop("Invalid visual graphs.")
  keys <- character()
  for(graph in value$graphs) {
    project_keys(graph,c("key","positions"),"visual graph")
    validate_master_text(graph$key,"Graph key")
    if(graph$key %in% keys) stop("Duplicate visual graph key.")
    keys <- c(keys,graph$key)
    if(!is.list(graph$positions) || !is.null(names(graph$positions))) stop("Invalid visual positions.")
    ids <- character()
    for(point in graph$positions) {
      project_keys(point,c("id","x","y"),"visual position")
      validate_master_text(point$id,"Visual node ID")
      if(point$id %in% ids) stop("Duplicate visual node ID.")
      ids <- c(ids,point$id)
      for(coord in c("x","y")) if(!is.numeric(point[[coord]]) || length(point[[coord]])!=1L || !is.finite(point[[coord]])) stop("Invalid visual coordinate.")
    }
  }
  invisible(TRUE)
}
export_visual_project_json <- function(project_name, serialized_visualization) {
  value <- jsonlite::fromJSON(serialized_visualization,simplifyVector=FALSE)
  validate_visualization(value)
  export_project_json(project_name,value)
}

project_integer <- function(x, minimum = 0) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x >= minimum &&
    x == floor(x) && x < .Machine$integer.max
}

restore_project <- function(serialized_project, validate_only = FALSE) {
  validate_master_text(serialized_project, "Project file")
  candidate <- tryCatch(jsonlite::fromJSON(serialized_project, simplifyVector = FALSE),
    error = function(e) stop("Malformed project JSON."))
  restore_project_candidate(candidate, validate_only)
}

# Check JSON and CSV imports using the same project checks.
# Keep CSV numbers in R to avoid precision loss from another JSON conversion.
restore_project_candidate <- function(candidate, validate_only = FALSE) {
  if(!project_integer(candidate$projectFormatVersion,1)||candidate$projectFormatVersion!=12)
    stop("Unsupported project format/version. v0.1 requires format 12.")
  visualization <- candidate$visualization
  if(!is.null(visualization)) validate_visualization(visualization)
  if("visualization" %in% names(candidate)) {
    if(is.null(visualization)) stop("Invalid visualization metadata.")
    if(sum(names(candidate)=="visualization")!=1L) stop("Duplicate visualization section.")
    candidate$visualization <- NULL
  }
  derivedAccounting <- candidate$derivedAccounting
  if("derivedAccounting" %in% names(candidate)) {
    if(sum(names(candidate)=="derivedAccounting")!=1L||is.null(derivedAccounting))stop("Invalid derived accounting section.")
    candidate$derivedAccounting <- NULL
  }
  restored_accounting <- if(!is.null(derivedAccounting))parse_accounting_state(derivedAccounting) else NULL
  actualProduction <- candidate$actualProduction
  if("actualProduction" %in% names(candidate)) {
    if(sum(names(candidate)=="actualProduction")!=1L||is.null(actualProduction))stop("Invalid actual production section.")
    candidate$actualProduction <- NULL
  }
  production <- candidate$production
  if("production" %in% names(candidate)) {
    if(sum(names(candidate)=="production")!=1L || is.null(production)) stop("Invalid production section.")
    candidate$production <- NULL
  }
  project_keys(candidate, c("projectFormat", "projectFormatVersion", "metadata", "tables", "counters"), "project")
  if (!identical(candidate$projectFormat, "manufacturing-cost-ghg") ||
      !project_integer(candidate$projectFormatVersion, 1) || candidate$projectFormatVersion!=12)
    stop("Unsupported project format/version.")
  if(is.null(production)) stop("Missing cumulative production state.")
  if(is.null(actualProduction)) stop("Missing Activity Response state.")
  project_keys(candidate$metadata, "name", "metadata")
  validate_master_text(candidate$metadata$name, "Project name")
  project_keys(candidate$tables, names(.project_templates), "table")
  project_keys(candidate$counters, c("MAT", "WIP", "FP", "ACTY", "ITEM", "EQIP", "ENY", "BOM", "Routing", "ProductionOrder"), "counter")
  if (!all(vapply(candidate$counters, project_integer, logical(1)))) stop("Invalid ID counters.")

  tables <- .project_templates
  for (table in names(tables)) {
    rows <- candidate$tables[[table]]
    if (!is.list(rows) || !is.null(names(rows))) stop("Table rows must be JSON arrays.")
    template <- tables[[table]]
    for (row in rows) {
      if(table=="EQIP.TBL") {
        expected_cuEELstd <- row$uEELstd / row$ltCapacity
        if(is.null(row$cuEELstd) || !isTRUE(all.equal(row$cuEELstd,expected_cuEELstd,tolerance=1e-14)))
          stop("Invalid derived cuEELstd in EQIP.TBL.")
        row$cuEELstd <- expected_cuEELstd
      }
      project_keys(row, names(template), paste(table, "row"))

      for (column in names(template)) {
        if (table %in% c("BOM.ECSI", "Routing.ECSI") && column == "fpID" && is.null(row[[column]])) row[column] <- list(NA_integer_)
        if(table=="ProductionOrder.ECSI" && is.list(template[[column]])) {
          row[column] <- list(production_id_list(row[[column]],column %in% c("chilNodeID","bomID")))
          next
        }
        value <- row[[column]]
        if (table %in% c("BOM.ECSI", "Routing.ECSI") && column == "fpID" && is.numeric(value) && length(value) == 1L && is.na(value)) next
        if (is.character(template[[column]])) {
          validate_master_text(value, column, !grepl("Description$", column))
        } else if (grepl("ID$", column)) {
          if (!project_integer(value, 1)) stop("Invalid ID in ", table, ".")
        } else if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) stop("Invalid number in ", table, ".")
      }
      if(table=="ProductionOrder.ECSI") {
        parsed <- as.data.frame(row[c("orderID","fpID","orderQuantity","orderStart","orderEnd","orderStatus")],stringsAsFactors=FALSE)
        for(column in c("nodeID","chilNodeID","routeID","bomID","itemID")) parsed[[column]] <- I(list(row[[column]]))
      } else parsed <- as.data.frame(row[names(template)],stringsAsFactors=FALSE)
      tables[[table]] <- rbind(tables[[table]],parsed)
    }
    id <- names(template)[1]
    if (anyDuplicated(tables[[table]][[id]])) stop("Duplicate ID in ", table, ".")
    kind <- sub("\\.(ECSI|TBL)$", "", table)
    if (candidate$counters[[kind]] < max(c(0, tables[[table]][[id]]))) stop("Counter below allocated IDs.")
  }
  for (i in seq_len(nrow(tables$MAT.TBL))) {
    row <- tables$MAT.TBL[i, ]
    validate_material(row$matName, row$matDescription, row$qMstd, row$uOMqM, row$uMELstd, row$uOMuMEL)
  }
  for (category in tables$ACTY.ECSI$actyCat) validate_activity_category(category)
  for (kind in c("EQIP", "ENY")) {
    refs <- tables[[paste0(kind, ".TBL")]]
    for (i in seq_len(nrow(refs))) validate_resource(kind, as.list(refs[i, , drop = FALSE]))
  }
  expected <- buildItemMaster(tables$MAT.TBL,tables$WIP.ECSI,tables$FP.ECSI)
  items <- tables$ITEM.TBL
  if (anyDuplicated(expected$itemID) || nrow(expected) != nrow(items)) stop("Invalid item registrations.")
  expected <- expected[order(expected$itemID), , drop = FALSE]
  actual <- items[order(items$itemID), , drop = FALSE]
  rownames(expected) <- rownames(actual) <- NULL
  if (!isTRUE(all.equal(expected, actual, check.attributes = FALSE))) stop("Item registry relationships do not match master data.")

  validate_structure(tables$BOM.ECSI, tables$Routing.ECSI, tables$ITEM.TBL, tables$ACTY.ECSI, tables$EQIP.TBL, tables$ENY.TBL)
  validate_orders(tables$ProductionOrder.ECSI,tables$FP.ECSI)
  restored_production <- restore_production_state(production,tables$ProductionOrder.ECSI)
  restored_actuals <- restore_activity_response_state(actualProduction,tables$ProductionOrder.ECSI,restored_production)
  executed <- unique(restored_actuals$table$orderID)
  if(any(tables$ProductionOrder.ECSI$orderID %in% executed & tables$ProductionOrder.ECSI$orderStatus!="Completed")) stop("Saved observations require Completed order status.")
  if(any(tables$ProductionOrder.ECSI$orderStatus=="Completed" & !tables$ProductionOrder.ECSI$orderID %in% executed))
    stop("Completed orders require their saved Activity Response observations.")
  # Finish checking the imported project before replacing the open project.
  status <- list(name = candidate$metadata$name, hasData = any(vapply(tables, nrow, integer(1)) > 0))
  if(!is.null(visualization)) status$visualization <- visualization
  result <- project_json(status)
  if (validate_only) return(result)
  working <- map_itemized_masters(tables[itemized_master_names],tables$ITEM.TBL)
  for(table in itemized_master_names)tables[[table]] <- working[[table]]
  for (table in names(tables)) assign(table, tables[[table]], envir = .GlobalEnv)
  .last_mat_id <<- candidate$counters$MAT
  .master_last_ids <<- unlist(candidate$counters[c("WIP", "FP", "ACTY")], use.names = TRUE)
  .resource_last_ids <<- unlist(candidate$counters[c("EQIP", "ENY")], use.names = TRUE)
  .structure_last_ids <<- unlist(candidate$counters[c("BOM", "Routing")], use.names = TRUE)
  ActyResponse.ECSI <<- restored_actuals$table
  .last_activity_response_id <<- restored_actuals$lastJobID
  .last_order_id <<- candidate$counters$ProductionOrder
  ProNodeTree.ECSI <<- restored_production$tree
  .last_node_id <<- restored_production$lastNodeID
  .production_generations <<- restored_production$records
  if(exists("restore_accounting_state",mode="function"))restore_accounting_state(restored_accounting)
  .project_name <<- candidate$metadata$name
  result
}

reset_project <- function() {
  if(exists("clear_accounting_state",mode="function"))clear_accounting_state()
  for (table in names(.project_templates)) assign(table, .project_templates[[table]], envir = .GlobalEnv)
  working <- map_itemized_masters(mget(itemized_master_names,.GlobalEnv),ITEM.TBL)
  for(table in itemized_master_names)assign(table,working[[table]],.GlobalEnv)
  .last_mat_id <<- 0L
  .master_last_ids <<- c(WIP = 0L, FP = 0L, ACTY = 0L)
  .resource_last_ids <<- c(EQIP = 0L, ENY = 0L)
  .structure_last_ids <<- c(BOM = 0L, Routing = 0L)
  ActyResponse.ECSI <<- empty_activity_response(); .last_activity_response_id <<- 0L
  .last_order_id <<- 0L
  clear_production()
  .project_name <<- "Untitled project"
  get_project_status_json()
}

