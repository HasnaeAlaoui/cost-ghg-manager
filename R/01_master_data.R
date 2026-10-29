# Baseline schemas: 1. AIS_SCM_ExistingData.Rmd, FP (54), WIP (80), ACTY (181).
# Start with empty master tables.
WIP.ECSI <- data.frame(wipID = integer(),
  wipName = character(), wipDescription = character(), stringsAsFactors = FALSE)
FP.ECSI <- data.frame(fpID = integer(),
  fpName = character(), fpDescription = character(), stringsAsFactors = FALSE)
ACTY.ECSI <- data.frame(actyID = integer(), actyName = character(),
  actyDescription = character(), actyCat = character(), stringsAsFactors = FALSE)
.master_last_ids <- c(WIP = 0L, FP = 0L, ACTY = 0L)
ACTIVITY_CATEGORIES <- c(PrimaryActy = "Primary activity", SecondaryActy = "Secondary activity")
get_activity_categories_json <- function() as.character(jsonlite::toJSON(
  lapply(names(ACTIVITY_CATEGORIES), function(value) list(value = value, label = unname(ACTIVITY_CATEGORIES[[value]]))), auto_unbox = TRUE))
validate_activity_category <- function(category) {
  validate_master_text(category, "Activity category")
  if (!(category %in% names(ACTIVITY_CATEGORIES))) stop("Unsupported activity category.")
}

master_spec <- function(kind) {
  switch(kind,
    WIP = list(table = "WIP.ECSI", id = "wipID", name = "wipName", description = "wipDescription", item = TRUE),
    FP = list(table = "FP.ECSI", id = "fpID", name = "fpName", description = "fpDescription", item = TRUE),
    ACTY = list(table = "ACTY.ECSI", id = "actyID", name = "actyName", description = "actyDescription", item = FALSE),
    stop("Unknown master data type."))
}

validate_master_text <- function(value, label, required = TRUE) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      (required && !nzchar(trimws(value)))) stop(label, " must be a single", if (required) " non-empty" else "", " text value.")
}

master_position <- function(rows, id_column, id) {
  if (!is.numeric(id) || length(id) != 1L || !is.finite(id) || id < 1 || id != floor(id))
    stop("Invalid entity ID.")
  pos <- which(rows[[id_column]] == id)
  if (length(pos) != 1L) stop("Record not found.")
  pos
}

get_master_json <- function(kind, id = NULL) {
  spec <- master_spec(kind)
  rows <- get(spec$table, envir = .GlobalEnv)
  if (is.null(id)) return(get_materials_json(rows, ITEM.TBL))
  pos <- master_position(rows, spec$id, id)
  as.character(jsonlite::toJSON(rows[pos, , drop = FALSE], dataframe = "rows", digits = NA))
}

mutate_master <- function(kind, operation, id = NULL, name = NULL, description = NULL, category = NULL) {
  spec <- master_spec(kind)
  rows <- get(spec$table, envir = .GlobalEnv)
  if(spec$item)rows <- source_master_rows(rows)
  if (!(operation %in% c("create", "update", "delete"))) stop("Unknown master operation.")
  if (operation != "delete") {
    validate_master_text(name, "Name")
    validate_master_text(description, "Description", FALSE)
    if (kind == "ACTY") validate_activity_category(category)
  }
  if (operation != "create") {
    pos <- master_position(rows, spec$id, id)

  }
  if (operation == "delete") {
    if (exists("assert_design_unreferenced", mode = "function")) assert_design_unreferenced(kind, id)

    rows <- rows[-pos, , drop = FALSE]
  } else if (operation == "create") {
    id <- max(c(.master_last_ids[[kind]], rows[[spec$id]])) + 1L
    row <- list(id)
    names(row) <- spec$id
    row[[spec$name]] <- trimws(name)
    row[[spec$description]] <- description
    if (kind == "ACTY") row$actyCat <- trimws(category)
    rows <- rbind(rows, as.data.frame(row, stringsAsFactors = FALSE))
  } else {
    rows[pos, c(spec$name, spec$description)] <- list(trimws(name), description)
    if (kind == "ACTY") rows$actyCat[pos] <- trimws(category)
  }
  if(spec$item) result <- publish_itemized_master(spec$table,rows)
  else {
    result <- get_materials_json(rows,ITEM.TBL)
    assign(spec$table,rows,.GlobalEnv)
    if(exists("sync_routing_names",mode="function"))sync_routing_names()
  }
  if (operation == "create") {
    .master_last_ids[[kind]] <<- id
  }
  result
}

create_wip <- function(wipName, wipDescription) mutate_master("WIP", "create", name = wipName, description = wipDescription)
update_wip <- function(wipID, wipName, wipDescription) mutate_master("WIP", "update", wipID, wipName, wipDescription)
delete_wip <- function(wipID) mutate_master("WIP", "delete", wipID)
get_wip_json <- function(wipID = NULL) get_master_json("WIP", wipID)

create_fp <- function(fpName, fpDescription) mutate_master("FP", "create", name = fpName, description = fpDescription)
update_fp <- function(fpID, fpName, fpDescription) mutate_master("FP", "update", fpID, fpName, fpDescription)
delete_fp <- function(fpID) mutate_master("FP", "delete", fpID)
get_fp_json <- function(fpID = NULL) get_master_json("FP", fpID)

create_activity <- function(actyName, actyDescription, actyCat) mutate_master("ACTY", "create", name = actyName, description = actyDescription, category = actyCat)
update_activity <- function(actyID, actyName, actyDescription, actyCat) mutate_master("ACTY", "update", actyID, actyName, actyDescription, actyCat)
delete_activity <- function(actyID) mutate_master("ACTY", "delete", actyID)
get_activity_json <- function(actyID = NULL) get_master_json("ACTY", actyID)

# Working master itemization and composition dependency policy.
itemized_master_names <- c("MAT.TBL","WIP.ECSI","FP.ECSI")
source_master_rows <- function(rows) rows[setdiff(names(rows),"itemID")]
map_itemized_masters <- function(masters,items) {
  for(kind in c("MAT","WIP","FP")) {
    table <- model_table_name(kind);rows <- source_master_rows(masters[[table]])
    column <- paste0(tolower(kind),"ID")
    rows$itemID <- items$itemID[match(paste(rep(erp_category(kind),nrow(rows)),rows[[column]],sep=":"),item_entity_keys(items))]
    if(anyNA(rows$itemID))stop("Master has an unresolved entity/item identity.")
    masters[[table]] <- rows[c(names(rows)[1],"itemID",setdiff(names(rows),c(names(rows)[1],"itemID")))]
  }
  masters
}
ITEM.TBL <- buildItemMaster(MAT.TBL,WIP.ECSI,FP.ECSI)
.initial_itemized_masters <- map_itemized_masters(mget(itemized_master_names,.GlobalEnv),ITEM.TBL)
for(.itemized_table in itemized_master_names)assign(.itemized_table,.initial_itemized_masters[[.itemized_table]],.GlobalEnv)
rm(.initial_itemized_masters,.itemized_table)

# Composition is frozen once structures exist; no automatic rebinding.
assert_item_composition_change <- function() {
  if(nrow(ProductionOrder.ECSI)||nrow(ProNodeTree.ECSI)||nrow(ActyResponse.ECSI))
    stop("Master composition is blocked while production orders, generated nodes or observations exist.")
  if(nrow(BOM.ECSI)||nrow(Routing.ECSI))
    stop("Master composition is locked while Product Structure or Production Structure exists. Clear BOM.ECSI and Routing.ECSI before adding, deleting, reordering or replacing MAT/WIP/FP.")
}
publish_itemized_master <- function(table,rows) {
  previous <- get(table,.GlobalEnv)
  if(!identical(previous[[1]],rows[[1]])) assert_item_composition_change()
  masters <- mget(itemized_master_names,.GlobalEnv);masters[[table]] <- rows
  items <- buildItemMaster(masters$MAT.TBL,masters$WIP.ECSI,masters$FP.ECSI)
  working <- map_itemized_masters(masters,items)
  result <- get_materials_json(working[[table]],items)
  for(master in itemized_master_names)assign(master,working[[master]],.GlobalEnv)
  ITEM.TBL <<- items
  sync_routing_names()
  result
}
