# Internal identity templates preserve the baseline columns. Model TBLs
# include the equipment and energy reference parameters.
.eqip_identity_schema <- data.frame(eqipID = integer(), eqipName = character(),
  eqipDescription = character(), eqipCat = character(), ltCapacity = double())
.eny_identity_schema <- data.frame(enyID = integer(), enyName = character(), enyDescription = character())
EQIP.TBL <- transform(.eqip_identity_schema, pEstd = double(), uOMpE = character(),
  qEstd = double(), uOMqE = character(), uEELstd = double(), uOMuEEL = character(), cuEELstd = double())
ENY.TBL <- transform(.eny_identity_schema, emEstd = double(), uOMemE = character())
.resource_last_ids <- c(EQIP = 0L, ENY = 0L)

resource_fields <- list(
  equipment = list(
    list(name = "eqipCat", label = "Equipment category · eqipCat", options = c("ProductionEQIP", "AssemblyEQIP")),
    list(name = "ltCapacity", label = "Lifetime capacity · ltCapacity", type = "number"),
    list(name = "pEstd", label = "Full cost rate · pEstd (EQIP.TBL)", type = "number"),
    list(name = "uOMpE", label = "Full cost rate unit · uOMpE", options = c("EUR/min")),
    list(name = "qEstd", label = "Variable cost rate · qEstd (EQIP.TBL)", type = "number"),
    list(name = "uOMqE", label = "Variable cost rate unit · uOMqE", options = c("EUR/min")),
    list(name = "uEELstd", label = "Equipment emission liability · uEELstd (EQIP.TBL)", type = "number"),
    list(name = "uOMuEEL", label = "Emission liability unit · uOMuEEL", options = c("kgCO2e/eqip"))),
  energy = list(
    list(name = "emEstd", label = "Emission factor · emEstd (ENY.TBL)", type = "number"),
    list(name = "uOMemE", label = "Emission factor unit · uOMemE", options = c("kgCO2e/kWh"))))
get_resource_fields_json <- function() as.character(jsonlite::toJSON(resource_fields, auto_unbox = TRUE))

validate_resource <- function(kind, row) {
  prefix <- if (kind == "EQIP") "eqip" else "eny"
  validate_master_text(row[[paste0(prefix, "Name")]], "Name")
  validate_master_text(row[[paste0(prefix, "Description")]], "Description", FALSE)
  fields <- resource_fields[[if (kind == "EQIP") "equipment" else "energy"]]
  for (field in fields) {
    value <- row[[field$name]]
    if (!is.null(field$options)) {
      validate_master_text(value, field$name)
      if (!(value %in% field$options)) stop("Unsupported ", field$name, ".")
    } else if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0 ||
               (field$name == "ltCapacity" && value == 0)) stop("Invalid ", field$name, ".")
  }
  if(kind=="EQIP" && !is.finite(row$uEELstd / row$ltCapacity)) stop("Invalid derived cuEELstd.")
}

get_resource_json <- function(kind, id = NULL) {
  rows <- get(paste0(kind, ".TBL"), .GlobalEnv)
  if (is.null(id)) return(get_materials_json(rows, ITEM.TBL))
  pos <- master_position(rows, names(rows)[1], id)
  as.character(jsonlite::toJSON(rows[pos, , drop = FALSE], dataframe = "rows", digits = NA))
}

mutate_resource <- function(kind, operation, id = NULL, values = NULL) {
  master_name <- model_table_name(kind)
  reference_name <- paste0(kind, ".TBL")
  master <- get(master_name, .GlobalEnv)
  refs <- get(reference_name, .GlobalEnv)
  id_column <- names(master)[1]
  if (!operation %in% c("create", "update", "delete")) stop("Unknown resource operation.")
  if (operation != "delete") validate_resource(kind, values)
  if (operation != "create") {
    pos <- master_position(master, id_column, id)
    ref_pos <- master_position(refs, id_column, id)
  }
  if (operation == "delete") {
    if (exists("assert_design_unreferenced", mode = "function")) assert_design_unreferenced(kind, id)
    master <- master[-pos, , drop = FALSE]
    refs <- refs[-ref_pos, , drop = FALSE]
  } else {
    if (operation == "create") id <- max(c(.resource_last_ids[[kind]], master[[id_column]])) + 1L
    row <- c(setNames(list(id), id_column), values)
    name_column <- if (kind == "EQIP") "eqipName" else "enyName"
    row[[name_column]] <- trimws(row[[name_column]])
    if(kind=="EQIP") row$cuEELstd <- row$uEELstd / row$ltCapacity
    if (operation == "create") {
      master <- rbind(master, as.data.frame(row[names(master)], stringsAsFactors = FALSE))
      refs <- rbind(refs, as.data.frame(row[names(refs)], stringsAsFactors = FALSE))
    } else {
      master[pos, ] <- row[names(master)]
      refs[ref_pos, ] <- row[names(refs)]
    }
  }
  result <- get_materials_json(refs, ITEM.TBL)
  assign(master_name, master, .GlobalEnv)
  assign(reference_name, refs, .GlobalEnv)
  if (exists("sync_routing_names", mode = "function")) sync_routing_names()
  if (operation == "create") .resource_last_ids[[kind]] <<- id
  result
}

create_equipment <- function(eqipName, eqipDescription, eqipCat, ltCapacity, pEstd, uOMpE, qEstd, uOMqE, uEELstd, uOMuEEL) {
  mutate_resource("EQIP", "create", values = as.list(environment()))
}
update_equipment <- function(eqipID, eqipName, eqipDescription, eqipCat, ltCapacity, pEstd, uOMpE, qEstd, uOMqE, uEELstd, uOMuEEL) {
  values <- as.list(environment()); values$eqipID <- NULL
  mutate_resource("EQIP", "update", eqipID, values)
}
delete_equipment <- function(eqipID) mutate_resource("EQIP", "delete", eqipID)
get_equipment_json <- function(eqipID = NULL) get_resource_json("EQIP", eqipID)
create_energy <- function(enyName, enyDescription, emEstd, uOMemE) mutate_resource("ENY", "create", values = as.list(environment()))
update_energy <- function(enyID, enyName, enyDescription, emEstd, uOMemE) {
  values <- as.list(environment()); values$enyID <- NULL
  mutate_resource("ENY", "update", enyID, values)
}
delete_energy <- function(enyID) mutate_resource("ENY", "delete", enyID)
get_energy_json <- function(enyID = NULL) get_resource_json("ENY", enyID)

