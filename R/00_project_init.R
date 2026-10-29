model_table_name <- function(kind) paste0(kind, if (kind %in% c("MAT", "EQIP", "ENY")) ".TBL" else ".ECSI")
# Application material master combines baseline MAT.TBL identification with
# the reference price/GHG attributes previously derived in MAT.TBL.
# Initialize empty tables with base R.
library(jsonlite)
library(dplyr)

MATERIAL_UOMS <- list(
  financial = c("EUR/pc", "EUR/kg", "EUR/m", "EUR/kWh"),
  ghg = c("kgCO2e/pc", "kgCO2e/kg", "kgCO2e/m", "kgCO2e/kWh")
)

# Keep deleted IDs from being reused.
.last_mat_id <- 0L

MAT.TBL <- data.frame(
  matID = integer(0),
  matName = character(0),
  matDescription = character(0),
  qMstd = double(0),
  uOMqM = character(0),
  uMELstd = double(0),
  uOMuMEL = character(0),
  stringsAsFactors = FALSE
)

get_materials_json <- function(materials = MAT.TBL, items = ITEM.TBL) {
  as.character(jsonlite::toJSON(
    list(
      columns = names(materials), rows = materials,
      items = list(columns = names(items), rows = items),
      uoms = MATERIAL_UOMS
    ),
    dataframe = "rows",
    auto_unbox = FALSE,
    digits = NA,
    na = "null"
  ))
}

validate_material <- function(matName, matDescription, qMstd, uOMqM, uMELstd, uOMuMEL) {
  if (!is.character(matName) || length(matName) != 1L ||
      is.na(matName) || !nzchar(trimws(matName))) {
    stop("Material name must not be empty.")
  }
  if (!is.character(matDescription) || length(matDescription) != 1L ||
      is.na(matDescription)) {
    stop("Material description must be a single text value.")
  }
  if (!is.numeric(qMstd) || length(qMstd) != 1L || !is.finite(qMstd) || qMstd < 0) {
    stop("Material price qMstd must be a finite, non-negative number.")
  }
  if (!is.numeric(uMELstd) || length(uMELstd) != 1L || !is.finite(uMELstd) || uMELstd < 0) {
    stop("Material emission liability uMELstd must be a finite, non-negative number.")
  }
  if (!is.character(uOMqM) || length(uOMqM) != 1L ||
      is.na(uOMqM) || !(uOMqM %in% MATERIAL_UOMS$financial)) {
    stop("Select a supported price UOM uOMqM.")
  }
  if (!is.character(uOMuMEL) || length(uOMuMEL) != 1L ||
      is.na(uOMuMEL) || !(uOMuMEL %in% MATERIAL_UOMS$ghg)) {
    stop("Select a supported GHG UOM uOMuMEL.")
  }

  invisible(TRUE)
}

create_material <- function(matName, matDescription, qMstd, uOMqM, uMELstd, uOMuMEL) {
  validate_material(matName, matDescription, qMstd, uOMqM, uMELstd, uOMuMEL)
  # Prepare both tables before updating them.
  next_mat_id <- max(c(.last_mat_id, MAT.TBL$matID)) + 1L
  materials <- rbind(source_master_rows(MAT.TBL), data.frame(
    matID = next_mat_id,
    matName = trimws(matName),
    matDescription = matDescription,
    qMstd = as.double(qMstd),
    uOMqM = trimws(uOMqM),
    uMELstd = as.double(uMELstd),
    uOMuMEL = trimws(uOMuMEL),
    stringsAsFactors = FALSE
  ))
  result <- publish_itemized_master("MAT.TBL",materials)
  .last_mat_id <<- next_mat_id
  result
}

material_position <- function(matID) {
  if (!is.numeric(matID) || length(matID) != 1L || !is.finite(matID) ||
      matID < 1 || matID != floor(matID)) stop("Invalid material ID.")
  pos <- which(MAT.TBL$matID == matID)
  if (length(pos) != 1L) stop("Material not found.")
  pos
}

get_item_registry_json <- function() {
  as.character(jsonlite::toJSON(
    list(columns = names(ITEM.TBL), rows = ITEM.TBL),
    dataframe = "rows", auto_unbox = FALSE, digits = NA, na = "null"
  ))
}

get_material_json <- function(matID) {
  pos <- material_position(matID)
  as.character(toJSON(MAT.TBL[pos, , drop = FALSE], dataframe = "rows", digits = NA))
}

update_material <- function(matID, matName, matDescription, qMstd, uOMqM, uMELstd, uOMuMEL) {
  pos <- material_position(matID)
  validate_material(matName, matDescription, qMstd, uOMqM, uMELstd, uOMuMEL)
  materials <- MAT.TBL
  materials[pos, c("matName", "matDescription", "qMstd", "uOMqM", "uMELstd", "uOMuMEL")] <-
    list(trimws(matName), matDescription, as.double(qMstd), uOMqM, as.double(uMELstd), uOMuMEL)
  result <- publish_itemized_master("MAT.TBL",materials)
  result
}

delete_material <- function(matID) {
  pos <- material_position(matID)
  # Check material references before deletion.
  if (exists("assert_design_unreferenced", mode = "function")) assert_design_unreferenced("MAT", matID)

  materials <- MAT.TBL[-pos, , drop = FALSE]
  result <- publish_itemized_master("MAT.TBL",materials)
  result
}
