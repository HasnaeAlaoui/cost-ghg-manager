# Check CSV tables before replacing project data.
.csv_tables <- c("MAT.TBL", "WIP.ECSI", "FP.ECSI", "ACTY.ECSI",
  "EQIP.TBL", "ENY.TBL", "BOM.ECSI", "Routing.ECSI")
csv_table <- function(table) {
  if (!is.character(table) || length(table) != 1L || !table %in% .csv_tables)
    stop("Unsupported CSV table.")
  switch(table,
    BOM.ECSI = data.frame(bomID=integer(),parCat=character(),parID=integer(),
      childCat=character(),childID=integer(),rMstd=double(),matUOM=character(),fpID=integer()),
    Routing.ECSI = data.frame(routeID=integer(),wipFpCat=character(),wipFpID=integer(),
      actyID=integer(),eqipID=integer(),enyID=integer(),rEstd=double(),
      eqipUOM=character(),powEstd=double(),powUOM=character(),fpID=integer()),
    .project_templates[[table]])
}
csv_derived <- function(table) switch(table,
  EQIP.TBL = "cuEELstd",
  BOM.ECSI = c("parentItemName", "childItemName"),
  Routing.ECSI = c("actyName", "eqipName", "enyName"), character())
csv_error <- function(table, row, column, message) stop(
  table, ", data row ", row, ", column ", column, ": ", message, call. = FALSE)

csv_read <- function(table, contents) {
  template <- csv_table(table)
  validate_master_text(contents, "CSV file")
  contents <- sub("^\ufeff", "", contents)
  # Read everything as literal text: no implicit row names, NA coercion or fill.
  raw <- tryCatch(withCallingHandlers(read.table(text = contents, header = FALSE,
    sep = ",", quote = '"', comment.char = "", colClasses = "character",
    na.strings = NULL, blank.lines.skip = TRUE, fill = FALSE, strip.white = FALSE,
    check.names = FALSE, allowEscapes = FALSE), warning = function(w) stop(w)),
    error = function(e) stop(table, ": malformed CSV: ", conditionMessage(e)))
  if (!nrow(raw)) stop(table, ": missing header row.")
  headers <- unname(unlist(raw[1, ], use.names = FALSE))
  raw <- raw[-1, , drop = FALSE]
  # Desktop write.csv() defaults to a numbered, unnamed row-index column.
  if (length(headers) && headers[1] == "") {
    if (!identical(raw[[1]], as.character(seq_len(nrow(raw)))))
      stop(table, ": unnamed first column must contain R row numbers 1..n; use row.names=FALSE.")
    headers <- headers[-1]; raw <- raw[-1]
  }
  if (anyDuplicated(headers)) stop(table, ": duplicate column names: ",paste(unique(headers[duplicated(headers)]),collapse=", "))
  required <- setdiff(names(template), csv_derived(table))
  if(table %in% c("BOM.ECSI","Routing.ECSI"))required <- setdiff(required,"fpID")
  missing <- setdiff(required, headers); extra <- setdiff(headers, names(template))
  if (length(missing)) stop(table, ": missing columns: ", paste(missing, collapse = ", "))
  if (length(extra)) stop(table, ": unknown columns: ", paste(extra, collapse = ", "))
  names(raw) <- headers
  result <- template[rep(NA_integer_, nrow(raw)), , drop = FALSE]
  for (column in names(template)) {
    if(column=="fpID" && !column %in% headers) {
      result[[column]] <- rep(NA_integer_,nrow(raw));next
    }
    if (column %in% csv_derived(table)) {
      result[[column]] <- if(is.numeric(template[[column]])) rep(0,nrow(raw)) else rep("",nrow(raw))
      next
    }
    values <- raw[[column]]
    if (is.character(template[[column]])) {
      bad <- which(!nzchar(trimws(values)) & !grepl("Description$", column))
      if (length(bad)) csv_error(table, bad[1], column, "required text is empty.")
      result[[column]] <- values
    } else {
      nullable <- table %in% c("BOM.ECSI", "Routing.ECSI") && column == "fpID"
      empty <- trimws(values) %in% c("", "NA")
      numbers <- suppressWarnings(as.numeric(values))
      valid <- grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", trimws(values)) & is.finite(numbers)
      if (grepl("ID$", column)) valid <- valid & numbers >= 1 & numbers == floor(numbers) & numbers < .Machine$integer.max
      bad <- which(!(valid | (nullable & empty)))
      if (length(bad)) csv_error(table, bad[1], column, "required finite number / positive integer ID is missing or invalid.")
      numbers[empty] <- NA_real_
      result[[column]] <- if (is.integer(template[[column]])) as.integer(numbers) else numbers
    }
  }
  if(table=="EQIP.TBL") result$cuEELstd <- result$uEELstd / result$ltCapacity
  id <- names(template)[1]
  if (anyDuplicated(result[[id]])) csv_error(table, anyDuplicated(result[[id]]), id, "duplicate ID.")
  for (i in seq_len(nrow(result))) tryCatch({
    row <- as.list(result[i, , drop = FALSE])
    if (table == "MAT.TBL") do.call(validate_material, row[c("matName", "matDescription", "qMstd", "uOMqM", "uMELstd", "uOMuMEL")])
    if (table == "ACTY.ECSI") tryCatch(validate_activity_category(row$actyCat),
      error = function(e) stop("column actyCat: ", conditionMessage(e)))
    if (table == "EQIP.TBL") {
      if (!row$eqipCat %in% c("ProductionEQIP", "AssemblyEQIP")) stop("column eqipCat: unsupported category.")
      if (row$ltCapacity <= 0) stop("column ltCapacity: must be positive.")
    }
    if (table %in% c("EQIP.TBL", "ENY.TBL")) validate_resource(sub("\\.TBL$", "", table), row)
  }, error = function(e) stop(table, ", data row ", i, ": ", conditionMessage(e), call. = FALSE))
  if(table=="BOM.ECSI") return(mapBOMItems(result,ITEM.TBL))
  if(table=="Routing.ECSI") return(mapRoutingItems(result,ITEM.TBL,ACTY.ECSI,EQIP.TBL,ENY.TBL))
  result
}

csv_candidate <- function(table, contents, companion = "") {
  rows <- csv_read(table, contents)
  if (nrow(ProductionOrder.ECSI) || nrow(ProNodeTree.ECSI) || nrow(ActyResponse.ECSI))
    stop("CSV replacement is blocked while production orders, generated nodes or observations exist. Save the project and import into a new project, or remove unlocked orders first.")
  tables <- mget(names(.project_templates), .GlobalEnv)
  for(master in itemized_master_names)tables[[master]] <- source_master_rows(tables[[master]])
  tables[[table]] <- rows
  replaced <- table
  if (nzchar(companion)) stop("Import one complete model table at a time; companion CSVs are not supported.")
  items <- buildItemMaster(tables$MAT.TBL,tables$WIP.ECSI,tables$FP.ECSI)
  if(table %in% itemized_master_names) assert_item_composition_change()
  tables$ITEM.TBL <- items
  # Look up BOM and routing names from the referenced master records.
  for (side in c("parent", "child")) tables$BOM.ECSI[[paste0(side, "ItemName")]] <-
    items$itemName[match(tables$BOM.ECSI[[paste0(side, "ItemID")]], items$itemID)]
  for (kind in c("ACTY", "EQIP", "ENY")) {
    prefix <- tolower(kind); master <- tables[[model_table_name(kind)]]
    tables$Routing.ECSI[[paste0(prefix, "Name")]] <- master[[paste0(prefix, "Name")]][
      match(tables$Routing.ECSI[[paste0(prefix, "ID")]], master[[paste0(prefix, "ID")]])]
  }
  # Report the row causing a BOM or routing error before checking the full project.
  for (target in c("BOM.ECSI", "Routing.ECSI")) for (i in seq_len(nrow(tables[[target]])))
    tryCatch(validate_structure(
      if (target == "BOM.ECSI") tables[[target]][i, , drop = FALSE] else tables$BOM.ECSI[0, ],
      if (target == "Routing.ECSI") tables[[target]][i, , drop = FALSE] else tables$Routing.ECSI[0, ],
      items, tables$ACTY.ECSI, tables$EQIP.TBL, tables$ENY.TBL),
      error = function(e) stop(target, ", data row ", i, ": ", conditionMessage(e), call. = FALSE))
  document <- jsonlite::fromJSON(serialize_project(), simplifyVector = FALSE)
  for (target in names(tables)) {
    document$tables[[target]] <- production_rows(tables[[target]])
    kind <- sub("\\.(ECSI|TBL)$", "", target)
    document$counters[[kind]] <- max(document$counters[[kind]], 0, tables[[target]][[1]])
  }
  tryCatch(restore_project_candidate(document, validate_only = TRUE), error = function(e)
    stop("Replacement rejected: ", conditionMessage(e),
      if (grepl("Resource reference/master mismatch", conditionMessage(e))) " Import the complete resource TBL file." else "", call. = FALSE))
  list(document = document, tables = tables[replaced])
}

preview_csv_json <- function(table, contents, companion = "") {
  candidate <- csv_candidate(table, contents, companion)
  previews <- lapply(names(candidate$tables), function(name) list(table = name,
    columns = names(candidate$tables[[name]]), count = nrow(candidate$tables[[name]]),
    rows = head(candidate$tables[[name]], 50)))
  project_json(list(previews = previews, message = "Replace the listed tables in full. ITEM.TBL and derived relationship names will be synchronized. Unified item IDs are generated in FP / WIP / MAT row order; source IDs are preserved. Preview shows up to 50 rows per table."))
}
import_csv_json <- function(table, contents, companion = "") {
  # Check the import again against the current project before applying it.
  candidate <- csv_candidate(table, contents, companion)
  restore_project_candidate(candidate$document)
}
export_csv_json <- function(table) {
  csv_table(table)
  connection <- textConnection("output", "w", local = TRUE)
  on.exit(close(connection))
  rows <- get(table, .GlobalEnv)
  if(table %in% itemized_master_names)rows <- source_master_rows(rows)
  if(table %in% c("BOM.ECSI","Routing.ECSI"))rows <- sourceStructureRows(table,rows)
  # write.table's usual 15-digit formatting can lose double precision. Emit
  # round-trip decimal representations while retaining ordinary numeric CSV cells.
  quoted <- which(vapply(rows, is.character, logical(1)))
  for (column in names(rows)[vapply(rows, is.numeric, logical(1))]) {
    value <- rows[[column]]
    rows[[column]] <- ifelse(is.na(value), NA_character_, sprintf("%.17g", value))
  }
  write.table(rows, connection, sep = ",", row.names = FALSE,
    col.names = TRUE, quote = quoted, na = "", qmethod = "double", eol = "\n")
  project_json(list(name = paste0(table, ".csv"), contents = paste0(paste(output, collapse = "\n"), "\n")))
}

sourceStructureRows <- function(table, rows, items = ITEM.TBL) {
  resolve <- function(ids) {
    positions <- match(ids,items$itemID)
    if(anyNA(positions))stop("Unresolved item reference during CSV export.")
    items[positions,,drop=FALSE]
  }
  if(table=="BOM.ECSI") {
    parent <- resolve(rows$parentItemID);child <- resolve(rows$childItemID)
    return(data.frame(bomID=rows$bomID,parCat=source_category(parent$erpCat),parID=parent$erpID,
      childCat=source_category(child$erpCat),childID=child$erpID,rMstd=rows$rMstd,matUOM=rows$matUOM,fpID=rows$fpID))
  }
  item <- resolve(rows$itemID)
  data.frame(routeID=rows$routeID,wipFpCat=source_category(item$erpCat),wipFpID=item$erpID,
    rows[c("actyID","eqipID","enyID","rEstd","eqipUOM","powEstd","powUOM","fpID")])
}
