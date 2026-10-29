# Exact baseline design schemas, including nullable fpID and Routing name columns.
BOM.ECSI <- data.frame(bomID = integer(), fpID = integer(), parentItemID = integer(), parentItemName = character(),
  childItemID = integer(), childItemName = character(), rMstd = double(), matUOM = character())
Routing.ECSI <- data.frame(routeID = integer(), fpID = integer(), itemID = integer(),
  actyID = integer(), actyName = character(), eqipID = integer(), eqipName = character(),
  enyID = integer(), enyName = character(), rEstd = double(), eqipUOM = character(),
  powEstd = double(), powUOM = character())
.structure_last_ids <- c(BOM = 0L, Routing = 0L)

structure_item <- function(id, types, items = ITEM.TBL) {
  pos <- master_position(items, "itemID", id)
  if (!(source_category(items$erpCat)[pos] %in% types)) stop("Invalid item type for this relationship.")
  items[pos, , drop = FALSE]
}
structure_fp <- function(item, scope = 0, products = FP.ECSI) {
  if (scope == 0) return(if (source_category(item$erpCat) == "FP") item$erpID else NA_integer_)
  master_position(products, "fpID", scope)
  if (source_category(item$erpCat) == "FP" && scope != item$erpID) stop("FP parent must use its own product context.")
  scope
}
validate_fp_scope <- function(scope, parent, items) {
  if (length(scope) != 1L || !is.numeric(scope)) stop("Invalid fpID context.")
  if (source_category(parent$erpCat) == "FP") {
    if (is.na(scope) || scope != parent$erpID) stop("Invalid FP context.")
  } else if (!is.na(scope) && !(scope %in% items$erpID[source_category(items$erpCat) == "FP"])) stop("Unknown fpID context.")
}
structure_positive <- function(value, label, zero = FALSE) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value < 0 || (!zero && value == 0)) stop("Invalid ", label, ".")
}
structure_unit <- function(value, choices) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !(value %in% choices)) stop("Invalid standard UOM.")
}

# Use Kahn's ordering to check BOM cycles and assign graph levels.
bom_levels <- function(bom) {
  ids <- unique(c(bom$parentItemID, bom$childItemID))
  remaining <- ids
  levels <- setNames(rep(0L, length(ids)), as.character(ids))
  while (length(remaining)) {
    roots <- remaining[!remaining %in% bom$childItemID[bom$parentItemID %in% remaining]]
    if (!length(roots)) stop("Circular BOM structure is not allowed.")
    for (parent in roots) for (child in bom$childItemID[bom$parentItemID == parent])
      levels[[as.character(child)]] <- max(levels[[as.character(child)]], levels[[as.character(parent)]] + 1L)
    remaining <- setdiff(remaining, roots)
  }
  levels
}
validate_structure <- function(bom, routes, items = ITEM.TBL, activities = ACTY.ECSI,
                               equipment = EQIP.TBL, energy = ENY.TBL) {
  if (anyDuplicated(paste(bom$fpID, bom$parentItemID, bom$childItemID))) stop("Duplicate BOM relation.")
  for (i in seq_len(nrow(bom))) {
    row <- bom[i, ]
    parent <- structure_item(row$parentItemID, c("WIP", "FP"), items)
    child <- structure_item(row$childItemID, c("MAT", "WIP"), items)
    if (row$parentItemName != parent$itemName || row$childItemName != child$itemName) stop("BOM item name mismatch.")
    if (row$parentItemID == row$childItemID) stop("A BOM item cannot reference itself.")
    validate_fp_scope(row$fpID, parent, items)
    structure_positive(row$rMstd, "rMstd")
    structure_unit(row$matUOM, c("pc/unit", "m/unit"))
  }
  bom_levels(bom)
  if (anyDuplicated(paste(routes$fpID, routes$itemID, routes$actyID, routes$eqipID, routes$enyID))) stop("Duplicate route.")
  for (i in seq_len(nrow(routes))) {
    row <- routes[i, ]
    parent <- structure_item(row$itemID, c("WIP", "FP"), items)
    validate_fp_scope(row$fpID, parent, items)
    for (pair in list(list(activities, "actyID", "actyName"), list(equipment, "eqipID", "eqipName"), list(energy, "enyID", "enyName"))) {
      pos <- master_position(pair[[1]], pair[[2]], row[[pair[[2]]]])
      if (row[[pair[[3]]]] != pair[[1]][[pair[[3]]]][pos]) stop("Routing resource name mismatch.")
    }
    structure_positive(row$rEstd, "rEstd")
    structure_positive(row$powEstd, "powEstd", TRUE)
    structure_unit(row$eqipUOM, "min/unit")
    structure_unit(row$powUOM, "kW")
  }
  invisible(TRUE)
}
sync_routing_names <- function() {
  if (!exists("Routing.ECSI", .GlobalEnv, inherits = FALSE)) return(invisible(NULL))
  Routing.ECSI$actyName <<- ACTY.ECSI$actyName[match(Routing.ECSI$actyID, ACTY.ECSI$actyID)]
  Routing.ECSI$eqipName <<- EQIP.TBL$eqipName[match(Routing.ECSI$eqipID, EQIP.TBL$eqipID)]
  Routing.ECSI$enyName <<- ENY.TBL$enyName[match(Routing.ECSI$enyID, ENY.TBL$enyID)]
  BOM.ECSI$parentItemName <<- ITEM.TBL$itemName[match(BOM.ECSI$parentItemID, ITEM.TBL$itemID)]
  BOM.ECSI$childItemName <<- ITEM.TBL$itemName[match(BOM.ECSI$childItemID, ITEM.TBL$itemID)]
}
assert_design_unreferenced <- function(kind, id) {
  if(exists("assert_actual_master_unreferenced",mode="function")) assert_actual_master_unreferenced(kind,id)
  if(kind=="FP" && exists("ProductionOrder.ECSI",.GlobalEnv,inherits=FALSE) && any(ProductionOrder.ECSI$fpID==id)) stop("FP is referenced by ProductionOrder.")
  if (kind %in% c("MAT", "WIP", "FP")) {
    item <- ITEM.TBL$itemID[source_category(ITEM.TBL$erpCat) == kind & ITEM.TBL$erpID == id]
    if (any(BOM.ECSI$parentItemID %in% item | BOM.ECSI$childItemID %in% item) || any(Routing.ECSI$itemID %in% item))
      stop("Record is referenced by Product Structure; remove its relations first.")
    if (kind == "FP" && (any(BOM.ECSI$fpID == id, na.rm = TRUE) || any(Routing.ECSI$fpID == id, na.rm = TRUE))) stop("FP is referenced as design context.")
  } else {
    column <- switch(kind, ACTY = "actyID", EQIP = "eqipID", ENY = "enyID")
    if (any(Routing.ECSI[[column]] == id)) stop("Resource is referenced by Routing; remove its routes first.")
  }
}
structure_mutate <- function(kind, operation, id = NULL, row = NULL) {
  table <- if (kind == "BOM") BOM.ECSI else Routing.ECSI
  key <- names(table)[1]
  if (operation == "create") {
    id <- max(c(.structure_last_ids[[kind]], table[[key]])) + 1L
    row[[key]] <- id
    table <- rbind(table, as.data.frame(row[names(table)], stringsAsFactors = FALSE))
  } else {
    pos <- master_position(table, key, id)
    if (operation == "delete") table <- table[-pos, , drop = FALSE]
    else { row[[key]] <- id; table[pos, ] <- row[names(table)] }
  }
  validate_structure(if (kind == "BOM") table else BOM.ECSI, if (kind == "Routing") table else Routing.ECSI)
  result <- as.character(jsonlite::toJSON(list(ok = TRUE), auto_unbox = TRUE))
  assign(model_table_name(kind), table, .GlobalEnv)
  if (operation == "create") .structure_last_ids[[kind]] <<- id
  result
}
bom_row <- function(parentItemID, childItemID, rMstd, matUOM, scope = 0, items = ITEM.TBL, products = FP.ECSI) list(
  fpID = structure_fp(structure_item(parentItemID, c("WIP", "FP"), items), scope, products),
  parentItemID = parentItemID, parentItemName = structure_item(parentItemID, c("WIP", "FP"), items)$itemName,
  childItemID = childItemID, childItemName = structure_item(childItemID, c("MAT", "WIP"), items)$itemName, rMstd = rMstd, matUOM = matUOM)
create_bom <- function(parentItemID, childItemID, rMstd, matUOM, scope = 0) structure_mutate("BOM", "create", row = bom_row(parentItemID, childItemID, rMstd, matUOM, scope))
update_bom <- function(bomID, parentItemID, childItemID, rMstd, matUOM, scope = 0) structure_mutate("BOM", "update", bomID, bom_row(parentItemID, childItemID, rMstd, matUOM, scope))
delete_bom <- function(bomID) structure_mutate("BOM", "delete", bomID)
route_row <- function(itemID, actyID, eqipID, enyID, rEstd, eqipUOM, powEstd, powUOM, scope = 0, items = ITEM.TBL, activities = ACTY.ECSI, equipment = EQIP.TBL, energy = ENY.TBL, products = FP.ECSI) list(
  fpID = structure_fp(structure_item(itemID, c("WIP", "FP"), items), scope, products), itemID = itemID,
  actyID = actyID, actyName = activities$actyName[master_position(activities, "actyID", actyID)],
  eqipID = eqipID, eqipName = equipment$eqipName[master_position(equipment, "eqipID", eqipID)],
  enyID = enyID, enyName = energy$enyName[master_position(energy, "enyID", enyID)],
  rEstd = rEstd, eqipUOM = eqipUOM, powEstd = powEstd, powUOM = powUOM)
create_route <- function(itemID, actyID, eqipID, enyID, rEstd, eqipUOM, powEstd, powUOM, scope = 0) structure_mutate("Routing", "create", row = route_row(itemID, actyID, eqipID, enyID, rEstd, eqipUOM, powEstd, powUOM, scope))
update_route <- function(routeID, itemID, actyID, eqipID, enyID, rEstd, eqipUOM, powEstd, powUOM, scope = 0) structure_mutate("Routing", "update", routeID, route_row(itemID, actyID, eqipID, enyID, rEstd, eqipUOM, powEstd, powUOM, scope))
delete_route <- function(routeID) structure_mutate("Routing", "delete", routeID)

# BOM and routing graph; no production occurrences are generated here.
structure_graph <- function(kind, parent = 0, context = 0) {
  if (!(kind %in% c("bom", "route", "production"))) stop("Invalid graph kind.")
  bom <- BOM.ECSI; routes <- Routing.ECSI
  root <- parent
  if (parent > 0) {
    selected <- structure_item(parent,c("WIP","FP"))
    if (context == 0 && source_category(selected$erpCat) == "FP") context <- selected$erpID
  }
  if (context > 0) {
    fp <- master_position(FP.ECSI, "fpID", context)
    root <- item_for_entity("FP",FP.ECSI$fpID[fp])$itemID
    bom <- bom[is.na(bom$fpID) | bom$fpID == context, , drop = FALSE]
    routes <- routes[is.na(routes$fpID) | routes$fpID == context, , drop = FALSE]
  }
  if (kind %in% c("production","route") && context == 0)
    return(list(nodes=list(),edges=list(),direction=if(kind=="route") "LR" else "TB",context=context,
      note="Select a finished-product context to display its complete definition."))
  # Keep design rows reachable from the selected item.
  if (root > 0) {
    reached <- root
    repeat {
      next_ids <- union(reached,bom$childItemID[bom$parentItemID %in% reached])
      if (length(next_ids)==length(reached)) break
      reached <- next_ids
    }
    bom <- bom[bom$parentItemID %in% reached, , drop=FALSE]
    routes <- routes[routes$itemID %in% reached, , drop=FALSE]
  }
  nodes <- list(); edges <- list()
  add_node <- function(id, label, type, details = "") {
    if (!id %in% vapply(nodes, function(x) x$id, character(1))) nodes[[length(nodes)+1L]] <<- list(id=id,label=label,type=type,details=details)
  }
  add_item <- function(id) {
    item <- structure_item(id, c("MAT", "WIP", "FP"))
    add_node(paste0("item:",id),item$itemName,source_category(item$erpCat))
  }
  add_edge <- function(from,to,label="",details="") edges[[length(edges)+1L]] <<- list(from=from,to=to,label=label,details=details)
  route_details <- function(row) paste("routeID",row$routeID,"| fpID",ifelse(is.na(row$fpID),"shared",row$fpID),
    "| Equipment:",row$eqipName,"| Energy:",row$enyName,"| rEstd:",row$rEstd,row$eqipUOM,"| powEstd:",row$powEstd,row$powUOM)
  # Process flow shows produced items only; raw materials remain in the BOM view.
  if (kind == "route") {
    if (root > 0) add_item(root)
    produced <- ITEM.TBL$itemID[source_category(ITEM.TBL$erpCat) %in% c("WIP","FP")]
    bom <- bom[bom$childItemID %in% produced, , drop=FALSE]
    for (id in unique(c(bom$childItemID,bom$parentItemID))) add_item(id)
  }
  if (kind != "route") {
    if (root > 0) add_item(root)
    for (id in unique(c(bom$childItemID,bom$parentItemID))) add_item(id)
  }
  if (kind != "bom") {
    for (i in seq_len(nrow(routes))) {
      row <- routes[i, ]; add_item(row$itemID)
      op <- paste0("route:",row$routeID) # existing design-row identity, not occurrence ID
      add_node(op,row$actyName,"ACTY",route_details(row))
      if (kind == "route") add_edge(op,paste0("item:",row$itemID),paste(row$rEstd,row$eqipUOM),route_details(row))
      else add_edge(paste0("item:",row$itemID),op,paste(row$rEstd,row$eqipUOM),route_details(row))
    }
  }
  for (i in seq_len(nrow(bom))) {
    row <- bom[i, ]; label <- paste(row$rMstd,row$matUOM)
    compatible <- routes[routes$itemID == row$parentItemID &
      (is.na(routes$fpID) | is.na(row$fpID) | routes$fpID == row$fpID), , drop = FALSE]
    if (kind == "route") {
      if (nrow(compatible)) for (route in compatible$routeID)
        add_edge(paste0("item:",row$childItemID),paste0("route:",route),label)
      else add_edge(paste0("item:",row$childItemID),paste0("item:",row$parentItemID),label,"No routing defined for this produced item.")
    } else if (kind == "production" && nrow(compatible)) {
      for (route in compatible$routeID) add_edge(paste0("route:",route),paste0("item:",row$childItemID),label)
    } else add_edge(paste0("item:",row$parentItemID),paste0("item:",row$childItemID),label,
      if (kind == "production") "No routing defined for this produced item." else "")
  }
  # Arrange design nodes by level; quantities stay unchanged.
  ids <- vapply(nodes,function(x) x$id,character(1)); remaining <- ids
  levels <- setNames(rep(0L,length(ids)),ids)
  while(length(remaining)) {
    incoming <- vapply(Filter(function(e) e$from %in% remaining,edges),function(e)e$to,character(1))
    roots <- remaining[!remaining %in% incoming]
    if(!length(roots)) stop("Cannot lay out cyclic design graph.")
    for(root in roots) for(edge in Filter(function(e)e$from==root,edges)) levels[[edge$to]] <- max(levels[[edge$to]],levels[[root]]+1L)
    remaining <- setdiff(remaining,roots)
  }
  widest <- if(length(levels)) max(table(levels)) else 1L
  for(i in seq_along(nodes)) {
    level <- levels[[ids[i]]]
    slot <- sum(levels[seq_len(i)]==level)-1
    nodes[[i]]$x <- if(kind=="route") 110+level*280 else 110+((widest-sum(levels==level))/2+slot)*240
    nodes[[i]]$y <- if(kind=="route") 70+slot*120 else 70+level*140
  }
  note <- if(context==0) "All contexts; shared and product-specific definitions are additive." else "Shared plus matching fpID definitions; only the selected product's reachable hierarchy is shown."
  if(kind=="route") note <- paste(note,"Whole-product process flow; raw materials remain in BOM. Equipment, energy and power are in activity details.")
  if(kind!="bom" && anyDuplicated(routes$itemID)) note <- paste(note,"Multiple effective routes are shown as separate design branches; baseline tree generation requires exactly one route per produced item.")
  # Keep outputs above inputs in the hierarchy; arrows point toward produced outputs.
  if (kind %in% c("bom","production")) edges <- lapply(edges,function(edge) {
    child <- edge$to; edge$to <- edge$from; edge$from <- child; edge
  })
  list(nodes=nodes,edges=edges,direction=if(kind=="route") "LR" else "TB",context=context,note=note)
}
get_definition_graph_json <- function(kind, parent = 0, context = 0) as.character(jsonlite::toJSON(structure_graph(kind,parent,context), auto_unbox=TRUE))
get_bom_graph_json <- function() as.character(jsonlite::toJSON(structure_graph("bom"), auto_unbox = TRUE))
get_structure_json <- function(kind, parent = 0, view = "by") {
  if (!(kind %in% c("bom", "route")) || !(view %in% c("by", "all", "graph"))) stop("Invalid structure view.")
  if (parent != 0) structure_item(parent, c("WIP", "FP"))
  rows <- if (kind == "bom") BOM.ECSI else Routing.ECSI
  if (view == "by") rows <- rows[rows[[if (kind == "bom") "parentItemID" else "itemID"]] == parent, , drop = FALSE]
  if (view == "by" && kind == "bom") {
    rows <- data.frame(bomID = rows$bomID, Component = ITEM.TBL$itemName[match(rows$childItemID, ITEM.TBL$itemID)],
      Type = source_category(ITEM.TBL$erpCat)[match(rows$childItemID, ITEM.TBL$itemID)], rMstd = rows$rMstd, matUOM = rows$matUOM)
  }
  options <- function(rows, id, name) lapply(seq_len(nrow(rows)), function(i) list(value = rows[[id]][i],
    label = if(id=="itemID") paste0(rows$itemName[i]," (",source_category(rows$erpCat)[i]," ",rows$erpID[i],")") else rows[[name]][i]))
  children <- ITEM.TBL[source_category(ITEM.TBL$erpCat) %in% c("MAT", "WIP") & ITEM.TBL$itemID != parent, ]
  if (parent > 0 && nrow(children)) {
    children <- children[vapply(children$itemID, function(child) {
      edges <- rbind(BOM.ECSI, data.frame(bomID = 0, fpID = NA_integer_, parentItemID = parent,
        parentItemName = "", childItemID = child, childItemName = "", rMstd = 1, matUOM = "pc/unit"))
      tryCatch({ bom_levels(edges); TRUE }, error = function(e) FALSE)
    }, logical(1)), , drop = FALSE]
  }
  as.character(jsonlite::toJSON(list(columns = names(rows), rows = rows,
    contexts = options(FP.ECSI, "fpID", "fpName"),
    parents = options(ITEM.TBL[source_category(ITEM.TBL$erpCat) %in% c("WIP", "FP"), ], "itemID", "itemName"),
    children = options(children, "itemID", "itemName"), activities = options(ACTY.ECSI, "actyID", "actyName"),
    equipment = options(EQIP.TBL, "eqipID", "eqipName"), energy = options(ENY.TBL, "enyID", "enyName"),
    units = list(matUOM = c("pc/unit", "m/unit"), eqipUOM = c("min/unit"), powUOM = c("kW")),
    graph = structure_graph(kind, parent)), auto_unbox = TRUE, dataframe = "rows", digits = NA, na = "null"))
}
get_structure_row_json <- function(kind, id) {
  rows <- if (kind == "bom") BOM.ECSI else if (kind == "route") Routing.ECSI else stop("Invalid kind.")
  as.character(jsonlite::toJSON(rows[master_position(rows, names(rows)[1], id), , drop = FALSE], dataframe = "rows", digits = NA, na = "null"))
}
