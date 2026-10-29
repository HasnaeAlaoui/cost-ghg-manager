###############################################################
# UNIFIED ITEM MASTER (ITEM.TBL)
###############################################################
# Combine finished products, WIP and materials into one item master.
# Keep their original entity ID and generate a common itemID.
buildItemMaster <- function(MAT.TBL, WIP.ECSI, FP.ECSI) {
  validate_source_master(FP.ECSI,"fpID","fpName")
  validate_source_master(WIP.ECSI,"wipID","wipName")
  validate_source_master(MAT.TBL,"matID","matName")

  fpItems <- FP.ECSI |> transmute(erpID=as.integer(fpID),erpCat="final",itemName=fpName,itemCat="finished")
  wipItems <- WIP.ECSI |> transmute(erpID=as.integer(wipID),erpCat="semi",itemName=wipName,itemCat="intermediate")
  matItems <- MAT.TBL |> transmute(erpID=as.integer(matID),erpCat="mat",itemName=matName,itemCat="material")

  ITEM.TBL <- bind_rows(fpItems,wipItems,matItems) |>
    mutate(itemID=row_number()) |>
    select(itemID,erpID,erpCat,itemName,itemCat)
  ITEM.TBL
}

# Check that the master data contain valid entity IDs and names.
validate_source_master <- function(rows,id,name) {
  if(!is.data.frame(rows)||!all(c(id,name) %in% names(rows)))stop("Invalid source master.")
  ids <- rows[[id]]
  if(anyDuplicated(ids)||any(!is.finite(ids)|ids<1|ids!=floor(ids)|ids>=.Machine$integer.max))stop("Invalid ",id," values.")
  if(!is.character(rows[[name]])||anyNA(rows[[name]])||any(!nzchar(trimws(rows[[name]]))))stop("Invalid ",name," values.")
}
item_entity_keys <- function(items) paste(items$erpCat,items$erpID,sep=":")
item_for_entity <- function(kind,id,items=ITEM.TBL) {
  if(!kind %in% c("FP","WIP","MAT"))stop("Unknown source category: ",kind)
  pos <- which(items$erpCat==erp_category(kind) & items$erpID==id)
  if(length(pos)!=1L) stop("Item source is missing or ambiguous: ",kind," ",id,".")
  items[pos,,drop=FALSE]
}
validate_item_identities <- function(items) {
  if(!identical(names(items),c("itemID","erpID","erpCat","itemName","itemCat")) ||
     any(!source_category(items$erpCat) %in% c("FP","WIP","MAT")) ||
     anyNA(items$erpCat) || anyNA(items$itemCat) ||
     any(items$itemCat != unname(c(final="finished",semi="intermediate",mat="material")[items$erpCat])) ||
     anyDuplicated(items$itemID) || anyDuplicated(item_entity_keys(items)))
    stop("Invalid unified item identities.")
  invisible(TRUE)
}


# Explicit source CSV category conversion; names are never identity keys.
erp_category <- function(category) unname(c(FP="final",WIP="semi",MAT="mat")[category])
source_category <- function(category) unname(c(final="FP",semi="WIP",mat="MAT")[category])
