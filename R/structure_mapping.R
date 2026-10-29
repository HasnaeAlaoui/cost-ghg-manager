# Set the fpID used in BOM and Routing.
# Finished products use their own fpID, while WIP can be shared between products.
setFPContext <- function(itemType, entityID, fpID, ITEM.TBL) {
  if(is.null(fpID)) fpID <- rep(NA_integer_,length(entityID))
  if(any(!is.na(fpID) & !fpID %in% ITEM.TBL$erpID[ITEM.TBL$erpCat=="final"])) stop("Unknown fpID context.")
  if(any(itemType=="FP" & !is.na(fpID) & fpID!=entityID)) stop("FP parent must use its own product context.")
  fpID[itemType=="FP"] <- entityID[itemType=="FP"]
  fpID
}

###############################################################
# PRODUCT STRUCTURE MAPPING (BOM.ECSI)
###############################################################
# Map BOM source parent and child entities to their unified itemIDs.

mapBOMItems <- function(BOM.source, ITEM.TBL) {
  validate_item_identities(ITEM.TBL)
  if(any(!BOM.source$parCat %in% c("FP","WIP","MAT")) ||
     any(!BOM.source$childCat %in% c("FP","WIP","MAT")))stop("Unknown source category.")
  BOM.source$fpID <- setFPContext(BOM.source$parCat,
    BOM.source$parID,BOM.source$fpID,ITEM.TBL)

  BOM.ECSI <- BOM.source |>
    left_join(ITEM.TBL |> transmute(parCat=source_category(erpCat),parID=erpID,
      parentItemID=itemID,parentItemName=itemName),by=c("parCat","parID")) |>
    left_join(ITEM.TBL |> transmute(childCat=source_category(erpCat),childID=erpID,
      childItemID=itemID,childItemName=itemName),by=c("childCat","childID")) |>
    select(bomID,fpID,parentItemID,parentItemName,childItemID,childItemName,rMstd,matUOM) |>
    as.data.frame()
  if(anyNA(BOM.ECSI$parentItemID)||anyNA(BOM.ECSI$childItemID))
    stop("Item source is missing or ambiguous.")
  validate_structure(BOM.ECSI,Routing.ECSI[0,],ITEM.TBL)
  BOM.ECSI
}

###############################################################
# PRODUCTION STRUCTURE MAPPING (Routing.ECSI)
###############################################################
# Map each Routing source entity to its unified itemID.

mapRoutingItems <- function(Routing.source, ITEM.TBL, ACTY.ECSI, EQIP.TBL, ENY.TBL) {
  validate_item_identities(ITEM.TBL)
  if(any(!Routing.source$wipFpCat %in% c("FP","WIP")))stop("Unknown source category.")
  Routing.source$fpID <- setFPContext(Routing.source$wipFpCat,
    Routing.source$wipFpID,Routing.source$fpID,ITEM.TBL)

  Routing.ECSI <- Routing.source |>
    left_join(ITEM.TBL |> transmute(wipFpCat=source_category(erpCat),wipFpID=erpID,itemID),by=c("wipFpCat","wipFpID")) |>
    left_join(ACTY.ECSI |> select(actyID,actyName),by="actyID") |>
    left_join(EQIP.TBL |> select(eqipID,eqipName),by="eqipID") |>
    left_join(ENY.TBL |> select(enyID,enyName),by="enyID") |>
    select(routeID,fpID,itemID,actyID,actyName,eqipID,eqipName,enyID,enyName,
      rEstd,eqipUOM,powEstd,powUOM) |>
    as.data.frame()
  if(anyNA(Routing.ECSI$itemID))stop("Item source is missing or ambiguous.")
  validate_structure(BOM.ECSI[0,],Routing.ECSI,ITEM.TBL,ACTY.ECSI,EQIP.TBL,ENY.TBL)
  Routing.ECSI
}
