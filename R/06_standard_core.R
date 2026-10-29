# Canonical SCM baseline functions: minutes times EUR/min for monetary cost.
# Energy/GHG retains its baseline minute-to-hour conversions.
# Standard Production Activity Cost
sumPACstd <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rE <- tree$rEstd[[1]]
  pE <- tree$pEstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  # Standard activity cost of current production node
  nodePAC <- rE[i] * pE[i]

  chilNodes <- chilNode[[i]]

  # Terminal production node
  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodePAC)

  # Current node + recursively produced child nodes
  nodePAC +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPACstd(tree, child)
      )
    )
}

# Standard Variable Production Activity Cost
sumPACvstd <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rE <- tree$rEstd[[1]]
  qE <- tree$qEstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  # Standard variable activity cost of current production node
  nodePACv <- rE[i] * qE[i]

  chilNodes <- chilNode[[i]]

  # Terminal production node
  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodePACv)

  # Current node + recursively produced child nodes
  nodePACv +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPACvstd(tree, child)
      )
    )
}

# Standard Production Direct Cost
sumPDCstd <- function(tree, root = NULL){

  node      <- tree$nodeID[[1]]
  chilNode  <- tree$chilNodeID[[1]]
  item      <- tree$itemID[[1]]

  childItem <- tree$childItemID[[1]]
  rM        <- tree$rMstd[[1]]
  qM        <- tree$qMstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  items <- childItem[[i]]

  # Base case: no material inputs
  if (is.null(items) ||
      length(items) == 0 ||
      all(is.na(items)))
    return(0)

  rMi <- rM[[i]]
  qMi <- qM[[i]]

  sum(
    sapply(
      seq_along(items),
      function(k){

        # Purchased material:
        # standard material input × standard price
        if (!is.na(qMi[k])) {
          return(
            rMi[k] * qMi[k]
          )
        }

        # Internally produced material:
        # identify corresponding child production node
        chilNodes <- chilNode[[i]]

        j <- which(
          node %in% chilNodes &
          item == items[k]
        )

        if (length(j) == 0)
          return(0)

        # Immediate-parent BOM quantity ×
        # recursively calculated child material cost
        rMi[k] *
          sumPDCstd(
            tree,
            node[j][1]
          )
      }
    )
  )
}

# Standard Production Activity Carbon Footprint
sumPACFstd <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rE   <- tree$rEstd[[1]]
  powE <- tree$powEstd[[1]]
  emE  <- tree$emEstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  # Standard internal GHG emission of current production node
  nodePACF <- (rE[i] / 60) * powE[i] * emE[i]

  chilNodes <- chilNode[[i]]

  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodePACF)

  nodePACF +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPACFstd(tree, child)
      )
    )
}

# Standard Production Equipment Carbon Footprint
sumPECFstd <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rE    <- tree$rEstd[[1]]
  cuEEL <- tree$cuEELstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  # Standard equipment GHG liability of current production node
  nodePECF <- (rE[i] / 60) * cuEEL[i]

  chilNodes <- chilNode[[i]]

  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodePECF)

  nodePECF +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPECFstd(tree, child)
      )
    )
}

# Standard Production Material Carbon Footprint
sumPMCFstd <- function(tree, root = NULL){

  node      <- tree$nodeID[[1]]
  chilNode  <- tree$chilNodeID[[1]]
  item      <- tree$itemID[[1]]

  childItem <- tree$childItemID[[1]]

  rM   <- tree$rMstd[[1]]
  uMEL <- tree$uMELstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  items <- childItem[[i]]

  # No material inputs
  if (is.null(items) ||
      length(items) == 0 ||
      all(is.na(items)))
    return(0)

  rMi   <- rM[[i]]
  uMELi <- uMEL[[i]]

  sum(
    sapply(
      seq_along(items),
      function(k){

        # Purchased material:
        # standard material input × standard unit-emission liability
        if (!is.na(uMELi[k])) {

          return(
            rMi[k] * uMELi[k]
          )
        }

        # Internally produced material
        chilNodes <- chilNode[[i]]

        j <- which(
          node %in% chilNodes &
          item == items[k]
        )

        if (length(j) == 0)
          return(0)

        # Immediate-parent BOM quantity ×
        # recursively calculated child material footprint
        rMi[k] *
          sumPMCFstd(
            tree,
            node[j][1]
          )
      }
    )
  )
}
