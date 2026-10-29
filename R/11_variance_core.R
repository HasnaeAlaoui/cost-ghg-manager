# Verbatim analytical variance functions from baseline/4. StdABCostGHGAIS_SCM.Rmd.
sumPACEV <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rEact <- tree$rEact[[1]]
  rEadj <- tree$rEadj[[1]]
  qEstd <- tree$qEstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  nodeEV <- (rEact[i] - rEadj[i]) * qEstd[i]

  chilNodes <- chilNode[[i]]

  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodeEV)

  nodeEV +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPACEV(tree, child)
      )
    )
}

sumPACPV <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rEact <- tree$rEact[[1]]
  qEstd <- tree$qEstd[[1]]
  qEact <- tree$qEact[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  nodePV <- (qEact[i] - qEstd[i]) * rEact[i]

  chilNodes <- chilNode[[i]]

  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodePV)

  nodePV +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPACPV(tree, child)
      )
    )
}

# Production Direct Cost Efficiency Variance
sumPDCEV <- function(tree, root = NULL){

  node      <- tree$nodeID[[1]]
  chilNode  <- tree$chilNodeID[[1]]
  item      <- tree$itemID[[1]]

  childItem <- tree$childItemID[[1]]

  rMstd <- tree$rMstd[[1]]
  rMact <- tree$rMact[[1]]
  qMstd <- tree$qMstd[[1]]

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

  rMstdi <- rMstd[[i]]
  rMacti <- rMact[[i]]
  qMstdi <- qMstd[[i]]

  sum(
    sapply(
      seq_along(items),
      function(k){

        # Purchased material:
        # quantity deviation × standard material price
        if (!is.na(qMstdi[k])) {

          return(
            (rMacti[k] - rMstdi[k]) *
              qMstdi[k]
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

        child <- node[j][1]

        # Standard direct-material cost of one child unit
        childPDCstd <- sumPDCstd(
          tree,
          child
        )

        # Quantity deviation at this BOM level
        # + efficiency deviations inside child structure
        (rMacti[k] - rMstdi[k]) *
          childPDCstd +
          rMacti[k] *
          sumPDCEV(
            tree,
            child
          )
      }
    )
  )
}

# Production Direct Cost Price Variance
sumPDCPV <- function(tree, root = NULL){

  node      <- tree$nodeID[[1]]
  chilNode  <- tree$chilNodeID[[1]]
  item      <- tree$itemID[[1]]

  childItem <- tree$childItemID[[1]]

  rMact <- tree$rMact[[1]]
  qMstd <- tree$qMstd[[1]]
  qMact <- tree$qMact[[1]]

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

  rMacti <- rMact[[i]]
  qMstdi <- qMstd[[i]]
  qMacti <- qMact[[i]]

  sum(
    sapply(
      seq_along(items),
      function(k){

        # Purchased material:
        # actual quantity × price deviation
        if (!is.na(qMstdi[k])) {

          return(
            rMacti[k] *
              (qMacti[k] - qMstdi[k])
          )
        }

        # Internally produced material:
        # propagate price variance from child structure
        chilNodes <- chilNode[[i]]

        j <- which(
          node %in% chilNodes &
          item == items[k]
        )

        if (length(j) == 0)
          return(0)

        child <- node[j][1]

        rMacti[k] *
          sumPDCPV(
            tree,
            child
          )
      }
    )
  )
}

sumPACFEmV <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rEact   <- tree$rEact[[1]]
  powEact <- tree$powEact[[1]]
  emEstd  <- tree$emEstd[[1]]
  emEact  <- tree$emEact[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  ECact <- (rEact[i] / 60) * powEact[i]

  nodeEmV <- ECact * (emEact[i] - emEstd[i])

  chilNodes <- chilNode[[i]]

  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodeEmV)

  nodeEmV +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPACFEmV(tree, child)
      )
    )
}

sumPACFEnyV <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rEact   <- tree$rEact[[1]]
  rEadj   <- tree$rEadj[[1]]

  powEstd <- tree$powEstd[[1]]
  powEact <- tree$powEact[[1]]

  emEstd  <- tree$emEstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  ECact <- (rEact[i] / 60) * powEact[i]
  ECadj <- (rEadj[i] / 60) * powEstd[i]

  nodeEnyV <- (ECact - ECadj) * emEstd[i]

  chilNodes <- chilNode[[i]]

  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodeEnyV)

  nodeEnyV +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPACFEnyV(tree, child)
      )
    )
}

sumPECFEV <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rEact   <- tree$rEact[[1]]
  rEadj   <- tree$rEadj[[1]]
  cuEELstd <- tree$cuEELstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  nodeEV <- ((rEact[i] - rEadj[i]) / 60) *
    cuEELstd[i]

  chilNodes <- chilNode[[i]]

  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodeEV)

  nodeEV +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPECFEV(tree, child)
      )
    )
}

sumPECFELV <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rEact    <- tree$rEact[[1]]
  cuEELstd <- tree$cuEELstd[[1]]
  cuEELact <- tree$cuEELact[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  nodeELV <- (rEact[i] / 60) *
    (cuEELact[i] - cuEELstd[i])

  chilNodes <- chilNode[[i]]

  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodeELV)

  nodeELV +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPECFELV(tree, child)
      )
    )
}

sumPMCFEV <- function(tree, root = NULL){

  node      <- tree$nodeID[[1]]
  chilNode  <- tree$chilNodeID[[1]]
  item      <- tree$itemID[[1]]

  childItem <- tree$childItemID[[1]]

  rMstd  <- tree$rMstd[[1]]
  rMact  <- tree$rMact[[1]]
  uMELstd <- tree$uMELstd[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  items <- childItem[[i]]

  if (is.null(items) ||
      length(items) == 0 ||
      all(is.na(items)))
    return(0)

  rMstdi   <- rMstd[[i]]
  rMacti   <- rMact[[i]]
  uMELstdi <- uMELstd[[i]]

  sum(
    sapply(
      seq_along(items),
      function(k){

        # Purchased material
        if (!is.na(uMELstdi[k])) {

          return(
            (rMacti[k] - rMstdi[k]) *
              uMELstdi[k]
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

        child <- node[j][1]

        childPMCFstd <- sumPMCFstd(
          tree,
          child
        )

        (rMacti[k] - rMstdi[k]) *
          childPMCFstd +
          rMacti[k] *
          sumPMCFEV(
            tree,
            child
          )
      }
    )
  )
}

sumPMCFELV <- function(tree, root = NULL){

  node      <- tree$nodeID[[1]]
  chilNode  <- tree$chilNodeID[[1]]
  item      <- tree$itemID[[1]]

  childItem <- tree$childItemID[[1]]

  rMact   <- tree$rMact[[1]]
  uMELstd <- tree$uMELstd[[1]]
  uMELact <- tree$uMELact[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  items <- childItem[[i]]

  if (is.null(items) ||
      length(items) == 0 ||
      all(is.na(items)))
    return(0)

  rMacti   <- rMact[[i]]
  uMELstdi <- uMELstd[[i]]
  uMELacti <- uMELact[[i]]

  sum(
    sapply(
      seq_along(items),
      function(k){

        # Purchased material
        if (!is.na(uMELstdi[k])) {

          return(
            rMacti[k] *
              (uMELacti[k] - uMELstdi[k])
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

        child <- node[j][1]

        rMacti[k] *
          sumPMCFELV(
            tree,
            child
          )
      }
    )
  )
}
