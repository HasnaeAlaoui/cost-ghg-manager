# Canonical SCM baseline functions: minutes times EUR/min for monetary cost.
# Energy/GHG retains its baseline minute-to-hour conversions.
# Actual Production Activity Cost
sumPACact <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rE <- tree$rEact[[1]]
  pE <- tree$pEact[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  # Actual activity cost of current production node
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
          sumPACact(tree, child)
      )
    )
}

# Actual Variable Production Activity Cost
sumPACvact <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rE <- tree$rEact[[1]]
  qE <- tree$qEact[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  # Actual variable activity cost of current production node
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
          sumPACvact(tree, child)
      )
    )
}

# Actual Production Direct Cost
sumPDCact <- function(tree, root = NULL){

  node      <- tree$nodeID[[1]]
  chilNode  <- tree$chilNodeID[[1]]
  item      <- tree$itemID[[1]]

  childItem <- tree$childItemID[[1]]
  rM        <- tree$rMact[[1]]
  qM        <- tree$qMact[[1]]

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
        # Actual material input × Actual price
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
          sumPDCact(
            tree,
            node[j][1]
          )
      }
    )
  )
}

# Actual Production Activity Carbon Footprint
sumPACFact <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rE   <- tree$rEact[[1]]
  powE <- tree$powEact[[1]]
  emE  <- tree$emEact[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  # Actual internal GHG emission of current production node
  nodePACF <- (rE[i] / 60) * powE[i] * emE[i]

  chilNodes <- chilNode[[i]]

  # Terminal production node
  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodePACF)

  # Current node + recursively produced child nodes
  nodePACF +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPACFact(tree, child)
      )
    )
}

# Actual Production Equipment Carbon Footprint
sumPECFact <- function(tree, root = NULL){

  node     <- tree$nodeID[[1]]
  chilNode <- tree$chilNodeID[[1]]

  rE    <- tree$rEact[[1]]
  cuEEL <- tree$cuEELact[[1]]

  if (is.null(root))
    root <- setdiff(node, unlist(chilNode))[1]

  i <- match(root, node)

  if (is.na(i))
    return(0)

  # Actual equipment GHG liability of current production node
  nodePECF <- (rE[i] / 60) * cuEEL[i]

  chilNodes <- chilNode[[i]]

  # Terminal production node
  if (is.null(chilNodes) ||
      length(chilNodes) == 0 ||
      all(is.na(chilNodes)))
    return(nodePECF)

  # Current node + recursively produced child nodes
  nodePECF +
    sum(
      sapply(
        chilNodes,
        function(child)
          sumPECFact(tree, child)
      )
    )
}

# Actual Production Material Carbon Footprint
sumPMCFact <- function(tree, root = NULL){

  node      <- tree$nodeID[[1]]
  chilNode  <- tree$chilNodeID[[1]]
  item      <- tree$itemID[[1]]

  childItem <- tree$childItemID[[1]]

  rM   <- tree$rMact[[1]]
  uMEL <- tree$uMELact[[1]]

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
        # actual material input × actual unit-emission liability
        if (!is.na(uMELi[k])) {

          return(
            rMi[k] * uMELi[k]
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

        # Immediate-parent actual BOM quantity ×
        # recursively calculated child material footprint
        rMi[k] *
          sumPMCFact(
            tree,
            node[j][1]
          )
      }
    )
  )
}
