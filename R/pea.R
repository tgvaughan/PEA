# Basic tree traversal

getChildren <- function(tree, node) {
    return(tree$edge[tree$edge[,1]==node,2])
}

getParent <- function (tree, node) {
    return(tree$edge[tree$edge[,2]==node,1])
}

isLeaf <- function(tree, node) {
    return (length(getChildren(tree, node))==0)
}

isRoot <- function(tree, node) {
    return (length(getParent(tree, node))==0)
}

getRoot <- function(tree) {
    nodeMax <- max(tree$edge)

    for (i in 1:nodeMax) {
        if (isRoot(tree, i))
            return(i)
    }
}

# Get length of edge above node
getBranchLength <- function(tree, node) {
    if (isRoot(tree, node)) {
        return(tree$root.edge)
    } else {
        edgeNo <- which(tree$edge[,2]==node)
        return(tree$edge.length[edgeNo])
    }
}
    

# Find times associated with each node
getNodeHeights <- function (tree, node=NA, time=NA) {
    if (is.na(node)) {
        node <- getRoot(tree)
        time <- 0
    }

    times <- c(time)
    nodes <- c(node)
    
    for (child in getChildren(tree, node)) {
        childRes <- getNodeHeights(tree, child, time+getBranchLength(tree, child))
        times <- append(times, childRes$times)
        nodes <- append(nodes, childRes$nodes)
    }

    res <- list()
    res$times <- times
    res$nodes <- nodes

    # Assemble main result
    if (isRoot(tree, node)) {

        # Calculate node heights (ages)
        res$heights <- max(res$times)-res$times

        # Sort in order of increasing height
        sortedIdx <- sort(res$heights, index.return=T)$ix
        res$heights <- res$heights[sortedIdx]
        res$times <- res$times[sortedIdx]
        res$nodes <- res$nodes[sortedIdx]

        # Calculate lineage counts
        k <- 0
        for (i in 1:length(res$nodes)) {
            if (isLeaf(tree, res$nodes[i]))
                k[i+1] <- k[i] + 1
            else
                k[i+1] <- k[i] - 1
        }
        res$lineages <- k[-1]
    }
    
    return (res)
}
