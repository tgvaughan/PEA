\name{PEA}
\alias{getChildren}
\alias{getParent}
\alias{isLeaf}
\alias{isRoot}
\alias{getRoot}
\alias{getLeaves}
\alias{getBranchLength}
\alias{getNodeHeights}
\title{PEA methods for querying trees}
\description{
  Methods for querying APE time trees.
}

\usage{
getChildren(tree, node)
getParent(tree, node)
isLeaf(tree, node)
isRoot(tree, node)
getRoot(tree)
getLeaves(tree)
getBranchLength(tree, node)
getNodeHeights(tree)
}

\arguments{
  \item{tree}{An object of class phylo.}
  \item{node}{An integer reprenting a node (leaf or internal)
    of tree.}
}
