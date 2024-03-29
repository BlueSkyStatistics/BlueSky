% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Kmeans.R
\name{BSkyKMeans}
\alias{BSkyKMeans}
\title{K-Means Cluster}
\usage{
BSkyKMeans(
  vars,
  centers,
  iter.max = 10,
  num.seeds = 10,
  storeClusterInDataset = FALSE,
  varNameForCluster = "",
  dataset,
  excludeEnvPrefix = FALSE
)
}
\arguments{
\item{vars}{The variables to analyze in a vector of form c('var1','var2'...)}

\item{centers}{either the number of clusters, say k, or a set of initial (distinct) cluster centers. If a number, a random set of (distinct) 
rows in x is chosen as the initial centers.}

\item{iter.max}{the maximum number of iterations allowed.}

\item{num.seeds}{The number of different starting random seeds to use. Each random seed results in a different k-means solution.}

\item{storeClusterInDataset}{Save the cluster assignments to the dataset}

\item{varNameForCluster}{The variable names for the assigned clusters}

\item{dataset}{The dataset to analyze}
}
\value{

}
\description{
Performs K-means clustering
}
