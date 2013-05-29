\name{mvisClusterChart}
\alias{mvisClusterChart}
\title{ggplot2-style Cluster Chart
This function can generate ggplot2-style Cluster Chart.}
\usage{
  mvisClusterChart(data, xvar = "x", yvar = "y",
    clustervar = "cluster", namevar = NULL,
    centerDf = NULL, xlab = NULL, ylab = NULL,
    srcdir = getOption("mvis.html5.dir"))
}
\arguments{
  \item{data}{: the cluster result data.}

  \item{xvar}{: name of the character column which should
  displayed for X.}

  \item{yvar}{: name of the character column which should
  displayed for Y.}

  \item{clustervar:}{name of the character column which
  contains the class information.}

  \item{namevar:}{name of the character column which
  contains the name information, it could be used for
  searching on the chart. it could be contained in
  data.frame, and it could be a vector which have the same
  length with the number of data.frame rows.}

  \item{centerDf:}{the center of cluster result.}

  \item{xlab:}{a title for the x axis.}

  \item{ylab:}{a title for the y axis.}

  \item{srcdir:}{getOption("mvis.html5.dir"), the path of
  html5 file path.}
}
\value{
  the function will generate an html object, which could
  display by plot function.
}
\description{
  ggplot2-style Cluster Chart This function can generate
  ggplot2-style Cluster Chart.
}
\examples{
#'demo plots for mvisClusterChart


library(html5Vis)

## use iris as demo data, and use c("Petal.Length","Petal.Width") as the two factors for clustering.
kmeansCluster <- kmeans(iris[,c("Petal.Length","Petal.Width")], centers = 3)

## extract cluster information to iris data.
iris$cluster <- kmeansCluster$cluster

## Also to demostrate the search feature of web-side chart, we add an column to iris with Species plus the id.
iris$name <- paste( iris$Species, 1:length(iris$Species), sep="_")
## extract cluster center info.
center <- kmeansCluster$center
## using mvisClusterChart function to generate mvis object.
m3 <- mvisClusterChart(iris, xvar = "Petal.Length", yvar = "Petal.Width",
		clustervar = "cluster", namevar = "name", centerDf = center)
plot(m3)
}
\author{
  Zhou Yang \email{zhouyanga9 At gmail.com}

  Jian Li \email{jian.li At 188.com}
}

