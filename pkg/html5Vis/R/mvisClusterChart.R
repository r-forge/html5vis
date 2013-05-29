#' ggplot2-style Cluster Chart
#' This function can generate ggplot2-style Cluster Chart. 
#'
#' @param data : the cluster result data.
#' @param xvar : name of the character column which should displayed for X.
#' @param yvar : name of the character column which should displayed for Y.
#' @param clustervar: name of the character column which contains the class information.
#' @param namevar: name of the character column which contains the name information, 
#' it could be used for searching on the chart. it could be contained in data.frame, and
#' it could be a vector which have the same length with the number of data.frame rows.
#' @param centerDf: the center of cluster result.
#' @param xlab: a title for the x axis.
#' @param ylab: a title for the y axis.
#' @param srcdir: getOption("mvis.html5.dir"), the path of html5 file path.
#' @author Zhou Yang \email{zhouyanga9 At gmail.com}
#' @author Jian Li \email{jian.li At 188.com}
#' @return the function will generate an html object, which could display by plot function.
#' 
#' @examples #'demo plots for mvisClusterChart
#'
#' 
#' library(html5Vis)
#'
#' ## use iris as demo data, and use c("Petal.Length","Petal.Width") as the two factors for clustering. 
#' kmeansCluster <- kmeans(iris[,c("Petal.Length","Petal.Width")], centers = 3) 
#'
#' ## extract cluster information to iris data.
#' iris$cluster <- kmeansCluster$cluster
#'
#' ## Also to demostrate the search feature of web-side chart, we add an column to iris with Species plus the id.
#' iris$name <- paste( iris$Species, 1:length(iris$Species), sep="_")
#' ## extract cluster center info.
#' center <- kmeansCluster$center
#' ## using mvisClusterChart function to generate mvis object.
#' m3 <- mvisClusterChart(iris, xvar = "Petal.Length", yvar = "Petal.Width", 
#' 		clustervar = "cluster", namevar = "name", centerDf = center)
#' plot(m3)
#' 

mvisClusterChart <- function(data,  xvar = "x", yvar = "y", clustervar = "cluster", 
		namevar = NULL, centerDf = NULL, xlab = NULL, ylab = NULL,
		srcdir = getOption("mvis.html5.dir")
) {

	templatedir = getOption("mvis.template.dir")
	charttype <- "html5Vis.Cluster"
	chartid <- paste(charttype, basename(tempfile(pattern = "")), sep = "ID")
	
	if (is.null(xlab)){xlab =  xvar}
	if (is.null(ylab)){ylab =  yvar}

	jsonList <- .convertJsonList.Cluster(data, xvar, yvar, clustervar, namevar, centerDf, xlab, ylab)
	jsonStr <- toJSON(jsonList)
	
	headerHtml <- readLines(file.path(templatedir, "header.html"))
	footerHtml <- readLines(file.path(templatedir, "footer.html"))
	captionHtml <- readLines(file.path(templatedir, "caption.html"))
	chartHtml <- readLines(file.path(templatedir, "chart.cluster.html"))

	headerStr <- gsub("HEADER", chartid, headerHtml)
	headerStr <- gsub("HTML5FOLDERHERE", "html5", headerStr)
	footerStr <- footerHtml
	captionStr <- gsub("CHARTID", chartid, captionHtml)
	chartStr <- gsub("HTML5FOLDERHERE", "html5", chartHtml)
	chartStr <- gsub("<!--JSONHERE-->", paste("var outputData =", jsonStr), chartStr)
	
	headerStr <- paste(headerStr, collapse = "\n")
	footerStr <- paste(footerStr, collapse = "\n")
	captionStr <- paste(captionStr, collapse = "\n")
	chartStr <- paste(chartStr, collapse = "\n")

	outList <- list()
	outList$type <- charttype
	outList$chartid <- chartid
	outList$html <- list(header = headerStr, chart = chartStr, caption = captionStr, footer = footerStr)
	
	class(outList) <- c("mvis",  "list")
	return(outList)
}


