#' Display the corrplot(created by Taiyun Wei) on the web side.
#' 
#' This mvisCorrplot function refers the corrplot package (Taiyun Wei), and output an user-interactive 
#' chart on the web-side.
#'
#'
#' @param corrmatrix: The correlation matrix to be visualized. 
#' @param color: the color vector which contains more than one color to generate gradual color.
#' The first element means the color for maxValue and the last(second) one means the minValue; 
#' @author Zhou Yang \email{zhouyanga9 At gmail.com}
#' @author Jian Li \email{jian.li At 188.com}
#' @return the function will generate an html object, which could display by plot function.
#' 
#' @examples #'demo plots for mvisCorrplot
#'
#' library(html5Vis)
#' ## load mtcars as the demo data.
#' data(mtcars)
#' m4 <- mvisCorrplotChart(cor(mtcars))
#' plot(m4)
#'
#' 


mvisCorrplotChart <- function(corrmatrix, color = NULL,
		srcdir = getOption("mvis.html5.dir")
) {

	templatedir = getOption("mvis.template.dir")
	charttype <- "html5Vis.Corrplot"
	chartid <- paste(charttype, basename(tempfile(pattern = "")), sep = "ID")
	
	jsonList <- .convertJsonList.Corrplot(corrmatrix, color)
	jsonStr <- toJSON(jsonList)
	
	headerHtml <- readLines(file.path(templatedir, "header.html"))
	footerHtml <- readLines(file.path(templatedir, "footer.html"))
	captionHtml <- readLines(file.path(templatedir, "caption.html"))
	chartHtml <- readLines(file.path(templatedir, "chart.corrplot.html"))

	headerStr <- gsub("HEADER", chartid, headerHtml)
	headerStr <- gsub("HTML5FOLDERHERE", "html5", headerStr)
	footerStr <- footerHtml
	captionStr <- gsub("CHARTID", chartid, captionHtml)
	chartStr <- gsub("HTML5FOLDERHERE", "html5", chartHtml)
	chartStr <- gsub("<!--JSONHERE-->", paste("var mvisCorrplotData =", jsonStr), chartStr)
	
	headerStr <- paste(headerStr, collapse = "\n")
	footerStr <- paste(footerStr, collapse = "\n")
	captionStr <- paste(captionStr, collapse = "\n")
	chartStr <- paste(chartStr, collapse = "\n")
	
	outList <- list()
	outList$type <- charttype
	outList$chartid <- chartid
	outList$html <- list(header = headerStr, chart = chartStr, caption = captionStr, footer = footerStr)
	
	class(outList) <- c("mvis", "list")
	return(outList)
}
