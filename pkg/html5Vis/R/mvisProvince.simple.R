#' motion Chart for display time-series data.
#'
#' This mvisMotionChart function can generate user-interactive plots which displays the googleVis style motionChart
#' in HTML5 style. It is a backup strategy for displaying the time-series data.
#' 
#' @param data: a data.frame. The data has to have at least four columns with subject name (idvar), time (timevar) and two columns of numeric values. Further columns, numeric and character/factor are optional. The combination of idvar and timevar has to describe a unique row. 
#' The column names of the idvar and timevar have to be specified. Further columns, if not specified by the other arguments (xvar, yvar, colorvar, sizevar), will be assumed to be in the order of the arguments.
#' @param idvar: column name of data with the subject to be analysed.
#' @param xvar: column name of a numerical vector in data to be plotted on the x-axis.
#' @param yvar: column name of a numerical vector in data to be plotted on the y-axis.
#' @param colorvar: column name of data that identifies bubbles in the same series.
#' Use the same value to identify all bubbles that belong to the same series; 
#' bubbles in the same series will be assigned the same color. Series can be configured using the series option.
#' @param sizevar: values in this column are mapped to actual pixel values using the sizeAxis option.
#' @author Zhou Yang \email{zhouyanga9 AT gmail.com}
#' @author Jian Li \email{rweibo AT sina.com}
#' @return the function will generate an html object, which could display by plot function.
#' 
#' @examples #'demo plots for mvisMotionChart

#' ## Define which columns are used for the initial setup of the various
#' ## dimensions
#' M7 <- mvisMotionChart(Fruits, idvar="Fruit", timevar="Year",
#'                     xvar="Profit", yvar="Expenses",
#'                     colorvar="Location", sizevar="Sales")
#' plot(M7)


mvisProvince.base <- function(data, provincevar = "province", datavar = "value",
		srcdir = getOption("mvis.html5.dir")
) {

	templatedir = getOption("mvis.template.dir")
	charttype <- "geoVis"
	chartid <- paste(charttype, basename(tempfile(pattern = "")), sep = "ID")
	
	jsonList <- .convertJsonList.ProvinceDis.base(data, provincevar, datavar)
	jsonStr <- toJSON(jsonList)
	
	headerHtml <- readLines(file.path(templatedir, "header.html"))
	footerHtml <- readLines(file.path(templatedir, "footer.html"))
	captionHtml <- readLines(file.path(templatedir, "caption.html"))
	chartHtml <- readLines(file.path(templatedir, "chart.provinceDis.base.html"))

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
	
	class(outList) <-  c("mvis",  "list")
	return(outList)
}



