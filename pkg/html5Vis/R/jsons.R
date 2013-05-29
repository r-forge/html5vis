


.convertJsonList.Motion <- function(coreDf, idvar, timevar, xvar, yvar, colorvar, sizevar) {
	
	coreDf$COLOR_LEGEND <- coreDf[[colorvar]]
	coreDf$COLOR_VALUE <- .gg.color.hue(length(unique(coreDf$COLOR_LEGEND)))[as.numeric(as.factor(coreDf$COLOR_LEGEND))]
	dataList <- split(coreDf[order(coreDf[[idvar]], coreDf[[timevar]]), c(idvar, timevar, xvar, yvar, sizevar, "COLOR_VALUE", "COLOR_LEGEND")], 
			f = as.factor(coreDf[[idvar]]))
	
	timeDf <- data.frame(V1 = sort(unique(do.call("c", lapply(dataList, FUN = function(X) X[[timevar]])))), 
			stringsAsFactors  = FALSE)
	names(timeDf) <- timevar
	
	dataList <- lapply(dataList, FUN = function(X) merge(timeDf, X, all.x = TRUE))
	outList <- list()
	
	
	
	for (i in seq_along(dataList)) {
		tmpList <- list()
		tmpList$name <- names(dataList)[i]
		tmpList$color <- dataList[[i]]$COLOR_VALUE[!is.na(dataList[[i]]$COLOR_VALUE)][1]
		tmpList$legend <- dataList[[i]]$COLOR_LEGEND[!is.na(dataList[[i]]$COLOR_LEGEND)][1]
		tmpList$DotX <- lapply(1:nrow(dataList[[i]]), 
				FUN = function(X) c(as.character(dataList[[i]][[timevar]][X]), 
							ifelse(is.na(dataList[[i]][[xvar]][X]), 0, round(dataList[[i]][[xvar]][X], 3)))
		)
		tmpList$DotY <- lapply(1:nrow(dataList[[i]]), 
				FUN = function(X) c(as.character(dataList[[i]][[timevar]][X]), 
							ifelse(is.na(dataList[[i]][[yvar]][X]), 0, round(dataList[[i]][[yvar]][X], 3)))
		)
		tmpList$R <- lapply(1:nrow(dataList[[i]]), 
				FUN = function(X) c(as.character(dataList[[i]][[timevar]][X]), 
							ifelse(is.na(dataList[[i]][[sizevar]][X]), 0, round(dataList[[i]][[sizevar]][X], 3)))
		)
		outList[[i]] <- tmpList
	}
	return(outList)
}


# coreDf <- iris; xvar = "Petal.Length"; yvar = "Petal.Width"; 
# clustervar = "cluster";  namevar = "name"; centerDf = center
.convertJsonList.Cluster <- function(coreDf, xvar, yvar, clustervar,
		namevar, centerDf, xlab, ylab) {
	
	outList <- list()
	clusterData <- coreDf[c(xvar, yvar, clustervar)]
	
	if (is.null(namevar) || is.null(coreDf[[namevar]])) {
		pointName = 1:length(coreDf[,1])
	} else {
		pointName = coreDf[[namevar]]
	}
	
	clusterData$name = pointName
	
	if (is.null(centerDf)) {
		centerList <- lapply(unique(clusterData$cluster), 
				FUN = function(X) { 
					c(mean(clusterData[[xvar]][clusterData$cluster == X]),
							mean(clusterData[[yvar]][clusterData$cluster == X]))
				} 
		)
		centerDf = matrix(unlist(centerList), ncol= 2)
	} else {
		centerDf = as.matrix(centerDf, ncol=2)
		
		if (length(unique(clusterData$cluster)) !=  length(centerDf[,1])) {
			warning ("the length of centers is not equal to cluster numbers!")
			centerList <- lapply(unique(clusterData$cluster), 
					FUN = function(X){
						c(mean(clusterData[[xvar]][clusterData$cluster == X]),
								mean(clusterData[[yvar]][clusterData$cluster == X]))
					}
			)
			centerDf = matrix(unlist(centerList), ncol= 2)
		}
		
	}
	
	print(xlab)
	colnames(centerDf) <- NULL
	names(clusterData) <- c("DotX", "DotY", "cluster", "name")
	outList[["data"]] = as.matrix(clusterData)
	outList[["center"]] = centerDf
	outList[["label"]] = c(xlab, ylab)
	
	return(outList)
	
}


.convertJsonList.Corrplot <- function(corr, color ){
	
	if(!is.matrix(corr)&!is.data.frame(corr))
		stop("Need a matrix or data frame!")
	
	orderList = .orderListGenerate(corr, orderList = c("original", "AOE", "FPC", "hclust", "name"))
	if(is.null(color)){
		color = c("#053061", "#FFFFFF", "#67001F")
	}else{
		color = colorRampPalette(color)(3)
	}
	
	corr <- as.matrix(corr)
	colNames = colnames(corr)
	colnames(corr) = NULL
	rownames(corr) = NULL
	
	if (ncol(corr) == 1){
		outList <- list(
				matrixLength = ncol(corr),
				color = color,
				colNames = as.matrix(colNames),
				matrixData = corr,
				#method = method,
				#type = type,
				#order = order
				orderList = orderList
		)
		return (outList)
	}else{
		outList <- list(
				matrixLength = ncol(corr),
				color = color,
				colNames = colNames,
				matrixData = corr,
				#method = method,
				#type = type,
				#order = order
				orderList = orderList
		)
		
		return (outList)
		
	}
}



