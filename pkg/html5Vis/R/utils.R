

.mvis.httpd.handler <- function (path, query, ...) 
{
	path <- gsub("^/custom/googleVis/", "", path)
	f <- sprintf("%s%s%s", tempdir(), .Platform$file.sep, path)
	
	ext <- file_ext(path)
	contenttype <- switch(ext,
			"css" = "text/css",
			"gif" = "image/gif",
			"jpg" = "image/jpeg",
			"png" = "image/png",
			"svg" = "image/svg+xml",
			"html" = "text/html",
			"pdf" = "application/pdf",
			"ps" = "application/postscript", # in GLMMGibbs, mclust
			"sgml" = "text/sgml", # in RGtk2
			"xml" = "text/xml",  # in RCurl
			"text/plain")
	
	list(file = f, "content-type" = contenttype, "status code" = 200L)
}


.gg.color.hue <- function(n) {
	hues = seq(15, 375, length=n+1)
	hcl(h=hues, l=65, c=100)[1:n]
}



.orderListGenerate <- function (corr, orderList) 
{
	if (length(corr) == 1){
		returnList = list(original = matrix(1),
			AOE = matrix(1),
			FPC = matrix(1),
			name = matrix(1),
			hclust = matrix(1))
		return(returnList)
	}
    hclust.method <- "complete"
	
	returnList <- as.list(seq_along(orderList))
	names(returnList) <- orderList
	### Original Order
	returnList$original <- seq_along(colnames(corr))
	
	
	#### AOE Order
	x.eigen <- eigen(corr)$vectors[, 1:2]
    e1 <- x.eigen[, 1]
    e2 <- x.eigen[, 2]
    alpha <- ifelse(e1 > 0, atan(e2/e1), atan(e2/e1) + pi)
    ord <- rank(alpha)
	returnList$AOE <- ord
	
	### FPC Oder
    x.eigen <- eigen(corr)$vectors[, 1:2]
    e1 <- x.eigen[, 1]
    ord <- rank(e1)
	returnList$FPC <- ord
  
	### name Order
    returnList$name <- as.vector(rank(rownames(corr)))
	
	### hclust Order
    returnList$hclust <- order(order.dendrogram(as.dendrogram(hclust(as.dist(1 - 
            corr), method = hclust.method))))

    return(returnList)
}



