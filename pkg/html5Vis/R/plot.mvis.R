


plot.mvis <- function (x, tag = NULL, ...) 
{
	if (missing(tag)) 
		tag <- getOption("mvis.plot.tag")
	if (is.null(tag) | !("mvis" %in% class(x))) {
		if (!googleVis:::isServerRunning()) {
			tools:::startDynamicHelp()
		}
		env <- get(".httpd.handlers.env", asNamespace("tools"))
		env[["googleVis"]] <- .mvis.httpd.handler
		root.dir <- tempdir()
		print(paste("chart path", root.dir))
		if (!file.exists(root.dir)) dir.create(root.dir, recursive = TRUE)
		if (x$type == "geoVis") {
			file.copy(getOption("mvis.geoData.dir"), root.dir, recursive = TRUE)
		}
		if ("mvis" %in% class(x)) {
			chart.txt <- "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n  <title>%s</title>\n  <meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\" />\n  <style type=\"text/css\">\n    body {\n          color: #444444;\n          font-family: Arial,Helvetica,sans-serif;\n          font-size: 75%%;\n    }\n    a {\n          color: #4D87C7;\n          text-decoration: none;\n    }\n  </style>\n</head>\n<body>\n<p>\n  You find below the HTML code of the visualisation.<br />\n  You can copy and paste the code into an existing HTML page.<br />\n  For more information see also <a href=\"/library/googleVis/html/gvisMethods.html\">?print.mvis</a></p>\n<p><textarea rows=\"50\" name=\"html\" cols=\"80\">\n%s\n</textarea>\n</p>\n</body>\n</html>\n"
			chart.txt <- sprintf(chart.txt, x$chartid, gsub(">", 
							"&gt;", gsub("<", "&lt;", paste(unlist(x$html$chart), 
											collapse = "\n"))))
			cat(chart.txt, file = file.path(root.dir, paste("Chart_", 
									x$chartid, ".html", sep = "")))
			file <- file.path(root.dir, paste(x$chartid, ".html", 
							sep = ""))
		}
		else {
			basex <- basename(x)
			if (length(grep("htm", substr(basex, nchar(basex) - 
											3, nchar(basex)))) < 1) 
				warning("The file does not appear to be an html file.\n")
			file.copy(from = x, to = file.path(root.dir, basex), 
					...)
			file <- file.path(root.dir, basex)
		}
		print(x, file = file)
		.url <- sprintf("http://127.0.0.1:%s/custom/googleVis/%s", 
				tools:::httpdPort, basename(file))
		if (interactive()) {
			browseURL(.url, ...)
		}
		else {
			browseURL(.url, browser = "false", ...)
		}
		invisible(file)
	}
	else {
		if ("mvis" %in% class(x)) {
			return(print(x, tag = tag))
		}
		else {
			return(print(x))
		}
	}
}

print.mvis <- function (x, tag = NULL, file = "", ...) 
{
    if (is.null(tag)) 
        tag <- getOption("mvis.print.tag")
    if (!tag %in% getOption("mvis.tags"))
        stop(paste(tag, "is not a valid option. Set tag to NULL or one of the following:\n", 
            paste(getOption("mvis.tags"), collapse = ", ")))
    tag <- ifelse(tag %in% c("chartid", "type", "html"), tag, 
        paste(".", tag, sep = ""))
    output <- unlist(x)
    tag.names <- names(output)
    .id <- apply(t(tag), 2, function(y) grep(paste("\\", y, sep = ""), 
        tag.names))
    cat(output[.id], file = file, ...)
}



