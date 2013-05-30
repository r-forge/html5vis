  
.onAttach <- function(libname, pkgname ){
	packageStartupMessage( paste("# html5Vis Version:", packageDescription("html5Vis", fields = "Version"), "\n") )
	
	

  
  ## This function are required to set option in googleVis functions,
  ## e.g. to set startZoom and stopZoom windows in gvisAnnotatedTimeLine

  
	
	#options(mvis.html5.dir = chartr("\\", "/", system.file("html5", package = "html5Vis")))
	options(mvis.geoData.dir = chartr("\\", "/", system.file("data", package = "html5Vis")))
	options(mvis.template.dir = chartr("\\", "/", system.file("template", package = "html5Vis")))
	options(mvis.images.dir = chartr("\\", "/", system.file("images", package = "html5Vis")))


	
	options(mvis.plot.tag=NULL)
	options(mvis.print.tag="html")
	
	mvis.tags <- c("type",  "chartid", "html", "header", "chart", "jsHeader", "jsData", "jsDrawChart", 
			"jsDisplayChart", "jsFooter", "jsChart", "divChart", "caption", "footer")
	options(mvis.tags = mvis.tags)
	
}



