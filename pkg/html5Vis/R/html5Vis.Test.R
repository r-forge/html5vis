
##' Run package regression tests
##'
##' Executes the package regression tests, and produces a html report. The test suite is run with the RUnit package.
##' @title Run package regression tests
##' @param unitTestPath Path where the scripts are located.  By default, will use the package installation directory.
##' @param printTestProtocol Logical flag.  Should HTML reports be produced?
##' @return The results of executing the test suites via the function \code{runTestSuite}. 
##' @author Yang Zhou
##' @keywords debugging
##' @examples
##' x <- html5Vis.Test(printTestProtocol = FALSE)
##' summary(x)
##' 

html5Vis.Test <- function(unitTestPath = system.file(package="html5Vis", "unittests"), printTestProtocol = FALSE)
{
	stopifnot(require("RUnit", quietly = TRUE))
	print (unitTestPath)
	testSuite <- defineTestSuite("html5Vis unit test suite", dirs = unitTestPath, testFileRegexp = "^runit\\..+\\.[rR]$")
	res <- runTestSuite(testSuite)
	if(printTestProtocol) printHTMLProtocol(res, fileName = "html5Vis.html" )
	
	res
}



