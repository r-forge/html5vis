
##' Run package regression tests
##'
##' Executes the package regression tests, and produces a html report. The test suite is run with the RUnit package.
##' @title Run package regression tests
##' @param unitTestPath Path where the scripts are located.  By default, will use the package installation directory.
##' @param printTestProtocol Logical flag.  Should HTML reports be produced?
##' @return The results of executing the test suites via the function \code{runTestSuite}. 
##' @author Mango Solutions
##' @keywords debugging
##' @examples
##' x <- googleVis.Mango.Test(printTestProtocol = FALSE)
##' summary(x)
##' 


googleVis.Mango.Test <- function(unitTestPath = system.file(package="googleVis.Mango", "unittests"), printTestProtocol = FALSE)
{
	stopifnot(require("RUnit", quietly = TRUE))
	print (unitTestPath)
	testSuite <- defineTestSuite("googleVis.Mango unit test suite", dirs = unitTestPath, testFileRegexp = "^runit\\..+\\.[rR]$")
	res <- runTestSuite(testSuite)
	if(printTestProtocol) printHTMLProtocol(res, fileName = "googleVis.Mango.html" )
	
	res
}



