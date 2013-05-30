\name{html5Vis.Test}
\alias{html5Vis.Test}
\title{Run package regression tests}
\usage{
  html5Vis.Test(unitTestPath = system.file(package = "html5Vis", "unittests"),
    printTestProtocol = FALSE)
}
\arguments{
  \item{unitTestPath}{Path where the scripts are located.
  By default, will use the package installation directory.}

  \item{printTestProtocol}{Logical flag.  Should HTML
  reports be produced?}
}
\value{
  The results of executing the test suites via the function
  \code{runTestSuite}.
}
\description{
  Run package regression tests
}
\details{
  Executes the package regression tests, and produces a
  html report. The test suite is run with the RUnit
  package.
}
\examples{
x <- googleVis.Mango.Test(printTestProtocol = FALSE)
summary(x)
}
\author{
  Mango Solutions
}
\keyword{debugging}

