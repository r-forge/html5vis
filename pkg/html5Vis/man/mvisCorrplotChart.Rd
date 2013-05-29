\name{mvisCorrplotChart}
\alias{mvisCorrplotChart}
\title{Display the corrplot(created by Taiyun Wei) on the web side.}
\usage{
  mvisCorrplotChart(corrmatrix, color = NULL,
    srcdir = getOption("mvis.html5.dir"))
}
\arguments{
  \item{corrmatrix:}{The correlation matrix to be
  visualized.}

  \item{color:}{the color vector which contains more than
  one color to generate gradual color. The first element
  means the color for maxValue and the last(second) one
  means the minValue;}
}
\value{
  the function will generate an html object, which could
  display by plot function.
}
\description{
  This mvisCorrplot function refers the corrplot package
  (Taiyun Wei), and output an user-interactive chart on the
  web-side.
}
\examples{
#'demo plots for mvisCorrplot

library(html5Vis)
## load mtcars as the demo data.
data(mtcars)
m4 <- mvisCorrplotChart(cor(mtcars))
plot(m4)
}
\author{
  Zhou Yang \email{zhouyanga9 At gmail.com}

  Jian Li \email{jian.li At 188.com}
}

