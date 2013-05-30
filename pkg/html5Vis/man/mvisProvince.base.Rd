\name{mvisProvince.base}
\alias{mvisProvince.base}
\title{motion Chart for display time-series data.}
\usage{
  mvisProvince.base(data, provincevar = "province",
    datavar = "value",
    srcdir = getOption("mvis.html5.dir"))

  mvisProvince.base(data, provincevar = "province",
    datavar = "value",
    srcdir = getOption("mvis.html5.dir"))
}
\arguments{
  \item{data:}{a data.frame. The data has to have at least
  four columns with subject name (idvar), time (timevar)
  and two columns of numeric values. Further columns,
  numeric and character/factor are optional. The
  combination of idvar and timevar has to describe a unique
  row. The column names of the idvar and timevar have to be
  specified. Further columns, if not specified by the other
  arguments (xvar, yvar, colorvar, sizevar), will be
  assumed to be in the order of the arguments.}

  \item{idvar:}{column name of data with the subject to be
  analysed.}

  \item{xvar:}{column name of a numerical vector in data to
  be plotted on the x-axis.}

  \item{yvar:}{column name of a numerical vector in data to
  be plotted on the y-axis.}

  \item{colorvar:}{column name of data that identifies
  bubbles in the same series. Use the same value to
  identify all bubbles that belong to the same series;
  bubbles in the same series will be assigned the same
  color. Series can be configured using the series option.}

  \item{sizevar:}{values in this column are mapped to
  actual pixel values using the sizeAxis option.}

  \item{data:}{a data.frame. The data has to have at least
  four columns with subject name (idvar), time (timevar)
  and two columns of numeric values. Further columns,
  numeric and character/factor are optional. The
  combination of idvar and timevar has to describe a unique
  row. The column names of the idvar and timevar have to be
  specified. Further columns, if not specified by the other
  arguments (xvar, yvar, colorvar, sizevar), will be
  assumed to be in the order of the arguments.}

  \item{idvar:}{column name of data with the subject to be
  analysed.}

  \item{xvar:}{column name of a numerical vector in data to
  be plotted on the x-axis.}

  \item{yvar:}{column name of a numerical vector in data to
  be plotted on the y-axis.}

  \item{colorvar:}{column name of data that identifies
  bubbles in the same series. Use the same value to
  identify all bubbles that belong to the same series;
  bubbles in the same series will be assigned the same
  color. Series can be configured using the series option.}

  \item{sizevar:}{values in this column are mapped to
  actual pixel values using the sizeAxis option.}
}
\value{
  the function will generate an html object, which could
  display by plot function.

  the function will generate an html object, which could
  display by plot function.
}
\description{
  This mvisMotionChart function can generate
  user-interactive plots which displays the googleVis style
  motionChart in HTML5 style. It is a backup strategy for
  displaying the time-series data.

  This mvisMotionChart function can generate
  user-interactive plots which displays the googleVis style
  motionChart in HTML5 style. It is a backup strategy for
  displaying the time-series data.
}
\examples{
#'demo plots for mvisMotionChart
## Define which columns are used for the initial setup of the various
## dimensions
M7 <- mvisMotionChart(Fruits, idvar="Fruit", timevar="Year",
                    xvar="Profit", yvar="Expenses",
                    colorvar="Location", sizevar="Sales")
plot(M7)
#'demo plots for mvisMotionChart
## Define which columns are used for the initial setup of the various
## dimensions
M7 <- mvisMotionChart(Fruits, idvar="Fruit", timevar="Year",
                    xvar="Profit", yvar="Expenses",
                    colorvar="Location", sizevar="Sales")
plot(M7)
}
\author{
  Zhou Yang \email{zhouyanga9 AT gmail.com}

  Jian Li \email{rweibo AT sina.com}

  Zhou Yang \email{zhouyanga9 AT gmail.com}

  Jian Li \email{rweibo AT sina.com}
}

