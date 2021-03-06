% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge-tools.R
\name{merge.with.order}
\alias{merge.with.order}
\title{Function merge data frames without loosing order of either X or Y frame
Shamelessly stolen from: https://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/}
\usage{
\method{merge}{with.order}(x, y, ..., sort = F, keep_order)
}
\arguments{
\item{x}{first data frame to merge}

\item{y}{second data frame to merge}

\item{sort}{whether to sort the column (defaults to FALSE)}

\item{keep_order}{keep order in x (1) or y (2)}

\item{by}{merge by single column}

\item{by.x}{merge by column in x}

\item{by.y}{merge by column in y}
}
\value{
merged data frame
}
\description{
Function merge data frames without loosing order of either X or Y frame
Shamelessly stolen from: https://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
}
