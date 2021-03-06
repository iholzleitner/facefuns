% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facefuns3d.R
\name{facefuns3d}
\alias{facefuns3d}
\title{Quick start to shape analyses of 3-D data}
\usage{
facefuns3d(data, pc_criterion = "broken_stick", quiet = FALSE)
}
\arguments{
\item{data}{Three-dimensional array of dimensions p, k, and n. p = number of landmarks, k = dimension (2D or 3D), n = number of specimens}

\item{pc_criterion}{Criterion used to choose which PCs to retain. See \link[facefuns]{select_pcs}}

\item{quiet}{If false prints short summary of loaded data}
}
\value{
Returns a list of the following components:
\item{array}{Three-dimensional array containing Procrustes-aligned data}
\item{average}{Coordinates of sample average for plotting}
\item{pc_info}{List of selected PCs (including their SD, variance explained and cumulative variance explained), number of selected PCs, criterion used to select PCs}
\item{pc_scores}{Principal component scores}
\item{pc_plot}{PCs for plotting. Will by default create list of coordinates for all selected PCs at +/- 3SDs. To create plots of other PCs or at different level of SD, please see \link[facefuns]{plot_2dpcs}}
\item{summary}{Short summary of key descriptives}
}
\description{
\lifecycle{maturing}

Performs some of the routine steps for getting landmark data ready for shape analyses, such as Procrustes alignment and principal component analysis. For more details see vignettes
\code{vignette("facefuns", package = "facefuns")} and \code{vignette("intro3d", package = "facefuns")}
}
\keyword{internal}
