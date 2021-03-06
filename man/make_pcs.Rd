% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_pcs.R
\name{make_pcs}
\alias{make_pcs}
\title{Create list of PCs}
\usage{
make_pcs(pca_output, ref, which_pcs, vis_sd)
}
\arguments{
\item{pca_output}{Object of class \code{prcomp} or \code{gm.prcomp}}

\item{ref}{2-D or 3-D coordinates of reference face}

\item{which_pcs}{Which PCs are to be created. Single number or vector, maximum length is 3}

\item{vis_sd}{Extent of desired manipulation in units of standard deviation}
}
\value{
A list of 2-D or 3-D coordinates for the reference face at \code{-vis_SD} and \code{vis_SD} standard deviations for each principal component
}
\description{
\lifecycle{maturing}
}
\examples{

data(LondonSet_aligned)
data_aligned <- LondonSet_aligned
pca_output <- geomorph::gm.prcomp(data_aligned)
ref <- geomorph::mshape(data_aligned)
pc_list <- make_pcs(pca_output, ref, which_pcs = 1:3, vis_sd = 3)

}
