% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_tem_to_tps.R
\name{convert_tem_to_tps}
\alias{convert_tem_to_tps}
\title{Read Webmorph template files}
\usage{
convert_tem_to_tps(path_to_tem, remove_points = NA, path_to_tps = NA)
}
\arguments{
\item{path_to_tem}{Path to directory containing template files}

\item{remove_points}{Vector containing number of landmarks to be deleted. If no value is entered, all landmarks are retained}

\item{path_to_tps}{Name and path of TPS file to be created}
}
\description{
Reads Webmorph templates and converts them to TPS. For more details see the corresponding vignette:
\code{vignette("TEMtoTPS", package = "facefuns")}
}
