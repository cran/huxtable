% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr.R
\name{mutate.huxtable}
\alias{mutate.huxtable}
\alias{mutate}
\alias{dplyr-verbs}
\title{Use dplyr verbs with huxtable objects}
\usage{
\method{mutate}{huxtable}(.data, ..., copy_cell_props = TRUE)
}
\arguments{
\item{.data}{A huxtable.}

\item{...}{Arguments passed to \code{\link[dplyr:mutate]{dplyr::mutate()}}.}

\item{copy_cell_props}{Logical: copy cell and column properties from existing
columns.}
}
\description{
Huxtable can be used with dplyr verbs \code{\link[dplyr:select]{dplyr::select()}}, \code{\link[dplyr:rename]{dplyr::rename()}},
\code{dplyr::relocate()}, \code{\link[dplyr:slice]{dplyr::slice()}}, \code{\link[dplyr:arrange]{dplyr::arrange()}}, \code{\link[dplyr:mutate]{dplyr::mutate()}}
and \code{\link[dplyr:mutate]{dplyr::transmute()}}. These will return huxtables. Other verbs like
\code{\link[dplyr:summarise]{dplyr::summarise()}} will simply return data frames as normal;
\code{\link[dplyr:pull]{dplyr::pull()}} will return a vector. \code{mutate} has an extra option, detailed
below.
}
\details{
If \code{mutate} creates new columns, and the argument \code{copy_cell_props}
is missing or \code{TRUE}, then cell and column properties will be copied from
existing columns to their left, if there are any. Otherwise, they will be the
standard defaults. Row and table properties, and properties of cells in
existing columns, remain unchanged.
}
\examples{
ht <- hux(a = 1:5, b = 1:5, c = 1:5, d = 1:5, add_colnames = FALSE)
bold(ht)[c(1, 3), ] <- TRUE
bold(ht)[, 1] <- TRUE
ht2 <- dplyr::select(ht, b:c)
ht2
bold(ht2)
ht3 <- dplyr::mutate(ht, x = a + b)
ht3
bold(ht3)
ht4 <- dplyr::mutate(ht, x = a + b,
      copy_cell_props = FALSE)
bold(ht4)
}
