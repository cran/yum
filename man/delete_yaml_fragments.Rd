% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delete_yaml_fragments.R
\name{delete_yaml_fragments}
\alias{delete_yaml_fragments}
\title{Delete all YAML fragments from a file}
\usage{
delete_yaml_fragments(
  file,
  text,
  delimiterRegEx = "^---$",
  ignoreOddDelimiters = FALSE,
  silent = TRUE
)
}
\arguments{
\item{file}{The path to a file to scan; if provided, takes precedence
over \code{text}.}

\item{text}{A character vector to scan, where every element should
represent one line in the file; can be specified instead of \code{file}.}

\item{delimiterRegEx}{The regular expression used to locate YAML
fragments.}

\item{ignoreOddDelimiters}{Whether to throw an error (FALSE) or
delete the last delimiter (TRUE) if an odd number of delimiters is
encountered.}

\item{silent}{Whether to be silent (TRUE) or informative (FALSE).}
}
\value{
A list of character vectors.
}
\description{
These function deletes all YAML fragments from a file, returning
a character vector without the lines that specified the YAML
fragments.
}
\examples{
yum::delete_yaml_fragments(text=c("---", "First YAML fragment", "---",
                                   "Outside of YAML",
                                   "---", "Second fragment", "---",
                                   "Also outside of YAML"));

}
