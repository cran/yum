% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_yaml_fragment_indices.R
\name{find_yaml_fragment_indices}
\alias{find_yaml_fragment_indices}
\title{Find the indices ('line numbers') of all YAML fragments from a file}
\usage{
find_yaml_fragment_indices(
  file,
  text,
  invert = FALSE,
  returnFragmentIndices = TRUE,
  returnPairedIndices = TRUE,
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

\item{invert}{Set to \code{TRUE} to return the indices of the character
vector that are \emph{not} YAML fragments.}

\item{returnFragmentIndices}{Set to \code{TRUE} to return all indices of the
relevant fragments (i.e. including intermediate indices).}

\item{returnPairedIndices}{Whether to return two vectors with the
start and end indices, or pair them up in vectors of 2.}

\item{delimiterRegEx}{The regular expression used to locate YAML
fragments.}

\item{ignoreOddDelimiters}{Whether to throw an error (FALSE) or
delete the last delimiter (TRUE) if an odd number of delimiters is
encountered.}

\item{silent}{Whether to be silent (TRUE) or informative (FALSE).}
}
\value{
A list of numeric vectors with start and end indices
}
\description{
These function finds all YAML fragments from a file, returning
their start and end indices or all indices of all lines in the (non-)YAML
fragments.
}
\examples{
### Create simple text vector with the right delimiters
simpleExampleText <-
  c(
    "---",
    "First YAML fragment",
    "---",
    "Outside of YAML",
    "This, too.",
    "---",
    "Second fragment",
    "---",
    "Also outside of YAML",
    "Another one outside",
    "Last one"
  );

yum::find_yaml_fragment_indices(
  text=simpleExampleText
);

yum::find_yaml_fragment_indices(
  text=simpleExampleText,
  returnFragmentIndices = FALSE
);

yum::find_yaml_fragment_indices(
  text=simpleExampleText,
  invert = TRUE
);

}
