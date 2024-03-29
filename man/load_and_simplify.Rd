% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_and_simplify.R, R/load_and_simplify_dir.R
\name{load_and_simplify}
\alias{load_and_simplify}
\alias{load_and_simplify_dir}
\title{Load YAML fragments in one or multiple files and simplify them}
\usage{
load_and_simplify(
  text,
  file,
  yamlFragments = NULL,
  select = ".*",
  simplify = ".*",
  delimiterRegEx = "^---$",
  ignoreOddDelimiters = FALSE,
  encoding = "UTF-8",
  silent = TRUE
)

load_and_simplify_dir(
  path,
  recursive = TRUE,
  fileRegexes = c("^[^\\\\.]+.*$"),
  select = ".*",
  simplify = ".*",
  delimiterRegEx = "^---$",
  ignoreOddDelimiters = FALSE,
  encoding = "UTF-8",
  silent = TRUE
)
}
\arguments{
\item{text}{As \code{text} or \code{file}, you can specify a \code{file} to read with
encoding \code{encoding}, which will then be read using \code{\link[base:readLines]{base::readLines()}}. If the
argument is named \code{text}, whether it is the path to an existing file is checked
first, and if it is, that file is read. If the argument is named \code{file}, and it
does not point to an existing file, an error is produced (useful if calling
from other functions). A \code{text} should be a character vector where every
element is a line of the original source (like provided by \code{\link[base:readLines]{base::readLines()}});
although if a character vector of one element \emph{and} including at least one
newline character (\verb{\\\\n}) is provided as \code{text}, it is split at the newline
characters using \code{\link[base:strsplit]{base::strsplit()}}. Basically, this behavior means that the
first argument can be either a character vector or the path to a file; and if
you're specifying a file and you want to be certain that an error is thrown if
it doesn't exist, make sure to name it \code{file}.}

\item{file}{As \code{text} or \code{file}, you can specify a \code{file} to read with
encoding \code{encoding}, which will then be read using \code{\link[base:readLines]{base::readLines()}}. If the
argument is named \code{text}, whether it is the path to an existing file is checked
first, and if it is, that file is read. If the argument is named \code{file}, and it
does not point to an existing file, an error is produced (useful if calling
from other functions). A \code{text} should be a character vector where every
element is a line of the original source (like provided by \code{\link[base:readLines]{base::readLines()}});
although if a character vector of one element \emph{and} including at least one
newline character (\verb{\\\\n}) is provided as \code{text}, it is split at the newline
characters using \code{\link[base:strsplit]{base::strsplit()}}. Basically, this behavior means that the
first argument can be either a character vector or the path to a file; and if
you're specifying a file and you want to be certain that an error is thrown if
it doesn't exist, make sure to name it \code{file}.}

\item{yamlFragments}{A character vector of class \code{yamlFragment} where
every element corresponds to one line of the YAML fragments, or a list
of multiple such character vectors (of class \code{yamlFragments}). Specify
either \code{yamlFragments} (which, if specified, takes precedence over \code{file}
and \code{text}), \code{file}, or \code{text} (\code{file} takes precedence over \code{text}).}

\item{select}{A vector of regular expressions specifying object names
to retain. The default (\verb{.*}) matches everything, so by default, all
objects are retained.}

\item{simplify}{A regular expression specifying which elements to
simplify (default is everything)}

\item{delimiterRegEx}{The regular expression used to locate YAML
fragments.}

\item{ignoreOddDelimiters}{Whether to throw an error (FALSE) or
delete the last delimiter (TRUE) if an odd number of delimiters is
encountered.}

\item{encoding}{The encoding to use when calling \code{\link[=readLines]{readLines()}}. Set to
NULL to let \code{\link[=readLines]{readLines()}} guess.}

\item{silent}{Whether to be silent (\code{TRUE}) or informative (\code{FALSE}).}

\item{path}{The path containing the files.}

\item{recursive}{Whether to also process subdirectories (\code{TRUE})
or not (\code{FALSE}).}

\item{fileRegexes}{A vector of regular expressions to match the files
against: only files matching one or more regular expressions in this
vector are processed. The default regex (\verb{^[^\\.]+.*$}) matches all
files except those that start with a period (\code{.}).}
}
\value{
A list of objects, where each object corresponds to one
item specified in the read YAML fragment(s) from the source file
or text. If the convention of the \code{rock}, \code{dct} and \code{justifier}
packages is followed, each object in this list contains one or
more named objects (lists), where the name indicates the type
of information contained. Each of those objects (lists) then
contains one or more objects of that type, such as metadata or
codes for \code{rock}, a decentralized construct taxonomy element
for \code{dct}, and a justification, decision, assertion, or source
for \code{justifier}.
}
\description{
These function extracts all YAML fragments from a file or text (\code{load_and_simplify})
or from all files in a directory (\code{load_and_simplify_dir}) and loads them
by calling \code{\link[=load_yaml_fragments]{load_yaml_fragments()}}, and then calls \code{\link[=simplify_by_flattening]{simplify_by_flattening()}},
on the result, returning the resulting list.
}
\examples{
yum::load_and_simplify(text="
---
firstObject:
  id: firstFragment
---
Outside of YAML
---
otherObjectType:
  -
    id: secondFragment
    parentId: firstFragment
  -
    id: thirdFragment
    parentId: firstFragment
---
Also outside of YAML");

}
