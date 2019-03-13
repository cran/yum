#' Extract all YAML fragments from a file
#'
#' These function extracts all YAML fragments from a file,
#' returning a list of character vectors containing the extracted
#' fragments.
#'
#' @param text,file As `text` or `file`, you can specify a `file` to read with
#' encoding `encoding`, which will then be read using [base::readLines()]. If the
#' argument is named `text`, whether it is the path to an existing file is checked
#' first, and if it is, that file is read. If the argument is named `file`, and it
#' does not point to an existing file, an error is produced (useful if calling
#' from other functions). A `text` should be a character vector where every
#' element is a line of the original source (like provided by [base::readLines()]);
#' although if a character vector of one element *and* including at least one
#' newline character (`\\n`) is provided as `text`, it is split at the newline
#' characters using [base::strsplit()]. Basically, this behavior means that the
#' first argument can be either a character vector or the path to a file; and if
#' you're specifying a file and you want to be certain that an error is thrown if
#' it doesn't exist, make sure to name it `file`.
#' @param delimiterRegEx The regular expression used to locate YAML
#' fragments.
#' @param ignoreOddDelimiters Whether to throw an error (FALSE) or
#' delete the last delimiter (TRUE) if an odd number of delimiters is
#' encountered.
#' @param encoding The encoding to use when calling [readLines()]. Set to
#' NULL to let [readLines()] guess.
#' @param silent Whether to be silent (`TRUE`) or informative (`FALSE`).
#'
#' @return A list of character vectors, where each vector corresponds to
#' one YAML fragment in the source file or text.
#' @examples
#' extract_yaml_fragments(text="
#' ---
#' First: YAML fragment
#'   id: firstFragment
#' ---
#' Outside of YAML
#' ---
#' Second: YAML fragment
#'   id: secondFragment
#'   parentId: firstFragment
#' ---
#' Also outside of YAML
#' ");
#' @export
extract_yaml_fragments <- function(text,
                                   file,
                                   delimiterRegEx = "^---$",
                                   ignoreOddDelimiters = FALSE,
                                   encoding="UTF-8",
                                   silent=TRUE) {

  if (missing(file)) {
    if (missing(text)) {
      stop("Provide either a `file` or a `text` to scan!");
    } else {
      if ((length(text) == 1) && file.exists(text)) {
        allLines <- readLines(text,
                              encoding=encoding,
                              warn=FALSE);
      } else {
        allLines <- text;
        if ((length(allLines) == 1) && grepl('\n', allLines)) {
          allLines <-
            strsplit(allLines,
                     "\n")[[1]];
        }
      }
    }
  } else {
    if (file.exists(file)) {
      allLines <- readLines(file,
                            encoding=encoding,
                            warn=FALSE);
    } else {
      stop("The file you specified in argument `file` ('",
           paste0(file, collapse=" "),
           "') does not exist. If you meant to provide a text ",
           "to process, please use argument `text`");
    }
  }

  yamlFragments <- grep(delimiterRegEx,
                        allLines);

  if (length(yamlFragments) == 0) {
    return(NULL);
  } else {
    if (!silent) {
      cat("Identified ", length(yamlFragments),
          " lines matching delimiterRegEx '",
          delimiterRegEx, "': ",
          vecTxt(yamlFragments),
          ".\n",
          sep="");
    }
  }

  if (!is.even(length(yamlFragments))) {
    if (ignoreOddDelimiters) {
      yamlFragments <-
        yamlFragments[-length(yamlFragments)];
    } else {
      stop("Extracted an uneven number of lines with specifications ",
           "(the regular expression for the specification ",
           "delimiter that was specified was '", delimiterRegEx,
           "'). To ignore the last delimiter, specify ",
           "'ignoreOddDelimiters=TRUE'.");
    }
  }

  yamlFragmentIndices <- seq_along(yamlFragments);

  if (length(yamlFragmentIndices) == 2) {
    indexSets <-
      list(seq(yamlFragments[1],
               yamlFragments[2]));
  } else {
    indexSets <-
      mapply(seq,
             yamlFragments[is.odd(yamlFragmentIndices)],
             yamlFragments[is.even(yamlFragmentIndices)],
             SIMPLIFY=FALSE);
  }

  res <-
    lapply(indexSets,
           function(i, x=allLines) {
             return(structure(x[i],
                              class="yamlFragment"));
           });

  class(res) <-
    c("yamlFragments", "list");

  return(res);

}
