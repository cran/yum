#' Delete all YAML fragments from a file
#'
#' These function deletes all YAML fragments from a file, returning
#' a character vector without the lines that specified the YAML
#' fragments.
#'
#' @param file The path to a file to scan; if provided, takes precedence
#' over `text`.
#' @param text A character vector to scan, where every element should
#' represent one line in the file; can be specified instead of `file`.
#' @param delimiterRegEx The regular expression used to locate YAML
#' fragments.
#' @param ignoreOddDelimiters Whether to throw an error (FALSE) or
#' delete the last delimiter (TRUE) if an odd number of delimiters is
#' encountered.
#' @param silent Whether to be silent (TRUE) or informative (FALSE).
#'
#' @return A list of character vectors.
#' @examples
#' yum::delete_yaml_fragments(text=c("---", "First YAML fragment", "---",
#'                                    "Outside of YAML",
#'                                    "---", "Second fragment", "---",
#'                                    "Also outside of YAML"));
#'
#' @export
delete_yaml_fragments <- function(file,
                                  text,
                                  delimiterRegEx = "^---$",
                                  ignoreOddDelimiters = FALSE,
                                  silent=TRUE) {
  if (missing(file)) {
    if (missing(text)) {
      stop("Provide either a `file` or a `text` to scan!");
    } else {
      allLines <- text;
    }
  } else {
    allLines <- readLines(file);
  }

  yamlFragments <- grep(delimiterRegEx,
                        allLines);

  if (length(yamlFragments) == 0) {
    return(allLines);
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

  ### Rewritten using base R to remove `purrr` dependency
  # indexSets <- purrr::map2(.x=yamlFragments[is.odd(yamlFragmentIndices)],
  #                          .y=yamlFragments[is.even(yamlFragmentIndices)],
  #                          .f=`:`);

  indexPairIndices <-
    list(yamlFragments[is.odd(yamlFragmentIndices)],
         yamlFragments[is.even(yamlFragmentIndices)]);

  indexSets <-
    lapply(seq_along(indexPairIndices[[1]]),
           function(pair) {
             return(seq(indexPairIndices[[1]][pair],
                        indexPairIndices[[2]][pair]));
           });

  return(allLines[-do.call(c,
                           indexSets)]);

}
