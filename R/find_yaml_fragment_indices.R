#' Find the indices ('line numbers') of all YAML fragments from a file
#'
#' These function finds all YAML fragments from a file, returning
#' their start and end indices or all indices of all lines in the (non-)YAML
#' fragments.
#'
#' @param file The path to a file to scan; if provided, takes precedence
#' over `text`.
#' @param text A character vector to scan, where every element should
#' represent one line in the file; can be specified instead of `file`.
#' @param invert Set to `TRUE` to return the indices of the character
#' vector that are *not* YAML fragments.
#' @param returnFragmentIndices Set to `TRUE` to return all indices of the
#' relevant fragments (i.e. including intermediate indices).
#' @param returnPairedIndices Whether to return two vectors with the
#' start and end indices, or pair them up in vectors of 2.
#' @param delimiterRegEx The regular expression used to locate YAML
#' fragments.
#' @param ignoreOddDelimiters Whether to throw an error (FALSE) or
#' delete the last delimiter (TRUE) if an odd number of delimiters is
#' encountered.
#' @param silent Whether to be silent (TRUE) or informative (FALSE).
#'
#' @return A list of numeric vectors with start and end indices
#' @examples ### Create simple text vector with the right delimiters
#' simpleExampleText <-
#'   c(
#'     "---",
#'     "First YAML fragment",
#'     "---",
#'     "Outside of YAML",
#'     "This, too.",
#'     "---",
#'     "Second fragment",
#'     "---",
#'     "Also outside of YAML",
#'     "Another one outside",
#'     "Last one"
#'   );
#'
#' yum::find_yaml_fragment_indices(
#'   text=simpleExampleText
#' );
#'
#' yum::find_yaml_fragment_indices(
#'   text=simpleExampleText,
#'   returnFragmentIndices = FALSE
#' );
#'
#' yum::find_yaml_fragment_indices(
#'   text=simpleExampleText,
#'   invert = TRUE
#' );
#'
#' @export
find_yaml_fragment_indices <- function(file,
                                       text,
                                       invert = FALSE,
                                       returnFragmentIndices = TRUE,
                                       returnPairedIndices = TRUE,
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
    if (returnFragmentIndices) {
      if (invert) {
        return(seq_along(allLines));
      } else {
        return(NULL);
      }
    } else {
      if (invert) {
        if (returnPairedIndices) {
          return(list(c(0, length(allLines))));
        } else {
          return(list(0, length(allLines)));
        }
      } else {
        if (returnPairedIndices) {
          return(NULL);
        } else {
          return(NULL);
        }
      }
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

  indexPairIndices <-
    list(yamlFragments[is.odd(yamlFragmentIndices)],
         yamlFragments[is.even(yamlFragmentIndices)]);

  if (invert) {

    corrected_yamlStarts <-
      indexPairIndices[[1]] - 1;

    corrected_yamlEnds <-
      indexPairIndices[[2]] + 1;

    if (corrected_yamlStarts[1] < 2) {
      firstRegularLine <- corrected_yamlEnds[1];
      corrected_yamlStarts <- corrected_yamlStarts[-1];
    } else {
      firstRegularLine <- 1;
    }

    if (utils::tail(corrected_yamlEnds, 1) > length(allLines)) {
      lastRegularLine <- utils::tail(corrected_yamlStarts, 1);
      corrected_yamlEnds <- utils::head(corrected_yamlEnds, -1);
    } else {
      lastRegularLine <- length(allLines);
    }

    nonYamlStarts <-
      unique(c(firstRegularLine, corrected_yamlEnds));

    nonYamlEnds <-
      unique(c(corrected_yamlStarts, lastRegularLine));

    indexPairIndices <-
      list(nonYamlStarts,
           nonYamlEnds);

  }

  if (returnFragmentIndices) {

    res <-
      lapply(seq_along(indexPairIndices[[1]]),
             function(pair) {
               return(seq(indexPairIndices[[1]][pair],
                          indexPairIndices[[2]][pair]));
             });

  } else if (returnPairedIndices) {

    res <-
      lapply(seq_along(indexPairIndices[[1]]),
             function(pair) {
               return(c(indexPairIndices[[1]][pair],
                        indexPairIndices[[2]][pair]));
             });

  } else {

    res <- indexPairIndices;

  }

  return(res);

}
