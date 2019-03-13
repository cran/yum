#' Load all YAML fragments from all character vectors in a list
#'
#' These function extracts all YAML fragments from character vectors
#' in a list, returning a list of character vectors containing the
#' extracted fragments.
#'
#' This function calls [yaml::yaml.load()] on all character vectors
#' in a list. It then returns a list where each element is a list
#' with the parsed fragments in a file.
#'
#' @param x The list containing the character vectors.
#' @param recursive Whether to first `unlist` the list (`TRUE`)
#' or not (`FALSE`).
#' @inheritParams load_yaml_fragments
#'
#' @return A list of lists of objects.
#' @examples
#' yamlList <- list(c(
#' "---",
#' "-",
#' "  id: firstFragment",
#' "---"), c(
#' "---",
#' "-",
#' "  id: secondFragment",
#' "  parentId: firstFragment",
#' "---"));
#' yum::load_yaml_list(yamlList);
#' @export
load_yaml_list <- function(x,
                           recursive = TRUE,
                           select=".*",
                           delimiterRegEx = "^---$",
                           ignoreOddDelimiters = FALSE,
                           encoding="UTF-8",
                           silent=TRUE) {

  if (recursive) {
    x <-
      flatten_list_of_lists(x);
  }

  res <-
    lapply(x,
           function(obj) {
             if (("yamlFragment" %in% class(obj)) ||
                 ("yamlFragments" %in% class(obj))) {
               return(load_yaml_fragments(yamlFragments=obj,
                                          text=NULL,
                                          file=NULL,
                                          select=select,
                                          delimiterRegEx = delimiterRegEx,
                                          ignoreOddDelimiters = ignoreOddDelimiters,
                                          encoding = encoding,
                                          silent=silent));
             }
           });

  names(res) <-
    names(x);

  class(res) <-
    c("yumFromList", "list");

  return(res);

}
