#' Flatten a list of lists to a list of atomic vectors
#'
#' This function takes a hierarchical structure of lists and
#' extracts all atomic vectors, returning one flat list of all
#' those vectors.
#'
#' @param x The list of lists.
#'
#' @return A list of atomic vectors.
#' @export
#'
#' @examples ### First create a list of lists
#' listOfLists <-
#'   list(list(list(1:3, 8:5), 7:7), list(1:4, 8:2));
#' yum::flatten_list_of_lists(listOfLists);
flatten_list_of_lists <- function(x) {
  if (is.atomic(x)) {
    return(x);
  } else if (all(unlist(lapply(x, is.atomic)))) {
    return(x);
  } else {
    return(do.call(c,
                   lapply(x,
                          flatten_list_of_lists)));
  }
}
