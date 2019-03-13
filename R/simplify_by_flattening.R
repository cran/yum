#' Simplify the structure of extracted YAML fragments
#'
#' This function does some cleaning and simplifying to allow
#' efficient specification of elements in the YAML fragments.
#'
#' @param x Extracted (and loaded) YAML fragments
#' @param simplify A regular expression specifying which elements to
#' simplify (default is everything)
# #' @param stopOnError Whether to give an error and stop when encountering
# #' an unexpected structure or only give a warning.
#' @param .level Internal argument to enable slightly-less-than-elegant 'recursion'.
#'
#' @return A simplified list (but still a list)
#' @export
#'
#' @examples yamlFragmentExample <- '
#' ---
#' source:
#'   -
#'     id: src_1
#'     label: "Label 1"
#'   -
#'     id: src_2
#'     label: "Label 2"
#' assertion:
#'   -
#'     id: assertion_1
#'     label: "Assertion 1"
#'   -
#'     id: assertion_2
#'     label: "Assertion 2"
#' ---
#' ';
#' loadedExampleFragments <-
#'   load_yaml_fragments(yamlFragmentExample);
#' simplified <-
#'   simplify_by_flattening(loadedExampleFragments);
#'
#' ### Pre simmplification:
#' str(loadedExampleFragments);
#'
#' ### Post simmplification:
#' str(simplified);
#'
simplify_by_flattening <- function(x,
                                   simplify = ".*",
#                                   stopOnError=TRUE,
                                   .level=1) {

  ### Normally, if the elements of the provided object (at 'root level')
  ### have no names, we've been provided with an object containing
  ### several distinct YAML fragments. Process those separately and
  ### concatenate the resulting objects together.
  if (is.null(names(x))) {
    # if (.level == 1) {
      return(do.call(c,
                     lapply(x,
                            simplify_by_flattening,
                            simplify=simplify,
                            .level=2)));
    # } else {
    #   errMsg <-
    #     "Objects at the second level must have names to be able to simplify, but they don't!";
    #   if (stopOnError) {
    #     stop(errMsg);
    #   } else {
    #     warning(errMsg);
    #   }
    # }
  }

  ### If we get to this point, the elements of this entity have names;
  ### therefore, this is the 'second call' if we've originall been called
  ### with an object containing several YAML fragments. Therefore,
  ### apply all names of these objects to children without names

  return(do.call(c,
                 lapply(names(x),
                        function(currentName) {
                          ### If the children have names as well, return this
                          ### object as is (which effectively lifts it to the
                          ### 'root' level
                          if (!is.null(names(x[[currentName]]))) {
                            ### Add one level that will then be removed when
                            ### concatenating after having returned the result
                            res <-
                              list(x[[currentName]]);
                            names(res) <- currentName;
                            return(res);
                          } else {
                            ### In this case, the children have no names (aw...), and
                            ### so either give them all the current name if we match
                            ### the regex (assume these are all objects of that type),
                            ### or add a level if we don't match.

                            ### Only rename children if we satisfy the regex
                            if (grepl(simplify,
                                      currentName)) {
                              names(x[[currentName]]) <-
                                rep(currentName,
                                    length(x[[currentName]]));
                              return(x[[currentName]]);
                            } else {
                              ### If we don't rename and extract here, add one
                              ### level that will then be removed when concatenating
                              res <-
                                list(x[[currentName]]);
                              names(res) <- currentName;
                              return(res);
                            }
                          }
                        })));

}
