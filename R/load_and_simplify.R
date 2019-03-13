#' Load YAML fragments in one or multiple files and simplify them
#'
#' These function extracts all YAML fragments from a file or text (`load_and_simplify`)
#' or from all files in a directory (`load_and_simplify_dir`) and loads them
#' by calling [load_yaml_fragments()], and then calls [simplify_by_flattening()],
#' on the result, returning the resulting list.
#'
#' @inheritParams load_yaml_fragments
#' @inheritParams load_yaml_dir
#' @inheritParams simplify_by_flattening
#'
#' @return A list of objects, where each object corresponds to one
#' item specified in the read YAML fragment(s) from the source file
#' or text. If the convention of the `rock`, `dct` and `justifier`
#' packages is followed, each object in this list contains one or
#' more named objects (lists), where the name indicates the type
#' of information contained. Each of those objects (lists) then
#' contains one or more objects of that type, such as metadata or
#' codes for `rock`, a decentralized construct taxonomy element
#' for `dct`, and a justification, decision, assertion, or source
#' for `justifier`.
#' @rdname load_and_simplify
#' @examples
#' yum::load_and_simplify(text="
#' ---
#' firstObject:
#'   id: firstFragment
#' ---
#' Outside of YAML
#' ---
#' otherObjectType:
#'   -
#'     id: secondFragment
#'     parentId: firstFragment
#'   -
#'     id: thirdFragment
#'     parentId: firstFragment
#' ---
#' Also outside of YAML");
#'
#' @export
load_and_simplify <- function(text,
                              file,
                              yamlFragments=NULL,
                              select=".*",
                              simplify = ".*",
                              delimiterRegEx = "^---$",
                              ignoreOddDelimiters = FALSE,
                              encoding="UTF-8",
                              silent=TRUE) {

  # if (!requireNamespace("yaml", quietly = TRUE)) {
  #   stop("To parse YAML content, the \"yaml\" package is required. ",
  #        "Please install it using `install.packages('yaml');`.",
  #        call. = FALSE);
  # }

  ### When calling these functions, we do not yet select any elements;
  ### we do this afterwards ourselves.

  if (!is.null(yamlFragments)) {
    res <- load_yaml_fragments(yamlFragments=yamlFragments,
                               select=".*",
                               delimiterRegEx=delimiterRegEx,
                               ignoreOddDelimiters=ignoreOddDelimiters,
                               encoding=encoding,
                               silent=silent);
  } else if ((!missing(file)) || (!missing(text))) {
    if (!missing(text)) {
      res <- load_yaml_fragments(text=text,
                                 select=".*",
                                 delimiterRegEx=delimiterRegEx,
                                 ignoreOddDelimiters=ignoreOddDelimiters,
                                 encoding=encoding,
                                 silent=silent);
    } else if (!missing(file)) {
      res <- load_yaml_fragments(file=file,
                                 select=".*",
                                 delimiterRegEx=delimiterRegEx,
                                 ignoreOddDelimiters=ignoreOddDelimiters,
                                 encoding=encoding,
                                 silent=silent);
    }
  } else {
    stop("Provide either a `file` or a `text` to scan!");
  }

  res <-
    simplify_by_flattening(res,
                           simplify = simplify);

  res <-
    res[grep(select,
             names(res))];

  if (is.null(res)) {
    res <- list();
  }

  class(res) <-
    c("simplifiedYum", "list");

  return(res);

}
