#' @rdname load_and_simplify
#' @export
load_and_simplify_dir <- function(path,
                                  recursive = TRUE,
                                  fileRegexes = c("^[^\\.]+.*$"),
                                  select=".*",
                                  simplify = ".*",
                                  delimiterRegEx = "^---$",
                                  ignoreOddDelimiters = FALSE,
                                  encoding="UTF-8",
                                  silent=TRUE) {

  ### Do not yet select any elements;
  ### we do this afterwards ourselves.

  res <- load_yaml_dir(path=path,
                       recursive=recursive,
                       fileRegexes=fileRegexes,
                       select=".*",
                       delimiterRegEx = delimiterRegEx,
                       ignoreOddDelimiters = ignoreOddDelimiters,
                       encoding = encoding,
                       silent=silent);

  ### First remove the names of this list; `load_yaml_dir' names the
  ### elements using the filenames
  names(res) <- NULL;

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
