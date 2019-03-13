#' Load all YAML fragments from all files in a directory
#'
#' These function extracts all YAML fragments from all files in a
#' directory returning a list of character vectors containing the
#' extracted fragments.
#'
#' These function extracts all YAML fragments from all files in a
#' directory and then calls [yaml::yaml.load()] to parse them. It
#' then returns a list where each element is a list with the parsed
#' fragments in a file.
#'
#' @param path The path containing the files.
#' @param recursive Whether to also process subdirectories (`TRUE`)
#' or not (`FALSE`).
#' @param fileRegexes A vector of regular expressions to match the files
#' against: only files matching one or more regular expressions in this
#' vector are processed. The default regex (`^[^\.]+.*$`) matches all
#' files except those that start with a period (`.`).
#' @inheritParams extract_yaml_dir
#' @inheritParams load_yaml_fragments
#'
#' @return A list of lists of objects.
#' @examples ### First get the directory where 'yum' is installed
#' yumDir <- system.file(package="yum");
#' ### Specify the path of some example files
#' examplePath <- file.path(yumDir, "extdata");
#' ### Show files (should be three .dct files)
#' list.files(examplePath);
#' ### Load these files
#' yum::load_yaml_dir(path=examplePath);
#' @export
load_yaml_dir <- function(path,
                          recursive = TRUE,
                          fileRegexes = c("^[^\\.]+.*$"),
                          select=".*",
                          delimiterRegEx = "^---$",
                          ignoreOddDelimiters = FALSE,
                          encoding="UTF-8",
                          silent=TRUE) {

  if (!dir.exists(path)) {
    stop("Directory '",
         path,
         "' does not exist!");
  }

  fileList <-
    list.files(path=path,
               pattern=fileRegexes,
               recursive=recursive,
               full.names=TRUE);

  res <- lapply(fileList,
                load_yaml_fragments,
                select=select,
                delimiterRegEx = delimiterRegEx,
                ignoreOddDelimiters = ignoreOddDelimiters,
                encoding = encoding,
                silent=silent);

  names(res) <-
    fileList;

  class(res) <-
    c("yumFromDir", "list");

  return(res);

}
