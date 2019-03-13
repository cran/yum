### Basically what Marc Schwartz suggested at Thu Jul 1 19:10:28 CEST 2010
### on the R-help mailing list, see https://stat.ethz.ch/pipermail/r-help/2010-July/244299.html

#' Checking whether numbers are odd or even
#'
#' @param vector The vector to process
#'
#' @return A logical vector.
#' @rdname oddOrEven
#'
#' @examples is.odd(4);
#  is.even(4);
#' @export
is.odd <- function(vector) {
  return((vector %% 2) != 0);
}

#'@rdname oddOrEven
#'@export
is.even <- function(vector) {
  return((vector %% 2) == 0);
}
