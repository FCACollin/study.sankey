#' Factor: NA is "Missing" level
#'
#' Assign the value "Missing" to missing values of a factor. Build as [addNA()].
#'
#' @param x (`factor`)
#' @param missing_lvl (`character`)
#'
#' @export
#' @examples
#'
#' animals <- as.factor(c("cat", "dog", NA))
#' animals
#' add_missing(animals)
#'
add_missing <- function(x, missing_lvl = "Missing") {

  assertthat::assert_that(is.factor(x))

  ll <- levels(x)
  ll <- c(ll, missing_lvl)
  x <- as.character(x)
  x <- ifelse(is.na(x), missing_lvl, x)
  factor(x, levels = ll, exclude = NULL)
}
