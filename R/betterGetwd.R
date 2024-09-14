#' @title Better getwd()
#'
#' @return An address with all the escape slashes already added for you.
#' @export
#'
#' @examples
mygetwd <- function() {
  address <- getwd()
  gsub("/", "//", address)
}
