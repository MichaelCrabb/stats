#' @title Better getwd()
#'
#' @return An address with all the escape slashes already added for you.
#' @export
#'
#' @examples
#' \dontrun{mygetwd()}
#'
bettergetwd <- function() {
  address <- getwd()
  gsub("/", "//", address)
}
