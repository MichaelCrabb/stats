#' Better getwd
#'
#' @return A valid setwd input
#' @export
#'
#' @examples
#' \dontrun{bettergetwd()}
#'
bettergetwd <- function() {
  address <- getwd()
  gsub("/", "//", address)
}

bettergetwd()

