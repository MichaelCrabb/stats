#' Central Limit Theorem
#'
#' @param n Number of observations per iteration
#' @param iter Number of iterations
#' @param a Lower bound of uniform distribution
#' @param b Upper bound of uniform distribution
#'
#' @return Histogram of the sum of observations across iterations
#'
#' @export
myclt <- function(n, iter, a = 0, b = 1) {
  y <- runif(n * iter, a, b)
  data <- head(matrix(y, nr = n, nc = iter, byrow = T))
  sm <- apply(data, 2, sum)
  hist(sm)
}
