#' Mybin function
#'
#' @param iter Iter
#' @param n N
#' @param p P
#'
#' @return Bin
#' @export
#'
mybin <- function(iter = 100, n = 10, p = 0.5) {
  # make a matrix to hold the samples
  # initially filled with NA's
  sam.mat <- matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  # Make a vector to hold the number of successes in each trial
  succ <- numeric(iter)

  # Fill each column with a new sample
  for (i in 1:iter) {
    sam.mat[, i] <- sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))

    # Calculate a statistic from the sample (in this case, it is the sum)
    succ[i] <- sum(sam.mat[, i])
  }

  return(succ)
}
