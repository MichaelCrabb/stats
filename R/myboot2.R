#' Bootstrap Confidence Interval Function
#'
#' Computes a bootstrap confidence interval for a specified function (e.g., mean, median).
#'
#' @param x A numeric vector of data to bootstrap.
#' @param fun A character string specifying the function to apply (e.g., "mean", "median").
#' @param n The number of bootstrap samples to generate. Default is 1000.
#' @param conf.level Confidence level for the interval. Default is 0.95.
#' @param xlab A character string for the label of the x-axis (optional).
#'
#' @return A list with bootstrap samples and confidence interval.
#' @export
myboot2 <- function(x, fun = "mean", n = 1000, conf.level = 0.95, xlab = "Bootstrap CI") {
  # Check if the specified function is valid
  func <- match.fun(fun)

  # Generate bootstrap samples
  boot_samples <- replicate(n, func(sample(x, length(x), replace = TRUE)))

  # Calculate the confidence interval
  ci <- quantile(boot_samples, probs = c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2))

  # Optional: create a histogram of bootstrap samples
  hist(boot_samples, main = paste("Bootstrap Distribution of", xlab), xlab = xlab, col = "lightblue", border = "white")
  abline(v = ci, col = "red", lwd = 2)

}

