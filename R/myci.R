#' My Confidence Interval
#'
#' @param data data
#' @param conf.level conf.level
#'
#' @return Confidence Interval
#' @export
myci <- function(data, conf.level = 0.95) {
  n <- length(data)
  mean_data <- mean(data)
  sd_data <- sd(data)

  error <- qt(1 - (1 - conf.level) / 2, df = n - 1) * (sd_data / sqrt(n))
  lower_bound <- mean_data - error
  upper_bound <- mean_data + error

  return(c(lower_bound, upper_bound))
}
