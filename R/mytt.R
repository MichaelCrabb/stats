#' mytt
#'
#' @param x 
#' @param y 
#' @param mu 
#' @param paired 
#' @param conf.level 
#'
#' @return
#' @export
#' 
myttest <- function(x, y, mu = 0, paired = FALSE, conf.level = 0.95) {
  # Check input lengths for paired tests
  if (paired && length(x) != length(y)) stop("Lengths of x and y must be equal for paired tests.")
  
  # Perform the t-test with equal variances for independent samples
  t_result <- t.test(x, y, mu = mu, paired = paired, conf.level = conf.level, var.equal = TRUE)
  
  # Create a data frame for `df` output, handling unequal lengths
  n_x <- length(x)
  n_y <- length(y)
  max_len <- max(n_x, n_y)
  data <- c(x, rep(NA, max_len - n_x), y, rep(NA, max_len - n_y))
  v <- c(rep("x", n_x), rep(NA, max_len - n_x), rep("y", n_y), rep(NA, max_len - n_y))
  df_output <- data.frame(data = data, v = v)
  
  # Create the output list
  output <- list(
    ttest = t_result,
    df = df_output,
    paired = paired
  )
  
  # Set class attribute
  class(output) <- "mytt"
  
  return(output)
}