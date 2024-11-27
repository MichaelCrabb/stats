#' ntickets
#'
#' @param N Number of tickets sold
#' @param gamma True probability of overbooking for the flight
#' @param p Probability of a show
#'
#' @return Values and discrete and continuous graphs
#'
#' @importFrom stats pbinom pnorm dnorm
#' @importFrom graphics curve par polygon
#'
#' @export
#'
ntickets <- function(N, gamma, p) {
  # Discrete method: Calculate the objective function using the binomial distribution
  # Objective function = 1 - gamma - P(X <= N), where X is a binomial random variable n is the number of tickets to sell, N is the number of seats, gamma is the overbooking
  # probability, and p is the probability that a person shows up
  objective_discrete <- function(n) {
    1 - gamma - stats::pbinom(N, size = n, prob = p)
  }

  # Continuous method: Calculate the objective function using the normal approximation
  # The binomial distribution is approximated by a normal distribution when n is large
  # We use the mean and standard deviation of the binomial distribution for the approximation
  objective_continuous <- function(n) {
    mean <- n * p # mean of the binomial distribution
    sd <- sqrt(n * p * (1 - p)) # standard deviation of the binomial distribution
    1 - gamma - stats::pnorm(N, mean = mean, sd = sd) # normal CDF for approximation
  }

  # Define the range of n values to test
  # First block is for less than 300 tickets sold
  # Second is for 300 or more tickets sold
  # This is just for the niceness of the graph
  if (N < 300) {
    n_values <- seq(N - 10, N + 30, 1)
  }
  else {
    n_values <- seq(N, N + 40, 1)
  }

  # Discrete method: Apply the objective function to each n values apply applies the objective_discrete function to all values in n_values
  obj_discrete_values <- sapply(n_values, objective_discrete)

  # Find the n value that minimizes the absolute value of the objective function
  # This is the optimal number of tickets for the discrete case
  nd <- n_values[which.min(abs(obj_discrete_values))]

  # Continuous method: Apply the objective function to each n value
  obj_continuous_values <- sapply(n_values, objective_continuous)

  # Find the n value that minimizes the absolute value of the objective function
  # This is the optimal number of tickets for the continuous (normal approximation) case
  nc <- n_values[which.min(abs(obj_continuous_values))]

  # Create a data frame for the discrete case
  # This will be used to plot the values of the objective function against n
  df_discrete <- data.frame(n = n_values, Objective = obj_discrete_values)

  # Create a data frame for the continuous case
  df_continuous <- data.frame(n = n_values, Objective = obj_continuous_values)

  # Plot the discrete case
  p1 <- ggplot2::ggplot(df_discrete, ggplot2::aes(x = n, y = Objective)) +
    ggplot2::geom_point(color = "blue") + # Plot points in blue
    ggplot2::geom_line(linetype = "dashed") + # Dashed line to connect the points
    ggplot2::geom_vline(xintercept = nd, color = "red") + # Red vertical line at the optimal n (discrete)
    ggplot2::geom_hline(yintercept = 0, color = "red") + # Red horizontal line at y = 0 (objective threshold)
    ggplot2::ylim(-0.05, 1) +  # Set the y-axis limit
    ggplot2::ggtitle(paste("Objective Vs n to find optimal tickets sold\nDiscrete case N =", N, "gamma =", gamma, "p =", p)) +
    ggplot2::xlab("n") + # Label for x-axis (number of tickets sold)
    ggplot2::ylab("Objective") # Label for y-axis (objective function value)

  # Plot the continuous case
  p2 <- ggplot2::ggplot(df_continuous, ggplot2::aes(x = n, y = Objective)) +
    ggplot2::geom_line() + # Plot a solid line for the continuous case
    ggplot2::geom_vline(xintercept = nc, color = "blue") + # Blue vertical line at the optimal n (continuous)
    ggplot2::geom_hline(yintercept = 0, color = "black") + # Black horizontal line at y = 0 (objective threshold)
    ggplot2::ylim(-0.05, 1) +  # Set the y-axis limit
    ggplot2::ggtitle(paste("Objective Vs n to find optimal tickets sold\nContinuous case N =", N, "gamma =", gamma, "p =", p)) +
    ggplot2::xlab("n") + # Label for x-axis (number of tickets sold)
    ggplot2::ylab("Objective") # Label for y-axis (objective function value)

  # Print the two plots
  print(p1) # Print the discrete plot
  print(p2) # Print the continuous plot

  # Return the results as a named list, including the optimal number of tickets for both discrete and continuous cases,
  # and the input parameters (N, p, gamma) for reference
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

#' @keywords internal
utils::globalVariables(c("n", "Objective", "x"))
