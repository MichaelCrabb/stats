#' myncurve
#'
#' @param mu The mean
#' @param sigma The standard deviation
#' @param a Value for area calculation
#'
#' @return The area
#' @export
#'
#'
myncurve = function(mu, sigma, a){
  # Plot the curve for the normal distribution
  old_par <- par(no.readonly = TRUE)
  par(mar=c(5, 4, 4, 2) + 0.1)

  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        main = "Normal Distribution Curve",
        col = "Black",
        ylab = "Density",
        xlab = "X",
        lwd = 2)

  # Shade the area under the curve from -∞ to a
  x_fill = seq(mu - 3 * sigma, a, length = 1000)
  y_fill = dnorm(x_fill, mean = mu, sd = sigma)
  polygon(c(x_fill, a, mu - 3 * sigma), c(y_fill, 0, 0), col = "Blue")

  curve(dnorm(x, mean=mu, sd=sigma), add=TRUE, lwd=2)

  # Calculate the probability P(X ≤ a)
  prob = pnorm(a, mean = mu, sd = sigma)

  par(old_par)

  # Return the probability in a list
  return(prob)
}


