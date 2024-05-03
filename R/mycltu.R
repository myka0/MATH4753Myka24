#' This function generates a Monte Carlo simulation for estimating the distribution of sample means drawn from a uniform distribution.
#'
#' @param n The sample size.
#' @param iter The number of iterations for the simulation.
#' @param a The lower bound of the uniform distribution (default is 0).
#' @param b The upper bound of the uniform distribution (default is 10).
#'
#' @return A histogram and density plot of the sample means along with theoretical density curves.
#'
#' @export
#'
#' @examples
#' mycltu(100, 1000)
mycltu = function(n, iter, a = 0, b = 10) {
  # Generate random numbers from uniform distribution
  y = runif(n * iter, a, b)

  x <- NULL

  # Reshape into a matrix with n rows and iter columns
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Calculate sample means for each iteration
  w = apply(data, 2, mean)

  # Generate histogram of sample means
  param = hist(w, plot = FALSE)
  ymax = max(param$density)
  ymax = 1.1 * ymax
  hist(w, freq = FALSE, ylim = c(0, ymax), main = paste("Histogram of sample mean",
                                                        "\n", "sample size= ", n, sep = ""), xlab = "Sample mean")

  # Add density plot of sample means
  lines(density(w), col = "Blue", lwd = 3)

  # Add theoretical density curves
  curve(dnorm(x, mean = (a + b) / 2, sd = (b - a) / (sqrt(12 * n))), add = TRUE, col = "Red", lty = 2, lwd = 3)
  curve(dunif(x, a, b), add = TRUE, lwd = 4)
}
