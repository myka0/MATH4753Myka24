#' Determine Optimal Number of Tickets to Sell
#'
#' This function calculates the optimal number of tickets to sell based on the provided parameters using both discrete and continuous distributions.
#'
#' @param N The total number of tickets available.
#' @param gamma A parameter representing the target revenue.
#' @param p The probability of selling a single ticket.
#'
#' @return A named list containing the optimal number of tickets to sell using discrete (`nd`) and continuous (`nc`) distributions, along with the provided parameters.
#' @export
#'
#' @examples
#' # Example usage:
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N, gamma, p) {
  x <- NULL
  # Calculate number of tickets using discrete distribution
  nd <- N + which.min(abs(1 - gamma - pbinom(q = N, size = N:(N * (1 / p)), p))) - 1

  # Define objective function for continuous case
  f <- function(x) {
    abs(1 - gamma - pnorm((N + 0.5), (x * p), (sqrt(x * p * (1 - p)))))
  }

  # Find the optimal number of tickets using optimization for continuous case
  nc <- optimize(f, interval = c(N, (N * (1 / p))))$minimum

  # Print named list
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))

  # Plot discrete distribution
  plot(x = N:(N + 4 * (nd - N)),
       y = 1 - gamma - pbinom(N, N: (N + 4 * (nd - N)), p),
       type = "b",
       xlab = "n",
       ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold",
                    "\n", "(", nd, ") gamma = ", gamma, " N = ", N, " discrete", sep = ""))

  # Add vertical line for optimal number of tickets (discrete)
  abline(v = nd, h = 0, col = "red")

  # Plot continuous distribution
  curve(1 - gamma - pnorm((N + 0.5), (x * p), (sqrt(x * p * (1 - p)))),
        N, (N + 4 * (nd - N)),
        xlab = "n",
        ylab = "Objective",
        main = paste("Objective Vs. n to find optimal tickets sold",
                     "\n", "(", nc, ") gamma = ", gamma , " N = ", N, "continuous", sep = ""))

  # Add vertical line for optimal number of tickets (continuous)
  abline(v = nc, h = 0, col = "blue")
}
