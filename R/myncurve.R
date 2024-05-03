#' Plot a normal distribution
#'
#' @param mu mean of the normal distribution
#' @param sigma standard deviation of the normal distribution
#' @param a value to calculate cumulative probability up to
#'
#' @return graph of a normal distribution
#' @export
#'
#' @examples myncurve(4,2,3)
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(-100,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(-100,xcurve,a),c(0,ycurve,0),col="Red")
  list(mu = mu, sigma = sigma)
  pnorm(a,mu,sigma)
}
