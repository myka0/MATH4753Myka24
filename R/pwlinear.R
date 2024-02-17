#' Piecewise linear model
#'
#' @param x x value
#' @param xk xk value
#' @param coef coefficient
#'
#' @return piecewise linear model
#' @export
#'
#' @examples
#' pwlinear(0, 18, 4)
pwlinear = function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x - xk)*(x - xk > 0)
}
