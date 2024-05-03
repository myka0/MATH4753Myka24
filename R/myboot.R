#' Bootstrap Confidence Interval Estimation
#'
#' This function calculates bootstrap confidence intervals for a given statistic
#' (mean, median, variance, etc.) based on resampling with replacement.
#'
#' @param iter Number of bootstrap iterations.
#' @param x Vector of data from which to draw samples.
#' @param fun The statistic to compute on each bootstrap sample. Default is "mean".
#' @param alpha The significance level for the confidence interval. Default is 0.05.
#' @param cx Expansion factor for text size in the plot. Default is 1.5.
#' @param ... Additional arguments to be passed to the hist function for plotting.
#' @import stats
#'
#' @return A list containing the bootstrap confidence interval (ci) for the specified statistic,
#' the chosen statistic (fun), and the original data vector (x).
#'
#' @examples
#' set.seed(42)
#' data <- rnorm(100, mean = 10, sd = 2)
#' boot_result <- myboot(iter = 10000, x = data, fun = "mean", alpha = 0.05)
#' boot_result$ci
#'
#' @export
myboot<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
