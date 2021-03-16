#' @title myncurve
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The upper bound of the lower tail
#'
#' @return A plot of the normal distribution with the lower tail shaded, the area pasted onto the plot, and a output of the probability.
#'
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), main = paste0("Norm(", mu, ",", sigma, ")"), col = "blue", lwd = 2, ylab = "Density")
  xcurve <- seq(qnorm(0.000001, mu, sigma), a, length.out = 1000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon(c(qnorm(0.000001, mu ,sigma), xcurve, a), c(0, ycurve, 0), col = "light blue")
  prob <- round(pnorm(a, mu, sigma), 4)
  text(a, dnorm(a, mu, sigma) / 2, paste0("Area = ", prob))
  return(prob)
}
