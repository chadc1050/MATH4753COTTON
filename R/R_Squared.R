#' @Title R_Squared Calculator for a Linear Regression
#'
#' @param x The name of your x variable
#' @param y The name of your y variable
#' @param lm Linear regression line of the data
#' @return The value of R_Squared
#' @export
#'
#' @examples
#' x <- df$BHDiameter, y <- df$Height, lm <- lm.spruce
coef_corr=function(x,y,lm){
  y_bar = mean(y)
  y_hat = lm$coef[1] + x * lm$coef[2]

  MSS = sum((y_hat - y_bar)^2)
  TSS = sum((y - y_bar)^2)

  R_squared = MSS/TSS
  R_squared
}
