#' @title mybin
#'
#' @param iter Number of interations of binomial
#' @param n Number of trials
#' @param p Probability of success in each trial
#'
#' @return Number of successes in each simulation
#' @export
#'
#' @examples
#' iter <- 100, n <- 10, p <- 0.5
mybin=function(iter,n, p){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
