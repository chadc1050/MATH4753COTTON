#' @title mybin
#'
#' @param iter Number of interations of binomial
#' @param n Number of trials
#' @param p Probability of success in each trial
#'
#' @return Number of successes in each simulation
#' @export
#'
#'

mybin <- function(iter, n, p) {
  sam.mat = matrix(NA, nr = n, nc = iter, byrow = TRUE)
  succ = c()
  for( i in 1:iter){
    sam.mat[,i] = sample(c(1,0), n, replace = TRUE, prob = c(p, 1 - p))
    succ[i] = sum(sam.mat[,i])
  }
  return(succ)
}
