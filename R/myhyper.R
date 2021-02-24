#' @title myhyper
#'
#' @param iter Number of iterations of simulation
#' @param N Total number of elements
#' @param r Number of successes in N elements
#' @param n Number of elements drawn
#'
#' @return A barplot of the hypergeometric simulation and a probability table
#' @export

myhyper = function(iter, N, r, n){
  sam.mat = matrix(NA, nr = n, nc = iter, byrow = TRUE)
  succ = c()
  for(i in 1:iter){
    sam.mat[,i] = sample(rep(c(1, 0), c(r, N - r)), n, replace = FALSE)
    succ[i] = sum(sam.mat[,i])
  }
  succ.tab = table(factor(succ, levels = 0:n))
  hyperplot <- barplot(succ.tab / (iter), col = "light green", main = "Hypergeometric Simulation", xlab = "Number of successes")
  tab <- succ.tab / iter
  return(hyperplot, tab)
}
