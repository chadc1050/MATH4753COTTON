#' @title myhyper
#'
#' @param iter Number of iterations of simulation
#' @param N Total number of elements
#' @param r Number of successes in N elements
#' @param n Number of elements drawn
#'
#' @return A table of the simulated successes
#' @export

myhyper = function(iter, N, r, n){
  sam.mat = matrix(NA, nr = n, nc = iter, byrow = TRUE)
  succ = c()
  for(i in 1:iter){
    sam.mat[,i] = sample(rep(c(1, 0), c(r, N - r)), n, replace = FALSE)
    succ[i] = sum(sam.mat[,i])
  }
  succ.tab = table(factor(succ, levels = 0:n))
  tab <- succ.tab
  return(tab)
}
