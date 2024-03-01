#' Simulate Binomial Experiments
#'
#' This function simulates binomial experiments, where each experiment consists of `n` trials
#' with success probability `p`, and creates a barplot for the proportions of successes.
#'
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#'
#' @param iter The number of iterations to simulate.
#' @param n The number of trials per iteration.
#' @param p The probability of success on each trial.
#'
#' @return A table of proportions of successes.
#' @export
#'
#' @examples
#' mybin(100, 10, 0.7)
#'
mybin <- function(iter=100, n=10, p=0.7){
  succ <- numeric(iter)
  for(i in 1:iter){
    succ[i] <- sum(sample(c(1,0), n, replace=TRUE, prob=c(p,1-p)))
  }
  succ.tab <- table(factor(succ, levels=0:n))
  barplot(succ.tab/iter, col=rainbow(n+1), main=paste(iter, "Iterations"), xlab="Number of successes")
  return(succ.tab/iter)
}
