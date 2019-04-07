#' Boostrap using given data and statistic
#'
#' @param fun function to calculate on each sample
#' @param data data to use for boostrapping. Should be a respresentative sample
#' @param nreps number of times to bootstrap
#' @param verbose default is 1 which will create a graph. To turn this off use verbose = 0.
#'
#' @return results from boostrapping. A vector of length @param nreps containing each statistic calculated
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' bootstrap(mean, x, 10000)
bootstrap <- function(fun, data, nreps, verbose = 1){
  results <- NULL
  for(i in 1:nreps){
    currentSample <- sample(data, replace = TRUE)
    results[i] <- fun(currentSample)
  }
  meanStat <- mean(results)

  fakeData <- data.frame(Rep = results,
                         Mean = meanStat)

  ggplot(data = fakeData,
         mapping = aes(x = results)) +
    geom_histogram(bins = 30) +
    theme_bw() +
    geom_vline(aes(xintercept = Mean,
                   color = "Mean")) +
    scale_color_colorblind() +
    labs(x = "Statistic",
         y = "Count",
         title = paste("Results of Boostrapping", nreps, "Times" ))
  return(results)
}
