#' Boostrap using given data and statistic
#'
#' @param fun function to calculate on each sample
#' @param data data to use for boostrapping. Should be a respresentative sample
#' @param nreps number of times to bootstrap
#'
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' bootstrap(mean, x, 1000)
bootstrap <- function(fun, data, nreps){
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
    geom_histogram() +
    theme_bw() +
    geom_vline(aes(xintercept = Mean,
                   color = "Mean")) +
    scale_color_colorblind() +
    labs(x = "Statistic",
         y = "Count",
         title = paste("Results of Boostrapping", nreps, "Times" ))
}
