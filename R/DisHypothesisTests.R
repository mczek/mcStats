#' @title Label discrete PDF
#' @description labels a discrete pdf
#' @param x x value
#' @param obsVal observed event
#' @param expVal expected value
#'
#' @return vector of labels for x value in relation to observed event
#' @export
#'
#' @examples
#' labelDisPDF(0:10, 3, 5)
labelDisPDF <- function(x, obsVal, expVal){
  y <- rep(x = "More Extreme Event", length(x))
  y[abs(x - expVal) < abs(obsVal - expVal)] <- "Less Extreme Event"
  y[x == obsVal] <- "Observed Event"
  y[x == expVal -(obsVal - expVal)] <- "Equally Extreme Event"
  return(y)
}

showProp.Test <- function(x, n, p = 0.5){
  testResult <- binom.test(x, n, p)
  testStat <- testResult$statistic
  pointEstimate <- x/n
  Diff <- p - x/n

  fakeData <- data.frame(x = 0:n,
                         Probability = dbinom(x = 0:n,
                                              size = n,
                                              prob = p),
                         Event = labelDisPDF(0:n,
                                             obsVal = x,
                                             expVal = p*n))

  ggplot(fakeData, aes_(x = ~ x, y = ~ Probability)) +
    geom_bar(mapping = aes_(fill = ~ Event),
             stat = "identity",
             color = "black") +
    theme_bw() +
    scale_fill_manual(values = c("Observed Event" = "#E69F00",
                                 "Equally Extreme Event" = "#000000",
                                 "More Extreme Event" = "#56B4E9",
                                 "Less Extreme Event" = "#FFFFFF")) +
    labs(x = "X",
         y = "Probability",
         title = "Results of Proportion Test")
}
