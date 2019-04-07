#' @title Print "hello world!"
#' @description print "hello world!"
#' @examples
#' hello()
#' @export

hello <- function(){
  print("hello world!")
}

#' @title Normal Distribution PDF
#' @description Returns density of t distribution (uses the normal distribution for now) if in shaded region
#' @param x x value
#' @param mu mean of distribution
#' @param sigma std. dev. of distribution
#'
#' @examples
#' normPDF(0)
#' @export
normPDF <- function(x, mu = 0, sigma = 1){
  return(exp(-(x - mu)^2/2)    /sqrt(2*pi))
}

#' @title T-Distribution PDF
#' @description Returns density of t distribution (uses the normal distribution for now) if in shaded region
#' @param x x value
#' @param lBound x value that should be shaded below
#' @param uBound x value that should be shaded above
#' @param mu mean of distribution
#' @param sigma std. dev. of distribution
#'
tShade <- function(x, lBound, uBound, mu = 0, sigma = 1){
  y <- normPDF(x, mu, sigma)
  y[x > lBound & x < uBound] <- NA
  return(y)
}


#' @title Conduct t-test
#' @description Runs t-test and outputs graph for interpretation
#' @param group1 continuous data to test
#' @param group2 optional: second group to include for two sample t-test
#' @param mu optional: mean to test against for one-sample t-test
#' @param verbose default is 1 which will create a graph. To turn this off use verbose = 0.
#'
#' @return results of call to t.test
#'
#' @import ggplot2 stats ggthemes
#'
#' @examples
#' x <- rnorm(100)
#' showT.Test(x, verbose = 0)
#' showT.Test(x)
#'
#' @export
showT.Test <- function(group1, group2 = NULL, mu = 0, verbose = 1){
  mu1 <- mean(group1)
  sigma1 <- sd(group1)
  testResult <- t.test(group1, group2, mu = mu)
  print(testResult)
  testStat <- testResult$statistic

  xlimVal <- max(abs(testStat) + 1, 3)
  fakeData <- data.frame(x = c(-testStat, testStat),
                         Statistic = c("Equally Extreme Event", "Your Test Statistic"))

  uBound = abs(testStat)
  lBound = -uBound

  if(verbose > 0){
    plt <- ggplot(fakeData, aes(x=x)) +
      stat_function(fun = dnorm) +
      stat_function(data = data.frame(x = c(-xlimVal, xlimVal)),
                    mapping = aes(x = x),
                    fun = tShade,
                    geom = "area",
                    fill = "blue",
                    args = list(lBound = lBound,
                                uBound = uBound),
                    n = 500) +
      geom_vline(aes(xintercept = x,
                     color = Statistic),
                 size = 3) +
      scale_color_colorblind() +
      theme_bw() +
      labs(x = "Test Statistic",
           y = "Density",
           title = "Result of T-Test")
    print(plt)
  }
  return(testResult)
}
