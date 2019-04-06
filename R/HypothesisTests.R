#' Print "hello world!"
#'
#' @examples
#' hello()
#' @export

hello <- function(){
  print("hello world!")
}

#' Returns density of t distribution (uses the normal distribution for now) if in shaded region
#'
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

#' Returns density of t distribution (uses the normal distribution for now) if in shaded region
#'
#' @param x x value
#' @param lBound x value that should be shaded below
#' @param uBound x value that should be shaded above
#' @param mu mean of distribution
#' @param sigma std. dev. of distribution
#'
tShade <- function(x, lBound, uBound, mu = 0, sigma = 1){
  print(x)
  y <- normPDF(x, mu, sigma)
  y[x > lBound & x < uBound] <- NA
  return(y)
}



#' Runs t-test and outputs graph for interpretation
#'
#' @import ggplot2
#'
#' @param group1 continuous data to test
#' @param group2 optional: second group to include for two sample t-test
#' @param mu optional: mean to test against for one-sample t-test
#'
#' @examples
#' x <- rnorm(100)
#' tTest(x)
#'
#' @export
tTest <- function(group1, group2 = NULL, mu = 0){
  mu1 <- mean(group1)
  sigma1 <- sd(group1)
  testResult <- t.test(group1, group2, mu = mu)
  print(testResult)
  testStat <- testResult$statistic

  xlimVal <- max(abs(testStat) + 1, 3)
  fakeData <- data.frame(x = c(-xlimVal, xlimVal, testStat),
                         m = factor(1))

  # ggplot(data = fakeData,
  #        aes(x = x)) +
  #   geom_vline(xintercept = c(testStat, -testStat),
  #              color = c("gold", "black")) +
  #   geom_area(stat = "function",
  #             fun = dnorm,
  #             fill = "blue",
  #             xlim = (c(-xlimVal, testStat)))
  uBound = abs(testStat)
  lBound = -uBound
  print(xlimVal)
  ggplot(fakeData, aes(x=x)) +
    stat_function(fun = dnorm) +
    stat_function(data = data.frame(x = c(-xlimVal, xlimVal)),
                  mapping = aes(x = x),
                  fun = tShade,
                  geom = "area",
                  fill = "blue",
                  args = list(lBound = lBound,
                              uBound = uBound),
                  n = 500) +
    geom_vline(xintercept = c(testStat, -testStat),
               color = c("gold", "black"),
               size = 3)


  # if(!is.null(group2)){
  #
  # }
}
