#' @title Print "hello world!"
#' @description print "hello world!"
#' @examples
#' hello()
#' @export

hello <- function(){
  print("hello world!")
}

#' dnorm but with more arguments
#' @description compute density of normal distribution while allowing for more arguments which are ignored
#' @param x x value
#' @param mean mean of normal distribution
#' @param sd std. dev. of noraml distribution
#' @param log logical; if TRUE probabilities are given as log(p). See stats::dnorm
#' @param ... extra parameters which are ignored
#' @return density of normal distribution
mcDNorm <- function(x, mean = 0, sd = 1, log = FALSE, ...){
  return(dnorm(x, mean, sd, log))
}

#' @title Used to shade in a PDF
#' @description Returns density with extreme event region having NAs
#'
#' @param fun density function to use
#' @param testStat test statistic value
#' @param x x value
#' @param ... optional parameters passed to density function
#'
#' @return density if outside of extreme event region
shadePDF <- function(x, fun, testStat, ...){
  y <- fun(x,...)
  y[abs(x) < abs(testStat)] <- NA
  return(y)
}


#' @title Highlight extreme events
#' @description Make graph highlighting events more extreme than observed sample
#'
#' @param testStat test statistic
#' @param densFun function that computes appropriate density
#' @param ... extra arguments passed to density function
#'
#' @return results of call testFun
#'
#' @import ggplot2 ggthemes
#'
#' @examples
#' x <- rnorm(100)
#' showT.Test(x, verbose = 0)
#' showT.Test(x)
#'
#' @export
showXtremeEvents <- function(testStat, densFun, ...){
  # print(...)
  xlimVal <- max(abs(testStat) + 1, 3)
  fakeData <- data.frame(x = c(-testStat, testStat),
                         Statistic = c("Equally Extreme Event", "Your Test Statistic"))
  # extraArgs <- ...
  plt <- ggplot(fakeData, aes_(x = ~ x)) +
    stat_function(fun = densFun,
                  args = list(df = df)) +
    stat_function(data = data.frame(x = c(-xlimVal, xlimVal)),
                  mapping = aes_(x = ~ x),
                  fun = shadePDF,
                  geom = "area",
                  fill = "blue",
                  args = list(fun = densFun,
                              testStat = testStat,
                              df = df),
                  n = 500) +
    geom_vline(aes_(xintercept = ~ x,
                    color = ~ Statistic),
               size = 3) +
    scale_color_colorblind() +
    theme_bw() +
    labs(x = "Test Statistic",
         y = "Density",
         title = "Result of T-Test")
  print(plt)
  return(plt)
}


#' @title Conduct z-test
#' @description Runs z-test and outputs graph for interpretation
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
showZ.Test <- function(group1, group2 = NULL, mu = 0, verbose = 1){
  testResult <- t.test(group1, group2, mu = mu)
  testStat <- testResult$statistic
  df <- testResult$parameter

  if(verbose > 0){
    showXtremeEvents(testStat = testStat,
                    densFun = mcDNorm)
  }
  return(testResult)
}

#' @title Conduct z-test
#' @description Runs z-test and outputs graph for interpretation
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
  testResult <- t.test(group1, group2, mu = mu)
  testStat <- testResult$statistic
  df <- testResult$parameter

  if(verbose > 0){
    showXtremeEvents(testStat = testStat,
                     densFun = dt,
                     df = df)
  }
  return(testResult)
}


