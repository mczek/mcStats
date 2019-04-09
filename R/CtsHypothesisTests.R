#' @title Print "hello world!"
#' @description print "hello world!"
#' @examples
#' hello()
#' @export

hello <- function(){
  print("hello world!")
}

#' @title dnorm but with more arguments
#' @description compute density of normal distribution while allowing for more arguments which are ignored
#' @param x x value
#' @param mean mean of normal distribution
#' @param sd std. dev. of noraml distribution
#' @param log logical; if TRUE probabilities are given as log(p). See stats::dnorm
#' @param ... extra parameters which are ignored
#' @export
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
#' @return density if outside of extreme event region
#' @export
shadePDFCts <- function(x, fun, testStat, ...){
  y <- fun(x,...)
  y[abs(x) < abs(testStat)] <- NA
  return(y)
}


#' @title Highlight extreme events
#' @description Make graph highlighting events more extreme than observed sample
#'
#' @param testStat test statistic
#' @param densFun function that computes appropriate density
#' @param xlims  x limits of the graph to be used. This is passed to ggplot
#' @param degFree degrees of freedom when only one is needed. This gets passed into densFun
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
showXtremeEventsCts <- function(testStat, densFun, degFree, xlims, ...){
  # print(...)

  fakeData <- data.frame(x = c(-testStat, testStat),
                         Statistic = c("Equally Extreme Event", "Your Test Statistic"))
  # extraArgs <- ...
  plt <- ggplot(fakeData, aes_(x = ~ x)) +
    stat_function(fun = densFun,
                  args = list(df = degFree)) +
    stat_function(data = data.frame(x = xlims),
                  mapping = aes_(x = ~ x),
                  fun = shadePDFCts,
                  geom = "area",
                  fill = "blue",
                  args = list(fun = densFun,
                              testStat = testStat,
                              df = degFree),
                  n = 500) +
    geom_vline(aes_(xintercept = ~ x,
                    color = ~ Statistic),
               size = 3) +
    xlim(xlims) +
    scale_color_colorblind() +
    theme_bw() +
    labs(x = "Test Statistic",
         y = "Density",
         title = "Result of T-Test")
  print(plt)
  return(plt)
}

#' @title Conduct z-test
#' @description Runs z-test and outputs graph for interpretation using stats::t.test
#'
#' @param group1 continuous data to test
#' @param group2 optional: second group to include for two sample t-test
#' @param mu optional: mean to test against for one-sample t-test
#' @param paired boolean, if TRUE perform matched pairs t-test
#' @param verbose default is 1 which will create a graph. To turn this off use verbose = 0.
#'
#' @return results of call to t.test
#'
#' @import ggplot2 stats ggthemes
#' @examples
#' x <- rnorm(100)
#' showT.Test(x, verbose = 0)
#' showT.Test(x)
#'
#' @export
showT.Test <- function(group1, group2 = NULL, mu = 0, paired = FALSE, verbose = 1){
  testResult <- t.test(group1, group2, mu = mu)
  testStat <- testResult$statistic
  degFree <- testResult$parameter
  xlimVal <- max(abs(testStat) + 1, 3)

  if(verbose > 0){
    showXtremeEventsCts(testStat = testStat,
                     densFun = dt,
                     xlims = c(-xlimVal, xlimVal),
                     degFree = degFree)
  }
  return(testResult)
}

#' Show Chi-Square Test
#' @description show results of a chi-square test visually using \link[stats]{chisq.test}
#'
#' @param x a numeric vector or matrix. x and can also be factors
#' @param y a numeric vector
#' @param p a vector of proabilities the same length as x. Used for goodness-of-fit tests. Must be a valid distribution
#' @param simulate.p.value boolean, if TRUE use simulation to estimate p-value
#' @param nreps if simulate.p.value = TRUE number of simulations to complete
#' @param verbose level of visual output, 0 = silent
#'
#' @return results of \link[stats]{chisq.test} call
#' @export
#'
#' @examples
#' showChiSq.Test(x = c(1,2,1), y= c(1,2,2))
showChiSq.Test <- function(x, y = NULL, p = rep(1/length(x), length(x)),
                           simulate.p.value = FALSE, nreps = 2000, verbose = 1){
  testResult <- chisq.test(x, y, p, simulate.p.value, B = nreps)
  testStat <- testResult$statistic
  degFree <- testResult$parameter
  xlims <- c(0, max(qchisq(0.99, degFree), testStat + 1))

  if(verbose > 0){
    showXtremeEventsCts(testStat = testStat,
                        densFun = dchisq,
                        xlims = xlims,
                        degFree = degFree)
  }
  return(testResult)

}
