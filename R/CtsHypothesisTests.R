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

mcDT <- function(x, df, ...){
  return(dt(x, df))
}

mcDF <- function(x, df1, df2, ...){
  return(df(x, df1, df2))
}

mcDChiSq <- function(x, df, ...){
  return(dchisq(x, df))
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
#' @param verbose if verbose > 0 the resulting graph is printed
#' @param ... extra arguments passed to density function
#' @param degFree1 first degrees of freedom parameter when more than one is needed
#' @param degFree2 second degrees of freedom parameter when more than one is needed
#' @param testID name of hypothesis test
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
showXtremeEventsCts <- function(testID, testStat, densFun, degFree = NULL, degFree1 = NULL,
                                degFree2 = NULL, xlims, verbose = 1, ...){
  # print(...)

  fakeData <- data.frame(x = c(-testStat, testStat),
                         Statistic = c("Equally Extreme Event", "Your Test Statistic"))
  # extraArgs <- ...
  plt <- ggplot(fakeData, aes_(x = ~ x)) +
    stat_function(fun = densFun,
                  args = list(df = degFree,
                              df1 = degFree1,
                              df2 = degFree2)) +
    stat_function(data = data.frame(x = xlims),
                  mapping = aes_(x = ~ x),
                  fun = shadePDFCts,
                  geom = "area",
                  fill = "#56B4E9",
                  args = list(fun = densFun,
                              testStat = testStat,
                              df = degFree,
                              df1 = degFree1,
                              df2 = degFree2),
                  n = 500) +
    geom_vline(aes_(xintercept = ~ x,
                    color = ~ Statistic),
               size = 3) +
    xlim(xlims) +
    scale_color_colorblind() +
    theme_bw() +
    labs(x = "Test Statistic",
         y = "Density",
         title = paste("Result of", testID))
  if(verbose > 0){
    print(plt)
  }
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
    showXtremeEventsCts(testID = "T-Test",
                        testStat = testStat,
                        densFun = mcDT,
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
  testResult <- chisq.test(x = x, y = y, p = p, simulate.p.value = simulate.p.value, B = nreps)
  testStat <- testResult$statistic
  degFree <- testResult$parameter
  xlims <- c(0, max(qchisq(0.99, degFree), testStat + 1))

  if(verbose > 0){
    showXtremeEventsCts(testID = "Chi-Sq. Test",
                        testStat = testStat,
                        densFun = mcDChiSq,
                        xlims = xlims,
                        degFree = degFree)
  }
  return(testResult)
}

#' @title Show results of ANOVA
#' @description Visualization of distributional results of ANOVA. Please see \link[stats]{aov} for more
#' information on parameters
#' @import gridExtra
#' @param formula formula specifying a model.
#' @param data data on which to perform ANOVA
#' @param verbose if verbose > 0 the resulting graph is printed
#' @param ... Arguments passed to lm. See \link[stats]{aov} for more detail
#'
#' @return output of call to \link[stats]{aov}
#' @export
#' @examples
#' showANOVA(yield ~  N + P + K, npk)
showANOVA <- function(formula, data = NULL, verbose = 1, ...){
  anovaResults <- aov(formula, data, ...)
  resultsTable <- summary(anovaResults)[[1]]
  degFree2 <- resultsTable$Df[dim(resultsTable)[1]]
  nInputs <- (dim(resultsTable)[1]-1)
  inputNames <- rownames(resultsTable)
  if(verbose > 0){
    pltList <- NULL
    for(i in 1:nInputs){
      degFree1 <- resultsTable$Df[i]
      testStat <- resultsTable$`F value`[i]
      xlims <- c(0, max(qf(0.99, degFree1, degFree2), testStat + 1))
      pltList[[i]] <- showXtremeEventsCts(testID = paste("ANOVA:", inputNames[i]),
                                     testStat = resultsTable$`F value`[i],
                                     densFun = mcDF,
                                     xlims = xlims,
                                     degFree1 = degFree1,
                                     degFree2 = degFree2,
                                     verbose = 0)

    }
    # grid.arrange(pltList)
    do.call(grid.arrange, c(pltList, ncol = floor(sqrt(nInputs))))

  }
}
