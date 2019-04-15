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

#' Show Extreme Events from a Discrete Distribution
#'
#' @param testID name of test being performed. This is used to title the graph
#' @param obsVal observed x value
#' @param expVal expected x value
#' @param xVals domain of x (possible values)
#' @param probFun probability mass function for the given distribution
#' @param ... addition arguments passed to probFun
#'
#' @return graph coloring events by how extreme they are under the null hypothesis
#' @export
#'
#' @examples
#' showXtremeEventsDis("Prop. Test", 3, 5, 0:10, probFun = dbinom, size = 10, prob = 0.5)
showXtremeEventsDis <- function(testID, obsVal, expVal, xVals, probFun, ...){
  fakeData <- data.frame(x = xVals,
                         Probability = probFun(xVals, ...),
                         Event = labelDisPDF(xVals,
                                             obsVal,
                                             expVal))

  plt <- ggplot(fakeData, aes_(x = ~ x, y = ~ Probability)) +
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
         title = paste("Results of", testID))
  print(plt)
  return(plt)
}


#' Show results of proportion test using \link[stats]{binom.test}
#'
#' @param x x value
#' @param n number of repetitions
#' @param p probability of success in one Bernoulli trial
#'
#' @return output of call to \link[stats]{binom.test}
#' @export
#'
#' @examples
#' showProp.Test(3, 10)
showProp.Test <- function(x, n, p = 0.5){
  testResult <- binom.test(x, n, p)
  obsVal <- testResult$statistic
  showXtremeEventsDis(testID = "Proportion Test",
                      obsVal = obsVal,
                      expVal = n*p,
                      xVals = 0:n,
                      probFun = dbinom,
                      size = n,
                      prob = p)
  return(testResult)
}

wilcoxonProb <- function(x, n, k){
  #computes P_n(X = x)
  if(x < k){
    return(0)
  }
  nums2SelectFrom <- min(n,x)
  subsets <- utils::combn(1:nums2SelectFrom, k)
  count <- 0
  nSubsets <- choose(nums2SelectFrom,k)
  for(i in 1:nSubsets){
    s <- subsets[,i]
    count <- count + (sum(s) == x)
  }
  return(count/choose(n,k))
}

mcDWilcox <- function(x, n, k){
  return(sapply(x, wilcoxonProb, n = n, k = k))
}

#' Visualize Results of a Wilcoxon Rank Sum Test
#'
#' @param cat1 vector of measurements for group 1
#' @param cat2 vector of measurements for group 2
#'
#' @return list containing p-value and test statistic
#' @export
#'

showWilcoxon.Test <- function(cat1, cat2){
  #assume values are unique
  x <- c(cat1, cat2)
  x <- sort(x)
  N <- length(x)
  sumRank1<- sumRank2 <- 0

  for (i in 1:N){
    if(x[i] %in% cat1){
      sumRank1 <- sumRank1 + i
    } else{
      sumRank2 <- sumRank2 + i
    }
  }

  testStat <- sumRank2
  k <- length(cat2)
  if(sumRank1 >= sumRank2){
    testStat <- sumRank1
    k <- length(cat1)
  }

  probs <- NULL
  for (i in 0:(N*k)){
    probs[i+1] <- mcDWilcox(i, N, k)
  }

  ExpVal <- k*(N+1)/2

  p <- sum(probs[labelDisPDF(0:(N*k), testStat, ExpVal) != "Less Extreme Event"])

  showXtremeEventsDis(testID = "Wilcoxon Rank Sum Test",
                      testStat,
                      expVal = ExpVal,
                      xVals = (sum(1:k)):(sum((N-k+1):N)),
                      probFun = mcDWilcox,
                      n = N,
                      k = k)

  print(paste("p-value:", p))
  result <- list(statistic = testStat,
                 p.value = p)
  return(result)
}




