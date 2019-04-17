context("testGraphs.R")

test_that("mcDNorm",{
  expect_equal(mcDNorm(0), 0.3989423, tolerance = 0.000001)
  expect_equal(mcDNorm(c(1,2,3)), c(0.241970725, 0.053990967, 0.004431848), tolerance = 0.000001)
})

test_that("showT.Test runs error-free",{
  x <- rnorm(10)
  expect_warning(object = showT.Test(x),regexp =  NA)
})

test_that("showProp.Test runs error-free",{
  expect_warning(object = showProp.Test(3, 10),regexp =  NA)
})

test_that("showChiSq.Test runs error-free",{
  expect_warning(object = showChiSq.Test(x = c(100,200,100), y= c(100,200,200)), regexp =  "approximation")
})

test_that("showProp.Test runs error-free",{
  expect_warning(object = showProp.Test(3, 10), regexp =  NA)
})

test_that("showANOVA runs error-free",{
  expect_warning(object = showANOVA(yield ~  N + P + K + block + block:P, npk), regexp =  NA)
})

test_that("showOLS runs error-free",{
  expect_warning(object = showOLS(mpg ~ cyl  + qsec, mtcars), regexp =  NA)
})

test_that("showWilcoxonTest runs error-free",{
  expect_warning(object = {x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
  y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
  showWilcoxon.Test(x,y)}, regexp =  NA)
})



