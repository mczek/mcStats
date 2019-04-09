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
  expect_warning(object = showChiSq.Test(x = c(1,2,1), y= c(1,2,2)), regexp =  c("approximation", "geom_vline"))
})


