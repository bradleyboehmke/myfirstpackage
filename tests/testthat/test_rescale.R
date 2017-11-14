set.seed(123)
x <- runif(25, min = 5, max = 20)
y <- c("12", x)

test_that("rescale provides proper messages and warnings", {
  expect_error(rescale(y))
})

test_that("rescale has correct dimensions and output type", {
  expect_is(rescale(x), "numeric")
  expect_length(rescale(x), 25)
})

test_that("rescale computes correctly", {
  expect_equal(rescale(x)[1:2], c(0.26, 0.78))
})
