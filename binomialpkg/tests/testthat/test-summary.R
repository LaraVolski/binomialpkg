context("summary measures")

#aux_mean

test_that("aux_mean works as expected", {
expect_equal(aux_mean(2, 0.5), 1)
expect_is(aux_mean(3, 0.5), "numeric")
expect_length(aux_mean(10, 0.5), 1)
})

#aux_variance

test_that("aux_variance works as expected", {
  expect_equal(aux_variance(2, 0.5), 0.5)
  expect_is(aux_variance(3, 0.5), "numeric")
  expect_length(aux_variance(10, 0.5), 1)
})


#aux_mode
test_that("aux_mode works as expected", {
  expect_equal(aux_mode(2, 0.5), 1)
  expect_is(aux_mode(3, 0.5), "numeric")
  expect_length(aux_mode(10, 0.5), 1)
})


#aux_skewness
test_that("aux_skewness works as expected", {
  expect_length(aux_skewness(2, 0.5), 1)
  expect_is(aux_skewness(2, 0.5), "numeric")
  expect_equal(aux_skewness(5, 0.5), 0)
})


#aux_kurtosis
test_that("aux_kurtosis works as expected", {
  expect_length(aux_kurtosis(2, 0.5), 1)
  expect_is(aux_kurtosis(2, 0.5), "numeric")
  expect_equal(aux_kurtosis(5, 0.5), -0.4)
})
