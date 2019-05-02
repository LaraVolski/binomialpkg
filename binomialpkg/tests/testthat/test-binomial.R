context("binomial")

#bin_choose

  test_that("bin_choose works as expected", {
    expect_equal(bin_choose(5, 3), 10)
    expect_error(bin_choose(5, 10), "k cannot be greater than n")
    expect_is(bin_choose(5, 2), "numeric")
  })

#bin_probability

  test_that("bin_probability works as expected", {
    expect_equal(bin_probability(1, 5, 0.5), 0.15625)
    expect_is(bin_probability(1, 5, 0.5), "numeric")
    expect_length(bin_probability(1, 5, 0.5), 1)
    expect_error(bin_probability(5, 1, 0.5), "success cannot be greater than trials")
    expect_error(bin_probability(-5, 0.2, 1), "trials cannot be a negative number and/or must be an integer")
  })


#bin_distribution

  test_that("bin_distribution works as expected", {
    expect_is(bin_distribution(5, 0.5), "data.frame")
    expect_is(bin_distribution(5, 0.5), "bindis")
    expect_error(bin_distribution(5, -0.5), "prob has to be a number between 0 and 1")
    expect_type(bin_distribution(5, 0.5), "list")
  })

#bin_cumulative

  test_that("bin_cumulative works as expected", {
    expect_is(bin_cumulative(5, 0.5), "data.frame")
    expect_is(bin_cumulative(5, 0.5), "bincum")
    expect_error(bin_cumulative(5, -0.5), "prob has to be a number between 0 and 1")
    expect_type(bin_cumulative(5, 0.5), "list")
  })
