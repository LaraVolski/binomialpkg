context("checkers")

# check_prob

test_that("check_prob works as expected", {
  expect_true(check_prob(0.5))
  expect_length(check_prob(0.5), 1)
  expect_error(check_prob(2), "prob has to be a number between 0 and 1")
})

# check_trials()

test_that("check_trials works as expected", {
  expect_true(check_trials(5))
  expect_length(check_trials(5), 1)
  expect_error(check_trials(-12), 'trials cannot be a negative number and/or must be an integer')
})


# check_success()

test_that("check_success works as expected", {
  expect_true(check_success(2, 5))
  expect_length(check_success(3, 10), 1)
  expect_error(check_success(5, 2), "success cannot be greater than trials")
})



