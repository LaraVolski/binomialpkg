## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(binomialpkg)

## ----bin_choose----------------------------------------------------------
# First Example
bin_choose(n = 5, k = 2)
# Second Example
bin_choose(5, 0)
# Third Example
bin_choose(5, 1:3)

## ----bin_probability-----------------------------------------------------
# Example 1
bin_probability(k = 2, n = 5, p = 0.5)

# Example 2
bin_probability(0:2, 5, 0.5)

# Example 3
bin_probability(55, 100, 0.45)

## ----bin_distribution----------------------------------------------------
bin_distribution(5, 0.5)

## ----plot.bindis---------------------------------------------------------
# Example 1
dis1 <- bin_distribution(5, 0.5)
barplot(dis1$probability, names.arg = dis1$success, xlab = "successes", ylab = "probability")


## ----bin_cumulative------------------------------------------------------
bin_cumulative(5, 0.5)

## ----plot.bincum---------------------------------------------------------
dis2 <- bin_cumulative(5, 0.5)
plot(dis2)

## ----bin_variable--------------------------------------------------------
bin_variable(5, 0.5)

## ------------------------------------------------------------------------
# Example 1
bin1 <- bin_variable(10, 0.3)
binsum1 <- summary(bin1)
binsum1

## ------------------------------------------------------------------------
bin_mean(10, 0.3)

## ------------------------------------------------------------------------
bin_variance(10, 0.3)

## ------------------------------------------------------------------------
bin_mode(10, 0.3)

## ------------------------------------------------------------------------
bin_skewness(10, 0.3)

## ------------------------------------------------------------------------
bin_kurtosis(10, 0.3)

