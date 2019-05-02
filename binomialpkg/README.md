---
title: "README.md"
author: "Lara Volski"
date: "May 2, 2019"
output: html_document
---

# Overview 
Binomialpkg can be used in R to help you calculate the binomial distribution as well as look at functions related to the binomial dsitribution.

The key functions are as follows:

- `bin_choose()` calculates the number of combos in which k can occur in n trials.
- `bin_probability()` demonstrates the probability of k amount of successes in n trial if each individual trial has a p probability of success
- `bin_distribution()` returns a data frame with probability by success.
- `bin_cumulative()` returns a data frame with the cumulative probability as the desired amount of success increases
- `bin_variable()` prints your parameters
- `bin_mean()` finds the mean or expected value of your binomial distribution
- `bin_variance()` finds the variance of your binomial distribution
- `bin_mode()` finds the most likely number of successes
- `bin_skewness()` measures the asymmetry of your binomial distribution
- `bin_kurtosis()` measures the 'tailedness' of your binomial distribution

# Motivation
This package can be used to aid in the process of investigating the binomial distribution of your desired trials, probabilities, and successes. It is the final project in the UC Berkeley class **Stats 133** and was created to help me understand how to make projects in r.

# Installation
Please follow these steps to install it

- ` install.packages("devtools")`

Without Vignettes

- `devtools::install_github("LaraVolski/binomialpkg")`

With Vignettes

- `devtools::install_github("LaraVolski/binomialpkg", build_vignettes = T)`

# Usage
- `bin_choose(5,2)`
- `bin_probability(2,5,0.5)`
- `bin_distribution(5, 0.5)`
- `bin_cumulative(5, 0.5)`
- `bin_variable(5, 0.5)`
- `bin_mean(10, 0.3)`
- `bin_variance(10, 0.3)`
- `bin_mode(10, 0.3)`
- `bin_skewness(10, 0.3)`
- `bin_kurtosis(10, 0.3)`


