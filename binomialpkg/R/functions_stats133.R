# Functions

###### 1.1 Private Checker Functions ######

# check_prob()
# A private auxiliary function that will test if an input prob is a valid probability value (i.e. 0 <= p <= 1)

check_prob <- function(p){
  if ((0 <= p) & (p <= 1)){
    return(TRUE)
  }
  else {
    stop('prob has to be a number between 0 and 1')
  }
}


# check_trials()
# Private auxiliary function check_trials() to test if an input trials is a valid value for number of trials (i.e. n is a non-negative integer)

check_trials <- function(n){
  if ((n >= 0) & (n%%1==0)){
    return(TRUE)
  }
  else {
    stop('trials cannot be a negative number and/or must be an integer')
  }
}

# check_success()
# Private auxiliary function to test if an input sucess is a valid value for number of successes (i.e. 0 <= k <= n)
check_success <- function(k, n){
  if (((sum(k > n)) == 0) & (is.vector(k) == T)){
    return(TRUE)
  }
  else{
    stop('success cannot be greater than trials')
  }
}


###### 1.2 Private Auxiliary Functions ######

#aux_mean()
#The expected number of successes in n trials

aux_mean <- function(n, p){
  n * p
}


#aux_variance()

aux_variance <- function(n, p){
  (n * p) * (1 - p)
}

# aux_mode()
# the most likely number of success in n independent trials w probility of success on each trial
aux_mode <- function(n, p){
  floor((n * p) + p)
}


#aux_skewness()
# a measure of asymmetry of the probability distribution of a random variable about its mean.
aux_skewness <- function(n, p){
  (1 - (2 * p)) / (sqrt((n * p) * (1 - p)))
}

aux_skewness(3, 0.1)

#aux_kurtosis()
# A measure of the "tailedness" of the probability distribution of a random variable.

aux_kurtosis <- function(n, p){
  (1 - ((6 * p) * (1 - p))) / ((n * p) * (1 - p))
}

aux_kurtosis(3, 6)


###### 1.3) Function bin_choose() #######

#' @title bin_choose
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param n The number of trials
#' @param k Possible successes
#' @return the number of combinations
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)

bin_choose <- function(n, k){
  if(min(k) > n){
    stop('k cannot be greater than n')}
  else{
  combofunc <- function(vec){
    combo <- factorial(n) / (factorial(vec) * factorial(n - vec))
    return(combo)
  }
  return(sapply(k, FUN = combofunc))
  }
}


###### 1.4) Function bin_probability() #######

#' @title bin_probability
#' @description Calculates the probability of getting x success in y trials
#' @param p The probability of success in 1 trial
#' @param n The number of trials
#' @param k Possible successes
#' @return The probability
#' @export
#' @examples
#' bin_probability(k = 2, n = 5, p = 0.5)
#' bin_probability(k = 0:2, n = 5, p = 0.5)
#' bin_probability(k = 55, n = 100, p = 0.45)

bin_probability <- function(k, n, p){
  if (!check_trials(n)){
    stop('invalid trials value')}
  if (!check_prob(p)){
    stop('invalid probability')}
  if (!check_success(k,n)){
    stop('invalid success value')}
  else{
vec <- bin_choose(n,k) * ((p^k) * ((1 - p)^(n - k)))
return(vec)
  }
}

###### 1.5) Function bin_distribution() #######

#' @title bin_distribution
#' @description A data frame of probility by success
#' @param n The number of trials
#' @param p The probability in 1 trial
#' @return A data frame with two classes, bindis and data.frame
#' @export
#' @examples
#' bin_distribution(n = 5, p = 0.5)

bin_distribution <- function(n, p){
  dataframe <- data.frame(success = 0:n, probability = bin_probability(k = 0:n, n, p))
  class(dataframe) = c('data.frame', 'bindis')
  return(dataframe)
  }

bin_distribution(n = 5, p = 0.5)

###### 1.5) Function plot.bindis() #######
#' @export
plot.bindis <- function(n, p){
  dis1 <- bin_distribution(n, p)
  barplot(dis1$probability, names.arg = dis1$success, xlab = "successes", ylab = "probability")
}

plot.bindis(n = 5, p = 0.5)


###### 1.6) Function bin_cumulative() #######

#' @title bin_cumulative
#' @description Creates a data frame, has two classes 'bincum' and 'data.frame'
#' @param n The number of trials
#' @param p The probability in 1 trial
#' @return A data frame with success, probability, and cumulative probability
#' @export
#' @examples
#' bin_cumulative(n = 5, p = 0.5)

bin_cumulative <- function(n, p){
  dataframe <- data.frame(success = 0:n,
                          probability = bin_probability(k = 0:n, n, p),
                          cumulative = cumsum(bin_probability(k = 0:n, n, p)))
  class(dataframe) = c('bincum', 'data.frame')
  return(dataframe)
}

bin_cumulative(n = 5, p = 0.5)


###### 1.6) Function plot.bincum() #######
#' @export
plot.bincum <- function(x){
  plot(x$success, x$cumulative, xlab = 'Successes', ylab = 'Probability')
  lines(x$success, x$cumulative)
}

###### 1.7) Function bin_variable() #######

#' @title bin_variable
#' @description A list of class binvar
#' @param n The number of trials
#' @param p The probability in 1 trial
#' @return A list with named elements trials and prob
#' @export
#' @examples
#' bin_variable(n = 5, p = 0.5)

bin_variable <- function(n, p){
trials = n
prob = bin_probability(k = 0:trials, n, p)
  if (!check_trials(n)){
    stop('invalid trials value')}
  if (!check_prob(p)){
    stop('invalid probability')}
  else{
    bin_variable_list <- list("trials" = trials, "prob" = p)
    class(bin_variable_list) = 'binvar'
    return(bin_variable_list)
  }
}


###### 1.7) Function print.binvar() #######

#' @export

print.binvar <- function(x){
  cat("'Binomial variable'")
  # insert 2 new lines
  cat("\n\n", append = TRUE)
  cat("Paramaters", append = TRUE)
  # insert new line
  cat("\n - number of trials:", x$trials,
       append = TRUE)
  # insert new line
  cat("\n - prob of success:", x$p, "\n",
     append = TRUE)
  invisible(x)
}

print(bin_variable(10, 0.3))

###### 1.7) Function summary.binvar() #######

#' @export

summary.binvar <- function(x){
  summary_list <- list("trials" = x$trials,
                       "prob" = x$p,
                       "mean" = aux_mean(x$trials, x$p),
                       "variance" = aux_variance(x$trials, x$p),
                       "mode" = aux_mode(x$trials, x$p),
                       "skewness" = aux_skewness(x$trials, x$p),
                       "kurtosis" = aux_kurtosis(x$trials, x$p))
  class(summary_list) <- c('summary.binvar')
  return(summary_list)
  invisible(x)
}

summary.binvar(bin_variable(10, 0.3))


###### 1.7) Function print.summary.binvar() #######

#' @export

print.summary.binvar <- function(x){
  cat("'Summary Binomial'")
  # insert new line
  cat("\n\n", append = TRUE)
  cat("Paramaters", append = TRUE)
  # insert new line
  cat("\n - number of trials:", x$trials,
      append = TRUE)
  # insert new line
  cat("\n - prob of success:", x$p, "\n",
      append = TRUE)
  # insert new line
  cat("\n\n", append = TRUE)
  cat("Measures", append = TRUE)
  # insert new line
  cat("\n - mean:", aux_mean(x$trials, x$p),
      append = TRUE)
  # insert new line
  cat("\n - variance:", aux_variance(x$trials, x$p),
      append = TRUE)
  # insert new line
  cat("\n - mode:", aux_mode(x$trials, x$p),
      append = TRUE)
  # insert new line
  cat("\n - skewness:", aux_skewness(x$trials, x$p),
      append = TRUE)
  # insert new line
  cat("\n - kurtosis:", aux_kurtosis(x$trials, x$p),
      append = TRUE)
  invisible(x)
}

print.summary.binvar(bin_variable(10, 0.3))

###### 1.8) Functions of measures #######

#' @title bin_mean
#' @description Calculates the expected number of successes in n trials
#' @param n The number of trials
#' @param p The probability in 1 trial
#' @return The expected number of successes in n trials
#' @export
#' @examples
#' bin_mean(10, 0.03)

bin_mean <- function(n, p){
  if (!check_trials(n)){
    stop('invalid trials value')}
  if (!check_prob(p)){
    stop('invalid probability')}
  else {
    return(aux_mean(n,p))
  }
}


#' @title bin_variance
#' @description The variance using the auxiliary function
#' @param n The number of trials
#' @param p The probability in 1 trial
#' @return A number representing the variance
#' @export
#' @examples
#' bin_variance(10, 0.03)

bin_variance <- function(n, p){
  if (!check_trials(n)){
    stop('invalid trials value')}
  if (!check_prob(p)){
    stop('invalid probability')}
  else {
    return(aux_variance(n,p))
  }
}

bin_variance(10, 0.3)

#' @title bin_mode
#' @description Calculates the most likely number of success in n independent trials w probility of success on each trial
#' @param n The number of trials
#' @param p The probability in 1 trial
#' @return the most likely number of success in n independent trials w probility of success on each trial
#' @export
#' @examples
#' bin_mode(10, 0.03)

bin_mode <- function(n, p){
  if (!check_trials(n)){
    stop('invalid trials value')}
  if (!check_prob(p)){
    stop('invalid probability')}
  else {
    return(aux_mode(n,p))
  }
}


#' @title bin_skewness
#' @description a measure of asymmetry of the probability distribution of a random variable about its mean.
#' @param n The number of trials
#' @param p The probability in 1 trial
#' @return A number representing the variance
#' @export
#' @examples
#' bin_skewness(10, 0.03)

bin_skewness <- function(n, p){
  if (!check_trials(n)){
    stop('invalid trials value')}
  if (!check_prob(p)){
    stop('invalid probability')}
  else {
    return(aux_skewness(n,p))
  }
}

#' @title bin_kurtosis
#' @description A measure of the "tailedness" of the probability distribution of a random variable.
#' @param n The number of trials
#' @param p The probability in 1 trial
#' @return A number representing the "tailedness"
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)

bin_kurtosis <- function(n, p){
  if (!check_trials(n)){
    stop('invalid trials value')}
  if (!check_prob(p)){
    stop('invalid probability')}
  else {
    return(aux_kurtosis(n,p))
  }
}




