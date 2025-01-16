if (!requireNamespace("NHANES", quietly = TRUE))
  install.packages("NHANES")
library(NHANES)

# Load the dataset
data("NHANES")
library(dplyr)
source(file.path(here::here(), "testing_functions.R"))

# Analyze health outcomes by education
means_income <- aggregate(log(HHIncomeMid) ~ Education, data = NHANES, mean)[,2]
vars_income <- aggregate(log(HHIncomeMid) ~ Education, data = NHANES, var)[,2] / 
  aggregate(log(HHIncomeMid) ~ Education, data = NHANES, length)[,2]

means_sleep <- aggregate(SleepHrsNight ~ Education, data = NHANES, mean)[,2]
vars_sleep <- aggregate(SleepHrsNight ~ Education, data = NHANES, var)[,2] / 
  aggregate(SleepHrsNight ~ Education, data = NHANES, length)[,2]

means_mental <- aggregate(DaysMentHlthBad ~ Education, data = NHANES, mean)[,2]
vars_mental <- aggregate(DaysMentHlthBad ~ Education, data = NHANES, var)[,2] / 
  aggregate(DaysMentHlthBad ~ Education, data = NHANES, length)[,2]

# Table 1: Exploratory analysis of these variables
data.frame(Education=levels(NHANES$Education),
           N=as.integer(table(NHANES$Education)),
           LogIncome=round(means_income, 2), 
           HoursSleep=round(means_sleep, 2), 
           DaysBadMentalHealth=round(means_mental, 2))

# Y=log income, X=education group: ranks 5/5
rank_test(means_income,vars_income, alpha=0.001)
set_test(means_income,vars_income, 3, alpha=0.001)

# Y=sleep hours, X=education group: top ranks 1/5 (alpha=0.1), bottom 0
rank_test(means_sleep, vars_sleep, alpha=0.1)
test_for_lowest(means_sleep, vars_sleep, alpha=0.1)
set_test(means_sleep,vars_sleep, 3, alpha=0.1)

# Y=bad mental health, X=education group: top ranks 0/5, bottom 1
rank_test(means_mental,vars_mental, alpha=0.1)
test_for_lowest(means_mental,vars_mental, alpha=0.01)
set_test(means_mental,vars_mental, 3, alpha=0.1)


############## Validation #####################
# Simulate data
true_means <- means_sleep
true_vars <- vars_sleep
n_sampled_sets <- 10000
set.seed(1)
sampled_data <- rnorm(length(true_means)*n_sampled_sets, mean=true_means, sd=sqrt(true_vars))
mat <- matrix(sampled_data, nrow=n_sampled_sets, byrow=TRUE)

# Get rankings
true_ranking <- rev(order(true_means))
all_rankings <- apply(mat, 1, function(row) {rev(order(row))})
all_rankings <- matrix(all_rankings, nrow=n_sampled_sets, byrow=TRUE)

# Calculate number of stable ranks in each
for (alpha in c(0.05, 0.1, 0.2)) {
  num_stable <- apply(mat, 1, rank_test, vars=true_vars, alpha=alpha, verbose=FALSE, return_p_vals=FALSE)
  num_same <- sum(sapply(1:n_sampled_sets, function(i) {
    K <- num_stable[i]
    ifelse(K>0, all(true_ranking[1:K] == all_rankings[i, 1:K]), TRUE)
  }))
  num_errors <- n_sampled_sets-num_same
  fwer <- num_errors/n_sampled_sets
  print(paste0("alpha=", alpha, ", FWER=", fwer, ", mean K=", round(mean(num_stable),1)))
}


# Stability of Top-K set
for (alpha in c(0.1, 0.2)) {
  for (K in 2:3) {
    true_set <- sort(true_ranking[1:K])
    test_outcome <- apply(mat, 1, set_test, vars=true_vars, K=K, alpha=alpha, verbose=FALSE, return_p_val=FALSE)
    # print(round(mean(test_outcome=="reject")*100,1))
    rej_freq <- round(mean(test_outcome=="reject")*100,1)
    num_same <- sum(sapply(1:n_sampled_sets, function(i) {
      outcome <- test_outcome[i]
      ifelse(outcome=="reject", all(true_set == sort(all_rankings[i, 1:K])), TRUE)
    }))
    num_errors <- n_sampled_sets-num_same
    fwer <- num_errors/n_sampled_sets
    print(paste0("K=", K, ", alpha=", alpha))
    print(paste0("FWER (%): ", fwer*100, ", Rej. Freq (%): ", rej_freq))
    print("########")
  }
}
