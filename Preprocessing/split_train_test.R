.saveworkspace.split_train_test.R <- c(ls(),"jobs.train","jobs.test","jobs")

# load("jobs.RDa")

set.seed(6)

reserved_fraction <- 1/4 # fraction of data reserved for validation
r <- 1/(reserved_fraction)

N_obs <- dim(jobs)[1]
ids <- seq(1,N_obs)
test_ids <- sample( N_obs , N_obs %/% r )
train_ids <- ids[ !(ids %in% test_ids) ]

jobs.train <- jobs[train_ids,]
jobs.test <- jobs[test_ids,]
save(list=c("jobs.train","jobs.test","jobs"),file="jobs.RData")

rm(list=ls()[!(ls() %in% .saveworkspace.split_train_test.R)])
