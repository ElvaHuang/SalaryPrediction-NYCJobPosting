
##########################################################

##--     Exploratory Analysis and Visualizations      --##

##########################################################

# install.packages("ggplot2")
library(ggplot2)
library(lattice)
library(lubridate)

# system.time(source("data_processing.R"))
# cat("Don't worry about the message '1403 failed to parse' - those are just mising values")
load("jobs.RDa")

table(jobs$salary_frequency)
table(jobs$X__of_positions)
table(year(jobs$posting_date))

names(jobs)
ggplot(jobs[jobs$salary_frequency=="Annual",],aes(x=hours_shift)) + geom_histogram()
summary(density(jobs$hours_shift))
str(jobs$hours_shift)
length(unique(jobs$hours_shift))
sum(is.na(jobs$hours_shift))
sum(is.na(jobs$hours_shift) & jobs$salary_frequency=="Hourly")
sum(jobs$salary_frequency=="Hourly")

ggplot(jobs[jobs$salary_frequency=="Annual",], aes(x=salary_range)) + geom_histogram() + ggtitle("Salary Ranges, Annual Salaries")
ggplot(jobs[jobs$salary_frequency=="Annual",], aes(x=salary_midpoint)) + geom_histogram() + ggtitle("Salary Midpoints, Annual Salaries")
ggplot(jobs[jobs$salary_frequency=="Annual",], aes(x=salary_range_from)) + geom_histogram() + ggtitle("Salary Midpoints, Annual Salaries")
ggplot(jobs[jobs$salary_frequency=="Annual",], aes(x=salary_range_to)) + geom_histogram() + ggtitle("Salary Midpoints, Annual Salaries")
ggplot(jobs[jobs$salary_frequency=="Annual",], aes(x=log(salary_midpoint))) + geom_histogram() + ggtitle("Salary Midpoints, Annual Salaries")
ggplot(jobs[jobs$salary_frequency=="Annual",], aes(x=salary_midpoint,y=salary_range)) + geom_point() + ggtitle("Annual Salaries")
ggplot(jobs[jobs$salary_frequency=="Annual",], aes(x=salary_midpoint,y=salary_range/salary_midpoint)) + geom_point() + ggtitle("Annual Salaries")

ggplot(jobs[jobs$salary_frequency=="Annual",], aes(x=salary_midpoint,y=salary_range)) + geom_point() + ggtitle("Annual Salaries")

# log?
log_lower <- log(jobs$salary_range_from)
log_upper <- log(jobs$salary_range_to)
ggplot(data.frame(log_lower,log_upper), aes(x=(log_upper+log_lower)/2,y=log_upper-log_lower)) + geom_point() + ggtitle("Annual Salaries")

ggplot(jobs[jobs$salary_frequency=="Hourly",], aes(x=salary_range)) + geom_histogram() + ggtitle("Salary Ranges, Hourly Salaries")
ggplot(jobs[jobs$salary_frequency=="Hourly",], aes(x=salary_midpoint)) + geom_histogram() + ggtitle("Salary Midpoints, Hourly Salaries")
ggplot(jobs[jobs$salary_frequency=="Hourly",], aes(x=salary_midpoint,y=salary_range)) + geom_point() + ggtitle("Hourly Salaries")

ggplot(jobs[jobs$salary_frequency=="Daily",], aes(x=salary_range)) + geom_histogram() + ggtitle("Salary Ranges, Daily Salaries")
ggplot(jobs[jobs$salary_frequency=="Daily",], aes(x=salary_midpoint)) + geom_histogram() + ggtitle("Salary Midpoints, Daily Salaries")
ggplot(jobs[jobs$salary_frequency=="Daily",], aes(x=salary_midpoint,y=salary_range)) + geom_point() + ggtitle("Daily Salaries")

# bullet points in description => complexity?
sum(grepl("Ã¢â‚¬Â¢",jobs$job_description,fixed=T))/1658

# interesting stuff in here...
