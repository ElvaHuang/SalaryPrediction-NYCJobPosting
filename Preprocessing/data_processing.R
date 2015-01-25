
##########################################################

##--     Preprocessing: Cleaning                      --##

##########################################################

.saveworkspace.data_processing.R <- c(ls(),"jobs")

# install.packages("XML")
# install.packages("lubridate")
library(XML)
library(lubridate)
library(stringr)

cat("Parse XML...")
jobs.xml <- xmlParse("NYC_jobs.xml")
jobs.list <- xmlToList(jobs.xml)
jobs.list <- jobs.list$row
names(jobs.list) <- seq_along(jobs.list)

#--- pre-processing to make sure the columns match up in each row ---#
cat("pre-processing to make sure the columns match up in each row...")
ln <- numeric(length(jobs.list))

for(i in seq_along(ln)){
  ln[i] <- length(names(jobs.list[[i]]))
}

lwhich <- which(ln==max(ln))
v <- logical(length(lwhich)-1)

for(i in seq_along(v)){
  v[i] <- identical(names(jobs.list[[lwhich[i]]]),names(jobs.list[[lwhich[i+1]]]))
}

names(jobs.list[[30]])[which(!( names(jobs.list[[30]]) %in% names(jobs.list[[29]]) ))]
lnwhich <- which(ln!=26)

for( l in lnwhich){
  jobs.list[[l]]["post_until"] <- NA # fill in with NA's to make the rows conform
  jobs.list[[l]] <- jobs.list[[l]][c(1:22,26,23:25)] # reorder columns to conform
}

#--- make a data frame and convert some column types ---#

cat("make a data frame and convert some column types...")
jobs <- data.frame(do.call(rbind,jobs.list),stringsAsFactors=F)
jobs$.attrs <- NULL

jobs <- data.frame(lapply(jobs,gsub,pattern=" 00:00:00",replace="",fixed=T),stringsAsFactors=F) # not strictly necessary, but makes the resulting lubridate objects easier to handle
head(apply(jobs,1,grep,pattern="^[[:digit:]]+$"),20)[c(1,3,20)]
# 1, 4, 9, and 10 are definitely numeric

for(k in c(1,4,9,10)){
  jobs[,k] <- as.numeric(jobs[,k],as.numeric)
}

for(k in c("post_until","posting_date","posting_updated")){
  jobs[,k] <- mdy(jobs[,k])
}

jobs <- cbind(jobs[,1:9],
              salary_range = jobs$salary_range_to - jobs$salary_range_from,
              salary_midpoint = (jobs$salary_range_to + jobs$salary_range_from) / 2,
              jobs[,10:25])

#--- Clean the data ---#
cat("Clean the data")

# jobs2 <- jobs
# jobs2$posting_type <- NULL
# nrow(unique(jobs2))
# length(unique(jobs$job_id))
# 
# jobs2 <- unique(jobs2)
# 
# View(jobs2[which(duplicated(jobs2$job_id)),])

# Correctly code NA in strings
sapply(jobs,`%in%`,x="NULL")
system.time(for(i in seq_len(nrow(jobs))){
  for(j in seq_len(length(jobs))){
    if(jobs[i,j]=="NULL" && !is.na(jobs[i,j])) jobs[i,j] <- NA
  }
})

# Drop "Day Tour" jobs
.whiches <- which(jobs$salary_frequency=="Daily" &
  (jobs$hours_shift=="Day Tour" | is.na(jobs$hours_shift)))
cbind(jobs$business_title[.whiches],jobs$hours_shift[.whiches])

.whiches <- which(jobs$hours_shift=="Day Tour")
jobs <- jobs[-.whiches,]

# Assume 40 hours a week otherwise
jobs$hours_shift[1]
.whiches <- which(jobs$hours_shift == jobs$hours_shift[1])
jobs$hours_shift[.whiches] <- NA
jobs$hours_week <- numeric(nrow(jobs))
jobs$hours_week[is.na(jobs$hours_shift)] <- 40

# Extract work hours
.whiches <- grepl("[[:digit:]]{2} *(hr|hou)",jobs$hours_shift,ignore.case=T)
.whiches <- .whiches | grepl("[[:digit:]]{2} *per",jobs$hours_shift,ignore.case=T)
jobs$hours_week[.whiches] <- as.numeric(str_match(jobs$hours_shift[.whiches],"([[:digit:]]{2})")[,1])

.whiches <- grepl("24/7",jobs$hours_shift,fixed=T)
jobs$hours_week[.whiches] <- 40

.whiches <- !.whiches & grepl("[[:digit:]][^[:digit:]]+[[:digit:]]",jobs$hours_shift)
jobs$hours_week[jobs$hours_shift=="59-17 Junction Blvd Corona Ny"] <- 40
.whiches <- .whiches & (jobs$hours_week == 0)
.whiches <- which(grepl("9.*5",jobs$hours_shift))
jobs$hours_week[.whiches] <- 40
.whiches <- which(grepl("3.*11",jobs$hours_shift))
jobs$hours_week[.whiches] <- 40
grep("[[:digit:]]",jobs$hours_shift[which(jobs$hours_week==0)])
jobs$hours_week[which(jobs$hours_week==0)] <- 40
sum(jobs$hours_week==0,na.rm=T)
# wow what a mess

jobs$work_location[jobs$work_location == "Not Used"] <- NA

#--- Convert all salaries to annual ---#
cat("Convert all salaries to annual...")

# assume 11 days (2.14 wks) vacation a year, based on page 479 of
# http://www.bls.gov/ncs/ebs/benefits/2012/ebbl0050.pdf
# so 354 paid days a year => 354/7 = 50.57 paid weeks
jobs$annual_salary_midpoint <- jobs$annual_salary_lower <- jobs$annual_salary_upper <- 0

freq <- "Annual"
jobs[jobs$salary_frequency==freq,] <- within(jobs[jobs$salary_frequency==freq,],{
     annual_salary_lower <- salary_range_from
     annual_salary_upper <- salary_range_to
})

freq <- "Hourly"
jobs[jobs$salary_frequency==freq,] <- within(jobs[jobs$salary_frequency==freq,],{
     annual_salary_lower <- 50.57*hours_week*salary_range_from
     annual_salary_upper <- 50.57*hours_week*salary_range_to
})

freq <- "Daily"
jobs[jobs$salary_frequency==freq,] <- within(jobs[jobs$salary_frequency==freq,],{
  annual_salary_lower <- 354*salary_range_from
  annual_salary_upper <- 354*salary_range_to
})

jobs$annual_salary_midpoint <- (jobs$annual_salary_upper + jobs$annual_salary_lower) /2
jobs$annual_salary_range <- jobs$annual_salary_upper - jobs$annual_salary_lower
jobs$salary <- jobs$annual_salary_midpoint # to save typing later!



cat("save data frame `jobs` to jobs.RDa")
save(jobs,file="jobs.RDa")

rm(list=ls()[!(ls() %in% .saveworkspace.data_processing.R)])
