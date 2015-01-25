

##########################################################

##--     Preprocessing: Fix Geocode Missing Data      --##

##########################################################

load("jobs.Rda")

repeat_locations <- which(jobs$work_location==jobs$work_location_1)
na_boro <- which(gisBoro$ARC_Zone %in% c("E","S","T","D","R","K"))

View(jobs$work_location[na_boro])

jobs$gisBoro[grep("Brooklyn",jobs$work_location,fixed=T)] <- 3 #brooklyn
jobs$gisBoro[grep("Wards Island",jobs$work_location,fixed=T)] <- 1 #manhattan
jobs$gisBoro[grep("Elm",jobs$work_location_1,fixed=T)] <- 4 #queens
jobs$gisBoro[grep("Glenwood",jobs$work_location,fixed=T)] <- 3 #brooklyn
jobs$gisBoro[grep("Olmsted",jobs$work_location,fixed=T)] <- 4 #queens
jobs$gisBoro[grep("Quee",jobs$work_location)] <- 4 #queens

jobs$gisBoro[jobs$gisBoro==" "] <- NA

jobs$gisBoro <- factor(jobs$gisBoro)

save(jobs,file="jobs.Rda")

