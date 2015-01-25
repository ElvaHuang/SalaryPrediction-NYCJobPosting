

##########################################################

##--     Variable Construction: Consolidate           --##

##########################################################

load("jobs.Rda")
getwd()

#library(tm)
#library(topicmodels)
#library(stm)
library(stringr)

##bin the agency variable
names(jobs)
#out<-textProcessor(jobs$agency,NULL)
#vocab<-out$vocab
#docs<-out$documents
#fit<-stm(documents=docs,vocab=vocab,K=5,data=NULL)

#category<-labelTopics(fit,topics=1:5)
#names(fit)
#agency<-apply(fit$theta,1,which.max)

table(jobs$agency)
fin<-c(1,9,19,21,16,32) #finance
infra<-c(7,11,17,18,23,26,28,38,25,10) # infrastructure
soc<-c(2,6,8,27,31,34,35,39) # social
law<-c(3,4,13,14,15,20,22,24,25,29,33,37)
sec<-c(5,30,36)
it<-12

sum(jobs$agency %in% unique(jobs$agency)[fin]) #137
sum(jobs$agency %in% unique(jobs$agency)[infra]) #313
sum(jobs$agency %in% unique(jobs$agency)[soc]) #538
sum(jobs$agency %in% unique(jobs$agency)[law]) #488
sum(jobs$agency %in% unique(jobs$agency)[sec]) #51
sum(jobs$agency == unique(jobs$agency)[it]) #121

jobs$agency.bin<-NA
jobs$agency.bin[jobs$agency %in% unique(jobs$agency)[fin]]<-"finance"
jobs$agency.bin[jobs$agency %in% unique(jobs$agency)[infra]]<-"infrastructure"
jobs$agency.bin[jobs$agency %in% unique(jobs$agency)[soc]]<-"socialService"
jobs$agency.bin[jobs$agency %in% unique(jobs$agency)[law]]<-"law"
jobs$agency.bin[jobs$agency %in% unique(jobs$agency)[sec]]<-"security"
jobs$agency.bin[jobs$agency %in% unique(jobs$agency)[it]]<-"IT"

table(jobs$agency.bin)
jobs$agency.bin<-as.factor(jobs$agency.bin)

##bin the level variable

jobs$level.bin<-NA
jobs$level.bin[jobs$level=="00" | jobs$level=="01" |jobs$level=="02" |jobs$level=="03" |jobs$level=="04" ]="entry"
jobs$level.bin[jobs$level=="M1" |jobs$level=="M2" |jobs$level=="M3" |jobs$level=="M4" |jobs$level=="M5" |jobs$level=="M6" |jobs$level=="M7"]="manager"
jobs$level.bin[jobs$level=="4A" |jobs$level=="4B" |jobs$level=="3B" ]="chief"

table(jobs$level.bin)
jobs$level.bin<-as.factor(jobs$level.bin)

#write.csv(jobs,file="jobs.csv")

##bin the residency requriement variable

head(jobs$residency_requirement)
index<-str_detect(jobs$residency_requirement,"(not|no)")
jobs$residency.bin<-"required"
jobs$residency.bin[index]<-"NotRequired"

table(jobs$residency.bin)
jobs$residency.bin<-as.factor(jobs$residency.bin)



##geocode the addresses

#Online<-read.csv("./address/AddressOriginal.csv",header=T)
#gisZip<-read.csv("./address/gisZip.csv",header=T)
gisBoro<-read.csv("gisBoro.csv",header=T)

##match records

#names(Online)
#Online$desc<-as.character(Online$desc)
#head(Online$desc)

##extract the zip code info from the online geocodeing results
#x<-str_sub(Online$desc,nchar(Online$desc)-4,nchar(Online$desc))
#i<-str_detect(x,"[[:digit:]]{5}")
#x[!i]<-NA
#Online$zip<-x

#table(Online$zip)
#length(which(is.na(Online$zip)))  #359

##extract the zip code info from gis geocoding

#names(gisZip)
#class(gisZip$gisZip)
#unique(gisZip$gisZip)
#sum(is.na(gisZip$gisZip))  #281

#gisZip$agency<-jobs$agency
#save(gisZip,file="gisZip.Rda")

##extract the boro info from gis geocoding

View(gisBoro)
names(gisBoro)
unique(gisBoro$ARC_Zone)
sum(is.na(gisBoro$ARC_Zone))

gisBoro$agency<-jobs$agency
save(gisBoro,file="gisBoro.Rda")

#jobs$gisZip <- gisZip$gisZip
jobs$gisBoro <- gisBoro$ARC_Zone

##reading score construction

if(!require("koRpus")) install.packages("koRpus")
library("koRpus")
jobs$text=paste(jobs$business_title,jobs$civil_service_title,jobs$division_work_unit,jobs$job_description,jobs$minimum_qual_requirements,jobs$preferred_skills)

for (i in 1:length(jobs$text)){
  writeLines(jobs$text[i], paste("text",i,".txt", sep=""))
}

ll.files <- list.files(".", pattern="text[[:digit:]]+\\.txt", full.names=TRUE)
ll.tagged <- lapply(ll.files, tokenize, lang="en")
ll.gradelevel <- lapply(ll.tagged,flesch.kincaid)
list_fk = lapply(ll.gradelevel, slot, "Flesch.Kincaid")
grades = sapply(list_fk, "[[", "grade")

jobs$reading_score=grades


##text length construction

word=strsplit(jobs$text," ")
len=c()
for (i in 1:1638) {len[i]=length(word[[i]])}

##read the existed scoreLength data in 
scoreLength<-read.csv("scoreLength.csv")
names(scoreLength)

jobs$length_text<-scoreLength$length_text
jobs$reading_score<-scoreLength$reading_score

##save our data
names(jobs)
save(jobs,file="jobs.Rda")


