
##########################################################

##--     Variable Construction: STM                   --##

##########################################################

require(stm)
require(FNN)

#use jobs.Rdata
#####STM
#jobs.train$text=paste(jobs.train$business_title,jobs.train$civil_service_title,jobs.train$division_work_unit,jobs.train$job_description,jobs.train$minimum_qual_requirements,jobs.train$preferred_skills)
#library(stm)
#####transfer agency to level
#as.factor(jobs.train$agency)
#fin<-c(1,9,19,21,16,32) #finance
#infra<-c(7,11,17,18,23,26,28,38,25,10) # infrastructure
#soc<-c(2,6,8,27,31,34,35,39) # social
#law<-c(3,4,13,14,15,20,22,24,25,29,33,37)
#sec<-c(5,30,36)
#it<-12

#jobs.train$agency.bin<-NA
#jobs.train$agency.bin[jobs.train$agency %in% unique(jobs.train$agency)[fin]]<-"finance"
#jobs.train$agency.bin[jobs.train$agency %in% unique(jobs.train$agency)[infra]]<-"infrastructure"
#jobs.train$agency.bin[jobs.train$agency %in% unique(jobs.train$agency)[soc]]<-"socialService"
#jobs.train$agency.bin[jobs.train$agency %in% unique(jobs.train$agency)[law]]<-"law"
#jobs.train$agency.bin[jobs.train$agency %in% unique(jobs.train$agency)[sec]]<-"security"
#jobs.train$agency.bin[jobs.train$agency %in% unique(jobs.train$agency)[it]]<-"IT"

#jobs.train$agency.bin<-as.factor(jobs.train$agency.bin)

##bin the level variable
#jobs.train$level.bin<-NA
#jobs.train$level.bin[jobs.train$level=="00" | jobs.train$level=="01" |jobs.train$level=="02" |jobs.train$level=="03" |jobs.train$level=="04" ]="entry"
#jobs.train$level.bin[jobs.train$level=="M1" |jobs.train$level=="M2" |jobs.train$level=="M3" |jobs.train$level=="M4" |jobs.train$level=="M5" |jobs.train$level=="M6" |jobs.train$level=="M7"]="manager"
#jobs.train$level.bin[jobs.train$level=="4A" |jobs.train$level=="4B" |jobs.train$level=="3B" ]="chief"

#table(jobs.train$level.bin)
#jobs.train$level.bin<-as.factor(jobs.train$level.bin)
#####stm

metadata=cbind(jobs$level.bin,jobs$agency.bin)
metadata=data.frame(metadata)
out=textProcessor(jobs$text, metadata=metadata,lowercase=TRUE,
                  removestopwords=TRUE,removenumbers=TRUE,
                  removepunctuation=TRUE,stem=TRUE)

docs<-out$documents
vocab<-out$vocab
meta=out$meta

#####select K


######testtest
#amc=topics7$theta
#amc=log(amc)
#amc=data.frame(amc)
#amc$salary=jobs.train$salary
#lm2=lm(salary~.,amc)
####test


std=function(x){if(length(which(is.na(x)))==0) (x-mean(x))/sd(x) else
  (x-mean(x,na.rm=T))/sd(x,na.rm=T)}

word=strsplit(jobs$text," ")
len=c()
for (i in 1:length(jobs$text)) {len[i]=length(word[[i]])}


for (i in seq(15,30,by=5)){
  topics=stm(documents=docs,vocab=vocab, K=i,
             prevalence=~jobs$agency.bin+jobs$level.bin,
             content=jobs$agency.bin,
             data=meta)
  write.csv(topics$theta,paste("theta",i,".csv",sep=""),row.names=FALSE)
  write.csv(topics$mu$mu,paste("mu",i,".csv",sep=""),row.names=FALSE)
  write.csv(topics$mu$gamma,paste("gamma",i,".csv",sep=""),row.names=FALSE)
  kl=c()
  for (j in 1:6){
    M1=exp(topics$beta$logbeta[[j]])
    index=which(jobs$agency.bin==levels(jobs$agency.bin)[j])
    M2=topics$theta[index,]
    svdM1=svd(M1)
    CM1=svdM1$d
    CM1=std(CM1)
    len1=len[index]
    CM2=len1 %*% M2
    CM2=unlist(as.list(std(CM2)))
    kl[j]=mean(abs(KL.dist(CM1,CM2,k=4)))
  }
  kl_avg[i/5]=mean(kl)
}

write.csv(kl_avg,"KL_avg.csv",row.names=FALSE)
#