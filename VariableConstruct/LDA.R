
#########################################################

##--        Variable Construction: LDA               --##

#########################################################

library("topicmodels")
library("tm")
library("FNN")
load("jobs.Rdata")

jobs.train$text = paste(jobs.train$business_title,jobs.train$civil_service_title,
	jobs.train$division_work_unit, jobs.train$job_description,
	jobs.train$minimum_qual_requirements,jobs.train$preferred_skills)

dir.create(file.path(getwd(),"texts_train"))

for (i in 1:length(jobs.train$text)){
	writeLines(jobs.train$text[i], paste("texts_train/text",i,".txt", sep=""))
}

std = function(x){ if(length(which(is.na(x)))==0) (x-mean(x))/sd(x) else
 (x-mean(x,na.rm=T))/sd(x,na.rm=T) }

word = strsplit(jobs.train$text," ")
len = c()
for (i in 1:length(jobs.train$text)) {len[i]=length(word[[i]])}


cp <- Corpus(DirSource("texts_train"))
dtm = DocumentTermMatrix(cp,control = list(removePunctuation = TRUE, stopwords = TRUE,
	removeNumbers = TRUE, tolower = TRUE, stemming = TRUE))


dir.create(file.path(getwd(), "LDA_topic_doc"))

kl = c()
for (i in seq(5,100,by = 5)){
	lll = LDA(dtm, k = i)
	topic_doc = slot(lll, "gamma")
	write.csv(topic_doc,paste("LDA_topic_doc/LDA", i, ".csv",sep=""), row.names = FALSE)
	prob = posterior(lll)
	M1 = prob$terms
	M2 = prob$topics
	svdM1 = svd(M1)
	CM1 = svdM1$d
	CM1 = std(CM1)
	CM2 = len %*% M2
	CM2 = unlist(as.list(std(CM2)))
	kl[i/5] = mean(abs(KL.dist(CM1,CM2,k=4)))
}

write.csv(kl,"kl_lda.csv", row.names = FALSE)
