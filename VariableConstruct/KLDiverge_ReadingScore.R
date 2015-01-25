
#######################################################################

##--    Variable Construction:Reading Score & KL Divergence        --##

#######################################################################

#Reading score

library("koRpus")
jobs$text = paste (jobs$business_title,jobs$civil_service_title,jobs$division_work_unit,jobs$job_description,jobs$minimum_qual_requirements,jobs$preferred_skills)

for ( i in 1:length(jobs$text) ){
	writeLines(jobs$text[i], paste("texts/text", i, ".txt", sep="") )
}

ll.files <- list.files("texts", pattern="*.txt", full.names=TRUE)
ll.tagged <- lapply(ll.files, tokenize, lang="en")
ll.gradelevel <- lapply(ll.tagged,flesch.kincaid)
list_fk = lapply(ll.gradelevel, slot, "Flesch.Kincaid")
grades = sapply(list_fk, "[[", "grade")

jobs$reading_score = grades


#LDA

library("topicmodels")
library("tm")
library("FNN")

std = function (x) {if(length(which(is.na(x)))==0) (x-mean(x))/sd(x) else
 (x-mean(x,na.rm=T))/sd(x,na.rm=T) }

word = strsplit(jobs$text," ")
len = c()
for ( i in 1:1638 ) { len[i] = length(word[[i]]) }

cp <- Corpus(DirSource("texts"))
dtm = DocumentTermMatrix(cp, control = list (removePunctuation=TRUE,stopwords=TRUE,removeNumbers=TRUE,
tolower = TRUE) )

kl = c()
for (i in seq (5,100,by = 5) ){
	lll = LDA(dtm,k=i)
	prob = posterior(lll)
	M1 = prob$terms
	M2 = prob$topics
	#M1=slot(lll,"beta")
	#M2=slot(lll,"gamma")
	svdM1 = svd(M1)
	CM1 = svdM1$d
	CM1 = std(CM1)
	CM2 = len %*% M2
	CM2 = unlist(as.list(std(CM2)))
	kl[ i/5 ] = mean(abs(KL.dist(CM1,CM2,k=5)))
}

numT = seq(5,100,by=5)

plot(numT, kl, type="l", xlab="Number of topics", ylab="Symmetric KL divergence")
