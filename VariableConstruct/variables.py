#Terminal:
#export PYTHONPATH=$PYTHONPATH:/Library/Python/2.6/site-packages

#R code
#load("/Users/baba_dong/Desktop/final/jobs.RData")
#jobs$salary_midpoint=(jobs$salary_range_from+jobs$salary_range_to)/2
#summary(jobs$salary_midpoint)
#jobs <- cbind(jobs, salary_category=cut(jobs$salary_midpoint, breaks=c(7.5,45110,67500,88620,180000))) 
#write.csv(jobs, file="jobs.csv", row.names=FALSE)


import nltk
import nltk.data
import csv
from ngramDemo import ngram
from nltk.tokenize import *
from nltk.corpus import stopwords

tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')


#IMPORT DATA
jobs=csv.reader(open("jobs.csv","rb"))


alljobs=[]
for row in jobs:
	alljobs.append(row)


alljobstext={}
for row in alljobs:
	alljobstext[row[0]]=row[13]+row[14]+row[15]


def jobWF(jobstext):
    dic={}
    for jid in jobstext.keys():
        dic[jid]=wordgram(jobstext[jid])
    return dic


def removeStopwords(wordlist):
     return [ word for word in wordlist if word not in stopwords.words('english') ]

#Generate unigrams, bigrams, and trigrams along with their frequencies
def wordgram(text):
	ng=ngram()
	text=text.lower()
	ng.unigram(text)
	ng.bigram(text)
	ng.trigram(text)
	u=ng.saveAll()
	return u



#Select k top words with highest scores for each class.
#score=number of times of that word is issued by all jobs in one salary class
      #/number of times of that word is issued by all jobs in all classes
#Still in progress



#For each job, estimate a probability(score) for each top word

def jobProb(wordLists,jobWF):
    wordList=sum(wordLists,[])
    jobProb={}
    for i,wf in jobWF.items():
        SUM=0
        for k,v in wf.items():
            SUM=SUM+v            
        prob={}
        sum1=0
        sum2=0
        for w in wordList:            
            if w in wf.keys():
                    #probability of each word for each job=
                    #number of times this top word is issued by this job
                    #/number of times all words are issued by this job
                if SUM!=0:
                    prob[w]=float(wf[w])/float(SUM)
                else:
                    prob[w]=0
            else:
                prob[w]=0
        jobProb[i]=prob
    return jobProb

#test
#dic={'j1':{'finance':10,'insurance':1},'j2':{'insurance':2},\
#     'j3':{'IT':5,'computer':3,'insurance':3},\
#     'j4':{}}
dic=jobWF(alljobstext)
topwordlists=[['finance','insurance'],['IT','computer']]
prob=userProb(topwordlists,dic)
#print prob






