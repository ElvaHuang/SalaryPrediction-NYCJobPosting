
##########################################################

##--     Preprocessing: Bad Words Cleaning            --##

##########################################################

bad_words <- grep("[^[:alpha:]]",vocab)
bad_words %in% docs[[1]][1,]
docs2 <- docs

for(i in 1:length(docs)) docs2[[i]][1,] <- vocab[docs[[i]][1,]]
docs2 <- lapply(docs2, function(doc) doc[, -which(doc[1,] %in% vocab[bad_words]) ])
vocab2 <- vocab[-bad_words]

for(i in 1:length(docs2)) docs2[[i]][1,] <- which(vocab2 %in% docs2[[i]][1,])
docs2 <- lapply(docs2,function(doc) t(apply(doc,1,as.numeric)) )