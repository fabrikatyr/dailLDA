#Load/install necessary packages
#install.packages("devtools")
#install.packages("knitr")

library(devtools)
#install.packages(c("plyr","reshape", "proxy", "shiny"))
#install_github("cpsievert/LDAvis")
#install_github("fabrikatyr/LDAtools")
#install.packages("knitr")
#install.packages("topicmodels")
library(topicmodels)
#install.packages("NLP")
library(NLP)
#install.packages("tm")
library(tm)
library(plyr)
#install.packages("XML")
library(XML)
#install.packages("stringi")
library(stringi)
library(proxy)
library(shiny)
#install.packages("stats4")
#require(stats4) 
require(methods) 
require(modeltools)
#install.packages("slam")
require(slam)
#install.packages("rJava")
library(rJava)
#install.packages("splitstackshape")
library(splitstackshape)

install.packages("wordnet")
library(wordnet)
setDict("C://csvn/dict")
#install.packages("SnowballC")
library("SnowballC")
#install.packages("Rmpfr")
library(Rmpfr)
library(reshape2)
#library(reshape)
install.packages("ggplot2")
library(ggplot2)

#Dev packages
library(LDAtools)
library(LDAvis)


data <- read.csv("Dail_debate_20140820_Topics.csv")



#get all the records into a giant corpus for analysis

  datadata <- data[,2]
  #test statement to check with small set
  #datadata2 <- data[1:5000,2]

  corp <- Corpus(VectorSource(datadata))
  #Basic Cleanse of corpus
  #removing off topic dail words  
  dailstops <- c("point", "view","refer","put"   ,"time","provide","members","taoiseach" ,  "regard"  , "service" ,  "take"   ,   "way"   ,   "work"  ,   "year" ,  "ask"    ,  "bill"    , "can"    ,  "case"  ,   "come"   ,  "get"   ,   "made"   ,  "make"  ,  "need", "now"  , "one"  ,    "place"   , "provid"  , "public" ,"year","house","must","text","also","will","minister","I","this","the","deputy","deputies","government","we","people","matter","issue","country","it","to","state")
  
  #Preprocess the text and convert to document-term matrix
  dtm.control <- list(
  tolower = TRUE,
  removePunctuation = TRUE,
  removeNumbers = TRUE,
  stopwords = c(stopwords("english"),dailstops),
  stemming = TRUE,
  wordLengths = c(3, Inf),
  weighting = weightTf
  )

  corp.dtm <- DocumentTermMatrix(corp,control = dtm.control)
  #trim things down to save on speed
  rowcorp.dtm <- removeSparseTerms(corp.dtm, 0.850)

  m <- as.matrix(rowcorp.dtm)  
  termcount <-findFreqTerms(rowcorp.dtm, lowfreq=1000) 
  frequentwords <- sort(colSums(m[,termcount]), decreasing=TRUE)
  frequentwords <-row.names(data.frame(frequentwords))
  
  #use slam package to check for empty rows
  #rowTotals <- rollup(corp.dtm, 2, na.rm=TRUE, FUN = sum)  
  

  #remove all docs without words
  rowTotals <- rowSums(as.matrix(rowcorp.dtm))
  rowcorp.dtm   <- rowcorp.dtm[rowTotals> 0, ]           
  
  #capture responses with 'no topic'
  blankrow <- which(rowTotals==0)
  blank.dtm   <- corp.dtm[rowTotals== 0, ]           #docs with no topic
  

#Optimum topic number
#function to get the harmonic mean of the loglikelihoods, and then   
harmonicMean <- function(logLikelihoods, precision=2000L) {

  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# The log-likelihood values are then determined by first fitting the model using for example

k = 20
burnin = 1000
iter = 1000
keep = 50

#only going as high as 25, as manually classifying any higher that that will take a lot of manual effort
ks <- seq(10, 50, by = 5)
#creating 'ks' lda models of the corpus
models <- lapply(ks, function(k) LDA(rowcorp.dtm, k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep)))
saveRDS(models, 'dailmodelsdemo9.RDS')
#models = readRDS('dailmodels.RDS')


#maxmized value of likelihood function
logLiks <- lapply(models, function(L)  L@logLiks[-c(1:(burnin/keep))])

hm <- sapply(logLiks, function(h) harmonicMean(h))
#number of Parameters
k = sapply(models, function(L) sum(length(L@beta) + length(L@gamma)))

#getting the AIC of the model
AICs = 2*k -2*hm


# Find optimal model
library(ggplot2)

#install_github("noamross/noamtools")
#library(noamtools)
ldaplot <- ggplot(data.frame(hm, AICs), aes(x=ks, y=-AICs)) + geom_path(lwd=1.5) +
  theme(text = element_text(family='Lato'),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Relative Parsimony of Model (negative AIC)') +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of Dailrecords", atop(italic("How many distinct topics in the debate?"), ""))))
ldaplot
ggsave('LDA_AIC5.png', ldaplot, width=10, height=7)
# Tried to model the no. of distinct topics in the dail debates
opt <- models[which.min(AICs)][[1]]
#opt <- modelestimate

top.opt = ks[which.min(AICs)]
#top.opt= 25
# Extract the 'guts' of the optimal model
doc.id <- opt@wordassignments$i
token.id <- opt@wordassignments$j
topic.id <- opt@wordassignments$v
vocab <- opt@terms

# Get the phi matrix using LDAviz
dat <- getProbs(token.id, doc.id, topic.id, vocab, K = max(topic.id), sort.topics = "byTerms")
phi <- t(dat$phi.hat)
# NOTE TO SELF: these things have to be numeric vectors or else runVis() will break...add a check in check.inputs
token.frequency <- as.numeric(table(token.id))
topic.id <- dat$topic.id
topic.proportion <- as.numeric(table(topic.id)/length(topic.id))

# Run the visualization locally using LDAvis
z <- check.inputs(K=max(topic.id), W=max(token.id), phi, token.frequency, vocab, topic.proportion)
json <- with(z, createJSON(K=max(topic.id), phi, token.frequency, 
                           vocab, topic.proportion))



#runShiny(phi, token.frequency, vocab, topic.proportion)
serVis(json, out.dir="dail_lda9", open.browser = FALSE)

#getting table of who spoke about what

gammaDF <- as.data.frame(opt@gamma) 

names(gammaDF) <- (1:top.opt)

toptopics <- as.data.frame(cbind(document = row.names(gammaDF),topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
#remove the rows that made not sense
parsedset <- data[-blankrow[],]
finaltable <- cbind(parsedset,toptopics)

dataout <- write.csv(finaltable, file = "dailrecordsparsed2.csv")



