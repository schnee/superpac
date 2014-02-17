require(wordcloud)
require(tm)
require(RWeka)
p <- read.csv("./committee_summary.csv", header=TRUE)
mydata.corpus = Corpus(VectorSource(p$Name))
# make each letter lowercase
mydata.corpus <- tm_map(mydata.corpus, tolower)
# remove punctuation
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
tri.tdm <- TermDocumentMatrix(mydata.corpus)
freqs = findFreqTerms(tri.tdm, lowfreq=6)
theFrequencies= as.matrix(tri.tdm[freqs,dimnames(tri.tdm)$Docs])
theFreqTerms.df= stack(rowSums(theFrequencies))
theFreqTerms.df=theFreqTerms.df[with(theFreqTerms.df, order(-values)),]
tops = as.character(theFreqTerms.df[1:20,]$ind)
wordcloud(mydata.corpus)
