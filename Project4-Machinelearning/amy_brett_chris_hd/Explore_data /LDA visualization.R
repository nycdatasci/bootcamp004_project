### reference site
#http://cpsievert.github.io/LDAvis/reviews/reviews.html


library(tm)
library(SnowballC)
library(Matrix)
library(lda)
library(LDAvis)
library(servr)
library(openNLP)
setwd("~/Documents/kaggle home depot")
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

# read data
train_o= read.csv("train.csv",stringsAsFactors=FALSE, fileEncoding="latin1");
test_o= read.csv("test.csv",stringsAsFactors=FALSE, fileEncoding="latin1");
descp_o=read.csv("product_descriptions.csv",stringsAsFactors=FALSE, fileEncoding="latin1")
# merge 
# reference:http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
train=merge(x = train_o, y = descp_o, by = "product_uid", all.x = TRUE)
test=merge(x=test_o,y=descp_o,by="product_uid",all.x = T)

txt = paste(train$search_term,train$product_title);
# txt = c(txt,paste(test$search_term,test$product_title)); # too much data I have to reduce the size


# pre-processing
txt <- gsub("'", "", txt)  # remove apostrophes
txt <- gsub("[[:punct:]]", " ", txt)  # replace punctuation with space
txt <- gsub("[[:cntrl:]]", " ", txt)  # replace control characters with space
txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
txt <- tolower(txt)  # force to lowercase
txtTag <- tagPOS(txt)
sapply(strsplit(txtTag,"[[:punct:]]*/VB.?"),function(x) sub("(^.*\\s)(\\w+$)", "\\2", x))


# tokenize on space and output as a list:
doc.list <- strsplit(txt, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# stop words
stop_words = c('http','www','img','border','color','style','padding','table','font','thi','inch','ha','width','height')

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)



# Compute some statistics related to the data set:
D <- length(documents)
W <- length(vocab)  
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
N <- sum(doc.length)  
term.frequency <- as.integer(term.table)

# MCMC and model tuning parameters:
K <- 20
G <- 1000
alpha <- 0.02
eta <- 0.02

# Fit the model:
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  


### Visualizing the fitted model with LDAvis
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

results <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = vocab,
                term.frequency = term.frequency)


# create the JSON object to feed the visualization:
json <- createJSON(phi = results$phi, 
                   theta = results$theta, 
                   doc.length = results$doc.length, 
                   vocab = results$vocab, 
                   term.frequency = results$term.frequency)

serVis(json, out.dir = './', open.browser = FALSE)
system("mv index.html results.html")