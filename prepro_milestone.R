library(ggplot2)
library("R.utils")
library(stringr)
library(tm)

set.seed(1234)
#filename <- c("en_US.twitter.txt","en_US.news.txt","en_US.blogs.txt")
filename <- c("en_US.news.txt")
sampletext <- c()

for (j in filename)
{
  cl <- countLines(j)
  samplesize <- 500
  sampleindex <- sample(seq(1,cl),samplesize)
  print(paste("file:",j," Number of lines:",cl))
  con <- file(description=j, open="r")
  for(i in 1:cl) {
    tmp <- readLines(con, n=1)
    if (length(which(sampleindex == i))>0){
      # Remove non ASCII chars, punctuation signs and numbers before storing
      tmp <- iconv(tmp, "latin1", "ASCII", sub="")
      tmp <- str_replace_all(tmp, "[[:punct:]]", "")
      tmp <- str_replace_all(tmp, "[[:digit:]]", "")
      tmp <- str_replace_all(tmp,"(^[[:space:]]+|[[:space:]]+$)", "")
      sampletext <- c(sampletext,tolower(tmp))
    }
  }
  close(con)
  vs <- VectorSource(sampletext)
  term_matrix <-DocumentTermMatrix(Corpus(vs))
  count_repetitions <- sort(colSums(as.matrix(term_matrix)), decreasing=TRUE)
  top_cut <- 20
  top_terms <- count_repetitions[1:top_cut]
  dfauxtot <- data.frame(keyName=names(count_repetitions), value=count_repetitions, row.names=NULL)

  
  dfaux <- transform(dfauxtot, keyName=reorder(keyName,value))
  dfaux <- dfauxtot[1:top_cut,]
  dfaux <- transform(dfaux, keyName=reorder(keyName,value))
  histo_all <- ggplot(dfaux, aes(keyName, value)) + xlab("Word") + ylab (paste("Repetitions in",length(filename)*samplesize,"senteces")) +
                geom_bar(width = 0.75, stat="identity",color="white",fill = "Light Blue") + 
                theme(legend.position = "none") + ggtitle("1-gram, all words") +
                coord_flip() + theme_bw()
  print(histo_all)
  stopwords <- c("a","about","above","after","again","against","all","also","am","an","and","any","are","arent","as",
                 "at","be","because","been","before","being","below","between","both","but","by","cant","cannot",
                 "could","couldnt","did","didnt","do","does","doesnt","doing","dont","down","during","each",
                 "few","for","from","further","get","got","had","hadnt","has","hasnt","have","havent","having",
                 "he","hed","hes","her","here","heres","hers","herself","him","himself","his","how",
                 "hows","i","id","im","ive","if","in","into","is","isnt","it","its","itself","lets","me","more",
                 "most","my","myself","no","nor","not","of","off","on","once","only","or","other","ought","our",
                 "ours","ourselves","out","over","own","same","she","shed","shes","should","shouldnt","so","some",
                 "such","than","that","thats","the","their","theirs","them","there","theres","these","they",
                 "theyd","theyll","theyve","this","those","through","to","too","under","until","up","very",
                 "was","wasnt","we","wed","were","weve","werent","what","whats","when","whens","where","which",
                 "while","who","whos","whom","why","whys","will","with","wont","would","wouldn","you","youd","youll",
                 "youre","youve","your","yours","yourself","yourselves")
  dfns <- dfauxtot[!is.element(dfauxtot$keyName,stopwords),]
  dfns <- dfns[rev(order(dfns$value)),][1:top_cut,]
  dfns <- transform(dfns, keyName=reorder(keyName,value))
  histo_nonstop<- ggplot(dfns, aes(keyName, value)) + xlab("Word") + ylab (paste("Repetitions in",length(filename)*samplesize,"senteces")) +
    geom_bar(width = 0.75, stat="identity",color="white",fill = "Coral") + 
    theme(legend.position = "none") + ggtitle("1-gram, excluding stop words") +
    coord_flip() + theme_bw()
  print(histo_nonstop)
}