#Heramb Vijay Uttarwar
#A20398330
#CS-422 HW-4

library(data.table)
library(curl)
library(cluster)
library(factoextra)
library(psych)

setwd("C:/Users/Heramb/Desktop/CS 422/HW 4/")
rm(list=ls())

lang <- fread('https://people.sc.fsu.edu/~jburkardt/datasets/hartigan/file46.txt')
#Used to fetch the data from the address

lang1 <- read.csv("lang", header = T, sep = ",", row.names = 'Country')
lang1$X=NULL

#2.1(a)
hc.single <- factoextra::eclust(lang1, "hclust", hc_method="single")
fviz_dend(hc.single, show_labels=TRUE, palette="jco", as.ggplot=T)

hc.complete <- factoextra::eclust(lang1, "hclust", hc_method="complete")
fviz_dend(hc.complete, show_labels=TRUE, palette="jco", as.ggplot=T)

hc.average <- factoextra::eclust(lang1, "hclust", hc_method="average")
fviz_dend(hc.average, show_labels=TRUE, palette="jco", as.ggplot=T)

#2.1(b)

#1.Method->Single
#Two singleton clusters {Great Britain,Ireland},{West Germany,Austria},{Luxemburg,Switzerland},{France, Belgium},{Denmark, Norway}

#2.Method->Complete
#Two singleton clusters {Denmark, Norway}, {Great Britain,Ireland},{West Germany,Austria},{Luxemburg,Switzerland}, {France, Belgium}

#3.Method->Average
#Two singleton clusters {Portugal,Spain},{Denmark, Norway},{France, Belgium}, {Great Britain,Ireland},{West Germany,Austria},{Luxemburg,Switzerland}

#2.1(c)
#I think Italy should be clustered with a large cluster. Looking at the raw data, Italy has higher dissimilarity with other countries.
#Thus, if we cut at certain cutoff will lead Italy as a outlier and will form a cluster with lesser dissimilarity.

#2.1(d)
#Purity as the linkage strategy that produces the most two-singleton cluster, there is only one method i.e "Average".
#Linkage with method="Average" is pure by definition

#2.1(e)
cuttree.125<-cutree(hc.average,h=125)
table(cuttree.125)
#There are 7 clusters at height 125.

#2.1(f)

hc.single1 <- factoextra::eclust(lang1, "hclust", k=7, hc_method="single")
fviz_dend(hc.single1, show_labels=TRUE, palette="jco", as.ggplot=T)

hc.complete1<- factoextra::eclust(lang1, "hclust", k=7, hc_method="complete")
fviz_dend(hc.complete1, show_labels=TRUE, palette="jco", as.ggplot=T)

hc.average1 <- factoextra::eclust(lang1, "hclust",k=7, hc_method="average")
fviz_dend(hc.average1, show_labels=TRUE, palette="jco", as.ggplot=T)

#2.1(g)
library(fpc)
ct<-dist(lang1)

stats <- cluster.stats(ct, hc.single1$cluster, silhouette=TRUE)
stats$dunn
stats$avg.silwidth

stats1 <- cluster.stats(ct, hc.complete1$cluster, silhouette=TRUE)
stats1$dunn
stats1$avg.silwidth

stats2<- cluster.stats(ct, hc.average1$cluster,silhouette=TRUE)
stats2$dunn
stats2$avg.silwidth

#2.1(h)
#Dunn index for hc.average is maximum. Thus, hc.average is the best cluster.

#2.1(i)
#silhouette width for hc.complete is maximum. Thus, hc.complete is the best cluster.



#2.2
library(textreuse)
files <- list.files("C:/Users/Heramb/Desktop/CS 422/HW 4/corpus", full.names = T)
minhash <- minhash_generator(n=160, seed=100)

#2.2(a)
corpus <- TextReuseCorpus(files, tokenizer = tokenize_ngrams, n = 5,
                          minhash_func = minhash, keep_tokens = TRUE)
length(unlist(tokens(corpus)))

#2.2(b)
library(magrittr)
totaltokens <- tokens(corpus)
corpusMat <- list.files("C:/Users/Heramb/Desktop/CS 422/HW 4/corpus", full.names=F)
doc_dict <- unlist(tokens(corpus)) %>% unique()
Matr <- lapply(totaltokens, function(set, dict) {   as.integer(dict %in% set)}, dict = doc_dict) %>% data.frame() 
tempSetName <-setNames( Matr, paste( corpusMat, 1:length(corpusMat)) )
rownames(Matr) <- doc_dict
dim(Matr)
#--->Dimensions of characteristics matrix:- Number of unique shingles(rows)  * Number of files(columns)
#i.e. 17614*100

#2.2(c)
tokens(corpus[["orig_taske"]])[1:5]

#2.2(d)
#As we choose only 240 rows for signature matrix, The dimensions of signature matrix will become 240*100.
# while we have generated Characteristic matrix of dimension 17614*100.
# reduction of size of problem is 98.637%.

#2.2(e)
lsh_probability(h = 240, b =  60, s = 0.3)
#This probability is less than the desired one and thus increasing the bands to 80.
lsh_probability(h = 240, b =  80, s = 0.3)
#We get the desired probability i.e 80 at bands=80.

#2.2(f)
buckets <- lsh(corpus, bands = 80)
candidates <- lsh_candidates(buckets)
noofcandidatepair<-nrow(candidates)
#Number of candidate pairs
noofcandidatepair

#2.2(g)
lshResult <- lsh_compare(candidates, corpus, jaccard_similarity)
lshResult[order(lshResult$score,decreasing = TRUE),][1:5,]

#2.2(h)
#If we dont use Locality Sensative Hashing and directly examined every pair for similarity then
# Number of pairs of documents to be examined  = (No of Documents)C2
# Here we can write => No of Documents = 100
# No of pairs = 100C2 = 100!/(98!*2!) = 4950
# Solution for 2.2 (h)(ii)
# No of candidate pairs generated in 2.2 (f) = 72.
# The ratio of doc pair to candidate pair number is : 4950/72 = 68.75
# It shows that if we dont do Locality Sensative Hashing the number of comparisons we have to do is 68.75 times than number of comparisons we will do after doing Locality Sensative Hashing.

#2.3(a)

u.item<-read.csv("C:/Users/Heramb/Desktop/CS 422/HW 4/ml-100k/u.item", sep = "|",comment.char = "#")
u.item$X1=NULL
u.data<-read.csv("C:/Users/Heramb/Desktop/CS 422/HW 4/ml-100k/u.data", sep = "\t", header = T,comment.char = "#")
#2.3(i)
user200_rownumber <- which(u.data$user== 200)
user50_rownumber <- which(u.data$user == 50)

user200 <- u.data[user200_rownumber,]
user50 <- u.data[user50_rownumber,]

movies200 <-u.item[user200[,2],]
movies50 <- u.item[user50[,2],]

movie.matrix200 <- movies200[,6:24] 
genre200 <- apply(movie.matrix200,2,mean)
vector200 <- as.vector(genre200)

movie.matrix50 <- movies50[,6:24]
genre50 <- apply(movie.matrix50,2,mean)
vector50 <- as.vector(genre50)

cosine <- function(x, y) {
  # Need to do error checking:
  # 1) Ensure x and y are vectors.
  
  sum(x*y)/(norm(x, type="2") * norm(y, type="2"))
}

cosine(vector50,vector200)
#cluster_similarity(vector200, vector50, similarity="jaccard", method="independence")

#2.3(ii)
movie127 <- u.item[127,]
vector127 <- as.vector(movie127[,6:24])
cosine(vector127,vector50)

#2.3(iii)
cosine(vector127,vector200)

#2.3(iv)
#The movie 127 will be recommended to user 50.

#2.3(b)
library(reshape2)
library(reshape)

utilitymatrix<-matrix(0,6,11)

for(i in 1:length(unlist(u.data[,1])))
{
  if(u.data[i,1]==1 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],1]<-u.data[i,3]
  }
  
  if(u.data[i,1]==21 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],2]<-u.data[i,3]
  }
  if(u.data[i,1]==44 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],3]<-u.data[i,3]
  }
  if(u.data[i,1]==59 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],4]<-u.data[i,3]
  }
  if(u.data[i,1]==72 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],5]<-u.data[i,3]
  }
  if(u.data[i,1]==82 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],6]<-u.data[i,3]
  }
  if(u.data[i,1]==102 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],7]<-u.data[i,3]
  }
  if(u.data[i,1]==234 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],8]<-u.data[i,3]
  }
  if(u.data[i,1]==268 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],9]<-u.data[i,3]
  }
  if(u.data[i,1]==409 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],10]<-u.data[i,3]
  }
  if(u.data[i,1]==486 && u.data[i,2]<7)
  {
    utilitymatrix[u.data[i,2],11]<-u.data[i,3]
  }
}
colnames(utilitymatrix)<-c("user1","user21","user44","user59","user72","user82","user102","user234","user268","user409","user486")

#View(utilitymatrix)


means <- apply(utilitymatrix, 1, function(x) mean(x, na.rm=T))

means

for (i in 1:dim(utilitymatrix)[1]) {
  for (j in 1:dim(utilitymatrix)[2])
  {
    if(utilitymatrix[i,j]>0)
    {
      utilitymatrix[i,j] <- utilitymatrix[i,j] - means[i]
    }
  }
}
similarmovie<-matrix(0,6,1)

for (i in 1:dim(utilitymatrix)[1])
{
  similarmovie[i,1]<-round(cosine(utilitymatrix[5,], utilitymatrix[i, ]), digits=2)
}


rownames(similarmovie)<-c("1","2","3","4","5","6")
a<-as.numeric(rownames(similarmovie)[order(similarmovie, decreasing=TRUE)][1:6])

rating268<-((similarmovie[a[2],1]*utilitymatrix[a[2],9])+(similarmovie[a[3],1]*utilitymatrix[a[3],9])+(similarmovie[a[4],1]*utilitymatrix[a[4],9]))/(similarmovie[a[2],1]+similarmovie[a[3],1]+similarmovie[a[4],1])
rating268

