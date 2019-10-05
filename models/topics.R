##############################

# PCA SUR TOUTES LES DONNÉES # Beaucoup plus rapide que le LDA et donne des meilleurs résultats

##############################

#install.packages('tidytext')
library(data.table)
library('tidytext')
library(magrittr)
library(tm)
library(tidyr)
library(tidytext)
library(dplyr)
library(janeaustenr)
library(quanteda)
#install.packages("tm")
library(tm)
#install.packages('topicmodels')
library(topicmodels)
#install.packages('lsa')
library(lsa)

stop_words2=stop_words[!stop_words$word %in%

                         c('not','no',
                           # 'very','only','more',
                           'up',
                           #'so',
                           'few','most','too',
                           #'some',
                           'off',
                           #"isn't","aren't","wasn't","weren't","hasn't",
                           #"haven't","hadn't","doesn't","don't","didn't","won't",
                           #"wouldn't","shan't","shouldn't","can't","cannot","couldn't",
                           #"mustn't",
                           "look","out","welcome","better","best","full",
                           "generally","great","high","highest","higher","mostly","rooms","room",
                           "several","showed","shows","small","smaller","smallest","problem",
                           "never","needs","needing","nobody","never","needed","need"),]



myCorpus <- fread("../data/Hotel_Reviews.csv", data.table = F)
myCorpus$Reviewer_Nationality=as.character(myCorpus$Reviewer_Nationality)

Corpus_neg_nation = myCorpus[as.character(myCorpus$Negative_Review) != 'No Negative' & as.character(myCorpus$Negative_Review) != ' ',]
reviews_neg <- data.frame(txt = as.character(Corpus_neg_nation$Negative_Review),
                          doc = as.character(row.names(Corpus_neg_nation)),
                          stringsAsFactors = FALSE)

reviews_neg_ngram  = mutate(reviews_neg, text = gsub(x = txt, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 3, n_min = 2) %>%
  separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words2$word) %>%
  filter(!word2 %in% stop_words2$word) %>%
  filter(!word3 %in% stop_words2$word) %>%
  unite(ngram,word1, word2,word3, sep = " ")


#Pour enlever les 'NA' dans les bigram de mots
reviews_neg_ngram = mutate(reviews_neg_ngram, ngram = gsub(x = ngram, pattern = " NA", replacement = ""))
reviews_neg_ngram = mutate(reviews_neg_ngram, ngram = gsub(x = ngram, pattern = "NA", replacement = ""))

dim(reviews_neg_ngram)
dim(ngram_counts)
names(reviews_neg_ngram)
head(reviews_neg_ngram)
names(reviews_neg_ngram)
reviews_neg_ngram$ngram

# reviews_neg_ngram_dt <- as.data.table(reviews_neg_ngram)
# setkeyv(reviews_neg_ngram_dt, c("doc", "ngram"))
# ngram_counts_dt <- reviews_neg_ngram_dt[, .(.N), by = c("doc", "ngram")]

ngram_counts <- reviews_neg_ngram %>%
  count(doc, ngram)

ngram_counts =   ngram_counts %>%
  bind_tf_idf(doc, ngram, n) %>%
  arrange(desc(n))

ngram_counts2 = top_n(x=ngram_counts,n=10000, n)
ngram_counts3 = top_n(x=ngram_counts2,n=1000, tf_idf)
ngram_counts4 = ngram_counts3[,c(1:3)]

reviews_neg_ngram_dtm <- ngram_counts4 %>%
  cast_dtm(doc, ngram, n)

#On va enlever les terms les moins fréquents de manière à garder seulement les 1116 plus fréquents
#reviews_neg_ngram_dtm = removeSparseTerms(reviews_neg_ngram_dtm, 0.9996)

#On va ensuite enlever les reviews qui ne contiennent aucun des 1116 terms les plus fréquents
raw.sum=apply(reviews_neg_ngram_dtm,1,FUN=sum)
reviews_neg_ngram_dtm=reviews_neg_ngram_dtm[raw.sum!=0,]
total_TD_Neg=as.matrix(reviews_neg_ngram_dtm)

#total_TD_Neg_t=t(total_TD_Neg)
#russie_TD_Neg_t
#x.matrice =russie_TD_Neg_t[,colSums(russie_TD_Neg_t)!=0]

x.matrice =total_TD_Neg[,colSums(total_TD_Neg)!=0] #On ne veut pas de colonnes avec seulement des zéros
#x.matrice=newtable[,colSums(newtable)!=0] #On ne veut pas de colonnes avec seulement des zéros
#Centrer et réduire la matrice des fréquences

Lesmoyennes=as.matrix(t(apply(x.matrice,2,mean)))
Lesecart_types=sqrt(as.matrix(t(apply(x.matrice,2,var))))
z.matrice=x.matrice

for (j in 1:ncol(x.matrice)) {
  z.matrice[,j]= (x.matrice[,j] - Lesmoyennes[1,j]) / Lesecart_types[1,j]
}

t1 <- Sys.time()
a=svd(z.matrice, nv=20)
t2 <- Sys.time()
t2 - t1  # temps que cela a pris

d=a$d
u=a$u
v=a$v

D <- diag(a$d)

#lescomposantes = D %*% D %*% t(a$v) #Prendre la matrice des composante multipliée par la diagonale de la matrice des valeurs singulières
lescomposantes = v
lesmots = as.matrix(colnames(z.matrice))
lescomposantes_mots <- as.matrix(data.frame(lescomposantes, row.names=lesmots[,1])) #Pour mettre les mots en noms de lignes
les30composantes_mots=lescomposantes_mots[,1:20]

#Enlever les mots ayant des loading trop bas

loading_min = -1000
bas_loadings=NULL


for (i in 1:nrow(les30composantes_mots)) {
  if ( max(les30composantes_mots[i,])<loading_min ) {bas_loadings=c(bas_loadings,i)}
}


les30composantes_mots=les30composantes_mots[-bas_loadings,]

t1 <- Sys.time()
composantes.varimax=varimax(les30composantes_mots) #Rotation varimax pour équilibrer les variances des 50 meilleures composantes (il faudrait aussi trouver une façon d'enlever au préalable les mots n'ayant pas au moins un loading >= un certain seuil pour les 50 premières composantes
varimax.loadings=composantes.varimax$loadings
t2 <- Sys.time()
t2 - t1  # temps que cela a pris

#FONCTION: TROUVER LES 5 mots MAXIMUM POUR CHAQUE TOPIC##
topics_5mots = NA
topics_5mots = as.data.frame(topics_5mots)
topics_5mots[1:ncol(varimax.loadings),] = c(1:ncol(varimax.loadings))
topics_5mots[,2:6]=NA
loading_min = -1000 #Valeur minimum que les loadings doivent avoir pour être affichés dans le talbeau
n=nrow(varimax.loadings)



for (i in 1:ncol(varimax.loadings)) {
  for (j in 2:6)
  {
    if ( sort( varimax.loadings[,i],partial=n-(j-2) )[n-(j-2)] >= loading_min ) {  #pour max: n-0, pour second max: n-1, et ainsi de suite
      if (length(names(which(varimax.loadings[,i]==sort(varimax.loadings[,i],partial=n-(j-2))[n-(j-2)]))) > 1){
        nb=length(names(which(varimax.loadings[,i]==sort(varimax.loadings[,i],partial=n-(j-2))[n-(j-2)])))
        topics_5mots[i,2:(nb+1)] = names(which(varimax.loadings[,i]==sort(varimax.loadings[,i],partial=n-(j-2))[n-(j-2)]))
        break

      }
      topics_5mots[i,j]=names(which(varimax.loadings[,i]==sort(varimax.loadings[,i],partial=n-(j-2))[n-(j-2)]))
    }
  }
}


topics_5ngrams = NA
topics_5ngrams = as.data.frame(topics_5ngrams)
topics_5ngrams[1:ncol(varimax.loadings),] = c(1:ncol(varimax.loadings))
topics_5ngrams[,2:6]=NA

loading_min = -0.01 #Valeur minimum que les loadings doivent avoir pour être affichés dans le talbeau
n=nrow(varimax.loadings)


for (i in 1:ncol(varimax.loadings))

{
  if ( sort( varimax.loadings[,i],partial=n-(6-2) )[n-(6-2)] >= loading_min ) {  #pour max: n-0, pour second max: n-1, et ainsi de suite
    topics_5ngrams[i,2:6]=names(sort(varimax.loadings[which(varimax.loadings[,i] >= sort(varimax.loadings[,i],partial=n-(6-2))[n-(6-2)]),i],decreasing =T)[1:5])
  }

}


#Calcul (très simple) des poids des topics pour chaque document pour ensuite calculer le poids par nationnalité (groupes de documents)
#lesngrams=row.names(varimax.loadings)
#lestopics=names(varimax.loadings[1,])

x.topics = matrix(nrow=nrow(varimax.loadings),ncol=ncol(varimax.loadings))

for(i in 1:ncol(varimax.loadings)){
  x.topics[,i] = rowSums(x.matrice[,as.character(topics_5ngrams[i,2:length(topics_5ngrams[i,])])])
}
