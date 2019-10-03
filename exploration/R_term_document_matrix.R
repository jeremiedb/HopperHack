#install.packages("tm")
library(tm)
myCorpus <- read.csv("//nassmb/usagers_02$/labrecjo/HACKATHON/HOPPER_HACKATHON_2019/515k-hotel-reviews-data-in-europe/Hotel_Reviews.csv")
myCorpus$Reviewer_Nationality=as.character(myCorpus$Reviewer_Nationality)


#corpus <- Corpus(VectorSource(myCorpus$Negative_Review))

#test=TermDocumentMatrix(corpus)

#test2=as.matrix(test)

#unique(myCorpus$Reviewer_Nationality)

russie=myCorpus[as.character(myCorpus$Reviewer_Nationality)==' Russia ' & as.character(myCorpus$Negative_Review) != 'No Negative' & as.character(myCorpus$Negative_Review) != ' ',]



russie_corpus_Neg =  Corpus(VectorSource(russie$Negative_Review))

####################
# UNI-GRAM de MOTS #
####################
#russie_TD_Neg = TermDocumentMatrix(russie_corpus_Neg)

#russie_TD_Neg=as.matrix(russie_TD_Neg)
#russie_TD_Neg_t=t(russie_TD_Neg)

#################################
# BI-GRAMS et TRI-GRAMS de MOTS #
#################################
#install.packages('tidytext')
library('tidytext')
library(magrittr)
library(tm)
library(tidyr)
library(tidytext)
library(dplyr)
library(janeaustenr)
library(quanteda)

reviews_neg <- data.frame(txt = as.character(russie$Negative_Review),
                          doc = as.character(row.names(russie)),
                      stringsAsFactors = FALSE)

#Pour enlever les stop words avant de faire les bigram
#reviews_neg2 = reviews_neg %>% 
         #     unnest_tokens(output = word, input = txt) %>% 
            #  anti_join(stop_words) %>% 
           #   count(word, sort = TRUE)
#stop_words2$word
stop_words2=stop_words[!stop_words$word %in% 
                         c('not','no','very','only','more','up','so','few','most','too',
                           'some','off',"isn't","aren't","wasn't","weren't","hasn't",
                           "haven't","hadn't","doesn't","don't","didn't","won't",
                           "wouldn't","shan't","shouldn't","can't","cannot","couldn't",
                           "mustn't","look","out","welcome","better","best","full",
                           "generally","great","high","highest","higher","mostly","rooms","room",
                           "several","showed","shows","small","smaller","smallest","problem",
                           "never","needs","needing","nobody","never","needed","need"),]

reviews_neg_ngram  = mutate(reviews_neg, text = gsub(x = txt, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
 
                      unnest_tokens(ngram, text, token = "ngrams", n = 3, n_min = 3) %>% 
                      separate(ngram, c("word1", "word2"
                                        , "word3"
                                        ), sep = " ") %>% 
                      filter(!word1 %in% stop_words2$word) %>%
                      filter(!word2 %in% stop_words2$word) %>% 
                      filter(!word3 %in% stop_words2$word) %>% 
                      unite(ngram,word1, word2,
                            word3, 
                            sep = " ") 

#stop_words[stop_words$lexicon=='snowball',]$word
#unique(stop_words$lexicon)
#reviews_neg_ngram <- reviews_neg2 %>% 
 # head(1000) %>%    # otherwise this will get very large
  #anti_join(stop_words) %>% 
 # unnest_tokens(ngram, txt, token = "ngrams", n = 3, n_min = 2) 

ngram_counts <- reviews_neg_ngram %>%
  count(doc, ngram,sort = TRUE)

reviews_neg_ngram_dtm <- ngram_counts %>%
  cast_dtm(doc, ngram, n)

#install.packages('topicmodels')
library(topicmodels)
reviews_neg_ngram_lda <- LDA(reviews_neg_ngram_dtm , k = 50, control = list(seed = 1234))


top_terms <- tidy(reviews_neg_ngram_lda) %>%
  group_by(topic) %>%
  top_n(1, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


########################################
# section avec PCA et rotation varimax
########################################

russie_TD_Neg=as.matrix(reviews_neg_ngram_dtm)
russie_TD_Neg_t=t(russie_TD_Neg)


#install.packages('lsa')
library(lsa)

#russie_TD_Neg_t
#x.matrice =russie_TD_Neg_t[,colSums(russie_TD_Neg_t)!=0]

x.matrice =russie_TD_Neg[,colSums(russie_TD_Neg)!=0] #On ne veut pas de colonnes avec seulement des zéros

#x.matrice=newtable[,colSums(newtable)!=0] #On ne veut pas de colonnes avec seulement des zéros


#Centrer et réduire la matrice des fréquences
Lesmoyennes=as.matrix(t(apply(x.matrice,2,mean)))

Lesecart_types=sqrt(as.matrix(t(apply(x.matrice,2,var))))

z.matrice=x.matrice

for (j in 1:ncol(x.matrice)) {
  z.matrice[,j]= (x.matrice[,j] - Lesmoyennes[1,j]) / Lesecart_types[1,j]
}


t1 <- Sys.time()
a=svd(z.matrice, nv=50)
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

les50composantes_mots=lescomposantes_mots[,1:50]


#Enlever les mots ayant des loading trop bas
loading_min = 0.01

bas_loadings=NULL

for (i in 1:nrow(les50composantes_mots)) { 
  if ( max(les50composantes_mots[i,])<loading_min ) {bas_loadings=c(bas_loadings,i)}
  
}

les50composantes_mots=les50composantes_mots[-bas_loadings,]

t1 <- Sys.time()
composantes.varimax=varimax(les50composantes_mots) #Rotation varimax pour équilibrer les variances des 50 meilleures composantes (il faudrait aussi trouver une façon d'enlever au préalable les mots n'ayant pas au moins un loading >= un certain seuil pour les 50 premières composantes
varimax.loadings=composantes.varimax$loadings
t2 <- Sys.time()
t2 - t1  # temps que cela a pris


#FONCTION: TROUVER LES 5 mots MAXIMUM POUR CHAQUE TOPIC##

topics_5mots = NA
topics_5mots = as.data.frame(topics_5mots)
topics_5mots[1:ncol(varimax.loadings),] = c(1:ncol(varimax.loadings))
topics_5mots[,2:50]=NA


loading_min = -0.01 #Valeur minimum que les loadings doivent avoir pour être affichés dans le talbeau

n=nrow(varimax.loadings)

for (i in 1:ncol(varimax.loadings)) 
{ 

  for (j in 2:11) 
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


names(which(varimax.loadings[,2]==sort(varimax.loadings[,2],partial=n-(2-2))[n-(2-2)]))
names(which(varimax.loadings[,2]==sort(varimax.loadings[,2],partial=n-(3-2))[n-(3-2)]))



######
# Exemple dans mon travail chez Provalis
######

library(lsa)

x.matrice =russie_TD_Neg_t[,colSums(russie_TD_Neg_t)!=0] #On ne veut pas de colonnes avec seulement des zéros

x.matrice=cor(x.matrice)

#Centrer et réduire la matrice des fréquences
Lesmoyennes=as.matrix(t(apply(x.matrice,2,mean)))

Lesecart_types=sqrt(as.matrix(t(apply(x.matrice,2,var))))

z.matrice=x.matrice

for (j in 1:ncol(x.matrice)) {
  z.matrice[,j]= (x.matrice[,j] - Lesmoyennes[1,j]) / Lesecart_types[1,j]
}


t1 <- Sys.time()
#a=svd(z.matrice)
a=svd(x.matrice)
t2 <- Sys.time()
t2 - t1  # temps que cela a pris

d=a$d
u=a$u
v=a$v

D <- diag(a$d)

#vari<-nrow(x.matrice)-1 #Calculate the divisor for the variance
#vectors<-a$v #Specify eigenvectors for svd
#sdev<-a$d/sqrt(vari) #Calculate standard deviation of new variables
#values<-sdev*sdev #Sqaure the standard deviation to find the eigenvalues
#scores<-scale(x.matrice)%*%vectors #Calculate the new components


#lescomposantes = t(D %*% D %*% a$v) #Prendre la matrice des composante multipliée par la diagonale de la matrice des valeurs singulières
#lescomposantes = t(D[,1:50] %*% t(D[,1:50]) %*% t(a$v)) #Prendre la matrice des composante multipliée par la diagonale de la matrice des valeurs singulières



lescomposantes = D %*% D %*% t(v)
#lescomposantes=v
lesmots = as.matrix(colnames(x.matrice))
#lescomposantes_mots <- as.matrix(data.frame(lescomposantes, row.names=lesmots[,1])) #Pour mettre les mots en noms de lignes
lescomposantes_mots <- as.matrix(data.frame(t(lescomposantes), row.names=lesmots[,1])) 


les50composantes_mots=lescomposantes_mots[,1:50]


#Enlever les mots ayant des loading trop bas
#loading_min = 0.001

#bas_loadings=NULL

#for (i in 1:nrow(les50composantes_mots)) { 
#if ( max(les50composantes_mots[i,])<loading_min ) {bas_loadings=c(bas_loadings,i)}

#}

#les50composantes_mots=les50composantes_mots[-bas_loadings,]

t1 <- Sys.time()
composantes.varimax=varimax(les50composantes_mots) #Rotation varimax (scale pour normaliser) pour équilibrer les variances des 50 meilleures composantes (il faudrait aussi trouver une façon d'enlever au préalable les mots n'ayant pas au moins un loading >= un certain seuil pour les 50 premières composantes
varimax.loadings=composantes.varimax$loadings
#varimax.loadings=scale(les50composantes_mots)

t2 <- Sys.time()
t2 - t1  # temps que cela a pris

#test=D[,1:50] %*% t(D[,1:50]) %*% varimax.loadings

#FONCTION: TROUVER LES 5 mots MAXIMUM POUR CHAQUE TOPIC##

topics_5mots = NA
topics_5mots = as.data.frame(topics_5mots)
topics_5mots[1:ncol(varimax.loadings),] = c(1:ncol(varimax.loadings))
topics_5mots[,2:50]=NA


loading_min = -1000 #Valeur minimum que les loadings doivent avoir pour être affichés dans le talbeau

n=nrow(varimax.loadings)

for (i in 1:ncol(varimax.loadings)) 
{ 
  j=2
  for (j in 2:11) 
  {
    if ( sort( varimax.loadings[,i],partial=n-(j-2) )[n-(j-2)] >= loading_min ) { 
      
      if (length(names(which(varimax.loadings[,i]==sort(varimax.loadings[,i],partial=n-(j-2))[n-(j-2)]))) > 1){
        
        nb=length(names(which(varimax.loadings[,i]==sort(varimax.loadings[,i],partial=n-(j-2))[n-(j-2)])))
        topics_5mots[i,2:nb+1] = names(which(varimax.loadings[,i]==sort(varimax.loadings[,i],partial=n-(j-2))[n-(j-2)]))
        break
      }
      
      topics_5mots[i,j]=names(which(varimax.loadings[,i]==sort(varimax.loadings[,i],partial=n-(j-2))[n-(j-2)])) 
      
      } #pour max: n-0, pour second max: n-1, et ainsi de suite
  }	                    
}

sort( varimax.loadings[,i],partial=n-(2-2) )[n-(2-2)]

names(which(varimax.loadings[,i]==sort(varimax.loadings[,i],partial=n-(j-2))[n-(j-2)]))
j

