rm(list=ls())
library(data.table)
library(stringr)
library(text2vec)

dt <- fread("../data/Hotel_Reviews.csv", data.table = T)
table(dt$Reviewer_Nationality)

prep_fun = function(x) {
  x %>%
    # make text lower case
    str_to_lower %>%
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alpha:]]", " ") %>%
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}

dt[, review := paste(Positive_Review, Negative_Review)]
dt$review = prep_fun(dt$Negative_Review)

it = itoken(dt$review, progressbar = T)
v = create_vocabulary(it) %>%
  prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer)

tfidf = TfIdf$new()
lsa = LSA$new(n_topics = 10)

# pipe friendly transformation
doc_embeddings = dtm %>%
  fit_transform(tfidf) %>%
  fit_transform(lsa)
