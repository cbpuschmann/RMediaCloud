# LIBRARIES

#if(!require("jsonlite")) {install.packages("jsonlite"); library("jsonlite")}
#if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
#if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")}
require("jsonlite")
require("tidyverse")
require("quanteda")

# QUERY STRINGS

# Media
q.media <- "https://api.mediacloud.org/api/v2/media/"
q.media.single <- paste0(q.media, "single/")
q.media.list <- paste0(q.media, "list")
# Stories
q.stories_public <- "https://api.mediacloud.org/api/v2/stories_public/"
q.stories_public.single <- paste0(q.stories_public, "single/")
q.stories_public.list <- paste0(q.stories_public, "list")
q.stories_public.count <- paste0(q.stories_public, "count")
q.stories_public.tag_count <- paste0(q.stories_public, "tag_count")
q.stories_public.word_matrix <- paste0(q.stories_public, "word_matrix")
# Word Counting
q.wc <- "https://api.mediacloud.org/api/v2/wc/"
q.wc.list <- paste0(q.wc, "list")
# Stats
q.stats <- "https://api.mediacloud.org/api/v2/stats/"
q.stats.list <- paste0(q.stats, "list")


# FUNCTIONS

mc_media <- function(id = 1, key)
{
  api.url <- paste0(q.media.single, id, "?key=", key)
  result <- fromJSON(api.url) %>% select(-media_source_tags)  # omit tags to simplify results data frame
  return(result)
}

mc_media_tags <- function(id = 1, key)  # use this function to retrieve tag sets for sources by id
{
  api.url <- paste0(q.media.single, id, "?key=", key)
  result <- fromJSON(api.url)$media_source_tags
  result <- result[][[1]]
  return(result)
}

mc_media_list <- function(q = "", name = "", rows = 20, key)
{
  api.url <- paste0(q.media.list, "?q=", URLencode(q), "&name=", URLencode(name), "&rows=", rows, "&key=", key)
  result <- fromJSON(api.url) %>% select(-media_source_tags)  # omit tags to simplify results data frame
  return(result)
}

mc_stories <- function(id = 1, key)
{
  api.url <- paste0(q.stories_public.single, id, "?key=", key)
  result <- fromJSON(api.url) %>%
    select(-story_tags) %>%  # omit tags to simplify results data frame
    mutate(collect_date = as.POSIXct(collect_date), publish_date = as.POSIXct(publish_date))
  return(result)
}

mc_stories_list <- function(q = "", media = "", collection = "", date_from = "", date_to = "", feeds_id = NULL, rows = 20, last_processed_stories_id = 0, key)
{
  if (media != "") q <- paste0(q, " AND media_id:", media)
  if (collection != "") fq <- paste0("&fq=tags_id_media:", collection) else fq <- ""
  if (date_from != "" | date_to != "") fq <- paste0(fq, "&fq=publish_date:%5B", date_from, "T00:00:00.000Z+TO+", date_to, "T00:00:00.000Z%5D")
  if (collection == "" & date_from == "" & date_to == "") fq <- ""
  api.url <- paste0(q.stories_public.list, "?q=", URLencode(q), fq, "&feeds_id=", feeds_id, "&rows=", rows, "&last_processed_stories_id=", last_processed_stories_id, "&key=", key)
  result <- fromJSON(api.url)
  if (!is.list(result)) result <- select(result, -story_tags)  # omit tags to simplify results data frame
  return(result)
}

mc_stories_count <- function(q = "", media = "", collection = "", date_from = "", date_to = "", split = FALSE, split_period = NULL, key) # split_period = day, week, month, year
{
  if (media != "") q <- paste0(q, " AND media_id:", media)
  if (collection != "") fq <- paste0("&fq=tags_id_media:", collection) else fq <- ""
  if (date_from != "" | date_to != "") fq <- paste0(fq, "&fq=publish_date:%5B", date_from, "T00:00:00.000Z+TO+", date_to, "T00:00:00.000Z%5D")
  if (collection == "" & date_from == "" & date_to == "") fq <- ""
  if (split) split <- 1 else split <- 0
  api.url <- paste0(q.stories_public.count, "?q=", URLencode(q), "&fq=", fq, "&split=", split, "&split_period=", split_period, "&key=", key)
  result <- fromJSON(api.url)$counts
  #%>% mutate(date = as.POSIXct(date))
  return(result)
}

mc_stories_tag_count <- function(q = "", media = "", limit = 100, key)
{
  if (media != "") fq <- paste0(q, "&fq=media_id:", media) else fq <- ""
  api.url <- paste0(q.stories_public.tag_count, "?q=", URLencode(q), "&fq=", fq, "&limit=", limit, "&key=", key)
  result <- fromJSON(api.url)
  return(result)
}

mc_stories_wm <- function(q = "", media = "", collection = "", date_from = "", date_to = "", rows = 100, max_words = NULL, quanteda_dfm = FALSE, key)
{
  if (media != "") q <- paste0(q, " AND media_id:", media)
  if (collection != "") fq <- paste0("&fq=tags_id_media:", collection) else fq <- ""
  if (date_from != "" | date_to != "") fq <- paste0(fq, "&fq=publish_date:%5B", date_from, "T00:00:00.000Z+TO+", date_to, "T00:00:00.000Z%5D")
  if (collection == "" & date_from == "" & date_to == "") fq <- ""
  api.url <- paste0(q.stories_public.word_matrix, "?q=", URLencode(q), "&fq=", fq, "&rows=", rows, "&max_words=", max_words, "&key=", key)
  print("Retrieving results...")
  result <- fromJSON(api.url)
  words <- result$word_list[,2]  # use base forms, not lemmas
  documents <- names(result$word_matrix)
  dtm <- matrix(0, nrow = length(documents), ncol = length(words), dimnames = list(documents, words))
  print("Fitting document term matrix...")
  for (i in seq_along(documents))
  {
    counts <- unlist(result$word_matrix[[documents[i]]])
    dtm[documents[i], as.numeric(names(counts))+1] <- unname(counts)
  }
  if (quanteda_dfm) dtm <- as.dfm(dtm)
  return(dtm)
}

mc_wordcounts <- function(q = "", media = "", collection = "", date_from = "", date_to = "", num_words = 50, sample_size = 100, remove_stopwords = TRUE, key)
{
  if (media != "") q <- paste0(q, " AND media_id:", media)
  if (collection != "") fq <- paste0("&fq=tags_id_media:", collection) else fq <- ""
  if (date_from != "" | date_to != "") fq <- paste0(fq, "&fq=publish_date:%5B", date_from, "T00:00:00.000Z+TO+", date_to, "T00:00:00.000Z%5D")
  if (collection == "" & date_from == "" & date_to == "") fq <- ""
  if (remove_stopwords) include_stopwords <- 0 else include_stopwords <- 1
  api.url <- paste0(q.wc.list, "?q=", URLencode(q), "&fq=", fq, "&num_words=", num_words, "&sample_size=", sample_size, "&include_stopwords=", include_stopwords, "&include_stats=", include_stats, "&key=", key)
  result <- fromJSON(api.url)
  return(result)
}

mc_stats <- function(key)
{
  api.url <- paste0(q.stats.list, "?key=", key)
  result <- fromJSON(api.url) %>% unlist()
  result <- data.frame(MediaCloud.Stats = result)
  return(result)
}

