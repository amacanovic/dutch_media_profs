
# last ID we have
"https://openalex.org/W3127941377"

remaining <- filter(relevant_oa_pubs, !id %in% twitter_mentions$id)
# last tw id is this one
which(remaining$id == "https://openalex.org/W3127941377")

remaining2 <- remaining[which(remaining$id == "https://openalex.org/W2982102540"):nrow(remaining),]
# pending for twitter IDs
dbWriteTable(con, "remaining_oa_pubs_for_twitter_id", remaining2)

# done with 549
# batch size
batch_size <- 1000
# vector of indices to loop through
batches <- seq(from=1, to=nrow(remaining2), by=batch_size)
# to be able to subset, also add the final index+1
batches <- c(batches, length(remaining2)+1)

# per batch
for (i in 550:length(batches)){
  # get the list
  batch_pubs <- remaining2[batches[i]:(batches[i+1]-1),]
  
  tweet_info_full <- NA
  
  # try getting the tweet info
  try(tweet_info_full <- altmetric_twitter_retriever(publication_list = batch_pubs,
                                                     api_key = altmetric_api_key),
      silent = TRUE)
  
  # and write to the database
  # if not empty
  if (!all(is.na(tweet_info_full))){
    # if there is no table yet, write it as a new table
    if (! dbExistsTable(con, "altmetric_pub_att_twitter")){
      # write to the database
      dbWriteTable(con, "altmetric_pub_att_twitter", tweet_info_full, row.names=FALSE, append=TRUE) 
      # otherwise, append
    }else{
      # check fields in the existing table
      fields <- dbListFields(con, "altmetric_pub_att_twitter")
      # if needed, pad the dataset
      if(!all(fields %in% colnames(tweet_info_full))){
        n_missing <- which(!fields %in% colnames(tweet_info_full))
        padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
        colnames(padding) <- fields[which(!fields %in% colnames(tweet_info_full))]
        tweet_info_full <- bind_cols(tweet_info_full,
                                     padding)
        tweet_info_full <- tweet_info_full[fields]
      }
      # only leave these fields in
      tweet_info_full <-  tweet_info_full %>%
        select(all_of(fields))
      dbAppendTable(con, "altmetric_pub_att_twitter", tweet_info_full, row.names=NULL, append=TRUE) 
    }
  }
  print(paste("done with", i, "out of", length(batches), sep = " "))
}