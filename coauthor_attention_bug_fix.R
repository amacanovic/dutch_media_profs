# coauthor_attention_list <-  read_csv("coauthor_attention_list_partial.csv")
# coauthor_altmetric_list <- read_csv("coauthor_altmetric_list.csv")
coauthor_attention_list <- data.frame(matrix(NA, nrow = 0, ncol = 4))
colnames(coauthor_attention_list) <- c("mention_type", "year", "yearly_count", "id")


how_many <- 20000

for (i in 30000:50000){
  # not sure this is correct!
  orcid <- coauthor_altmetric_list$au_orcid[i]
  coauthor_attention <- NA
  # query the info
  try(coauthor_attention <- altmetric_api_orcid_caller(orcid = orcid,
                                                       api_secret = api_secret,
                                                       api_key = api_key,
                                                       endpoint = "attention"))
  
  # if any data, unnest twice to unravel the info
  if (!all(is.na(coauthor_attention))){
    #deduplicate
    coauthor_attention <- coauthor_attention %>% distinct(id, .keep_all =  TRUE)
    coauthor_attention <- unnest(coauthor_attention, cols = c("meta"))
    coauthor_attention <- unnest(coauthor_attention, cols = c("dates"))
    # now, extract the year and group mentions by year
    coauthor_attention$year <- year(ymd(coauthor_attention$date))
    coauthor_attention_year <- coauthor_attention %>%
      group_by(id, year)%>%
      summarise(yearly_count = sum(count))
    # tidy up the column names
    colnames(coauthor_attention_year)[1] <- c("mention_type")
    # add in the coauthor OA ID
    coauthor_attention_year$id <- coauthor_altmetric_list$au_id[i]
    
    if (nrow(coauthor_attention_year)>0){
      # filter out the duplicates
      coauthor_attention_year <- filter(coauthor_attention_year, 
                                        ! id %in% coauthor_attention_list$id)
      
      coauthor_attention_list <- rbind(coauthor_attention_list,
                                       coauthor_attention_year)
    }
  }
  print(paste("done with", i, "out of", how_many))
}

write_csv(coauthor_attention_list, "temp_altm/coauthor_attention_list_30_50k.csv")