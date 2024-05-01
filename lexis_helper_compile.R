library(groundhog)
packages_to_load <- c("readr", "dplyr", "stringr", "tidyr", "tidyverse",
                      "RPostgres", "lubridate", "readtext",
                      "DBI", "RODBC", "odbc")
groundhog.library(packages_to_load, date = "2023-12-01")

library("LexisNexisTools")

folder <- "../lexis_crawl/downloads/zipfiles"
all_docs1 <- lnt_read(x = folder, convert_date = F, extract_paragraphs = F)
saveRDS(all_docs1, "news_data/all_docs1.RDS")

folder2 <- "../lexis_crawl/downloads/zipfiles2"
all_docs2 <- lnt_read(x = folder2, convert_date = F, extract_paragraphs = F)

folder3 <- "../lexis_crawl/downloads/zipfiles3"
all_docs3 <- lnt_read(x = folder3, convert_date = F, extract_paragraphs = FALSE)

folder4 <- "../lexis_crawl/downloads/zipfiles4"
all_docs4 <- lnt_read(x = folder4, convert_date = F, extract_paragraphs = F)

folder5 <- "../lexis_crawl/downloads/zipfiles5/"
all_docs5 <- lnt_read(x = folder5, convert_date = F, extract_paragraphs = FALSE)

saveRDS(all_docs1, "news_data/all_docs1.RDS")
saveRDS(all_docs2, "news_data/all_docs2.RDS")
saveRDS(all_docs3, "news_data/all_docs3.RDS")
saveRDS(all_docs4, "news_data/all_docs4.RDS")
saveRDS(all_docs5, "news_data/all_docs5.RDS")

for (i in 1:5){
  get_all <- get(paste0("all_docs", i))
  meta_data <- get_all@meta
  meta_data$profile_id <- str_split_i(meta_data$Source_File, "/zips/", 2)
  meta_data$profile_id <- str_split_i(meta_data$profile_id, "_", 1)
  
  meta_data$date_eng <- tolower(meta_data$Date)
  # replace dutch dates with english for easier conversion later on
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "januari",
                                     "january")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "februari",
                                     "february")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "maart",
                                     "march")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "mei",
                                     "may")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "mai",
                                     "may")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "juni",
                                     "june")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "juli",
                                     "july")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "augustus",
                                     "august")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "oktober",
                                     "october")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                     "dezember",
                                     "december")
  
  # convert the dates
  meta_data$date_tidy <- lubridate::parse_date_time(meta_data$date_eng,  c("dmy", "md,y"))
  
  # get the year
  meta_data$year <- year(meta_data$date_tidy)
  
  # select columns of interest
  meta_data_sel <- meta_data %>%
    select(ID, Newspaper, Length:Author, Headline, profile_id, date_tidy, year)
  
  colnames(meta_data_sel) <- tolower(colnames(meta_data_sel))
  
  article_content <- get_all@articles
  colnames(article_content) <- tolower(colnames(article_content))
  
  news_mentions <- merge(meta_data_sel,
                          article_content,
                          by = "id")
  
  # profile ID to match the rest
  news_mentions$profile_id <- paste0("https://www.narcis.nl/person/RecordID/", news_mentions$profile_id)
  
  assign(paste0("news_mentions",i), news_mentions)
  
}

rm(all_docs1)
rm(all_docs2)
rm(all_docs3)
rm(all_docs4)
rm(all_docs5)
gc()
# bind the first three together

news_mentions_1_3 <- rbind(news_mentions1,
                           news_mentions2,
                           news_mentions3)

# get the latest dates from this per profile_id
latest_dates <- news_mentions_1_3 %>%
  filter(!is.na(date_tidy))%>%
  group_by(profile_id)%>%
  summarise(date_tidy = min(date_tidy))

colnames(latest_dates)[2] <- "cutoff_date"

news_mentions4_merge <- merge(news_mentions4,
                              latest_dates,
                              by = "profile_id")

news_mentions4_merge <- filter(news_mentions4_merge,
                               date_tidy < cutoff_date)

news_mentions_1_4 <- rbind(news_mentions_1_3,
                           news_mentions4_merge[, -11])


# list files in zip5
zip5_files <- list.files(path = "../lexis_crawl/downloads/zipfiles5/",
                                pattern="*.zip", 
                                full.names = F)

zip5_files <- paste0("https://www.narcis.nl/person/RecordID/", str_remove(zip5_files, ".zip"))

news_mentions_1_4_filt <- filter(news_mentions_1_4,
                                 ! profile_id %in% zip5_files)
  
news_mentions_1_5 <- rbind(news_mentions_1_4_filt,
                           news_mentions5)

news_mentions_final <- news_mentions_1_5[, -1]

news_mentions_final$profile_id <- ifelse(news_mentions_final$profile_id == "https://www.narcis.nl/person/RecordID/PRS1239403/PRS1239403", "https://www.narcis.nl/person/RecordID/PRS1239403", news_mentions_final$profile_id)
news_mentions_final$profile_id <- ifelse(news_mentions_final$profile_id == "https://www.narcis.nl/person/RecordID/PRS1239771/PRS1239771", "https://www.narcis.nl/person/RecordID/PRS1239771", news_mentions_final$profile_id)
news_mentions_final$profile_id <- ifelse(news_mentions_final$profile_id == "https://www.narcis.nl/person/RecordID/PRS1241869/PRS1241869", "https://www.narcis.nl/person/RecordID/PRS1241869", news_mentions_final$profile_id)
news_mentions_final$profile_id <- ifelse(news_mentions_final$profile_id == "https://www.narcis.nl/person/RecordID/PRS1243082/PRS1243082", "https://www.narcis.nl/person/RecordID/PRS1243082", news_mentions_final$profile_id)

saveRDS(news_mentions_final,"news_data/combined_zipfiles_1_5_not_fully_deduped.RDS")

########################### additional

all_docs6 <- readRDS("news_data/all_docs6.RDS")
all_docs7 <- readRDS("news_data/all_docs7.RDS")
all_docs8 <- readRDS("news_data/all_docs8.RDS")
all_docs9 <- readRDS("news_data/all_docs9.RDS")
all_docs10 <- readRDS("news_data/all_docs10.RDS")
all_docs11 <- readRDS("news_data/all_docs11.RDS")
all_docs12 <- readRDS("news_data/all_docs12.RDS")
all_docs13 <- readRDS("news_data/all_docs13.RDS")

for (i in 6:13){
  get_all <- get(paste0("all_docs", i))
  meta_data <- get_all@meta
  meta_data$profile_id <- str_split_i(meta_data$Source_File, "/zips/", 2)
  meta_data$profile_id <- str_split_i(meta_data$profile_id, "_", 1)
  
  meta_data$date_eng <- tolower(meta_data$Date)
  # replace dutch dates with english for easier conversion later on
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "januari",
                                    "january")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "februari",
                                    "february")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "maart",
                                    "march")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "mei",
                                    "may")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "mai",
                                    "may")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "juni",
                                    "june")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "juli",
                                    "july")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "augustus",
                                    "august")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "oktober",
                                    "october")
  meta_data$date_eng <- str_replace(meta_data$date_eng,
                                    "dezember",
                                    "december")
  
  # convert the dates
  meta_data$date_tidy <- lubridate::parse_date_time(meta_data$date_eng,  c("dmy", "md,y"))
  
  # get the year
  meta_data$year <- year(meta_data$date_tidy)
  
  # select columns of interest
  meta_data_sel <- meta_data %>%
    select(ID, Newspaper, Length:Author, Headline, profile_id, date_tidy, year)
  
  colnames(meta_data_sel) <- tolower(colnames(meta_data_sel))
  
  article_content <- get_all@articles
  colnames(article_content) <- tolower(colnames(article_content))
  
  news_mentions <- merge(meta_data_sel,
                         article_content,
                         by = "id")
  
  # profile ID to match the rest
  news_mentions$profile_id <- paste0("https://www.narcis.nl/person/RecordID/", news_mentions$profile_id)
  
  assign(paste0("news_mentions",i), news_mentions)
  
}

rm(all_docs6)
rm(all_docs7)
rm(all_docs8)
rm(all_docs9)
rm(all_docs10)
rm(all_docs11)
rm(all_docs12)
rm(all_docs13)
gc()

news_mentions_6_13 <- rbind(news_mentions6,
                            news_mentions7,
                            news_mentions8, 
                            news_mentions9,
                            news_mentions10,
                            news_mentions11, 
                            news_mentions12,
                            news_mentions13)

saveRDS(news_mentions_6_13, "news_data/combined_zipfiles_6_13_not_fully_deduped.RDS")

news_mentions_final_1_5 <- readRDS("news_data/combined_zipfiles_1_5_not_fully_deduped.RDS")
news_mentions_6_13 <- readRDS("news_data/combined_zipfiles_6_13_not_fully_deduped.RDS")


news_mentions_final_1_5_excluding_6_13 <- filter(news_mentions_final_1_5,
                                                 !profile_id %in% news_mentions_6_13$profile_id)

news_mentions_final_1_5_including_6_13 <- rbind(news_mentions_final_1_5_excluding_6_13,
                                                news_mentions_6_13[,-1])

exclude_profs <- c('PRS1260654',
                   'PRS1264232',
                   'PRS1290223',
                   'PRS1290912',
                   'PRS1291282',
                   'PRS1298775',
                   'PRS1299517',
                   'PRS1303190',
                   'PRS1308364',
                   'PRS1313821',
                   'PRS1314292',
                   'PRS1315919',
                   'PRS1316094',
                   'PRS1321926',
                   'PRS1324504',
                   'PRS1325131',
                   'PRS1329040',
                   'PRS1330089',
                   'PRS1331627',
                   'PRS1331980',
                   'PRS1332877',
                   'PRS1334007',
                   'PRS1338934',
                   'PRS1341238',
                   'PRS1349009',
                   'PRS1350774',
                   'PRS1260039',
                   'PRS1265665',
                   'PRS1276211',
                   'PRS1336203',
                   'PRS1329967',
                   'PRS1334028')

news_mentions_final <- filter(news_mentions_final_1_5_including_6_13,
                              ! profile_id %in% paste0("https://www.narcis.nl/person/RecordID/", exclude_profs))


saveRDS(news_mentions_final, "news_data/combined_zipfiles_1_13_not_fully_deduped.RDS")
#################

news_mentions_final$dupl <- duplicated(news_mentions_final)

news_mentions_final_dedupe <- filter(news_mentions_final,
                                     dupl == FALSE)

news_mentions_final_dedupe <- news_mentions_final_dedupe %>% select(-dupl)

saveRDS(news_mentions_final_dedupe, "news_data/combined_zipfiles_1_13_deduped.RDS")

#### CHECK COVERAGE

articles_output <- news_mentions_final %>%
  group_by(profile_id)%>%
  summarise(retrieved = n())

articles_output$profile_id <- str_remove(articles_output$profile_id, "https://www.narcis.nl/person/RecordID/")

result_count_list_merge <- read_csv("../lexis_crawl/result_list_full.csv")

result_count_list_merge <- result_count_list_merge[, -c(3:4)]

result_count_list_merge <- merge(result_count_list_merge,
                                 articles_output,
                                 by = "profile_id",
                                 all.x = TRUE)

result_count_list_merge$retrieved <- ifelse(is.na(result_count_list_merge$retrieved),
                                      0,
                                      result_count_list_merge$retrieved)

result_count_list_merge$share_retrieved <- ifelse(result_count_list_merge$hits_identified == 0,
                                            100, 
                                            result_count_list_merge$retrieved / result_count_list_merge$hits_identified * 100)



result_count_list_merge <- filter(result_count_list_merge, ! profile_id %in% exclude_profs)


write_csv(result_count_list_merge, "../lexis_crawl/result_list_full_final.csv")
