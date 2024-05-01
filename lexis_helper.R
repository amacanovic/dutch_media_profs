library(groundhog)
packages_to_load <- c("readr", "dplyr", "stringr", "tidyr", "tidyverse",
                      "RPostgres", "lubridate", "readtext",
                      "DBI", "RODBC", "odbc")
groundhog.library(packages_to_load, date = "2023-12-01")

library("LexisNexisTools")


folder <- "../lexis_crawl/downloads/zipfiles4"
all_docs <- lnt_read(x = folder, convert_date = F, extract_paragraphs = F)
meta_data <- all_docs@meta
meta_data$profile_id <- str_split_i(meta_data$Source_File, "/zips/", 2)
meta_data$profile_id <- str_split_i(meta_data$profile_id, "_", 1)

articles_output <- meta_data %>%
  group_by(profile_id)%>%
  summarise(retrieved = n())

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

article_content <- all_docs@articles
colnames(article_content) <- tolower(colnames(article_content))

news_mentions <- merge(meta_data_sel,
                       article_content,
                       by = "id")

# profile ID to match the rest
news_mentions$profile_id <- paste0("https://www.narcis.nl/person/RecordID/", news_mentions$profile_id)

national_news_nl <- c(
  "De Telegraaf",
  "De Telegraaf.nl",
  "Algemeen Dagblad",
  "AD/Algemeen Dagblad.nl",
  "AD/Algemeen Dagblad",
  "Metro",
  "Metro (NL)",
  "De Volkskrant",
  "De Volkskrant.nl",
  "NRC Handelsblad",
  "Trouw",
  "Trouw.nl",
  "Het Financieele Dagblad",
  "Het Financieele Dagblad (Abstract)/MD Info",
  "FD.nl",
  "Reformatorisch Dagblad",
  "Nederlands Dagblad",
  "NRC Next",
  "NRC",
  "NRC.NEXT",
  "NRC.nl",
  "Het Financieele Dagblad (Abstract)/MD Info"
)

regional_news_nl <- c(
  "De Limburger",
  "Gelderlander",
  "De Twentsche Courant Tubantia",
  "De Gelderlander",
  "De Gelderlander.nl",
  "Stentor",
  "Noordhollands Dagblad",
  "Dagblad van het Noorden",
  "Brabants Dagblad",
  "BN/DeStem",
  "BN De Stem.nl",
  "Tubantia",
  
  "Tubantia.nl",
  "Eindhovens Dagblad",
  "BN De Stem",
  "Leeuwarder Courant",
  "Het Parool",
  "Het Parool.nl",
  "Provinciale Zeeuwse Courant",
  "Haarlems Dagblad",
  "Leidsch Dagblad",
  "De Gooi- en Eemlander",
  "Nieuwsblad Noordoost-Friesland",
  "Steenwijker Courant",
  "Friesch Dagblad",
  "AD/Haagsche Courant",
  "AD/Rotterdams Dagblad",
  "AD/Utrechts Nieuwsblad",
  "Noordhollands Dagblad",
  "AD/Groene Hart",
  "AD/De Dordtenaar",
  "AD/Amersfoortse Courant",
  "AD/Rivierenland",
  "Limburgs Dagblad",
  "Brabants Dagblad.nl",
  "Eindhovens Dagblad.nl",
  "De Stentor.nl",
  "De Stentor",
  "De Stentor/Zwolse Courant",
  "De Stentor/Gelders Dagblad",
  "De Stentor/Zutphens Dagblad",
  "De Stentor/Nieuw Kamper Dagblad",
  "De Stentor/Dagblad Flevoland",
  "De Stentor/Veluws Dagblad",
  "De Stentor/Deventer Dagblad",
  "Dagblad Tubantia/Twentsche Courant",
  "de limburger.nl",
  "De Stentor/Apeldoornse Courant",
  "DAGBLAD VAN HET NOORDEN",
  "De Stentor/Sallands Dagblad",
  "De Stentor/Gelders Dagblad",
  "De Stentor/Zutphens Dagblad",
  "De Stentor/Zwolse Courant",
  "De Groene Amsterdammer",
  "De Stentor/Apeldoornse Courant",
  "De Stentor/Dagblad Flevoland",
  "Dagblad van het Noorden.nl",
  "Noordhollands Dagblad.nl",
  "De Stentor/Deventer Dagblad",
  "Rotterdams Dagblad",
  "Friesch Dagblad",
  "Haarlems Dagblad.nl",
  "Meppeler Courant",
  "IJmuider Courant.nl",
  "Dagblad voor Zuidwest-Nederland",
  "Zwolse Courant",
  "Ijmuider Courant",
  "Leidsch Dagblad.nl",
  "Utrechts Nieuwsblad",
  "Haagsche Courant",
  "Leeuwarder Courant.nl",
  "De Stentor/Nieuw Kamper Dagblad",
  "Amersfoortse Courant",
  "Goudsche Courant",
  "Friesch Dagblad.nl",
  "Dagblad Rivierenland",
  "PZC.nl",
  "Woerdense Courant",
  "Goudse post",
  "Tubantia.nl",
  "nu.nl",
  "Voorburgse Courant",
  "De Stad Wageningen",
  "De Stem",
  "Knack Magazine",
  "IJmuider Courant",
  "De Brug Nijmegen",
  "de Volkskrant (Abstract) / MD Info")

professional_pub_nl <- c(
  "De Accountant",
  "Cobouw",
  "Boerderij",
  "Logistiek",
  "Psychologie Magazine",
  "Boerderij Vandaag",
  "Nieuwsblad Transport",
  "Vrij Nederland",
  "Food & Agri business",
  "Pakblad",
  "Nieuwsblad Transport",
  "restaurantonline.co.uk",
  "Groenten en Fruit",
  "Pluimveehouderij",
  "Logistiek Krant")

high_profile_int <- c(
  "The Times Higher Education Supplement",
  "Financial Times (London, England)",
  "The New York Times", 
  "Business Wire",
  "The Times (London)",
  "MailOnline",
  "Guardian.com",
  "The Guardian - Final Edition",
  "Guardian Weekly",
  "Asian News International (ANI)",
  "Associated Press International",
  "Associated Press Online",
  "FT.com",
  "The Toronto Star",
  "thetimes.co.uk",
  "Xinhua General News Service",
  "FT.com Headlines",
  "The Guardian (London)",
  "The International Herald Tribune",
  "The Guardian",
  "CNN Wire",
  "telegraph.co.uk",
  "The Daily Telegraph (London)",
  "The Independent (United Kingdom)",
  "The Independent (London)",
  "The Guardian(London)",
  "Independent.co.uk",
  "CNN.com",
  "The Sunday Times (London)",
  "NEW YORK TIMES")

# government and press releases
pr_govt <- c("Targeted News Service", 
             "US Fed News",
             "US Official News",
             "PR Newswire", 
             "M2 PressWIRE", 
             "Premium Official News",
             "European Union News", 
             "TendersInfo", 
             "GlobeNewswire",
             "PR Newswire Europe",
             "PR.com",
             "ENP Newswire",
             "FinancialWire")

# university newspapers
uni_news <- c("University Wire")

# news bites
news_bites <- c("News Bites - Private Companies",
                "News Bites - People in Business",
                "News Bites - Benelux: Netherlands",
                "News Bites - Nordic: Denmark",
                "OzEquities News Bites (Australia)",
                "Australian Company News Bites - Stock Report",
                "News Bites - Western Europe: Spain",
                "News Bites - Western Europe : Germany",
                "News Bites - Western Europe",
                "News Bites - Western Europe: Switzerland",
                "News Bites - Nordic: Sweden")

# irrelevant sources
irrelevant <-	"FD (Fair Disclosure) Wire"

news_mentions$newspaper_tidy <- str_replace(news_mentions$newspaper, " \\/ ", "\\/")

# national Dutch sources
news_mentions$national_nl <- ifelse(tolower(news_mentions$newspaper_tidy) %in% tolower(national_news_nl), 1, 0)
# local Dutch sources
news_mentions$regional_nl <- ifelse(tolower(news_mentions$newspaper_tidy) %in% tolower(regional_news_nl), 1, 0)
# higher-profile interantional sources
news_mentions$high_prof_intl <- ifelse(tolower(news_mentions$newspaper_tidy) %in% tolower(high_profile_int), 1, 0)
# newsletters by "news bites"
news_mentions$newsletter <- ifelse(tolower(news_mentions$newspaper_tidy) %in% tolower(news_bites), 1, 0)
# PR documents
news_mentions$pr_news <- ifelse(tolower(news_mentions$newspaper_tidy) %in% tolower(pr_govt), 1, 0)
# unviersity newspapers
news_mentions$uni_news <- ifelse(tolower(news_mentions$newspaper_tidy) %in% tolower(uni_news), 1, 0)
# blogs
news_mentions$blog <- ifelse(str_detect(tolower(news_mentions$newspaper_tidy), "blog"), 1, 0)
news_mentions$blog <- ifelse(is.na(news_mentions$blog), 0, news_mentions$blog)
# irrelevant
news_mentions$irrelevant <- ifelse(tolower(news_mentions$newspaper_tidy) %in% tolower(irrelevant), 1, 0)

# no source
news_mentions$source_unknown <- ifelse(news_mentions$newspaper_tidy == "", 1, 0)
news_mentions$source_unknown <- ifelse(is.na(news_mentions$newspaper_tidy), 1, news_mentions$source_unknown)

# and the remainder
news_mentions$source_unclassified <- ifelse(news_mentions$national_nl == 0 & news_mentions$regional_nl == 0 &
                                              news_mentions$high_prof_intl == 0 & news_mentions$newsletter == 0 &
                                              news_mentions$pr_news == 0 & news_mentions$uni_news == 0 &
                                              news_mentions$blog == 0 &
                                              news_mentions$source_unknown == 0, 1, 0)

# get a categorical variable
news_mentions$source_type <- ifelse(news_mentions$national_nl == 1,
                                    "national_nl",
                                    ifelse(news_mentions$regional_nl == 1,
                                           "regional_nl",
                                           ifelse(news_mentions$high_prof_intl == 1,
                                                  "high_prof_intl",
                                                  ifelse(news_mentions$newsletter == 1,
                                                         "newsletter",
                                                         ifelse(news_mentions$pr_news == 1,
                                                                "pr_news", 
                                                                ifelse(news_mentions$uni_news == 1,
                                                                       "uni_news",
                                                                       ifelse(news_mentions$blog == 1,
                                                                              "blog",
                                                                              ifelse(news_mentions$irrelevant == 1,
                                                                                     "irrelevant",
                                                                                     ifelse(news_mentions$source_unknown == 1,
                                                                                            "unknown",
                                                                                            "other")))))))))

news_mentions_2 <- news_mentions

news_mentions_2$profile_id <- str_remove(news_mentions_2$profile_id, "https://www.narcis.nl/person/RecordID/")

news_mentions_2$source <- "new"

# default port here, change if needed

port <- 5432
user <- "postgres"
password <- "dutchmediaprofssql"
database_name <- "postgres"


con <- RPostgres::dbConnect(RPostgres::Postgres(),
                            dbname= database_name,
                            port = port,
                            user = user, 
                            password = password)

news_mentions_old <- dbReadTable(con, "lexis_nexis_mentions")

news_mentions_old$source <- "old"

news_mentions <- rbind(news_mentions_old, news_mentions_2)

news_mentions$dupl <- duplicated(news_mentions[,2:22])
table(news_mentions$dupl)

news_mentions$remove <- paste(news_mentions$dupl, news_mentions$source, sep = "_")

news_mentions <- news_mentions %>%
  filter(remove != "TRUE_new")%>%
  select(-dupl, -source, - remove)

articles_output <- news_mentions %>%
  group_by(profile_id)%>%
  summarise(retrieved = n())

result_count_list_merge <- read_csv("../lexis_crawl/result_list_full.csv")

result_count_list_merge <- result_count_list_merge %>%
  select(-retrieved, -share_retrieved)


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
