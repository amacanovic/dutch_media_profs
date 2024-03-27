prof_year_p_c_g_a_t_l_l <- read_csv("prof_year_p_c_g_a_t_l_l_18_3.csv")
oa_coauthor_matching <- dbReadTable(con, "oa_coauthor_matching")
oa_coauthor_matching <- oa_coauthor_matching[c("id", "au_id", "profile_id", "publication_year")]
profile_ids <- unique(prof_year_p_c_g_a_t_l_l$profile_id)
coauthor_year_p_c_a_g <- read_csv("coauthor_year_p_c_a_g_11_3.csv")
oa_coauthor_info_name <- dbReadTable(con, "oa_coauthor_name_list")

oa_coauthor_info <- dbReadTable(con, "oa_coauthor_info")
coauthor_gender <- dbReadTable(con, "coauthor_name_gender")
prof_gender_names <- dbReadTable(con, "gender_table")%>%
  distinct(first, .keep_all = TRUE)%>%
  select(first, inferred_gender)

colnames(coauthor_gender)[2] <- "first"
coauthor_gender <- coauthor_gender %>%
  filter(first != "")%>%
  select(first, gender)

# recode the variable
coauthor_gender$gender <- ifelse(coauthor_gender$gender == "male", "m",
                                 ifelse(coauthor_gender$gender == "female", "w", coauthor_gender$gender))

colnames(prof_gender_names)[2] <- "gender"

# combine
all_names <- rbind(coauthor_gender,
                   prof_gender_names)%>%
  distinct(first, .keep_all = TRUE)


# filter out "and" which seems to just be a mistake
oa_coauthor_info_name <- oa_coauthor_info_name %>%
  filter(! tolower(first) %in% c("van", "and", "den"))

oa_coauthor_info_name$first <- tolower(oa_coauthor_info_name$first)

coauthor_name_gender <- merge(oa_coauthor_info_name,
                              all_names,
                              by = "first",
                              all.x = TRUE,
                              all.y = FALSE)

coauthor_name_gender <- coauthor_name_gender %>%
  select(id, gender)%>%
  filter(!is.na(gender))

colnames(coauthor_name_gender) <- c("au_id", "inferred_gender")
oa_coauthor_info_gender <- merge(oa_coauthor_info,
                                 coauthor_name_gender,
                                 all.x = TRUE,
                                 by = "au_id")

oa_coauthor_info_gender$inferred_gender <- ifelse(is.na(oa_coauthor_info_gender$inferred_gender),
                                                  "unknown",
                                                  oa_coauthor_info_gender$inferred_gender)


profile_id = profile_ids[i]
prof_panel = prof_year_p_c_g_a_t_l_l
prof_coauthor_matching = oa_coauthor_matching
prof_coauthor_panel = coauthor_year_p_c_a_g
prof_coauthor_info_w_gender = oa_coauthor_info_gender

###############################################################################################

current_profile_id <- profile_id
# get info on this prof
prof_panel <- filter(prof_panel,
                     profile_id == current_profile_id)

# get into on this prof's coauthors only for the years in which they coauthored
prof_coauthors <- filter(prof_coauthor_matching, 
                         profile_id == current_profile_id & publication_year >= 1974)
# now, extract the statistics on these coauthors from out coauthor panel
prof_coauthor_data <- filter(prof_coauthor_panel,
                             id %in% prof_coauthors$au_id)

# and select the relevant coauthors to check gender composition per paper
prof_coauthors_oa_gender <- filter(prof_coauthor_info_w_gender,
                                   profile_id == current_profile_id)

# get coauthor_d + year variable to filter on
prof_coauthors$au_year <- paste0(prof_coauthors$au_id, "_", prof_coauthors$publication_year)
prof_coauthor_data$au_year <- paste0(prof_coauthor_data$id, "_", prof_coauthor_data$year)

prof_coauthor_data_rel <- filter(prof_coauthor_data,
                                 au_year %in% prof_coauthors$au_year)

# get yearly professor coauthor data breakdown
prof_coauthor_data_year <- prof_coauthor_data_rel %>%
  group_by(year)%>%
  summarise_at(vars(count_pubs:coa_attn_twitter_by_total), sum, na.rm = TRUE)

prof_coauthor_data_rel$gender <- ifelse(is.na(prof_coauthor_data_rel$gender), 
                                        "unknown", 
                                        prof_coauthor_data_rel$gender)
# gender breakdown
prof_coauthor_gender_breakdown <- prof_coauthor_data_rel %>%
  group_by(gender, year)%>%
  summarise(n = n())%>%
  pivot_wider(names_from = gender, values_from = n)%>%
  replace(is.na(.), 0)

if (!"unknown" %in% colnames(prof_coauthor_gender_breakdown)){
  prof_coauthor_gender_breakdown$unknown <- 0
}

if (!"m" %in% colnames(prof_coauthor_gender_breakdown)){
  prof_coauthor_gender_breakdown$m <- 0
}

if (!"w" %in% colnames(prof_coauthor_gender_breakdown)){
  prof_coauthor_gender_breakdown$w <- 0
}

prof_coauthor_gender_breakdown <- prof_coauthor_gender_breakdown[c("year", "m", "unknown", "w")]

colnames(prof_coauthor_gender_breakdown)[which(colnames(prof_coauthor_gender_breakdown) == "m")] <- "coa_m"
colnames(prof_coauthor_gender_breakdown)[which(colnames(prof_coauthor_gender_breakdown) == "w")] <- "coa_w"
colnames(prof_coauthor_gender_breakdown)[which(colnames(prof_coauthor_gender_breakdown) == "unknown")] <- "coa_u"

# combined coauthor yearly summary
prof_coauthor_data_year <- merge(prof_coauthor_data_year,
                                 prof_coauthor_gender_breakdown,
                                 by = "year")

# tidy up the colnames
colnames(prof_coauthor_data_year)[2:9] <- paste0("coa_", colnames(prof_coauthor_data_year)[2:9])

# and get a lagged equivalent
prof_coauthor_data_year_lag <- prof_coauthor_data_year %>%
  arrange(year)%>%
  mutate_at(vars(contains('coa_')), lag)

colnames(prof_coauthor_data_year_lag)[-1] <- paste0(colnames(prof_coauthor_data_year)[-1], "_l")

# merge the two together and
# get cumulative info on coauthors for the professor in question 
prof_coauthor_data_all <- merge(prof_coauthor_data_year,
                                prof_coauthor_data_year_lag,
                                by = "year")

# if we want cumulative, we first need to remove duplicate coauthor counts, keeping 
# only the most recent
years <- unique(prof_coauthor_data_rel$year)
colnames_cumulative <- c("prof_tot_count_pubs",
                         "prof_tot_cited_by",
                         "prof_tot_count_pubs_total_oa",
                         "prof_tot_cited_by_total_oa",
                         "prof_tot_cited_by_before_2012",
                         "prof_tot_count_pubs_before_2012",
                         "prof_tot_cited_by_total_all",
                         "prof_tot_count_pubs_total_all",     
                         "prof_tot_coa_attn_twitter_by",
                         "prof_tot_coa_attn_news_by",
                         "prof_tot_coa_attn_blog_by",
                         "prof_tot_coa_attn_news_by_total",
                         "prof_tot_coa_attn_blog_by_total",
                         "prof_tot_coa_attn_twitter_by_total",
                         "prof_tot_unique_coa_m",
                         "prof_tot_unique_coa_u",
                         "prof_tot_unique_coa_w")

colnames_cumulative_all <- c("year", "prof_tot_count_pubs", "prof_tot_cited_by", 
                             "prof_tot_count_pubs_total_oa", "prof_tot_cited_by_total_oa", 
                             "prof_tot_cited_by_before_2012", "prof_tot_count_pubs_before_2012",
                             "prof_tot_cited_by_total_all",  "prof_tot_count_pubs_total_all", 
                             "prof_tot_coa_attn_twitter_by",  "prof_tot_coa_attn_news_by", 
                             "prof_tot_coa_attn_blog_by", "prof_tot_coa_attn_news_by_total", 
                             "prof_tot_coa_attn_blog_by_total", "prof_tot_coa_attn_twitter_by_total",
                             "prof_tot_unique_coa_m",
                             "prof_tot_unique_coa_u",
                             "prof_tot_unique_coa_w",
                             "prof_tot_count_pubs_l", "prof_tot_cited_by_l", "prof_tot_count_pubs_total_oa_l",
                             "prof_tot_cited_by_total_oa_l",  "prof_tot_cited_by_before_2012_l", 
                             "prof_tot_count_pubs_before_2012_l", "prof_tot_cited_by_total_all_l", 
                             "prof_tot_count_pubs_total_all_l", "prof_tot_coa_attn_twitter_by_l", 
                             "prof_tot_coa_attn_news_by_l",   "prof_tot_coa_attn_blog_by_l",   
                             "prof_tot_coa_attn_news_by_total_l", "prof_tot_coa_attn_blog_by_total_l",
                             "prof_tot_coa_attn_twitter_by_total_l",
                             "prof_tot_unique_coa_m_l",
                             "prof_tot_unique_coa_u_l",
                             "prof_tot_unique_coa_w_l",
                             "share_w_coa_all",
                             "share_w_coa_known",
                             "share_w_coa_all_l",
                             "share_w_coa_known_l")

prof_coauthor_cumulative_data_all <- as.data.frame(matrix(NA, ncol = length(colnames_cumulative_all), nrow = 0))
colnames(prof_coauthor_cumulative_data_all) <- colnames_cumulative_all

# loop  through the years to accumulate this
# additionally, get the average gender composition
# of author's papers in that year
for (year_in_question in years){
  prof_coauthor_data_year <- filter(prof_coauthor_data_rel,
                                    year <= year_in_question)
  
  if (nrow(prof_coauthor_data_year) > 0){
    
    prof_coauthor_data_year <- prof_coauthor_data_year %>%
      group_by(id)%>%
      slice(which.max(year))%>%
      ungroup()
    
    cumulative_year <- prof_coauthor_data_year %>%
      select(-year)%>%
      summarise(across(where(is.numeric), \(x) sum(x, na.rm = T), .names = "prof_tot_{.col}"))
    
    cumulative_year_gender <- prof_coauthor_data_year %>%
      group_by(gender)%>%
      summarise(n = n())%>%
      pivot_wider(names_from = gender, values_from = n)%>%
      replace(is.na(.), 0)
    
    if(! "unknown" %in% colnames(cumulative_year_gender)){
      cumulative_year_gender$unknown <- 0
    }
    if(! "m" %in% colnames(cumulative_year_gender)){
      cumulative_year_gender$m <- 0
    }
    if(! "w" %in% colnames(cumulative_year_gender)){
      cumulative_year_gender$w <- 0
    }
    cumulative_year_gender <- cumulative_year_gender[c("m", "unknown", "w")]
    colnames(cumulative_year_gender) <- c("m", "u", "w")
    
    colnames(cumulative_year_gender) <- paste0("prof_tot_unique_coa_", colnames(cumulative_year_gender))
    
    cumulative_year$year <- year_in_question
    
    cumulative_year <- cbind(cumulative_year,
                             cumulative_year_gender)
    
    cumulative_year <- cumulative_year %>% relocate(year)
    
    prof_coauthor_data_year_lag <- filter(prof_coauthor_data_rel,
                                          year <= year_in_question - 1)
  } else {
    cumulative_year <- as.data.frame(matrix(0, ncol = length(colnames_cumulative), nrow = 1))
    colnames(cumulative_year) <- colnames_cumulative
    cumulative_year$year <- year_in_question
  }
  
  prof_coauthor_data_year_lag <- filter(prof_coauthor_data_rel,
                                        year <= year_in_question - 1)
  
  if (nrow(prof_coauthor_data_year_lag) > 0){
    prof_coauthor_data_year_lag <- prof_coauthor_data_year_lag %>%
      group_by(id)%>%
      slice(which.max(year))%>%
      ungroup()
    
    cumulative_year_lag <- prof_coauthor_data_year_lag %>%
      select(-year)%>%
      summarise(across(where(is.numeric), \(x) sum(x, na.rm = T), .names = "prof_tot_{.col}_l"))
    
    cumulative_year_gender_lag <- prof_coauthor_data_year_lag %>%
      group_by(gender)%>%
      summarise(n = n())%>%
      pivot_wider(names_from = gender, values_from = n)%>%
      replace(is.na(.), 0)
    
    if(! "unknown" %in% colnames(cumulative_year_gender_lag)){
      cumulative_year_gender_lag$unknown <- 0
    }
    if(! "m" %in% colnames(cumulative_year_gender_lag)){
      cumulative_year_gender_lag$m <- 0
    }
    if(! "w" %in% colnames(cumulative_year_gender_lag)){
      cumulative_year_gender_lag$w <- 0
    }
    cumulative_year_gender_lag <- cumulative_year_gender_lag[c("m", "unknown", "w")]
    colnames(cumulative_year_gender_lag) <- paste0(c("m", "u", "w"), "_l")
    colnames(cumulative_year_gender_lag) <- paste0("prof_tot_unique_coa_", colnames(cumulative_year_gender_lag))
    
    cumulative_year_lag$year <- year_in_question
    
    cumulative_year_lag <- cbind(cumulative_year_lag,
                                 cumulative_year_gender_lag)
    
  }else{
    cumulative_year_lag <- as.data.frame(matrix(NA, ncol = length(colnames_cumulative), nrow = 1))
    colnames(cumulative_year_lag) <- paste0(colnames_cumulative, "_l")
    cumulative_year_lag$year <- year_in_question
  }
  
  # merge cumulatives and lagged cumulatives
  cumulative_all <- merge(cumulative_year,
                          cumulative_year_lag,
                          by = "year")
  
  # add in gender composition
  
  ## gender composition
  gender_composition_paper <- prof_coauthors_oa_gender %>%
    filter(publication_year == year_in_question)%>%
    group_by(profile_id, id)%>%
    summarise(n_w = sum(inferred_gender == "w"),
              n_m = sum(inferred_gender == "m"),
              n_u = sum(inferred_gender == "unknown"))%>%
    mutate(share_w_all = n_w / (n_w + n_m + n_u),
           share_w_known = n_w / (n_w + n_m))%>%
    select(profile_id, id, share_w_all, share_w_known)%>%
    group_by(profile_id)%>%
    reframe(share_w_coa_all = round(mean(share_w_all, na.rm = TRUE), 3),
            share_w_coa_known = round(mean(share_w_known, na.rm = TRUE), 3))
  
  if (nrow(gender_composition_paper) == 0){
    gender_composition_paper <- cbind.data.frame(profile_id = current_profile_id,
                                                 share_w_coa_all = NA,
                                                 share_w_coa_known = NA)
  }
  
  ## gender composition lagged
  gender_composition_paper_lag <- prof_coauthors_oa_gender %>%
    filter(publication_year == year_in_question - 1)%>%
    group_by(profile_id, id)%>%
    summarise(n_w = sum(inferred_gender == "w"),
              n_m = sum(inferred_gender == "m"),
              n_u = sum(inferred_gender == "unknown"))%>%
    mutate(share_w_all = n_w / (n_w + n_m + n_u),
           share_w_known = n_w / (n_w + n_m))%>%
    select(profile_id, id, share_w_all, share_w_known)%>%
    group_by(profile_id)%>%
    summarise(share_w_coa_all = round(mean(share_w_all, na.rm = TRUE), 3),
              share_w_coa_known = round(mean(share_w_known, na.rm = TRUE), 3))
  
  if (nrow(gender_composition_paper_lag) == 0){
    gender_composition_paper_lag <- cbind.data.frame(profile_id = current_profile_id,
                                                     share_w_coa_all = NA,
                                                     share_w_coa_known = NA)
  }
  
  colnames(gender_composition_paper_lag) <- paste0(colnames(gender_composition_paper_lag), "_l")
  
  ## gender composition combined
  gender_composition_prof_year <- cbind(gender_composition_paper,
                                        gender_composition_paper_lag[, -1])
  
  cumulative_all <- cbind(cumulative_all,
                          gender_composition_prof_year[, -1])
  
  
  cumulative_all <- cumulative_all %>%
    relocate(year)
  
  prof_coauthor_cumulative_data_all <- rbind.data.frame(prof_coauthor_cumulative_data_all,
                                                        cumulative_all)
  
  
}

# merge prof info with coauthor data
prof_panel_merge <- merge(prof_panel,
                          prof_coauthor_data_all,
                          by = "year",
                          all.x = TRUE)

prof_panel_merge <- merge(prof_panel_merge,
                          prof_coauthor_cumulative_data_all,
                          by = "year",
                          all.x = TRUE)

