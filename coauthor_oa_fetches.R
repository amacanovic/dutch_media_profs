coauthor_oa_fetches <- function(oa_id_list){
  prof_coauthor_info_oa <- NA
  
  prof_coauthor_info_oa <- oa_fetch(
    entity = "authors", 
    openalex_id = oa_id_list)
  
  if (!all(is.na(prof_coauthor_info_oa))){
    
    # unnest the data 
    prof_coauthor_info_oa_unnest <- unnest(prof_coauthor_info_oa, cols = c(counts_by_year), names_sep = "_")%>%
      select(-x_concepts)
    
    # get their names
    # first, get all the name alternatives as well
    coauthor_name_variations <- prof_coauthor_info_oa %>%
      select(id, display_name, display_name_alternatives)%>%
      unnest(., cols = c(display_name_alternatives))
    # wide to long, with all variations
    coauthor_name_variations <- gather(coauthor_name_variations,
                                       type, 
                                       full_name, 
                                       display_name:display_name_alternatives, 
                                       factor_key=FALSE)
    
    # now, identify which ones are actualy names, and not just initials
    # get the first word, and detect if longer than 1 character and/or does not contain any full stops
    coauthor_name_variations$first <- word(coauthor_name_variations$full_name, 1)
    coauthor_name_variations$valid_name <- ifelse(str_detect(coauthor_name_variations$first, "\\."),
                                                  "FALSE",
                                                  "TRUE")
    # get the name variation lengths
    coauthor_name_variations$length_first <-  nchar(coauthor_name_variations$first) 
    coauthor_names <- data.frame(matrix(NA, nrow = 0, ncol = 3))
    # leave only the ones where we seem to have a shortest name longer than 2 characters
    coauthor_names <- filter(coauthor_name_variations, valid_name == TRUE)%>%
      group_by(id)%>%
      slice(which.max(length_first))%>%
      filter(., length_first > 2)%>%
      select(-valid_name, -length_first, -type)
    
    # select prof info
    coauthor_info <- data.frame(matrix(NA, nrow = 0, ncol = 14))
    columns <- c("id", "display_name", "orcid", "works_count",
                 "cited_by_count", "counts_by_year_year", "counts_by_year_works_count", 
                 "counts_by_year_cited_by_count", "affiliation_display_name", 
                 "affiliation_id", "affiliation_ror", "affiliation_country_code",
                 "affiliation_type", "works_api_url")
    # padding in case some columns are missing
    if(!all(columns %in% colnames(prof_coauthor_info_oa_unnest))){
      n_missing <- which(!columns %in% colnames(prof_coauthor_info_oa_unnest))
      padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
      colnames(padding) <- columns[which(!columns %in% colnames(prof_coauthor_info_oa_unnest))]
      prof_coauthor_info_oa_unnest <- bind_cols(prof_coauthor_info_oa_unnest,
                                                padding)
      prof_coauthor_info_oa_unnest <- prof_coauthor_info_oa_unnest[columns]
    }
    
    coauthor_info <- prof_coauthor_info_oa_unnest%>%
      select(id, display_name, orcid, works_count,
             cited_by_count, counts_by_year_year, counts_by_year_works_count, 
             counts_by_year_cited_by_count, affiliation_display_name, 
             affiliation_id, affiliation_ror, affiliation_country_code,
             affiliation_type, works_api_url)
    
    output <- list("names" = coauthor_names,
                   "info" = coauthor_info)
    
    return(output)
  }
}
