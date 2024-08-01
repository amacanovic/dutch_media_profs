fit_plm <- plm(news_all ~ 
                 news_all_l + 
                 # past period news
                 # citations
                 cited_by_total_all_l + 
                 # attention variables
                 alt_online_all_total_l + alt_twitter_total_l +
                 # coauthor publication variables
                 coa_tot_cited_by_total_l +
                 # coauthor attention variables
                 coa_online_all_total_l + coa_twitter_total_l, 
               data = prof_panel_filter, 
               index = c("profile_id", "year"), 
               model = "within", 
               effect = "twoways")

# Note how this is functionally identical to the lm() way 
coeftest(fit_plm, vcov = vcovHC, type = "HC1")

fit_plm <- plm(alt_online_all ~ 
                 alt_online_all_l + 
                 # past period news
                 # citations
                 cited_by_total_all_l + 
                 # attention variables
                 news_all_total_l + alt_twitter_total_l +
                 # coauthor publication variables
                 coa_tot_cited_by_total_l +
                 # coauthor attention variables
                 coa_online_all_total_l + coa_twitter_total_l, 
               data = prof_panel_filter, 
               index = c("profile_id", "year"), 
               model = "within", 
               effect = "twoways")

# Note how this is functionally identical to the lm() way 
coeftest(fit_plm, vcov = vcovHC, type = "HC1")



fit_plm <- plm(alt_twitter ~ 
                 alt_twitter_l + 
                 # past period news
                 # citations
                 cited_by_total_all_l + 
                 # attention variables
                 news_all_total_l + alt_online_all_total +
                 # coauthor publication variables
                 coa_tot_cited_by_total_l +
                 # coauthor attention variables
                 coa_online_all_total_l + coa_twitter_total_l, 
               data = prof_panel_filter, 
               index = c("profile_id", "year"), 
               model = "within", 
               effect = "twoways")

# Note how this is functionally identical to the lm() way 
coeftest(fit_plm, vcov = vcovHC, type = "HC1")


## RE

fit_plm <- plm(news_all ~ 
                 news_all_l + 
                 as.factor(inferred_gender)+
                 # past period news
                 # citations
                 cited_by_total_all_l + 
                 # attention variables
                 alt_online_all_total_l + alt_twitter_total_l +
                 # coauthor publication variables
                 coa_tot_cited_by_total_l +
                 # coauthor attention variables
                 coa_online_all_total_l + coa_twitter_total_l, 
               data = prof_panel_filter, 
               index = c("profile_id", "year"), 
               model = "random")

# Note how this is functionally identical to the lm() way 
coeftest(fit_plm, vcov = vcovHC, type = "HC1")

fit_plm <- plm(alt_online_all ~ 
                 alt_online_all_l + 
                 as.factor(inferred_gender)+
                 # past period news
                 # citations
                 cited_by_total_all_l + 
                 # attention variables
                 news_all_total_l + alt_twitter_total_l +
                 # coauthor publication variables
                 coa_tot_cited_by_total_l +
                 # coauthor attention variables
                 coa_online_all_total_l + coa_twitter_total_l, 
               data = prof_panel_filter, 
               index = c("profile_id", "year"), 
               model = "random")

# Note how this is functionally identical to the lm() way 
coeftest(fit_plm, vcov = vcovHC, type = "HC1")



fit_plm <- plm(alt_twitter ~ 
                 alt_twitter_l + 
                 as.factor(inferred_gender)+
                 # past period news
                 # citations
                 cited_by_total_all_l + 
                 # attention variables
                 news_all_total_l + alt_online_all_total +
                 # coauthor publication variables
                 coa_tot_cited_by_total_l +
                 # coauthor attention variables
                 coa_online_all_total_l + coa_twitter_total_l, 
               data = prof_panel_filter, 
               index = c("profile_id", "year"), 
               model = "random")

# Note how this is functionally identical to the lm() way 
coeftest(fit_plm, vcov = vcovHC, type = "HC1")