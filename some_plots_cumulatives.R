mean_values_year_field <- prof_panel_coa %>%
  filter(!is.na(general_field) & years_since_first_pub > 0 & year >= 2012) %>%
  group_by(inferred_gender, years_since_first_pub, general_field)%>%
  summarise(news_total = mean(log(news_all_total+1), na.rm = TRUE),
            news_sd = sd(log(news_all_total+1), na.rm = TRUE),
            online_total = mean(log(alt_online_all_total+1), na.rm = TRUE),
            online_sd = sd(log(alt_online_all_total+1+1), na.rm = TRUE),
            twitter_total =  mean(log(alt_twitter_total+1), na.rm = TRUE),
            twitter_sd = sd(log(alt_twitter_total+1+1), na.rm = TRUE),
            n = n())%>%
  mutate(news_se = news_sd / sqrt(n),
         news_lower_ci = news_total - qt(1 - (0.05 / 2), n - 1) * news_se,
         news_upper_ci = news_total + qt(1 - (0.05 / 2), n - 1) * news_se,
         online_se = online_sd / sqrt(n),
         online_lower_ci = online_total - qt(1 - (0.05 / 2), n - 1) * online_se,
         online_upper_ci = online_total + qt(1 - (0.05 / 2), n - 1) * online_se,
         twitter_se = twitter_sd / sqrt(n),
         twitter_lower_ci = twitter_total - qt(1 - (0.05 / 2), n - 1) * twitter_se,
         twitter_upper_ci = twitter_total + qt(1 - (0.05 / 2), n - 1) * twitter_se)%>%
  filter(!is.na(news_sd))%>%
  filter(!is.nan(news_total))

pt1 <- mean_values_year_field %>% select(inferred_gender:news_total, news_upper_ci, news_lower_ci)
pt2 <- mean_values_year_field %>% select(inferred_gender:general_field, online_total, online_upper_ci, online_lower_ci)
pt3 <- mean_values_year_field %>% select(inferred_gender:general_field, twitter_total, twitter_upper_ci, twitter_lower_ci)
colnames(pt1)[4:6] <- c("mean", "upper_ci", "lower_ci")
colnames(pt2)[4:6] <- c("mean", "upper_ci", "lower_ci")
colnames(pt3)[4:6] <- c("mean", "upper_ci", "lower_ci")
pt1$variable <- "News"
pt2$variable <- "Online news"
pt3$variable <- "Twitter"
mean_values_year_field <- rbind(pt1, pt2, pt3)


mean_values_year_field %>%
  ggplot(aes(x=years_since_first_pub, y=mean, color = inferred_gender, group = 1)) +
  #geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci), width = 0.1)+
  facet_grid(variable~general_field, scales = "free_y")+
  theme_bw()+
  theme(plot.title = element_text(size = 14),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11) ,
        axis.title.x = element_text(size = 12),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12))