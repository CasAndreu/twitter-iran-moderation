#===============================================================================
# App03-figureB1-B2.R 
# Purpose: To replicate Figures B1 and B2 in Appendix B, where I show
#          the distribution of count/continuous variables to identify skewed 
#          ones to log transform in our regression analyses.
# Article: The Geopolitics of Deplatforming: A Study of Suspensions of 
#          Politically-Interested Iranian Accounts on Twitter.
# Authors: Andreu Casas
# Journal: Political Communication
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)

# DATA
#===============================================================================
main <- read.csv("./data/model-data-anon.csv")

# DATA WRANGLING
#===============================================================================
# - make sure COUNT/CONTINUOUS variables are numeric
num_vars <- c("mean_embedsim", "max_embedsim",
              "ideo", "tweets2020", "political_n",
              "uncivil_n", "iran_infavor_avg", 
              "misinfo_n", "days_on_the_platform", "avg_daily_statuses", 
              "follower_friend_ratio", "platform_entropy", "prop_directed", 
              "prop_hashtag", "prop_retweets", "fa")

for (numvar in num_vars) {
  main[,numvar] <- as.numeric(main[,numvar])
}

# - create a long version of this dataset for plotting
main_long <- main[, c("user_id_anon", num_vars)] %>%
  gather(variable, value, -user_id_anon) %>%
  filter(!is.na(value))

# - human-readable labels for the variables
main_long <- main_long %>%
  mutate(v = dplyr::recode(factor(variable),
                           `mean_embedsim` = "Coordination (mean)",
                           `max_embedsim` = "Coordination (max.)",
                           `ideo` = "Principlist (Conservative)",
                           `tweets2020` = "Number of tweets (2020)",
                           `political_n` = "Number of political tweets (2020)",
                           `uncivil_n` = "Number of hateful tweets (2020)",
                           `iran_infavor_avg` = "In favor of Iranian government (mean)",
                           `misinfo_n` = "Number of (covid) misinfo tweets (2020)",
                           `days_on_the_platform` = "Number of days in the platform",
                           `avg_daily_statuses` = "Average number of daily tweets",
                           `follower_friend_ratio` = "Follower/Friend ratio",
                           `platform_entropy` = "Platform entropy",
                           `prop_directed` = "Prop. of 2020 tweets at somebody",
                           `prop_hashtag` = "Prop. of 2020 tweets with hashtag/s",
                           `prop_retweets` = "Prop. of 2020 tweets that are retweets",
                           `fa` = "Prop. tweets in Farsi"),
         log_transformed = ifelse(v %in% c(
           "Number of political tweets (2020)",
           "Coordination (mean)",
           "Coordination (max.)",
           "Number of (covid) misinfo tweets (2020)",
           "Principlist (Conservative)",
           "Number of hateful tweets (2020)",
           "Number of days in the platform",
           "Prop. of 2020 tweets that are retweets",
           "Prop. of 2020 tweets with hashtag/s",
           "Average number of daily tweets",
           "Platform entropy",
           "Follower/Friend ratio",
           "Number of tweets (2020)"
         ), "Log-transformed", "No log transformation")) %>%
  arrange(log_transformed) %>%
  mutate(v = factor(v, levels = unique(v)))


# MAIN: PLOTS
#===============================================================================
# - FIGURE B1: Skewed variables that I log-transform in the models
pdf("./figures/figureB1.pdf", width = 10, height = 9)
ggplot(main_long %>%
         filter(log_transformed == "Log-transformed"),
       aes(x = value)) +
  geom_density(fill = "gray80") +
  geom_rug(alpha = 0.5) +
  facet_wrap(~ v, scales = "free", ncol = 3) +
  theme(panel.background = element_blank(),
        strip.text = element_text(size = 10))
dev.off()

# - FIGURE B2: Non-skewed variables that I do NOT log-transform in the models
pdf("./figures/figureB2.pdf", width = 10, height = 3)
ggplot(main_long %>%
         filter(log_transformed != "Log-transformed") %>%
         mutate(v = factor(v, levels = c(
           "In favor of Iranian government (mean)",
           "Prop. of 2020 tweets at somebody",
           "Prop. tweets in Farsi"
         ))),
       aes(x = value)) +
  geom_density(fill = "gray80") +
  geom_rug(alpha = 0.5) +
  facet_wrap(~ v, scales = "free", ncol = 3) +
  theme(panel.background = element_blank(),
        strip.text = element_text(size = 10))
dev.off()