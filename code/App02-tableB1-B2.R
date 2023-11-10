#===============================================================================
# App02-tableB1-B2.R
# Purpose: To replicate Tables B1 and B2 in Appendix B, where we report 
#          coefficient tables for the main model in Figure 3, as well as 
#          five additional model specifications.
# Article: The Geopolitics of Deplatforming: A Study of Suspensions of 
#          Politically-Interested Iranian Accounts on Twitter.
# Authors: Mehdi Zamani & Andreu Casas
# Journal: Political Communication
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(broom)
library(xtable)
# - our utils file
source("./code/00-functions.R")

# DATA
#===============================================================================
# - load the model dataset
main <- read.csv("./data/model-data-anon.csv")


# DATA WRANGLING
#===============================================================================
# - make sure numeric variables are numeric
num_vars <- c("mean_embedsim", "max_embedsim",
              "user_verified", "ideo", "tweets2020", "political_n",
              "political_prop", "uncivil_n", "uncivil_prop",
              "iran_infavor_avg", "iran_infavor_max",
              "suspended", "misinfo_n",
              "days_on_the_platform",
              "avg_daily_statuses", "follower_friend_ratio",
              "platform_entropy", "prop_directed", "geolocated_bin",
              "prop_hashtag", "prop_retweets", "twitter_for_websites_bin",
              "ar", "en", "fa")

for (numvar in num_vars) {
  main[,numvar] <- as.numeric(main[,numvar])
}

# - building a categorical version of the tweets2020 variable, in which we try
#   to isolate the people who sent a crazy number of tweets (>=90th percentile)
main <- main %>%
  mutate(manytweets2020_90 = ifelse(tweets2020 > 924, 1, 0))

# - create a log version of the numeric variables that are skewed according to
#   the descriptive figures generated in script 31.R
main <- main %>%
  mutate(political_log = log(political_n + 1),
         misinfo_log = log(misinfo_n + 1),
         uncivil_log = log(uncivil_n + 1),
         #tweets2020_log = log(tweets2020 + 1),
         tweets2020_log = log(tweets2020),
         days_on_the_platform_log = log(days_on_the_platform),
         avg_daily_statuses_log = log(avg_daily_statuses + 1),
         mean_embedsim_log = log(mean_embedsim),
         max_embedsim_log = log(max_embedsim),
         follower_friend_ratio_log = log(follower_friend_ratio + 0.01),
         ideo_log = log(ideo + 0.01),
         platform_entropy_log = log(platform_entropy + 0.01),
         prop_hashtag_log = log(prop_hashtag + 0.01),
         prop_retweets_log = log(prop_retweets + 0.01),
         iran_infavor_max_log = log(iran_infavor_max + 0.01)
  )


# MAIN
#===============================================================================


# OUTPUT
#===============================================================================
