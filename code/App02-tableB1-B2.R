#===============================================================================
# App02-tableB1-B2.R
# Purpose: To replicate Tables B1 and B2 in Appendix B, where we report 
#          coefficient tables for the main model in Figure 3, as well as 
#          five additional model specifications.
# Article: The Geopolitics of Deplatforming: A Study of Suspensions of 
#          Politically-Interested Iranian Accounts on Twitter.
# Authors: Andreu Casas
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
#   to isolate the people who sent an abnoraml number of tweets (>=90th percentile)
main <- main %>%
  mutate(manytweets2020_90 = ifelse(
    tweets2020 > as.numeric(quantile(main$tweets2020, probs = 0.9)), 1, 0))

# - create a log version of the numeric variables that are skewed according to
#   the descriptive figures shown in Figures B1/B2 in Appendix B.
main <- main %>%
  mutate(political_log = log(political_n + 1),
         misinfo_log = log(misinfo_n + 1),
         uncivil_log = log(uncivil_n + 1),
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


# MAIN: FITTING THE MODELS
#===============================================================================

# MODEL 1: no iran sent (because many NAs given that only for political tweets),
#          mean embedsim to measure coordination
#-------------------------------------------------------------------------------
model01 <- glm(suspended ~ 
                 user_verified +
                 ideo_log +
                 mean_embedsim_log +
                 tweets2020_log +
                 political_log +
                 misinfo_log +
                 uncivil_log +
                 days_on_the_platform_log +
                 avg_daily_statuses_log +
                 follower_friend_ratio_log +
                 platform_entropy_log + 
                 prop_directed + 
                 geolocated_bin +
                 prop_hashtag_log + 
                 prop_retweets_log + 
                 twitter_for_websites_bin  +
                 fa,
               data = main, family = "binomial")

# MODEL 2: no iran sent (because many NAs given that only for political tweets),
#          max embedsim to measure coordination
#-------------------------------------------------------------------------------
model02 <- glm(suspended ~ 
                 user_verified +
                 ideo_log +
                 max_embedsim_log +
                 tweets2020_log +
                 political_log +
                 misinfo_log +
                 uncivil_log +
                 days_on_the_platform_log +
                 avg_daily_statuses_log +
                 follower_friend_ratio_log +
                 platform_entropy_log + 
                 prop_directed + 
                 geolocated_bin +
                 prop_hashtag_log + 
                 prop_retweets_log + 
                 twitter_for_websites_bin  +
                 fa,
               data = main, family = "binomial")

# MODEL 3: including iran sentiment 
#          using mean embed sim to measure coordination
#          MAIN MODEL reported in Figure 3 of the paper.
#-------------------------------------------------------------------------------
model03 <- glm(suspended ~ 
                 user_verified +
                 ideo_log +
                 iran_infavor_avg +
                 mean_embedsim_log +
                 tweets2020_log +
                 political_log +
                 misinfo_log +
                 uncivil_log +
                 days_on_the_platform_log +
                 avg_daily_statuses_log +
                 follower_friend_ratio_log +
                 platform_entropy_log + 
                 prop_directed + 
                 geolocated_bin +
                 prop_hashtag_log + 
                 prop_retweets_log + 
                 twitter_for_websites_bin  +
                 fa,
               data = main, family = "binomial")


# MODEL 4: Similar specification as in Model 3, but only including users who
#          tweet in Farsi and are likely to do so from inside Iran. Removing
#          `user_verified` because we are left only with non-suspended verified
#          users -- so no variation at all for this predictor.
#-------------------------------------------------------------------------------
model04 <- glm(suspended ~ 
                  ideo_log +
                  iran_infavor_avg +
                  mean_embedsim_log +
                  tweets2020_log +
                  political_log +
                  misinfo_log +
                  uncivil_log +
                  days_on_the_platform_log +
                  avg_daily_statuses_log +
                  follower_friend_ratio_log +
                  platform_entropy_log + 
                  prop_directed + 
                  geolocated_bin +
                  prop_hashtag_log + 
                  prop_retweets_log + 
                  twitter_for_websites_bin, 
                data = main %>% filter(shutdown_active == 0, fa > 0.5), family = "binomial")

# MODEL 5: Similar specification as in Model 3, but adding an interaction between
#          the coordination measure and the variable measuring support for the
#          Iranian government.
#-------------------------------------------------------------------------------
model05 <- glm(suspended ~ 
                  user_verified +
                  ideo_log +
                  iran_infavor_avg *
                  mean_embedsim_log +
                  tweets2020_log +
                  political_log +
                  misinfo_log +
                  uncivil_log +
                  days_on_the_platform_log +
                  avg_daily_statuses_log +
                  follower_friend_ratio_log +
                  platform_entropy_log + 
                  prop_directed + 
                  geolocated_bin +
                  prop_hashtag_log + 
                  prop_retweets_log + 
                  twitter_for_websites_bin  +
                  fa,
                data = main, family = "binomial")

# MODEL 6: Similar specification as in Model 3, but without using the log version
#          of the log-transformed variables in Model 3.
#-------------------------------------------------------------------------------
# - version with NO log transformations
model06 <- glm(suspended ~ 
                  mean_embedsim + 
                  user_verified +
                  ideo +
                  iran_infavor_avg +
                  tweets2020 +
                  political_n +
                  misinfo_n + 
                  uncivil_n +
                  days_on_the_platform +
                  avg_daily_statuses + 
                  follower_friend_ratio +
                  platform_entropy + 
                  prop_directed + 
                  geolocated_bin +
                  prop_hashtag + 
                  prop_retweets + 
                  twitter_for_websites_bin +
                  fa, 
                data = main, family = "binomial")

# OUTPUT: COEFFICIENTS TALBES FOR THE MODELS
#===============================================================================
# - initialize an empty table where we'll be adding info from each of the models
model_table <- NULL

# - list of all the models
models <- list(model01, model02, model03, model04, model05, model06)

# - iterate through the different models and pull quantities of interest for the
#   coefficient tables
for (i in 1:length(models)) {
  m <- models[[i]]
  m_tidy <- tidy(m)
  aic <- m$aic
  n <- nrow(m$model)
  model_col <- m_tidy %>%
    mutate(coef_se = ifelse(p.value < 0.05,
                            paste0(
                              round(estimate, 4), " (",
                              round(std.error, 4), ")*"
                            ),
                            paste0(
                              round(estimate, 4), " (",
                              round(std.error, 4), ") "
                            ))) %>%
    dplyr::select(term, coef_se)
  model_col <- rbind(model_col,
                     data.frame(term = c("N", "AIC"),
                                coef_se = c(n, round(aic, 2))))
  colnames(model_col)[2] <- paste0("Model ", i)
  if (is.null(model_table)) {
    model_table <- model_col
  } else {
    model_table <- full_join(model_table, model_col)
  }
}

# - give human-readable names to the variables and sort them
model_table02 <- model_table %>%
  mutate(Variable = dplyr::recode(term,
                                  `mean_embedsim` = "Coordination (mean)",
                                  `mean_embedsim_log` = "Coordination (mean) (logged)",
                                  `max_embedsim` = "Coordination (max.)",
                                  `max_embedsim_log` = "Coordination (max.) (logged)",
                                  `user_verified` = "Verified user (binary)",
                                  `ideo` = "Principlist (Conservative)",
                                  `ideo_log` = "Principlist (Conservative) (logged)",
                                  `tweets2020` = "Number of tweets (2020)",
                                  `tweets2020_log` = "Number of tweets (2020) (logged)",
                                  `political_n` = "Number of political tweets (2020)",
                                  `political_log` = "Number of political tweets (2020) (logged)",
                                  `uncivil_n` = "Number of hateful tweets (2020)",
                                  `uncivil_log` = "Number of hateful tweets (2020) (logged)",
                                  `iran_infavor_avg` = "In favor of Iranian government (mean)",
                                  `iran_infavor_max` = "In favor of Iranian government (max.)",
                                  `iran_infavor_avg:mean_embedsim_log` = "[In favor of Iranian gov.] X [Coordination (mean) (logged)]",
                                  `misinfo_n` = "Number of (covid) misinfo tweets (2020)",
                                  `misinfo_log` = "Number of (covid) misinfo tweets (2020) (logged)",
                                  `days_on_the_platform` = "Number of days in the platform",
                                  `days_on_the_platform_log` = "Number of days in the platform (logged)",
                                  `avg_daily_statuses` = "Average number of daily tweets",
                                  `avg_daily_statuses_log` = "Average number of daily tweets (logged)",
                                  `follower_friend_ratio` = "Follower/Friend ratio",
                                  `follower_friend_ratio_log` = "Follower/Friend ratio (logged)",
                                  `platform_entropy` = "Platform entropy",
                                  `platform_entropy_log` = "Platform entropy (logged)",
                                  `prop_directed` = "Prop. of 2020 tweets at somebody",
                                  `geolocated_bin` = "Geo-enabled tweets (binary)",
                                  `prop_hashtag` = "Prop. of 2020 tweets with hashtag/s",
                                  `prop_hashtag_log` = "Prop. of 2020 tweets with hashtag/s (logged)",
                                  `prop_retweets` = "Prop. of 2020 tweets that are retweets",
                                  `prop_retweets_log` = "Prop. of 2020 tweets that are retweets (logged)",
                                  `twitter_for_websites_bin` = "Platform: Twitter Web Client (binary)",
                                  `fa` = "Prop. tweets in Farsi (2020)"),
         Variable = factor(as.character(Variable), levels = c(
           "(Intercept)",
           "Principlist (Conservative)",
           "Principlist (Conservative) (logged)",
           "In favor of Iranian government (mean)",
           "[In favor of Iranian gov.] X [Coordination (mean) (logged)]",
           "Coordination (mean)",
           "Coordination (mean) (logged)",
           "Coordination (median)",
           "Coordination (max.)",
           "Coordination (max.) (logged)",
           "Verified user (binary)",
           "Number of tweets (2020)",
           "Number of tweets (2020) (logged)",
           "Number of tweets 90th percentile, binary (2020)",
           "Number of political tweets (2020)",
           "Number of political tweets (2020) (logged)",
           "Number of hateful tweets (2020)",
           "Number of hateful tweets (2020) (logged)",
           "Number of (covid) misinfo tweets (2020)",
           "Number of (covid) misinfo tweets (2020) (logged)",
           "Number of days in the platform",
           "Number of days in the platform (logged)",
           "Average number of daily tweets",
           "Average number of daily tweets (logged)",
           "Follower/Friend ratio",
           "Follower/Friend ratio (logged)",
           "Platform entropy",
           "Platform entropy (logged)",
           "Prop. of 2020 tweets at somebody",
           "Geo-enabled tweets (binary)",
           "Prop. of 2020 tweets with hashtag/s",
           "Prop. of 2020 tweets with hashtag/s (logged)",
           "Prop. of 2020 tweets that are retweets",
           "Prop. of 2020 tweets that are retweets (logged)",
           "Platform: Twitter Web Client (binary)",
           "Prop. tweets in Farsi (2020)",
           "N",
           "AIC"))) %>%
  arrange(Variable)

# - latex code for TABLE B1 (/!\ some empty rows for some predictors that are 
#   not relevant for the first 3 models need to be removed manually)
print(xtable(model_table02[, c(8, 2:4)]), include.rownames = FALSE)


# - latex code for TABLE B2 (/!\ some empty rows for some predictors that are 
#   not relevant for the last 3 models need to be removed manually)
print(xtable(model_table02[, c(8, 5:7)]), include.rownames = FALSE)
