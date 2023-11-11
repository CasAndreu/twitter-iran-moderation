#===============================================================================
# 03-table02.R
# Purpose: To replicate Table 2 of the paper, where I conduct some bivariate
#          comparisons between suspended and non-suspended users, along a set of
#          relevant features. 
# Article: The Geopolitics of Deplatforming: A Study of Suspensions of 
#          Politically-Interested Iranian Accounts on Twitter.
# Authors: Andreu Casas
# Journal: Political Communication
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(scales)
library(xtable)

# DATA
#===============================================================================
# - load a dataset with user-level info for the suspended and non-suspended 
#   accounts in the dataset. N = 171,091.
main <- read.csv("./data/model-data-anon.csv")

# DATA WRANGLING
#===============================================================================
# - make sure numeric variables are numeric
num_vars <- c("mean_embedsim", "max_embedsim", #"median_embedsim",
              "user_verified", "ideo", "tweets2020", "political_n",
              "political_prop", "uncivil_n", "uncivil_prop",
              "iran_infavor_avg",  "iran_infavor_max", #"iran_infavor_median",
              "suspended", "misinfo_n",
              "statuses_n", "friends_n", "followers_n", "days_on_the_platform",
              "avg_daily_statuses", "follower_friend_ratio",
              "platform_entropy", "prop_directed", "geolocated_bin",
              "prop_hashtag", "prop_retweets", "twitter_for_websites_bin",
              "ar", "en", "fa")

for (numvar in num_vars) {
  main[,numvar] <- as.numeric(main[,numvar])
}

# - building a categorical version of the tweets2020 variable, in which I try
#   to isolate the people who sent a disproportionate number of tweets (>=90th percentile)
main <- main %>%
  mutate(manytweets2020_90 = ifelse(
    tweets2020 > as.numeric(quantile(main$tweets2020, probs = 0.9)), 1, 0))

# - standardize coordination/similarity & ideology measures
main <- main %>%
  mutate(mean_embedsim_std = rescale(mean_embedsim, c(0,1)),
         ideo_std = rescale(ideo, c(0,1)),
         iran_infavor_avgstd = rescale(iran_infavor_avg, c(0,1)))

# - run the comparisons for the covariates of interest
compare_stats <- function(data) {
  out_db <- data %>%
    group_by(suspended) %>%
    summarise(
      # Number of tweets
      tweets_n = paste0(
        round(mean(tweets2020, na.rm = TRUE)),
        " [",
        round(t.test(tweets2020, na.rm = TRUE)$conf.int[[1]]),
        "-",
        round(t.test(tweets2020, na.mr = TRUE)$conf.int[[2]]), 
        "]"),
      # Number of users in the 90% percentile of num of tweets
      tweets_90p = round(sum(manytweets2020_90, na.rm = TRUE) / n(), 2),
      
      # Number of days in the platform
      days_n = paste0(
        round(mean(days_on_the_platform, na.rm = TRUE)),
        " [",
        round(t.test(days_on_the_platform, na.rm = TRUE)$conf.int[[1]]),
        "-",
        round(t.test(days_on_the_platform, na.mr = TRUE)$conf.int[[2]]), 
        "]"),
      
      # Avg daily posts
      daily_posts_avg = paste0(
        round(mean(avg_daily_statuses, na.rm = TRUE), 2),
        " [",
        round(t.test(avg_daily_statuses, na.rm = TRUE)$conf.int[[1]], 2),
        "-",
        round(t.test(avg_daily_statuses, na.mr = TRUE)$conf.int[[2]], 2), 
        "]"),
      
      # Avg follower-friend ratio
      follower_friend_avgratio = paste0(
        round(mean(follower_friend_ratio, na.rm = TRUE), 2),
        " [",
        round(t.test(follower_friend_ratio, na.rm = TRUE)$conf.int[[1]], 2),
        "-",
        round(t.test(follower_friend_ratio, na.mr = TRUE)$conf.int[[2]], 2), 
        "]"),
      
      # Avg platform entropy
      platform_entropy_avg = paste0(
        round(mean(platform_entropy, na.rm = TRUE), 2),
        " [",
        round(t.test(platform_entropy, na.rm = TRUE)$conf.int[[1]], 2),
        "-",
        round(t.test(platform_entropy, na.mr = TRUE)$conf.int[[2]], 2), 
        "]"),
      
      # Avg prop of directed tweets
      prop_directed_avg = paste0(
        round(mean(prop_directed, na.rm = TRUE), 2),
        " [",
        round(t.test(prop_directed, na.rm = TRUE)$conf.int[[1]], 2),
        "-",
        round(t.test(prop_directed, na.mr = TRUE)$conf.int[[2]], 2), 
        "]"),
      
      # Prop geolocated
      prop_geolocated_group = round(sum(geolocated_bin, na.rm = TRUE) / n(), 2),
      
      # Avg prop hashtag
      prop_hash_avg = paste0(
        round(mean(prop_hashtag, na.rm = TRUE), 2),
        " [",
        round(t.test(prop_hashtag, na.rm = TRUE)$conf.int[[1]], 2),
        "-",
        round(t.test(prop_hashtag, na.mr = TRUE)$conf.int[[2]], 2), 
        "]"),
      
      # Avg prop retweets
      prop_retweets_avg = paste0(
        round(mean(prop_retweets, na.rm = TRUE), 2),
        " [",
        round(t.test(prop_retweets, na.rm = TRUE)$conf.int[[1]], 2),
        "-",
        round(t.test(prop_retweets, na.mr = TRUE)$conf.int[[2]], 2), 
        "]"),
      
      # Prop Platform: Twitter Web Client
      twitter_for_websites_prop = round(sum(twitter_for_websites_bin) / n(), 2),
      
      # Number of political tweets
      political_n = paste0(
        round(mean(political_n, na.rm = TRUE)),
        " [",
        round(t.test(political_n, na.rm = TRUE)$conf.int[[1]]),
        "-",
        round(t.test(political_n, na.mr = TRUE)$conf.int[[2]]), 
        "]"),
      
      political_prop = paste0(
        round(mean(political_prop, na.rm = TRUE), 2),
        " [",
        round(t.test(political_prop, na.rm = TRUE)$conf.int[[1]], 2),
        "-",
        round(t.test(political_prop, na.mr = TRUE)$conf.int[[2]], 2), 
        "]"),
      
      # Uncivil
      uncivil_n = paste0(
        round(mean(uncivil_n, na.rm = TRUE)),
        " [",
        round(t.test(uncivil_n, na.rm = TRUE)$conf.int[[1]]),
        "-",
        round(t.test(uncivil_n, na.mr = TRUE)$conf.int[[2]]), 
        "]"),
      
      uncivil_prop = paste0(
        round(mean(uncivil_prop, na.rm = TRUE), 3),
        " [",
        round(t.test(uncivil_prop, na.rm = TRUE)$conf.int[[1]], 3),
        "-",
        round(t.test(uncivil_prop, na.mr = TRUE)$conf.int[[2]], 3), 
        "]"),
      
      # Covid-misinfo tweets
      covid_misinfo_n = paste0(
        round(mean(misinfo_n, na.rm = TRUE)),
        " [",
        round(t.test(misinfo_n, na.rm = TRUE)$conf.int[[1]]),
        "-",
        round(t.test(misinfo_n, na.mr = TRUE)$conf.int[[2]]), 
        "]"),
      
      # Verified
      verified_prop = round(sum(user_verified)/n(), 3),
      
      # Ideology
      ideo = paste0(
        round(mean(ideo_std, na.rm = TRUE), 3),
        " [",
        round(t.test(ideo_std, na.rm = TRUE)$conf.int[[1]], 3),
        "-",
        round(t.test(ideo_std, na.mr = TRUE)$conf.int[[2]], 3), 
        "]"),
      
      # Coordination
      coordination = paste0(
        round(mean(mean_embedsim_std, na.rm = TRUE), 3),
        " [",
        round(t.test(mean_embedsim_std, na.rm = TRUE)$conf.int[[1]], 3),
        "-",
        round(t.test(mean_embedsim_std, na.mr = TRUE)$conf.int[[2]], 3), 
        "]"),
      
      # In favor of Iranian government
      iran_infavor = paste0(
        round(mean(iran_infavor_avgstd, na.rm = TRUE), 3),
        " [",
        round(t.test(iran_infavor_avgstd, na.rm = TRUE)$conf.int[[1]], 3),
        "-",
        round(t.test(iran_infavor_avgstd, na.mr = TRUE)$conf.int[[2]], 3), 
        "]"),
      
      # Avg. Prop. of messages in FA
      langprop_fa = paste0(
        round(mean(fa, na.rm = TRUE), 3),
        " [",
        round(t.test(fa, na.rm = TRUE)$conf.int[[1]], 3),
        "-",
        round(t.test(fa, na.mr = TRUE)$conf.int[[2]], 3), 
        "]"),
      
      # Avg. Prop. of messages in EN
      langprop_en = paste0(
        round(mean(en, na.rm = TRUE), 3),
        " [",
        round(t.test(en, na.rm = TRUE)$conf.int[[1]], 3),
        "-",
        round(t.test(en, na.mr = TRUE)$conf.int[[2]], 3), 
        "]"),
      
      # Avg. Prop. of messages in AR
      langprop_ar = paste0(
        round(mean(ar, na.rm = TRUE), 3),
        " [",
        round(t.test(ar, na.rm = TRUE)$conf.int[[1]], 3),
        "-",
        round(t.test(ar, na.mr = TRUE)$conf.int[[2]], 3), 
        "]")
      
    ) %>% t()
  return(out_db[2:nrow(out_db),])
}

out_db <- as.data.frame(compare_stats(main))

# - convert variables to row names for the table
out_db$variable <- rownames(out_db)
# - define the row names
cols <- c("Non-Suspended", "Suspended", "Variable")
colnames(out_db) <- cols
out_db <- out_db[,c("Variable", "Non-Suspended", "Suspended")]

# - provide human-readable names to the variables being compared. Also, sort them
#   so that I have bot-relevant features at the top, then the rest, and at the
#   end the 2 key features of interest (ideo and prop. supportive Iran Gov.)
out_db <- out_db %>%
  mutate(Variable = recode(factor(as.character(Variable)),
                           `coordination` = "Avg. Coordination score {0-1}",
                           `iran_infavor` = "Avg. Prop. In favor of Iranian government {0-1}",
                           `verified_prop` = "Prop. of verified users",
                           `ideo` = "Avg. Principlist (Conservative) score {0-1}",
                           `tweets_n` = "Avg. Number of tweets (2020)",
                           `political_n` = "Avg. Number of political tweets (2020)",
                           `political_prop` = "Avg. Prop. of political tweets (2020)",
                           `uncivil_n` = "Avg. Number of hateful tweets (2020)",
                           `uncivil_prop` = "Avg. Prop. of hateful tweets (2020)",
                           `covid_misinfo_n` = "Avg. Number of Covid-Misinfo tweets (2020)",
                           `tweets_90p` = "Prop. in the 90th most active percentile (2020)",
                           `days_n` = "Avg. Number of days since account creation",
                           `daily_posts_avg` = "Avg. daily posts",
                           `follower_friend_avgratio` = "Avg. Follower/Friend ratio",
                           `platform_entropy_avg` = "Avg. Entropy of platform use",
                           `prop_directed_avg` = "Avg. Proportion of tweets at somebody",
                           `prop_geolocated_group` = "Prop. of Geo-enabled accounts",
                           `prop_hash_avg` = "Avg. Proportion of tweets with a hashtag",
                           `prop_retweets_avg` = "Avg. Proportion of retweets",
                           `twitter_for_websites_prop` = "Prop. using Twitter Web Client platform",
                           `langprop_fa` = "Avg. Prop. tweets in Farsi (2020)",
                           `langprop_en` = "Avg. Prop. tweets if English (2020)",
                           `langprop_ar` = "Avg. Prop. tweets in Arabic (2020)"
  ),
  Variable = factor(Variable, levels = c(
    "Avg. Number of days since account creation",
    "Avg. daily posts",
    "Avg. Follower/Friend ratio",
    "Avg. Entropy of platform use",
    "Avg. Proportion of directed tweets",
    "Prop. of Geo-enabled accounts",
    "Avg. Proportion of tweets with a hashtag",
    "Avg. Proportion of tweets at somebody",
    "Avg. Proportion of retweets",
    "Prop. using Twitter Web Client platform",
    "Avg. Number of tweets (2020)",
    "Prop. in the 90th most active percentile (2020)",
    "Prop. of verified users",
    "Avg. Number of political tweets (2020)",
    "Avg. Prop. of political tweets (2020)",
    "Avg. Number of hateful tweets (2020)",
    "Avg. Prop. of hateful tweets (2020)",
    "Avg. Number of Covid-Misinfo tweets (2020)",
    "Avg. Coordination score {0-1}",
    "Avg. Prop. tweets in Farsi (2020)",
    "Avg. Prop. tweets if English (2020)",
    "Avg. Prop. tweets in Arabic (2020)",
    "Avg. Principlist (Conservative) score {0-1}",
    "Avg. Prop. In favor of Iranian government {0-1}"))) %>%
  arrange(Variable)


# OUTPUT
#===============================================================================
# - Latex code for generating Table 2 in the paper
print(xtable(out_db), include.rownames = FALSE)

# ... output:
# \begin{table}[ht]
# \centering
# \begin{tabular}{lll}
# \hline
# Variable & Non-Suspended & Suspended \\ 
# \hline
# Avg. Number of days since account creation & 1337 [1332-1342] & 1067 [1023-1111] \\ 
# Avg. daily posts & 1.32 [1.28-1.35] & 7.12 [6.36-7.87] \\ 
# Avg. Follower/Friend ratio & 2.3 [1.7-2.9] & 111.7 [55.66-167.74] \\ 
# Avg. Entropy of platform use & 0.2 [0.19-0.2] & 0.2 [0.19-0.22] \\ 
# Prop. of Geo-enabled accounts & 0.03 & 0.02 \\ 
# Avg. Proportion of tweets with a hashtag & 0.21 [0.21-0.21] & 0.23 [0.22-0.24] \\ 
# Avg. Proportion of tweets at somebody & 0.48 [0.47-0.48] & 0.46 [0.45-0.47] \\ 
# Avg. Proportion of retweets & 0.23 [0.23-0.23] & 0.27 [0.26-0.29] \\ 
# Prop. using Twitter Web Client platform & 0.04 & 0.02 \\ 
# Avg. Number of tweets (2020) & 396 [390-402] & 1514 [1421-1606] \\ 
# Prop. in the 90th most active percentile (2020) & 0.10 & 0.39 \\ 
# Prop. of verified users & 0.003 & 0.001 \\ 
# Avg. Number of political tweets (2020) & 153 [151-156] & 562 [522-603] \\ 
# Avg. Prop. of political tweets (2020) & 0.35 [0.35-0.35] & 0.37 [0.36-0.38] \\ 
# Avg. Number of hateful tweets (2020) & 7 [7-7] & 33 [30-36] \\ 
# Avg. Prop. of hateful tweets (2020) & 0.008 [0.008-0.008] & 0.006 [0.005-0.008] \\ 
# Avg. Number of Covid-Misinfo tweets (2020) & 0 [0-0] & 1 [1-2] \\ 
# Avg. Coordination score \{0-1\} & 0.947 [0.947-0.948] & 0.974 [0.972-0.975] \\ 
# Avg. Prop. tweets in Farsi (2020) & 0.611 [0.609-0.613] & 0.559 [0.542-0.575] \\ 
# Avg. Prop. tweets if English (2020) & 0.136 [0.135-0.138] & 0.146 [0.135-0.157] \\ 
# Avg. Prop. tweets in Arabic (2020) & 0.07 [0.069-0.071] & 0.112 [0.101-0.122] \\ 
# Avg. Principlist (Conservative) score \{0-1\} & 0.112 [0.111-0.112] & 0.126 [0.122-0.13] \\ 
# Avg. Prop. In favor of Iranian government \{0-1\} & 0.429 [0.428-0.431] & 0.49 [0.479-0.501] \\ 
# \hline
# \end{tabular}
# \end{table}

# COLORING OF STATISTICALLY SIGNIFICANT FINDINGS: 
# - NUMERIC variables: I use (a lack of) overalp of the confidence intervals
# - PROPORTIONS: I use the following code to check whether there is a statistical
#                difference in proportions
prop.test( # NO (pvalue 0.2)
  x = c(
    nrow(main %>% filter(suspended == 0 & user_verified == 1)),
    nrow(main %>% filter(suspended == 1 & user_verified == 1))
  ),
  n = c(
    nrow(main %>% filter(suspended == 0)),
    nrow(main %>% filter(suspended == 1))
  ),
  conf.level=0.95
)

# ... prop. in the 90th percentile of activity
prop.test( # YES (pvalue < 0.01)
  x = c(
    nrow(main %>% filter(suspended == 0 & manytweets2020_90 == 1)),
    nrow(main %>% filter(suspended == 1 & manytweets2020_90 == 1))
  ),
  n = c(
    nrow(main %>% filter(suspended == 0)),
    nrow(main %>% filter(suspended == 1))
  ),
  conf.level=0.95
)

# ... prop. of geo-enabled accounts
prop.test( # YES (pvalue < 0.05)
  x = c(
    nrow(main %>% filter(suspended == 0 & geolocated_bin == 1)),
    nrow(main %>% filter(suspended == 1 & geolocated_bin == 1))
  ),
  n = c(
    nrow(main %>% filter(suspended == 0)),
    nrow(main %>% filter(suspended == 1))
  ),
  conf.level=0.95
)

# ... prop. using web client
prop.test( # YES (pavlue < 0.01)
  x = c(
    nrow(main %>% filter(suspended == 0 & twitter_for_websites_bin == 1)),
    nrow(main %>% filter(suspended == 1 & twitter_for_websites_bin == 1))
  ),
  n = c(
    nrow(main %>% filter(suspended == 0)),
    nrow(main %>% filter(suspended == 1))
  ),
  conf.level=0.95
)