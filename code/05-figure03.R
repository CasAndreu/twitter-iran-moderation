#===============================================================================
# 05-figure03.R
# Purpose: To replicate Figure 3 of the paper, where we show the marginal effects
#          from a logistic regression predicting account suspension as a function
#          of many covariates, plus the two key variables of interest (ideology
#          and support for the Iranian Government).
# Article: The Geopolitics of Deplatforming: A Study of Suspensions of 
#          Politically-Interested Iranian Accounts on Twitter.
# Authors: Andreu Casas
# Journal: Political Communication
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(ggplot2)
# - our utils file
source("./code/00-functions.R")

# DATA
#===============================================================================
# - load the anonymzed individual-level dataset with covariate information for
#   each account plus whether it's been suspended during the period of analysis
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

# - create a log version of the numeric variables that are skewed according to
#   the descriptive Figures B1 and B2 in Appendix B. 
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

# MAIN
#===============================================================================
# - fit the main model reported in Figure 3
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


# - calculating marginal effects (on the likelhood of an account being suspended) 
#   for a one standard deviation increase for continuous variables, and of being 
#   verified, etc. for binary variables. 
set.seed(12345) # ... setting random see as the exact estimates for the marginal effects
#                 can slightly change -- making sure for this replication these
#                 match the ones reported in the Figure in the paper. 
plot_db <- get_marfx_logistic(model = model03, 
                              model_dataset = main,
                              type = "likelihood")
plot_db <- plot_db %>%
  mutate(pe = round((pe - 1) * 100, 2),
         lwr = round((lwr -1) * 100, 2),
         upr = round((upr -1) * 100, 2))

# - provide human readable labels for the variables in the model
plot_db <- plot_db %>% 
  mutate(
    v = dplyr::recode(v,
                      `mean_embedsim_log` = "Coordination (mean) (logged)",
                      `max_embedsim_log` = "Coordination (max.) (logged)",
                      `user_verified` = "Verified user (binary)",
                      `ideo_log` = "Principlist (Conservative) (logged)",
                      `tweets2020_log` = "Number of tweets (2020) (logged)",
                      `political_log` = "Number of political tweets (2020) (logged)",
                      `uncivil_log` = "Number of hateful tweets (2020) (logged)",
                      `iran_infavor_avg` = "In favor of Iranian government (mean)",
                      `misinfo_log` = "Number of (covid) misinfo tweets (2020) (logged)",
                      `days_on_the_platform_log` = "Number of days in the platform (logged)",
                      `avg_daily_statuses_log` = "Average number of daily tweets (logged)",
                      `follower_friend_ratio_log` = "Follower/Friend ratio (logged)",
                      `platform_entropy_log` = "Platform entropy (logged)",
                      `prop_directed` = "Prop. of 2020 tweets at somebody",
                      `geolocated_bin` = "Geo-enabled tweets (binary)",
                      `prop_hashtag_log` = "Prop. of 2020 tweets with hashtag/s (logged)",
                      `prop_retweets_log` = "Prop. of 2020 tweets that are retweets (logged)",
                      `twitter_for_websites_bin` = "Platform: Twitter Web Client (binary)",
                      `fa` = "Prop. tweets in Farsi (2020)",
                      `en` = "Prop. tweets if English (2020)",
                      `ar` = "Prop. tweets in Arabic (2020)"),
    # - sort the variables so likely bot/human predictors show up at the bottom
    #   of the figure
    vtype = factor(ifelse(v %in% c(
      "Number of tweets (2020)",
      "Number of tweets (2020) (logged)",
      "Number of tweets 90th percentile (2020)",
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
      "Platform: Twitter Web Client (binary)"
    ), "bot features", "nonbot features"))
  ) %>%
  # - sort by effect size
  arrange(desc(vtype), pe)

# PLOT
#===============================================================================
# - a figure visualizing these marginal effects for the main model
pdf("./figures/figure03.pdf", width = 10, height = 7)
ggplot(plot_db %>%
         arrange(desc(vtype), pe) %>%
         mutate(v = factor(v, levels = rev(unique(v))),
                significant = ifelse(sign(lwr) == sign(upr), 1, 0)),
       aes(x = v, y = pe, ymin = lwr, ymax = upr)) +
  geom_polygon(inherit.aes = FALSE,
               data = data.frame(x = c(0.5, 10.75, 10.75, 0.5), y = c(-100, -100, 110, 110)),
               aes(x = x, y = y), alpha = 0.05) +
  geom_segment(aes(x = v, xend = v, y = lwr, yend = upr,
                   alpha = factor(significant))) +
  geom_point(size = 2,
             aes(alpha = factor(significant))) +
  geom_hline(yintercept = 0, color = "black") +
  geom_text(aes(x = as.numeric(v) + 0.4, y = pe, 
                label = paste0(ifelse(sign(pe) == 1, "+", ""), round(pe), "%")),
            size = 2.7) +
  coord_flip() +
  scale_x_discrete("") +
  scale_y_continuous("\nMarginal effect on the likelihood of account being suspended",
                     breaks = NULL, expand = c(0.01,0)) +
  scale_alpha_discrete(range = c(0.3, 1), guide = FALSE) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
        axis.line = element_line(),
        axis.title = element_text(size = 9),
        axis.text.y = element_text(face=c(rep("plain", 11), rep("bold", 2),
                                          rep("plain", 7))),
        strip.text = element_text(size = 10),
        axis.ticks = element_blank())
dev.off()
