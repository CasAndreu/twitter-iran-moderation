#===============================================================================
# 06-figure04.R
# Purpose: To replicate Figure 4 of the paper, where we show the hashtags and
#          elite accounts used/followed at higher or lower rate by (non)suspended
#          accounts.
#          GENERAL INSTRUCTIONS: Figure 1.A and 1.B are generated as separate 
#          figures -- and we then merged them "manually" into a single figure
#          for the paper. 
#          /!\ Figure 1.A, which contains hashtags in Farsi, we didn't succeed
#          at generating the correct Farsi unicodes when running the code in a
#          MacBook. Instead, we run this script in an Ubuntu machine (18.02) with
#          R 3.6.3 installed, in order to get the correct Farsi unicodes/spelling.
# Article: The Geopolitics of Deplatforming: A Study of Suspensions of 
#          Politically-Interested Iranian Accounts on Twitter.
# Authors: Mehdi Zamani & Andreu Casas
# Journal: Political Communication
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(ggplot2)

# DATA
#===============================================================================
# - dataset with information about % of suspended v. non-suspended users that 
#   used each of the unique HASHTAGS in at least 1 of their tweets. In this
#   dataset we already have only the top 
hash_db <- read.csv("./data/hash-freq-diff-suspended-nonsuspended.csv")

# - dataset with information about % of suspended v. non-suspended users that 
#   followed the unique ELITE accounts in our dataset
elite_db <- read.csv("./data/elite-freq-diff-suspended-nonsuspended.csv")


# MAIN
#===============================================================================

# [ A ] HASHTAGS
#-------------------------------------------------------------------------------
# - keep the top 40 and bottom 20 hashtags: keeping more from the top becasuse
#   it's where we see more substantive variation.
top_bottom_hash <- rbind(
  head(hash_db, 40),
  tail(hash_db, 20)
) %>%
  mutate(hashtag = factor(hashtag,
                          levels = rev(unique(hashtag))))

# - HASHTAGS PLOT
ggplot(top_bottom_hash,
       aes(x = hashtag, y = dif, fill = dif)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +
  geom_text(aes(label = hashtag, 
                x = hashtag,
                y = ifelse(dif > 0, -0.01, 0.01),
                hjust = ifelse(dif > 0, 1, 0))) +
  geom_text(aes(label = ifelse(dif > 0 ,
                               paste0("+", round(dif * 100, 1)),
                               round(dif * 100, 1)),
                x = hashtag,
                y = ifelse(dif > 0 , dif + 0.005, dif - 0.005),
                hjust = ifelse(dif > 0, 0, 1))) +
  coord_flip() +
  scale_y_continuous("\nDifference (percentage points) in the proportion of Suspended v. Non-Suspended\nusers that used these hashtags in 2020", 
                     breaks = NULL, limits = c(-0.3, 0.7)) +
  scale_x_discrete("", breaks = NULL) +
  scale_fill_gradient2(low = "green4", high = "red4") +
  theme(panel.background = element_blank(),
        legend.position = "none")
ggsave("./figures/figure04a.jpeg", width = 6, height = 10, device = "jpeg")


# [ B ] ELITE ACCOUNTS
#-------------------------------------------------------------------------------
# - plot top and bottom elite accounts (so the most followed by suspended v.
#.  non-suspended) 
plotdb <- rbind(
  elite_db %>% filter(dif > 0.01),
  elite_db %>% filter(dif < -0.01)
) %>%
  mutate(elite = paste0("@", elite),
         elite = factor(elite, levels = rev(elite)))

ggplot(plotdb,
       aes(x = elite, y = dif, fill = dif)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +
  geom_text(aes(label = elite, 
                x = elite,
                y = ifelse(dif > 0, -0.01, 0.01),
                hjust = ifelse(dif > 0, 1, 0))) +
  geom_text(aes(label = ifelse(dif > 0 ,
                               paste0("+", round(dif * 100, 1)),
                               round(dif * 100, 1)),
                x = elite,
                y = ifelse(dif > 0 , dif + 0.005, dif - 0.005),
                hjust = ifelse(dif > 0, 0, 1))) +
  coord_flip() +
  scale_y_continuous("\nDifference (percentage points) in the proportion of Suspended v. Non-Suspended\n that follow these elite accounts", 
                     breaks = NULL, limits = c(-0.15, 0.30)) +
  scale_x_discrete("", breaks = NULL) +
  scale_fill_gradient2(low = "green4", high = "red4") +
  theme(panel.background = element_blank(),
        legend.position = "none")
ggsave("./figures/figure04b.jpeg", width = 6, height = 10, device = "jpeg")
