#===============================================================================
# 02-figure01.R
# Purpose: To replicate Figure 1 of the paper, where we show cumulative amount
#          suspensions for the period under analysis. 
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
# - load dataset with anonymized info about the accounts we detected at some 
#   point as not being active, when, and whether they were not active because
#   suspended by Twitter or deleted by the user, and whether some went back to
#   being active.
stop_db <- read.csv("./data/stopped-existing-LABELED.csv")

# DATA WRANGLING
#===============================================================================
# - create a 'day' only variable
stop_db$day <- as.Date(sapply(as.character(stop_db$tstamp), function(x)
  strsplit(x, split = " ")[[1]][1]))

# - giving the same date to suspensions/deletions found in the same two-day 
#   period for plotting purposes (they are part of the same "checking batch")
stop_db <- stop_db %>%
  mutate(day02 = recode(factor(as.character(day)),
                        `2020-04-21` = "2020-04-20",
                        `2020-05-08` = "2020-05-06",
                        `2020-09-11` = "2020-09-10"))

# - distinguish between (partially) suspended and deleted; and get cummulative
#   counts
stop_db02 <- stop_db %>%
  filter(status != "restricted",
         !is.na(day)) %>%
  mutate(status02 = ifelse(status %in% c("exists", "suspended"),
                           "Suspended", "Deleted")) %>%
  group_by(status02, day02) %>%
  summarise(n = n()) %>%
  spread(status02, n) %>%
  mutate(Deleted_cum = cumsum(Deleted),
         Suspended_cum = cumsum(Suspended)) %>%
  dplyr::select(-Deleted, -Suspended) %>%
  gather(group, cum, -day02) %>%
  mutate(group = gsub("_cum", "", group),
         # - calculating cumulative suspensions -- we focus on these in the Fig.
         lab = ifelse(as.Date(day02) == max(as.Date(.$day02)), cum, NA))

# DATA WRANGLING
#===============================================================================
pdf("./figures/figure01.pdf", width = 7, height = 5)
ggplot(stop_db02 %>%
         # - only plotting suspensions, ignoring deletions by the users
         filter(group == "Suspended"),
       aes(x = as.Date(day02), y = cum)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  geom_point(size = 2.5, pch = 1) + 
  geom_line() +
  geom_text(inherit.aes = FALSE,
            data = stop_db02 %>%
              filter(group == "Suspended"),
            aes(x = as.Date(day02), y = cum + 125, label = lab)) +
  scale_y_continuous("Number of cumulative suspended accounts\n", expand = c(0,100),
                     limits = c(0, 3900),
                     breaks = seq(0, 4000, 500)) +
  scale_x_date("", expand = c(0,5), 
               limits = c(as.Date("2020-04-16"), as.Date("2020-09-30"))) +
  scale_color_manual("", values = c("darkorchid4", "gold3")) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dotted"),
        axis.line = element_line(),
        legend.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10))
dev.off()

