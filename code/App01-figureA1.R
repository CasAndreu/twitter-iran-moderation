#===============================================================================
# App01-figureA1.R
# Purpose: To replicate Figure A1 in Appendix A, where we show the average
#          ideology score attributed to Reformist-Independent-Principlist
#          politicians.
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
# - dataset with ideology scores estiamted for elites
elite_ideo <- read.csv("./data/elite-accounts-ideo-scores.csv")

# - dataset with info re political affiliation of the political elites
elite_aff <- read.csv("./data/elite-twitter-handles.csv")

# MAIN
#===============================================================================
# - merge the two datasets
elite_aff <- elite_aff %>%
  mutate(twitter = gsub("@", "", tolower(Twitter.handle)))

db <- left_join(elite_ideo, elite_aff)

# - standardize ideo score between 0 and 1; select only the elites for which
#   we have clear information about their political affiliation; and calcualte
#   average ideology
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
outdb <- db %>%
  rename(affiliation = `Political.affiliation`) %>%
  mutate(affiliation = dplyr::recode(affiliation,
                                     `Centrist` = "Independent",
                                     `Conservative` = "Principlist")) %>%
  filter(affiliation != "",
         affiliation %in% c("Principlist",
                            "Independent",
                            "Reformist")) %>%
  mutate(pe_std = range01(pe)) %>%
  group_by(affiliation) %>%
  summarise(n = n(),
            avg = mean(pe_std),
            lwr80 = t.test(pe_std, conf.level = .8)$conf.int[[1]],
            upr80 = t.test(pe_std, conf.level = .8)$conf.int[[2]],
            lwr95 = t.test(pe_std, conf.level = .95)$conf.int[[1]],
            upr95 = t.test(pe_std, conf.level = .95)$conf.int[[2]]) %>%
  as.data.frame() %>%
  mutate(affiliation = factor(affiliation,
                              levels = c("Reformist", "Independent", "Principlist")))

# PLOT
#===============================================================================
pdf("./figures/figureA1.pdf", width = 8, height = 6)
ggplot(outdb,
       aes(x = affiliation, y = avg, color = affiliation)) +
  geom_segment(aes(x = affiliation, xend = affiliation,
                   y = lwr95, yend = upr95), size = 1) +
  geom_segment(aes(x = affiliation, xend = affiliation,
                   y = lwr80, yend = upr80), size = 2) +
  geom_point(size = 4) +
  coord_flip()  +
  scale_x_discrete("") +
  scale_y_continuous("\nAverage ideology score") +
  scale_color_discrete("", guide = FALSE) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(linetype = "dotted"),
        axis.text = element_text(size = 12),
        axis.line.x = element_line())
dev.off()
