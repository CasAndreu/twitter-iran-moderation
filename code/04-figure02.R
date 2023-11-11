#===============================================================================
# 04-figure02.R
# Purpose: To replicate Figure 2 of the paper, where I show suspension rates by
#          ideological bins, and levels of support for the Iranian Government.
# Article: The Geopolitics of Deplatforming: A Study of Suspensions of 
#          Politically-Interested Iranian Accounts on Twitter.
# Authors: Andreu Casas
# Journal: Political Communication
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(ggplot2)

# DATA
#===============================================================================
# - load a dataset with user-level info for the suspended and non-suspended 
#   accounts in the dataset. N = 171,091.
model_data <- read.csv("./data/model-data-anon.csv")

# MAIN
#===============================================================================
# - transform necessary numeric variables to numeric
model_data <- model_data %>%
  mutate(ideo = as.numeric(as.character(ideo)),
         iran_infavor_avg = as.numeric(as.character(iran_infavor_avg)),
         suspended_lab = ifelse(suspended == "1", "Suspended", "Active"),
         # - create an categoricdal version of the "ideo" and "proiran" vars
         ideo_cat = cut(ideo, breaks = seq(0, 1, 0.25)),
         iran_infavor_cat = cut(iran_infavor_avg, breaks = seq(0, 1, 0.25)))

# - calculate nominators/denominators for each of these two new categorical vars
ideocat_data <- model_data %>%
  group_by(ideo_cat) %>%
  summarise(ideo_denom = n(),
            ideo_nom = length(which(suspended_lab == "Suspended")),
            # - calculate proportions
            susp_prop = round(ideo_nom / ideo_denom, 4)) %>%
  filter(!is.na(ideo_cat))

infavor_data <- model_data %>%
  filter(!is.na(iran_infavor_cat)) %>%
  group_by(iran_infavor_cat) %>%
  summarise(infavor_denom = n(),
            infavor_nom = length(which(suspended_lab == "Suspended")),
            susp_prop = round(infavor_nom / infavor_denom, 4))

# - merging these two datasets, so I can show it all in a single plot
plot_db <- rbind(
  ideocat_data %>%
    rename(cat = ideo_cat, y = ideo_nom) %>%
    mutate(variable = "Ideology", type = "Suspended") %>%
    dplyr::select(-ideo_denom),
  ideocat_data %>%
    rename(cat = ideo_cat) %>%
    mutate(y = ideo_denom - ideo_nom) %>%
    mutate(variable = "Ideology", type = "Active") %>%
    dplyr::select(-ideo_denom, -ideo_nom),
  infavor_data %>%
    rename(cat = iran_infavor_cat, y = infavor_nom) %>%
    mutate(variable = "Support Iranian Government", type = "Suspended") %>%
    dplyr::select(-infavor_denom),
  infavor_data %>%
    rename(cat = iran_infavor_cat) %>%
    mutate(y = infavor_denom - infavor_nom) %>%
    mutate(variable = "Support Iranian Government", type = "Active") %>%
    dplyr::select(-infavor_denom, -infavor_nom)
) %>%
  filter(type == "Suspended")


plot_db <- left_join(plot_db, table_db %>% dplyr::select(
  Category, N, Variable) %>%
    rename(cat = Category, variable = Variable))

# PLOT
#===============================================================================
pdf("./figures/figure02.pdf", width = 10, height = 5)
ggplot(plot_db,
       aes(x = cat, y = susp_prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = susp_prop + 0.005, 
                label = paste0(y, " / ", N, "\n(", susp_prop * 100, "%)")), size = 3) +
  facet_wrap(~ variable) +
  scale_x_discrete("\nIdeological bins") +
  scale_y_continuous("Percentage of suspended users in each ideological bin\n",
                     breaks = seq(0, 1, 0.01),
                     labels = paste0(seq(0, 100, 1), "%")) +
  theme(panel.background = element_blank(),
        strip.text = element_text(size = 12))
dev.off()

