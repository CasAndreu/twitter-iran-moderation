#===============================================================================
# 01-table01.R
# Purpose: To replicate Table 1 of the paper, where I provide information on
#          the performance of the machine learning models used to classify 
#          political, pro Iran government, and hateful tweets.
# Article: The Geopolitics of Deplatforming: A Study of Suspensions of 
#          Politically-Interested Iranian Accounts on Twitter.
# Authors: Andreu Casas
# Journal: Political Communication
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(xtable)

# DATA
#===============================================================================
# - load inform re performance of the Political classifier for the 5 folds
pol_acc <- read.csv("./data/accuracy-5fold-political.csv") %>%
  # ... add one column indicating to which classifier this data belongs
  mutate(classifier = "Political")

# - load inform re performance of the pro-IranGovernment classifier for 5 folds
proiran_acc <- read.csv("./data/accuracy-5fold-proirangov.csv") %>%
  # ... add one column indicating to which classifier this data belongs
  mutate(classifier = "Pro-IranGov")

# - load inform re performance of the hateful classifier for the 5 folds
hate_acc <- read.csv("./data/accuracy-5fold-hateful.csv") %>%
  # ... add one column indicating to which classifier this data belongs
  mutate(classifier = "Hateful")


# DATA WRANGLING
#===============================================================================
# - combining the performance information for the 3 classifiers
acc_db <- rbind(pol_acc, hate_acc, proiran_acc)

# - calculating 5-fold statistics
nfold_acc <- acc_db %>%
  group_by(classifier) %>%
  dplyr::summarise(Epochs = round(mean(epoch)),
            Accuracy = paste0(round(mean(accuracy), 2) * 100, "%"),
            Precision = paste0(round(mean(precision), 2) * 100, "%"),
            Recall = paste0(round(mean(recall), 2) * 100, "%"),
            `F1-Score` = paste0(round(mean(fscore), 2) * 100, "%")
            ) %>%
  # - sort the classifiers so the rows in the table show up in the same order as
  #   in the table in the paper
  as.data.frame() %>%
  mutate(classifier = factor(classifier, levels = c(
    "Political", "Hateful", "Pro-IranGov"
  ))) %>%
  arrange(classifier)

# - manually add information about how many tweets were coded for each of the
#   classifiers, and the % of true positives/negatives.
nfold_acc$`Labeled` <- c(2893, 1998, 1294)
nfold_acc$`Negative` <- c("56%", "79%", "50%")
nfold_acc$`Positive` <- c("44%", "21%", "50%")

# - sort columns in the same way they show up in Table 1
nfold_acc02 <- nfold_acc %>%
  dplyr::select(classifier, Labeled, Negative, Positive, Epochs,
                Accuracy, Precision, Recall, `F1-Score`)

# ... in Table 1 there is no column title/name for the classifier variable
colnames(nfold_acc02)[1] <- ""

# OUTPUT
#===============================================================================
# - print the latex code for generating Table 1
print(xtable(nfold_acc02, digits = rep(0, 10)), include.rownames = FALSE)

# ... PRINTED OUTPUT:
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrllrllll}
# \hline
# & Labeled & Negative & Positive & Epochs & Accuracy & Precision & Recall & F1-Score \\ 
# \hline
# Political & 2893 & 56\% & 44\% & 7 & 83\% & 81\% & 83\% & 82\% \\ 
# Hateful & 1998 & 79\% & 21\% & 2 & 88\% & 76\% & 66\% & 70\% \\ 
# Pro-IranGov & 1294 & 50\% & 50\% & 4 & 81\% & 77\% & 77\% & 76\% \\ 
# \hline
# \end{tabular}
# \end{table}
