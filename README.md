# The Geopolitics of Deplatforming: A Study of Suspensions of Politically-Interested Iranian Accounts on Twitter
This repository contains the replication material for the paper "The Geopolitics of Deplatforming: A Study of Suspensions of Politically-Interested Iranian Accounts on Twitter", by Andreu Casas, to be published at _Political Communication_.

## Data
The `./data/` directory contains the necessary data to replicate the analytical figures and tables of the paper. Below, I describe each of the datasets in this directory:

## Code
The `./code/` directory contains separate scripts to replicate each analytical figure in the article. The `./figures/` directory contains a copy of each of the figures generated by these scripts. 

### Tables/Figures Main Paper

___

- [`01-table01.R`](https://github.com/CasAndreu/twitter-iran-moderation/blob/main/code/01-table01.R): replicates Table 1 of the paper, where I provide information on the performance of the machine learning models used to classify political, pro Iran government, and hateful tweets.

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/table01.png">

<br>
<br>

- [`02-figure01.R`](https://github.com/CasAndreu/twitter-iran-moderation/blob/main/code/02-figure01.R): replicates Figure 1 of the paper, where I show cumulative amount suspensions for the period under analysis. 

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/figure01.png">

<br>
<br>

- [`03-table02.R`](https://github.com/CasAndreu/twitter-iran-moderation/blob/main/code/03-table02.R): replicates Table 2 of the paper, where I show simple descriptives for the covariates of interest, comparing suspended and non-suspended users.

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/table02.png">

<br>
<br>

- [`04-figure02.R`](https://github.com/CasAndreu/twitter-iran-moderation/blob/main/code/04-figure02.R): replicates Figure 2 of the paper, where I show suspension rates by ideological bins, and levels of support for the Iranian Government.

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/figure02.png">

<br>
<br>

- [`05-figure03.R`](https://github.com/CasAndreu/twitter-iran-moderation/blob/main/code/05-figure03.R): replicates Figure 3 of the paper, where I show the marginal effects from a logistic regression predicting account suspension as a function of many covariates, plus the two key variables of interest (ideology and support for the Iranian Government).

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/figure03.png">

<br>
<br>

- [`06-figure04.R`](https://github.com/CasAndreu/twitter-iran-moderation/blob/main/code/06-figure04.R): replicates Figure 4 of the paper, where I show the hashtags and elite accounts used/followed at higher or lower rate by (non)suspended accounts.

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/figure04.png">

<br>
<br>

### Tables/Figures Appendix
___

- [`App01-figureA1.R`](https://github.com/CasAndreu/twitter-iran-moderation/blob/main/code/App01-figureA1.R): replicates Figure A1 in Appendix A, where I show the average ideology score attributed to Reformist-Independent-Principlist politicians.

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/figureA1.png">

<br>
<br>

- [`App02-tableB1-B2.R`](https://github.com/CasAndreu/twitter-iran-moderation/blob/main/code/App02-tableB1-B2.R): replicates Tables B1 and B2 in Appendix B, where I report coefficient tables for the main model in Figure 3, as well as five additional model specifications.

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/tableB1.png">

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/tableB2.png">

<br>
<br>

- [`App03-figureB1-B2.R`](https://github.com/CasAndreu/twitter-iran-moderation/blob/main/code/App03-figureB1-B2.R): replicates Figures B1 and B2 in Appendix B, where I show the distribution of count/continuous variables to identify skewed ones to log transform in the regression analyses.

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/figureB1.png">

<img src = "https://github.com/CasAndreu/twitter-iran-moderation/blob/main/figures_png/figureB2.png">
