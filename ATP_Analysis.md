ATP Analysis
================
Jeremy Fischer
4/15/2022

Load libraries and 2015-2019 data

``` r
library(tidyverse)
library(lubridate)
library(knitr)
library(gbm)
library(caret)

filea <- './git_data/atp_matches_'

df_final <- data.frame()
for(i in 2015:2019){
  df_use <- read.csv(file=paste0(filea, i, ".csv"))
  df_final <- rbind(df_final, df_use)
}
str(df_final)
```

    ## 'data.frame':    14440 obs. of  49 variables:
    ##  $ tourney_id        : chr  "2015-339" "2015-339" "2015-339" "2015-339" ...
    ##  $ tourney_name      : chr  "Brisbane" "Brisbane" "Brisbane" "Brisbane" ...
    ##  $ surface           : chr  "Hard" "Hard" "Hard" "Hard" ...
    ##  $ draw_size         : int  28 28 28 28 28 28 28 28 28 28 ...
    ##  $ tourney_level     : chr  "A" "A" "A" "A" ...
    ##  $ tourney_date      : int  20150104 20150104 20150104 20150104 20150104 20150104 20150104 20150104 20150104 20150104 ...
    ##  $ match_num         : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ winner_id         : int  105357 103813 105902 104871 105373 105238 103997 105032 105062 106423 ...
    ##  $ winner_seed       : int  NA NA NA NA NA 7 NA NA NA NA ...
    ##  $ winner_entry      : chr  "WC" "" "WC" "" ...
    ##  $ winner_name       : chr  "John Millman" "Jarkko Nieminen" "James Duckworth" "Jeremy Chardy" ...
    ##  $ winner_hand       : chr  "R" "L" "R" "R" ...
    ##  $ winner_ht         : num  183 185 183 188 190 180 190 193 183 196 ...
    ##  $ winner_ioc        : chr  "AUS" "FIN" "AUS" "FRA" ...
    ##  $ winner_age        : num  25.6 33.5 23 27.9 25.5 ...
    ##  $ loser_id          : int  105733 106045 104468 104979 103781 104122 104731 103720 105657 103898 ...
    ##  $ loser_seed        : int  NA NA 6 NA NA NA 5 NA NA 8 ...
    ##  $ loser_entry       : chr  "Q" "Q" "" "" ...
    ##  $ loser_name        : chr  "Rhyne Williams" "Denis Kudla" "Gilles Simon" "Andrey Golubev" ...
    ##  $ loser_hand        : chr  "R" "R" "R" "R" ...
    ##  $ loser_ht          : num  NA 180 183 185 183 183 203 180 193 185 ...
    ##  $ loser_ioc         : chr  "USA" "USA" "FRA" "KAZ" ...
    ##  $ loser_age         : num  23.8 22.4 30 27.5 33.6 ...
    ##  $ score             : chr  "6-3 6-1" "4-6 6-1 6-4" "6-2 6-2" "6-4 6-4" ...
    ##  $ best_of           : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ round             : chr  "R32" "R32" "R32" "R32" ...
    ##  $ minutes           : int  65 104 68 69 144 78 110 59 113 75 ...
    ##  $ w_ace             : int  6 4 4 7 9 6 6 14 3 9 ...
    ##  $ w_df              : int  2 0 0 1 4 1 5 0 1 1 ...
    ##  $ w_svpt            : int  44 92 45 53 130 49 78 47 84 52 ...
    ##  $ w_1stIn           : int  24 59 27 39 79 31 51 34 56 30 ...
    ##  $ w_1stWon          : int  19 39 20 31 55 24 35 30 42 25 ...
    ##  $ w_2ndWon          : int  14 17 11 11 27 12 14 8 16 14 ...
    ##  $ w_SvGms           : int  8 14 8 10 16 9 11 9 15 10 ...
    ##  $ w_bpSaved         : int  1 4 2 0 6 3 3 2 1 1 ...
    ##  $ w_bpFaced         : int  1 7 3 0 8 4 5 2 3 2 ...
    ##  $ l_ace             : int  3 6 2 9 4 3 12 1 17 5 ...
    ##  $ l_df              : int  4 1 1 2 4 2 3 2 2 3 ...
    ##  $ l_svpt            : int  50 83 56 57 95 78 76 45 89 52 ...
    ##  $ l_1stIn           : int  31 50 37 38 62 53 43 22 56 33 ...
    ##  $ l_1stWon          : int  20 26 22 30 40 32 30 13 46 23 ...
    ##  $ l_2ndWon          : int  5 19 5 8 19 8 17 14 15 7 ...
    ##  $ l_SvGms           : int  8 13 8 10 15 8 11 8 15 9 ...
    ##  $ l_bpSaved         : int  1 3 10 1 4 11 4 1 3 1 ...
    ##  $ l_bpFaced         : int  5 8 15 3 8 15 7 4 6 4 ...
    ##  $ winner_rank       : int  153 73 125 31 34 23 177 85 69 149 ...
    ##  $ winner_rank_points: int  328 689 430 1195 1094 1455 282 586 705 341 ...
    ##  $ loser_rank        : int  220 123 21 72 110 71 16 84 201 25 ...
    ##  $ loser_rank_points : int  221 440 1730 691 505 700 2080 595 242 1365 ...

## Data manipulation and variable engineering

Convert dates to date objects, and calculate sets completed per match
using the score variable. Also reorder variables to run ELO ratings
code.

Calculate games won per match for the winner and loser using the score
variable.

Drop 1,225 Davis Cup matches as these do not affect ranking and are team
based tournaments (8% of all matches).

Drop 16 ATP Next Gen Finals matches as these use a different scoring
system. (0.1% of all matches).

Drop 465 (3.2% of all matches) walkover, in progress, retired, and
defaulted matches.

Create a tournament index variable to easily identify tournaments across
the 5 years.

Calculate sets won per match for both the winner and loser.

Create ELO ratings

Here is a result of the top 20 ELO ratings ending in 2019

Next I’ll create a rolling year over year player statistical summary
starting with calendar year 2015.

Merge match results from Doha 2016 through the 2019 Tour Finals with
player year over year stats.

Next randomly assign winners to Player 1 and Player 2 variables and
split the data into training and testing sets with 75% of tournaments in
the training set and the remaining 25% in the testing set.

## Modeling

Start by fitting a logistic regression model and evaluating prediction
accuracy on the test set.

``` r
earliest=67
latest=333
r_for_modeling <- read.csv(file='./git_data/r_for_modeling.csv')
train <- r_for_modeling[r_for_modeling$tourney_index %in% earliest:(earliest+200),]
test <- r_for_modeling[r_for_modeling$tourney_index %in% (earliest+201):latest,]
```

``` r
m1 <- glm(P1Wins ~ P1_Elo + P2_Elo + P1_Rank + P2_Rank + P1_P_Win + P2_P_Win + 
            P1_P_Sets + P2_P_Sets + P1_Ace_Sets + P2_Ace_Sets, 
          data=train, family = "binomial")

p.glm <- predict(m1, newdata=test, type='response')
p.win <- round(p.glm)

table(Predicted=p.win, Actual=test$P1Wins)
```

    ##          Actual
    ## Predicted   0   1
    ##         0 969 308
    ##         1 276 922

``` r
sum(diag(table(p.win, test$P1Wins)))/sum(table(p.win, test$P1Wins))
```

    ## [1] 0.7640404

Here we get an accuracy of 76.4%

Next evaluate performance from gbm()

``` r
#case-wise deletion from glm()
drop <- as.integer(m1$na.action) #116 records
train_NA <- train[-drop,]

set.seed(2016)
m1.gbm <- gbm(P1Wins ~ P1_Elo + P2_Elo + P1_Rank + P2_Rank + P1_P_Win + P2_P_Win + P1_P_Sets + P2_P_Sets +
                P1_Ace_Sets + P2_Ace_Sets, 
              data=train_NA, 
              distribution = 'bernoulli',
              n.trees = 3000, 
              interaction.depth = 2,
              shrinkage = 0.05,
              cv.folds = 5,
              verbose = FALSE)

best.iter = gbm.perf(m1.gbm, method = 'cv')
```

![](ATP_Analysis_files/figure-gfm/gbm-1.png)<!-- -->

``` r
best.iter
```

    ## [1] 1073

``` r
m1.gbm
```

    ## gbm(formula = P1Wins ~ P1_Elo + P2_Elo + P1_Rank + P2_Rank + 
    ##     P1_P_Win + P2_P_Win + P1_P_Sets + P2_P_Sets + P1_Ace_Sets + 
    ##     P2_Ace_Sets, distribution = "bernoulli", data = train_NA, 
    ##     n.trees = 3000, interaction.depth = 2, shrinkage = 0.05, 
    ##     cv.folds = 5, verbose = FALSE)
    ## A gradient boosted model with bernoulli loss function.
    ## 3000 iterations were performed.
    ## The best cross-validation iteration was 1073.
    ## There were 10 predictors of which 10 had non-zero influence.

``` r
summary(m1.gbm)
```

![](ATP_Analysis_files/figure-gfm/gbm-2.png)<!-- -->

    ##                     var   rel.inf
    ## P2_Elo           P2_Elo 29.041891
    ## P1_Elo           P1_Elo 28.126808
    ## P2_P_Win       P2_P_Win  7.677845
    ## P1_P_Win       P1_P_Win  7.233177
    ## P1_Ace_Sets P1_Ace_Sets  5.993770
    ## P2_Ace_Sets P2_Ace_Sets  5.288316
    ## P2_P_Sets     P2_P_Sets  4.997314
    ## P2_Rank         P2_Rank  4.118283
    ## P1_Rank         P1_Rank  4.067302
    ## P1_P_Sets     P1_P_Sets  3.455294

``` r
p.boost <- predict(m1.gbm, newdata = test, type = 'response', n.trees = best.iter)
p.boost <- round(p.boost)
table(Predicted=p.boost, Actual=test$P1Wins)
```

    ##          Actual
    ## Predicted    0    1
    ##         0 1010  272
    ##         1  263  985

``` r
sum(diag(table(p.boost, test$P1Wins)))/sum(table(p.boost, test$P1Wins))
```

    ## [1] 0.7885375

Our accuracy is improved to 78.8% with the best number of trees at 1073.
We can try to improve the algorithm by providing a grid search for the
learning rate, interaction depth, and number of trees.

``` r
myGrid <- expand.grid(n.trees = c(150, 1200, 2000),
                      interaction.depth = c(1, 2, 3, 4, 5, 6),
                      shrinkage = c(0.01, 0.10, 0.15),
                      n.minobsinnode = c(10))
trainctrl <- trainControl(method = 'cv', number = 5, returnResamp = 'all')

set.seed(2016)
m2.gbm.caret <- train(as.factor(P1Wins) ~ P1_Elo + P2_Elo + P1_Rank + P2_Rank + P1_P_Win + P2_P_Win + P1_P_Sets +
                        P2_P_Sets + P1_Ace_Sets + P2_Ace_Sets, 
                      data=train_NA, 
                      method = "gbm", 
                      distribution = "bernoulli",
                      trControl = trainctrl,
                      verbose = FALSE, 
                      tuneGrid = myGrid)

# With caret gbm object, predict will perform case-wise deletion on test set.
# Thus to evaluate performance need to drop NA records
test_p <- na.omit(test %>% select(P1Wins, P1_Elo, P2_Elo, P1_Rank, P2_Rank, P1_P_Win, P2_P_Win, P1_P_Sets, P2_P_Sets, P1_Ace_Sets, P2_Ace_Sets))

# predict in this case creates a factor variable for some reason
p.boost.caret <- predict(m2.gbm.caret, newdata = test_p, type = 'raw')

table(Predicted=p.boost.caret, Actual=test_p$P1Wins)
sum(diag(table(p.boost.caret, test_p$P1Wins)))/sum(table(p.boost.caret, test_p$P1Wins))
```
