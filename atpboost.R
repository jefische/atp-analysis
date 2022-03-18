##########################
## Load Packages and Data
##########################
library(tidyverse)
library(lubridate)
library(scales)
library(reshape2)
library(car) #scatterplot matrix function
library(broom) # augment() to extract dataframe from glm object

filea <- 'C:/Users/blue_/Documents/Kaggle/atp_tennis/git_data/atp_matches_'

df_final <- data.frame()
for(i in 2015:2019){
  df_use <- read.csv(file=paste0(filea, i, ".csv"))
  df_final <- rbind(df_final, df_use)
}

########################
# Variable Engineering #
########################

df_final$tourney_date <- as.Date.character(df_final$tourney_date, "%Y%m%d", origin = "2015-01-01")


firstDate <- as.Date("2015-01-04")
matches <- df_final %>% arrange(tourney_date, tourney_id, match_num)
matches <- matches[,c(11,19,5:7,1:4,26,24,25,27,12,14:15,20,22,23,28:46,48)]
matches <- matches %>%
  rowwise() %>%
  mutate(RET = sum(grepl("RET", unlist(str_split(score, " +"))))) %>%
  mutate(score_count = length(unlist(str_split(score, "-|\\s")))) %>%
  mutate(w_final_set = ifelse(RET==1, unlist(str_split(score, "-|\\s"))[score_count-2], NA)) %>%
  mutate(l_final_set = ifelse(RET==1, unlist(str_split(score, "-|\\s"))[score_count-1], NA)) %>%
  mutate(sets_completed = ifelse(RET==1, length(unlist(str_split(score, " +")))-2, 
                              ifelse(score %in% c("W/O", "Walkover"), 0, length(unlist(str_split(score, " +"))))),
                              .after = score) %>%
  mutate(Year = year(tourney_date), .after = match_num)

matches$tourney_index <- 1
k=1
for (i in 2:nrow(matches)){
  if (matches$tourney_id[i]!=matches$tourney_id[i-1]){
    k=k+1
  }
  matches$tourney_index[i]=k
}

#Convert rounds to integers and drop BR, RR (Davis Cup, Tour Finals, Olympics)
matches$round[matches$draw_size %in% c(28,32) & matches$round %in% c("R32")] <-1
matches$round[matches$draw_size %in% c(28,32) & matches$round %in% c("R16")] <-2

matches$round[matches$draw_size %in% c(48,56,64) & matches$round %in% c("R64")] <-1
matches$round[matches$draw_size %in% c(48,56,64) & matches$round %in% c("R32")] <-2
matches$round[matches$draw_size %in% c(48,56,64) & matches$round %in% c("R16")] <-3

matches$round[matches$draw_size %in% c(96,128) & matches$round %in% c("R128")] <-1
matches$round[matches$draw_size %in% c(96,128) & matches$round %in% c("R64")] <-2
matches$round[matches$draw_size %in% c(96,128) & matches$round %in% c("R32")] <-3
matches$round[matches$draw_size %in% c(96,128) & matches$round %in% c("R16")] <-4

matches$round[matches$round== "QF"] <-5
matches$round[matches$round== "SF"] <-6
matches$round[matches$round== "F"] <-7

matches$Wsets <- 0
matches$Wsets[matches$best_of==3 & matches$sets_completed %in% c(2,3)] <- 2
matches$Wsets[matches$best_of==5 & matches$sets_completed %in% c(3,4,5)] <- 3

matches$Lsets <- 0
matches$Lsets[matches$best_of==3 & matches$sets_completed==1] <- 0
matches$Lsets[matches$best_of==3 & matches$sets_completed==2] <- 0
matches$Lsets[matches$best_of==3 & matches$sets_completed==3] <- 1
matches$Lsets[matches$best_of==5 & matches$sets_completed==3] <- 0
matches$Lsets[matches$best_of==5 & matches$sets_completed==4] <- 1
matches$Lsets[matches$best_of==5 & matches$sets_completed==5] <- 2

matches$Wsets[matches$RET==1] <- NA
matches$Lsets[matches$RET==1] <- NA

matches$P_Wsets <- NA
matches$P_Wsets <- matches$Wsets/(matches$Wsets+matches$Lsets)

## Elo calculations ##

playersToElo <- new.env(hash=TRUE)
matchesCount <- new.env(hash=TRUE)


# Run computeElo for elo results in an environment indexed by player names
computeElo <- function() {
  apply(matches,1,updateMatchesCountByRow)
  apply(matches,1,computeEloByRow)
  
  return(playersToElo)
}

### Elo computation details ##################################################################################

computeEloByRow <- function(row) {
  updateElo(playersToElo, row[1], row[2], row[1], row[3],row[4],row[5])
  return(0)
}

updateMatchesCountByRow <- function(row) {
  updateMatchesCount(row[1],row[2])
  return(0)
}

updateMatchesCount <- function (playerA, playerB) {
  if(is.null(matchesCount[[playerA]])) { matchesCount[[playerA]] <- 0 }
  if(is.null(matchesCount[[playerB]])) { matchesCount[[playerB]] <- 0 }
  matchesCount[[playerA]] <- matchesCount[[playerA]]+1
  matchesCount[[playerB]] <- matchesCount[[playerB]]+1
}

updateElo <- function (plToElo, playerA, playerB, winner, level, matchDate,matchNum) {
  rA <- tail(plToElo[[playerA]]$ranking,n=1)
  rB <- tail(plToElo[[playerB]]$ranking,n=1)
  
  if(is.null(rA)) {
    plToElo[[playerA]] <- data.frame(ranking=1500, date=firstDate, num=0)
    rA <- 1500
  }
  if(is.null(rB)) {
    plToElo[[playerB]] <- data.frame(ranking=1500, date=firstDate, num=0)
    rB <- 1500
  }
  
  eA <- 1 / (1 + 10 ^ ((rB - rA)/400))
  eB <- 1 / (1 + 10 ^ ((rA - rB)/400))
  
  if (winner==playerA) {
    sA <- 1
    sB <- 0
  } else {
    sA <- 0
    sB <- 1
  }
  
  kA <- 250/((matchesCount[[playerA]]+5)^0.4)
  kB <- 250/((matchesCount[[playerB]]+5)^0.4)
  k <- ifelse(level == "G", 1.1, 1)
  
  rA_new <- rA + (k*kA) * (sA-eA)
  rB_new <- rB + (k*kB) * (sB-eB)
  
  plToElo[[playerA]] <- rbind(plToElo[[playerA]],data.frame(ranking=rA_new, date=matchDate, num=matchNum))
  plToElo[[playerB]] <- rbind(plToElo[[playerB]],data.frame(ranking=rB_new, date=matchDate, num=matchNum))
}



# Gives the highest elo ratings
summaryPlayers <- function() {
  playersToMax <- data.frame(ranking=1500,meanr=1500,medianr=1500,name="Nobody")
  for (pl in ls(playersToElo)) {
    player <- playersToElo[[pl]]
    ## player <- player[order(player$date,player$num,decreasing=TRUE),]
    ## player <- player[!duplicated(player$date),]
    ## player <- player[order(player$date,player$num,decreasing=FALSE),]
    
    newRow <- data.frame(ranking=max(player$ranking),meanr=mean(player$ranking),medianr=median(player$ranking),name=pl)
    playersToMax <- rbind(playersToMax,newRow)
  }
  
  playersToMax <- playersToMax[order(playersToMax$ranking,decreasing=TRUE),]
  return(playersToMax)
}

computeElo()

length(playersToElo) # 809 players
names(playersToElo) # to view all players

length(union(matches$winner_name, matches$loser_name)) # 809 players
length(unique(c(unique(matches$winner_name), unique(matches$loser_name)))) # 809 players
which(!(names(playersToElo) %in% ID)) # check for differences


matches %>% filter(tourney_level=="G") %>% group_by(tourney_name, tourney_date, tourney_id, tourney_index) %>% summarise(n())

# US Open 2015,   date = 2015-08-31, id = 2015-560, index = 113
# US Open 2016,   date = 2016-08-29, id = 2016-560, index = 252
# Wimbledon 2015, date = 2015-06-29, id = 2015-540, index = 72
# Wimbledon 2016, date = 2016-06-27, id = 2016-540, index = 218

p.tourney.id <- '2016-540'
p.tourney.date <- as.Date('2016-06-27')
p.tourney.index <- 218

earliest = min(matches$tourney_index[matches$tourney_date > '2015-12-31'])
latest = max(matches$tourney_index)

Player_YOY_stats = data.frame()

for(idx in earliest:latest){
  print(idx)
  #Grab players who are in tournament idx
  ID <- unique(c(unique(matches$winner_name[matches$tourney_index == idx]), 
                 unique(matches$loser_name[matches$tourney_index == idx])))
  
  p.tourney.date <- matches$tourney_date[matches$tourney_index==idx][1]
  p.tourney.name <- matches$tourney_name[matches$tourney_index==idx][1]
  
  #Build YOY stats for tournaments prior to idx
  df_YOY <- data.frame()
  c=1
  for(i in ID){
    df1 <- matches[(matches$winner_name==i | matches$loser_name==i) & matches$tourney_index < idx,] 
  
    if(dim(df1)[1] != 0){
      df1 <- df1 %>% 
        summarise(Player=i,
                  Country=sample(c(winner_ioc[winner_name==i], loser_ioc[loser_name==i]),1),
                  tourney_date,
                  Age=floor(mean(c(winner_age[winner_name==i], loser_age[loser_name==i]), na.rm = TRUE)),
                  Rank=mean(c(winner_rank[winner_name==i], loser_rank[loser_name==i]), na.rm = TRUE)) %>%
        arrange(tourney_date)
    
      df1 <- df1[dim(df1)[1],]
      p_stats <- matches[(matches$winner_name==i | matches$loser_name==i) & 
                           matches$tourney_date >= p.tourney.date - 365 & matches$tourney_index < idx,
                         c('winner_name', 'loser_name', 'tourney_date', 'match_num', 'tourney_name', 'sets_completed',
                           'w_ace', 'l_ace', 'Wsets', 'Lsets')]
    
      names(df1)[3] <- "prior_tourney_date"
      df1$tourney_name <- p.tourney.name
      df1$tourney_date <- p.tourney.date
      df1$Days_since <- df1$tourney_date-df1$prior_tourney_date
      df1$tourney_index <- idx
      df1$Matches_Played <- dim(p_stats)[1]
      df1$Matches_Won <- sum(p_stats$winner_name==i)
      df1$Matches_Lost <- sum(p_stats$loser_name==i)
      df1$P_win <- df1$Matches_Won/df1$Matches_Played 
      df1$Sets_Won <- sum(p_stats$Wsets[p_stats$winner_name==i], p_stats$Lsets[p_stats$loser_name==i], na.rm = TRUE)
      df1$Sets_Lost <- sum(p_stats$Wsets, p_stats$Lsets, na.rm = TRUE) - df1$Sets_Won
      df1$P_sets <- df1$Sets_Won/(df1$Sets_Won + df1$Sets_Lost)
      df1$Ace_sets <- sum(p_stats$w_ace[p_stats$winner_name==i], p_stats$l_ace[p_stats$loser_name==i], na.rm = TRUE)/(df1$Sets_Won + df1$Sets_Lost)
      df1$Elo <- tail(playersToElo[[i]]$ranking[playersToElo[[i]]$date <= df1$tourney_date],1)
    
      df_YOY <- rbind(df_YOY, df1)
      c=c+1
    }
    else{
      df1 <- data.frame(Player=i, Country=NA, prior_tourney_date=as.Date(NA), Age=NA, Rank=NA, tourney_name=p.tourney.name,
                        tourney_date=p.tourney.date, Days_since=0, tourney_index=idx, Matches_Played=0, Matches_Won=0, 
                        Matches_Lost=0, P_win=0, Sets_Won=0, Sets_Lost=0, P_sets=0, Ace_sets=0, Elo=1500)
    
      df_YOY <- rbind(df_YOY, df1)
      c=c+1
    }
  }
  Player_YOY_stats <- rbind(Player_YOY_stats, df_YOY)
}

wimbledon_2016 <- matches$tourney_index[matches$tourney_id=='2016-540'][1]
wimbledon_2017 <- matches$tourney_index[matches$tourney_id=='2017-540'][1]
us_open_2017 <- matches$tourney_index[matches$tourney_id=='2017-560'][1]

results <- matches[matches$tourney_index %in% wimbledon_2016:us_open_2017, 
                   c('winner_name', 'loser_name', 'tourney_date', 'tourney_name', 'match_num', 'tourney_id',
                     'tourney_index', 'surface', 'draw_size', 'best_of', 'winner_rank', 'loser_rank')]

modeling_df <- Player_YOY_stats[Player_YOY_stats$tourney_index %in% wimbledon_2016:us_open_2017,]

results_for_modeling <- merge(results, modeling_df, 
                              by.x = c("winner_name", "tourney_index", "tourney_name", "tourney_date"), 
                              by.y = c("Player", "tourney_index", "tourney_name", "tourney_date"), 
                              all.x = TRUE)
names(results_for_modeling)[13:26] <- paste0("W_", names(results_for_modeling)[13:26])

results_for_modeling <- merge(results_for_modeling, modeling_df, 
                              by.x = c("loser_name", "tourney_index", "tourney_name", "tourney_date"), 
                              by.y = c("Player", "tourney_index", "tourney_name", "tourney_date"), 
                              all.x = TRUE)
names(results_for_modeling)[27:40] <- paste0("L_", names(results_for_modeling)[27:40])

results_for_modeling <- results_for_modeling[,c(5,1,4,3,6,7,2,8:15,17:29,31:40)] # Drop W_Rank(16) and L_Rank(30)
results_for_modeling <- results_for_modeling %>% arrange(tourney_index, match_num)


#Randomly assign winners and losers to player 1/2 variables.
set.seed(201608)
r_for_modeling <- results_for_modeling %>%
  mutate(assign = runif(dim(results_for_modeling)[1]),
         Player1 = ifelse(assign <= .5, winner_name, loser_name),
         Player2 = ifelse(assign > .5, winner_name, loser_name),
         P1Wins = ifelse(Player1 == winner_name, 1, 0),
         P1_Rank = ifelse(assign <= .5, winner_rank, loser_rank),
         P2_Rank = ifelse(assign > .5, winner_rank, loser_rank),
         P1_Country = ifelse(assign <= .5, W_Country, L_Country),
         P2_Country = ifelse(assign > .5, W_Country, L_Country),
         P1_Age = ifelse(assign <= .5, W_Age, L_Age),
         P2_Age = ifelse(assign > .5, W_Age, L_Age),
         P1_Matches_Played = ifelse(assign <= .5, W_Matches_Played, L_Matches_Played),
         P2_Matches_Played = ifelse(assign > .5, W_Matches_Played, L_Matches_Played),
         P1_Matches_Won = ifelse(assign <= .5, W_Matches_Won, L_Matches_Won),
         P2_Matches_Won = ifelse(assign > .5, W_Matches_Won, L_Matches_Won),
         P1_Matches_Lost = ifelse(assign <= .5, W_Matches_Lost, L_Matches_Lost),
         P2_Matches_Lost = ifelse(assign > .5, W_Matches_Lost, L_Matches_Lost),
         P1_P_Win = ifelse(assign <= .5, W_P_win, L_P_win),
         P2_P_Win = ifelse(assign > .5, W_P_win, L_P_win),
         P1_Ace_Sets = ifelse(assign <= .5, W_Ace_sets, L_Ace_sets),
         P2_Ace_Sets = ifelse(assign > .5, W_Ace_sets, L_Ace_sets),
         P1_P_Sets = ifelse(assign <= .5, W_P_sets, L_P_sets),
         P2_P_Sets = ifelse(assign > .5, W_P_sets, L_P_sets),
         P1_Elo = ifelse(assign <= .5, W_Elo, L_Elo),
         P2_Elo = ifelse(assign > .5, W_Elo, L_Elo)) %>%
  select(Player1, Player2, P1Wins, tourney_date, tourney_name, tourney_index, match_num, 
         P1_Rank, P2_Rank, P1_Country,  P1_Age, P1_Matches_Played, P1_Matches_Won, 
         P1_Matches_Lost, P1_P_Win, P1_P_Sets, P1_Ace_Sets, P1_Elo, P2_Country,P2_Age,
         P2_Matches_Played, P2_Matches_Won, P2_Matches_Lost, P2_P_Win, P2_P_Sets, P2_Ace_Sets, P2_Elo)

write.csv(r_for_modeling, file='r_for_modeling.csv')

##################
# Modeling glm() #
##################
filea <- 'C:/Users/blue_/Documents/Kaggle/atp_tennis/git_data/r_for_modeling.csv'
r_for_modeling <- read.csv(file=filea)

wimbledon_2016 <- 218
wimbledon_2017 <- 372
us_open_2017 <- 385


train <- r_for_modeling[r_for_modeling$tourney_index %in% wimbledon_2016:(us_open_2017-1),]
test <- r_for_modeling[r_for_modeling$tourney_index==us_open_2017,]

sapply(train, function(x) sum(is.na(x))) # drop records with missing ranking
train <- train[!is.na(train$P1_Rank),]
train <- train[!is.na(train$P2_Rank),]
train <- train[!is.na(train$P1_P_Win),]
train <- train[!is.na(train$P2_P_Win),]
train <- train[!is.na(train$P1_P_Sets),]
train <- train[!is.na(train$P2_P_Sets),]

m1 <- glm(P1Wins ~ P1_Elo + P2_Elo + P1_Rank + P2_Rank + P1_P_Win + P2_P_Win + P1_P_Sets + P2_P_Sets, 
          data=train, family = "binomial")
summary(m1)

p.glm <- predict(m1, newdata=test, type='response')
p.win <- round(p.glm)

table(Predicted=p.win, Actual=test$P1Wins)
sum(diag(table(p.win, test$P1Wins)))/sum(table(p.win, test$P1Wins))

# Elo 79% accuracy
# Elo + ranking 81% accuracy
# Elo + ranking + PWin + PSets 82% accuracy

##################
# Modeling gbm() #
##################
library(gbm) # gbm.perf()
library(caret) # train(), trainControl(), getModelInfo(), resamples()
source('build_tree.R') # converts gbm.object to data.tree for plotting

m1.gbm <- gbm(P1Wins ~ P1_Elo + P2_Elo + P1_Rank + P2_Rank + P1_P_Win + P2_P_Win + P1_P_Sets + P2_P_Sets, 
              data=train, 
              distribution = 'bernoulli',
              n.trees = 3000, 
              interaction.depth = 2,
              shrinkage = 0.05,
              cv.folds = 5)

m1.gbm
summary(m1.gbm) # will not work if P1Wins is a factor, must be an integer for distribution='bernoulli'

#Best num of iterations
best.iter = gbm.perf(m1.gbm, method = 'OOB') #recommends 292
best.iter = gbm.perf(m1.gbm, method = 'cv') #need cv.folds arg >1 in gbm() call for this. recommends 1101

print(pretty.gbm.tree(m1.gbm, i.tree = 1))
plot(build_tree(m1.gbm, i.tree = 1))

# Predictions are on the canonical scale, which for binomial is the log-odds scale. 
# You can use the type="response" when predicting to put it on the probability scale.
p.boost <- predict(m1.gbm, newdata = test, type = 'response')
p.boost <- round(p.boost)

table(Predicted=p.boost, Actual=test$P1Wins)
sum(diag(table(p.boost, test$P1Wins)))/sum(table(p.boost, test$P1Wins)) #81%

#n.trees = 1101, accuracy = 79%

###################################
# Modeling gbm with caret train() #
###################################

getModelInfo()$gbm$parameters
trainctrl <- trainControl(method = 'repeatedcv', number = 5, repeats = 3)
trainctrl <- trainControl(method = 'cv', number = 5, returnResamp = 'all')

#Note for classification caret needs a factor variable
m1.gbm.caret <- train(as.factor(P1Wins) ~ P1_Elo + P2_Elo + P1_Rank + P2_Rank + P1_P_Win + P2_P_Win + P1_P_Sets + P2_P_Sets, 
                      data=train, 
                      method = "gbm", 
                      distribution = "bernoulli",
                      trControl = trainctrl,
                      verbose = FALSE)

m1.gbm.caret
summary(m1.gbm.caret)
p.boost.caret <- predict(m1.gbm.caret, newdata = test)
p.boost.caret

table(Predicted=p.boost.caret, Actual=test$P1Wins)
sum(diag(table(p.boost.caret, test$P1Wins)))/sum(table(p.boost.caret, test$P1Wins)) #80%


myGrid <- expand.grid(n.trees = c(1100),
                      interaction.depth = c(1, 2, 3, 4, 5, 6),
                      shrinkage = c(0.01, 0.10, 0.15),
                      n.minobsinnode = c(10))

m2.gbm.caret <- train(as.factor(P1Wins) ~ P1_Elo + P2_Elo + P1_Rank + P2_Rank + P1_P_Win + P2_P_Win + P1_P_Sets + P2_P_Sets, 
                      data=train, 
                      method = "gbm", 
                      distribution = "bernoulli",
                      trControl = trainctrl,
                      verbose = FALSE, 
                      tuneGrid = myGrid)

m2.gbm.caret
summary(m2.gbm.caret)
p2.boost.caret <- predict(m2.gbm.caret, newdata = test)
p2.boost.caret

table(Predicted=p2.boost.caret, Actual=test$P1Wins)
sum(diag(table(p2.boost.caret, test$P1Wins)))/sum(table(p2.boost.caret, test$P1Wins)) #81%
confusionMatrix(p2.boost.caret, as.factor(test$P1Wins))


###########################
# Modeling randomForest() #
###########################
m1.rf.caret <- train(P1Wins ~ P1_Elo + P2_Elo, data=train, method = "rf",
                     trControl = trainctrl)
m1.rf.caret

summary(resamples(list(gbm = m1.gbm.caret, random_forest = m1.rf.caret)))
















m2.gbm <- gbm(P1Wins~P1_Rank + P2_Rank + P1_P_Win + P2_P_Win + P1_P_Sets + P2_P_Sets + P1_Elo + P2_Elo, 
              data=train, 
              distribution = 'bernoulli', 
              n.trees = 5000, 
              interaction.depth = 2,
              shrinkage = 0.01, 
              cv.folds = 5)




#################



































us_open_2016 <- matches %>% filter(tourney_id=='2016-560') %>%
  select(winner_name, loser_name, tourney_date, match_num, round, score, sets_completed, winner_rank, loser_rank)

sapply(train, function(x) sum(is.na(x))) # 14 heights, 3 L_Perc_win, 3 L_Ace_sets, 1 W_Perc_win, 1 W_Ace_sets missing

#Training set on Wimbledon 2016
train <- merge(wimbledon_2016, df_YOY[,-c(2,4,7)], by.x = "winner_name", by.y = "Player", all.x = TRUE)
names(train)[10:18] <- paste0("W_", names(train)[10:18])
train <- merge(train, df_YOY[,-c(2,4,7)], by.x = "loser_name", by.y = "Player", all.x = TRUE)
names(train)[19:27] <- paste0("L_", names(train)[19:27])
train <- train[,c(2,1,3:27)] %>% arrange(match_num)

#Test on US Open 2016
test <- merge(us_open_2016, df_YOY[,-c(2,4,7)], by.x = "winner_name", by.y = "Player", all.x = TRUE)
names(test)[10:18] <- paste0("W_", names(test)[10:18])
test <- merge(test, df_YOY[,-c(2,4,7)], by.x = "loser_name", by.y = "Player", all.x = TRUE)
names(test)[19:27] <- paste0("L_", names(test)[19:27])
test <- test[,c(2,1,3:27)] %>% arrange(match_num)

sapply(train, function(x) sum(is.na(x))) # 14 heights, 3 L_Perc_win, 3 L_Ace_sets, 1 W_Perc_win, 1 W_Ace_sets missing


#Impute win and ace percentages based on rank for:
# Alessandro Giannessi (243) winner and loser
# Guido Andreozzi (133)
# Christian Harrison (694)

model$L_Perc_win[model$loser_name=='Alessandro Giannessi'] <-
  mean(df_panel_boost$Perc_win[df_panel_boost$Rank >= 233 & df_panel_boost$Rank <= 253], na.rm = TRUE) 

model$W_Perc_win[model$winner_name=='Alessandro Giannessi'] <-
  mean(df_panel_boost$Perc_win[df_panel_boost$Rank >= 233 & df_panel_boost$Rank <= 253], na.rm = TRUE)

model$L_Perc_win[model$loser_name=='Guido Andreozzi'] <-
  mean(df_panel_boost$Perc_win[df_panel_boost$Rank >= 123 & df_panel_boost$Rank <= 143], na.rm = TRUE) 

model$L_Perc_win[model$loser_name=='Christian Harrison'] <-
  mean(df_panel_boost$Perc_win[df_panel_boost$Rank >= 594 & df_panel_boost$Rank <= 794], na.rm = TRUE) 

model$L_Ace_sets[model$loser_name=='Alessandro Giannessi'] <-
  mean(df_panel_boost$Ace_sets[df_panel_boost$Rank >= 233 & df_panel_boost$Rank <= 253], na.rm = TRUE) 

model$W_Ace_sets[model$winner_name=='Alessandro Giannessi'] <-
  mean(df_panel_boost$Ace_sets[df_panel_boost$Rank >= 233 & df_panel_boost$Rank <= 253], na.rm = TRUE) 

model$L_Ace_sets[model$loser_name=='Guido Andreozzi'] <-
  mean(df_panel_boost$Ace_sets[df_panel_boost$Rank >= 123 & df_panel_boost$Rank <= 143], na.rm = TRUE) 

model$L_Ace_sets[model$loser_name=='Christian Harrison'] <-
  mean(df_panel_boost$Ace_sets[df_panel_boost$Rank >= 594 & df_panel_boost$Rank <= 794], na.rm = TRUE) 











