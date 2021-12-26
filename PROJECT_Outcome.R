library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(e1071)
library(psych)
library(corrplot)
library(corrgram)
library(pROC)
library(leaps)
library(readr)
library(nnet)
library(randomForest)
library(MLmetrics)


#Build the dataframe----
season0 <- read.csv("D1 (14).csv", stringsAsFactors = T)
season1 <- read.csv("D1 (12).csv", stringsAsFactors = T)
season2 <- read.csv("D1 (11).csv", stringsAsFactors = T)
season3 <- read.csv("D1 (10).csv", stringsAsFactors = T)
season4 <- read.csv("D1 (9).csv", stringsAsFactors = T)
season5 <- read.csv("D1 (8).csv", stringsAsFactors = T)
season6 <- read.csv("D1 (7).csv", stringsAsFactors = T)

cols_1 <- c("Date", "HomeTeam", "AwayTeam",	"FTHG",	"FTAG",	"FTR"	,"HS"	,"AS"	,"HST","AST","HF","AF","HC","AC","HY","AY","HR","AR","BbAvH","BbAvD","BbAvA")
cols_2 <- c("Date", "HomeTeam", "AwayTeam",	"FTHG",	"FTAG",	"FTR"	,"HS"	,"AS"	,"HST","AST","HF","AF","HC","AC","HY","AY","HR","AR","AvgH","AvgD","AvgA")

season0 <- season0[cols_1]
season1 <- season1[cols_1]
season2 <- season2[cols_1]
season3 <- season3[cols_1]
season4 <- season4[cols_1]
season5 <- season5[cols_2]
season6 <- season6[cols_2]

season0 <- rename(season0, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA)
season1 <- rename(season1, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA)
season2 <- rename(season2, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA)
season3 <- rename(season3, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA)
season4 <- rename(season4, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA)
season5 <- rename(season5, H_Odds = AvgH, D_Odds = AvgD, A_Odds = AvgA)
season6 <- rename(season6, H_Odds = AvgH, D_Odds = AvgD, A_Odds = AvgA)

df <- rbind(season0, season1, season2, season3, season4, season5, season6)
rm(season0, season1, season2, season3, season4, season5, season6)

sum(is.na(df))

df$Date <- as.Date(df$Date, "%d/%m/%y")
df$goals <- df$FTHG + df$FTAG

#Build functions to compute new variables-------
#create a function that takes a teams last 6/total games and sums up the stats
stat_function <- function(df, team, date) {
  last_6 <- tail(df[(df$HomeTeam == team | df$AwayTeam == team) & (df$Date < date),])
  past <- df[(df$HomeTeam == team | df$AwayTeam == team) & (df$Date < date),]
  point_count <- 0
  scored_count <- 0
  conceded_count <- 0
  shots_count <- 0
  shots_on_target_count <- 0
  corners_count <- 0
  fouls_count <- 0
  yellow_card_count <- 0
  red_card_count <- 0
  point_count_6 <- 0
  scored_count_6 <- 0
  conceded_count_6 <- 0
  shots_count_6 <- 0
  shots_on_target_count_6 <- 0
  corners_count_6 <- 0
  fouls_count_6 <- 0
  yellow_card_count_6 <- 0
  red_card_count_6 <- 0
  if(nrow(last_6) < 3){
    
  }
  else {
    for (match in 1:nrow(last_6)){
      m <- last_6[match,]
      if (m$HomeTeam == team){
        scored_count_6 <- scored_count_6 + m$FTHG
        conceded_count_6 <- conceded_count_6 + m$FTAG
        shots_count_6 <- shots_count_6 + m$HS
        shots_on_target_count_6 <- shots_on_target_count_6 + m$HST
        corners_count_6 <- corners_count_6 + m$HC
        fouls_count_6 <- fouls_count_6 + m$HF
        yellow_card_count_6 <- yellow_card_count_6 + m$HY*10
        red_card_count_6 <- red_card_count_6 + m$HR*25
        if (m$FTR == "H"){
          point_count_6 <- point_count_6 + 3
        }
        else if (m$FTR == "D"){
          point_count_6 <- point_count_6 +1
        }
      }
      else {
        scored_count_6 <- scored_count_6 + m$FTAG
        conceded_count_6 <- conceded_count_6 + m$FTHG
        shots_count_6 <- shots_count_6 + m$AS
        shots_on_target_count_6 <- shots_on_target_count_6 + m$AST
        corners_count_6 <- corners_count_6 + m$AC
        fouls_count_6 <- fouls_count_6 + m$AF
        yellow_card_count_6 <- yellow_card_count_6 + m$AY*10
        red_card_count_6 <- red_card_count_6 + m$AR*25
        if (m$FTR == "A"){
          point_count_6 <- point_count_6 + 3
        }
        else if (m$FTR == "D"){
          point_count_6 <- point_count_6 +1
        }
      }
    }
  }

  if(nrow(past) < 3){
    
  }
  else {
    for (match in 1:nrow(past)){
      m <- past[match,]
      if (m$HomeTeam == team){
        scored_count <- scored_count + m$FTHG
        conceded_count <- conceded_count + m$FTAG
        shots_count <- shots_count + m$HS
        shots_on_target_count <- shots_on_target_count + m$HST
        corners_count <- corners_count + m$HC
        fouls_count <- fouls_count + m$HF
        yellow_card_count <- yellow_card_count + m$HY*10
        red_card_count <- red_card_count + m$HR*25
        if (m$FTR == "H"){
          point_count <- point_count + 3
        }
        else if (m$FTR == "D"){
          point_count <- point_count +1
        }
      }
      else {
        scored_count <- scored_count + m$FTAG
        conceded_count <- conceded_count + m$FTHG
        shots_count <- shots_count + m$AS
        shots_on_target_count <- shots_on_target_count + m$AST
        corners_count <- corners_count + m$AC
        fouls_count <- fouls_count + m$AF
        yellow_card_count <- yellow_card_count + m$AY*10
        red_card_count <- red_card_count + m$AR*25
        if (m$FTR == "A"){
          point_count <- point_count + 3
        }
        else if (m$FTR == "D"){
          point_count <- point_count +1
        }
      }
    }
  }
  games_count <- nrow(past)
  points <- point_count/games_count
  mean_scored <- scored_count/games_count
  mean_conceded <- conceded_count/games_count
  mean_shots <- shots_count/games_count
  mean_shots_on_target <- shots_on_target_count/games_count
  mean_corners <- corners_count/games_count
  mean_fouls <- fouls_count/games_count
  card_points <- (yellow_card_count + red_card_count)/games_count
  card_points_6 <- yellow_card_count_6 + red_card_count_6
  
  result <<- data.frame("point_form" = point_count_6,
                        "scored_form" = scored_count_6,
                        "conceded_form" = conceded_count_6,
                        "shots_form" = shots_count_6,
                        "shots_on_target_form" = shots_on_target_count_6,
                        "corners_form" = corners_count_6,
                        "fouls_form" = fouls_count_6,
                        "card_points_form" = card_points_6,
                        "mean_points" <- points,
                        "mean_scored" = mean_scored,
                        "mean_conceded" = mean_conceded,
                        "mean_shots" = mean_shots,
                        "mean_shots_on_target" = mean_shots_on_target,
                        "mean_corners" = mean_corners,
                        "mean_fouls" = mean_fouls,
                        "card_points" = card_points
                        )
  
  result
}


#create a function that takes a teams last 6/total games vs Opponent and sums up the stats
stat_function_opp <- function(df, HT, AT, date){
  last_6 <- tail(df[((df$HomeTeam == HT & df$AwayTeam == AT) | (df$HomeTeam == AT & df$AwayTeam == HT)) & (df$Date < date),])
  past <-df[((df$HomeTeam == HT & df$AwayTeam == AT) | (df$HomeTeam == AT & df$AwayTeam == HT)) & (df$Date < date),]
  point_count <- 0
  scored_count <- 0
  conceded_count <- 0
  shots_count <- 0
  shots_on_target_count <- 0
  corners_count <- 0
  fouls_count <- 0
  yellow_card_count <- 0
  red_card_count <- 0
  point_count_6 <- 0
  scored_count_6 <- 0
  conceded_count_6 <- 0
  shots_count_6 <- 0
  shots_on_target_count_6 <- 0
  corners_count_6 <- 0
  fouls_count_6 <- 0
  yellow_card_count_6 <- 0
  red_card_count_6 <- 0
  if(nrow(last_6) == 0){
   
  }
  else{
    for (match in 1:nrow(last_6)){
      m <- last_6[match,]
      if (m$HomeTeam == HT){
        scored_count_6 <- scored_count_6 + m$FTHG
        conceded_count_6 <- conceded_count_6 + m$FTAG
        shots_count_6 <- shots_count_6 + m$HS
        shots_on_target_count_6 <- shots_on_target_count_6 + m$HST
        corners_count_6 <- corners_count_6 + m$HC
        fouls_count_6 <- fouls_count_6 + m$HF
        yellow_card_count_6 <- yellow_card_count_6 + m$HY*10
        red_card_count_6 <- red_card_count_6 + m$HR*25
        if (m$FTR == "H"){
          point_count_6 <- point_count_6 + 3
        }
        else if (m$FTR == "D"){
          point_count_6 <- point_count_6 +1
        }
      }
      else {
        scored_count_6 <- scored_count_6 + m$FTAG
        conceded_count_6 <- conceded_count_6 + m$FTHG
        shots_count_6 <- shots_count_6 + m$AS
        shots_on_target_count_6 <- shots_on_target_count_6 + m$AST
        corners_count_6 <- corners_count_6 + m$AC
        fouls_count_6 <- fouls_count_6 + m$AF
        yellow_card_count_6 <- yellow_card_count_6 + m$AY*10
        red_card_count_6 <- red_card_count_6 + m$AR*25
        if (m$FTR == "A"){
          point_count_6 <- point_count_6 + 3
        }
        else if (m$FTR == "D"){
          point_count_6 <- point_count_6 +1
        }
      }
    }
  }
  if(nrow(past) == 0){
   
  }
  else{
    for (match in 1:nrow(past)){
      m <- past[match,]
      if (m$HomeTeam == HT){
        scored_count <- scored_count + m$FTHG
        conceded_count <- conceded_count + m$FTAG
        shots_count <- shots_count + m$HS
        shots_on_target_count <- shots_on_target_count + m$HST
        corners_count <- corners_count + m$HC
        fouls_count <- fouls_count + m$HF
        yellow_card_count <- yellow_card_count + m$HY*10
        red_card_count <- red_card_count + m$HR*25
        if (m$FTR == "H"){
          point_count <- point_count + 3
        }
        else if (m$FTR == "D"){
          point_count <- point_count +1
        }
      }
      else {
        scored_count <- scored_count + m$FTAG
        conceded_count <- conceded_count + m$FTHG
        shots_count <- shots_count + m$AS
        shots_on_target_count <- shots_on_target_count + m$AST
        corners_count <- corners_count + m$AC
        fouls_count <- fouls_count + m$AF
        yellow_card_count <- yellow_card_count + m$AY*10
        red_card_count <- red_card_count + m$AR*25
        if (m$FTR == "A"){
          point_count <- point_count + 3
        }
        else if (m$FTR == "D"){
          point_count <- point_count +1
        }
      }
    }
  }
  games_count <- nrow(past)
  points <- point_count/games_count
  mean_scored <- scored_count/games_count
  mean_conceded <- conceded_count/games_count
  mean_shots <- shots_count/games_count
  mean_shots_on_target <- shots_on_target_count/games_count
  mean_corners <- corners_count/games_count
  mean_fouls <- fouls_count/games_count
  card_points <- (yellow_card_count + red_card_count)/games_count
  card_points_6 <- yellow_card_count_6 + red_card_count_6
  
  
  result <<- data.frame("point_form_opp" = point_count_6,
                        "scored_form_opp" = scored_count_6,
                        "conceded_form_opp" = conceded_count_6,
                        "shots_form_opp" = shots_count_6,
                        "shots_on_target_form_opp" = shots_on_target_count_6,
                        "corners_form_opp" = corners_count_6,
                        "fouls_form_opp" = fouls_count_6,
                        "card_points_form_opp" = card_points_6,
                        "mean_poins_opp" <- points,
                        "mean_scored_opp" = mean_scored,
                        "mean_conceded_opp" = mean_conceded,
                        "mean_shots_opp" = mean_shots,
                        "mean_shots_on_target_opp" = mean_shots_on_target,
                        "mean_corners_opp" = mean_corners,
                        "mean_fouls_opp" = mean_fouls,
                        "card_points_opp" = card_points
  )
  
  result
}


#Apply functions to dataframe----
#Create cols for the new variables that will be computed by the functions
#Distinguish between: HT and AT, Last 6 games and total past games, all games and against specific opponent

df$HT_Form_Points <- NA
df$HT_Form_Scored <- NA
df$HT_Form_Conceded <- NA
df$HT_shots_form <- NA
df$HT_shots_on_target_form <- NA
df$HT_corners_form <- NA
df$HT_fouls_form <- NA
df$HT_card_points_form <- NA

df$HT_Mean_Points <- NA
df$HT_Mean_Scored <- NA
df$HT_Mean_Conceded <- NA
df$HT_Mean_shots <- NA
df$HT_Mean_shots_on_target <- NA
df$HT_Mean_corners <- NA
df$HT_Mean_fouls<- NA
df$HT_Mean_card_points <- NA

df$AT_Form_Points <- NA
df$AT_Form_Scored <- NA
df$AT_Form_Conceded <- NA
df$AT_shots_form <- NA
df$AT_shots_on_target_form <- NA
df$AT_corners_form <- NA
df$AT_fouls_form <- NA
df$AT_card_points_form <- NA

df$AT_Mean_Points <- NA
df$AT_Mean_Scored <- NA
df$AT_Mean_Conceded <- NA
df$AT_Mean_shots <- NA
df$AT_Mean_shots_on_target <- NA
df$AT_Mean_corners <- NA
df$AT_Mean_fouls <- NA
df$AT_Mean_card_points <- NA

df$HT_Form_Points_opp <- NA
df$HT_Form_Scored_opp <- NA
df$HT_Form_Conceded_opp <- NA
df$HT_shots_form_opp <- NA
df$HT_shots_on_target_form_opp <- NA
df$HT_corners_form_opp <- NA
df$HT_fouls_form_opp <- NA
df$HT_card_points_form_opp <- NA

df$HT_Mean_Points_opp <- NA
df$HT_Mean_Scored_opp <- NA
df$HT_Mean_Conceded_opp <- NA
df$HT_Mean_shots_opp <- NA
df$HT_Mean_shots_on_target_opp <- NA
df$HT_Mean_corners_opp <- NA
df$HT_Mean_fouls_opp <- NA
df$HT_Mean_card_points_opp <- NA

df$AT_Form_Points_opp <- NA
df$AT_Form_Scored_opp <- NA
df$AT_Form_Conceded_opp <- NA
df$AT_shots_form_opp <- NA
df$AT_shots_on_target_form_opp <- NA
df$AT_corners_form_opp <- NA
df$AT_fouls_form_opp <- NA
df$AT_card_points_form_opp <- NA

df$AT_Mean_Points_opp <- NA
df$AT_Mean_Scored_opp <- NA
df$AT_Mean_Conceded_opp <- NA
df$AT_Mean_shots_opp <- NA
df$AT_Mean_shots_on_target_opp <- NA
df$AT_Mean_corners_opp <- NA
df$AT_Mean_fouls_opp <- NA
df$AT_Mean_card_points_opp <- NA


#apply the functions on every match of the df
for (i in 1:nrow(df)){
  stat_home <- stat_function(df, df[i,]$HomeTeam, df[i,]$Date)
  df[i,]$HT_Form_Points <- stat_home[1]
  df[i,]$HT_Form_Scored <- stat_home[2]
  df[i,]$HT_Form_Conceded <- stat_home[3]
  df[i,]$HT_shots_form <- stat_home[4]
  df[i,]$HT_shots_on_target_form <- stat_home[5]
  df[i,]$HT_corners_form <- stat_home[6]
  df[i,]$HT_fouls_form <- stat_home[7]
  df[i,]$HT_card_points_form <- stat_home[8]

  df[i,]$HT_Mean_Points <- stat_home[9]
  df[i,]$HT_Mean_Scored <- stat_home[10]
  df[i,]$HT_Mean_Conceded <- stat_home[11]
  df[i,]$HT_Mean_shots <- stat_home[12]
  df[i,]$HT_Mean_shots_on_target <- stat_home[13]
  df[i,]$HT_Mean_corners <- stat_home[14]
  df[i,]$HT_Mean_fouls<- stat_home[15]
  df[i,]$HT_Mean_card_points <- stat_home[16]

  stat_away <- stat_function(df, df[i,]$AwayTeam, df[i,]$Date)
  df[i,]$AT_Form_Points <- stat_away[1]
  df[i,]$AT_Form_Scored <- stat_away[2]
  df[i,]$AT_Form_Conceded <- stat_away[3]
  df[i,]$AT_shots_form <- stat_away[4]
  df[i,]$AT_shots_on_target_form <- stat_away[5]
  df[i,]$AT_corners_form <- stat_away[6]
  df[i,]$AT_fouls_form <- stat_away[7]
  df[i,]$AT_card_points_form <- stat_away[8]

  df[i,]$AT_Mean_Points <- stat_away[9]
  df[i,]$AT_Mean_Scored <- stat_away[10]
  df[i,]$AT_Mean_Conceded <- stat_away[11]
  df[i,]$AT_Mean_shots <- stat_away[12]
  df[i,]$AT_Mean_shots_on_target <- stat_away[13]
  df[i,]$AT_Mean_corners <- stat_away[14]
  df[i,]$AT_Mean_fouls<- stat_away[15]
  df[i,]$AT_Mean_card_points <- stat_away[16]
  
  stat_home_opp <- stat_function_opp(df, df[i,]$HomeTeam, df[i,]$AwayTeam, df[i,]$Date)
  df[i,]$HT_Form_Points_opp <- stat_home_opp[1]
  df[i,]$HT_Form_Scored_opp <- stat_home_opp[2]
  df[i,]$HT_Form_Conceded_opp <- stat_home_opp[3]
  df[i,]$HT_shots_form_opp <- stat_home_opp[4]
  df[i,]$HT_shots_on_target_form_opp <- stat_home_opp[5]
  df[i,]$HT_corners_form_opp <- stat_home_opp[6]
  df[i,]$HT_fouls_form_opp <- stat_home_opp[7]
  df[i,]$HT_card_points_form_opp <- stat_home_opp[8]
  
  df[i,]$HT_Mean_Points_opp <- stat_home_opp[9]
  df[i,]$HT_Mean_Scored_opp <- stat_home_opp[10]
  df[i,]$HT_Mean_Conceded_opp <- stat_home_opp[11]
  df[i,]$HT_Mean_shots_opp <- stat_home_opp[12]
  df[i,]$HT_Mean_shots_on_target_opp <- stat_home_opp[13]
  df[i,]$HT_Mean_corners_opp <- stat_home_opp[14]
  df[i,]$HT_Mean_fouls_opp<- stat_home_opp[15]
  df[i,]$HT_Mean_card_points_opp <- stat_home_opp[16]
  
  stat_away_opp <- stat_function_opp(df, df[i,]$AwayTeam, df[i,]$HomeTeam, df[i,]$Date)
  df[i,]$AT_Form_Points_opp <- stat_away_opp[1]
  df[i,]$AT_Form_Scored_opp <- stat_away_opp[2]
  df[i,]$AT_Form_Conceded_opp <- stat_away_opp[3]
  df[i,]$AT_shots_form_opp <- stat_away_opp[4]
  df[i,]$AT_shots_on_target_form_opp <- stat_away_opp[5]
  df[i,]$AT_corners_form_opp <- stat_away_opp[6]
  df[i,]$AT_fouls_form_opp <- stat_away_opp[7]
  df[i,]$AT_card_points_form_opp <- stat_away_opp[8]
  
  df[i,]$AT_Mean_Points_opp <- stat_away_opp[9]
  df[i,]$AT_Mean_Scored_opp <- stat_away_opp[10]
  df[i,]$AT_Mean_Conceded_opp <- stat_away_opp[11]
  df[i,]$AT_Mean_shots_opp <- stat_away_opp[12]
  df[i,]$AT_Mean_shots_on_target_opp <- stat_away_opp[13]
  df[i,]$AT_Mean_corners_opp <- stat_away_opp[14]
  df[i,]$AT_Mean_fouls_opp<- stat_away_opp[15]
  df[i,]$AT_Mean_card_points_opp <- stat_away_opp[16]
}

#Convert the dataypes of the columns from list to numeric
d <- df

d[,23:86] <- lapply(d[,23:86], unlist)
d[,23:86] <- lapply(d[,23:86], as.numeric)

#Remove NaN-rows in the data
sum(is.na(d))
d <- na.omit(d)
str(d)

#Round the values
d <- d %>% mutate_if(is.numeric, ~round(.,2))

#Remove unneeded df's
rm(result, stat_away, stat_away_opp, stat_home, stat_home_opp)

###Model Building-------
d_odds <- d

#Remove columns that we don't need for the model
drop <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "HS", "AS", "HST", "AST", "HF", "AF", "HC", "AC", "HY", "AY", "HR", "AR", "H_Odds", "D_Odds", "A_Odds", "goals", "Is_over_2.5")
d <- d[,!(names(d) %in% drop)]

#Feature Selection----

#identify correlated predictors
cor_mat <- cor(subset(d, select = -FTR))
highlyCorrelated <- findCorrelation(cor_mat, cutoff=0.75)

highlyCorrelated <- highlyCorrelated[!highlyCorrelated %in% 1]
names(df[,highlyCorrelated])

d <- d[,-highlyCorrelated]

#Recursive Feature Elimination
set.seed(101)
rfeC <- rfeControl(functions = rfFuncs, method="cv", number=10)

results <- rfe(d[,2:37], d[,1], sizes=(1:18), rfeControl=rfeC)

print(results)
predictors(results)
plot(results, type=c("g", "o"))

p <- head(results$optVariables, 11)

p <- c("FTR", p)

d <- d[,p]

#Descriptive statistics of our variables
names(d)
plot(d$FTR)
summary(d)


train <- head(d, n=1558)
test <- tail(d,n=306)


#Train models----

#Setup control function to make rolling forecasts for every matchday (9 games) based on the whole data before the respective matchday

ctrl <- trainControl(method = "timeslice",
                     initialWindow = 306,
                     horizon = 9,
                     fixedWindow = FALSE,
                     skip = 9,
                     classProbs = TRUE,
                     savePredictions = TRUE,
                     summaryFunction = multiClassSummary,
                     allowParallel = TRUE)


#K-nearest Neighbors
d_kknn <- train(FTR~., data=train, method="kknn", metric="ROC", 
                trControl=ctrl, preProcess = c("center", "scale") )
print(d_kknn)

#Penalized Discriminant Analysis
d_pda <- train(FTR~., data=train, method="pda", metric="ROC", 
               trControl=ctrl, preProcess = c("center", "scale") )
print(d_pda)

#Shrinkage Discriminant Analysis
d_sda <- train(FTR~., data=train, method="sda", metric="ROC", 
               trControl=ctrl, preProcess = c("center", "scale") )
print(d_sda)


#High Dimensional Discriminant Analysis
d_hdda <- train(FTR~., data=train, method="hdda", metric="ROC", 
                trControl=ctrl, preProcess = c("center", "scale") )
print(d_hdda)

#Single C5.0 Tree
d_C5 <- train(FTR~., data=train, method="C5.0Tree", metric="ROC", 
              trControl=ctrl, preProcess = c("center", "scale") )
print(d_C5)


resample_results <- resamples(list(KKNN=d_kknn,PDA=d_pda,SDA=d_sda, 
                                   HDDA=d_hdda, C5TREE=d_C5))

summary(resample_results,metric = "Accuracy")

bwplot(resample_results , metric = "Accuracy")
bwplot(resample_results , metric = "Mean_Specificity")
bwplot(resample_results , metric = "Mean_Sensitivity")


#Best model is the Shrinkage Discriminant Analysis with a mean accuracy of 51%

conf_Matrix <- confusionMatrix(d_sda$pred$obs, d_sda$pred$pred)
conf_Matrix
#Good H prediction, Bad D predictions, Bad A predictions

preds <- predict(d_sda, test)

confusionMatrix(test$FTR, preds)

#Test Accuracy is now 51%


#Prediction accuracy of Bookmakers

sum(d_odds$D_Odds > d_odds$H_Odds & d_odds$D > d_odds$A_Odds)
#->Bookmakers never bet on draws

d_odds$pred <- as.factor(ifelse(d_odds$H_Odds < d_odds$A_Odds, "H", "A"))

d_odds$pred <- as.numeric(d_odds$pred)
d_odds$FTR <- as.numeric(d_odds$FTR)

d_odds$right_pred <- ifelse(d_odds$FTR == d_odds$pred, 1, 0)
bookmaker_acc <- sum(d_odds$right_pred)/nrow(d_odds)
bookmaker_acc

#Our accuracy is higher, but bookmakers never bet on draws, so every D is a wrong prediction
#The bookmakers goal is not to predict the games correctly, but to provide their
#odds in such a way, that they make profit out of it

#Now we want to test the economic efficiency of our outcome probabilities with the bookmakers odds
#Betting strategy: Always bet on the favorite
#We bet 1$ hypothetically on every predicted match outcome
#If the prediction is right, we multiply the specific odd for the outcome with 1$ and add it to our "bank account"
#At the end we see, how much money we lost/won

bet_test <- tail(d_odds, n=306)

bet <- cbind(bet_test[,c("FTR", "H_Odds", "D_Odds", "A_Odds")], preds)
bet$acc <- NA

bet$FTR <- as.factor(bet$FTR)
bet$FTR <- ifelse(bet$FTR == 1, "A", ifelse(bet$FTR == 2, "D", "H"))

for (i in 1:nrow(bet)){
  b <- bet[i,]
  if (b$FTR == b$preds){
    if(b$FTR == "H"){
      bet[i,]$acc <- b$H_Odds - 1
    }
    else if(b$FTR == "D"){
      bet[i,]$acc <- b$D_Odds - 1
    }
    else {
      bet[i,]$acc <- b$A_Odds - 1
    }
  }
  else {
    bet[i,]$acc <- -1
  }
}

bank_account <- sum(bet$acc)
bank_account
#By betting 1$ on the predicted result of every match we do lose 13.83$ over 1 season, which is not too bad to start with
#As we analyzed before, our predictions are way better when we look at home win predictions
#Now we want to find out what our profit, would be if we just place bets when the prediction is a home win

h_bet <- bet[bet$preds == "H",]

sum(h_bet$acc)
#We still lose 7.54$ over the season

mean(h_bet$H_Odds)
mean(h_bet$D_Odds)
mean(h_bet$A_Odds)

#Odds for home wins are way lower, so we don't earn that much with right predictions


