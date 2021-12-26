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
library(rpart)
library(tree)
library(rpart.plot)
library(doParallel)
library(MLmetrics)
library(klaR)

#Build the dataframe----
season0 <- read.csv("D1 (14).csv", stringsAsFactors = T) 
season1 <- read.csv("D1 (12).csv", stringsAsFactors = T)
season2 <- read.csv("D1 (11).csv", stringsAsFactors = T)
season3 <- read.csv("D1 (10).csv", stringsAsFactors = T)
season4 <- read.csv("D1 (9).csv", stringsAsFactors = T)
season5 <- read.csv("D1 (8).csv", stringsAsFactors = T)
season6 <- read.csv("D1 (7).csv", stringsAsFactors = T)

cols_1 <- c("Date", "HomeTeam", "AwayTeam",	"FTHG",	"FTAG",	"FTR"	,"HS"	,"AS"	,"HST","AST","HF","AF","HC","AC","HY","AY","HR","AR","BbAvH","BbAvD","BbAvA","BbAv.2.5","BbAv.2.5.1")
cols_2 <- c("Date", "HomeTeam", "AwayTeam",	"FTHG",	"FTAG",	"FTR"	,"HS"	,"AS"	,"HST","AST","HF","AF","HC","AC","HY","AY","HR","AR","AvgH","AvgD","AvgA","Avg.2.5","Avg.2.5.1")

season0 <- season0[cols_1]
season1 <- season1[cols_1]
season2 <- season2[cols_1]
season3 <- season3[cols_1]
season4 <- season4[cols_1]
season5 <- season5[cols_2]
season6 <- season6[cols_2]

season0 <- rename(season0, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA, Over_2.5 = BbAv.2.5, Under_2.5 = BbAv.2.5.1)
season1 <- rename(season1, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA, Over_2.5 = BbAv.2.5, Under_2.5 = BbAv.2.5.1)
season2 <- rename(season2, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA, Over_2.5 = BbAv.2.5, Under_2.5 = BbAv.2.5.1)
season3 <- rename(season3, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA, Over_2.5 = BbAv.2.5, Under_2.5 = BbAv.2.5.1)
season4 <- rename(season4, H_Odds = BbAvH, D_Odds = BbAvD, A_Odds = BbAvA, Over_2.5 = BbAv.2.5, Under_2.5 = BbAv.2.5.1)
season5 <- rename(season5, H_Odds = AvgH, D_Odds = AvgD, A_Odds = AvgA, Over_2.5 = Avg.2.5, Under_2.5 = Avg.2.5.1)
season6 <- rename(season6, H_Odds = AvgH, D_Odds = AvgD, A_Odds = AvgA, Over_2.5 = Avg.2.5, Under_2.5 = Avg.2.5.1)

df <- rbind(season0, season1, season2, season3, season4, season5, season6)
rm(season0, season1, season2, season3, season4, season5, season6)

sum(is.na(df))

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

df$Date <- as.Date(df$Date, "%d/%m/%y")
df$goals <- df$FTHG + df$FTAG
df$Is_over_2.5 <- ifelse(df$goals > 2.5,1,0)

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


#apply the form functions on every match of the df
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

d[,26:89] <- lapply(df[,26:89], unlist)
d[,26:89] <- lapply(df[,26:89], as.numeric)


#Remove NaN-rows in the data
sum(is.na(d))
d <- na.omit(d)
str(d)

#Round the values
d <- d %>% mutate_if(is.numeric, ~round(.,2))

#Remove unneeded df's
rm(result,stat_away, stat_away_opp, stat_home, stat_home_opp)

###Model Building-------
d_odds <- d

#Remove columns that we don't need for the model
drop <- c("Date", "HomeTeam", "AwayTeam",	"FTHG",	"FTAG",	"FTR"	,"HS"	,"AS"	,"HST","AST","HF","AF","HC","AC","HY","AY","HR","AR", "H_Odds", "A_Odds", "D_Odds", "Over_2.5", "Under_2.5", "goals" ,"home_team_win", "away_team_win", "draw", "result")
d <- d[,!(names(d) %in% drop)]

d$Is_over_2.5 <- as.factor(d$Is_over_2.5)
levels(d$Is_over_2.5) <- c("Yes", "No")


plot(d$Is_over_2.5)
mean(df$goals)

#Feature Selection----

#identify correlated predictors
cor_mat <- cor(subset(d, select = -Is_over_2.5))
highlyCorrelated <- findCorrelation(cor_mat, cutoff=0.75)

highlyCorrelated <- highlyCorrelated[!highlyCorrelated %in% 1]
names(d[,highlyCorrelated])

d <- d[,-highlyCorrelated]


#Recursive Feature Elimination
set.seed(101)
rfeC <- rfeControl(functions = rfFuncs, method="cv", number=10)


results <- rfe(d[,2:37], d[,1], sizes=(1:18), rfeControl=rfeC)

print(results)
predictors(results)
plot(results, type=c("g", "o"))

p <- head(results$optVariables, 7)
p <- c("Is_over_2.5", p)

d <- d[,p]


#Train models----

#Setup control function to make rolling forecasts for every matchday (9 games) based on the whole data before the respective matchday

ctrl <- trainControl(method = "timeslice",
                     initialWindow = 306,
                     horizon = 9,
                     fixedWindow = FALSE,
                     skip = 9,
                     classProbs = TRUE,
                     savePredictions = TRUE,
                     summaryFunction = twoClassSummary,
                     allowParallel = TRUE)


#Logistic Regression
d.log <-  train(Is_over_2.5 ~ ., data=d, method="glm", family="binomial", metric ="ROC", trControl=ctrl)
confusionMatrix(d.log$pred$pred, d.log$pred$obs)

#Linear Discriminant Analysis
d.lda <-  train(Is_over_2.5 ~ ., data=d, method="lda", metric ="ROC", trControl=ctrl)
confusionMatrix(d.lda$pred$pred, d.lda$pred$obs)

#K nearest neighbors classification
d.knn <-  train(Is_over_2.5 ~ ., data=d, method="knn", metric ="ROC", trControl=ctrl, tuneLength = 10)
confusionMatrix(d.knn$pred$pred, d.knn$pred$obs)

#SVM
d.svm <- train(Is_over_2.5~., data=d, method="svmRadial", metric="ROC", trControl=ctrl)
confusionMatrix(d.svm$pred$pred, d.svm$pred$obs)

#SVM with grid
svm.grid <- expand.grid(sigma = c(.01, .03, .05), C=c(.25, .75, 1))
d.svmg <- train(Is_over_2.5~., data=d, method="svmRadial", metric="ROC", trControl=ctrl, tuneGrid=svm.grid)
confusionMatrix(d.svmg$pred$pred, d.svmg$pred$obs)

#Naive Bayes
d.nb <- train(Is_over_2.5~., data=d, method="nb", metric="ROC", trControl=ctrl)
confusionMatrix(d.nb$pred$pred, d.nb$pred$obs)


#lets compare all resampling approaches
d.models <- list(logit=d.log, lda=d.lda, knn=d.knn, svm = d.svm, svmg = d.svmg, nb = d.nb)
d.resamples = resamples(d.models)

#plot performance comparisons
bwplot(d.resamples, metric="ROC") 
bwplot(d.resamples, metric="Sens")
bwplot(d.resamples, metric="Spec") 


#calculate ROC curves on resampled data
d.log.roc<- roc(response= d.log$pred$obs, predictor=d.log$pred$Yes)
d.lda.roc<- roc(response= d.lda$pred$obs, predictor=d.lda$pred$Yes)
d.knn.roc<- roc(response= d.knn$pred[d.knn$pred$k==23,]$obs, predictor=d.knn$pred[d.knn$pred$k==23,]$Yes)
d.svm.roc<- roc(response= d.svm$pred$obs, predictor=d.svm$pred$Yes)
d.svmg.roc<- roc(response= d.svmg$pred$obs, predictor=d.svmg$pred$Yes)
d.nb.roc<- roc(response= d.nb$pred$obs, predictor=d.nb$pred$Yes)


#build combined ROC plot with resampled ROC curves
plot(d.log.roc, legacy.axes=T, col="Blue", lty=2)
plot(d.lda.roc, add=T, col="Green", lty=2)
plot(d.knn.roc, add=T, col="Red", lty=2)
plot(d.svm.roc, add=T, col="Yellow")
plot(d.svmg.roc, add=T, col="Purple")
plot(d.nb.roc, add=T, col="Pink")
legend(x=1, y=1, legend=c("Logit", "LDA", "KNN", "SVM", "SVMg", "NB"), col=c("blue","green","red", "yellow", "purple", "pink"),lty=1, cex = 1)

#extract threshold from roc curve  get threshold at coordinates top left most corner
d.log.Thresh<- coords(d.log.roc, x="best", best.method="closest.topleft")
d.log.Thresh
d.lda.Thresh<- coords(d.lda.roc, x="best", best.method="closest.topleft")
d.lda.Thresh
d.knn.Thresh<- coords(d.knn.roc, x="best", best.method="closest.topleft")
d.knn.Thresh
d.svm.Thresh<- coords(d.svm.roc, x="best", best.method="closest.topleft")
d.svm.Thresh
d.svmg.Thresh<- coords(d.svmg.roc, x="best", best.method="closest.topleft")
d.svmg.Thresh
d.nb.Thresh<- coords(d.nb.roc, x="best", best.method="closest.topleft")
d.nb.Thresh

#lets make new predictions with this cut-off and recalculate confusion matrix
d.log.newpreds <- as.factor(ifelse(d.log$pred$Yes > 0.4242194, "Yes", "No"))
d.lda.newpreds <- as.factor(ifelse(d.lda$pred$Yes > 0.4242657, "Yes", "No"))
d.knn.newpreds <- as.factor(ifelse(d.knn$pred$Yes > 0.4130435, "Yes", "No"))
d.svm.newpreds <- as.factor(ifelse(d.svm$pred$Yes > 0.4309935, "Yes", "No"))
d.svmg.newpreds <- as.factor(ifelse(d.svmg$pred$Yes > 0.4379796, "Yes", "No"))
d.nb.newpreds <- as.factor(ifelse(d.nb$pred$Yes > 0.4557247, "Yes", "No"))

#recalculate confusion matrix with new cut off predictions
confusionMatrix(d.log.newpreds, d.log$pred$obs)
confusionMatrix(d.lda.newpreds, d.lda$pred$obs)
confusionMatrix(d.knn.newpreds, d.knn$pred$obs)
confusionMatrix(d.svm.newpreds, d.svm$pred$obs)
confusionMatrix(d.svmg.newpreds, d.svmg$pred$obs)
confusionMatrix(d.nb.newpreds, d.nb$pred$obs)


#NB is the best 
#The overall accuracy dropped from 57% to 55%
#But: Sensitivity increased from 38% to 55%
#Specificity decreased from 70% to 55%
#More balanced predictions

nb.c <- confusionMatrix(d.nb.newpreds, d.nb$pred$obs)

nb.acc <- nb.c$overall["Accuracy"]
nb.sens <- nb.c$byClass["Sensitivity"]
nb.spec <- nb.c$byClass["Specificity"]

#Calculate accuracy of the sports betting provider

d_odds$pred <- as.factor(ifelse(d_odds$Over_2.5 < d_odds$Under_2.5,"1", "0"))
d_odds$Is_over_2.5 <- as.factor(d_odds$Is_over_2.5)
confusionMatrix(d_odds$pred, d_odds$Is_over_2.5)

odds.c <- confusionMatrix(d_odds$pred, d_odds$Is_over_2.5)

odds.acc <- odds.c$overall["Accuracy"]
odds.sens <- odds.c$byClass["Sensitivity"]
odds.spec <- odds.c$byClass["Specificity"]

par(mfrow=c(1,3))
barplot(cbind(nb.acc, odds.acc), main="Accuracy")
barplot(cbind(nb.sens, odds.sens), main="Sensitivity")
barplot(cbind(nb.spec, odds.spec), main="Specificity")


#Our accuracy is lower, but we have a higher TPR. That means we are better in predicting games
#with more than 2.5 goals per game, while the bookmakers are much better in predicting
#low scoring games

#Now we want to test the economic efficiency of our model with the bookmakers odds
#Betting strategy: Always bet on the prediction
#We bet 1$ hypothetically on every predicted event (Over or under 2.5 goals)
#If the prediction is right, we multiply the specific odd for the outcome with 1$ and add it to our "bank account"
#At the end we see, how much money we lost/won

bet_test <-tail(d_odds, n=306)

preds <- predict(d.nb, bet_test)

bet <- cbind(bet_test[,c("Is_over_2.5", "Over_2.5", "Under_2.5")], preds)
bet$acc <- NA

for (i in 1:nrow(bet)){
  b <- bet[i,]
  if (b$Is_over_2.5 == 1 & b$preds == "Yes"){
    bet[i,]$acc <- b$Over_2.5 -1
    }
  else {
    if (b$Is_over_2.5 == 0 & b$preds == "No"){
      bet[i,]$acc <- b$Under_2.5 -1
    }
    else {
      bet[i,]$acc <- -1
    }
  }
}

sum(bet$acc)      

#We lose $49.23 with our betting strategy over the course of 1 season
#As our TPR is better than the bookmakers, we may have better chances by only betting on
#over 2.5 predictions

sum(bet[bet$preds == "Yes",]$acc)
#We indeed lose way less money than with our first strategy