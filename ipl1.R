#packages : dplyr, tidyr,ggplot2
#Libraries
library(dplyr)
library(caTools)
library(rpart)
library(randomForest)

#Reading the csv
matches <- read.csv("matches.csv")
deliveries <- read.csv("deliveries.csv")

#Cleaning up
matches$umpire3 <- NULL
matches$toss_winner = matches$toss_winner %>% as.character()
matches$winner = matches$winner %>% as.character()
matches$winner = ifelse(matches$winner == "", "Tied", matches$winner)  %>% as.character() 
matches$date = as.Date(matches$date)

matches$team2 = matches$team2 %>% as.character()
matches$team1 = matches$team1 %>% as.character()
matches$toss_decision = matches$toss_decision %>% as.character()

#Missing values in city
matches$city <- sub("^$", "Dubai", matches$city)

#Team1 Home/Away
matches <-mutate(matches,t1ha = ifelse(team1 == "Deccan Chargers" & city == "Hyderabad",1,ifelse(team1 == "Kochi Tuskers Kerala" & city == "Kochi" | city == "Indore",1,ifelse(team1 == "Rajasthan Royals" & city == "Jaipur",1,ifelse(team1 == "Delhi Daredevils" & city == "Delhi",1,ifelse(team1 == "Chennai Superkings" & city == "Chennai",1,ifelse(team1 == "Kings XI Punjab" & city == "Mohali",1,ifelse(team1 == "Pune Warriors" & city == "Pune",1,ifelse(team1 == "Mumbai Indians" & city == "Mumbai",1,ifelse(team1 == "Royal Challengers Bangalore" & city == "Bangalore",1,ifelse(team1 == "Kolkata Knight Riders" & city == "Kolkata",1,ifelse(team1 == "Sunrisers Hyderabad" & city == "Hyderabad",1,ifelse(team1 == "Gujarat Lions" & city == "Rajkot",1,ifelse(team1 == "Rising Pune Supergiants" & city == "Pune",1,0))))))))))))))
#Team2 Home/Away
matches <-mutate(matches,t2ha = ifelse(team2 == "Deccan Chargers" & city == "Hyderabad",1,ifelse(team2 == "Kochi Tuskers Kerala" & city == "Kochi" | city == "Indore",1,ifelse(team2 == "Rajasthan Royals" & city == "Jaipur",1,ifelse(team2 == "Delhi Daredevils" & city == "Delhi",1,ifelse(team2 == "Chennai Superkings" & city == "Chennai",1,ifelse(team2 == "Kings XI Punjab" & city == "Mohali",1,ifelse(team2 == "Pune Warriors" & city == "Pune",1,ifelse(team2 == "Mumbai Indians" & city == "Mumbai",1,ifelse(team2 == "Royal Challengers Bangalore" & city == "Bangalore",1,ifelse(team2 == "Kolkata Knight Riders" & city == "Kolkata",1,ifelse(team2 == "Sunrisers Hyderabad" & city == "Hyderabad",1,ifelse(team2 == "Gujarat Lions" & city == "Rajkot",1,ifelse(team2 == "Rising Pune Supergiants" & city == "Pune",1,0))))))))))))))

#Number of matches played by each team
matchesTeam <- matches %>%
  group_by(team = team1) %>%
  summarise(matches = n())%>%
  full_join(matches %>%
              group_by(team = team2)%>%
              summarise(matches = n()), by = "team") %>%
  mutate("matches" = `matches.x` + `matches.y`)%>%
  select(team, matches)

options(digit = 2)

#Matches Won
matchesTeam <- matchesTeam %>%
  left_join(matches %>%
              filter(winner != "")%>%
              group_by(team = winner)%>%
              summarise("Match Won" = n()), by = "team")

matchesTeam$team <- as.factor(matchesTeam$team)


#Win Percetage
matchesTeam <- mutate(matchesTeam, "Win %" = `Match Won`/matches * 100)

#Winning record
matchesTeam <- mutate(matchesTeam, "Win Record" = `matches` - `Match Won`)

#Runs Scored
matchesTeam <- matchesTeam %>%
  left_join(deliveries %>%
              group_by(team = batting_team)%>%
              summarise("Runs Scored" = sum(total_runs)), by = "team")

#Average Runs Scored in an Innings
matchesTeam <- mutate(matchesTeam, "Runs Scored per Innings" = `Runs Scored`/matches)
matchesTeam$`Runs Scored per Innings` <- ceiling(matchesTeam$`Runs Scored per Innings`)

#Wickets
matchesTeam <- matchesTeam %>%
  left_join(deliveries %>%
              group_by(team = bowling_team)%>%
              summarise("Wickets" = sum(player_dismissed!="")), by = "team")

#Average wickets in an Innings
matchesTeam <- mutate(matchesTeam, "Wickets per inning" = `Wickets`/matches)
matchesTeam$`Wickets per inning` <- ceiling(matchesTeam$`Wickets per inning`)

View(matchesTeam)
#Tidying winner variable
for (i in 1:length(matches$id)) {
  if (matches$team1[i] == matches$winner[i]) {
    matches$winner[i] = 0
  } else if (matches$team2[i] == matches$winner[i]) {
    matches$winner[i] = 1
  } else {
    matches$winner[i] = 0
  }
}
matches$winner <- matches$winner %>% as.numeric()
#Team1 team 2 tosswin variable
matches$t1tw <- NULL
matches$t2tw <- NULL
for (i in 1:length(matches$id)) {
  if (matches$team1[i] == matches$toss_winner[i]) {
    matches$t1tw[i] = 1
    matches$t2tw[i] = 0
  } else {
    matches$t1tw[i] = 0
    matches$t2tw[i] = 1
  }
}

#Team1 team 2 batfirst field first variable
matches$t1twbf <- 0
matches$t1twff <- 0
matches$t2twbf <- 0
matches$t2twff <- 0

for(i in 1:length(matches$id)) {
  if(matches$t1tw[i] == 1){
  
    if(matches$toss_decision[i] == "field"){
      matches$t1twff[i] = 1
      matches$t1twbf[i] = -1
    } else {
      matches$t1twff[i] = -1
      matches$t1twbf[i] = 1
      }
  } else {
    if(matches$toss_decision[i] == "field"){
      matches$t2twff[i] = 1
      matches$t2twbf[i] = -1
    } else {
      matches$t2twff[i] = -1
      matches$t2twbf[i] = 1
    }
  }
}  

mdf <- merge(merge(matches, matchesTeam, by.x=c("team1"), by.y=c("team"), all.x=TRUE),
             matchesTeam, by.x=c("team2"), by.y=c("team"), all.x=TRUE)
mdf <- with(mdf,  mdf[order(id) , ])
mdf$toss_decision <- NULL
mdf$date <- NULL
mdf$toss_winner <- NULL
mdf$result <- NULL
mdf$dl_applied <- NULL
mdf$player_of_match <- NULL
mdf$venue <- NULL
mdf$umpire1 <- NULL
mdf$umpire2 <- NULL
mdf <- plyr::rename(mdf,c("matches.x"="team1tm","Match Won.x"="team1mw","Win %.x"="team1wp","Win Record.x"="team1wr","Runs Scored.x"="team1rs","Runs Scored per Innings.x"="team1rspi","Wickets.x"="team1w","Wickets per inning.x"="team1wpi","matches.y" = "team2tm","Match Won.y"="team2mw","Win %.y" = "team2wp","Win Record.y"="team2wr","Runs Scored.y"="team2rs","Runs Scored per Innings.y"="team2rspi","Wickets.y"="team2w","Wickets per inning.y" = "team2wpi"))
mdf<-mdf[,c(3,2,1,4,5,7:32,6)]
rownames(mdf) <- 1:nrow(mdf)
mdf$winner <- mdf$winner %>% as.factor()
mdf$team1tm <- (mdf$team1tm - mean(mdf$team1tm))/sd(mdf$team1tm)
mdf$team2tm <- (mdf$team2tm - mean(mdf$team2tm))/sd(mdf$team2tm)
mdf$team1w <- (mdf$team1w - mean(mdf$team1w))/sd(mdf$team1w)
mdf$team2w <- (mdf$team2w - mean(mdf$team2w))/sd(mdf$team2w)
set.seed(123)
x <- sample.split(mdf,SplitRatio = 0.90)
train <- mdf[x,1:32]
test <- mdf[!x,1:32]

model <- glm (winner ~ t1ha + t1tw + team1tm + team1wp + team1rspi + team1w + t2ha + team2tm + team2wp + team2rspi + team2w, data = train, family = 'binomial')
summary(model)
predict <- predict(model,test, type = 'response')
table(test$winner, predict > 0.5)


fit <- rpart(winner ~ t1ha + t1tw + team1tm + team1wp + team1rspi + team1w + t2ha + team2tm + team2wp + team2rspi + team2w, data = train, method = 'class')
predicted= predict(fit,test)
rownames(predicted) <- 1:nrow(predicted)
predicted[,1] <- round(predicted[,1])
predicted[,2] <- round(predicted[,2])

preddata <- predicted %>% as.data.frame()
preddata$winner <- test$winner
for(i in 1:72) {
  if(preddata[i,1] == 1 & preddata[i,3] == 0 | preddata[i,2] == 1 & preddata[i,3] == 1){
    preddata[i,4] = 1
  } else {
    preddata[i,4] = 0
  }
}
sum(preddata$V4)/72 * 100
View(preddata)

fitrf <- randomForest(winner ~ t1ha + t1tw + team1tm + team1wp + team1rspi + team1w + t2ha + team2tm + team2wp + team2rspi + team2w , data = train)
predictionrf <- predict(fitrf, test)
table(predictionrf,test$winner)

