#packages : dplyr, tidyr,ggplot2
#Libraries
library(dplyr)
library(ggplot2)
library(tidyr)

#Reading the csv
matches <- read.csv("matches.csv")
deliveries <- read.csv("deliveries.csv")



#Tidying up
matches$umpire3 <- NULL
matches$toss_winner = matches$toss_winner %>% as.character()
matches$winner = matches$winner %>% as.character()
matches$winner = ifelse(matches$winner == "", "Tied", matches$winner) %>% as.factor()
matches$date = as.Date(matches$date)

#Missing values in city
matches$city <- sub("^$", "Dubai", matches$city)


#Merging the data sets
ipl <- merge(deliveries, matches, by.x = "match_id", by.y = "id" )


#New Features for EDA
matches$toss_match = ifelse(matches$toss_winner == matches$winner, 1, 0) %>% as.factor()
matches$winner_do = ifelse(matches$win_by_runs>0, "bat", "field") %>% as.factor()

#Plotting toss win against number of wins
ggplot(matches, aes(reorder(winner, date), fill = toss_match)) + 
  geom_bar(color = "black", width = 0.8, position = position_dodge(width=0.8)) +
  xlab("Winning Team") + ylab("Number of wins") +
  ggtitle("Winning Team vs Winning Toss") + 
  scale_fill_discrete(guide = guide_legend(title = "toss_match")) +
  theme(axis.text.x=element_text(angle=75, hjust=1))


#Plotting Bat first/Field First of winning teams
ggplot(matches, aes(reorder(winner, date), fill = winner_do)) + 
  geom_bar(color = "black", width = 0.8, position = position_dodge(width=0.8)) +
  xlab("Winning Team") + ylab("Number of wins") +
  ggtitle("Winning team bat first/field first") + 
  scale_fill_discrete(guide = guide_legend(title = "winner_do")) +
  theme(axis.text.x=element_text(angle=75, hjust=1))

#Team Performance at Home ground and Away
t<-ipl %>% 
  filter((result=="normal" | result == "tie") & batting_team %in% c("Kolkata Knight Riders", "Royal Challengers Bangalore",
                                                                    "Chennai Super Kings","Kings XI Punjab","Rajasthan Royals",
                                                                    "Delhi Daredevils","Mumbai Indians","Gujarat Lions"))

#KKR Perforamance
kkr_match_played<-t %>% 
  filter(batting_team=="Kolkata Knight Riders") %>% 
  mutate(ground_type = if_else(city == "Kolkata","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_match_played = n_distinct(match_id))

kkr_match_won<-t %>% 
  filter(batting_team=="Kolkata Knight Riders" & winner == "Kolkata Knight Riders") %>% 
  mutate(ground_type = if_else(city == "Kolkata","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_win = n_distinct(match_id))

KKR<-merge(kkr_match_played, kkr_match_won, by ="ground_type")

KKR<-KKR %>% 
  mutate(winning_perc = (total_win/total_match_played)*100,
         team = "KKR") 

#Chennai Super Kings
csk_match_played<-t %>% 
  filter(batting_team=="Chennai Super Kings") %>% 
  mutate(ground_type = if_else(city == "Chennai","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_match_played = n_distinct(match_id))

csk_match_won<-t %>% 
  filter(batting_team=="Chennai Super Kings" & winner == "Chennai Super Kings") %>% 
  mutate(ground_type = if_else(city == "Chennai","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_win = n_distinct(match_id))

CSK<-merge(csk_match_played, csk_match_won, by ="ground_type")

CSK<-CSK %>% 
  mutate(winning_perc = (total_win/total_match_played)*100,
         team ="CSK") 

#Mumbai Indians
mi_match_played<-t %>% 
  filter(batting_team=="Mumbai Indians") %>% 
  mutate(ground_type = if_else(city == "Mumbai","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_match_played = n_distinct(match_id))

mi_match_won<-t %>% 
  filter(batting_team=="Mumbai Indians" & winner == "Mumbai Indians") %>% 
  mutate(ground_type = if_else(city == "Mumbai","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_win = n_distinct(match_id))

MI<-merge(mi_match_played, mi_match_won, by ="ground_type")

MI<-MI %>% 
  mutate(winning_perc = (total_win/total_match_played)*100,
         team= "MI") 

#Kings XI punjab
KXIP_match_played<-t %>% 
  filter(batting_team=="Kings XI Punjab") %>% 
  mutate(ground_type = if_else(city == "Chandigarh","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_match_played = n_distinct(match_id))

KXIP_match_won<-t %>% 
  filter(batting_team=="Kings XI Punjab" & winner == "Kings XI Punjab") %>% 
  mutate(ground_type = if_else(city == "Chandigarh","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_win = n_distinct(match_id))

KXIP<-merge(KXIP_match_played, KXIP_match_won, by ="ground_type")

KXIP<-KXIP %>% 
  mutate(winning_perc = (total_win/total_match_played)*100,
         team="KXIP") 
#Rajasthan Royal
RR_match_played<-t %>% 
  filter(batting_team=="Rajasthan Royals") %>% 
  mutate(ground_type = if_else(city == "Jaipur","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_match_played = n_distinct(match_id))

RR_match_won<-t %>% 
  filter(batting_team=="Rajasthan Royals" & winner == "Rajasthan Royals") %>% 
  mutate(ground_type = if_else(city == "Jaipur","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_win = n_distinct(match_id))

RR<-merge(RR_match_played, RR_match_won, by ="ground_type")

RR<-RR %>% 
  mutate(winning_perc = (total_win/total_match_played)*100,
         team = "RR") 

#Royal Challengers Bangalore
RCB_match_played<-t %>% 
  filter(batting_team=="Royal Challengers Bangalore") %>% 
  mutate(ground_type = if_else(city == "Bangalore","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_match_played = n_distinct(match_id))

RCB_match_won<-t %>% 
  filter(batting_team=="Royal Challengers Bangalore" & winner == "Royal Challengers Bangalore") %>% 
  mutate(ground_type = if_else(city == "Bangalore","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_win = n_distinct(match_id))

RCB<-merge(RCB_match_played, RCB_match_won, by ="ground_type")

RCB<-RCB %>% 
  mutate(winning_perc = (total_win/total_match_played)*100,
         team ="RCB")

#Delhi Daredevils

DD_match_played<-t %>% 
  filter(batting_team=="Delhi Daredevils") %>% 
  mutate(ground_type = if_else(city == "Delhi","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_match_played = n_distinct(match_id))

DD_match_won<-t %>% 
  filter(batting_team=="Delhi Daredevils" & winner == "Delhi Daredevils") %>% 
  mutate(ground_type = if_else(city == "Delhi","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_win = n_distinct(match_id))

DD<-merge(DD_match_played, DD_match_won, by ="ground_type")

DD<-DD %>% 
  mutate(winning_perc = (total_win/total_match_played)*100,
         team = "DD")


#Gujarat Lions

GL_match_played<-t %>% 
  filter(batting_team=="Gujarat Lions") %>% 
  mutate(ground_type = if_else(city == "Rajkot","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_match_played = n_distinct(match_id))

GL_match_won<-t %>% 
  filter(batting_team=="Gujarat Lions" & winner == "Gujarat Lions") %>% 
  mutate(ground_type = if_else(city == "Rajkot","Home","Away")) %>% 
  group_by(ground_type) %>% 
  summarise(total_win = n_distinct(match_id))

GL<-merge(GL_match_played, GL_match_won, by ="ground_type")

GL <-GL %>% 
  mutate(winning_perc = (total_win/total_match_played)*100,
         team = "GL")

team_performances<-rbind(CSK,DD,KKR,MI,KXIP,RCB,RR,GL)
team_performances <- team_performances[,c(5,1,2,3,4)]
View(team_performances)

team_performances %>% 
  ggplot(aes(x = team, y = winning_perc,fill = ground_type))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(list(title = "Teams Performance", x = "Team", y = "Winning Percentage"))+
  theme(axis.text.x=element_text(angle=75, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=8))


#Tidying Toss Column Bat = 0 Field = 1
temp.map <- c("field"=1,"bat"=0)
matches$toss_decision <- temp.map[as.character(matches$toss_decision)]

#Tidying data
matches$date <- format(matches$date,"%m")
matches$date <- as.integer(matches$date)
matches$toss_decision = matches$toss_decision %>% as.factor()
matches$city = matches$city %>% as.factor()
matches$dl_applied <- NULL
matches$result <- NULL
matches$venue <- NULL
matches$player_of_match <- NULL
matches$win_by_runs <- NULL
matches$win_by_wickets <- NULL
matches$toss_match <- NULL
matches$umpire1 <- NULL
matches$umpire2 <- NULL
matches$winner_do <- NULL

write.csv(matches, file = "Matches_mod.csv",row.names = FALSE)




