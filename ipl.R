#packages : dplyr, tidyr

matches <- read.csv("matches.csv")
deliveries <- read.csv("deliveries.csv")

ipl <- merge(deliveries, matches, by.x = "match_id", by.y = "id" )

#Converting season column to factor
ipl$season <- as.factor(ipl$season)

#Toss winning advantage
matches$toss_match<-ifelse(as.character(matches$toss_winner)==as.character(matches$winner),"Won","Lost")

ggplot(matches[which(!is.na(matches$toss_match)),],aes(toss_match, fill = toss_match))+ 
  geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("Toss winning Advantage")


#Number of matches played by each team
mT <- matches %>%
  group_by(team = team1) %>%
  summarise(matches = n())%>%
  full_join(matches %>%
              group_by(team = team2)%>%
              summarise(matches = n()), by = "team") %>%
  mutate("matches" = `matches.x` + `matches.y`)%>%
  select(team, matches)

#Matches won
mT <- mT %>%
  left_join(matches %>%
              filter(winner != "")%>%  
              group_by(team = winner)%>%
              summarise("Match Won" = n()), by = "team")


#Win Percetage
mT <- mutate(mT, "wPerc" = `Match Won`/matches * 100)

#Matches won in 2015
mT <- mT %>%
  left_join(matches %>%
              filter(winner != "",season == 2015)%>%  
              group_by(team = winner)%>%
              summarise("Match Won in 2015" = n()), by = "team")

#Win percentage in 2015
mT <- mutate(mT, "wPerc2015" = `Match Won in 2015`/matches * 100)

#plotting wPerc
ggplot(mT, aes(x = reorder(team, -`wPerc`), y = `wPerc`, fill = `wPerc`)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  labs(title = "Win Ratio")+
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0),
        axis.text.y = element_text(size = 7.5),
        plot.title = element_text(size =20, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 10))


#Team Performance at Home ground and Away
t<-ipl %>% 
  filter((result=="normal" | result == "tie") & batting_team %in% c("Kolkata Knight Riders", "Royal Challengers Bangalore",
                                                                    "Chennai Super Kings","Kings XI Punjab","Rajasthan Royals",
                                                                    "Delhi Daredevils","Mumbai Indians","Gujarat Lions"))
View(mT)

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

team_performances<-rbind(CSK, DD,KKR,MI,KXIP,RCB,RR,GL)

View(team_performances)

team_performances %>% 
  ggplot(aes(x = team, y = winning_perc,fill = ground_type))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(list(title = "Teams Performance", x = "Team", y = "Winning Percentage"))+
  theme(axis.text.x=element_text(angle=75, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=8))


