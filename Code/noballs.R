rm(list = ls())
library(dplyr)
library(ggplot2)
library(stringr)
source("~/Documents/Work/Messy Fractals/IPL 2020/SeasonSource/season_source.R")
ipl <- read.csv("~/Documents/Work/Messy Fractals/Data/Ball-by-ball CSV/IPL 2018 - 21 Cricsheet.csv",stringsAsFactors = F)
ipl <- create_columns(ipl)

# No balls
ipl <- ipl %>% mutate(NoBall = ifelse(ExtraNB > 0,1,0))
ipl$BallNo <- sapply(ipl$DeliveryID, function(x){as.numeric(str_split(x,"\\.")[[1]][2])})
ipl %>% group_by(Season) %>% dplyr::summarise(NoBalls = sum(NoBall),
                                              BallsPerNo = sum(IsBallBowled)/sum(NoBall),
                                              BallsBowled = sum(IsBallBowled),
                                              Overs = sum(IsBallBowled)/6)

noballs <- ipl %>% filter(NoBall==1) %>% dplyr::select(MatchID,InningsNo,DeliveryID,bowler,striker,non_striker,ExtraNB)
# write.csv(noballs,"~/Documents/Writing/Bracket/No Balls/NoBalls.csv",row.names = F)

nb <- read.csv("/home/apoorva/Documents/Writing/Bracket/No Balls/Data/NoBalls.csv",stringsAsFactors = F)
nb <- nb %>% mutate(Season = as.integer(substr(MatchID,1,4)),
                    WrongCall = ifelse(is.na(WrongCall),0,WrongCall))
ptags <- read.csv("/home/apoorva/Documents/Work/Messy Fractals/Data/Player Tagging/ptags2.csv",stringsAsFactors = F)
nb <- nb %>% left_join(ptags %>% dplyr::select(Player,BowlSpec) %>% rename(bowler=Player)) %>% 
  mutate(BowlType = case_when(BowlSpec %in% c("Medium","Fast","FastM","Mfast") ~ "Seamer",
                              BowlSpec %in% c("Legbreak","Orthodox","Offbreak","Chinaman") ~ "Spinner"))
nbseason <- nb %>% group_by(BowlType,Season) %>% dplyr::summarise(NB = sum(Overstep-WrongCall))
se <- ipl %>% left_join(ptags %>% dplyr::select(Player,BowlSpec) %>% rename(bowler=Player)) %>% 
  mutate(BowlType = case_when(BowlSpec %in% c("Medium","Fast","FastM","Mfast") ~ "Seamer",
                              BowlSpec %in% c("Legbreak","Orthodox","Offbreak","Chinaman") ~ "Spinner")) %>% 
  group_by(Season,BowlType) %>% dplyr::summarise(Overs = sum(IsBallBowled)/6)
nbseason <- nbseason %>% left_join(se) %>% mutate(OversPerNB = Overs/NB) %>% mutate(ThirdUmp=ifelse(Season > 2019,1,0))
nbseason %>%  group_by(ThirdUmp) %>% dplyr::summarise(OversPerNB = sum(Overs)/sum(NB))
nbseason %>%  group_by(Season) %>% dplyr::summarise(OversPerNB = sum(Overs)/sum(NB))
ggplot(nbseason %>%  group_by(Season) %>% dplyr::summarise(OversPerNB = sum(Overs)/sum(NB)),
       aes(x=Season,y=OversPerNB)) + geom_col(fill="#ff9a73",width=0.6)+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="avenir"),
        axis.text = element_text(family="avenir"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.title = element_text(family="avenir",hjust = 0.5),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# For seamers
nbseason %>% filter(BowlType=="Seamer") %>%  group_by(ThirdUmp) %>% dplyr::summarise(OversPerNB = sum(Overs)/sum(NB))
ggplot(nbseason %>% filter(BowlType=="Seamer"),aes(x=Season,y=OversPerNB)) + geom_col()
# For spinners
nbseason %>% filter(BowlType=="Spinner") %>%  group_by(ThirdUmp) %>% dplyr::summarise(OversPerNB = sum(Overs)/sum(NB))
2400/471
2400/81.2
ggplot(nbseason %>% filter(BowlType=="Spinner"),aes(x=Season,y=OversPerNB)) + geom_col(fill="#ff9a73",width=0.6)+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="avenir"),
        axis.text = element_text(family="avenir"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.title = element_text(family="avenir",hjust = 0.5),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))


# Are more no-balls called at the death now?
nbdeseason <- nb %>% filter(DeliveryID > 15) %>% group_by(Season) %>% 
  dplyr::summarise(NB = sum(Overstep-WrongCall)) %>% mutate(ThirdUmp=ifelse(Season > 2019,1,0))
se <- ipl %>% filter(DeliveryID > 15) %>% group_by(Season) %>% dplyr::summarise(Overs = sum(IsBallBowled)/6)
nbdeseason <- nbdeseason %>% left_join(se) %>% mutate(OversPerNB = Overs/NB)
ggplot(nbdeseason,aes(x=Season,y=OversPerNB)) + geom_col(fill="#ff9a73",width=0.6)+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="avenir"),
        axis.text = element_text(family="avenir"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.title = element_text(family="avenir",hjust = 0.5),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))

# No-balls at the death
nbdeseason %>% group_by(ThirdUmp) %>% dplyr::summarise(OversPerNB = sum(Overs)/sum(NB))

# by ball number in the over!
totalpreump <- ipl %>% filter(Season <= 2019) %>% group_by(BallNo) %>%
  dplyr::summarise(Balls = sum(IsBallBowled))
totalpostump <- ipl %>% filter(Season > 2019) %>% group_by(BallNo) %>%
  dplyr::summarise(Balls = sum(IsBallBowled))

nb$BallNo <- sapply(nb$DeliveryID, function(x){as.numeric(str_split(x,"\\.")[[1]][2])})
nb$Over <- sapply(nb$DeliveryID, function(x){as.numeric(str_split(x,"\\.")[[1]][1]) + 1})
ballpostump <- nb %>% filter(Season > 2019) %>% group_by(BallNo) %>% 
  dplyr::summarise(NB = sum(Overstep-WrongCall)) %>% full_join(totalpostump) %>% mutate(OversPerNB=Balls/(6*NB))
b2 <- ggplot(ballpostump,aes(x=BallNo,y=OversPerNB)) + geom_col(fill="#ff9a73",width=0.6)+ggtitle("Post-TV")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="avenir"),
        axis.text = element_text(family="avenir"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.title = element_text(family="avenir",hjust = 0.5),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))
ballpreump <- nb %>% filter(Season <= 2019) %>% group_by(BallNo) %>%
  dplyr::summarise(NB = sum(Overstep-WrongCall)) %>% full_join(totalpreump) %>% mutate(OversPerNB=Balls/(6*NB))
b1 <- ggplot(ballpreump,aes(x=BallNo,y=OversPerNB)) + geom_col(fill="#ff9a73",width=0.6)+ggtitle("Pre-TV")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "#c1c3c4"),
        text=element_text(family="avenir"),
        axis.text = element_text(family="avenir"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.title = element_text(family="avenir",hjust = 0.5),
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                        colour = "#c1c3c4"))
gridExtra::grid.arrange(b1,b2,ncol=1)

# after one no ball had been found in the over!
allovers <- ipl %>% distinct(Season,MatchID,InningsNo,Over)
overnb <- nb %>% group_by(Season,MatchID,InningsNo,Over) %>% dplyr::summarise(NoBalls = sum(Overstep-WrongCall)) %>% 
  full_join(allovers) %>% tidyr::replace_na(list(NoBalls=0)) %>% 
  group_by(Season,NoBalls) %>% dplyr::summarise(Overs = length(unique(paste(MatchID,InningsNo,Over,sep="|"))))
totalovers <- ipl %>% group_by(Season) %>% dplyr::summarise(TotalOvers = length(unique(paste(MatchID,InningsNo,Over,sep="-"))))
overnb <- overnb %>% left_join(totalovers) %>% mutate(OversPC = Overs/TotalOvers,
                                                      ThirdUmp=ifelse(Season > 2019,1,0))
ggplot(overnb %>% filter(NoBalls==2),aes(x=Season,y=OversPC,fill=as.factor(NoBalls))) + geom_col(position="dodge")

overnb %>% group_by(ThirdUmp,NoBalls) %>% dplyr::summarise(OversPC = sum(Overs)/sum(TotalOvers),
                                                           Total = sum(TotalOvers),
                                                           Overs = sum(Overs))

# In overs where first four balls hadn't been no-balls, how often is a no-ball called on the last two balls?
# Take all the overs where first four balls didn't have no-balls
nbinfirst4 <- ipl %>% filter(BallNo <= 4) %>% group_by(MatchID,InningsNo,Over) %>% dplyr::summarise(NB = sum(ExtraNB)) %>% 
  filter(NB == 0) %>% mutate(OverID = paste(MatchID,InningsNo,Over,sep="-"))
iplwnb4 <- ipl %>% mutate(OverID = paste(MatchID,InningsNo,Over,sep="-")) %>% filter(OverID %in% nbinfirst4$OverID)
nb %>% mutate(OverID = paste(MatchID,InningsNo,Over,sep="-")) %>% filter(OverID %in% c(nbinfirst4$OverID)) %>% 
  filter(BallNo > 4) %>% mutate(ThirdUmp=ifelse(Season > 2019,1,0)) %>% group_by(ThirdUmp) %>% 
  dplyr::summarise(Overs = length(unique(OverID)),
                   NB = sum(Overstep - WrongCall))
nb %>% mutate(OverID = paste(MatchID,InningsNo,Over,sep="-")) %>% filter(BallNo >= 5) %>% 
  mutate(ThirdUmp=ifelse(Season > 2019,1,0)) %>% group_by(ThirdUmp) %>%
  dplyr::summarise(Overs = length(unique(OverID)),
                   NB = sum(Overstep - WrongCall))