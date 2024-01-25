library(readr)
library(dplyr)
library(tidyverse)
NFL_Play_by_Play_2009_2017_v4_csv <-
  read_csv("C:/Users/Alex/Downloads/NFL Play by Play 2009-2017 (v4).csv.zip")
View(NFL_Play_by_Play_2009_2017_v4_csv)

cname2017 <- colnames(NFL_Play_by_Play_2009_2017_v4_csv)


#Yards.Gained



NFL_Play_by_Play_2009_2017_v4_csv <-
  NFL_Play_by_Play_2009_2017_v4_csv %>% mutate(
    posteam = ifelse(
      PlayType == "Punt",
      ifelse(posteam == HomeTeam, AwayTeam, HomeTeam),
      posteam
    ),
    yrdline100 = ifelse(PlayType == "Punt", 100 - yrdline100, yrdline100),
    Drive = ifelse(PlayType == "Punt"  & !str_detect(tolower(desc), "fake punt"), 
                   ifelse((Touchdown == 1 | (Fumble == 1 & posteam != lead(posteam))),
                          Drive+.5,
                          Drive + 1), 
                   Drive)
  )

# NFL_Play_by_Play_2009_2017_v4_csv <- NFL_Play_by_Play_2009_2017_v4_csv %>% 
#   filter(!(PlayType == "Quarter End" & qtr %in% c(1,3)))

NFL_Play_by_Play_2009_2017_v4_csv <- NFL_Play_by_Play_2009_2017_v4_csv %>% 
  filter(!(PlayType %in% c("Quarter End", "Half End", "End of Game")))

# end.half.ind <- which(NFL_Play_by_Play_2009_2017_v4_csv$qtr == 2 & lead(NFL_Play_by_Play_2009_2017_v4_csv$qtr) == 3)
#   
# for (i in end.half.ind + c(0:(length(end.half.ind) - 1))) {
#   i = end.half.ind[1]
#   new.obs <- NFL_Play_by_Play_2009_2017_v4_csv[i, ]
#   new.obs[5:length(new.obs)] <- NA
#   new.obs$PlayType <- "End of Half"
#   NFL_Play_by_Play_2009_2017_v4_csv <-
#     rbind(
#       NFL_Play_by_Play_2009_2017_v4_csv[1:i, ],
#       new.obs,
#       NFL_Play_by_Play_2009_2017_v4_csv[(i + 1):nrow(NFL_Play_by_Play_2009_2017_v4_csv), ]
#     )
#   print(i)
#   print(dim(NFL_Play_by_Play_2009_2017_v4_csv))
# }

attach(NFL_Play_by_Play_2009_2017_v4_csv)

#Identifies rows where Yards.Gained is not equal to difference between yrdline100 and the previous yrdline100
Yards.Gained_discrepent <-
  which(Yards.Gained != yrdline100 - lead(yrdline100))

#Displays the PlayType for rows with discrepancies in Yards.Gained
PlayType[Yards.Gained_discrepent]

#Displays the unique PlayType values for rows with discrepancies in Yards.Gained
unique(PlayType[Yards.Gained_discrepent])

#Displays a table of PlayType frequencies for rows with discrepancies in Yards.Gained
table(PlayType[Yards.Gained_discrepent])

#Adjusts Penalty.Yards based on whether the team with possession of the ball committed the penalty or not.
#NFL_Play_by_Play_2009_2017_v4_csv$Penalty.Yards <-
#  ifelse(PenalizedTeam == posteam, -Penalty.Yards, Penalty.Yards)

ind <-
  which(
    Yards.Gained != yrdline100 - lead(yrdline100) - Penalty.Yards &
      PlayType == "Run" & lead(PlayType) != "Quarter End"
  )

View(
  NFL_Play_by_Play_2009_2017_v4_csv[sort(c(ind, ind + 1, ind + 2)),] %>%
    select(
      yrdline100,
      Yards.Gained,
      ydsnet,
      PlayType,
      Penalty.Yards,
      PenalizedTeam,
      posteam,
      desc
    )
)

#Probably need to remove Quarter End plays when Quarter is 1 and 3

#investigate Penalty.Yards, What is the pattern
#I closed and reopend R and the Penalty.Yards went away
#Meeting at 2:00 pm Thursday January 11th.

View(NFL_Play_by_Play_2009_2017_v4_csv %>%
       filter(Penalty.Yards < 0) %>%
  select(yrdline100, Yards.Gained, PlayType, PenaltyType, Penalty.Yards, PenalizedTeam, posteam, desc))

#Check for positive Penalty Yards bases on Penalized team and Possession team.
unique(PenaltyType[Penalty.Yards>0 & posteam == PenalizedTeam])


#Check for negative Penalty Yards bases on Penalized team and Possession team.
unique(PenaltyType[Penalty.Yards<0 & posteam != PenalizedTeam])

#Determine what ydsnet actually measures.
#select all variables with yds in name.

cname2017[str_detect(tolower(cname2017), "yards")]
# "Yards.Gained"    "AirYards"        "YardsAfterCatch" "Penalty.Yards"  

cname2017[str_detect(tolower(cname2017), "yds")]
# "ydstogo" "ydsnet" 

cname2017[str_detect(tolower(cname2017), "yrd")]
# "yrdln"      "yrdline100"

yards.set <-
  select(
    NFL_Play_by_Play_2009_2017_v4_csv,
    yrdline100,
    yrdln,
    Yards.Gained,
    ydsnet,
    PlayType,
    desc,
    AirYards,
    YardsAfterCatch,
    Penalty.Yards,
    ydstogo
  )
yards.set

#check for 1pt or 2pt conversion attempts.
#look at unique ydline100 values for different play types
#do summary of ydline100 for the playtype

#create own ydsnet variable with change in possession in mind. yrdline100 - (100 - yrdline100) 
new_ydsnet <- yrdline100 - (100 - yrdline100)

#posteam != lead(posteam) with same GameID, GameID == lead(GameID)
#check Quarter End for solutions

unique(PlayType)
play.type <- unique(PlayType)[1]
play.type

ind2 <- which(PlayType == play.type)
table(yrdline100[ind2])

View(NFL_Play_by_Play_2009_2017_v4_csv[ind2,] %>%
       select(yrdline100, Yards.Gained, PlayType, PenaltyType, Penalty.Yards, PenalizedTeam, posteam, desc))

#Yards.Gained for Kickoff is Return yardage.
pass <- unique(PlayType)[2]
pass

indpass <- which(PlayType == pass)
table(yrdline100[indpass])

run <- unique(PlayType)[3]
run

indrun <- which(PlayType == run)
table(yrdline100[indrun])

punt <- unique(PlayType)[4]
punt



indpunt <- which(PlayType == punt)
table(yrdline100[indpunt])

View(NFL_Play_by_Play_2009_2017_v4_csv[sort(c(indpunt-1, indpunt, indpunt+1)),] %>%
            select(yrdline100, Yards.Gained, Drive, PlayType, PenaltyType, Penalty.Yards, PenalizedTeam, posteam, desc))
#Similar to Kickoff, Yards.Gained is return yardage.

sack <- unique(PlayType)[5]
sack

indsack <- which(PlayType == sack)
table(yrdline100[indsack])

field_goal <- unique(PlayType)[6]
field_goal

indfield_goal <- which(PlayType == field_goal)
table(yrdline100[indfield_goal])

no_play <- unique(PlayType)[7]
no_play

indno_play <- which(PlayType == no_play)
table(yrdline100[indno_play])

quarter_end <- unique(PlayType)[8]
quarter_end

indquarter_end <- which(PlayType == quarter_end)
table(yrdline100[indquarter_end])
View(
  NFL_Play_by_Play_2009_2017_v4_csv[sort(c(indquarter_end - 1, indquarter_end, indquarter_end +
                                             1)), ] %>%
    select(
      yrdline100,
      Yards.Gained,
      PlayType,
      PenaltyType,
      Penalty.Yards,
      PenalizedTeam,
      posteam,
      desc
    )
)

two_minute_warning <- unique(PlayType)[9]
two_minute_warning

indtwo_minute_warning <- which(PlayType == two_minute_warning)
table(yrdline100[indtwo_minute_warning])

timeout <- unique(PlayType)[10]
timeout

indtimeout <- which(PlayType == timeout)
table(yrdline100[indtimeout])

extra_point <- unique(PlayType)[11]
extra_point

indextra_point <- which(PlayType == extra_point)
table(yrdline100[indextra_point])

qb_kneel <- unique(PlayType)[12]
qb_kneel

indqb_kneel <- which(PlayType == qb_kneel)
table(yrdline100[indqb_kneel])

end_of_game <- unique(PlayType)[13]
end_of_game

indend_of_game <- which(PlayType == end_of_game)
table(yrdline100[indend_of_game])

spike <- unique(PlayType)[14]
spike

ind_spike <- which(PlayType == spike)
table(yrdline100[ind_spike])

half_end <- unique(PlayType)[15]
half_end

ind_half_end <- which(PlayType == half_end)
table(yrdline100[ind_half_end])
View(
  NFL_Play_by_Play_2009_2017_v4_csv[sort(c(ind_half_end - 1, ind_half_end, ind_half_end +
                                             1, ind_half_end + 2)), ] %>%
    select(
      GameID,
      Date,
      yrdline100,
      Yards.Gained,
      PlayType,
      PenaltyType,
      Penalty.Yards,
      PenalizedTeam,
      posteam,
      desc, 
      Season
    )
)

#Check full context of kickoff where yrdline100 is 55+ or <= 15

table(lead(yrdline100[sort(c(ind2 + 1))]))

#check how to identify TD play ie pt total or something of that nature.
#Touchdown == 1 for next ind_td
#Difference between yrdline100[ind2] - yrdline100[ind2+1]

table(yrdline100[ind2] - yrdline100[ind2+1])
summary(yrdline100[ind2] - yrdline100[ind2+1])

#posteam change posteam != lead(posteam)
#change Possession team on a punt and flipping yrdline100 -> 100 - yrdline100
#Convert Drive incremented by 1 for punt play. Filter by TD and turnover. Drive number dynamic.

#Todo: Remove PlayType == Timeout and Two Minute Warning, checking continuity of yrdline100, Try and make sense of ydsnet, yrdline100 - lead(yrdline100), 
#Sum up Drives Cumulatively, yds, sacks

View(
  NFL_Play_by_Play_2009_2017_v4_csv[
    sort(c(
      indtwo_minute_warning - 1,
      indtwo_minute_warning,
      indtwo_minute_warning + 1
    )),
  ] %>%
    select(
      GameID,
      Date,
      yrdline100,
      Yards.Gained,
      PlayType,
      PenaltyType,
      Penalty.Yards,
      PenalizedTeam,
      posteam,
      desc,
      Season
    )
)

View(
  NFL_Play_by_Play_2009_2017_v4_csv[
    sort(c(
      indtimeout - 1,
      indtimeout,
      indtimeout + 1
    )),
  ] %>%
    select(
      GameID,
      Date,
      yrdline100,
      Yards.Gained,
      PlayType,
      PenaltyType,
      Penalty.Yards,
      PenalizedTeam,
      posteam,
      desc,
      Season
    )
)

NFL_Play_by_Play_2009_2017_v4_csv <-
  NFL_Play_by_Play_2009_2017_v4_csv %>%
  filter(!(PlayType %in% c("Timeout", "Two Minute Warning")))

#Identify Drive ending plays to new more detailed 
NFL_Play_by_Play_2009_2017_v4_csv$PlayType.new <-
  NFL_Play_by_Play_2009_2017_v4_csv$PlayType

NFL_Play_by_Play_2009_2017_v4_csv$PlayType.new[NFL_Play_by_Play_2009_2017_v4_csv$InterceptionThrown == 1 
                                               & NFL_Play_by_Play_2009_2017_v4_csv$PlayType == "Pass"
                                               ] <- "Interception"

#Reversals can happen with Touchdowns/Fumbles posteam same, score for opposition team goes up 6-8, GameID == lead(GameID)
def.td.ind <-
  which(
    NFL_Play_by_Play_2009_2017_v4_csv$Touchdown == 1 &
      NFL_Play_by_Play_2009_2017_v4_csv$posteam == lead(NFL_Play_by_Play_2009_2017_v4_csv$posteam) &
      NFL_Play_by_Play_2009_2017_v4_csv$Drive != lead(NFL_Play_by_Play_2009_2017_v4_csv$Drive) &
      !(
        NFL_Play_by_Play_2009_2017_v4_csv$qtr == 2 &
          lead(NFL_Play_by_Play_2009_2017_v4_csv$qtr == 3)
      ) &
      !(
        NFL_Play_by_Play_2009_2017_v4_csv$qtr == 4 &
          lead(NFL_Play_by_Play_2009_2017_v4_csv$qtr == 5)
      ) #&
      #NFL_Play_by_Play_2009_2017_v4_csv$DefTeamScore == lead(NFL_Play_by_Play_2009_2017_v4_csv$DefTeamScore)
  )  

NFL_Play_by_Play_2009_2017_v4_csv$PlayType.new[def.td.ind] <- "Defensive Touchdown"

#Offensive Touchdown approach relying on Touchdown indicator, Touchdown = 1 did not confirm there was an actual Touchdown.

# off.td.ind <- which(
#   NFL_Play_by_Play_2009_2017_v4_csv$Touchdown == 1 &
#     !(NFL_Play_by_Play_2009_2017_v4_csv$posteam == lead(NFL_Play_by_Play_2009_2017_v4_csv$posteam) &
#     NFL_Play_by_Play_2009_2017_v4_csv$Drive != lead(NFL_Play_by_Play_2009_2017_v4_csv$Drive) &
#     !(
#       NFL_Play_by_Play_2009_2017_v4_csv$qtr == 2 &
#         lead(NFL_Play_by_Play_2009_2017_v4_csv$qtr == 3)
#     ) &
#     !(
#       NFL_Play_by_Play_2009_2017_v4_csv$qtr == 4 &
#         lead(NFL_Play_by_Play_2009_2017_v4_csv$qtr == 5)
#     )) #&
#   #NFL_Play_by_Play_2009_2017_v4_csv$DefTeamScore == lead(NFL_Play_by_Play_2009_2017_v4_csv$DefTeamScore)
# )  
# 
# NFL_Play_by_Play_2009_2017_v4_csv$PlayType.new[off.td.ind] <- "Offensive Touchdown"

#Need to take care of Def TD that are last play in the half. 

#Drive Outcome Summary
drive_by_drive <- NFL_Play_by_Play_2009_2017_v4_csv %>%
  group_by(GameID, Drive) %>%
  summarise(
    n.passattempts = sum(PassAttempt),
    n.completions = sum(PassOutcome == "Complete", na.rm = TRUE),
    n.incompletions = sum(PassOutcome == "Incomplete Pass", na.rm = TRUE),
    n.rush = sum(RushAttempt),
    n.sacks = sum(Sack),
    n.fumbles = sum(Fumble),
    first.downs.gained = sum(FirstDown, na.rm = TRUE),
    Drive_outcome = ifelse(
      tail(PlayType.next, 1) == "Punt",
      tail(PlayType.next, 1),
      tail(PlayType.new, 1)
    ),
    last_play = tail(desc, 1),
    last_play_type = tail(PlayType, 1),
    last_down = tail(down, 1)
  )

View(drive_by_drive)
#use for summarizing data, if drive ended in punt spit out
NFL_Play_by_Play_2009_2017_v4_csv$ydsnet.next <- lead(NFL_Play_by_Play_2009_2017_v4_csv$ydsnet)

NFL_Play_by_Play_2009_2017_v4_csv$PlayType.next <- lead(NFL_Play_by_Play_2009_2017_v4_csv$PlayType)

#What pieces to involve to identify an interception and fumble.
NFL_Play_by_Play_2009_2017_v4_csv$change.of.pos <-
  NFL_Play_by_Play_2009_2017_v4_csv$posteam != lead(NFL_Play_by_Play_2009_2017_v4_csv$posteam) &
  NFL_Play_by_Play_2009_2017_v4_csv$GameID == lead(NFL_Play_by_Play_2009_2017_v4_csv$GameID) &
  !(
    NFL_Play_by_Play_2009_2017_v4_csv$qtr == 2 &
      lead(NFL_Play_by_Play_2009_2017_v4_csv$qtr == 3)
  ) &
  !(
    NFL_Play_by_Play_2009_2017_v4_csv$qtr == 4 &
      lead(NFL_Play_by_Play_2009_2017_v4_csv$qtr == 5)
  )

#How do we figure out off td that have been REVERSED?
#ExPointResult Not NA and TwoPointConv
off.td.ind.2 <- which(
  #NFL_Play_by_Play_2009_2017_v4_csv$Touchdown == 1 &
    !(NFL_Play_by_Play_2009_2017_v4_csv$posteam == lead(NFL_Play_by_Play_2009_2017_v4_csv$posteam) &
        NFL_Play_by_Play_2009_2017_v4_csv$Drive != lead(NFL_Play_by_Play_2009_2017_v4_csv$Drive) &
        !(
          NFL_Play_by_Play_2009_2017_v4_csv$qtr == 2 &
            lead(NFL_Play_by_Play_2009_2017_v4_csv$qtr == 3)
        ) &
        !(
          NFL_Play_by_Play_2009_2017_v4_csv$qtr == 4 &
            lead(NFL_Play_by_Play_2009_2017_v4_csv$qtr == 5)
        )) #&
  #NFL_Play_by_Play_2009_2017_v4_csv$DefTeamScore == lead(NFL_Play_by_Play_2009_2017_v4_csv$DefTeamScore)
  & (!is.na(lead(NFL_Play_by_Play_2009_2017_v4_csv$ExPointResult)) | !is.na(lead(NFL_Play_by_Play_2009_2017_v4_csv$TwoPointConv))) &
    NFL_Play_by_Play_2009_2017_v4_csv$PlayType != "No Play" &
    !(NFL_Play_by_Play_2009_2017_v4_csv$Touchdown == 0)
  
) 

NFL_Play_by_Play_2009_2017_v4_csv$PlayType.new[off.td.ind.2] <- "Offensive Touchdown"

#!def.td.ind + 1
off.td.new <- NFL_Play_by_Play_2009_2017_v4_csv$PlayType.new[(
  !is.na(NFL_Play_by_Play_2009_2017_v4_csv$ExPointResult) |
    !is.na(NFL_Play_by_Play_2009_2017_v4_csv$TwoPointConv)
)] <- "Offensive Touchdown"
