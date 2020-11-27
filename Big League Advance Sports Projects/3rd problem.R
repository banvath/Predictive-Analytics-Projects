#Idea: Factors to keep in mind while ranking the teams

#Criteria1:

#No of wins in total. Usually we rank the teams based on the number of wins. But is it enough? Does it reflect the 
#Concerns: 
#What if a team has played fewer games but won many games. Win ration matters
#what if two teams have the same number of wins? 
#What if a team has played more number of games against weak teams and less against strong teams, hence registered more number of wins? Generally, this is the common case in Cricket or in some other sport, 
#but in Basketball, everyteam gets to play against every other team with equal number of games, so we could probably ignore this concern, but it's a good idea to keep in mind.


#Criteria2:
#What is two teams have the same number of wins, but one team is winning with a great margin? In that case, we need to keep in to account the margin with which each team is winning?

#Criteria 3:
#What is a team is winning with a good streak. This reflects consistency, strength in the team as well?

#we are calulcating the following things to cover above criterias.
#HW: Home Wins
#AW: Away Wins
#TW: Total Wins
#PPG: Points Per game
#OPPG: Opponents points Per Game
#diff: difference between PPG and OPPG
#LS: Longest Streak
#WR : Win ratio


#We could use each of the above factors to rank the teams. I am prioritizg the factors based on my undestanding of the game and my understanding of the strength of a factor's impact on the rank.
#First priority: TW
#Seconf Priority: WR
#Third Prioirity: diff
#Fourth Priority: AW
#Fifth Priority :  LS
library(readr)
data <- read_csv("C:/Users/banva/Desktop/Sports Project/ACCGames1819.csv")
View(data)


data[ (data['HomeScore'] > data['AwayScore']),"HomeWins"]= 1
data[ (data['HomeScore'] < data['AwayScore']),"HomeWins"]= 0

data[ (data['HomeScore'] < data['AwayScore']),"AwayWins"]= 1
data[ (data['HomeScore'] > data['AwayScore']),"AwayWins"]= 0



standings <-data.frame(names =character(0),HW = numeric(0), AW =integer(0) , TW = integer(0))

standings = data.frame(names = c("Notre Dame Fighting Irish", "North Carolina State Wolfpack", "Clemson Tigers","Boston College Eagles" ,   "Syracuse Orange" ,  "North Carolina Tar Heels" ,    
                                "Wake Forest Demon Deacons" ,"Florida State Seminoles" , "Miami (FL) Hurricanes"
                                 ,"Duke Blue Devils"   ,  "Virginia Tech Hokies" , "Louisville Cardinals"         
                                 , "Virginia Cavaliers" ,"Pittsburgh Panthers"  , "Georgia Tech Yellow Jackets"))

for(i in standings$names){
standings[standings$names == i,"HW"] =   print(nrow(data[data$HomeTeam == i & data$HomeWins ==1, ]))
standings[standings$names == i,"AW"] =   print(nrow(data[data$AwayTeam == i & data$AwayWins ==1, ]))
standings[standings$names == i,"PPG"] = mean(c(mean(data[data$HomeTeam == i, "HomeScore" ]$HomeScore) ,mean(data[data$AwayTeam == i, "AwayScore" ]$AwayScore)))    
standings[standings$names == i,"OPPG"] = mean(c(mean(data[data$HomeTeam == i, "AwayScore" ]$AwayScore) ,mean(data[data$AwayTeam == i, "HomeScore" ]$HomeScore)))    
standings[standings$names == i,"GP"] = nrow(data[data$HomeTeam == i | data$AwayTeam == i, ])
}

standings$TW = standings$HW +standings$AW
standings$diff = standings$PPG - standings$OPPG



#Find the longest streak
df = data[order(data$GameDate) ,]

for(i in standings$names){
  
  df[(df$HomeTeam == i & df$HomeWins == 1) | (df$AwayTeam == i & df$AwayWins == 1),"result" ]= 1
  df[(df$HomeTeam == i & df$HomeWins == 0) | (df$AwayTeam == i & df$AwayWins == 0),"result" ]= 0
  temp= df[(df$HomeTeam == i ) | (df$AwayTeam == i ), c("GameDate" ,"AwayTeam", "AwayWins", "HomeTeam" ,"HomeWins" , "result")]
  
  max_value=0
  count =0
  
  for(j in 1: (nrow(temp)-1))
    {
    f= temp$result[j]
    s= temp$result[j+1]
    
    diff= temp$result[j+1]-temp$result[j]
    
    if(diff== 0 & f !=0){
      count =count+1
    }else{
      count= 0
    }
    max_value = max(max_value, count)
    }

  standings[standings$names == i, "LS"] = max_value+1
  
}

standings$WR = standings$TW/standings$GP
standings =standings[order(-standings$TW , -standings$WR,-standings$diff, -standings$AW, -standings$LS),]
standings$rank = 1:nrow(standings)
rownames(standings) = 1:nrow(standings)

write.csv(standings, file = "ACCRankings1819.csv")
