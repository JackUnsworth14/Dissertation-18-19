LeagueTablePredict <- function(Which.Model) {   # Call this with the model you wish to predict the table for
  
  FutureSeasonFixtures <- PL1819[c(11:dim(PL1819)[1]),c(3,4)]
  FutureSeasonFixtures <- data.frame(Team = FutureSeasonFixtures$HomeTeam , Opponent = FutureSeasonFixtures$AwayTeam)
  Predictions <- Predict1(FutureSeasonFixtures, Mod1)
  Predictions <- cbind(Predictions, data.frame(HTPoints = 0, ATPoints = 0) )     # Setting up points vectors
  TeamList <- data.frame( Teams = c( levels(Predictions$Team)))
  LeagueTable <- data.frame(Teams = TeamList, Point.Total = 0, GoalDifference = 0)   # Setting up league table
  
  for(i in 1:( dim(PL1819)[1] - 10 ) ) {
    
    if (Predictions$HomeGoals[i] > Predictions$AwayGoals[i]) {
      Predictions$HTPoints[i] <- 3
      Predictions$ATPoints[i] <- 0
      Winner <- which(LeagueTable$Teams == Predictions$Team[i])   # Finding the winning team in the league table
      Loser <- which(LeagueTable$Teams == Predictions$Opponent[i])   # Finding the losing team in the league table
      LeagueTable$Point.Total[Winner] <- LeagueTable$Point.Total[Winner] + 3   # Adding the points to the league table
      LeagueTable$GoalDifference[Winner] <- LeagueTable$GoalDifference[Winner] + round(Predictions$HomeGoals[i]) - round(Predictions$AwayGoals[i])
      LeagueTable$GoalDifference[Loser] <- LeagueTable$GoalDifference[Loser] + round(Predictions$AwayGoals[i]) - round(Predictions$HomeGoals[i])
    }
    else  if (Predictions$HomeGoals[i] < Predictions$AwayGoals[i]) {
      Predictions$HTPoints[i] <- 0
      Predictions$ATPoints[i] <- 3
      Winner <- which(LeagueTable$Teams == Predictions$Opponent[1])   # Finding the winning team in the league table
      Loser <- which(LeagueTable$Teams == Predictions$Teams[i])   # Finding the losing team in the league table
      LeagueTable$Point.Total[Winner] <- LeagueTable$Point.Total[Winner] + 3   # Adding the points to the league table
      LeagueTable$GoalDifference[Winner] <- LeagueTable$GoalDifference[Winner] - round(Predictions$HomeGoals[i]) + round(Predictions$AwayGoals[i])
      LeagueTable$GoalDifference[Loser] <- LeagueTable$GoalDifference[Loser] + round(Predictions$HomeGoals[i]) - round(Predictions$AwayGoals[i])
    }
    else  if (Predictions$HomeGoals[i] == Predictions$AwayGoals[i]) {
      Predictions$HTPoints[i] <- 1
      Predictions$ATPoints[i] <- 1
      k <- which(LeagueTable$Teams == Predictions$Team[i])   # Finding the team in the league table
      l <- which(LeagueTable$Teams == Predictions$Opponent[i])
      LeagueTable$Point.Total[k] <- LeagueTable$Point.Total[k] + 1   # Adding the points to the league table
      LeagueTable$Point.Total[l] <- LeagueTable$Point.Total[l] + 1   # Adding the points to the league table
    }
    else {
      warning('Error in goals predictions')
    }
    
    
  }
  LeagueTable <- cbind(data.frame(Position = c(1:20), LeagueTable[order(-LeagueTable$Point.Total),] ))
  return(LeagueTable)    # Return LeagueTable for the League Table according to predictions, or Predictions for the math results
  
  
}