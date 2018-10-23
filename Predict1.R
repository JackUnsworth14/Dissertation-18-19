Predict1 <- function(Fixtures, Which.Model) {  # Fixtures input must be in the form above, also input the correct model
  
  HomeScore <- rep(NA,dim(Fixtures)[1])
  AwayScore <- rep(NA,dim(Fixtures)[1])  # Setting empty vectors
  HomeExpectation <- rep(NA,dim(Fixtures)[1])
  AwayExpectation <- rep(NA,dim(Fixtures)[1])
  HomeScoreLowerBoundary <- rep(NA,dim(Fixtures)[1])
  
  for ( i in 1:dim(Fixtures)[1] ){
    
    HomeScore[i] =  round(predict(Which.Model, data.frame(home = 1, team= Fixtures$Team[i], opponent = Fixtures$Opponent[i]), type = 'response') )
    AwayScore[i] =  round(predict(Which.Model, data.frame(home = 0, team= Fixtures$Opponent[i], opponent = Fixtures$Team[i]), type = 'response') )  # Making Predictions
    HomeExpectation[i] = predict(Which.Model, data.frame(home = 1, team= Fixtures$Team[i], opponent = Fixtures$Opponent[i]), type = 'response')
    AwayExpectation[i] = predict(Which.Model, data.frame(home = 0, team= Fixtures$Opponent[i], opponent = Fixtures$Team[i]), type = 'response')  
  }
  Predictions <- data.frame(HomeGoals = HomeScore, AwayGoals = AwayScore, HomeExpectation = HomeExpectation, AwayExpectation = AwayExpectation)
  Predictions <- cbind(Fixtures, Predictions)   # Wrapping into nice data frame
  return(Predictions)
}
