# We use data from http://www.football-data.co.uk/englandm.php, this is updated weekly
# The descriptions of variables are in the file http://www.football-data.co.uk/notes.txt

# Loading packages required for the analysis:
# install.packages("devtools")
library(devtools)
# install.packages('ggplot2')
library(ggplot2)

# Set Working Directory
setwd("~/University/Dissertation") # Uni Computer


# Reading in Data 

PL1516 <- read.csv('PL1516.csv')  # Loading Data
PL1617 <- read.csv('PL1617.csv')
PL1718 <- read.csv('PL1718.csv')
PL1819 <- read.csv('PL1819.csv')

# Combining Data Sets
PL <- rbind(PL1516,PL1617,PL1718,PL1819)

mean(PL$FTHG)  # We see the home and away goals are different values by a significant amount
mean(PL$FTAG)

# Try plotting the distribution to see the way the goals vary
par(mfrow = c(2,1))  # Changes number of graphs on plot
barplot(prop.table(table(PL$FTHG)), main = 'Home Goals', xlab = 'Goals', ylab = 'Frequency', ylim = c(0,0.4))
barplot(prop.table(table(PL$FTAG)), main = 'Away Goals', xlab = 'Goals', ylab = 'Frequency', ylim = c(0,0.4))
par(mfrow = c(1,1))

# We assume the home and away goals are independently distributed, so the goals form independent samples
t.test(PL$FTHG,PL$FTAG,var.equal = FALSE)  # This gives statistical evidence of home advantage


## POISSON MODEL

poisson_model <- 
  rbind(
    data.frame(goals=PL$FTHG,
               team=PL$HomeTeam,
               opponent=PL$AwayTeam,
               home=1),
    data.frame(goals=PL$FTAG,
               team=PL$AwayTeam,
               opponent=PL$HomeTeam,
               home=0)) 
  
Mod1 <- glm(goals ~ home + team +opponent, family=poisson(link=log),data=poisson_model)

# Can predict with this model now using e.g.




## Wrapper Function: Takes in the weeks fixtures in a data frame and outputs in the data frame the weeks likely results


FixturesGW8 <- data.frame( Team = c('Brighton','Crystal Palace','Leicester','Man United','Southampton','Burnley','Watford','Tottenham','Fulham','Liverpool'), Opponent = c('West Ham','Wolves','Everton','Newcastle','Chelsea','Huddersfield','Bournemouth','Cardiff','Arsenal','Man City') )  # This line should be changed for predictions to have home teams in the first set and away teams int he second









Fixtures <- FixturesGW8
HomeScore <- rep(NA,dim(Fixtures)[1])
AwayScore <- rep(NA,dim(Fixtures)[1])  # Setting empty vectors

for ( i in 1:dim(Fixtures)[1] ){
  
   HomeScore[i] =  round(predict(Mod1, data.frame(home = 1, team= Fixtures$Team[i], opponent = Fixtures$Opponent[i]), type = 'response') )
   AwayScore[i] =  round(predict(Mod1, data.frame(home = 0, team= Fixtures$Opponent[i], opponent = Fixtures$Team[i]), type = 'response') )  # Making Predictions
  
}
Predictions <- data.frame(HomeGoals = HomeScore, AwayGoals = AwayScore)
Predictions <- cbind(Fixtures, Predictions)   # Wrapping into nice data frame
Predictions






