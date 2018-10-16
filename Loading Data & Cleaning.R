# We use data from http://www.football-data.co.uk/englandm.php, this is updated weekly
# The descriptions of variables are in the file http://www.football-data.co.uk/notes.txt

# Loading packages required for the analysis:
install.packages("devtools")
library(devtools)
install.packages('ggplot2')
library(ggplot2)
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

