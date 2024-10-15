#---------------------------#
#--------- SDS 313 ---------#
#------   Homework 1   -----#
#------- Sep. 7, 2024 ------#
#---------------------------#

# Import data set and assign name

getwd()
setwd("~/Desktop/SDS Intro")
bond <- read.csv('Homework1_Bonds.csv')
View(bond)

# Question 1a - How many of these bonds were approved by voters, and how many were defeated? 

totalApproved <- bond[bond$Result == "Carried",]
totalDenied <- bond[bond$Result == "Defeated",]

# Question 1b -Are there any differences in the rates of approved bonds across the four different government types?

totalApprovedCity <- nrow(bond[bond$Type == "CITY" & bond$Result == "Carried",])
rateApprovedCity <- totalApprovedCity/nrow(bond[bond$Type == "CITY",]) * 100

totalApprovedISD <- nrow(bond[bond$Type == "ISD" & bond$Result == "Carried",])
rateApprovedISD <- totalApprovedISD/nrow(bond[bond$Type == "ISD",]) * 100

totalApprovedCOUNTY <- nrow(bond[bond$Type == "COUNTY" & bond$Result == "Carried",])
rateApprovedCOUNTY <- totalApprovedCOUNTY/nrow(bond[bond$Type == "COUNTY",]) * 100

totalApprovedWD <- nrow(bond[bond$Type == "WD" & bond$Result == "Carried",])
rateApprovedWD <- totalApprovedWD/nrow(bond[bond$Type == "WD",]) * 100


identical(rateApprovedCity,rateApprovedISD,rateApprovedCOUNTY,rateApprovedWD) #show differences in rate

# Question 2a - Is the margin a bond was approved by related to its cost?

bond$Votes_Total <- bond$VotesFor + bond$VotesAgainst

# Question 2b - When and where did the bond measure with the highest voter turnout occur? What was it for?

maxTurnout <- max(bond$Votes_Total)
row = bond[bond$Votes_Total == maxTurnout,]
row 

# Question 3a - Create a subset of this dataset that contains the bond measures that were approved and had at least 100 total votes.

bond2 <- bond[bond$Result == "Carried" & bond$Votes_Total >= 100,]

# Question 3b - Next, create a new variable within the subset data frame that gives the percentage of total votes that were for the bond measure and make a graph of the distribution of this new variable.

bond2$RateFor <- (bond2$VotesFor/bond2$Votes_Total) * 100
boxplot(bond2$RateFor, main = 'Boxplot of Approved Bond Votes (not including low turnout)', xlab = 'Percent rate of Votes in Favor (%)', pch = 20, col = c('orange'),horizontal = TRUE)
fivenum(bond2$RateFor) # Distribution
sd(bond2$RateFor)

#Question 4 - Is the margin a bond was approved by related to its cost?

cor(bond2$RateFor, bond2$Amount) #Farther from 1, closer to 0 weaker the relationship 

plot(bond2$RateFor, bond2$Amount, main = "Bond Approval Rate Margins vs. Bond Cost Allocated", xlab = "Approval Margin (%)", ylab = "Cost ($)", pch=20)



