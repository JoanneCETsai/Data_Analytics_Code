#Factor Analysis
#psych package
#GPArotation
install.packages("psych")
install.packages("GPArotation")
library(psych)
library(GPArotation)

frame<-read.csv("/Users/chiaentsai/Desktop/Data Analytics/R tutorial/factor_analysis/TestScores.csv")
#1 Factor
SolFact<-factanal(frame, factors=1, cvar=FALSE, method="mle")
SolFact
#2 FACTORS
SolFact<-factanal(frame, factors=2, method="mle")
SolFact

#Remove Office Skills
frame<-read.csv("TestScoresNoOfficeSkills.csv")
SolFact<-factanal(frame, factors=2, method="mle")
SolFact


#Suppose no Rotations
SolFact<-factanal(frame, factors=2, rotation="none")
SolFact
