# First load the necessary packages
library(factoextra)
library(FactoMineR)

# Read in the data
frame<-read.csv("GymnasticsDataNoAshtonLocklear.csv", row.names = 1)

# Run PCA
Sol<-PCA(frame, scale.unit=TRUE, graph=TRUE)

# Create new data frames for prediction
SimoneNew = data.frame(Vault=31.2,Bars=29.4,Beam=30.1,Floor=29.15)
MorganNew = data.frame(Vault=28.8,Bars=29.15,Beam=27.65,Floor=27.7)

# Project Simone Biles and Morgan Hurd into the PCA space
predict(Sol,newdata=SimoneNew)
predict(Sol,newdata=MorganNew)