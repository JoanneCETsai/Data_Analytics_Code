#classification trees with acquisition data

frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw7_classification_trees/aquisitionacceptanceCART.csv")
head(frame)

ctout <- ctree(Accept ~ .,data=frame) # Recall that the . indicates that R should use all the 
# variables in the dataset. 

ctpred <- predict(ctout, data = frame)
mean(ctpred == frame$Accept)
plot(ctout)
