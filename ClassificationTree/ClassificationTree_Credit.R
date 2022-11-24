#Classification trees
frame<-read.csv("CreditNaiveBayes.csv")
print(frame)

library(partykit)
ctout <- ctree(Group ~ Credit.Borrows.Score+Debt.to.Income,data=frame)
ctpred <- predict(ctout,frame) #This predicts the categories the borrowers will fall into. Note that 
# for demonstration purposes here we’re making predictions with the same set of data we used # to make the classification tree, which you should not do in practice.
print(ctpred)
mean(ctpred == frame$Group) #Check the percentage of time that the classification tree correctly classifies a data point
plot(ctout) #plot your classification tree
