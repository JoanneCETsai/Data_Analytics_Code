#SVM Kernel Example
library(plotly)

frame<-read.csv("ExampleKernel.csv")
head(frame)
plot(frame$x, frame$y, col=frame$Group+1)
plot_ly(x=frame$x, y=frame$y, z=frame$kernalvalue, 
type="scatter3d", mode = "markers", color=frame$Group+1)

library(e1071)
frame$Group=as.factor(frame$Group) # Make this group a factor, so each category in the group 
# is a level of the factor. 
result<-svm(Group~x+y+kernalvalue, kernel= "linear", data=frame) #kernel = "linear" tells R 
#to use a linear kernel. The default is a radial kernel. 
predWorth<-predict(result, data=frame) # Predict the groups based on the svm model
table(predWorth, frame$Group) # Compare predictions to your data
