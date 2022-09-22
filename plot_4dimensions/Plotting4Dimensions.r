install.packages("plot3D")
library("plot3D")  
#3D plot of the Data without DC  
#load the data frame, assign each attribute to a variable:
frameNoDC<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/plot_4dimensions/FourAttributeDataNoDC.csv", row.names=1)
x<-frameNoDC$Normalized.2015.spending
y<-frameNoDC$Normalized.GPD.per.Capita
z<-frameNoDC$Normalized.Poverty.Rate 
a<-frameNoDC$Normalized.violent.crime

#### R has a lot of options to customize plots. Take a look at the help page 
# for scatter3D to see what the arguments in the function below mean, 
# and to see some of the other plotting options. 
scatter3D(x = x, y = y, z = z, color=a, phi=0, theta=25, 
          bty="g", xlab="Norm. Educ. Spend.", 
          ylab="Norm. GDP per Cap.", zlab="Norm. Pov. Rate  ", 
          clab="Norm. Viol. Crime",type="h", ticktype="detailed")
text3D(x,y,z,labels=rownames(frameNoDC), add=TRUE, colkey=FALSE, cex=0.5)
