#Gymnastics analysis all four events
library(factoextra)
frame1<-read.csv("/Users/chiaentsai/Desktop/Data Analytics/R tutorial/pca/GymnasticsData.csv", row.names = 1)
cor(frame1) #leaving ashton in generates issues because of missing data, which we see in the correlation matrix


#Gymnastics PCA analysis all four events NO Ashton Locklear
library(FactoMineR)
frame<-read.csv("/Users/chiaentsai/Desktop/Data Analytics/R tutorial/PCA in R example 2/GymnasticsDataNoAshtonLocklear.csv", row.names = 1)
cor(frame) #correlation matrix with Ashton for comparison with when she is not included
Sol<-PCA(frame, scale.unit=TRUE, graph=TRUE)
Sol$eig #print eigenvalues
fviz_eig(Sol) #Scree plot
Sol$var #prints coord, cor, cos2 and contrib-eigenvectors are coord/sqrt(eigenvalue)
Sol$ind$coord #individuals expressed by PCAs
fviz_pca_var(Sol, col.var="cos2") #correlation circle with cos2
fviz_pca_biplot(Sol, col.var="cos2") #coordinates of individuals in PCA space and correlations between PCA and variables
fviz_pca_biplot(Sol, col.ind="cos2") #coordinates of individuals in PCA space and correlations between PCA and variables
