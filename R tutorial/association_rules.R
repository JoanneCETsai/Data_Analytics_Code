#Association Rules
#F. Maxwell Harper and Joseph A. Konstan. 2015. The MovieLens Datasets: History and Context. 
#ACM Transactions on Interactive Intelligent Systems (TiiS) 5, 4: 19:1????"19:19.?https://doi.org/10.1145/2827872

install.packages('https://cran.rstudio.com/bin/macosx/contrib/4.1/arules_1.7-4.tgz') #怎麼不行

library(Matrix)
library(arules)
library(arulesViz)
library(grid)
library(ggplot2)


moviesdataset<-read.transactions("/Users/chiaentsai/Desktop/Data Analytics/R tutorial/moviedataset.csv",
                                 format="basket", sep=",", quote = "")  
print(moviesdataset)


itemFrequencyPlot(moviesdataset, topN=10, type="absolute")
frequentItems<-eclat(moviesdataset, parameter=list(supp=0.2,maxlen=5))
inspect(head(frequentItems, 10))# just show the first 10 item sets

install.packages("arulesViz")
library("arulesViz")
rules <- apriori (moviesdataset, parameter = list(supp = 0.20, conf = 0.4, minlen=2, maxlen=5)) # ignore rules null lhs and movie in rhs
rules_conf <- sort (rules, by="lift", decreasing=TRUE) # sort rules from best to worst based on their lift scores
inspect(head(rules_conf, 10)) #look at the first 10 rules
plot(rules_conf)

#plot by order of the rules (number of items in the rule)
plot(rules, method="two-key plot")


#interactive interrogration of the plot
sel <- plot(rules, measure=c("support", "lift"), shading = "confidence", interactive=TRUE )


# table of rules
plot(rules, method="grouped")

#suppose I want to see the rules with a right hand side of The Lord of the Rings: The Return of the King (2003)
rules <- apriori (moviesdataset, parameter = list(supp = 0.20, conf = 0.4), appearance=list(rhs=c("Lord of the Rings: The Return of the King  The (2003)"))) 
rules_conf <- sort (rules, by="lift", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf, 13)) # write all 13 rules
plot(rules_conf)

#suppose I want to see the rules with a left hand side of The Lord of the Rings: The Return of the King (2003)
rules <- apriori (moviesdataset, parameter = list(supp = 0.20, conf = 0.4), appearance=list(lhs=c("Lord of the Rings: The Return of the King  The (2003)"))) 
rules_conf <- sort (rules, by="lift", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))
plot(rules_conf)
