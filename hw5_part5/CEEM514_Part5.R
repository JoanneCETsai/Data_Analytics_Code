library(pscl)

# Read in data
df <- read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw5_part5/AcquisitionAcceptance.csv")
colnames(df)
# Create interaction term between After and a Price variable of your choosing
# by replacing YOURVARHERE with the desired variable name
df$Interaction <- df$After * df$Price75

# Construct logistic regression with interaction term
model <- glm(Accept~Distance+HomeTenure+Education345+After+Price75+Price90+Price110+Price125+Interaction,
             family=binomial(link='logit'),data=df)

# Examine statistical significance of model coefficients
summary(model)

# Compute McFadden Pseudo R-Squared for Model Evaluation
pR2(model)['McFadden']


