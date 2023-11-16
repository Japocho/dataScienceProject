#added header to agaricus-lepiota.data file for easier analysis
#read in data
df <- read.table("agaricus-lepiota.data", fileEncoding="UTF-8", sep=",", header=TRUE)

set.seed(2324532)
#removed the column Stalk.root because had over 2.5k missing values
df = subset(df, select = -c(Stalk.Root))

#gives you counts of characters per column
table(df$Bruises)

#veil.type only has a single value so its of no use so deleted column
df = subset(df, select = -c(Veil.Type))

#package for decision tree
library(party)

#all variables are initally set to chr and we need them as factors for the decision tree
factorDf <- df
factorDf <- as.data.frame(unclass(df), stringsAsFactors = TRUE)

factorDf
#split data into train and test
index <- sample(1:nrow(factorDf), round(0.8 * nrow(factorDf)))
train <- factorDf[index,]
test <- factorDf[-index,]

factorDf
#formula for y based on all variables of dataframe
fmla <- Classes~Cap.Shape+Cap.Surface+Cap.Color+Bruises+Odor+Gill.Attachment+Gill.Spacing+Gill.Size+Gill.Color+Stalk.Shape+Stalk.Surface.Above.Ring+Stalk.Surface.Below.Ring+Stalk.Color.Above.Ring+Stalk.Color.Below.Ring+Veil.Color+Ring.Number+Ring.Type+Spore.Print.Color+Population+Habitat

#make decision tree
tree<-ctree(fmla, data=train)

#plots the tree
plot(tree)

#shows confusion matrix of tree
pred <- predict(tree, test)
cm <- table(pred, test$Classes)
cm