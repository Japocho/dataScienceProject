#added header to agaricus-lepiota.data file for easier analysis
#read in data
df <- read.table("agaricus-lepiota.data", fileEncoding="UTF-8", sep=",", header=TRUE)

#removed the column Stalk.root because had over 2.5k missing values
df = subset(df, select = -c(Stalk.Root))

#gives you counts of characters per column
table(df$Bruises)

#veil.type only has a single value so its of no use so deleted column
df = subset(df, select = -c(Veil.Type))
summary(df)

col_names<-names(df) #vector of column names
set.seed(1)
#displays counts for categorical variables by column
for(i in col_names){
  print(i)
  print(table(df[,i]))
  cat("\n")
}

dfByGillColor<- df[df$Gill.Color == "y",]
simple = subset(dfByGillColor, select = c(Gill.Color, Classes))
table(simple)

#package for decision tree
library(party)

#all variables are initally set to chr and we need them as factors for the decision tree
factorDf <- df
factorDf <- as.data.frame(unclass(df), stringsAsFactors = TRUE)

#split data into train and test
index <- sample(1:nrow(factorDf), round(0.8 * nrow(factorDf)))
train <- factorDf[index,]
test <- factorDf[-index,]

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

treeDf <- data.frame(seed=c(), accuracy=c())
for(i in 0:50)
{
  set.seed(i)
  index <- sample(1:nrow(factorDf), round(.80 * nrow(factorDf)))
  train <- factorDf[index,]
  test <- factorDf[-index,]
  nrow(train)
  nrow(test)
  print(paste("At seed", i)) 
  model<-ctree(fmla, data=train)
  pred <- predict(model, test)
  cm = table(test$Classes, pred)
  accuracy <- 1-((cm[2]+cm[3])/sum(cm))
  treeDf <- rbind(treeDf, list(i, accuracy))
  print(paste("added", i, accuracy))
}

treeDf

factorDf <- df
factorDf <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
intDf <- sapply(factorDf, as.integer)
str(intDf)


ans <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
ans <- sapply(ans, as.numeric)
ans

install.packages("neuralnet")
#-----------------------------------------------------
#name data something better
library(neuralnet)
shroomdata <- df
shroomdata <- factorDf
#convert p and e to integers 1 and 0 respectively in shroomdata
shroomdata$Classes <- as.integer(shroomdata$Classes == 'p')
#convert all characters to integers
shroomdata <-sapply(shroomdata, as.numeric)
#generate random number so results are same every time
set.seed(500)
#Split the dataset into test and train sets
index <- sample(1:nrow(shroomdata), round(0.80 * nrow(shroomdata)))
train <- shroomdata[index, ]
test <- shroomdata[-index, ]
nrow(test)
#formula USE IT
fmla <- Classes~Cap.Shape+Cap.Surface+Cap.Color+Bruises+Odor+Gill.Attachment+Gill.Spacing+Gill.Size+Gill.Color+Stalk.Shape+Stalk.Surface.Above.Ring+Stalk.Surface.Below.Ring+Stalk.Color.Above.Ring+Stalk.Color.Below.Ring+Veil.Color+Ring.Number+Ring.Type+Spore.Print.Color+Population+Habitat
#hidden was 0
#train the neural net
perceptronModel <- neuralnet(fmla, data = train, hidden = c(4,3), learningrate = 0.1, linear.output = FALSE,)
pred <- neuralnet::compute(perceptronModel, test)
yhat <- pred$net.result
yhat <- round(yhat, 0)
nrow(yhat)
cm <- table(test[,1], yhat)
cm
plot(perceptronModel)

#-------Support Vector Machines-----------------
install.packages('e1071') 
library(e1071) 

shroomdata <- factorDf
#convert p and e to integers 1 and 0 respectively in shroomdata
shroomdata$Classes <- as.integer(shroomdata$Classes == 'p')
#convert all characters to integers
shroomdata <-sapply(shroomdata, as.numeric)

index <- sample(1:nrow(shroomdata), round(0.80 * nrow(shroomdata)))
train <- shroomdata[index, ]
nrow(train)
test <- shroomdata[-index, ]
nrow(test)

fmla <- Classes~Cap.Shape+Cap.Surface+Cap.Color+Bruises+Odor+Gill.Attachment+Gill.Spacing+Gill.Size+Gill.Color+Stalk.Shape+Stalk.Surface.Above.Ring+Stalk.Surface.Below.Ring+Stalk.Color.Above.Ring+Stalk.Color.Below.Ring+Veil.Color+Ring.Number+Ring.Type+Spore.Print.Color+Population+Habitat

classifier = svm(formula = fmla, 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear')

y_pred = predict(classifier, test)
cm = table(test[,1], y_pred) 
cm
