#read in data
df <- read.table("agaricus-lepiota-original.data", fileEncoding="UTF-8", sep=",", header=FALSE)

#added headers to dataframe
colnames(df) <- c("Classes","Cap.Shape","Cap.Surface","Cap.Color","Bruises","Odor",
                  "Gill.Attachment","Gill.Spacing","Gill.Size","Gill.Color","Stalk.Shape",
                  "Stalk.Root","Stalk.Surface.Above.Ring","Stalk.Surface.Below.Ring",
                  "Stalk.Color.Above.Ring","Stalk.Color.Below.Ring","Veil.Type","Veil.Color",
                  "Ring.Number","Ring.Type","Spore.Print.Color","Population","Habitat")

# Function to get unique values for each column
get_unique_values <- function(column) {
  unique_values <- unique(column)
}

lapply(df, get_unique_values)

#removed the column Stalk.root because had over 2.5k missing values
df = subset(df, select = -c(Stalk.Root))

#veil.type only has a single value so its of no use so deleted column
df = subset(df, select = -c(Veil.Type))

#all variables are initally set to chr and we need them as factors for the decision tree
factorDf <- df
factorDf <- as.data.frame(unclass(df), stringsAsFactors = TRUE)

#split data into train and test
index <- sample(1:nrow(factorDf), round(0.8 * nrow(factorDf)))
train <- factorDf[index,]
test <- factorDf[-index,]

#formula for y based on all variables of dataframe
fmla <- Classes~Cap.Shape+Cap.Surface+Cap.Color+Bruises+Odor+Gill.Attachment+Gill.Spacing+Gill.Size+Gill.Color+Stalk.Shape+Stalk.Surface.Above.Ring+Stalk.Surface.Below.Ring+Stalk.Color.Above.Ring+Stalk.Color.Below.Ring+Veil.Color+Ring.Number+Ring.Type+Spore.Print.Color+Population+Habitat

#package for decision tree
install.packages("party")
library(party)

#make decision tree from party library
tree<-ctree(fmla, data=train)

#plots the tree
plot(tree)

#shows confusion matrix of tree
pred <- predict(tree, test)
cm <- table(pred, test$Classes)
cm

factorDf <- df
factorDf <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
intDf <- sapply(factorDf, as.integer)
str(intDf)

#-----------------------------------------------------
install.packages("neuralnet")
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

#formula
fmla <- Classes~Cap.Shape+Cap.Surface+Cap.Color+Bruises+Odor+Gill.Attachment+Gill.Spacing+Gill.Size+Gill.Color+Stalk.Shape+Stalk.Surface.Above.Ring+Stalk.Surface.Below.Ring+Stalk.Color.Above.Ring+Stalk.Color.Below.Ring+Veil.Color+Ring.Number+Ring.Type+Spore.Print.Color+Population+Habitat

#train the neural net
perceptronModel <- neuralnet(fmla, data = train, hidden = c(4,3), learningrate = 0.1, linear.output = FALSE,)

#get the predicted values for the test dataframe
pred <- neuralnet::compute(perceptronModel, test)

#grab the result from its prediction and round to either 1 or 0
yhat <- pred$net.result
yhat <- round(yhat, 0)

#get confusion matrix of test vs predicted
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
