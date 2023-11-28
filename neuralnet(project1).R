#install packages
install.packages('tidyverse')
library(tidyverse)
install.packages('neuralnet')
library(neuralnet)

#read in the data
#THIS IS THE OLD WAY TO READ IN THE DATA (I HAD MANUALLY ADDED THE COLNAMES IN NOTEPAD BEFORE)
#shroom <- read.table('agaricus-lepiota.data', fileEncoding="UTF-8", sep=",", header=TRUE)
#View(shroom)

#read in data
shroom <- read.table("agaricus-lepiota-original.data", fileEncoding="UTF-8", sep=",", header=FALSE)

#added headers to dataframe
colnames(shroom) <- c("Classes","Cap.Shape","Cap.Surface","Cap.Color","Bruises","Odor",
                  "Gill.Attachment","Gill.Spacing","Gill.Size","Gill.Color","Stalk.Shape",
                  "Stalk.Root","Stalk.Surface.Above.Ring","Stalk.Surface.Below.Ring",
                  "Stalk.Color.Above.Ring","Stalk.Color.Below.Ring","Veil.Type","Veil.Color",
                  "Ring.Number","Ring.Type","Spore.Print.Color","Population","Habitat")

#see how many "?"'s are in Stalk.Root
missingsr <- subset(shroom, shroom$Stalk.Root=="?")
View(missingsr)
#remove Stalk.Root
shroom1 <- subset(shroom, select = -c(Stalk.Root))
View(shroom1)

#this is kind of a summary for shroom1
str(shroom1)

#show the count of each value in each column
a<-names(shroom1)
for(i in a){
  print(i)
  print(table(shroom1[,i]))
  cat("\n\n")
}
#delete veil type
shroom2 <- subset(shroom1, select = -c(Veil.Type))
View(shroom2)

#show the count without veil type
a<-names(shroom2)
for(i in a){
  print(i)
  print(table(shroom2[,i]))
  cat("\n\n")
}


# NEURAL NETWORK STUFF

#name data something better
shroomdata <- shroom2
shroomdata <- as.data.frame(unclass(shroomdata), stringsAsFactors=TRUE)

#convert p and e to integers 1 and 0 respectively in shroomdata
shroomdata$Classes <- as.integer(shroomdata$Classes == 'p')

#convert all characters to integers
shroomdata <-sapply(shroomdata, as.numeric)

#generate random number so results are same every time
set.seed(500)

#Split the dataset into test and train sets
index <- sample(1:nrow(shroomdata), round(0.7 * nrow(shroomdata)))
train <- shroomdata[index, ]
test <- shroomdata[-index, ]

#formula USE IT
fmla <- Classes~Cap.Shape+Cap.Surface+Cap.Color+Bruises+Odor+Gill.Attachment+Gill.Spacing+Gill.Size+Gill.Color+Stalk.Shape+Stalk.Surface.Above.Ring+Stalk.Surface.Below.Ring+Stalk.Color.Above.Ring+Stalk.Color.Below.Ring+Veil.Color+Ring.Number+Ring.Type+Spore.Print.Color+Population+Habitat

#train the neural net
perceptronModel <- neuralnet(fmla, data = train, hidden = 3, learningrate = 0.1, linear.output = FALSE,)
pred <- neuralnet::compute(perceptronModel, test)
yhat <- pred$net.result
yhat <- round(yhat, 0)

#make confusion matrix
cm <- table(test[,1], yhat)
cm

#plot perceptron model
plot(perceptronModel)
