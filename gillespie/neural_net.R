install.packages("neuralnet")
library(neuralnet)

#read in table
df <- read.table("agaricus-lepiota.data", fileEncoding="UTF-8", sep=",", header=TRUE)

#removed the column Stalk.root because had over 2.5k missing values
df = subset(df, select = -c(Stalk.Root))

#veil.type only has a single value so its of no use so deleted column
df = subset(df, select = -c(Veil.Type))

#set all variables to factors instead of strings
factorDf <- as.data.frame(unclass(df), stringsAsFactors = TRUE)

clean_df <- factorDf

#convert p and e to integers 1 and 0 respectively in shroomdata
clean_df$Classes <- as.integer(clean_df$Classes == 'p')

#convert all characters to integers
clean_df <-sapply(clean_df, as.numeric)

#Split the dataset into test and train sets
index <- sample(1:nrow(clean_df), round(0.80 * nrow(clean_df)))
train <- clean_df[index, ]
test <- clean_df[-index, ]

#formula for model
fmla <- Classes~Cap.Shape+Cap.Surface+Cap.Color+Bruises+Odor+Gill.Attachment+Gill.Spacing+Gill.Size+Gill.Color+Stalk.Shape+Stalk.Surface.Above.Ring+Stalk.Surface.Below.Ring+Stalk.Color.Above.Ring+Stalk.Color.Below.Ring+Veil.Color+Ring.Number+Ring.Type+Spore.Print.Color+Population+Habitat

#train the neural net
perceptronModel <- neuralnet(fmla, data = train, hidden = c(4,3),
                            learningrate = 0.1,
                            linear.output = FALSE)

#get predicted values
predictionDf <- neuralnet::compute(perceptronModel, test)
pred <- predictionDf$net.result

#it outputs values that are floats so we must round them up or down to get the prediction
pred <- round(yhat, 0)

#get confusion matrix
cm <- table(test[,1], pred)
cm

#plot out perceptronModel
plot(perceptronModel)