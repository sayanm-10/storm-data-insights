#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : 
#  First Name : Sayan
#  Last Name  : Mukherjee
#  Id			    : 10430998
#  Date       : 04/20/2018
#  Comments   :

rm(list=ls())
#################################################

# Convert to factors for Random Tree

library(dplyr)
storm_data_relevant=storm_data_relevant %>% mutate_if(is.character, as.factor)

# Apply Random Forest
#install.packages("randomForest")
library(randomForest)

set.seed(100) # to achieve same result consistently

index <- sort(sample(nrow(storm_data_relevant), round(.30*nrow(storm_data_relevant))))
training <- storm_data_relevant[-index,]
test <- storm_data_relevant[index,]

rForest <- randomForest(INJURIES~., data=training, importance=TRUE, ntree=1000)
varImpPlot(rForest)

prediction <- predict(rForest, test)
wrong <- (test[,"INJURIES"] != prediction)
error_rate <- sum(wrong)/length(wrong)

# table(Prediction = prediction, Actual = test$INJURIES)
# importance(rForest)
