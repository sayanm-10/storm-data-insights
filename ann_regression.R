library("neuralnet")
library(nnet)

softmax <- function (x) { log(1 + exp(x)) }

storm_data.ann <- storm_data
# storm_data.ann <- storm_data.ann[, -grep("DAMAGE_CROPS|DAMAGE_PROPERTY", colnames(storm_data))]
index <- seq(1, nrow(storm_data.ann), by=5)
test <- storm_data.ann[index, ]
training <- storm_data.ann[-index, ]
training <- training[1:(nrow(training) / 4), ]
# training <- training[1:(nrow(training) / 2), ]

training <- as.data.frame(training)
test <- as.data.frame(test)

eq <- DAMAGE_PROPERTY ~
  MONTH_NAME +
  STATE_NEW_JERSEY +
  STATE_FLORIDA +
  STATE_OHIO +
  STATE_NEBRASKA +
  STATE_GEORGIA +
  STATE_INDIANA +
  STATE_VIRGINIA +
  STATE_GULF_OF_MEXICO +
  STATE_ARKANSAS +
  STATE_OKLAHOMA +
  STATE_ATLANTIC_NORTH +
  STATE_PENNSYLVANIA +
  STATE_WISCONSIN +
  STATE_MONTANA +
  STATE_MISSOURI +
  STATE_KANSAS +
  STATE_ATLANTIC_SOUTH +
  STATE_ALABAMA +
  STATE_NEVADA +
  STATE_NEW_MEXICO +
  STATE_ILLINOIS +
  STATE_TEXAS +
  STATE_WYOMING +
  STATE_IOWA +
  STATE_ARIZONA +
  STATE_MASSACHUSETTS +
  STATE_SOUTH_CAROLINA +
  STATE_MINNESOTA +
  STATE_NORTH_CAROLINA +
  STATE_WASHINGTON +
  STATE_KENTUCKY +
  STATE_MARYLAND +
  STATE_LAKE_SUPERIOR +
  STATE_LOUISIANA +
  STATE_CALIFORNIA +
  STATE_DISTRICT_OF_COLUMBIA +
  STATE_DELAWARE +
  STATE_WEST_VIRGINIA +
  STATE_MISSISSIPPI +
  STATE_TENNESSEE +
  STATE_COLORADO +
  STATE_RHODE_ISLAND +
  STATE_CONNECTICUT +
  STATE_ALASKA +
  STATE_OREGON +
  STATE_NEW_YORK +
  STATE_VERMONT +
  STATE_IDAHO +
  STATE_SOUTH_DAKOTA +
  STATE_NORTH_DAKOTA +
  STATE_LAKE_MICHIGAN +
  STATE_MICHIGAN +
  STATE_UTAH +
  STATE_LAKE_ERIE +
  STATE_MAINE +
  STATE_PUERTO_RICO +
  STATE_HAWAII +
  STATE_E_PACIFIC +
  STATE_NEW_HAMPSHIRE +
  STATE_VIRGIN_ISLANDS +
  STATE_LAKE_ST_CLAIR +
  STATE_GUAM +
  STATE_LAKE_HURON +
  STATE_AMERICAN_SAMOA +
  STATE_HAWAII_WATERS +
  STATE_LAKE_ONTARIO +
  STATE_ST_LAWRENCE_R

## METHOD 1
nn.fit <- neuralnet(
  DAMAGE_PROPERTY ~ .,
  data = training,
  # hidden = 20,
  # threshold = 0.001,
  # act.fct = softmax,
  # err.fct = "sse",
  linear.output = TRUE
)

nn.fit.comp <- compute(nn.fit, test[, nn.fit$model.list$variables])
nn.predictions <- as.numeric(nn.fit.comp$net.result)

nn.mse <- mean((test$DAMAGE_PROPERTY - nn.predictions) ^ 2)
nn.mse

plot(test$DAMAGE_PROPERTY, nn.predictions)

## METHOD 2
# nnet.fit <- nnet(
#   eq,
#   data = training,
#   size = 2,
#   linout = TRUE,
#   maxit = 1000
# )
# 
# nnet.predict <- predict(nnet.fit, newdata = test[, -grep("DAMAGE_PROPERTY|DAMAGE_CROPS", colnames(test))])
# nnet.mse <- mean(((test$DAMAGE_PROPERTY + test$DAMAGE_CROPS) - nnet.predict) ^ 2)
# nnet.mse
# 
# plot(test$DAMAGE_PROPERTY, nnet.predict)
