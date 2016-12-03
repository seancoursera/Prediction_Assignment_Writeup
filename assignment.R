library(dplyr)
library(caret)
library(ggplot2)
library(psych)
library(Factor)
install.packages("FactoMineR")

devtools::install_github("kassambara/factoextra")


pml_training <- read.csv('pml-training.csv')
pml_testing <- read.csv('pml-testing.csv')
pml_trd <- pml_training %>% 
    select(-starts_with('max_'), -starts_with('min_'), -starts_with('var_'), 
           -starts_with('avg_'), -starts_with('stddev_'), -starts_with('amplitude_'), -skewness_yaw_belt,
           -kurtosis_yaw_forearm, -skewness_yaw_forearm, -skewness_yaw_dumbbell, -kurtosis_yaw_dumbbell,
           -kurtosis_yaw_belt, -X, -user_name, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp,
           -kurtosis_roll_belt, -kurtosis_picth_belt, -skewness_roll_belt, -skewness_roll_belt.1,
           -kurtosis_roll_arm, -kurtosis_picth_arm, -kurtosis_yaw_arm, -skewness_roll_arm, -skewness_pitch_arm, 
           -skewness_yaw_arm, -kurtosis_roll_dumbbell, -kurtosis_picth_dumbbell, -skewness_roll_dumbbell, 
           -skewness_pitch_dumbbell, -kurtosis_roll_forearm, -kurtosis_picth_forearm, -skewness_roll_forearm, 
           -skewness_pitch_forearm, -new_window)

inPart <- createDataPartition(y = pml_trd$classe,
                              p=0.010, list = F)

part1 <- pml_trd[inPart,]

asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))
#convert all factors to numeric 
part1_1 <- data.frame(lapply(part1, function(x) as.numeric(x)))
part1_1 <- part1_1 %>% select(-classe)
part1_1 <- cbind(part1_1, part1$classe)
#

pmldata <- data.frame(lapply(pml_trd, function(x) as.numeric(x)))
pmldata <- pmldata %>% select(-classe)
pmldata <- cbind(pmldata, pml_trd$classe)


modelFit1 <- train(pmldata$`pml_trd$classe`~ ., method='rpart', preProcess='pca', data=pmldata)

modelFit2 <- train(pmldata$`pml_trd$classe`~ ., method='rf', preProcess='pca', data=pmldata)

confusionMatrix(pml_testing$class, predict(modelFit,pml_testing))


pml_testing1 <- pml_testing %>% select(-starts_with('max_'), -starts_with('min_'), -starts_with('var_'), 
                                       -starts_with('avg_'), -starts_with('stddev_'), -starts_with('amplitude_'), -skewness_yaw_belt,
                                       -kurtosis_yaw_forearm, -skewness_yaw_forearm, -skewness_yaw_dumbbell, -kurtosis_yaw_dumbbell,
                                       -kurtosis_yaw_belt, -X, -user_name, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp,
                                       -kurtosis_roll_belt, -kurtosis_picth_belt, -skewness_roll_belt, -skewness_roll_belt.1,
                                       -kurtosis_roll_arm, -kurtosis_picth_arm, -kurtosis_yaw_arm, -skewness_roll_arm, -skewness_pitch_arm, 
                                       -skewness_yaw_arm, -kurtosis_roll_dumbbell, -kurtosis_picth_dumbbell, -skewness_roll_dumbbell, 
                                       -skewness_pitch_dumbbell, -kurtosis_roll_forearm, -kurtosis_picth_forearm, -skewness_roll_forearm, 
                                       -skewness_pitch_forearm, -new_window)

pml_testing1 <- data.frame(lapply(pml_testing1, function(x) as.numeric(x)))  


predict(modelFit1, newdata = pml_testing1)

predict(modelFit2, newdata = pml_testing1)










