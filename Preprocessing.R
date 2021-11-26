rm(list=ls())

library(ggplot2)
library(dplyr)
library(caret)

# Change working directory if needed, Josse.
setwd("C:/Users/Daan/Documents/Projecten/DataMiningResearch")

brfss.df <- read.csv("BRFSS2015.csv", header = T)

summary(brfss.df)

# Change binary data still labeled as numeric as categorical
brfss.df$HeartDiseaseorAttack <- as.factor(brfss.df$HeartDiseaseorAttack)

brfss.df$HighBP <- as.factor(brfss.df$HighBP)

brfss.df$HighChol <- as.factor(brfss.df$HighChol)

brfss.df$CholCheck <- as.factor(brfss.df$CholCheck)

brfss.df$Smoker <- as.factor(brfss.df$Smoker)

brfss.df$Stroke <- as.factor(brfss.df$Stroke)

brfss.df$PhysActivity <- as.factor(brfss.df$PhysActivity)

brfss.df$Fruits <- as.factor(brfss.df$Fruits)

brfss.df$Veggies <- as.factor(brfss.df$Veggies)

brfss.df$HvyAlcoholConsump <- as.factor(brfss.df$HvyAlcoholConsump)

brfss.df$AnyHealthcare <- as.factor(brfss.df$AnyHealthcare)

brfss.df$NoDocbcCost <- as.factor(brfss.df$NoDocbcCost)

brfss.df$DiffWalk <- as.factor(brfss.df$DiffWalk)

brfss.df$Sex <- as.factor(brfss.df$Sex)

summary(brfss.df)


# Change remaining categorical data still labeled as numeric to categorical
brfss.df$Diabetes <- as.factor(brfss.df$Diabetes)

brfss.df$GenHlth <- as.factor(brfss.df$GenHlth)

brfss.df$MentHlth <- as.factor(brfss.df$MentHlth)

brfss.df$PhysHlth <- as.factor(brfss.df$PhysHlth)

brfss.df$Age <- as.factor(brfss.df$Age)

brfss.df$Education <- as.factor(brfss.df$Education)

brfss.df$Income <- as.factor(brfss.df$Income)

summary(brfss.df)


# Converting BMI from continuous value to categorical value.
  # This is performed by taking every BMI value and placing it in one of four categories:
    # 1. Underweight (BMI < 18.5)
    # 2. Healthy weight (18.5 <= BMI < 25)
    # 3. Overweight (25 <= BMI < 30)
    # 4. Class 1 Obese (30 <= BMI < 35)
    # 5. Class 2 Obese (35 <= BMI < 40)
    # 6. Class 3 Obese (BMI >= 40)
  # Categories taken from CDC (https://www.cdc.gov/obesity/adult/defining.html)
BMI.to.categorical <- function(BMI.value){
  if (BMI.value >= 40) {
    BMI.value <- "Class 3 Obese"
  } else if (BMI.value >= 35) {
    BMI.value <- "Class 2 Obese"
  } else if (BMI.value >= 30) {
    BMI.value <- "Class 1 Obese"
  } else if (BMI.value >= 25) {
    BMI.value <- "Overweight"
  } else if (BMI.value >= 18.5) {
    BMI.value <- "Healthy weight"
  } else {
    BMI.value <- "Underweight"
  }
}

brfss.df$BMI <- sapply(brfss.df$BMI, BMI.to.categorical)


# Give the following features more readable names: 
  

# Bar plots
ggplot(brfss.df, aes(x=HeartDiseaseorAttack)) + 
  geom_bar()

ggplot(brfss.df, aes(x=HighBP, fill=HeartDiseaseorAttack)) + 
  labs(fill="Has had Heart Disease\nor Heart Attack") +
  theme(legend.position="right") +
  scale_fill_grey() +
  geom_bar()

BMI.level.order <- c("Underweight", "Healthy weight", "Overweight", "Class 1 Obese", "Class 2 Obese", "Class 3 Obese")
ggplot(brfss.df, aes(x=factor(BMI, levels=BMI.level.order), fill=HeartDiseaseorAttack)) + 
  labs(fill="Has had Heart Disease\nor Heart Attack") +
  theme(legend.position="right") +
  scale_fill_grey() +
  geom_bar()

