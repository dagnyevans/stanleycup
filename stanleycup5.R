#Edit this working directory. 
#This is a good habit so you know you are storing everything in a single place.
setwd("/Users/Dagny/Documents/codeher/stanleycup")

#IMPORTING DATA INTO R
#import dataset - EDIT THIS FILENAME
playerstatshistory <- read.csv("playerstats.csv")


##Syntax for looking at data in system
#Summarize data frame
summary(playerstatshistory)

#Write to file
write.csv(playerstatshistory,file="playerstatexport.csv")

#Create data table and setkey in order to define player averages
library(data.table)

playerstatshistory <- data.table(playerstatshistory)

setkey(playerstatshistory,Player.Name)

playeravgs <- as.data.frame(playerstatshistory[,j=list(
  mean_shots = mean(Shots,na.rm=TRUE),
  mean_iCorsi = mean(iCorsi,na.rm=TRUE),
  mean_iFenwick = mean(iFenwick,na.rm=TRUE),
  mean_ShPct = mean(ShPct,na.rm=TRUE),
  mean_Shots.60 = mean(Shots.60,na.rm=TRUE),
  mean_iFenwick.60 = mean(iFenwick.60,na.rm=TRUE),
  mean_iCorsi.60 = mean(iCorsi.60,na.rm=TRUE),
  mean_Sv = mean(Sv.,na.rm=TRUE),
  mean_G = mean(G,na.rm=TRUE),
  mean_A = mean(A,na.rm=TRUE),
  mean_FirstA = mean(FirstA,na.rm=TRUE),
  mean_Points = mean(Points,na.rm=TRUE),
  mean_G.60 = mean(G.60,na.rm=TRUE),
  mean_A.60 = mean(A.60,na.rm=TRUE),
  mean_FirstA.60 = mean(FirstA.60,na.rm=TRUE),
  mean_Points.60 = mean(Points.60,na.rm=TRUE),
  mean_IGP = mean(IGP,na.rm=TRUE),
  mean_IAP = mean(IAP,na.rm=TRUE),
  mean_IPP = mean(IPP,na.rm=TRUE)), 
by=Player.Name])


##Updating values for 2015-2016 with mean
playerstatshistory$Shots <- ifelse(is.na(playerstatshistory$Shots),
                                   playeravgs$mean_shots[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$Shots)
playerstatshistory$iCorsi <- ifelse(is.na(playerstatshistory$iCorsi),
                                   playeravgs$mean_iCorsi[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$iCorsi)
playerstatshistory$G <- ifelse(is.na(playerstatshistory$G),
                                   playeravgs$mean_G[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$G)
playerstatshistory$A <- ifelse(is.na(playerstatshistory$A),
                                   playeravgs$mean_A[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$A)
playerstatshistory$FirstA <- ifelse(is.na(playerstatshistory$FirstA),
                                   playeravgs$mean_FirstA[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$FirstA)
playerstatshistory$Points <- ifelse(is.na(playerstatshistory$Points),
                                   playeravgs$mean_Points[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$Points)
playerstatshistory$iFenwick <- ifelse(is.na(playerstatshistory$iFenwick),
                                   playeravgs$mean_iFenwick[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$iFenwick)
playerstatshistory$ShPct<- ifelse(is.na(playerstatshistory$ShPct),
                                   playeravgs$mean_ShPct[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$ShPct)
playerstatshistory$G.60 <- ifelse(is.na(playerstatshistory$G.60),
                                   playeravgs$mean_G.60[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$G.60)
playerstatshistory$A.60 <- ifelse(is.na(playerstatshistory$A.60),
                                   playeravgs$mean_A.60[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$A.60)
playerstatshistory$FirstA.60 <- ifelse(is.na(playerstatshistory$FirstA.60),
                                   playeravgs$mean_FirstA.60[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$FirstA.60)
playerstatshistory$Points.60 <- ifelse(is.na(playerstatshistory$Points.60),
                                   playeravgs$mean_Points.60[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$Points.60)
playerstatshistory$Shots.60 <- ifelse(is.na(playerstatshistory$Shots.60),
                                   playeravgs$mean_Shots.60[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$Shots.60)
playerstatshistory$iFenwick.60 <- ifelse(is.na(playerstatshistory$iFenwick.60),
                                   playeravgs$mean_iFenwick.60[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$iFenwick.60)
playerstatshistory$iCorsi.60 <- ifelse(is.na(playerstatshistory$iCorsi.60),
                                   playeravgs$mean_iCorsi.60[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$iCorsi.60)
playerstatshistory$IGP <- ifelse(is.na(playerstatshistory$IGP),
                                   playeravgs$mean_IGP[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$IGP)
playerstatshistory$IAP <- ifelse(is.na(playerstatshistory$IAP),
                                   playeravgs$mean_shots[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$IAP)
playerstatshistory$IPP <- ifelse(is.na(playerstatshistory$IPP),
                                   playeravgs$mean_IPP[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$IPP)
playerstatshistory$Sv. <- ifelse(is.na(playerstatshistory$Sv.),
                                   playeravgs$mean_Sv[match(playerstatshistory$Player.Name, playeravgs$Player.Name)], playerstatshistory$Sv.)

#Replace null values in winners with 0
playerstatshistory$Winner[which(is.na(playerstatshistory$Winner))] <- 0


##Modeling and Analysis

#First Approach - Aggregating to Teamstats
teamstats <- aggregate(cbind(
              G=playerstatshistory$G,
              A=playerstatshistory$A,
              FirstA=playerstatshistory$FirstA,
              Points=playerstatshistory$Points,
              Shots=playerstatshistory$Shots, 
              iFenwick=playerstatshistory$iFenwick, 
              iCorsi=playerstatshistory$iCorsi,
              G.60=playerstatshistory$G.60,
              A.60=playerstatshistory$A.60,
              FirstA.60=playerstatshistory$FirstA.60,
              Points.60=playerstatshistory$Points.60,
              iCorsi.60=playerstatshistory$iCorsi.60, 
              iFenwick.60=playerstatshistory$iFenwick.60, 
              ShPct=playerstatshistory$ShPct, 
              Shots.60=playerstatshistory$Shots.60, 
              IGP=playerstatshistory$IGP,
              IPP=playerstatshistory$IPP,
              IAP=playerstatshistory$IAP,
              Sv.=playerstatshistory$Sv.,
              Winner=playerstatshistory$Winner)~Season+Team, 
            data=playerstatshistory, sum, na.rm=TRUE) 

#Split data into train and test groups
train <- subset(teamstats, Season=='2010-2011' | Season=='2011-2012' | Season=='2012-2013' | Season=='2013-2013' | Season=='2013-2014' | Season=='2014-2015', select=Season:Winner)
test <- subset(teamstats, Season=='2015-2016', select=Season:Winner)

#Tried using entire teamstats data set for training and testing (given small data set size)
#train <-teamstats
#test <- teamstats

#Import tree classification library
library(rpart)

#Train the model. 
#"fit" is where you are storing the results. 
#"Winner" is what you are trying to predict. 
#"Everything between the ~ and the , are the parameters you want the model to consider. 
fit <- rpart(
  Winner ~G + A + FirstA + Points + Shots + iFenwick + iCorsi + ShPct + G.60 + A.60 + FirstA.60 + Points.60 + Shots.60 + iCorsi.60 + iFenwick.60 + IGP + IAP + IPP + Sv., method="class",
  data=train)

#Run after analysis determined most correlated variables. No difference in results.
#fit2 <- rpart(
#  Winner ~Shots + FirstA + iCorsi, method="class",
#  data=train)

#Add predictions to the test data
test$Prediction95 <- predict(fit, test, type="class", level=95)
test$Prediction90 <- predict(fit, test, type="class", level=90)
test$Prediction85 <- predict(fit, test, type="class", level=85)
test$Prediction80 <- predict(fit, test, type="class", level=80)
test$Prediction75 <- predict(fit, test, type="class", level=75)
test$Prediction70 <- predict(fit, test, type="class", level=70)

#Compare the prediction against the true value - CHANGE "Winner"
#   to whatever your target variable is named
test$RightOrWrong95 <- test$Prediction95 == test$Winner
test$RightOrWrong90 <- test$Prediction90 == test$Winner
test$RightOrWrong85 <- test$Prediction85 == test$Winner
test$RightOrWrong80 <- test$Prediction80 == test$Winner
test$RightOrWrong75 <- test$Prediction75 == test$Winner
test$RightOrWrong70 <- test$Prediction70 == test$Winner

#Baseline if we just marked everything as false - CHANGE "Winner"
prop.table(table(test$Winner))

#How many we got right vs. wrong
table(test$RightOrWrong95) 
table(test$RightOrWrong90) 
table(test$RightOrWrong85) 
table(test$RightOrWrong80) 
table(test$RightOrWrong75) 
table(test$RightOrWrong70) 

#Proportion we got right vs. wrong
prop.table(table(test$RightOrWrong95)) 

#Computing Multiple Regression
reg1 <- lm(Winner ~  G + A + FirstA + Points + Shots + iFenwick + iCorsi + ShPct + G.60 + A.60 + FirstA.60 + Points.60 + Shots.60 + iCorsi.60 + iFenwick.60 + IGP + IAP + IPP + Sv., data=teamstats)

#Analysis of Variance Table
anova(reg1)

#Stepwise variables selection
#backward
regb <-step(reg1, direction="backward", trace=0)

#See results of backward regression
summary(regb)

#Second Approach - Run directly off playerstats


#Split data into train and test groups
#train <- subset(playerstatshistory, Season=='2010-2011' | Season=='2011-2012' | Season=='2012-2013' | Season=='2013-2013' | Season=='2013-2014' | Season=='2014-2015', select=Season:Winner)
#test <- subset(playerstatshistory, Season=='2015-2016', select=Season:Winner)

#Tried using entire teamstats data set for training and testing (given small data set size)
train <- playerstatshistory
test <- playerstatshistory

#Import tree classification library
library(rpart)

#Train the model. 
#"fit" is where you are storing the results. 
#"Winner" is what you are trying to predict. 
#"Everything between the ~ and the , are the parameters you want the model to consider. 
fit <- rpart(
  Winner ~G + A + FirstA + Points + Shots + iFenwick + iCorsi + ShPct + G.60 + A.60 + FirstA.60 + Points.60 + Shots.60 + iCorsi.60 + iFenwick.60 + IGP + IAP + IPP + Sv., method="class",
  data=train)

#Run after analysis determined most correlated variables. No difference in results.
#fit2 <- rpart(
#  Winner ~Sv. + IPP + IAP + iFenwick + iCorsi+ G, method="class",
#  data=train)

#Add predictions to the test data
test$Prediction95 <- predict(fit, test, type="class", level=95)
test$Prediction90 <- predict(fit, test, type="class", level=90)
test$Prediction85 <- predict(fit, test, type="class", level=85)
test$Prediction80 <- predict(fit, test, type="class", level=80)
test$Prediction75 <- predict(fit, test, type="class", level=75)
test$Prediction70 <- predict(fit, test, type="class", level=70)
test$Prediction50 <- predict(fit, test, type="class", level=50)
test$Prediction25 <- predict(fit, test, type="class", level=25)
test$Prediction0 <- predict(fit, test, type="class", level=0)

#Compare the prediction against the true value - CHANGE "Winner"
#   to whatever your target variable is named
test$RightOrWrong95 <- test$Prediction95 == test$Winner
test$RightOrWrong90 <- test$Prediction90 == test$Winner
test$RightOrWrong85 <- test$Prediction85 == test$Winner
test$RightOrWrong80 <- test$Prediction80 == test$Winner
test$RightOrWrong75 <- test$Prediction75 == test$Winner
test$RightOrWrong70 <- test$Prediction70 == test$Winner
test$RightOrWrong50 <- test$Prediction50 == test$Winner
test$RightOrWrong25 <- test$Prediction25 == test$Winner
test$RightOrWrong0 <- test$Prediction0 == test$Winner

#How many we got right vs. wrong
table(test$RightOrWrong95) 
table(test$RightOrWrong90) 
table(test$RightOrWrong85) 
table(test$RightOrWrong80) 
table(test$RightOrWrong75) 
table(test$RightOrWrong70) 
table(test$RightOrWrong50) 
table(test$RightOrWrong25) 
table(test$RightOrWrong0) 

#Proportion we got right vs. wrong
prop.table(table(test$RightOrWrong95)) 

#Export test results
write.csv(test,file="test.csv")

#Computing Multiple Regression
reg1 <- lm(Winner ~  G + A + FirstA + Points + Shots + iFenwick + iCorsi + ShPct + G.60 + A.60 + FirstA.60 + Points.60 + Shots.60 + iCorsi.60 + iFenwick.60 + IGP + IAP + IPP + Sv., data=playerstatshistory)

#Analysis of Variance Table
anova(reg1)

#Stepwise variables selection
#backward
regb <-step(reg1, direction="backward", trace=0)

summary(regb)
