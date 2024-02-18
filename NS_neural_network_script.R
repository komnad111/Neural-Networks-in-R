##NORTHSTAR BANK NEURAL NETWORK SCRIPT

#LOAD NECESSARY PACKAGES
library(haven)
library(dplyr)
library(fastDummies)
library(mosaic)
library(nnet)


#DATA PREPROCESSING
Stardat<-read_spss("Northstar_Bank_data.sav")

#Create dummy variables for day of week
idunno<-dummy_cols(Stardat, select_columns = 
             c('Ed'), remove_first_dummy = TRUE,remove_selected_columns = TRUE)

#Rescale the scale variables to 0-1.
#This speeds up the neural network calculations.
d_northstar_normdum <- mutate(idunno, 
          Age=(Age-min(Age))/(max(Age)-min(Age)),
          Employ = (Employ-min(Employ))/(max(Employ)-min(Employ)),
          Address = (Address-min(Address))/(max(Address)-min(Address)),
          Income = (Income-min(Income))/(max(Income)-min(Income)),
          Debtinc = (Debtinc-min(Debtinc))/(max(Debtinc)-min(Debtinc)),
          Creddebt = (Creddebt-min(Creddebt))/(max(Creddebt)-min(Creddebt)),
          Othdebt = (Othdebt-min(Othdebt))/(max(Othdebt)-min(Othdebt))) #dplyr 

#Create training and test data frames
Stardat_train<-filter(d_northstar_normdum, partition == "train") %>% select( - partition)
Stardat_test<-filter(d_northstar_normdum, partition == "test") %>% select( - partition)


#MODEL TRAINING
#Create neural network model with 5 nodes
set.seed(1)
nn5 <- nnet(Default ~., data = Stardat_train, size = 5, linout= F, decay = 0.01) #nnet


#MODEL TUNING
#Get training predictions
nn5_train_pred <-mutate(Stardat_train, Predict = predict(nn5, Stardat_train) %>% round())
mean(~(Default == Predict), data = nn5_train_pred)
#Classification Tables
  #Absolute values
    tally(Default ~ Predict, data = nn5_train_pred) %>% addmargins()
  #Fractions
    tally(Default ~ Predict, data = nn5_train_pred) %>% prop.table(margin=1)%>%round(3)


#MODEL TESTING
#Get test predictions
nn5_test_pred <-mutate(Stardat_test, Predict = predict(nn5, Stardat_test) %>% round())
mean(~(Default == Predict), data = nn5_test_pred)
#Classification Tables
#Absolute values
tally(Default ~ Predict, data = nn5_test_pred) %>% addmargins()
#Fractions
tally(Default ~ Predict, data = nn5_test_pred) %>% prop.table(margin=1)%>%round(3)
