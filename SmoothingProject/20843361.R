# Fill in the following fields
# UW_ID: 20843361
# Name: Liu, Bruce
# UW_email: b287liu@uwaterloo.ca
# Kaggle_public_score: 0.15326
# Kaggle_submission_count: 29
# Running_time: 484.87
# the running time is the time in seconds to run your SmoothingModel(), see evaluation.R for more details

# Only libraries and SmoothingModel function allowed in this file
# No setwd() and load() commands allowed here

# add library here using the library function
# all libraries need to be used in your code in the library::function format
### Your libraries start here
library(mgcv)
### Your libraries end here

# dtrain: data.frame for the training set
# dtest: data.frame for the test set
# should return a data.frame for prediction
SmoothingModel <- function(dtrain, dtest){
# Write all your code below, including preprocessing and functions.
# Only the final model should be fitted, no model selection steps allowed
# All plotting, diagnositic and model building/selection steps should be in your Rmd file.

### Your code starts here
  
    ### Preprocessing the dtrain data
    dtrain$grade <- as.numeric(factor(dtrain$grade, levels = c("Low Quality",
                                                               "Fair Quality", "Average",
                                                               "Above Average", "Good Quality",
                                                               "Very Good", "Excellent",
                                                               "Superior","Exceptional-A",
                                                               "Exceptional-B", "Exceptional-C",
                                                               "Exceptional-D")))
    maxpricelat <- dtrain$latitude[which.max(dtrain$price)]
    maxpricelon <- dtrain$longitude[which.max(dtrain$price)]
    proximity <- sqrt((dtrain$latitude - maxpricelat)^2 + (dtrain$longitude - maxpricelon)^2)    
    dtrain$proximity <- proximity
    dtrain$saledate <- as.numeric(as.Date(dtrain$saledate))
    dtrain$cndtn <- as.numeric(factor(dtrain$cndtn, levels = c("Poor","Fair","Average",
                                                    "Good", "Very Good", "Excellent")))
    dtrain$style <- as.numeric(factor(dtrain$style,levels = c("Split Foyer","1 Story",
                                                   "1.5 Story Fin", "1.5 Story Unfin",
                                                   "2.5 Story Unfin", "2 Story",
                                                   "Bi-Level","Split Level",
                                                   "2.5 Story Fin", "3 Story", "4 Story")))
    dtrain$ac <- factor(dtrain$ac, levels = c("N", "Y"))
    dtrain$ward <- as.numeric(factor(dtrain$ward, levels = c("Ward 7", "Ward 8", "Ward 5",
                                                  "Ward 6", "Ward 4", "Ward 1",
                                                  "Ward 3", "Ward 2")))
    dtrain$nbhd <- factor(dtrain$nbhd)
    dtrain$heat <- factor(dtrain$heat, levels = c("Wall Furnace", "Air-Oil", "Elec Base Brd",
                                                  "Gravity Furnace", "No Data","Evp Cool",
                                                  "Hot Water Rad", "Forced Air", "Water Base Brd",
                                                  "Ht Pump","Warm Cool","Air Exchng"))
    dtrain$roof <- factor(dtrain$roof)
    dtrain$extwall <- factor(dtrain$extwall)
    dtrain$intwall <- factor(dtrain$intwall)
    
    ### Preprocessing the dtest data
    proximity <- sqrt((dtest$latitude - maxpricelat)^2 + (dtest$longitude - maxpricelon)^2)
    dtest$proximity <- proximity
    dtest$saledate <- as.numeric(as.Date(dtest$saledate))
    dtest$grade <- as.numeric(factor(dtest$grade, levels = c("Low Quality",
                                                             "Fair Quality", "Average",
                                                             "Above Average", "Good Quality",
                                                             "Very Good", "Excellent",
                                                             "Superior","Exceptional-A",
                                                             "Exceptional-B", "Exceptional-C",
                                                             "Exceptional-D")))
    dtest$ward <- as.numeric(factor(dtest$ward, levels = c("Ward 7", "Ward 8", "Ward 5",
                                                "Ward 6", "Ward 4", "Ward 1",
                                                "Ward 3", "Ward 2")))
    dtest$heat <- factor(dtest$heat, levels = c("Wall Furnace", "Air-Oil", "Elec Base Brd",
                                                "Gravity Furnace", "No Data","Evp Cool",
                                                "Hot Water Rad", "Forced Air", "Water Base Brd",
                                                "Ht Pump","Warm Cool","Air Exchng"))
    for (i in 1:1000) {
      if (is.na(dtest[i,"ayb"])) {
        if (dtest[i, "heat"] == "Forced Air") {
          dtest[i, "ayb"] <- round(mean(dtrain[which(dtrain$heat == "Forced Air"),"ayb"],na.rm = TRUE))
        }
      }
      if (dtest[i, "intwall"] == "Terrazo") {
        dtest[i, "intwall"] <- "Lt Concrete"
      }
      if (dtest[i, "intwall"] == "Vinyl Sheet") {
        dtest[i, "intwall"] <- "Default"
      }
      if (dtest[i, "style"] == "Default") {
        dtest[i, "style"] <- "2 Story"
      }
    }
    dtest$cndtn <- as.numeric(factor(dtest$cndtn, levels = c("Poor","Fair","Average",
                                                    "Good", "Very Good", "Excellent")))
    dtest$nbhd <- factor(dtest$nbhd)
    dtest$style <- factor(dtest$style)
    dtest$ac <- factor(dtest$ac, levels = c("N", "Y"))
    dtest$roof <- factor(dtest$roof)
    dtest$extwall <- factor(dtest$extwall)
    dtest$intwall <- factor(dtest$intwall)
    dtest$style <- as.numeric(factor(dtest$style,levels = c("Split Foyer","1 Story",
                                                   "1.5 Story Fin", "1.5 Story Unfin",
                                                   "2.5 Story Unfin", "2 Story",
                                                   "Bi-Level", "Split Level",
                                                   "2.5 Story Fin", "3 Story", "4 Story")))
    # A simple local regression model fit
    fit <- mgcv::gam(price^(1/10)~s(proximity, by = nbhd)+s(saledate,ayb,eyb)
                     +te(gba, landarea)+s(grade)+s(bathrm)+s(fireplaces)+roof
                     +extwall+intwall+s(as.numeric(heat),by = ac)+s(hf_bathrm, k = 5)+s(ward, k = 8)
                     +s(cndtn, k = 6)+s(rooms), data=dtrain)
    
    pred <- predict.gam(fit, newdata=dtest, type = "response")
    res <- data.frame(Id=dtest$Id, price=pred^10)
    return(res)
	
### Your code ends here
} # end SmoothingModel
