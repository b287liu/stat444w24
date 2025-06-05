# UW ID: 20843361
# Name: Liu, Bruce
# UW email: b287liu@uwaterloo.ca
# Kaggle_public_score: 0.20230
# Kaggle_submission_count: 30
# Running_time: 2.03
# the running time is the time in seconds to run your RFModel(), see evaluation.R for more details


# fill in the above ID, name, and time, but leave other existing comments untouched
# Only libraries and RFModel function allowed in this file
# NO setwd, load, and write.csv commands allowed in this file
# add library here using the library function
# all libraries need to be used in your code in the library::function format
### Your libraries start here
# library for random forest
library(ranger)
library(impute)
### Your libraries end here

# dtrain: data.frame for the training set
# dtest: data.frame for the test set
# should return a data.frame for prediction
RFModel <- function(dtrain, dtest){
  # Write all your code below, including preprocessing and functions.
  # Only the final model should be fitted, no model selection/tuning steps allowed here
  # All plotting, diagnositic and model building/selection steps should be in your Rmd file.
  ### Your code starts here
  
  # dtrain preprocessing
  maxpricelat <- dtrain$latitude[which.max(dtrain$price)]
  maxpricelon <- dtrain$longitude[which.max(dtrain$price)]
  proximity <- sqrt((dtrain$latitude - maxpricelat)^2 + (dtrain$longitude - maxpricelon)^2)    
  dtrain$proximity <- proximity
  dtrain$grade <- factor(dtrain$grade, levels = c("Low Quality",
                                                  "Fair Quality", "Average",
                                                  "Above Average", "Good Quality",
                                                  "Very Good", "Excellent",
                                                  "Superior","Exceptional-A",
                                                  "Exceptional-B", "Exceptional-C",
                                                  "Exceptional-D"))  
  dtrain$saledate <- as.numeric(as.Date(dtrain$saledate))
  dtrain$cndtn <- factor(dtrain$cndtn, levels = c("Poor","Fair","Average",
                                                  "Good", "Very Good", "Excellent"))
  dtrain$ac <- factor(dtrain$ac, levels = c("N", "Y"))
  dtrain$ward <- factor(dtrain$ward, levels = c("Ward 7", "Ward 8", "Ward 5",
                                                "Ward 6", "Ward 4", "Ward 1",
                                                "Ward 3", "Ward 2"))
  dtrain$heat <- dtrain$heat <- factor(dtrain$heat, levels = c("Wall Furnace", "Air-Oil", "Elec Base Brd",
                                                               "Gravity Furnace", "No Data","Evp Cool",
                                                               "Hot Water Rad", "Forced Air", "Water Base Brd",
                                                               "Ht Pump","Warm Cool","Air Exchng"))
  for (i in 1:6000) {
    if (is.na(dtrain[i,"heat"])) {
      dtrain[i, "heat"] <- "No Data"
    }
  }
  dtrain$nbhd <- factor(dtrain$nbhd)
  dtrain$roof <- factor(dtrain$roof)
  dtrain$extwall <- factor(dtrain$extwall)
  dtrain$intwall <- factor(dtrain$intwall)
  dtrain$style <- factor(dtrain$style, levels = c("Split Foyer","1 Story",
                                                  "1.5 Story Fin", "1.5 Story Unfin",
                                                  "2.5 Story Unfin", "2 Story",
                                                  "Bi-Level","Split Level", "Default",
                                                  "2.5 Story Fin", "3 Story", "4 Story"))
  dtrainYears <- dtrain[,c(7:9)]
  knnYears <- impute::impute.knn(t(dtrainYears))
  imputedYears <- t(knnYears$data)
  dtrain$ayb[which(is.na(dtrain$ayb))] <- imputedYears[which(is.na(dtrain$ayb))]
  dtrain$yr_rmdl[which(is.na(dtrain$yr_rmdl))] <- imputedYears[6000 + which(is.na(dtrain$yr_rmdl))]
  dtrainRooms <- dtrain[, c("bathrm","hf_bathrm","rooms","bedrm",
                            "stories","kitchens","fireplaces")]
  knnRooms <- impute::impute.knn(t(dtrainRooms))
  imputedRooms <- t(knnRooms$data)
  dtrain$stories[which(is.na(dtrain$stories))] <- imputedRooms[24000 + which(is.na(dtrain$stories))]
  dtrain$kitchens[which(is.na(dtrain$kitchens))] <- imputedRooms[30000 + which(is.na(dtrain$kitchens))]
  dtrain$RoomRatio <- as.numeric(unlist((dtrain["bathrm"]+dtrain["hf_bathrm"]+dtrain["bedrm"]+dtrain["kitchens"]+dtrain["fireplaces"])/(dtrain["rooms"] + 1)))
  dtrain$stories[which(is.na(dtrain$stories))] <- imputedRooms[24000 + which(is.na(dtrain$stories))]
  dtrain$kitchens[which(is.na(dtrain$kitchens))] <- imputedRooms[30000 + which(is.na(dtrain$kitchens))]
  dtrainLocation <- dtrain[, c(23:27)]
  dtrainLocation$nbhd <- as.numeric(dtrainLocation$nbhd)
  dtrainLocation$ward <- as.numeric(factor(dtrainLocation$ward))
  dtrainLocation$quadrant <- as.numeric(factor(dtrainLocation$quadrant))
  knnLocation <- impute::impute.knn(t(dtrainLocation))
  imputedLocation <- t(knnLocation$data)
  
  
  for (i in c(1:6000)) {
    if (is.na(dtrain$quadrant[i])) {
      if (abs(imputedLocation[24000 + i]) <= 1) {
        dtrain$quadrant[i] <- "NE"
      } else if (abs(imputedLocation[24000 + i]) <= 2) {
        dtrain$quadrant[i] <- "NW"
      } else if (abs(imputedLocation[24000 + i]) <= 3) {
        dtrain$quadrant[i] <- "SE"
      } else {
        dtrain$quadrant[i] <- "SW"
      }
    }
  }
  dtrain$quadrant <- factor(dtrain$quadrant, levels = c("SW","SE","NE","NW"))
  dtrain$RpS <- dtrain$rooms/dtrain$stories
  dtrain$yrDiff <- dtrain$yr_rmdl - dtrain$ayb
  dtrain$styleStories <- as.numeric(dtrain$style)*dtrain$stories
  
  # dtest preprocessing
  proximity <- sqrt((dtest$latitude - maxpricelat)^2 + (dtest$longitude - maxpricelon)^2)
  dtest$proximity <- proximity
  dtest$RoomRatio <- as.numeric(unlist((dtest["bathrm"]+dtest["hf_bathrm"]+dtest["bedrm"]+dtest["kitchens"]+dtest["fireplaces"])/(dtest["rooms"] + 1)))
  dtest$grade <- factor(dtest$grade, levels = c("Low Quality",
                                                "Fair Quality", "Average",
                                                "Above Average", "Good Quality",
                                                "Very Good", "Excellent",
                                                "Superior","Exceptional-A",
                                                "Exceptional-B", "Exceptional-C",
                                                "Exceptional-D"))
  dtest$saledate <- as.numeric(as.Date(dtest$saledate))
  dtest$ward <- factor(dtest$ward, levels = c("Ward 7", "Ward 8", "Ward 5",
                                              "Ward 6", "Ward 4", "Ward 1",
                                              "Ward 3", "Ward 2"))
  dtest$cndtn <- factor(dtest$cndtn, levels = c("Poor","Fair","Average",
                                                "Good", "Very Good", "Excellent"))
  dtest$nbhd <- factor(dtest$nbhd)
  dtest$ac <- factor(dtest$ac, levels = c("N", "Y"))
  dtest$roof <- factor(dtest$roof)
  dtest$extwall <- factor(dtest$extwall)
  dtest$intwall <- factor(dtest$intwall)
  dtest$heat <- factor(dtest$heat, levels = c("Wall Furnace", "Hot Water Rad", 
                                              "Forced Air", "Water Base Brd",
                                              "Ht Pump","Warm Cool"))
  dtest$style <- factor(dtest$style, levels = c("Split Foyer","1 Story",
                                                "1.5 Story Fin", "1.5 Story Unfin",
                                                "2.5 Story Unfin", "2 Story",
                                                "Bi-Level","Split Level",
                                                "2.5 Story Fin", "3 Story"))
  
  dtestYears <- dtest[,c(8:10)]
  knnTestYears <- impute::impute.knn(t(dtestYears))
  imputedTestYears <- t(knnTestYears$data)
  dtest$ayb[which(is.na(dtest$ayb))] <- imputedTestYears[which(is.na(dtest$ayb))]
  dtest$yr_rmdl[which(is.na(dtest$yr_rmdl))] <- imputedTestYears[998 + which(is.na(dtest$yr_rmdl))]
  dtestLocation <- dtest[, c(23:27)]
  dtestLocation$nbhd <- as.numeric(dtestLocation$nbhd)
  dtestLocation$ward <- as.numeric(factor(dtestLocation$ward))
  dtestLocation$quadrant <- as.numeric(factor(dtestLocation$quadrant))
  knnTestLocation <- impute::impute.knn(t(dtestLocation))
  imputedTestLocation <- t(knnTestLocation$data)
  
  for (i in c(1:998)) {
    if (is.na(dtest$quadrant[i])) {
      if (abs(imputedTestLocation[3992 + i]) <= 1) {
        dtest$quadrant[i] <- "NE"
      } else if (abs(imputedTestLocation[3992 + i]) <= 2) {
        dtest$quadrant[i] <- "NW"
      } else if (abs(imputedTestLocation[3992 + i]) <= 3) {
        dtest$quadrant[i] <- "SE"
      } else {
        dtest$quadrant[i] <- "SW"
      }
    }
  }
  dtest$quadrant <- factor(dtest$quadrant, levels = c("SW","SE","NE","NW"))
  dtest$RpS <- dtest$rooms/dtest$stories
  dtest$yrDiff <- dtest$yr_rmdl - dtest$ayb
  dtest$styleStories <- as.numeric(dtest$style)*dtest$stories
  
  
  # please do not specify the number of trees, which will leave it as the default of 500
  # Note that the default trees for ranger is 500 trees.
  fit <- ranger::ranger(price^(1/15) ~ ., data = dtrain, mtry = 14,
                        respect.unordered.factors = TRUE, min.bucket = 1, min.node.size = 3,
                        max.depth = 44)
  pred <- predict(fit, data=dtest)
  res <- data.frame(Id=dtest$Id, price=pred$predictions^15)
  return(res)
  
  ### Your code ends here
} # end RFModel
