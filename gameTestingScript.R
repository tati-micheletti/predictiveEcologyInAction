# Quick script sourcing

# ALL LIBRARIES
# Dependencies
if (!require("Require")) {install.packages("Require"); require("Require")}
Require("sf")
Require("RCurl")
Require("googledrive")

# Libraries
Require("data.table")
Require("terra")
Require("reproducible")
Require("ggplot2")
Require("gridExtra")
Require("googledrive")

# Source all functions
source("landscapeSimulation.R")
source("turtleSimulation.R")
source("birdSimulation.R")
source("integratingSimulations.R")

# LANDSCAPE
landscape <- getInitialLandscape()
for (round in 2:10){
  print(paste0("Round ", round))
  Sys.sleep(1)
  simulateForestGrowth()
  simulateHumanDisturbance()
  simulateFire()
  changes <- checkChangesInLandscape()
  landscape <- updateLandscape(landscape, changes)
}
saveLandscapeResults(landscape, upload = TRUE)

# BIRDS
availableHabitatsBirds()
# birdsTable <- loadBirdsTable()
birdsTable <- generateBirdsTable() # Instead of the loadBirdsTable() manually made
bt <- calculateObservations(birdsTable)
# birdDataset <- fread("data/birdHabitat.csv")
birdDataset <- cbind(data.table(birdsTable), 
                     counts = bt[["counts"]]) # instead of "data/birdHabitat.csv" manually filled
birdModel <- glm(formula = counts ~ A + B + C + D + E, 
                 family = "poisson", data = birdDataset)
saveBirdResults(birdModel, upload = TRUE)

# TURTLE
# simulateDirection() # Not needed to test
# simulateDistance() # Not needed to test
# availableHabitats() # Not needed to test
# simulatePatchChoice(simulatePatchChoice(N = "A",
#                                         NE = "A",
#                                         E = "B",
#                                         SE = "A",
#                                         S = "A",
#                                         SW = "H",
#                                         W = "C",
#                                         NW = "B")) # insted of the simulatePatchChoice()
# turtleDataset <- fread("data/turtleArrivals.csv")
turtleDataset <- generateTurtleDataset() # instead of all simulations + loading data/turtleArrivals manually
turtleModel <- lm(formula = totalNumberOfArrivals ~ habitatType, 
                  data = turtleDataset)
forecasts <- round(predict(turtleModel, 
                           newdata = data.frame(habitatType = c(LETTERS[1:9])),
                           type = "response"), 0)
turtleResults <- data.table(habitatType = c(LETTERS[1:9]), 
                            habitatPreference = forecasts)
saveTurtleResults(turtleResults, upload = TRUE)

# INTEGRATING
allData <- getAllData()
initialLandscape <- getInitialLandscape()
finalLandscape <- allData$landscape
birdModel <- allData$birdResults
habitats <- merge(availableHabitatsTurtles(), allData$turtleResults, by = "habitatType")
birdMaps <- generateBirdMaps(initialLandscape, finalLandscape, birdModel)
turtleMaps <- generateTurtleMaps(initialLandscape, finalLandscape, habitats)
plotMaps(allMaps = c(initialLandscape, finalLandscape, 
                     birdMaps$Now, birdMaps$Future,
                     turtleMaps$Now, turtleMaps$Future))
answerToOurQuestion <- areBirdsGoodUmbrellaForTurtle(birdMaps, turtleMaps)
