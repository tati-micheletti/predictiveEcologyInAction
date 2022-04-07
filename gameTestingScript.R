# Quick script sourcing

# LANDSCAPE
if (!require("Require")) {install.packages("Require"); require("Require")}
Require("data.table")
Require("raster")
Require("googledrive")
source("landscapeSimulation.R")
landscape <- getInitialLandscape()
for (round in 1:10){
  print(paste0("Round ", round))
  simulateForestGrowth()
  simulateHumanDisturbance()
  simulateFire()
  changes <- checkChangesInLandscape()
  landscape <- updateLandscape(landscape, changes)
}
saveAndSendResults(landscape)

# BIRDS
if (!require("Require")) {install.packages("Require"); require("Require")}
Require("data.table")
Require("raster")
Require("googledrive")
source("birdSimulation.R")
availableHabitats()
# birdsTable <- loadBirdsTable()
birdsTable <- generateBirdsTable() # Instead of the loadBirdsTable() manually made
bt <- calculateObservations(birdsTable)
# birdDataset <- fread("data/birdHabitat.csv")
birdDataset <- cbind(data.table(birdsTable), 
                     counts = bt[["counts"]]) # instead of "data/birdHabitat.csv" manually filled
birdModel <- glm(formula = counts ~ A + B + C + D + E, 
                 family = "poisson", data = birdDataset)
saveAndSendResults(birdModel)

# TURTLE
if (!require("Require")) {install.packages("Require"); require("Require")}
Require("data.table")
Require("raster")
Require("googledrive")
source("turtleSimulation.R")
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
saveAndSendResults(turtleResults)

# INTEGRATING
if (!require("Require")) {install.packages("Require"); require("Require")}
Require("data.table")
Require("raster")
Require("googledrive")
Require("reproducible")
Require("ggplot2")
Require("gridExtra")
source("landscapeSimulation.R")
source("turtleSimulation.R")
source("integratingSimulations.R")
allData <- getAllData()
initialLandscape <- getInitialLandscape()
finalLandscape <- ratify(allData$landscape)
plot(initialLandscape)
plot(finalLandscape)
birdModel <- allData$birdResults
habitats <- merge(availableHabitats(visible = FALSE), allData$turtleResults)
birdMaps <- generateBirdMaps(initialLandscape, finalLandscape, birdModel)
plot(birdMaps, col = heat.colors(6))
turtleMaps <- generateTurtleMaps(initialLandscape, finalLandscape, habitats)
plot(turtleMaps, col = heat.colors(6))
answerToOurQuestion <- areBirdsGoodUmbrellaForTurtle(birdMaps, turtleMaps)


