## Simulating Turtle Movements

# This is the file where all functions for the Turtle Simulation group are 
# defined.

simulateDirection <- function(){
  direction <- sample(c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), 
                      size = 1, replace = FALSE)
  print(paste0("The turtle will travel ", direction, " to look for a ",
               "good place to spend the year."))
}

simulateDistance <- function(){
  distance <- sample(c(1:8), size = 1, replace = FALSE, 
                      prob = c(0.25, 0.19, 0.16, 0.13, 
                               0.11, 0.09, 0.05, 0.02))
  print(paste0("The turtle will travel for ", distance, " patches until it ",
               "arrives at the destination patch."))
}

availableHabitats <- function(visible = TRUE){
  ah <- structure(list(habitatType = c(
                   "A", "B", "C", "D", "E", "F", 
                   "G", "H", "I"), 
                 habitatDescription = c(
                   "Water",
                   "Old forest that is near water (at least 1 surrounding patch is water)",
                   "Middle-aged forest near water (at least 1 surrounding patch is water)",
                   "Young forest near water (at least 1 surrounding patch is water)",
                   "Old forest",
                   "Middle-aged forest",
                   "Young forest",
                   "Recently burned patch",
                   "Human disturbed patch"), 
                 habitatColor = c(
                   "light blue",
                   "dark green",
                   "middle green",
                   "light green",
                   "dark green",
                   "middle green",
                   "light green",
                   "light yellow",
                   "grey or black")), 
            row.names = c(NA, -9L), 
            class = c("data.table", "data.frame"))
  if (visible)
    print(ah)
  return(ah)
}

simulatePatchChoice <- function(N,
                                NE,
                                E,
                                SE,
                                S,
                                SW,
                                W,
                                NW){
  
  defaultProbs <- data.table(
    habitatType = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
    habitatProb = c(0.24, 0.19, 0.16, 0.13, 0.11, 0.09, 0.05, 0.02, 0.01)
    )
  
  availableHabs <- data.table(habitatType = c(N,NE,E,SE,S,SW,W,NW),
                              direction = c("N","NE","E","SE","S","SW","W","NW"))
  
  DT <- merge(availableHabs, defaultProbs, all.x = TRUE, by = "habitatType")
  
  patchChosen <- sample(DT[["habitatType"]],
                        size = 1, replace = FALSE, 
                        prob = DT[["habitatProb"]])
  
    definedPatch <- sample(availableHabs[habitatType == patchChosen, 
                                         direction], size = 1)
  
    print(paste0("The turtle found a nice wintering ground to the ", 
                 definedPatch, " of the destination patch"))
}

saveAndSendResults <- function(turtleResults){
  
  if (!file.exists("data/turtleResults.rds")){
    saveRDS(object = turtleResults, file = "data/turtleResults.rds")
  }
  folderID <- "1PLXw-M8qmQe1T0VKtXcvZCaTMaT7l7L6"
  drive_upload("data/turtleResults.rds", as_id(folderID))
  print("Results uploaded!")
  
}

print("All functions were correctly sourced! You are ready to start.") 

# Additional functions for testing the game:

simulatePatchChoiceSimple <- function(habitatTypeRandom){
  
  defaultProbs <- data.table(
    habitatType = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
    habitatProb = c(0.24, 0.19, 0.16, 0.13, 0.11, 0.09, 0.05, 0.02, 0.01)
  )
  
  availableHabs <- data.table(habitatType = habitatTypeRandom,
                              direction = c("N","NE","E","SE","S","SW","W","NW"))
  
  DT <- merge(availableHabs, defaultProbs, all.x = TRUE, by = "habitatType")
  
  patchChosen <- sample(DT[["habitatType"]],
                        size = 1, replace = FALSE, 
                        prob = DT[["habitatProb"]])
  
  definedPatch <- sample(availableHabs[habitatType == patchChosen, 
                                       habitatType], size = 1)
  return(definedPatch)
}

generateTurtleDataset <- function(nRounds = 5, nMovements = 10){
  arrivals <- rbindlist(lapply(1:nRounds, function(rounds){
    allMovs <- unlist(lapply(1:nMovements, function(movements){
      choice <- simulatePatchChoiceSimple(sample(LETTERS[1:9], size = 8, 
                                                 replace = TRUE))
      return(choice)
    }))
    DT <- data.table(table(allMovs))
    # make sure the data is completed with zeros too
    DT <- merge(data.table(allMovs = LETTERS[1:9]), DT, all.x = TRUE)
    DT[is.na(N), N := 0]
    return(DT)
  }))
  names(arrivals) <- c("habitatType", "totalNumberOfArrivals")
  return(arrivals)
} 
