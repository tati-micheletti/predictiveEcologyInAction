## Simulating Bird Abundance

# This is the file where all functions for the Bird Simulation group are 
# defined.

availableHabitats <- function(){
  ah <- structure(list(habitatType = c(
    "A", "B", "C", "D", "E", "F"), 
    habitatDescription = c(
      "Recently burned patch",
      "Young forest",
      "Human disturbed patch",      
      "Middle-aged forest",
      "Old forest",
      "Water"), 
    habitatColor = c(
      "light yellow",
      "light green",
      "grey or black",
      "middle green",
      "dark green",
      "light blue")), 
    row.names = c(NA, -6L), 
    class = c("data.table", "data.frame"))
  print(ah)
}

loadBirdsTable <- function(){
  DT <- fread("data/birdHabitat.csv", header = TRUE,
              select = c(LETTERS[1:5]))
  return(DT)
}

calculateObservations <- function(DT){
  DT2 <- data.table(structure(list(
    A = c(23L, 24L, 18L, 25L, 22L, 20L, 18L, 10L, 16L, 11L, 9L, 17L, 10L, 13L, 3L, 1L, 0L, 2L, 1L, 0L, 3L),
    B = c(5L, 6L, 4L, 7L, 9L, 5L, 6L, 32L, 26L, 34L, 27L, 20L, 32L, 28L, 11L, 17L, 12L, 7L, 18L, 13L, 15L),
    C = c(29L, 24L, 33L, 26L, 19L, 31L, 26L, 4L, 5L, 3L, 7L, 9L, 5L, 8L, 1L, 0L, 1L, 0L, 1L, 1L, 0L),
    D = c(12L, 16L, 14L, 9L, 17L, 12L, 15L, 20L, 21L, 19L, 25L, 22L, 20L, 19L, 5L, 4L, 3L, 8L, 8L, 4L, 7L),
    E = c(0L, 0L, 1L, 0L, 1L, 2L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 28L, 26L, 34L, 27L, 19L, 30L, 27L),
    counts = c(24L, 26L, 22L, 24L, 23L, 28L, 22L, 17L, 19L, 20L, 18L, 19L, 19L, 16L, 3L, 2L, 5L, 3L, 1L, 2L, 3L)),
    row.names = c(NA, -21L),
    class = c("data.frame")))

  countsMod <- glm(formula = counts ~ A + B + C + D + E, 
                   family = "poisson", data = DT2)
  
  predicted <- round(predict(object = countsMod, newdata = DT, 
                       type = "response"), 0)
  
  # Draw a bit randomly within the bounds to add a bit of deviance  
  predicted <- unlist(lapply(predicted, function(P) 
    round(rnorm(1, mean = P, sd = 0.7), 0)))

  predicted[predicted < 0] <- 0
  
  DT <- data.table(round = 1:length(predicted),
                   counts = as.numeric(predicted))
  print(paste0("The following birds were observed in each round: "))
  print(DT)
}

saveAndSendResults <- function(birdModel){
  
  if (!file.exists("data/birdResults.rds")){
    saveRDS(object = birdModel, file = "data/birdResults.rds")
  }
  folderID <- "1PLXw-M8qmQe1T0VKtXcvZCaTMaT7l7L6"
  drive_upload("data/birdResults.rds", as_id(folderID))
  print("Results uploaded!")
  
}

print("All functions were correctly sourced! You are ready to start.") 

# Additional functions for testing the game:

generateBirdsTable <- function(){
  DT <- structure(list(A = c(26L, 1L, 9L, 22L, 5L, 14L, 19L, 10L, 1L, 
                             6L, 0L, 12L, 2L, 16L, 11L, 19L, 10L, 1L, 6L, 0L, 12L, 2L, 16L, 
                             48L, 0L, 0L, 0L, 0L), 
                       B = c(10L, 21L, 18L, 11L, 12L, 3L, 4L, 14L, 4L, 12L, 
                             12L, 2L, 10L, 5L, 11L, 16L, 4L, 27L, 6L, 18L, 2L, 
                             12L, 3L, 0L, 0L, 48L, 0L, 0L), 
                       C = c(0L, 6L, 8L, 7L, 13L, 4L, 7L, 9L, 11L, 24L, 2L, 16L, 
                             15L, 24L, 11L, 7L, 9L, 11L, 24L, 2L, 16L, 15L, 24L, 
                             0L, 0L, 0L, 48L, 0L), 
                       D = c(0L, 16L, 5L, 6L, 18L, 22L, 2L, 11L, 5L, 0L, 16L, 
                             16L, 9L, 0L, 11L, 4L, 14L, 4L, 12L, 16L, 16L, 9L, 
                             0L, 0L, 0L, 0L, 0L, 48L), 
                       E = c(12L, 4L, 8L, 2L, 0L, 5L, 16L, 4L, 27L, 6L, 18L, 
                             2L, 12L, 3L, 4L, 2L, 11L, 5L, 0L, 12L, 2L, 10L, 
                             5L, 0L, 48L, 0L, 0L, 0L)), 
                  class = "data.frame", row.names = c(NA, -28L))
  
}
