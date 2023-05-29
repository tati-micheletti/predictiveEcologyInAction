## Simulating Bird Abundance

# This is the file where all functions for the Bird Simulation group are 
# defined.
# Create table:
bh <- structure(list(A = numeric(0), B = numeric(0), C = numeric(0), 
               D = numeric(0), E = numeric(0), G = numeric(0)), 
          row.names = c(NA, -1L), class = c("data.table", "data.frame"))
write.csv(bh, file = file.path(getwd(), "data/birdHabitat.csv"), row.names = FALSE)

availableHabitatsBirds <- function(visible = TRUE){
  ah <- structure(list(habitatType = c(
    "A", "B", "C", "D", "E", "G"), 
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
  if (visible) print(ah)
}

loadBirdsTable <- function(){
  DT <- fread("data/birdHabitat.csv", header = TRUE)
  return(DT)
}

calculateObservations <- function(DT){
  
  DT3 <- structure(list(A = c(6L, 2L, 0L, 10L, 17L, 0L, 0L, 0L, 0L, 12L, 3L, 5L, 0L, 10L, 3L), 
                        B = c(0L, 14L, 0L, 6L, 2L, 0L, 0L, 0L, 3L, 11L, 5L, 0L, 0L, 4L, 5L), 
                        C = c(0L, 0L, 4L, 0L, 0L, 4L, 1L, 2L, 0L, 0L, 2L, 6L, 2L, 0L, 2L), 
                        D = c(15L, 0L, 12L, 9L, 4L, 17L, 11L, 21L, 14L, 2L, 10L, 14L, 16L, 9L, 9L), 
                        E = c(0L, 9L, 1L, 0L, 0L, 0L, 4L, 0L, 4L, 0L, 1L, 0L, 5L, 2L, 1L),
                        G = c(4L, 0L, 8L, 0L, 2L, 4L, 9L, 2L, 4L, 0L, 4L, 0L, 2L, 0L, 5L),
                        counts = c(0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 2L, 1L, 2L, 0L, 1L, 1L)), 
                   row.names = c(NA, -15L), class = c("data.table", "data.frame"))
  # DT2 <- data.table(structure(list(
  #   A = c(23L, 24L, 18L, 25L, 22L, 20L, 18L, 10L, 16L, 11L, 9L, 17L, 10L, 13L, 3L, 1L, 0L, 2L, 1L, 0L, 3L),
  #   B = c(5L, 6L, 4L, 7L, 9L, 5L, 6L, 32L, 26L, 34L, 27L, 20L, 32L, 28L, 11L, 17L, 12L, 7L, 18L, 13L, 15L),
  #   C = c(29L, 24L, 33L, 26L, 19L, 31L, 26L, 4L, 5L, 3L, 7L, 9L, 5L, 8L, 1L, 0L, 1L, 0L, 1L, 1L, 0L),
  #   D = c(12L, 16L, 14L, 9L, 17L, 12L, 15L, 20L, 21L, 19L, 25L, 22L, 20L, 19L, 5L, 4L, 3L, 8L, 8L, 4L, 7L),
  #   E = c(0L, 0L, 1L, 0L, 1L, 2L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 28L, 26L, 34L, 27L, 19L, 30L, 27L),
  #   counts = c(24L, 26L, 22L, 24L, 23L, 28L, 22L, 17L, 19L, 20L, 18L, 19L, 19L, 16L, 3L, 2L, 5L, 3L, 1L, 2L, 3L)),
  #   row.names = c(NA, -21L),
  #   class = c("data.frame")))

  # countsMod <- glm(formula = counts ~ A + B + C + D + E, 
  #                  family = "poisson", data = DT2)
  countsMod <- glm(formula = counts ~ A + B + C + D + E + G, 
                   family = "poisson", data = DT3)
  
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

saveBirdResults <- function(birdModel, upload = FALSE){
  
  if (!file.exists("data/birdResults.rds")){
    saveRDS(object = birdModel, file = "data/birdResults.rds")
  }
  folderID <- "15QOytBmeU-8BBXhfclkIFa-wYBIfUoSA"
  if (upload) {
    drive_upload("data/birdResults.rds", as_id(folderID))
    print("Results uploaded!")
  } else {print("Results saved!")}
  
}

print("All functions were correctly sourced! You are ready to start.") 

# Additional functions for testing the game:

generateBirdsTable <- function(){
  DT <- structure(list(A = c(6L, 2L, 0L, 10L, 17L, 0L, 0L, 0L, 0L, 12L, 3L, 5L, 0L, 10L, 3L), 
                       B = c(0L, 14L, 0L, 6L, 2L, 0L, 0L, 0L, 3L, 11L, 5L, 0L, 0L, 4L, 5L), 
                       C = c(0L, 0L, 4L, 0L, 0L, 4L, 1L, 2L, 0L, 0L, 2L, 6L, 2L, 0L, 2L), 
                       D = c(15L, 0L, 12L, 9L, 4L, 17L, 11L, 21L, 14L, 2L, 10L, 14L, 16L, 9L, 9L), 
                       E = c(0L, 9L, 1L, 0L, 0L, 0L, 4L, 0L, 4L, 0L, 1L, 0L, 5L, 2L, 1L),
                       G = c(4L, 0L, 8L, 0L, 2L, 4L, 9L, 2L, 4L, 0L, 4L, 0L, 2L, 0L, 5L),
                       counts = c(0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 2L, 1L, 2L, 0L, 1L, 1L)), 
                  row.names = c(NA, -15L), class = c("data.table", "data.frame"))
  write.csv(DT, file = file.path(getwd(), "data/birdHabitat.csv"), row.names = FALSE)
  return(DT[,1:6])
}

startOverBirds <- function(){
  bh <- structure(list(A = numeric(0), B = numeric(0), C = numeric(0), 
                       D = numeric(0), E = numeric(0), G = numeric(0)), 
                  row.names = c(NA, -1L), class = c("data.table", "data.frame"))
  write.csv(bh, file = file.path(getwd(), "data/birdHabitat.csv"), row.names = FALSE)
  # 4. Delete the file birdResults.rds
  unlink(x = file.path(getwd(), "data", "birdResults.rds"))
  print("Ready for another round?")
}

