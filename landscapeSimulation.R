## Simulating Landscape Changes 

# This is the file where all functions for the Landscape Simulation group are 
# defined.

invisible(checkPath("data/", create = TRUE))

getInitialLandscape <- function(){
  
  landscape <- rast(nrow = 10, ncol = 10, res = 1, 
                    ext = c(0, 10, 0, 10))
colorsInitial <- c(
  rep(2, times = 6), rep(4, times = 2), rep(2, times = 2), # Row 1
  2, rep(1, times = 4), 2, rep(4, times = 2), 2, 3, # Row 2
  2, rep(1, times = 4), 2, rep(4, times = 2), 2, 3, # Row 3
  2, rep(1, times = 4), 2, rep(5, times = 2), 2, 3, # Row 4
  2, rep(1, times = 4), rep(2, times = 3), 6, 3, # Row 5
  5, 2, rep(3, times = 4), rep(2, times = 2), 6, 2, # Row 6
  5, 2, rep(3, times = 4), 2, rep(6, times = 2), 4, # Row 7
  5, 2, rep(3, times = 4), rep(6, times = 2), rep(2, times = 2), # Row 8
  rep(4, times = 2), 2, 5, rep(2, times = 2), rep(6, times = 2), rep(2, times = 2), # Row 9
  rep(4, times = 2), 2, rep(5, times = 2), 2, rep(6, times = 2), rep(1, times = 2) # Row 10
)
  values(landscape) <- colorsInitial
landscape <- terra::as.factor(landscape)
names(landscape) <- "landscape"

# setting color to raster

# Values go from 1 to 6
# 1 = 30 years old forest
# 2 = 20 years old forest
# 3 = 10 years old forest
# 4 = Burned in the last 10 years
# 5 = Human disturbance
# 6 = Water

coltab(landscape) <- data.frame(value = 1:6, 
                                col = c("darkgreen","forestgreen","yellowgreen",
                                        "bisque", "grey30", "deepskyblue"))

levels(landscape) <- data.table(ID = c(1:6),
                  landscapeClass = c("30_year_old_forest", "20_year_old_forest",
                                     "10_year_old_forest", "burned_in_the_last_10_years",
                                     "human_disturbance", "water"))
plot(landscape)
return(landscape)
}

simulateForestGrowth <- function(){
  fl <- "data/landscapeSimulationThroughTime.csv"
  if (!file.exists(fl)){
    # if this file doesn't exist, it means it is the first year of simulations
    # We will create the initial table:
    DT <- structure(list(Rows = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                  2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 
                                  3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 
                                  5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 
                                  6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 
                                  8L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 
                                  10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L), 
                         Columns = c("A","B", "C", "D", "E", "F", "G", "H", "I", "J", "A", "B", "C", "D",
                                     "E", "F", "G", "H", "I", "J", "A", "B", "C", "D", "E", "F", "G",
                                     "H", "I", "J", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                                     "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "A", "B", "C",
                                     "D", "E", "F", "G", "H", "I", "J", "A", "B", "C", "D", "E", "F",
                                     "G", "H", "I", "J", "A", "B", "C", "D", "E", "F", "G", "H", "I",
                                     "J", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "A", "B",
                                     "C", "D", "E", "F", "G", "H", "I", "J"), 
                         TimeStep1 = c("20_year_old_forest", 
                                       "20_year_old_forest", "20_year_old_forest", "20_year_old_forest",
                                       "20_year_old_forest", "20_year_old_forest", "burned_in_the_last_10_years",
                                       "burned_in_the_last_10_years", "20_year_old_forest", "20_year_old_forest",
                                       "20_year_old_forest", "30_year_old_forest", "30_year_old_forest",
                                       "30_year_old_forest", "30_year_old_forest", "20_year_old_forest",
                                       "burned_in_the_last_10_years", "burned_in_the_last_10_years",
                                       "10_year_old_forest", "10_year_old_forest", "20_year_old_forest",
                                       "30_year_old_forest", "30_year_old_forest", "30_year_old_forest",
                                       "30_year_old_forest", "20_year_old_forest", "burned_in_the_last_10_years",
                                       "burned_in_the_last_10_years", "20_year_old_forest", "10_year_old_forest",
                                       "20_year_old_forest", "30_year_old_forest", "30_year_old_forest",
                                       "30_year_old_forest", "30_year_old_forest", "20_year_old_forest",
                                       "human_disturbance", "human_disturbance", "20_year_old_forest",
                                       "10_year_old_forest", "20_year_old_forest", "30_year_old_forest",
                                       "30_year_old_forest", "30_year_old_forest", "30_year_old_forest",
                                       "20_year_old_forest", "20_year_old_forest", "20_year_old_forest",
                                       "water", "10_year_old_forest", "human_disturbance", "20_year_old_forest",
                                       "10_year_old_forest", "10_year_old_forest", "10_year_old_forest",
                                       "10_year_old_forest", "20_year_old_forest", "water", "water",
                                       "20_year_old_forest", "human_disturbance", "20_year_old_forest",
                                       "10_year_old_forest", "10_year_old_forest", "10_year_old_forest",
                                       "10_year_old_forest", "20_year_old_forest", "water", "water",
                                       "burned_in_the_last_10_years", "human_disturbance", "20_year_old_forest",
                                       "10_year_old_forest", "10_year_old_forest", "10_year_old_forest",
                                       "10_year_old_forest", "water", "water", "20_year_old_forest",
                                       "20_year_old_forest", "burned_in_the_last_10_years", "burned_in_the_last_10_years",
                                       "20_year_old_forest", "human_disturbance", "20_year_old_forest",
                                       "20_year_old_forest", "water", "water", "20_year_old_forest",
                                       "20_year_old_forest", "burned_in_the_last_10_years", "burned_in_the_last_10_years",
                                       "20_year_old_forest", "human_disturbance", "human_disturbance",
                                       "20_year_old_forest", "water", "water", "30_year_old_forest",
                                       "30_year_old_forest")), row.names = c(NA, -100L), 
                    class = c("data.table", "data.frame"))
    } else {
    DT <- fread("data/landscapeSimulationThroughTime.csv")
    # Check how many columns there are and append to it every new year at the
    # end of simulation.
    # Print message about which year of simulation it is
    print(paste0("Simulating forest growth for time step ", NCOL(DT)-2,"..."))
  }
  growth <- data.table(forestType = c("10_year_old_forest",
                                               "20_year_old_forest",
                                               "30_year_old_forest",
                                               "burned_in_the_last_10_years",
                                               "human_disturbance",
                                               "water"),
                                newType = c("20_year_old_forest",
                                            "30_year_old_forest",
                                            "30_year_old_forest",
                                            "10_year_old_forest",
                                            "human_disturbance",
                                            "water"))
  whichToUse <- names(DT)[NCOL(DT)]
  nameNewTS <- paste0("TimeStep", NCOL(DT)-2+1)
  ord <- c(names(DT), nameNewTS)
  # Append the new forest ages to the data table
  DTtemp <- merge(DT, growth, all.x = TRUE, 
                  by.x = whichToUse, by.y = "forestType")
  names(DTtemp)[names(DTtemp) == "newType"] <- nameNewTS
  DT2 <- DTtemp[, ..ord]
  setkey(DT2, "Rows", "Columns")
  write.csv(DT2, "data/landscapeSimulationThroughTime.csv", row.names = FALSE)
  print(paste0("Forest growth for time step ", NCOL(DT2)-2, " done!"))
  }

simulateHumanDisturbance <- function(intensity = 1){
  fl <- "data/landscapeSimulationThroughTime.csv"
  if (!file.exists(fl)){
    stop(paste0("The file landscapeSimulationThroughTime.csv has been erased. ",
                "Please restart the simulations from simulateForestGrowth()."))
  } else {
    DT <- fread("data/landscapeSimulationThroughTime.csv")
    # Check how many columns there are and append to it every new year at the
    # end of simulation.
  }
  disturbanceProbability <- data.table(forestType = c("10_year_old_forest",
                                               "20_year_old_forest",
                                               "30_year_old_forest",
                                               "burned_in_the_last_10_years",
                                               "human_disturbance",
                                               "water"),
                                distProbability = c(0.05*intensity, 
                                                    0.03*intensity,
                                                    0.01*intensity,
                                                    0.06*intensity,
                                                    0,
                                                    0))
  whichToUse <- names(DT)[NCOL(DT)]
  # Append fire and human disturbance probabilities to the data
  # use that to calculate
  DTtemp <- merge(DT, disturbanceProbability, all.x = TRUE, 
                  by.x = whichToUse, by.y = "forestType")
  DTtemp[, fp := rbinom(n = NROW(DTtemp), size = 1, 
                        prob = distProbability),]
  DT2 <- merge(DT, DTtemp[, c("Rows", "Columns", "fp")], 
               by = c("Rows", "Columns"))
  DT2[, NC := get(whichToUse)]
  DT2[fp == 1, NC := "human_disturbance"]
  # Need to replace the last column by the updated one
  if (all(DT2[["fp"]] == 0))
    print(paste0("No new human disturbance in time step ", NCOL(DT)-2)) else
      print(paste0("We have ", sum(DT2[["fp"]] == 1)," new human disturbances in time step ", NCOL(DT)-2))
  DT2[, c(paste(whichToUse), "fp") := NULL]
  names(DT2)[names(DT2) == "NC"] <- whichToUse
  write.csv(DT2, "data/landscapeSimulationThroughTime.csv", row.names = FALSE)
  print(paste0("Human disturbance simulation for time step ", NCOL(DT2)-2, 
               " done!"))
}

simulateFire <- function(intensity = 1){
  fl <- "data/landscapeSimulationThroughTime.csv"
  if (!file.exists(fl)){
    stop(paste0("The file landscapeSimulationThroughTime.csv has been deleted. ",
                "Please restart the simulations from simulateForestGrowth()."))
  } else {
    DT <- fread("data/landscapeSimulationThroughTime.csv")
    # Check how many columns there are and append to it every new year at the
    # end of simulation.
  }
  fireProbability <- data.table(forestType = c("10_year_old_forest",
                                               "20_year_old_forest",
                                               "30_year_old_forest",
                                               "burned_in_the_last_10_years",
                                               "human_disturbance",
                                               "water"),
                                burnProbability = c(0.06*intensity, 
                                                    0.13*intensity,
                                                    0.20*intensity,
                                                    0.04*intensity,
                                                    0,
                                                    0))
  whichToUse <- names(DT)[NCOL(DT)]
  # Append fire and human disturbance probabilities to the data
  # use that to calculate
  DTtemp <- merge(DT, fireProbability, all.x = TRUE, 
                  by.x = whichToUse, by.y = "forestType")
  DTtemp[, fp := rbinom(n = NROW(DTtemp), size = 1, 
                        prob = burnProbability),]
  DT2 <- merge(DT, DTtemp[, c("Rows", "Columns", "fp")], 
               by = c("Rows", "Columns"))
  DT2[, NC := get(whichToUse)]
  DT2[fp == 1, NC := "burned_in_the_last_10_years"]
  if (all(DT2[["fp"]] == 0))
    print(paste0("No fires in time step ", NCOL(DT)-2)) else
      print(paste0("We have ", sum(DT2[["fp"]] == 1)," new fires in time step ", NCOL(DT)-2))
  # Need to replace the last column by the updated one
  DT2[, c(paste(whichToUse), "fp") := NULL]
  names(DT2)[names(DT2) == "NC"] <- whichToUse
  write.csv(DT2, "data/landscapeSimulationThroughTime.csv", row.names = FALSE)
  print(paste0("Fire simulation for time step ", NCOL(DT2)-2, 
               " done!"))
}

checkChangesInLandscape <- function(){
  fl <- "data/landscapeSimulationThroughTime.csv"
  
  if (!file.exists(fl)){
    stop(paste0("The file landscapeSimulationThroughTime.csv has been deleted. ",
                "Please restart the simulation."))
  } else {
    DT <- fread("data/landscapeSimulationThroughTime.csv")
    # Check how many columns there are and append to it every new year at the
    # end of simulation.
  }
  nms <- names(DT)[(NCOL(DT)-1):(NCOL(DT))]
  whichToKeep <- c("Rows", "Columns", names(DT)[NCOL(DT)])
  # Select only the ones that changed
  DTtemp <- DT[get(nms[1]) != get(nms[2]), ..whichToKeep]
  # Which pixels were disturbed by humans
  whichDisturbed <- DTtemp[get(names(DT)[(NCOL(DT))]) == "human_disturbance", 
                           c("Rows", "Columns")]
  whichDisturbed[, XY := paste0(Rows, Columns)]
  print(paste0("The following pixels changed to human disturbance (pick between grey and black): ",
               paste(whichDisturbed[["XY"]], collapse = ", ")))
  # Which pixels were burned
  whichBurned <- DTtemp[get(names(DT)[(NCOL(DT))]) == "burned_in_the_last_10_years", 
                       c("Rows", "Columns")]
  whichBurned[, XY := paste0(Rows, Columns)]
  print(paste0("The following pixels changed to burned in the last 10 years (color light yellow): ",
               paste(whichBurned[["XY"]], collapse = ", ")))
  # In which pixels forest grew from burned to 10 years old 
  whichGrew10 <- DTtemp[get(names(DT)[(NCOL(DT))]) == "10_year_old_forest", 
                        c("Rows", "Columns")]
  whichGrew10[, XY := paste0(Rows, Columns)]
  print(paste0("The following pixels grew from burned in the last 10 years to ",
               "10 years old forest (color light green): ",
               paste(whichGrew10[["XY"]], collapse = ", ")))
  # In which pixels forest grew from 10 years old to 20 years old 
  whichGrew20 <- DTtemp[get(names(DT)[(NCOL(DT))]) == "20_year_old_forest", 
                        c("Rows", "Columns")]
  whichGrew20[, XY := paste0(Rows, Columns)]
  print(paste0("The following pixels grew from 10 year old forest ",
               "to 20 year old forest (color middle green): ",
               paste(whichGrew20[["XY"]], collapse = ", ")))
  # In which pixels forest grew from burned to 10 years old 
  whichGrew30 <- DTtemp[get(names(DT)[(NCOL(DT))]) == "30_year_old_forest", 
                        c("Rows", "Columns")]
  whichGrew30[, XY := paste0(Rows, Columns)]
  print(paste0("The following pixels grew from 20 year old forest ",
               "to 30 year old forest (color middle green): ",
               paste(whichGrew30[["XY"]], collapse = ", ")))
  return(invisible(DT))
}

updateLandscape <- function(landscape, changesDetected){
  whichToKeep <- c("Rows", "Columns", names(changesDetected)[NCOL(changesDetected)])
  tempDT <- changesDetected[, ..whichToKeep]
  # Make sure the order is correct!
  setkey(tempDT, "Rows", "Columns")
  
  codeDT <- data.table(code = 1:6, 
                       habitat = c("30_year_old_forest", "20_year_old_forest",
                                   "10_year_old_forest", "burned_in_the_last_10_years",
                                   "human_disturbance", "water"))
  newClass <- merge(tempDT, codeDT, 
                    by.x = names(changesDetected)[NCOL(changesDetected)], 
                    by.y = "habitat")
  newClass <- newClass[, c("Rows", "Columns", "code")]
  setkey(newClass, "Rows", "Columns")
  landscape2 <- landscape
  landscape2[] <- newClass[["code"]]
  coltab(landscape2) <- data.frame(value = 1:6,
                                  col = c("darkgreen","forestgreen","yellowgreen",
                                          "bisque", "grey30", "deepskyblue"))
  levels(landscape2) <- data.table(ID = c(1:6),
                                  landscapeClass = c("30_year_old_forest", "20_year_old_forest",
                                                     "10_year_old_forest", "burned_in_the_last_10_years",
                                                     "human_disturbance", "water"))
  plot(landscape2)
  print("Landscape updated!")
  return(landscape2)
}

saveLandscapeResults <- function(landscape, upload = FALSE){

  if (!file.exists("data/landscapeResults.tif")){
    writeRaster(landscape, filename = "data/landscapeResults.tif", 
                filetype = "GTiff")
  }
  folderID <- "1C8s1O_PKVz1wwWg9dh_qppTmG2_hRUoi"
  if (upload) {
    tryCatch(expr = {
      drive_upload("data/landscapeResults.tif", as_id(folderID))
      print("Results uploaded!")
      }, error = function(e){
               print(paste0("Upload failed. Results saved as: ", file.path(getwd(), "data/landscapeResults.tif")))
             })
  } else {print("Results saved!")}
   
}

startOverLandscape <- function(){
  unlink(x = file.path(getwd(), "data", "landscapeSimulationThroughTime.csv"))
  unlink(x = file.path(getwd(), "data", "landscapeResults.*"))
  print("Ready for another round?")
}

print("All functions were correctly sourced! You are ready to start.") 
 