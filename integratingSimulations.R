## Integrating all work done

# This is the file where all functions for integrating all the work done by 
# the three groups are defined.

getAllData <- function(){
  allFls <- data.table(drive_ls(path = as_id("1PLXw-M8qmQe1T0VKtXcvZCaTMaT7l7L6")))
  landscape <- prepInputs(url = paste0("https://drive.google.com/file/d/",
                                       allFls[name == "landscapeResults.tif", id]),
                          targetFile = "landscapeResults.tif", 
                          destinationPath = checkPath("results", create = TRUE))
  birdResults <- prepInputs(url = paste0("https://drive.google.com/file/d/",
                                         allFls[name == "birdResults.rds", id]),
                            targetFile = "birdResults.rds", 
                            destinationPath = checkPath("results", create = TRUE),
                            fun = "readRDS")
  turtleResults <- prepInputs(url = paste0("https://drive.google.com/file/d/",
                                         allFls[name == "turtleResults.rds", id]),
                            targetFile = "turtleResults.rds", 
                            destinationPath = checkPath("results", create = TRUE),
                            fun = "readRDS")
  return(list(landscape = landscape,
              birdResults = birdResults,
              turtleResults = turtleResults))
}

generateBirdMaps <- function(initialLandscape, finalLandscape, birdModel){
  birdMaps <-
    raster::stack(lapply(c("initialLandscape", "finalLandscape"),
                         function(MAP) {
                           lands <- get(MAP)
                           DT <- data.table(
                             pix = LETTERS[getValues(lands)],
                             A = 0,
                             B = 0,
                             C = 0,
                             D = 0,
                             E = 0,
                             F = 0
                           )
                           for (L in LETTERS[1:6])
                             DT[pix == L, eval(L) := 1]
                           DT[, pix := NULL]
                           predIL <- suppressWarnings(predict(birdModel, newdata = DT))
                           birdMap <- setValues(raster(lands), values = predIL)
                           return(birdMap)
                         }))
  names(birdMaps) <- c("Now", "Future")
  return(birdMaps)
}

generateTurtleMaps <- function(initialLandscape, finalLandscape, habitats){
  turtleMaps <- raster::stack(lapply(c("initialLandscape", "finalLandscape"), 
                                   function(MAP){
                                     lands <- get(MAP)
                                     pixWater <- lapply(1:ncell(lands), 
                                                                  function(CELL){
                                                                   ad <- adjacent(lands, CELL, 
                                                                             directions = 8, 
                                                                             pairs = FALSE, 
                                                                             include = TRUE)
                                                                   cellVal <- lands[ad]
                                                                   if (!6 %in% cellVal)
                                                                    return(NULL)
                                                                   valInCell <- lands[CELL]
                                                                   if (valInCell %in% c(4:6))
                                                                     return(NULL) else 
                                                                       return(data.table(cell = CELL,
                                                                                     oldVal = lands[CELL],
                                                                                     newVal = lands[CELL]+10))
                                                                  })
                                     pixWater[sapply(pixWater, is.null)] <- NULL
                                     pixWater <- rbindlist(pixWater)
                                     lands[pixWater[["cell"]]] <- pixWater[["newVal"]]
                                     habPref <- merge(habitats[, c("habitatType", "habitatPreference")],
                                                           data.table(rasValue = c(6, 11:13, 1:5),
                                                                      habitatType = LETTERS[1:9]), 
                                                      by = "habitatType")
                                     rasVals <- data.table(pixelID = 1:ncell(lands),
                                                           vals = getValues(lands))
                                     rasVals <- merge(rasVals, habPref,
                                                      by.x = "vals", 
                                                      by.y = "rasValue")
                                     setkey(rasVals, "pixelID")
                                     landsF <- setValues(x = raster(lands), 
                                                         values = rasVals[["habitatPreference"]])
                                     return(landsF)
                                   }))
  names(turtleMaps) <- c("Now", "Future")
  return(turtleMaps)
}

ggplotRegression <- function(fit) {
# FROM: https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_jitter() +
    stat_smooth(method = "lm", col = "red") +
    theme_light() +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared,3),
                       "; Intercept =",signif(fit$coef[[1]],3),
                       "; Slope =",signif(fit$coef[[2]],3),
                       "; p =",ifelse(signif(summary(fit)$coef[2,4],3) < 0.05, 
                                     "significant", "non-significant")))
}

areBirdsGoodUmbrellaForTurtle <- function(birdMaps, turtleMaps){
  corrResults <- lapply(1:2, function(index){
    DT <- data.table(turtles = getValues(turtleMaps[[index]]),
                     birds = getValues(birdMaps[[index]]))
    crResult <- cor(DT[["turtles"]], DT[["birds"]], method = "spearman")
    fit <- lm(turtles ~ birds, data = DT)
    p <- ggplotRegression(fit)
    return(p)
  })
  names(corrResults) <- c("Now", "Future")
 p <- grid.arrange(corrResults[["Now"]], corrResults[["Future"]], nrow = 1)
  return(p)
}


