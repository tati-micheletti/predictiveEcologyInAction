## Integrating all work done

# This is the file where all functions for integrating all the work done by 
# the three groups are defined.

invisible(checkPath("data/", create = TRUE))

getAllData <- function(){
  allFls <- data.table(drive_ls(path = as_id("15QOytBmeU-8BBXhfclkIFa-wYBIfUoSA")))
  landscape <- prepInputs(url = paste0("https://drive.google.com/file/d/",
                                       allFls[name == "landscapeResults.tif", id]),
                          targetFile = "landscapeResults.tif", 
                          destinationPath = checkPath("results", create = TRUE))
  coltab(landscape) <- c(NA, "darkgreen","forestgreen","yellowgreen",
                          "bisque", "grey30", "deepskyblue")
  
  levels(landscape) <- data.table(ID = c(1:6),
                                   landscapeClass = c("30_year_old_forest", "20_year_old_forest",
                                                      "10_year_old_forest", "burned_in_the_last_10_years",
                                                      "human_disturbance", "water"))
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
    c(lapply(c("initialLandscape", "finalLandscape"),
                         function(MAP) {
                           lands <- get(MAP)
                           DT <- data.table(
                             pix = LETTERS[values(lands)],
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
                           birdMap <- rast(lands)
                           values(birdMap) <- predIL
                           return(birdMap)
                         }))
  names(birdMaps) <- c("Now", "Future")
  return(birdMaps)
}

generateTurtleMaps <- function(initialLandscape, finalLandscape, habitats){
  turtleMaps <- c(lapply(c("initialLandscape", "finalLandscape"), 
                                   function(MAP){
                                     lands <- get(MAP)
                                     pixWater <- lapply(1:ncell(lands), 
                                                                  function(CELL){
                                                                   ad <- na.omit(as.numeric(adjacent(lands, CELL, 
                                                                             directions = 8, 
                                                                             pairs = FALSE, 
                                                                             include = TRUE)))
                                                                   cellType <- data.table(lands[ad])
                                                                   cellVal <- cellType[["landscapeClass"]]
                                                                   if (!"water" %in% cellVal)
                                                                    return(NULL)
                                                                   cellType <- data.table(lands[CELL])
                                                                   valInCell <- cellType[["landscapeClass"]]
                                                                   if (valInCell %in% c("burned_in_the_last_10_years",
                                                                                        "human_disturbance",
                                                                                        "water"))
                                                                     return(NULL) else
                                                                       return(data.table(cell = as.numeric(CELL),
                                                                                         oldVal = as.numeric(lands[CELL]),
                                                                                         newVal = as.numeric(lands[CELL])+10))
                                                                  })
                                     pixWater[sapply(pixWater, is.null)] <- NULL
                                     pixWater <- rbindlist(pixWater)
                                     lands[pixWater[["cell"]]] <- pixWater[["newVal"]]
                                     habPref <- merge(habitats[, c("habitatType", "habitatPreference")],
                                                           data.table(rasValue = c(6, 11:13, 1:5),
                                                                      habitatType = LETTERS[1:9]), 
                                                      by = "habitatType")
                                     rasVals <- data.table(pixelID = 1:ncell(lands),
                                                           vals = values(lands))
                                     rasVals2 <- merge(rasVals, habPref,
                                                      by.x = "vals.landscapeClass", 
                                                      by.y = "rasValue")
                                     setkey(rasVals2, "pixelID")
                                     landsF <- lands
                                     values(landsF) <- rasVals2[["habitatPreference"]]
                                     return(landsF)
                                   }))
  names(turtleMaps) <- c("Now", "Future")
  return(turtleMaps)
}

plotMaps <- function(allMaps, 
                     col = c(NULL, NULL,
                             heat.colors(6), heat.colors(6),
                             heat.colors(6), heat.colors(6)),
                     mapsNames = c("Initial Land", "Final Land", 
                                   "Initial Birds", "Final Birds", 
                                   "Initial Turtles", "Final Turtles")){
  if((nlayers2(allMaps) %% 2) == 0) {
    plot(allMaps, nr = nlayers2(allMaps)/2, main = mapsNames)
  } else {
    print(paste("Only pairs of maps accepted (i.e., even number of maps)"))
  }
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
    DT <- data.table(turtles = as.numeric(values(turtleMaps[[index]])),
                     birds = as.numeric(values(birdMaps[[index]])))
    crResult <- cor(DT[["turtles"]], DT[["birds"]], method = "spearman")
    fit <- lm(turtles ~ birds, data = DT)
    p <- ggplotRegression(fit)
    return(p)
  })
  names(corrResults) <- c("Now", "Future")
 p <- grid.arrange(corrResults[["Now"]], corrResults[["Future"]],
                   nrow = 1)
  return(p)
}


