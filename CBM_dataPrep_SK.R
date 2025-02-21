defineModule(sim, list(
  name = "CBM_dataPrep_SK",
  description = "A data preparation module to format and prepare user-provided input to the SpaDES forest-carbon modelling family.",
  keywords = NA,
  authors = c(
    person("Celine", "Boisvenue", email = "Celine.Boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.2", CBM_dataPrep_SK = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("CBM_dataPrep_SK.Rmd"),
  reqdPkgs = list(
    "data.table", "fasterize", "magrittr", "RSQLite", "sf", "terra",
    "reproducible (>=2.1.2)" ,
    "PredictiveEcology/CBMutils@development (>=0.0.7.9016)",
    "PredictiveEcology/LandR@development"
  ),
  parameters = rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA,
                    "Should caching of events or module be used?")
  ),

  inputObjects = bindrows(
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA,
                 sourceURL = NA), # FROM DEFAULTS
    expectsInput(
      objectName = "spinupSQL", objectClass = "dataset", desc = NA, sourceURL = NA), # FROM DEFAULTS
    expectsInput(
      objectName = "species_tr", objectClass = "dataset", desc = NA, sourceURL = NA), # FROM DEFAULTS
    expectsInput(
      objectName = "gcMeta", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      sourceURL =
        "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ/view?usp=sharing"), # FROM VOL2BIOMASS
    expectsInput(
      objectName = "gcMetaURL", objectClass = "character",
      desc = "URL for gcMeta"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = paste("User file containing:",
                   "`gcids`, `Age`, `MerchVolume`.",
                   "Default name `userGcM3`."),
      sourceURL = "https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m"),
    expectsInput(
      objectName = "userGcM3URL", objectClass = "character",
      desc = "URL for userGcM3"),
    expectsInput(
      objectName = "cbmAdmin", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries,",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids. This is used in the CBM_vol2biomass module"),
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"),
    expectsInput(
      objectName = "cbmAdminURL", objectClass = "character",
      desc = "URL for cbmAdmin"),
    expectsInput(
      objectName = "masterRaster", objectClass = "SpatRaster",
      desc = "Raster has NAs where there are no species and the pixel groupID where the pixels were simulated. It is used to map results",
      sourceURL = "https://drive.google.com/file/d/1zUyFH8k6Ef4c_GiWMInKbwAl6m6gvLJW"),
    expectsInput(
      objectName = "masterRasterURL", objectClass = "character",
      desc = "URL for `masterRaster` - optional, need this or a `masterRaster` object."),
    expectsInput(
      objectName = "ageRaster", objectClass = "SpatRaster",
      desc = "Raster ages for each pixel",
      sourceURL = "https://drive.google.com/file/d/1hylk0D1vO19Dpg4zFtnSNhnyYP4j-bEA"),
    expectsInput(
      objectName = "ageRasterURL", objectClass = "character",
      desc = "URL for ageRaster - optional, need this or a ageRaster"),
    expectsInput(
      objectName = "gcIndexRaster", objectClass = "SpatRaster",
      desc = "Raster giving the growth curve value for each pixel",
      sourceURL = "https://drive.google.com/file/d/1yunkaYCV2LIdqej45C4F9ir5j1An0KKr"),
    expectsInput(
      objectName = "gcIndexRasterURL", objectClass = "character",
      desc = "URL for gcIndexRaste - optional, need this or a ageRaster"),
    expectsInput(
      objectName = "spuLocator", objectClass = "sf|SpatRaster",
      desc = paste(
        "Spatial data source from which spatial unit IDs can be extracted.",
        "An output of CBM_defaults.")),
    expectsInput(
      objectName = "ecoLocator", objectClass = "sf|SpatRaster",
      desc = paste(
        "Spatial data source from which ecozone IDs extracted.",
        "An output of CBM_defaults.")),
    expectsInput(
      objectName = "disturbanceRasters", objectClass = "character",
      sourceURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",
      desc = paste(
        "Required input to CBM_core.",
        "If not specified elsewhere, the default source URL is provided.",
        "The default is the Wulder and White disturbance rasters for SK covering 1984-2011."
      )),
    expectsInput(
      objectName = "disturbanceRastersURL", objectClass = "character",
      desc = "URL for disturbanceRasters"),
    expectsInput(
      objectName = "userDist", objectClass = "data.table",
      sourceURL = "https://docs.google.com/spreadsheets/d/1fOikb83aOuLlFYIn6pjmC7Jydjcy77TH",
      desc = paste(
        "Table defines the values present in the disturbance rasters.",
        "This will be matched with CBM-CFS3 disturbances to create the 'mySpuDmids' table.",
        "Required if the user has provided non-default disturbanceRasters",
        "and the CBM_core input 'mySpuDmids' is not provided elsewhere."),
      columns = c(
        rasterID   = "ID links to pixel values in the disturbance rasters",
        wholeStand = "Specifies if the whole stand is disturbed (1 = TRUE; 0 = FALSE)",
        name       = "Disturbance name (e.g. 'Wildfire')"
      )),
    expectsInput(
      objectName = "userDistURL", objectClass = "character",
      desc = "URL for userDist"),
  ),

  outputObjects = bindrows(
    createsOutput(
      objectName = "allPixDT", objectClass = "data.table",
      desc = "Table summarizing raster input data with 1 row for every 'masterRaster' pixel (including NAs)",
      columns = c(
        pixelIndex      = "'masterRaster' cell index",
        ages            = "Stand ages extracted from input 'ageRaster'",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'",
        gcids           = "Growth curve IDs extracted from input 'gcIndexRaster'",
        ecozones        = "Ecozone IDs extracted from input 'ecoRaster'"
      )),
    createsOutput(
      objectName = "spatialDT", objectClass = "data.table",
      desc = paste(
        "Table summarizing raster input data with 1 row for every 'masterRaster' pixel that is not NA",
        "Required input to CBM_vol2biomass and CBM_core."),
      columns = c(
        pixelIndex      = "'masterRaster' cell index",
        pixelGroup      = "Pixel group ID",
        ages            = "Stand ages extracted from input 'ageRaster'",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'",
        gcids           = "Growth curve IDs extracted from input 'gcIndexRaster'",
        ecozones        = "Ecozone IDs extracted from input 'ecoRaster'"
      )),
    createsOutput(
      objectName = "level3DT", objectClass = "data.table",
      desc = paste(
        "Table associating pixel groups with their key attributes.",
        "Required input to CBM_vol2biomass and CBM_core."),
      columns = c(
        pixelGroup      = "Pixel group ID",
        ages            = "Stand ages extracted from input 'ageRaster' modified such that all ages are >=3",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'",
        gcids           = "Factor of growth curve IDs extracted from input 'gcIndexRaster'",
        ecozones        = "Ecozone IDs extracted from input 'ecoRaster'"
      )),
    createsOutput(
      objectName = "speciesPixelGroup", objectClass = "data.frame",
      desc = paste(
        "Table connecting pixel groups to species IDs.",
        "Required input to CBM_core."),
      columns = c(
        pixelGroup = "Pixel group ID",
        species_id = "Species ID"
      )),
    createsOutput(
      objectName = "curveID", objectClass = "character",
      desc = paste(
        "Column names in 'level3DT' that uniquely define each pixel group growth curve ID.",
        "Required input to CBM_vol2biomass")),
    createsOutput(
      objectName = "ecozones", objectClass = "numeric",
      desc = paste(
        "Ecozone IDs extracted from input 'ecoRaster' for each pixel group.",
        "Required input to CBM_vol2biomass")),
    createsOutput(
      objectName = "spatialUnits", objectClass = "numeric",
      desc = paste(
        "Spatial unit IDs extracted from input 'spuRaster' for each pixel group.",
        "Required input to CBM_vol2biomass")),
    createsOutput(
      objectName = "realAges", objectClass = "numeric",
      desc = paste(
        "Stand ages extracted from input 'ageRaster' for each pixel group.",
        "Required input to CBM_core.")),
    createsOutput(
      objectName = "disturbanceRasters", objectClass = "character",
      desc = paste(
        "List of disturbance rasters named by the disturbance year.",
        "This is either downloaded from the default URL or a user provided URL.",
        "Required input to CBM_core.")),
    createsOutput(
      objectName = "mySpuDmids", objectClass = "data.frame",
      desc = paste(
        "Table summarizing CBM-CFS3 disturbances possible within the spatial units.",
        "This links CBM-CFS3 disturbances with the values in the disturbance rasters.",
        "Required input to CBM_core."),
      columns = c(
        rasterID              = "Raster ID from 'userDist'",
        wholeStand            = "wholeStand flag from 'userDist'",
        spatial_unit_id       = "Spatial unit ID",
        disturbance_type_id   = "Disturbance type ID",
        disturbance_matrix_id = "Disturbance matrix ID",
        name                  = "Disturbance name",
        description           = "Disturbance description"
      )),
    createsOutput(
      objectName = "historicDMtype", objectClass = "numeric",
      desc = paste(
        "Historical disturbance type for each pixel group.",
        "Examples: 1 = wildfire; 2 = clearcut.",
        "Required input to CBM_core.")),
    createsOutput(
      objectName = "lastPassDMtype", objectClass = "numeric",
      desc = paste(
        "Last pass disturbance type for each pixel group.",
        "Examples: 1 = wildfire; 2 = clearcut.",
        "Required input to CBM_core."))
  )
))

doEvent.CBM_dataPrep_SK <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  ## Create sim$allPixDT and sim$spatialDT ----

  # Set which pixel group columns are assigned from which spatial inputs
  pgCols <- c(
    ages            = "ageRaster",
    gcids           = "gcIndexRaster",
    ecozones        = "ecoLocator",
    spatial_unit_id = "spuLocator"
  )

  # Read spatial inputs
  inRast <- list()
  for (rName in c("masterRaster", pgCols)){
    inRast[[rName]] <- sim[[rName]]
    if (is.null(inRast[[rName]])) stop(shQuote(rName), " input not found")
  }

  ## Convert masterRaster to SpatRaster
  for (rName in "masterRaster"){
    if (!inherits(inRast[[rName]], "SpatRaster")){
      inRast[[rName]] <- tryCatch(
        terra::rast(inRast[[rName]]),
        error = function(e) stop(
          shQuote(rName), " could not be converted to SpatRaster: ", e$message,
          call. = FALSE))
    }
  }

  ## Convert spatial inputs to SpatRaster and align with masterRaster
  for (rName in pgCols){

    if (inherits(inRast[[rName]], "sf")){

      inRast[[rName]] <- terra::rasterize(
        postProcess(
          inRast[[rName]],
          cropTo    = inRast$masterRaster,
          projectTo = inRast$masterRaster
        ) |> Cache(),
        inRast$masterRaster,
        field = names(inRast[[rName]])[[1]]
      )

    }else{

      inRast[[rName]] <- postProcess(
        inRast[[rName]],
        to     = inRast$masterRaster,
        method = "near"
      ) |> Cache()
    }
  }

  # Create sim$allPixDT: Summarize input values into table
  sim$allPixDT <- data.table::data.table(
    pixelIndex = 1:terra::ncell(inRast$masterRaster)
  )
  for (i in 1:length(pgCols)){
    sim$allPixDT[[names(pgCols)[[i]]]] <- terra::values(inRast[[pgCols[[i]]]])[,1]
  }
  setkeyv(sim$allPixDT, "pixelIndex")

  # Create sim$spatialDT: Summarize input raster values where masterRaster is not NA
  spatialDT <- sim$allPixDT[!is.na(terra::values(inRast$masterRaster)[,1]),]

  spatialDT_isNA <- is.na(spatialDT)
  if (any(spatialDT_isNA)){
    for (i in 1:length(pgCols)){
      if (any(spatialDT_isNA[, names(pgCols)[[i]]])) warning(
        "Pixels have been excluded from the simulation where there are no values in",
        shQuote(pgCols[[i]]))
    }
    spatialDT <- spatialDT[!apply(spatialDT_isNA, 1, any),]
  }

  # Create pixel groups: groups of pixels with the same attributes
  spatialDT$pixelGroup <- LandR::generatePixelGroups(
    spatialDT, maxPixelGroup = 0, columns = names(pgCols)
  )

  # Keep only essential columns
  sim$spatialDT <- spatialDT[, c("pixelIndex", "pixelGroup", names(pgCols)), with = FALSE]


  ## Create sim$level3DT, sim$realAges, and sim$curveID ----

  level3DT <- unique(sim$spatialDT[, -("pixelIndex")])
  setkeyv(level3DT, "pixelGroup")

  # Create sim$curveID
  sim$curveID <- c("gcids") #, "ecozones" # "id_ecozone"
  ##TODO add to metadata -- use in multiple modules

  # Set sim$level3DT$gcids to be a factor
  set(level3DT, j = "gcids",
      value = factor(CBMutils::gcidsCreate(level3DT[, sim$curveID, with = FALSE])))

  # Create 'realAges' output object and set ages to be >= 3
  ## Temporary fix to CBM_core issue: https://github.com/PredictiveEcology/CBM_core/issues/1
  sim$realAges <- level3DT[, ages]
  level3DT[ages <= 3, ages := 3]
  setorderv(level3DT, "pixelGroup")

  # Join with spinup parameters
  setkeyv(level3DT, "spatial_unit_id")
  spinupParameters <- as.data.table(sim$spinupSQL[, c(1, 7)])

  setkeyv(spinupParameters,"id")
  spinupParameters <- setNames(spinupParameters, replace(names(spinupParameters), names(spinupParameters) == 'id', 'spatial_unit_id'))
  retInt <- merge.data.table(level3DT, spinupParameters,
                             by = "spatial_unit_id", all.x = TRUE)
  setkeyv(retInt, "pixelGroup")
  setkeyv(level3DT, "pixelGroup")
  sim$level3DT <- retInt


  ## Create sim$ecozones and sim$spatialUnits ----

  # create sim$ecozones and sim$spatialUnits to subset vol2biomass growth curves
  sim$ecozones <- sim$level3DT$ecozones
  sim$spatialUnits <- sim$level3DT$spatial_unit_id


  ## Create sim$speciesPixelGroup ----

  gcMeta <- sim$gcMeta
  if (!inherits(gcMeta, "data.table")){
    gcMeta <- tryCatch(
      data.table::as.data.table(gcMeta),
      error = function(e) stop(
        "'gcMeta' could not be converted to data.table: ", e$message, call. = FALSE))
  }

  speciesPixelGroup <- gcMeta[sim$species_tr, on = .(species = name)]
  speciesPixelGroup <- speciesPixelGroup[gcids >= 1,]
  speciesPixelGroup <- speciesPixelGroup[,.(gcids, species_id)]
  speciesPixelGroup <- speciesPixelGroup[sim$spatialDT, on = .(gcids=gcids)]
  speciesPixelGroup <- unique(speciesPixelGroup[,.(pixelGroup, species_id)])
  sim$speciesPixelGroup <- speciesPixelGroup


  ## Create sim$mySpuDmids, sim$historicDMtype, and sim$lastPassDMtype ----

  # List disturbances possible within in each spatial unit
  spuIDs <- sort(unique(sim$level3DT$spatial_unit_id))
  listDist <- CBMutils::spuDist(spuIDs, sim$dbPath)

  if (!suppliedElsewhere("mySpuDmids", sim)){

    # Read user disturbances
    userDist <- sim$userDist

    if (!inherits(userDist, "data.table")){
      userDist <- tryCatch(
        data.table::as.data.table(userDist),
        error = function(e) stop(
          "'userDist' could not be converted to data.table: ", e$message, call. = FALSE))
    }

    # Match user disturbances with CBM-CFS3 disturbance matrices
    userDistSpu <- do.call(rbind, lapply(spuIDs, function(spuID){
      cbind(spatial_unit_id = spuID, userDist)
    }))

    askUser <- interactive() & !identical(Sys.getenv("TESTTHAT"), "true")
    if (askUser) message(
      "Prompting user to match input disturbances with CBM-CFS3 disturbance matrix IDs:")

    userDistMatch <- CBMutils::spuDistMatch(
      userDistSpu, listDist = listDist,
      ask = askUser
    ) |> Cache()

    sim$mySpuDmids <- cbind(
      userDistSpu[, setdiff(names(userDist), names(userDistMatch)), with = FALSE],
      userDistMatch)
  }

  # Set sim$historicDMtype to be wildfire
  sim$historicDMtype <- data.table::merge.data.table(
    sim$level3DT,
    subset(listDist, tolower(name) == "wildfire"),
    by = "spatial_unit_id"
  )$disturbance_type_id

  # Set sim$lastPassDMtype to be wildfire
  ## TODO: this is where it could be something else then fire
  sim$lastPassDMtype <- sim$historicDMtype


  ## Return simList ----

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  ## Data table inputs ----

  # 1. Growth and yield
  ## TODO add a data manipulation to adjust if the m3 are not given on a yearly basis.
  if (!suppliedElsewhere("userGcM3", sim)){

    if (suppliedElsewhere("userGcM3URL", sim) &
        !identical(sim$userGcM3URL, extractURL("userGcM3"))){

      sim$userGcM3 <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$userGcM3URL,
        fun = data.table::fread
      )

    }else{

      if (!suppliedElsewhere("userGcM3URL", sim, where = "user")) message(
        "User has not supplied growth curves ('userGcM3' or 'userGcM3URL'). ",
        "Default for Saskatchewan will be used.")

      sim$userGcM3 <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("userGcM3"),
        targetFile = "userGcM3.csv",
        fun        = data.table::fread
      )
      names(sim$userGcM3) <- c("gcids", "Age", "MerchVolume")
    }
  }

  # 2. Disturbance information
  if (!suppliedElsewhere("userDist", sim) & !suppliedElsewhere("mySpuDmids", sim)  &
      suppliedElsewhere("userDistURL", sim)){

    sim$userDist <- prepInputs(
      destinationPath = inputPath(sim),
      url = sim$userDistURL,
      fun = data.table::fread
    )
  }

  # 3. CBM admin
  if (!suppliedElsewhere("cbmAdmin", sim)){

    if (suppliedElsewhere("cbmAdminURL", sim) &
        !identical(sim$cbmAdminURL, extractURL("cbmAdmin"))){

      sim$cbmAdmin <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$cbmAdminURL,
        fun = data.table::fread
      )

    }else{

      if (!suppliedElsewhere("cbmAdminURL", sim, where = "user")) message(
        "User has not supplied CBM admin ('cbmAdmin' or 'cbmAdminURL'). ",
        "Default for Canada will be used.")

      sim$cbmAdmin <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("cbmAdmin"),
        targetFile = "cbmAdmin.csv",
        fun        = data.table::fread
      )
    }
  }


  ## Spatial inputs ----

  # 1. Master raster
  if (!suppliedElsewhere("masterRaster", sim)){

    if (suppliedElsewhere("masterRasterURL", sim) &
        !identical(sim$masterRasterURL, extractURL("masterRaster"))){

      sim$masterRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$masterRasterURL
      )

    }else{

      if (!suppliedElsewhere("masterRasterURL", sim, where = "user")) message(
        "User has not supplied a master raster ('masterRaster' or 'masterRasterURL'). ",
        "Default for Saskatchewan will be used.")

      masterRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("masterRaster"),
        targetFile = "ldSp_TestArea.tif",
        fun        = terra::rast
      )

      sim$masterRaster <- terra::classify(
        masterRaster, cbind(0, NA)
      ) |> Cache()
    }
  }

  # 2. Age raster
  if (!suppliedElsewhere("ageRaster", sim)){

    if (suppliedElsewhere("ageRasterURL", sim) &
        !identical(sim$ageRasterURL, extractURL("ageRaster"))){

      sim$ageRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$ageRasterURL
      )

    }else{

      if (!suppliedElsewhere("ageRasterURL", sim, where = "user")) message(
        "User has not supplied an age raster ('ageRaster' or 'ageRasterURL'). ",
        "Default for Saskatchewan will be used.")

      sim$ageRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("ageRaster"),
        targetFile = "age_TestArea.tif",
        fun        = terra::rast
      ) |> Cache()
    }
  }

  # 3. Growth curves
  if (!suppliedElsewhere("gcIndexRaster", sim)){

    if (suppliedElsewhere("gcIndexRasterURL", sim) &
        !identical(sim$gcIndexRasterURL, extractURL("gcIndexRaster"))){

      sim$gcIndexRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$gcIndexRasterURL
      )

    }else{

      if (!suppliedElsewhere("gcIndexRasterURL", sim, where = "user")) message(
        "User has not supplied a growth curve raster ('gcIndexRaster' or 'gcIndexRasterURL'). ",
        "Default for Saskatchewan will be used.")

      sim$gcIndexRaster <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("gcIndexRaster"),
        targetFile = "gcIndex.tif",
        fun        = terra::rast
      ) |> Cache()
    }
  }

  # 6. Disturbance rasters
  if (!suppliedElsewhere("disturbanceRasters", sim)){

    if (suppliedElsewhere("disturbanceRastersURL", sim) &
        !identical(sim$disturbanceRastersURL, extractURL("disturbanceRasters"))){

      drPaths <- preProcess(
        destinationPath = inputPath(sim),
        url = sim$disturbanceRastersURL,
        fun = NA
      )$targetFilePath

      # If extracted archive: list all files in directory
      if (dirname(drPaths) != inputPath(sim)){
        drPaths <- list.files(dirname(drPaths), full.names = TRUE)
      }

      # List files by year
      drInfo <- data.frame(
        path = drPaths,
        name = tools::file_path_sans_ext(basename(drPaths)),
        ext  = tolower(tools::file_ext(drPaths))
      )
      drInfo$year_regexpr <- regexpr("[0-9]{4}", drInfo$name)
      drInfo$year <- sapply(1:nrow(drInfo), function(i){
        if (drInfo[i,]$year_regexpr != -1){
          paste(
            strsplit(drInfo[i,]$name, "")[[1]][0:3 + drInfo[i,]$year_regexpr],
            collapse = "")
        }else NA
      })

      if (all(is.na(drInfo$year))) stop(
        "Disturbance raster(s) from 'disturbanceRasterURL' must be named with 4-digit years")
      drInfo <- drInfo[!is.na(drInfo$year),, drop = FALSE]

      # Choose file type to return for each year
      drYears <- unique(sort(drInfo$year))
      sim$disturbanceRasters <- sapply(setNames(drYears, drYears), function(drYear){
        drInfoYear <- subset(drInfo, year == drYear)
        if (nrow(drInfoYear) > 1){
          if ("grd" %in% drInfoYear$ext) return(subset(drInfoYear, ext == "grd")$path)
          if ("tif" %in% drInfoYear$ext) return(subset(drInfoYear, ext == "grd")$path)
          drInfoYear$size <- file.size(drInfoYear$path)
          drInfoYear$path[drInfoYear$size == max(drInfoYear$size)][[1]]
        }else drInfoYear$path
      })

    }else{

      if (!suppliedElsewhere("disturbanceRastersURL", sim, where = "user")) message(
        "User has not supplied disturbance rasters ('disturbanceRasters'). ",
        "Default for Saskatchewan will be used.")

      # Set years where disturbance rasters are available
      distYears <- 1985:2011

      preProcess(
        destinationPath = inputPath(sim),
        url         = extractURL("disturbanceRasters"),
        filename1   = "disturbance_testArea.zip",
        targetFile  = "ReadMe.txt",
        fun         = function() NULL,
        alsoExtract = do.call(c, lapply(distYears, function(simYear){
          paste0("disturbance_testArea/SaskDist_", simYear, c(".grd", ".gri", ".tif"))
        })))

      sim$disturbanceRasters <- setNames(
        file.path(inputPath(sim), "disturbance_testArea", paste0("SaskDist_", distYears, ".grd")),
        distYears)

      # Disturbance information
      if (!suppliedElsewhere("userDist", sim) & !suppliedElsewhere("userDistURL", sim) &
          !suppliedElsewhere("mySpuDmids", sim)){

        sim$userDist <- prepInputs(
          destinationPath = inputPath(sim),
          url        = extractURL("userDist"),
          targetFile = "userDist.csv",
          fun        = data.table::fread
        )
      }
    }
  }


  ## Return simList ----

  return(invisible(sim))

}
