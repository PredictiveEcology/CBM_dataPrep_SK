defineModule(sim, list(
  name = "CBM_dataPrep_SK",
  description = "A data preparation module to format and prepare user-provided input to the SpaDES forest-carbon modelling family.",
  keywords = NA,
  authors = c(
    person("Céline",  "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Alex M",  "Chubaty",   email = "achubaty@for-cast.ca",               role = c("ctb")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.2", CBM_dataPrep_SK = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("CBM_dataPrep_SK.Rmd"),
  reqdPkgs = list(
    "data.table", "sf", "terra",
    "reproducible (>=2.1.2)" ,
    "PredictiveEcology/CBMutils@development (>=2.0.3)",
    "PredictiveEcology/LandR@development"
  ),
  parameters = rbind(
    defineParameter(".useCache", "character", c(".inputObjects", "Init"), NA, NA, "Cache module events")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "userGcMeta", objectClass = "data.frame",
      sourceURL = "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ",
      desc = "Growth curve metadata"),
    expectsInput(
      objectName = "userGcMetaURL", objectClass = "character",
      desc = "URL for userGcMeta"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = "Growth curve volumes by age",
      sourceURL = "https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m"),
    expectsInput(
      objectName = "userGcM3URL", objectClass = "character",
      desc = "URL for userGcM3"),
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
      desc = "URL for gcIndexRaster"),
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
      objectName = "disturbanceRasters", objectClass = "list",
      sourceURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",
      desc = paste(
        "One or more sets of rasters containing locations of disturbance events for each year.",
        "If the list is named with disturbance event IDs, all non-NA cells will be considered events.",
        "If the list is length 1 and unnamed, the disturbance rasters must have pixel values matching event IDs.",
        "Each set of disturbance rasters must be a list or SpatRaster stack named with 4 digit years",
        "such that a single raster layer can be accessed for each disturbance year",
        "(e.g.  `disturbanceRasters[[\"1\"]][[\"2025\"]]`).",
        "The default rasters are the Wulder and White disturbance rasters for SK covering 1984-2011."
      )),
    expectsInput(
      objectName = "disturbanceRastersURL", objectClass = "character",
      desc = paste(
        "One or more URL for disturbanceRasters.",
        "If the vector is named, it must be named with the disturbance event IDs the raster includes events for.",
        "If the vector is not named, the raster values must be event IDs.")),
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      sourceURL = "https://drive.google.com/file/d/1n4fXwUkX5GPyWJgr0QQx65roAIaxmcWJ",
      desc = paste(
        "Table defining the disturbance event types.",
        "This associates CBM-CFS3 disturbances with the event IDs in the 'disturbanceEvents' table."),
      columns = c(
        eventID             = "Event type ID",
        disturbance_type_id = "Optional. CBM-CFS3 disturbance type ID. If not provided, the user will be prompted to choose IDs.",
        name                = "Optional. Disturbance name (e.g. 'Wildfire'). Required if 'disturbance_type_id' absent."
      )),
    expectsInput(
      objectName = "disturbanceMetaURL", objectClass = "character",
      desc = "URL for disturbanceMeta"),
    expectsInput(
      objectName = "dbPath", objectClass = "character",
      sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db",
      desc = paste(
        "Path to the CBM-CBM3 defaults database. ",
        "Required if disturbanceMeta is missing the 'disturbance_type_id' column"))
  ),

  outputObjects = bindrows(
    createsOutput(
      objectName = "allPixDT", objectClass = "data.table",
      desc = "Table summarizing raster input data with 1 row for every 'masterRaster' pixel (including NAs)",
      columns = c(
        pixelIndex      = "'masterRaster' cell index",
        ages            = "Cohort ages extracted from input 'ageRaster'",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'",
        gcids           = "Growth curve IDs extracted from input 'gcIndexRaster'",
        ecozones        = "Ecozone IDs extracted from input 'ecoRaster'"
      )),
    createsOutput(
      objectName = "standDT", objectClass = "data.table",
      desc = paste(
        "Table summarizing raster input data with 1 row for every 'masterRaster' pixel that is not NA",
        "Required input to CBM_core."),
      columns = c(
        pixelIndex      = "'masterRaster' cell index",
        area            = "Stand area in meters",
        spatial_unit_id = "Spatial unit IDs extracted from input 'spuLocator'"
      )),
    createsOutput(
      objectName = "cohortDT", objectClass = "data.table",
      desc = paste(
        "Table summarizing raster input data with 1 row for every 'masterRaster' pixel that is not NA",
        "Required input to CBM_core."),
      columns = c(
        cohortID        = "Cohort ID",
        pixelIndex      = "'masterRaster' cell index",
        ages            = "Cohort ages extracted from input 'ageRaster'",
        ageSpinup       = "Cohort ages raised to minimum of age 3 to use in the spinup",
        gcID            = "Growth curve IDs extracted from input 'gcIndexRaster'",
        gcids           = "Growth curve ID unique to every spatial unit"
      )),
    createsOutput(
      objectName = "userGcSPU", objectClass = "data.table",
      desc = "Table of growth curves and spatial unit combinations in the cohorts.",
      columns = list(
        gcID            = "Growth curve ID",
        spatial_unit_id = "CBM-CFS3 spatial unit ID"
      )),
    createsOutput(
      objectName = "curveID", objectClass = "character",
      desc = paste(
        "Column names in 'cohortDT' that uniquely define each pixel group growth curve ID.",
        "Required input to CBM_vol2biomass")),
    createsOutput(
      objectName = "userGcMeta", objectClass = "data.frame",
      desc = "Growth curve metadata"),
    createsOutput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = "Growth curve volumes by age"),
    createsOutput(
      objectName = "disturbanceEvents", objectClass = "data.table",
      desc = paste(
        "Table with disturbance events for each simulation year.",
        "The inputs 'disturbanceRasters' are aligned with the 'masterRaster'",
        "and the events are summarized into this table.",
        "Required input to CBM_core.")),
    createsOutput(
      objectName = "disturbanceMeta", objectClass = "data.frame",
      desc = "Table defining the disturbance event types. Required input to CBM_core.")
  )
))

doEvent.CBM_dataPrep_SK <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,

    init = {

      sim <- Init(sim)

      # Read annual disturbances
      sim <- scheduleEvent(sim, start(sim), "CBM_dataPrep_SK", "readDisturbanceEvents")
    },

    readDisturbanceEvents = {

      if (!is.null(sim$disturbanceRasters)){

        # Align disturbances with masterRaster and summarize in table
        newEvents <-  mapply(
          CBMutils::dataPrep_disturbanceRasters,
          disturbanceRasters = sim$disturbanceRasters,
          eventID  = lapply(1:length(sim$disturbanceRasters), function(i) names(sim$disturbanceRasters)[i]),
          MoreArgs = list(
            templateRast = sim$masterRaster,
            year         = time(sim)
          ),
          SIMPLIFY = FALSE) |> Cache()

        sim$disturbanceEvents <- do.call(rbind, c(
          if (!is.null(sim$disturbanceEvents)) list(sim$disturbanceEvents),
          newEvents
        ))
      }

      # Schedule for next year
      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_dataPrep_SK", "readDisturbanceEvents")
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  ## Create sim$standDT and sim$cohortDT ----

  # Set sim$curveID
  sim$curveID <- "gcID"

  # Set which pixel group columns are assigned from which spatial inputs
  pgCols <- c(
    ages            = "ageRaster",
    gcID            = "gcIndexRaster",
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
  allPixDT <- data.table::data.table(
    pixelIndex = 1:terra::ncell(inRast$masterRaster),
    area       = terra::values(terra::cellSize(inRast$masterRaster, unit = "m", mask = TRUE, transform = FALSE))[,1]
  )
  for (i in 1:length(pgCols)){
    allPixDT[[names(pgCols)[[i]]]] <- terra::values(inRast[[pgCols[[i]]]])[,1]
  }
  data.table::setkey(allPixDT, pixelIndex)

  # Save allPixDT
  sim$allPixDT <- allPixDT

  # Create sim$spatialDT: Summarize input raster values where masterRaster is not NA
  spatialDT <- sim$allPixDT[!is.na(terra::values(inRast$masterRaster)[,1]),]

  # For CBM_vol2biomass
  # Define unique growth curves with spatial_unit_id
  spatialDT$gcids <- factor(
    CBMutils::gcidsCreate(spatialDT[, .SD, .SDcols = c("spatial_unit_id", sim$curveID)])
  )
  sim$userGcSPU <- unique(spatialDT[, .SD, .SDcols = c("spatial_unit_id", sim$curveID)])

  # For CBM_core
  sim$standDT <- spatialDT[, .SD, .SDcols = c("pixelIndex", "area", "spatial_unit_id")]
  data.table::setkey(sim$standDT, pixelIndex)

  sim$cohortDT <- cbind(cohortID = spatialDT$pixelIndex,
                        spatialDT[, .SD, .SDcols = c("pixelIndex", "ages", "gcids", sim$curveID)])
  data.table::setkey(sim$cohortDT, cohortID)

  # Alter ages for the spinup
  ## Temporary fix to CBM_core issue: https://github.com/PredictiveEcology/CBM_core/issues/1
  sim$cohortDT[, ageSpinup := ages]
  sim$cohortDT[ageSpinup < 3, ageSpinup := 3]

  rm(spatialDT)


  ## Prepare sim$userGcMeta ----

  if (any(!c("species_id", "sw_hw", "canfi_species", "genus") %in% names(sim$userGcMeta))){

    if (!"species" %in% names(sim$userGcMeta)) stop(
      "userGcMeta requires the 'species' column to retrieve species data with CBMutils::sppMatch")

    if (!inherits(sim$userGcMeta, "data.table")){
      sim$userGcMeta <- tryCatch(
        data.table::as.data.table(sim$userGcMeta),
        error = function(e) stop(
          "userGcMeta could not be converted to data.table: ", e$message, call. = FALSE))
    }

    sppMatchTable <- CBMutils::sppMatch(
      sim$userGcMeta$species,
      return     = c("CBM_speciesID", "Broadleaf", "CanfiCode", "NFI"),
      otherNames = list(
        "White birch" = "Paper birch"
      ))[, .(
        species_id    = CBM_speciesID,
        sw_hw         = data.table::fifelse(Broadleaf, "hw", "sw"),
        canfi_species = CanfiCode,
        genus         = sapply(strsplit(NFI, "_"), `[[`, 1)
      )]

    sim$userGcMeta <- cbind(
      sim$userGcMeta[, .SD, .SDcols = setdiff(names(sim$userGcMeta), names(sppMatchTable))],
      sppMatchTable)
    rm(sppMatchTable)
  }


  ## Prepare sim$disturbanceMeta ----

  if (!is.null(sim$disturbanceMeta) && !"disturbance_type_id" %in% names(sim$disturbanceMeta)){

    if (is.null(sim$dbPath)) stop("'dbPath' input required to set disturbanceMeta 'disturbance_type_id'")

    if (!inherits(sim$disturbanceMeta, "data.table")){
      sim$disturbanceMeta <- tryCatch(
        data.table::as.data.table(sim$disturbanceMeta),
        error = function(e) stop(
          "'disturbanceMeta' could not be converted to data.table: ", e$message, call. = FALSE))
    }

    # Match user disturbances with CBM-CFS3 disturbance type IDs
    askUser <- interactive() & !identical(Sys.getenv("TESTTHAT"), "true")
    if (askUser) message(
      "Prompting user to match input disturbances with CBM-CFS3 disturbances:")

    data.table::setnames(
      sim$disturbanceMeta, c("name", "description"), c("nameUser", "descUser"),
      skip_absent = TRUE)

    sim$disturbanceMeta <- cbind(
      sim$disturbanceMeta, CBMutils::distMatch(
        sim$disturbanceMeta$nameUser,
        dbPath = sim$dbPath,
        ask    = askUser
      ) |> Cache()
    )
  }


  ## Return simList ----

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  ## Read inputs ----

  # Growth and yield metadata
  if (!suppliedElsewhere("userGcMeta", sim)){

    if (suppliedElsewhere("userGcMetaURL", sim) &
        !identical(sim$userGcMetaURL, extractURL("userGcMeta"))){

      sim$userGcMeta <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$userGcMetaURL,
        fun = data.table::fread
      )

    }else{

      if (!suppliedElsewhere("userGcMetaURL", sim, where = "user")) message(
        "User has not supplied growth curve metadata ('userGcMeta' or 'userGcMetaURL'). ",
        "Default for Saskatchewan will be used.")

      sim$userGcMeta <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("userGcMeta"),
        targetFile = "gcMetaEg.csv",
        fun        = data.table::fread
      )
      data.table::setnames(sim$userGcMeta, "gcids", "gcID")
      data.table::setkey(sim$userGcMeta, gcID)
    }
  }

  # Growth and yield table
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
      data.table::setnames(sim$userGcM3, names(sim$userGcM3), c("gcID", "Age", "MerchVolume"))
      data.table::setkeyv(sim$userGcM3, c("gcID", "Age"))
    }
  }

  # Master raster
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

  # Stand ages
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

  # Growth curves
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

  # Disturbances
  if (suppliedElsewhere("disturbanceMetaURL", sim) & !suppliedElsewhere("disturbanceMeta", sim)){

    sim$disturbanceMeta <- prepInputs(
      destinationPath = inputPath(sim),
      url = sim$disturbanceMetaURL,
      fun = data.table::fread
    )
  }

  if (!suppliedElsewhere("disturbanceRasters", sim) & !suppliedElsewhere("disturbanceEvents", sim)){

    if (suppliedElsewhere("disturbanceRastersURL", sim) &
        !identical(sim$disturbanceRastersURL, extractURL("disturbanceRasters"))){

      sim$disturbanceRasters <- lapply(
        sim$disturbanceRastersURL,
        CBMutils::dataPrep_disturbanceRastersURL,
        destinationPath = inputPath(sim)
      )

    }else{

      if (!suppliedElsewhere("disturbanceRastersURL", sim, where = "user")) message(
        "User has not supplied disturbance rasters ('disturbanceRasters' or 'disturbanceRastersURL'). ",
        "Default for Saskatchewan will be used.")

      sim$disturbanceRasters <- list(
        CBMutils::dataPrep_disturbanceRastersURL(
          destinationPath       = inputPath(sim),
          disturbanceRastersURL = extractURL("disturbanceRasters"),
          archive               = "disturbance_testArea.zip",
          targetFile            = "disturbance_testArea",
          alsoExtract           = do.call(c, lapply(1985:2011, function(simYear){
            paste0("disturbance_testArea/SaskDist_", simYear, c(".grd", ".gri", ".tif"))
          })))
      )

      # Disturbance information
      if (!suppliedElsewhere("disturbanceMeta", sim) & !suppliedElsewhere("disturbanceMetaURL", sim)){

        sim$disturbanceMeta <- prepInputs(
          destinationPath = inputPath(sim),
          url        = extractURL("disturbanceMeta"),
          targetFile = "SK_disturbances.csv",
          fun        = data.table::fread
        )
        sim$disturbanceMeta <- unique(
          sim$disturbanceMeta[, .(
            eventID = rasterID, disturbance_type_id, wholeStand,
            name = distName, description)])
      }
    }
  }


  ## Return simList ----

  return(invisible(sim))

}
