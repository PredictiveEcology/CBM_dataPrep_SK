
defineModule(sim, list(
  name = "CBM_dataPrep_SK",
  description = paste(
    "A data preparation module to format and prepare user-provided input to the SpaDES forest-carbon modelling family",
    "in the Saskatchewan study area."),
  keywords = NA,
  authors = c(
    person("Céline",  "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Alex M",  "Chubaty",   email = "achubaty@for-cast.ca",               role = c("ctb")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.2", CBM_dataPrep_SK = "2.0.0"),
  timeunit = "year",
  timeframe = as.POSIXlt(c(NA, NA)),
  citation = list("citation.bib"),
  documentation = list("CBM_dataPrep_SK.Rmd"),
  reqdPkgs = list(
    "reproducible (>=2.1.2)", "data.table", "terra"
  ),
  parameters = rbind(
    defineParameter(".useCache", "logical", "TRUE", NA, NA, "Cache module events")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "masterRaster", objectClass = "SpatRaster",
      desc = "Raster template defining the study area. Default is a test area in the managed forests of SK.",
      sourceURL = "https://drive.google.com/file/d/1zUyFH8k6Ef4c_GiWMInKbwAl6m6gvLJW"),
    expectsInput(
      objectName = "ageLocator", objectClass = "sf|SpatRaster",
      desc = "Spatial data source of stand ages. Default is the 2012 CASFRI inventory.",
      sourceURL = "https://drive.google.com/file/d/1hylk0D1vO19Dpg4zFtnSNhnyYP4j-bEA"),
    expectsInput(
      objectName = "ageDataYear", objectClass = "numeric",
      desc = "Year that the ages in `ageLocator` represent."),
    expectsInput(
      objectName = "ageSpinupMin", objectClass = "numeric",
      desc = "Minimum age for cohorts during spinup."),
    expectsInput(
      objectName = "gcIndexLocator", objectClass = "sf|SpatRaster",
      desc = "Spatial data source of growth curve index locations.", #TODO: Define default data source
      sourceURL = "https://drive.google.com/file/d/1yunkaYCV2LIdqej45C4F9ir5j1An0KKr"),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.frame",
      desc = "Growth curve metadata.", #TODO: Define default data source
      sourceURL = "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = "Growth curve volumes.", #TODO: Define default data source
      sourceURL = "https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m"),
    expectsInput(
      objectName = "disturbanceRasters", objectClass = "character",
      sourceURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",
      desc = paste(
        "The sourceURL is the Wulder and White disturbance rasters for SK covering 1984-2011.",
        "If this URL is provided by the user,",
        "the disturbances will be processed into a list of `disturbanceRasters` for CBM_dataPrep")),
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      sourceURL = "https://drive.google.com/file/d/1n4fXwUkX5GPyWJgr0QQx65roAIaxmcWJ",
      desc = paste(
        "If the Wulder and White disturbance rasters are used,",
        "the metadata table describing their events is provided."))
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "masterRaster", objectClass = "SpatRaster",
      desc = "Default `masterRaster` if not provided elsewhere by user."),
    createsOutput(
      objectName = "ageLocator", objectClass = "SpatRaster",
      desc = "Default `ageLocator` if not provided elsewhere by user."),
    createsOutput(
      objectName = "ageDataYear", objectClass = "integer",
      desc = "Data year of default `ageLocator` if not provided elsewhere by user."),
    createsOutput(
      objectName = "ageSpinupMin", objectClass = "integer",
      desc = "Default minimum age for cohorts during spinup is set to 3 if `ageLocator` not provided elsewhere by user."),
    createsOutput(
      objectName = "cohortLocators", objectClass = "SpatRaster",
      desc = "Contains the default `gcIndexLocator` if not provided elsewhere by user."),
    createsOutput(
      objectName = "gcKey", objectClass = "character",
      desc = "Default `gcMeta` and `userGcM3` growth curve ID if not provided elsewhere by user."),
    createsOutput(
      objectName = "gcMeta", objectClass = "data.table",
      desc = "Default `gcMeta` if not provided elsewhere by user."),
    createsOutput(
      objectName = "userGcM3", objectClass = "data.table",
      desc = "Default `userGcM3` if not provided elsewhere by user."),
    createsOutput(
      objectName = "disturbanceEvents", objectClass = "list",
      desc = "The Wulder and White disturbance raster events if they are used."),
    createsOutput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      desc = "The Wulder and White disturbance raster metadata if they are used.")
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

Init <- function(sim){

  # Add `gcIndexLocator` to the `cohortLocators` list
  if (!is.null(sim$gcIndexLocator)){
    sim$cohortLocators <- c(sim$cohortLocators, list(gcID = sim$gcIndexLocator))
  }

  # Read Wulder and White disturbances rasters
  if (identical(sim$disturbanceRasters, extractURL("disturbanceRasters"))){

    # Download archive of disturbance rasters
    archiveDir <- prepInputs(
      destinationPath = inputPath(sim),
      url         = extractURL("disturbanceRasters"),
      archive     = "disturbance_testArea.zip",
      targetFile  = "disturbance_testArea",
      alsoExtract = do.call(c, lapply(1985:2011, function(simYear){
        paste0("disturbance_testArea/SaskDist_", simYear, c(".grd", ".gri", ".tif"))
      })),
      fun = NA)

    # Prepare files by year
    grdFiles <- list.files(archiveDir, pattern = "\\.grd$", recursive = TRUE, full.names = TRUE)
    grdYears <- sapply(strsplit(tools::file_path_sans_ext(basename(grdFiles)), "_"), `[[`, 2)
    names(grdFiles) <- grdYears

    # Summarize events into a table
    simYears <- intersect(names(grdFiles), start(sim):end(sim))

    templateRast <- sim$masterRaster
    terra::values(templateRast) <- 1

    sim$disturbanceEvents <- do.call(rbind, c(
      list(sim$disturbanceEvents), lapply(simYears, function(simYear){

        grdRast <- postProcess(
          terra::rast(grdFiles[[simYear]]),
          to     = templateRast,
          method = "mode"
        ) |> Cache()

        yearEvents <- data.table::data.table(
          pixelIndex = as.integer(1:terra::ncell(grdRast)),
          year       = as.integer(simYear),
          eventID    = as.integer(terra::values(grdRast)[,1])
        )
        subset(yearEvents, eventID > 0 %in% TRUE)
    })))

    sim$disturbanceRasters <- NULL
  }

  return(sim)
}

.inputObjects <- function(sim) {

  # Master raster
  if (!suppliedElsewhere("masterRaster", sim) & !suppliedElsewhere("masterRasterURL", sim)){

    message("User has not supplied a master raster ('masterRaster' or 'masterRasterURL'). ",
            "Default for Saskatchewan will be used.")

    sim$masterRaster <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("masterRaster"),
      targetFile = "ldSp_TestArea.tif",
      fun        = terra::rast
    )

    sim$masterRaster <- terra::classify(
      sim$masterRaster, cbind(0, NA)
    )
  }

  # Stand ages
  if (!suppliedElsewhere("ageLocator", sim) & !suppliedElsewhere("ageLocatorURL", sim)){

    message("User has not supplied stand age locations ('ageLocator' or 'ageRasterURL'). ",
            "Default for Saskatchewan will be used.")

    sim$ageLocator <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("ageLocator"),
      targetFile = "age_TestArea.tif",
      fun        = terra::rast
    )

    sim$ageDataYear <- 2012
    sim$ageSpinupMin <- 3
  }

  # Growth curves
  if (!suppliedElsewhere("gcIndexLocator", sim) & !suppliedElsewhere("gcIndexLocatorURL", sim)){

    # Growth curve locations
    message("User has not supplied growth curve locations ('gcIndexLocator' or 'gcIndexLocatorURL'). ",
            "Default for Saskatchewan will be used.")

    sim$gcIndexLocator <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("gcIndexLocator"),
      targetFile = "gcIndex.tif",
      fun        = terra::rast
    )
  }

  # Growth curve volumes
  if (!suppliedElsewhere("userGcM3", sim) & !suppliedElsewhere("userGcM3URL", sim)){

    message("User has not supplied growth curve volumes ('userGcM3' or 'userGcM3URL'). ",
            "Default for Saskatchewan will be used.")

    sim$gcKey <- "gcID"

    sim$userGcM3 <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("userGcM3"),
      targetFile = "userGcM3.csv",
      fun        = data.table::fread
    )
    data.table::setnames(sim$userGcM3, names(sim$userGcM3), c("gcID", "Age", "MerchVolume"))
    data.table::setkeyv(sim$userGcM3, c("gcID", "Age"))

    # Growth curve metadata
    if (!suppliedElsewhere("gcMeta", sim) & !suppliedElsewhere("gcMetaURL", sim)){

      message("User has not supplied growth curve metadata ('gcMeta' or 'gcMetaURL'). ",
              "Default for Saskatchewan will be used.")

      sim$gcMeta <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("gcMeta"),
        targetFile = "gcMetaEg.csv",
        fun        = data.table::fread
      )
      data.table::setnames(sim$gcMeta, "gcids", "gcID")
      data.table::setkey(sim$gcMeta, gcID)
    }
  }

  # Disturbance metadata
  if (identical(sim$disturbanceRasters, extractURL("disturbanceRasters")) &
      !suppliedElsewhere("disturbanceMeta", sim) & !suppliedElsewhere("disturbanceMetaURL", sim)){

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

  # Return simList
  return(invisible(sim))

}


