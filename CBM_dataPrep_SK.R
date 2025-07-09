
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
  version = list(SpaDES.core = "1.0.2", CBM_dataPrep_SK = "2.0.0", CBM_dataPrep = "1.0.0"),
  loadOrder = list(before = c("CBM_defaults", "CBM_dataPrep"), after = c("CBM_vol2biomass_SK", "CBM_core")),
  timeunit = "year",
  timeframe = as.POSIXlt(c(NA, NA)),
  citation = list("citation.bib"),
  documentation = list("CBM_dataPrep_SK.Rmd"),
  reqdPkgs = list(
    "reproducible (>=2.1.2)", "data.table", "terra"
  ),
  parameters = rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Cache module events")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "masterRaster", objectClass = "SpatRaster",
      desc = "Raster template defining the study area. Default is a test area in the managed forests of SK.",
      sourceURL = "https://drive.google.com/file/d/1FmtbEKbkzufIifETONxOkoplGX50lrT_"),
    expectsInput(
      objectName = "ageLocator", objectClass = "sf|SpatRaster",
      desc = "Spatial data source of stand ages. Default is the 2012 CASFRI inventory.",
      sourceURL = "https://drive.google.com/file/d/1BV7_LI0hEc8g5AhKe5pfycrdWnVh_G4a"),
    expectsInput(
      objectName = "ageDataYear", objectClass = "numeric",
      desc = "Year that the ages in `ageLocator` represent."),
    expectsInput(
      objectName = "ageSpinupMin", objectClass = "numeric",
      desc = "Minimum age for cohorts during spinup."),
    expectsInput(
      objectName = "gcIndexLocator", objectClass = "sf|SpatRaster",
      desc = "Spatial data source of growth curve index locations.", #TODO: Define default data source
      sourceURL = "https://drive.google.com/file/d/1TJHdzS85BLRMEvT1I2SjYsur-FZM4hmn/"),
    expectsInput(
      objectName = "userGcMeta", objectClass = "data.table",
      desc = "Growth curve metadata.", #TODO: Define default data source
      sourceURL = "https://drive.google.com/file/d/1ugECJVNkglSSQFVqnk5ayG6q38l6AWe9"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.table",
      desc = "Growth curve volumes.", #TODO: Define default data source
      sourceURL = "https://drive.google.com/file/d/13s7fo5Ue5ji0aGYRQcJi-_wIb2-4bgVN"),
    expectsInput(
      objectName = "disturbanceRastersURL", objectClass = "character",
      sourceURL = "https://drive.google.com/file/d/1tsz57amfHjoLafGxjKYSWQPLD7HksdCa",
      desc = paste(
        "The sourceURL is the Wulder and White disturbance rasters for SK covering 1984-2011.",
        "If this URL is provided by the user,",
        "the disturbances will be processed into a list of `disturbanceRasters` for CBM_dataPrep")),
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      sourceURL = "https://drive.google.com/file/d/12z25mHl7McRm1ee7V7dgtSHdLP8-QEZp",
      desc = paste(
        "If the Wulder and White disturbance rasters are used,",
        "the metadata table describing their events is provided."))
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "masterRaster", objectClass = "SpatRaster",
      desc = "Default `masterRaster` if not provided elsewhere by user."),
    createsOutput(
      objectName = "adminLocator", objectClass = "character",
      desc = "Administrative boundary name set to 'Saskatchewan'"),
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
      objectName = "gcIndexLocator", objectClass = "SpatRaster",
      desc = "Default `gcIndexLocator` if not provided elsewhere by user."),
    createsOutput(
      objectName = "userGcMeta", objectClass = "data.table",
      desc = "Default `userGcMeta` if not provided elsewhere by user."),
    createsOutput(
      objectName = "userGcM3", objectClass = "data.table",
      desc = "Default `userGcM3` if not provided elsewhere by user."),
    createsOutput(
      objectName = "curveID", objectClass = "character",
      desc = "`gcIndexLocator`, `userGcMeta`, and `userGcM3` growth curve ID."),
    createsOutput(
      objectName = "disturbanceRasters", objectClass = "list",
      desc = "The Wulder and White disturbance rasters if they are used."),
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

  # Set admin boundary name
  sim$adminLocator <- "Saskatchewan"

  # Read Wulder and White disturbances rasters
  if (identical(sim$disturbanceRastersURL, extractURL("disturbanceRastersURL"))){

    # Download archive of disturbance rasters
    archiveDir <- prepInputs(
      destinationPath = inputPath(sim),
      url         = extractURL("disturbanceRastersURL"),
      archive     = "disturbance.zip",
      targetFile  = "disturbance",
      alsoExtract = do.call(c, lapply(1985:2011, function(simYear){
        paste0("disturbance/dist", simYear, c(".tif"))
      })),
      fun = NA)

    # Prepare files by year
    grdFiles <- list.files(archiveDir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
    # grdYears <- sapply(strsplit(tools::file_path_sans_ext(basename(grdFiles)), "_"), `[[`, 2)
    # names(grdFiles) <- grdYears

    # Set disturbanceRasters list
    sim$disturbanceRasters <- c(
      sim$disturbanceRasters,
      lapply(setNames(1:5, 1:5), function(eventID) grdFiles))
  }

  # Return simList
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  # Master raster
  if (!any(sapply(c("masterRaster", "masterRasterURL"), suppliedElsewhere, sim))){

    message("User has not supplied a master raster ('masterRaster' or 'masterRasterURL'). ",
            "Default for Saskatchewan will be used.")

    sim$masterRaster <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("masterRaster"),
      targetFile = "casfri_dom2.tif",
      fun        = terra::rast
    )

    sim$masterRaster <- terra::classify(
      sim$masterRaster, cbind(0, NA)
    )
  }

  # Stand ages
  if (!any(sapply(c("ageLocator", "ageLocatorURL"), suppliedElsewhere, sim))){

    message("User has not supplied stand age locations ('ageLocator' or 'ageRasterURL'). ",
            "Default for Saskatchewan will be used.")

    sim$ageLocator <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("ageLocator"),
      targetFile = "age1CASFRI.tif",
      fun        = terra::rast
    )

    sim$ageDataYear <- 2012
    sim$ageSpinupMin <- 3
  }

  # Growth curve locations
  if (!any(sapply(c("gcIndexLocator", "gcIndexLocatorURL"), suppliedElsewhere, sim))){

    message("User has not supplied growth curve locations ('gcIndexLocator' or 'gcIndexLocatorURL'). ",
            "Default for Saskatchewan will be used.")

    sim$gcIndexLocator <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("gcIndexLocator"),
      targetFile = "gcIndex.tif",
      fun        = terra::rast
    )
  }

  # Growth curve metadata
  if (!any(sapply(c("userGcMeta", "userGcMetaURL"), suppliedElsewhere, sim))){

    message("User has not supplied growth curve metadata ('userGcMeta' or 'userGcMetaURL'). ",
            "Default for Saskatchewan will be used.")

    sim$userGcMeta <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("userGcMeta"),
      targetFile = "gcMetaEg.csv",
      fun        = data.table::fread
    )
    data.table::setnames(sim$userGcMeta, names(sim$userGcMeta)[[1]], "curveID")
    data.table::setkey(sim$userGcMeta, curveID)
  }

  # Growth curve volumes
  if (!any(sapply(c("userGcM3", "userGcM3URL"), suppliedElsewhere, sim))){

    message("User has not supplied growth curve volumes ('userGcM3' or 'userGcM3URL'). ",
            "Default for Saskatchewan will be used.")

    sim$userGcM3 <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("userGcM3"),
      targetFile = "userGcM3.csv",
      fun        = data.table::fread
    )
    data.table::setnames(sim$userGcM3, names(sim$userGcM3), c("curveID", "Age", "MerchVolume"))
    data.table::setkeyv(sim$userGcM3, c("curveID", "Age"))
  }

  # Disturbance metadata
  if (identical(sim$disturbanceRastersURL, extractURL("disturbanceRastersURL")) &
      !any(sapply(c("disturbanceMeta", "disturbanceMetaURL"), suppliedElsewhere, sim))){

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
    sim$disturbanceMeta$sourceValue <- sim$disturbanceMeta$eventID
  }

  # Return simList
  return(invisible(sim))

}


