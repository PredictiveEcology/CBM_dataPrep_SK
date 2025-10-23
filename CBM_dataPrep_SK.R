
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
      desc = "Raster template defining the study area. Default is a test area in the managed forests of SK."),
    expectsInput(
      objectName  = "adminLocator",
      objectClass = "sourceID|sf|SpatRaster|character",
      desc = "Canada administrative boundary name(s). Defaults to 'Saskatchewan'."),
    expectsInput(
      objectName = "ageLocator", objectClass = "sf|SpatRaster",
      desc = "Spatial data source of cohort ages. Default is the 2012 CASFRI inventory backtracked to 1985.",
      sourceURL = "https://drive.google.com/file/d/1ip4VGdKjPhQjElxJjHkM_1BoVM2C_sXs"),
    expectsInput(
      objectName = "ageDataYear", objectClass = "numeric",
      desc = "Year that the ages in `ageLocator` represent."),
    expectsInput(
      objectName = "ageSpinupMin", objectClass = "numeric",
      desc = "Minimum age for cohorts during spinup."),
    expectsInput(
      objectName = "spsLocator", objectClass = "sf|SpatRaster",
      desc = "Spatial data source of cohort species locations.",
      sourceURL = "https://drive.google.com/file/d/1wQwL595kkxseq9CtDKLvHQgJ8JxS8x-L"),
    expectsInput(
      objectName = "prodLocator", objectClass = "sf|SpatRaster",
      desc = "Spatial data source of productivity class locations.",
      sourceURL = "https://drive.google.com/file/d/14JfZm4sIxxKT5pxaEBE0Sf2HH10dOe_c"),
    expectsInput(
      objectName = "userGcMeta", objectClass = "data.table",
      desc = "Growth curve metadata.", #TODO: Define default data source
      sourceURL = "https://drive.google.com/file/d/1rnBRxkvUj7whrVJ8vr9-IpjNTUTdQl-F"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.table",
      desc = "Growth curve volumes.", #TODO: Define default data source
      sourceURL = "https://drive.google.com/file/d/1rnBRxkvUj7whrVJ8vr9-IpjNTUTdQl-F"),
    expectsInput(
      objectName = "disturbanceRastersURL", objectClass = "character",
      sourceURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",
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
      desc = "Default `adminLocator` if not provided elsewhere by user."),
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
      objectName = "cohortLocators", objectClass = "list",
      desc = "List of cohort locators including `spsLocator` and `prodLocator`."),
    createsOutput(
      objectName = "curveID", objectClass = "data.table",
      desc = "Default `curveID` if not provided elsewhere by user."),
    createsOutput(
      objectName = "userGcMeta", objectClass = "data.table",
      desc = "Default `userGcMeta` if not provided elsewhere by user."),
    createsOutput(
      objectName = "userGcM3", objectClass = "data.table",
      desc = "Default `userGcM3` if not provided elsewhere by user."),
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

      sim <- PrepCohortData(sim)

      if (identical(sim$disturbanceRastersURL, extractURL("disturbanceRastersURL"))){
        sim <- PrepTestDisturbances(sim)
      }
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) return(invisible(sim))

PrepCohortData <- function(sim){

  cohortData <- list()

  # Species
  if (!is.null(sim$spsLocator)){

    spsCol <- if (!is.null(terra::cats(sim$spsLocator)[[1]])){
      names(terra::cats(sim$spsLocator)[[1]])[[2]]
    }else "species"

    cohortData[[spsCol]] <- sim$spsLocator
  }

  # Site productivity
  if (!is.null(sim$prodLocator)){
    cohortData[["prodClass"]] <- sim$prodLocator
  }

  # Add data to cohort locators
  sim$cohortLocators <- c(sim$cohortLocators, cohortData)

  # Set curveID
  if (is.null(sim$curveID)) sim$curveID <- names(cohortData)

  # Return simList
  return(invisible(sim))
}

PrepTestDisturbances <- function(sim){

  # Disturbance metadata
  distMeta <- prepInputs(
    destinationPath = inputPath(sim),
    url        = extractURL("disturbanceMeta"),
    targetFile = "SK_disturbances.csv",
    fun        = data.table::fread
  )
  distMeta <- unique(
    distMeta[, .(eventID = rasterID, disturbance_type_id, wholeStand, name = distName, description)])
  distMeta$sourceValue <- distMeta$eventID

  sim$disturbanceMeta <- data.table::rbindlist(
    list(sim$disturbanceMeta, distMeta), fill = TRUE)

  # Download archive of disturbance rasters
  archiveDir <- prepInputs(
    destinationPath = inputPath(sim),
    url         = extractURL("disturbanceRastersURL"),
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

  # Set disturbanceRasters list
  sim$disturbanceRasters <- c(
    sim$disturbanceRasters,
    lapply(setNames(1:5, 1:5), function(eventID) grdFiles))

  # Return simList
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  # Admin boundary name
  if (!suppliedElsewhere("adminLocator", sim)) sim$adminLocator <- "Saskatchewan"

  # Master raster
  if (!suppliedElsewhere("masterRaster")){

    message("User has not supplied a master raster ('masterRaster'). ",
            "Default for Saskatchewan will be used.")

    sim$masterRaster <- terra::rast(
      crs  = "EPSG:3979",
      res  = 30,
      vals = 1L,
      xmin = -1077673.4762,
      xmax =  -426673.4762,
      ymin =   108487.9315,
      ymax =   971077.9315
    )
  }

  # Cohort species
  if (!suppliedElsewhere("spsLocator")){

    sim$spsLocator <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("spsLocator"),
      targetFile = "casfri_dom2-Byte.tif",
      fun        = terra::rast
    )

    # Source: https://drive.google.com/file/d/1w_qoT87TwjClWLz8sheBEzrNoPYrGpN6
    levels(sim$spsLocator) <- data.frame(
      value = 1:7,
      LandR = c("Abie_bal", "Popu_bal", "Pice_mar", "Pinu_ban", "Popu_tre", "Betu_pap", "Pice_gla")
    )
  }

  # Cohort ages
  if (!suppliedElsewhere("ageLocator")){

    message("User has not supplied cohort age locations ('ageLocator''). ",
            "Default for Saskatchewan will be used.")

    sim$ageLocator <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("ageLocator"),
      targetFile = "age1CASFRI-Byte.tif",
      fun        = terra::rast
    )
    sim$ageDataYear  <- 1985
    sim$ageSpinupMin <- 3
  }

  # Growth curves
  if (!suppliedElsewhere("userGcMeta", sim) & !suppliedElsewhere("userGcM3", sim)){

    message("User has not supplied growth curves ('userGcMeta' and 'userGcM3'). ",
            "Default for Saskatchewan will be used.")

    # Read growth curves from CSV
    yieldTbl <- prepInputs(
      destinationPath = inputPath(sim),
      url        = extractURL("userGcM3"),
      targetFile = "yield_1_7.csv",
      fun        = data.table::fread
    ) |> subset(ForestDistrict == "Saskatchewan" & !is.na(prodClass))

    # Create unique curve ID
    yieldTbl[, curveID := .GRP, by = eval(paste0("Age", 0:250))]

    if (!suppliedElsewhere("userGcMeta", sim)){

      sim$userGcMeta <- unique(yieldTbl[, .(species = Species, prodClass, curveID)])
      data.table::setkey(sim$userGcMeta, species, prodClass)

      # Rename conifers
      sim$userGcMeta[species == "Unspecified conifers - Genus type", species := "Coniferous"]

      # Apply trembling aspen curves to white birch
      sim$userGcMeta[species == "White birch", curveID := sim$userGcMeta[
        species == "Trembling aspen" & prodClass == "M", curveID]]

      # Save to outputs directory
      outCSV <- file.path(outputPath(sim), "CBM_dataPrep_SK", "userGcMeta.csv")
      dir.create(dirname(outCSV), recursive = TRUE, showWarnings = FALSE)
      data.table::fwrite(sim$userGcMeta, outCSV)
    }

    if (!suppliedElsewhere("userGcM3", sim)){

      sim$userGcM3 <- data.table::rbindlist(apply(
        unique(yieldTbl[, .SD, .SDcols = c("curveID", paste0("Age", 0:250))]),
        1, function(r){
        data.table::data.table(
          curveID     = r[["curveID"]],
          Age         = 0:250,
          MerchVolume = r[paste0("Age", 0:250)]
        )
      }, simplify = FALSE))
      data.table::setkey(sim$userGcM3, curveID, Age)

      # Save to outputs directory
      outCSV <- file.path(outputPath(sim), "CBM_dataPrep_SK", "userGcM3.csv")
      dir.create(dirname(outCSV), recursive = TRUE, showWarnings = FALSE)
      data.table::fwrite(sim$userGcM3, outCSV)
    }

    # Site productivity
    if (!suppliedElsewhere("prodLocator")){

      sim$prodLocator <- prepInputs(
        destinationPath = inputPath(sim),
        url        = extractURL("prodLocator"),
        targetFile = "site_productivity.tif",
        fun        = terra::rast
      )

      # Source: https://drive.google.com/file/d/1fMpm2m-oaLFjfZLsxOIKz2KDr7II_QiV
      levels(sim$prodLocator) <- data.frame(
        value     = 0:3,
        prodClass = c(NA, "G", "M", "P")
      )
    }
  }

  # Return simList
  return(invisible(sim))

}


