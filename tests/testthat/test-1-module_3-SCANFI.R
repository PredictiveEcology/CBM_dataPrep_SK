
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SCANFI 2020 data", {

  ## Run simInit and spades ----

  ## Skip test if source data is not already available
  ## Source data is too large to download for a test
  testthat::skip_if(
    !file.exists(file.path(spadesTestPaths$inputPath, "SCANFI-2020")),
    message = "inputs directory does not contain SCANFI-2020 data")

  # Set up project
  projectName <- "1-module_3-SCANFI-2020"
  times       <- list(start = 2020, end = 2020)

  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      modules = "CBM_dataPrep_SK",
      times   = times,
      paths   = list(
        projectPath = spadesTestPaths$projectPath,
        modulePath  = spadesTestPaths$modulePath,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
      ),

      # Set required packages for project set up
      require = "terra",

      # Set study area
      masterRaster = terra::rast(
        crs  = "EPSG:3979",
        res  = 30,
        vals = 1L,
        xmin = -687696,
        xmax = -681036,
        ymin =  711955,
        ymax =  716183
      ),

      # Set input data sources
      ageLocator = "SCANFI-2020-age",
      spsLocator = "SCANFI-2020-LandR"
    )
  )

  # Run simInit
  simTestInit <- SpaDEStestMuffleOutput(
    SpaDES.core::simInit2(simInitInput)
  )

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDEStestMuffleOutput(
    SpaDES.core::spades(simTestInit)
  )

  expect_s4_class(simTest, "simList")


  ## Check outputs ----

  # curveID
  expect_equal(simTest$curveID, c("LandR", "prodClass"))

  # species
  expect_true(inherits(simTest$cohortLocators$LandR, "SpatRaster"))
  expect_equal(terra::unique(simTest$cohortLocators$LandR)[[1]], c(
    "Abie_bal", "Betu_pap", "Pice_mar", "Pice_gla", "Pinu_ban", "Popu_bal", "Popu_tre"
  ))
  spsVals <- terra::values(simTest$cohortLocators$LandR, mat = FALSE)
  expect_equal(
    data.table::data.table(val = spsVals)[, .N, by = "val"][order(val)],
    data.table::data.table(
      val = c(0:7),
      N   = c(4477, 4, 97, 23727, 59, 1900, 773, 629)
    ), tolerance = 10, scale = 1)
})

