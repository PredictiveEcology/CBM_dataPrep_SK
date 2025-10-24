
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration: CBM: SCANFI 2020 data", {

  ## Run simInit and spades ----

  ## Skip test if source data is not already available
  ## Source data is too large to download for a test
  testthat::skip_if(
    !file.exists(file.path(spadesTestPaths$inputPath, "SCANFI-2020")),
    message = "inputs directory does not contain SCANFI-2020 data")

  # Set up project
  projectName <- "2-intg_3-SCANFI-2020"
  times       <- list(start = 2020, end = 2020)

  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      modules = c(
        "CBM_dataPrep_SK",
        paste0("PredictiveEcology/CBM_dataPrep@",       Sys.getenv("BRANCH_NAME", "development")),
        paste0("PredictiveEcology/CBM_vol2biomass_SK@", Sys.getenv("BRANCH_NAME", "development")),
        paste0("PredictiveEcology/CBM_core@",           Sys.getenv("BRANCH_NAME", "development"))
      ),
      times   = times,
      paths   = list(
        projectPath = spadesTestPaths$projectPath,
        modulePath  = spadesTestPaths$temp$modules,
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

  expect_true(!is.null(simTest$emissionsProducts))

})

