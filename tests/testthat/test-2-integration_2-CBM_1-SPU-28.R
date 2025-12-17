
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration: CBM: SK test area (SPU 28)", {

  ## Run simInit and spades ----

  # Skip on GHA
  testthat::skip_on_ci()

  # Set up project
  projectName <- "2-intg_2-CBM_1-SPU-28"
  times       <- list(start = 1985, end = 1985)

  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      modules = c(
        paste0("PredictiveEcology/CBM_defaults@",     Sys.getenv("BRANCH_NAME", "development")),
        "CBM_dataPrep_SK",
        paste0("PredictiveEcology/CBM_dataPrep@",     Sys.getenv("BRANCH_NAME", "development")),
        paste0("PredictiveEcology/CBM_vol2biomassK@", Sys.getenv("BRANCH_NAME", "development")),
        paste0("PredictiveEcology/CBM_core@",         Sys.getenv("BRANCH_NAME", "development"))
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
        ext  = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
        res  = 30,
        vals = 1L
      )
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


