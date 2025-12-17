
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration: CBM: SK", {

  ## Run simInit and spades ----

  # Only run manually
  testthat::skip_on_ci()
  testthat::skip_if(testthat::is_testing())

  # Set up project
  projectName <- "2-intg_2-CBM_3-SK"
  times       <- list(start = 1985, end = 1985)

  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      modules = c(
        paste0("PredictiveEcology/CBM_defaults@",    Sys.getenv("BRANCH_NAME", "development")),
        "CBM_dataPrep_SK",
        paste0("PredictiveEcology/CBM_dataPrep@",    Sys.getenv("BRANCH_NAME", "development")),
        paste0("PredictiveEcology/CBM_vol2biomass@", Sys.getenv("BRANCH_NAME", "development")),
        paste0("PredictiveEcology/CBM_core@",        Sys.getenv("BRANCH_NAME", "development"))
      ),
      times   = times,
      paths   = list(
        projectPath = spadesTestPaths$projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
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


