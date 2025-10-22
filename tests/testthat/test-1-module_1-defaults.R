
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: defaults", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "1-module_1-defaults"
  times       <- list(start = 1985, end = 1985)

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

  # masterRaster
  expect_true(!is.null(simTest$masterRaster))
  expect_true(inherits(simTest$masterRaster, "SpatRaster"))

  # adminLocator
  expect_true(!is.null(simTest$adminLocator))
  expect_equal(simTest$adminLocator, "Saskatchewan")

  # ageLocator
  expect_true(!is.null(simTest$ageLocator))
  expect_true(inherits(simTest$ageLocator, "SpatRaster"))

  # ageDataYear
  expect_true(!is.null(simTest$ageDataYear))
  expect_true(inherits(simTest$ageDataYear, "numeric"))

  # ageSpinupMin
  expect_true(!is.null(simTest$ageSpinupMin))
  expect_true(inherits(simTest$ageSpinupMin, "numeric"))

  # spsLocator
  expect_true(!is.null(simTest$spsLocator))
  expect_true(inherits(simTest$spsLocator, "SpatRaster"))

  # prodLocator
  expect_true(!is.null(simTest$prodLocator))
  expect_true(inherits(simTest$prodLocator, "SpatRaster"))

  # userGcMeta
  expect_true(!is.null(simTest$userGcMeta))
  expect_true(inherits(simTest$userGcMeta, "data.table"))
  expect_true("curveID" %in% names(simTest$userGcMeta))

  # userGcM3
  expect_true(!is.null(simTest$userGcM3))
  expect_true(inherits(simTest$userGcM3, "data.table"))
  expect_true("curveID" %in% names(simTest$userGcM3))

  # disturbanceRasters
  expect_true(is.null(simTest$disturbanceRasters))

  # disturbanceMeta
  expect_true(is.null(simTest$disturbanceMeta))

})


