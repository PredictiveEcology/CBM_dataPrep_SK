
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with study AOI", {

  ## Run simInit and spades ----

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "2-withAOI")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set up project
  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      times = list(start = 1998, end = 2000),

      modules = "CBM_dataPrep_SK",
      paths   = list(
        projectPath = projectPath,
        modulePath  = spadesTestPaths$modulePath,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(projectPath, "outputs")
      ),

      require = "sf",

      ecoLocator = sf::st_read(file.path(spadesTestPaths$testdata, "ecoLocator.shp"), quiet = TRUE),
      spuLocator = sf::st_read(file.path(spadesTestPaths$testdata, "spuLocator.shp"), quiet = TRUE),

      # Set study area
      masterRaster = file.path(spadesTestPaths$testdata, "masterRaster-withAOI.tif"),

      # Test matching user disturbances with CBM-CFS3 disturbances
      disturbanceMeta = rbind(
        data.frame(eventID = 1, wholeStand = 1, name = "Wildfire"),
        data.frame(eventID = 2, wholeStand = 1, name = "Clearcut harvesting without salvage"),
        data.frame(eventID = 3, wholeStand = 0, name = "Generic 20% mortality"),
        data.frame(eventID = 4, wholeStand = 1, name = "Deforestation")
      ),
      dbPath = {
        dbPath <- file.path(spadesTestPaths$inputPath, "dbPath.db")
        if (!file.exists(dbPath)) download.file(
          url      = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db",
          destfile = dbPath,
          mode     = "wb",
          quiet    = TRUE)
        dbPath
      }
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


  ## Check output 'standDT' ----

  expect_true(!is.null(simTest$standDT))
  expect_true(inherits(simTest$standDT, "data.table"))

  for (colName in c("pixelIndex", "area", "spatial_unit_id")){
    expect_true(colName %in% names(simTest$standDT))
    expect_true(all(!is.na(simTest$standDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$standDT), "pixelIndex")

  # Expect that there is 1 row for every non-NA cell in masterRaster
  mrValues <- terra::values(terra::rast(file.path(spadesTestPaths$testdata, "masterRaster-withAOI.tif")))
  expect_equal(nrow(simTest$standDT), sum(!is.na(mrValues[,1])))
  expect_equal(simTest$standDT$pixelIndex, which(!is.na(mrValues[,1])))


  ## Check output 'cohortDT' ----

  expect_true(!is.null(simTest$cohortDT))
  expect_true(inherits(simTest$cohortDT, "data.table"))

  for (colName in c("cohortID", "pixelIndex", "gcids", "ages")){
    expect_true(colName %in% names(simTest$cohortDT))
    expect_true(all(!is.na(simTest$cohortDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$cohortDT), "cohortID")

  # Check spinup ages are all >= 3
  expect_true("ageSpinup" %in% names(simTest$cohortDT))
  expect_equal(simTest$cohortDT$ageSpinup[simTest$cohortDT$ages >= 3],
               simTest$cohortDT$ages[simTest$cohortDT$ages >= 3])
  expect_true(all(simTest$ageSpinup[simTest$cohortDT$ages < 3] == 3))

  # Expect that there is 1 row for every non-NA cell in masterRaster
  expect_equal(nrow(simTest$cohortDT), sum(!is.na(mrValues[,1])))
  expect_equal(simTest$cohortDT$pixelIndex, which(!is.na(mrValues[,1])))


  ## Check output 'curveID' ----

  expect_true(!is.null(simTest$curveID))
  expect_true(length(simTest$curveID) >= 1)
  expect_true("gcids" %in% simTest$curveID)
  expect_true(all(simTest$curveID %in% names(simTest$cohortDT)))


  ## Check output 'gcMeta' ----

  expect_true(!is.null(simTest$gcMeta))
  expect_true(inherits(simTest$gcMeta, "data.table"))

  for (colName in c("gcids", "species_id", "sw_hw")){
    expect_true(colName %in% names(simTest$gcMeta))
    expect_true(all(!is.na(simTest$gcMeta[[colName]])))
  }


  ## Check output 'userGcM3' ----

  expect_true(!is.null(simTest$userGcM3))
  expect_true(inherits(simTest$userGcM3, "data.table"))

  for (colName in c("gcids", "Age", "MerchVolume")){
    expect_true(colName %in% names(simTest$userGcM3))
    expect_true(all(!is.na(simTest$userGcM3[[colName]])))
  }


  ## Check output 'disturbanceEvents' -----

  expect_true(!is.null(simTest$disturbanceEvents))
  expect_true(inherits(simTest$disturbanceEvents, "data.table"))

  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(simTest$disturbanceEvents))
    expect_true(is.integer(simTest$disturbanceEvents[[colName]]))
    expect_true(all(!is.na(simTest$disturbanceEvents[[colName]])))
  }

  expect_true(all(simTest$disturbanceEvents$pixelIndex %in% simTest$allPixDT$pixelIndex))
  expect_true(all(simTest$disturbanceEvents$year       %in% 1998:2000))

  expect_equal(nrow(simTest$disturbanceEvents), 1393)


  ## Check output 'disturbanceMeta' ----

  expect_true(!is.null(simTest$disturbanceMeta))
  expect_true(inherits(simTest$disturbanceMeta, "data.table"))

  expect_equal(nrow(simTest$disturbanceMeta), 4)

  # Check that disturbances have been matched correctly
  rowsExpect <- rbind(
    data.frame(
      eventID             = 1,
      wholeStand          = 1,
      name                = "Wildfire",
      disturbance_type_id = 1
    ),
    data.frame(
      eventID             = 2,
      wholeStand          = 1,
      name                = "Clearcut harvesting without salvage",
      disturbance_type_id = 204
    ),
    data.frame(
      eventID             = 3,
      wholeStand          = 0,
      name                = "Generic 20% mortality",
      disturbance_type_id = 168
    ),
    data.frame(
      eventID             = 4,
      wholeStand          = 1,
      name                = "Deforestation",
      disturbance_type_id = 7
    )
  )

  for (i in 1:nrow(rowsExpect)){
    expect_equal(nrow(merge(simTest$disturbanceMeta, rowsExpect[i,], by = names(rowsExpect))), 1)
  }
})

