
if (!testthat::is_testing()){
  suppressPackageStartupMessages(library(testthat))
  testthat::source_test_helpers(env = globalenv())
}

# Source work in progress SpaDES module testing functions
suppressPackageStartupMessages(library(SpaDES.core))
tempScript <- tempfile(fileext = ".R")
download.file(
  "https://raw.githubusercontent.com/suz-estella/SpaDES.core/refs/heads/suz-testthat/R/testthat.R",
  tempScript, quiet = TRUE)
source(tempScript)

# Set up testing global options
SpaDEStestSetGlobalOptions()

# Set up testing directories
spadesTestPaths <- SpaDEStestSetUpDirectories(require = "googledrive")


## Download standard inputs that are usually provided by CBM_defaults or CBM_vol2biomass.

# Download CBM-CFS3 database usually provided by CBM_defaults
if (!file.exists(file.path(spadesTestPaths$temp$inputs, "cbm_defaults_v1.2.8340.362.db"))){
  download.file(
    url      = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db",
    destfile = file.path(spadesTestPaths$temp$inputs, "cbm_defaults_v1.2.8340.362.db"),
    mode     = "wb",
    quiet    = TRUE
  )
}

# Download gcMetaEg usually provided by CBM_vol2biomass
if (!file.exists(file.path(spadesTestPaths$temp$inputs, "gcMetaEg.csv"))){
  withr::with_options(
    c(googledrive_quiet = TRUE),
    googledrive::drive_download(
      "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ",
      path = file.path(spadesTestPaths$temp$inputs, "gcMetaEg.csv")
    ))
}

