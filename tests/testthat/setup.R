
if (!testthat::is_testing()){
  library(testthat)
  testthat::source_test_helpers(env = globalenv())
}

# Set teardown environment
teardownEnv <- if (testthat::is_testing()) testthat::teardown_env() else parent.frame()

# Set up testing directories
testDirs <- SpaDEStestSetUpDirectories(teardownEnv = teardownEnv)

# Set global options
SpaDEStestLocalOptions(teardownEnv = teardownEnv)


## Download standard inputs that are usually provided by CBM_defaults or CBM_vol2biomass.
## RDS data provided where creation of these outputs is more complex than a simple downloads

# Download CBM-CFS3 database usually provided by CBM_defaults
download.file(
  url      = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db",
  destfile = file.path(testDirs$temp$inputs, "dbPath.db"),
  mode     = "wb",
  quiet    = TRUE
)

# Download gcMetaEg usually provided by CBM_vol2biomass
withr::with_options(
  c(googledrive_quiet = TRUE),
  googledrive::drive_download(
    "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ",
    path = file.path(testDirs$temp$inputs, "gcMetaEg.csv")
  ))

