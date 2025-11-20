
# Map SCANFI species to SK curves
SCANFImatchSpeciesToCurves <- function(
    masterRaster, spsMatch, spsRast = NULL,
    parallel.cores = NULL, parallel.tileSize = 2500,
    inputDir = getOption("spades.inputPath", ".")){

  if (is.null(spsRast)){
    message("SCANFI species: reading species layers")
    spsRast <- CBMutils::CBMsourcePrepInputs(
      "SCANFI-2020-LandR", inputPath = inputDir, verbose = -1)$source
  }

  message("SCANFI species: reading species table")
  spsNames <- names(spsRast)

  spsMatch <- data.table::as.data.table(spsMatch)
  spsMatch[, id := match(curve, unique(spsMatch$curve))]
  spsMatch[, intersect(spsNames, names(spsMatch)) := lapply(.SD, as.logical),
           .SDcols = intersect(spsNames, names(spsMatch))]
  spsMatch[,   setdiff(spsNames, names(spsMatch)) := FALSE]

  message("SCANFI species: reading study area bounds")
  adminMask <- sf::st_intersection(
    CBMutils::CBMsourcePrepInputs(
      "StatCan-admin", inputPath = inputDir, verbose = -1)$source |>
      subset(admin == "Saskatchewan") |>
      sf::st_segmentize(10000) |>
      sf::st_transform(sf::st_crs(spsRast)) |>
      sf::st_union(),
    sf::st_as_sfc(sf::st_bbox(masterRaster)) |>
      sf::st_segmentize(10000) |>
      sf::st_transform(sf::st_crs(spsRast))
  )

  spsOut <- terra::crop(terra::rast(
    ext = terra::ext(spsRast),
    crs = terra::crs(spsRast),
    res = terra::res(spsRast)
  ), adminMask, snap = "out")

  if (is.null(parallel.cores) || is.na(parallel.cores)){

    message("SCANFI species: cropping data to study area")

    spsRast <- terra::crop(spsRast, adminMask, mask = FALSE, snap = "out")

    message("SCANFI species: extracting leading species")

    spsMax <- terra::app(spsRast, "max")
    spsMax <- terra::mask(spsMax, terra::vect(adminMask))
    spsMax <- terra::classify(spsMax, cbind(0, NA))

    cellIdx <- terra::cells(spsMax)

    spsIDs <- data.table::data.table(terra::extract(spsRast, cellIdx))
    spsIDs[, cellIdx := cellIdx]
    rm(cellIdx)

    spsIDs[, max := terra::extract(spsMax, cellIdx)]
    rm(spsMax)

    spsIDs[, (spsNames) := lapply(.SD, `==`, max), .SDcols = spsNames]
    spsIDs[, max := NULL]

    message("SCANFI species: matching leading species to SK curves")

    spsIDs <- data.table::merge.data.table(spsIDs, spsMatch, by = spsNames, all.x = TRUE)

    if (any(is.na(spsIDs$id))){
      notFound <- unique(spsIDs[is.na(id), .SD, .SDcols = spsNames])
      stop("SCANFI leading species combination(s) have not been assigned a curve: ",
           paste(shQuote(apply(notFound, 1, function(r) paste(names(r)[r], collapse = ";"))),
                 collapse = ", "))
    }

    spsIDs[, setdiff(c("cellIdx", "id"), names(spsIDs)) := NULL]

  }else{

    tileExt <- terra::getTileExtents(spsOut, parallel.tileSize)

    message("SCANFI species: reading leading species in ", nrow(tileExt), " tiles")

    spsRastFiles <- terra::sources(spsRast)
    names(spsRastFiles) <- names(spsRast)

    spsRast <- terra::rast(spsRastFiles[[1]])

    spsIDs <- data.table::rbindlist(parallel::mclapply(
      lapply(1:nrow(tileExt), function(i){
        terra::cells(spsRast, terra::vect(sf::st_crop(adminMask, tileExt[i,])), touches = TRUE)[,2]
      }),
      mc.cores = parallel.cores,
      function(cellIdx){

        s <- data.table::as.data.table(lapply(spsRastFiles, function(f){
          terra::extract(terra::rast(f), cellIdx)
        }))

        s[, max := apply(s, 1, max)]
        s[, cellIdx := cellIdx]
        rm(cellIdx)

        s <- s[max > 0,]
        if (nrow(s) > 0){

          s[, (spsNames) := lapply(.SD, `==`, max), .SDcols = spsNames]
          s[, max := NULL]
          s <- data.table::merge.data.table(s, spsMatch, by = spsNames, all.x = TRUE)

          if (any(is.na(s$id))){
            notFound <- unique(s[is.na(id), .SD, .SDcols = spsNames])
            stop("SCANFI leading species combination(s) have not been assigned a curve: ",
                 paste(shQuote(apply(notFound, 1, function(r) paste(names(r)[r], collapse = ";"))),
                       collapse = ", "))
          }
        }else s[, id := numeric(0)]

        s[, .(cellIdx, id)]
      }))

    spsIDs[, cellIdx := terra::cellFromXY(spsOut, terra::xyFromCell(spsRast, cellIdx))]
  }

  message("SCANFI species: returning leading species")

  terra::values(spsOut) <- 0L
  terra::set.values(spsOut, spsIDs$cellIdx, spsIDs$id)
  levels(spsOut) <- rbind(cbind(id = 0, curve = NA), unique(spsMatch[, .(id, curve)]))
  return(spsOut)
}



