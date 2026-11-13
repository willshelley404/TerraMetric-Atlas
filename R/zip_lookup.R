# ── ZIP codes for counties and tracts (hover labels) ─────────────────────────
# zipcodeR::zip_code_db has one centroid per ZIP, not boundaries, so a ZIP belongs to the
# polygon that contains its centroid. Approximate by design: a ZIP that straddles two
# counties is listed only in the one holding its centroid. Tracts too small to contain a
# centroid get the nearest ZIP, flagged so the label can say so.

.ZIP_ENV <- new.env(parent = emptyenv())

#' Standard ZIPs as points, most populous first (so a truncated list shows the big ones).
#' NULL when zipcodeR is not installed.
.zip_points <- function() {
  if (!is.null(.ZIP_ENV$pts)) return(.ZIP_ENV$pts)
  if (!requireNamespace("zipcodeR", quietly = TRUE)) return(NULL)
  z <- as.data.frame(zipcodeR::zip_code_db)
  z <- z[z$zipcode_type %in% "Standard" & !is.na(z$lat) & !is.na(z$lng),
         c("zipcode", "lat", "lng", "population")]
  z$population[is.na(z$population)] <- 0
  z <- z[order(-z$population), ]
  .ZIP_ENV$pts <- sf::st_as_sf(z, coords = c("lng", "lat"), crs = 4326)
  .ZIP_ENV$pts
}

#' ZIPs per polygon, in the row order of `polys` (needs a GEOID column).
#' Returns tibble(GEOID, zip_list = "90011, 91331, ...", zip_n, zip_nearest). zip_list is
#' NA where nothing was found (or zipcodeR / the geometry is unusable).
zips_by_polygon <- function(polys, nearest = FALSE) {
  ids <- as.character(polys$GEOID)
  out <- tibble::tibble(GEOID = ids, zip_list = NA_character_, zip_n = NA_integer_,
                        zip_nearest = FALSE)
  pts <- .zip_points()
  if (is.null(pts) || nrow(polys) == 0) return(out)
  tryCatch({
    old <- suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
    g <- sf::st_transform(sf::st_geometry(polys), 4326)
    hits <- suppressWarnings(sf::st_intersects(g, pts))
    zips <- pts$zipcode
    for (i in seq_along(hits)) {
      idx <- sort(hits[[i]])               # pts is population-ordered
      if (length(idx)) {
        out$zip_list[i] <- paste(zips[idx], collapse = ", ")
        out$zip_n[i] <- length(idx)
      }
    }
    miss <- which(is.na(out$zip_list))
    if (nearest && length(miss)) {
      nn <- suppressWarnings(sf::st_nearest_feature(sf::st_point_on_surface(g[miss]), pts))
      out$zip_list[miss] <- zips[nn]
      out$zip_n[miss] <- 1L
      out$zip_nearest[miss] <- TRUE
    }
    out
  }, error = function(e) {
    message("[Risk] zip lookup failed: ", conditionMessage(e))
    out
  })
}

#' Hover text per element: "ZIP 80439", "ZIPs 80439, 80452", "ZIPs 90011, 91331, … +275
#' more" or "Nearest ZIP 37402". NA where there is no ZIP. Arguments may be NULL.
zip_label <- function(zip_list, zip_n = NULL, nearest = FALSE, max_show = 5) {
  n <- length(zip_list)
  if (n == 0) return(NA_character_)
  zip_n <- rep_len(if (is.null(zip_n)) NA_integer_ else zip_n, n)
  nearest <- rep_len(nearest, n) %in% TRUE
  vapply(seq_len(n), function(i) {
    zl <- zip_list[i]
    if (is.na(zl) || !nzchar(zl)) return(NA_character_)
    parts <- strsplit(zl, ", ", fixed = TRUE)[[1]]
    total <- if (is.na(zip_n[i])) length(parts) else zip_n[i]
    if (nearest[i]) return(paste("Nearest ZIP", parts[1]))
    shown <- utils::head(parts, max_show)
    paste0(if (total == 1) "ZIP " else "ZIPs ", paste(shown, collapse = ", "),
           if (total > length(shown)) sprintf(" +%d more", total - length(shown)) else "")
  }, character(1))
}
