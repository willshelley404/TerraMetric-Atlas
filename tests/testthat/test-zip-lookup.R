skip_if_not_installed("zipcodeR")

box <- function(x0, y0, x1, y1) sf::st_polygon(list(rbind(
  c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))

test_that("zips_by_polygon lists the ZIPs whose centroid is inside, biggest first", {
  # A box around Clear Creek County, CO (80439, 80452 sit inside it)
  polys <- sf::st_sf(GEOID = "08019", geometry = sf::st_sfc(box(-106, 39.5, -105.3, 39.9), crs = 4326))
  z <- zips_by_polygon(polys)
  parts <- strsplit(z$zip_list, ", ")[[1]]
  expect_true(all(c("80439", "80452") %in% parts))
  expect_equal(z$zip_n, length(parts))
  expect_false(z$zip_nearest)
})

test_that("tracts without a ZIP centroid get the nearest ZIP, flagged", {
  tiny <- sf::st_sf(GEOID = "x", geometry = sf::st_sfc(box(-116.5001, 39.0001, -116.5, 39.0), crs = 4326))
  expect_true(is.na(zips_by_polygon(tiny)$zip_list))
  z <- zips_by_polygon(tiny, nearest = TRUE)
  expect_true(z$zip_nearest)
  expect_equal(z$zip_n, 1L)
  expect_match(z$zip_list, "^[0-9]{5}$")
})

test_that("zip_label truncates, pluralises and tolerates missing values", {
  expect_equal(zip_label("80439"), "ZIP 80439")
  expect_equal(zip_label("80439, 80452"), "ZIPs 80439, 80452")
  expect_equal(zip_label("1, 2, 3, 4, 5, 6, 7", 7), "ZIPs 1, 2, 3, 4, 5 +2 more")
  expect_equal(zip_label("37402", 1, nearest = TRUE), "Nearest ZIP 37402")
  expect_true(is.na(zip_label(NA_character_)))
  expect_true(is.na(zip_label("")))
})

test_that("hover labels show the ZIPs when present and stay unchanged without", {
  with <- as.character(risk_tooltips_compact_vec("Clear Creek County, CO", "+0.6 pp", "Top 9%",
                                                 zips = "ZIPs 80439, 80452")[[1]])
  without <- as.character(risk_tooltips_compact_vec("Clear Creek County, CO", "+0.6 pp", "Top 9%")[[1]])
  expect_match(with, "ZIPs 80439, 80452", fixed = TRUE)
  expect_false(grepl("ZIP", without, fixed = TRUE))
  full <- as.character(risk_tooltips_vec("A", value_text = "1", footer = "src",
                                         zips = "ZIP 80439")[[1]])
  expect_match(full, "ZIP 80439", fixed = TRUE)
})
