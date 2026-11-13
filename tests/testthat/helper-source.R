# Loaded automatically by testthat before the test files.
suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(htmltools)
})

for (f in c("data_census.R", "zip_lookup.R", "data_risk.R", "risk_metrics.R", "map_risk.R")) {
  p <- file.path("..", "..", "R", f)
  if (file.exists(p)) source(p, local = FALSE)
}

# Tests never read the repo's starter copy unless they pass seed_dir explicitly.
options(risk.seed_dir = "")

# Most tests read the full hover block; compact hover labels are tested on their own.
options(risk.hover_detail = TRUE)

# One row of wide ACS output with every *E column .derive_metrics() reads.
make_acs_wide <- function(...) {
  row <- list(
    GEOID = "00000", NAME = "Test",
    total_popE = 1000, median_ageE = 40,
    labor_forceE = 500, employedE = 480, unemployedE = 20, not_in_labor_forceE = 300,
    median_hh_incomeE = 70000, per_capita_incomeE = 35000,
    poverty_universeE = 1000, poverty_countE = 120,
    housing_unitsE = 450, occupied_unitsE = 400, vacant_unitsE = 50,
    median_home_valueE = 300000, median_rentE = 1200,
    rent_burden_universeE = 200, rb_30_34E = 20, rb_35_39E = 10,
    rb_40_49E = 15, rb_50_plusE = 30, rb_not_computedE = 10,
    education_total_25plusE = 700, bachelorsE = 150, mastersE = 60,
    professionalE = 15, doctorateE = 10
  )
  over <- list(...)
  row[names(over)] <- over
  as.data.frame(row, stringsAsFactors = FALSE)
}
