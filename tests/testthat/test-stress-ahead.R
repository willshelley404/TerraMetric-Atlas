test_that("shift-share gap is relative to US", {
  s <- stress_ahead(fx_industry(), fx_weights())
  expect_lt(s$stress_ahead_pp[s$GEOID == "13295"], 0)
  ok <- !is.na(s$proj_pct)
  ind <- fx_industry()
  w <- ind$emp_total[match(s$GEOID, ind$GEOID)][ok]
  expect_equal(sum(w * s$proj_pct[ok]) / sum(w), s$us_proj_pct[1], tolerance = 1e-6)
  expect_length(unique(s$us_proj_pct), 1)
})

test_that("zero employment gives NA projection", {
  s <- stress_ahead(fx_industry(), fx_weights())
  z <- s[s$GEOID == "15005", ]
  expect_true(is.na(z$stress_ahead_pp))
  expect_true(is.na(z$share_manufacturing))
  expect_true(is.na(z$sa_drv1_label))
  expect_false(any(is.nan(s$stress_ahead_pp)))
  expect_false(any(is.infinite(s$stress_ahead_pp)))
})

test_that("drag drivers are most negative contributions", {
  s <- stress_ahead(fx_industry(), fx_weights())
  w <- s[s$GEOID == "13295", ]
  expect_equal(w$sa_drv1_label, "Manufacturing")
  expect_lt(w$sa_drv1_pp, 0)
  expect_lte(w$sa_drv1_pp, w$sa_drv2_pp)
})

test_that("shift-share carries its attributes", {
  s <- stress_ahead(fx_industry(), fx_weights())
  expect_equal(attr(s, "ref_month"), as.Date("2026-10-01"))
  expect_equal(attr(s, "temphelp_m_pct"), -4.2)
  expect_equal(attr(s, "acs_year"), 2024)
})

test_that("missing Stress ahead marks layer unavailable, others ok", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = NULL)
  st <- attr(l, "layer_status")
  expect_equal(st[["stress_ahead"]], "unavailable")
  expect_equal(st[["stress_now"]], "ok")
  expect_equal(st[["poverty_rate"]], "ok")
  h <- vapply(.county_tooltips(l, "stress_ahead", RISK_LAYERS$stress_ahead,
                               attr(l, "asof")[["stress_ahead"]], st[["stress_ahead"]]),
              as.character, "")
  expect_true(all(grepl("Insufficient data", h, fixed = TRUE)))
})

test_that("Stress ahead is loading, then stale, from the component status", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), loading = "stress_ahead")
  expect_equal(attr(l, "layer_status")[["stress_ahead"]], "loading")
  sa <- fx_stress_ahead(); attr(sa, "stale") <- TRUE
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = sa)
  expect_equal(attr(l, "layer_status")[["stress_ahead"]], "stale")
})

test_that("Stress ahead as-of and drivers in tooltip", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead())
  expect_equal(attr(l, "asof")[["stress_ahead"]], "CES Oct 2026 · ACS 2020–2024")
  expect_equal(attr(l, "layer_status")[["stress_ahead"]], "ok")
  tips <- vapply(.county_tooltips(l, "stress_ahead", RISK_LAYERS$stress_ahead,
                                  attr(l, "asof")[["stress_ahead"]], "ok"), as.character, "")
  walker <- tips[l$GEOID == "13295"]
  expect_true(grepl("Manufacturing", walker, fixed = TRUE))
  expect_true(grepl("#ff6b6b", walker, fixed = TRUE))
  expect_true(grepl("pp</span>", walker, fixed = TRUE))
})

test_that("Stress ahead layer sits after Stress now and takes the sign-keyed pill", {
  expect_equal(names(RISK_LAYERS)[1:2], c("stress_now", "stress_ahead"))
  expect_equal(names(risk_lens_choices("context"))[2], RISK_LAYERS$stress_ahead$label)
  expect_equal(.rank_bad(c(90, 10), RISK_LAYERS$stress_ahead, c(-0.1, 0.2)), c(TRUE, FALSE))
})

test_that("side panel lists top industries and temp-help note", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead())
  h <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l))
  expect_true(grepl("Top resident industries", h, fixed = TRUE))
  expect_true(grepl("Temporary help payrolls (US): -4.2%", h, fixed = TRUE))
  expect_true(grepl("Manufacturing", h, fixed = TRUE))
  expect_true(grepl("US payrolls −3.0%/yr", h, fixed = TRUE))
  expect_equal(lengths(regmatches(h, gregexpr("<tr", h))), length(RISK_LAYERS) + 1)
  # No Stress ahead data: the blocks are omitted rather than shown empty.
  l0 <- build_county_layers(fx_county_base(), fx_momentum())
  h0 <- as.character(risk_side_panel(l0[l0$GEOID == "13295", ], "county", l0))
  expect_false(grepl("Top resident industries", h0, fixed = TRUE))
})

# Tooltip text after "Driven by" ("" when the block is absent).
.drivers_part <- function(tip) {
  i <- regexpr("Driven by", tip, fixed = TRUE)
  if (i < 0) "" else substring(tip, i)
}
.n_driver_rows <- function(tip) {
  lengths(regmatches(.drivers_part(tip), gregexpr("[+-][0-9.]+ pp</span>", .drivers_part(tip))))
}

test_that("only sectors that drag are listed as drivers", {
  s <- stress_ahead(fx_industry(), fx_weights())
  w <- s[s$GEOID == "13295", ]; cap <- s[s$GEOID == "09110", ]
  expect_false(is.na(w$sa_drv2_label))
  expect_true(is.na(w$sa_drv3_label) && is.na(w$sa_drv3_pp))   # fewer than 3 negatives
  expect_true(is.na(cap$sa_drv1_label))                          # no drag at all
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead())
  tips <- vapply(.county_tooltips(l, "stress_ahead", RISK_LAYERS$stress_ahead,
                                  attr(l, "asof")[["stress_ahead"]], "ok"), as.character, "")
  expect_equal(.n_driver_rows(tips[l$GEOID == "13295"]), 2)
  # No drag, so the block lists lifts instead (one row per positive contribution).
  expect_equal(.n_driver_rows(tips[l$GEOID == "09110"]), 2)
})

test_that("per-element driver labels line up across counties", {
  w <- fx_weights(); w$m_pct[w$group == "educ_health"] <- -2    # Capitol now drags too
  sa <- stress_ahead(fx_industry(), w); attr(sa, "weights") <- w
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = sa)
  tips <- vapply(.county_tooltips(l, "stress_ahead", RISK_LAYERS$stress_ahead,
                                  attr(l, "asof")[["stress_ahead"]], "ok"), as.character, "")
  walker <- .drivers_part(tips[l$GEOID == "13295"])
  cap <- .drivers_part(tips[l$GEOID == "09110"])
  expect_equal(sa$sa_drv1_label[sa$GEOID == "09110"], "Education & health")
  expect_true(grepl("Education &amp; health", cap, fixed = TRUE))
  expect_false(grepl("Manufacturing", cap, fixed = TRUE))
  expect_true(grepl("Manufacturing", walker, fixed = TRUE))
  expect_equal(.n_driver_rows(cap), 1)
})

test_that("Stress ahead shows 2 decimals; Stress now keeps 1", {
  expect_equal(RISK_LAYERS$stress_ahead$digits, 2)
  expect_equal(.risk_fmt(0.08, "pp", 2), "+0.08 pp")
  expect_equal(.risk_fmt(-0.004, "pp", 2), "+0.00 pp")          # signed zero normalised
  expect_equal(.risk_fmt(-0.04, "pp"), "+0.0 pp")
  expect_equal(.risk_fmt(c(-0.35, NA), "pp", 2), c("-0.35 pp", NA))
  expect_equal(.bin_labels(RISK_LAYERS$stress_ahead$bins, "pp", 2)[c(1, 4, 7)],
               c("below −0.60", "−0.10 to +0.10", "above +0.60"))
  expect_equal(.legend_format(RISK_LAYERS$stress_ahead)(type = "numeric", cuts = 0.25)[1], "0.25 pp")
})

test_that("a value that displays as zero is never a red pill", {
  cfg <- RISK_LAYERS$stress_ahead
  expect_equal(.rank_bad(c(40, 40, 40), cfg, c(-0.004, -0.04, 0.08)), c(FALSE, TRUE, FALSE))
  # Stress now (1 dp, higher is worse): +0.04 displays +0.0 and is not red either.
  expect_equal(.rank_bad(c(60, 60), RISK_LAYERS$stress_now, c(0.04, 0.2)), c(FALSE, TRUE))
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead())
  l$stress_ahead_pp <- c(-0.004, 0.08)
  tips <- vapply(.county_tooltips(l, "stress_ahead", cfg, "x", "ok"), as.character, "")
  expect_true(grepl("+0.00 pp", tips[1], fixed = TRUE))
  expect_false(grepl("#ff8787", tips[1], fixed = TRUE))          # pill text colour of a red pill
  expect_true(grepl("+0.08 pp", tips[2], fixed = TRUE))
})

test_that("missing sector momentum counts as 0 in the sum but flags the result partial", {
  w <- fx_weights(); w$m_pct[w$group == "retail"] <- NA
  s <- stress_ahead(fx_industry(), w)
  expect_true(isTRUE(attr(s, "partial")))
  w0 <- fx_weights(); w0$m_pct[w0$group == "retail"] <- 0
  expect_equal(s$stress_ahead_pp, stress_ahead(fx_industry(), w0)$stress_ahead_pp)
  expect_false(isTRUE(attr(stress_ahead(fx_industry(), fx_weights()), "partial")))
})

test_that("partial Stress ahead is marked partial in status and legend; stale wins", {
  w <- fx_weights(); w$m_pct[w$group == "retail"] <- NA
  sa <- stress_ahead(fx_industry(), w)
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = sa, sector_weights = w)
  expect_equal(attr(l, "layer_status")[["stress_ahead"]], "partial")
  m <- build_risk_county_map(l, "stress_ahead")
  title <- Filter(function(cl) cl$method == "addLegend", m$x$calls)[[1]]$args[[1]]$title
  expect_true(grepl("partial", title, fixed = TRUE))
  attr(sa, "stale") <- TRUE
  l2 <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = sa)
  expect_equal(attr(l2, "layer_status")[["stress_ahead"]], "stale")
})

test_that("Stress ahead is not rounded before it is ranked", {
  s <- stress_ahead(fx_industry(), fx_weights())
  ok <- !is.na(s$proj_pct)
  expect_equal(s$stress_ahead_pp[ok], s$proj_pct[ok] - s$us_proj_pct[ok])
  expect_false(isTRUE(all.equal(s$stress_ahead_pp[ok], round(s$stress_ahead_pp[ok], 2),
                                tolerance = 0)))
})

test_that("a county with no drag sectors lists its top lifts, in a neutral colour", {
  s <- stress_ahead(fx_industry(), fx_weights())
  cap <- s[s$GEOID == "09110", ]
  # Capitol is under-weight in shrinking manufacturing, so that is its biggest lift.
  expect_equal(cap$sa_lift1_label, "Manufacturing")
  expect_equal(cap$sa_lift2_label, "Education & health")
  expect_gt(cap$sa_lift1_pp, cap$sa_lift2_pp)
  expect_true(is.na(cap$sa_lift3_label))
  expect_true(is.na(s$sa_lift1_label[s$GEOID == "15005"]))
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead())
  tips <- vapply(.county_tooltips(l, "stress_ahead", RISK_LAYERS$stress_ahead,
                                  attr(l, "asof")[["stress_ahead"]], "ok"), as.character, "")
  cap_drv <- .drivers_part(tips[l$GEOID == "09110"])
  expect_true(grepl("Education &amp; health", cap_drv, fixed = TRUE))
  expect_true(grepl("+0.07 pp", cap_drv, fixed = TRUE))
  expect_false(grepl("#ff6b6b", cap_drv, fixed = TRUE))           # not the "worse" red
  # Walker has drags, so it keeps listing those.
  expect_true(grepl("#ff6b6b", .drivers_part(tips[l$GEOID == "13295"]), fixed = TRUE))
  # With no Stress ahead component the lift columns exist and are NA.
  l0 <- build_county_layers(fx_county_base(), fx_momentum())
  expect_true(all(is.na(l0$sa_lift1_label)))
})

test_that("industry panel carries the pandemic-years note with the ACS window", {
  l <- build_county_layers(fx_county_base(), fx_momentum(), stress_ahead = fx_stress_ahead())
  h <- as.character(risk_side_panel(l[l$GEOID == "13295", ], "county", l))
  expect_true(grepl(paste0("Industry mix: ACS 2020–2024 5-year (includes pandemic years; ",
                           "mix moves slowly)."), h, fixed = TRUE))
})
