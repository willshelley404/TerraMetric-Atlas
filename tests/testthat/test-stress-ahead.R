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
  expect_lte(w$sa_drv2_pp, w$sa_drv3_pp)
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
