test_that("theme_hsci loads", {
  expect_s3_class(theme_hsci(),"theme")
})

test_that("requesting too many colours from theme_hsci_discrete gives a warning", {
  expect_warning(coloropt_pal()(50))
})

test_that("requesting too many colours from theme_hsci_discrete still gives colors", {
  expect_length(suppressWarnings(coloropt_pal()(50)), 50)
})
