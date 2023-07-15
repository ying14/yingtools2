
# library(tidyverse);library(yingtools2);library(testthat)


test_that("as.mrn() works", {
  # leave alone
  expect_equal(as.mrn("31234567"),"31234567")
  # trim
  expect_equal(as.mrn("   31234567  "),"31234567")
  # add leading zeroes
  expect_equal(as.mrn("4"),"00000004")
  # convert to character
  expect_equal(as.mrn(31234),"00031234")
  # too many digits
  expect_error(as.mrn("312345672"))
  # non-numbers
  expect_error(as.mrn("3123x567"))
  # does not start with 0 or 3
  expect_error(as.mrn("12345678"))
  # NA remains as NA
  expect_true(is.na(as.mrn(NA)))
  # NULL remains as NULL
  expect_true(is.null(as.mrn(NULL)))
})




