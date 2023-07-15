
# library(tidyverse);library(yingtools2);library(testthat);library(phyloseq)
test_that("calc.distance works", {
  # filter.phyloseq works
  phy <- cid.phy %>% filter(Patient_ID=="301")
  expect_true(is(phy,"phyloseq"))
  expect_lt(ntaxa(phy),ntaxa(cid.phy))
  # calc.distance works
  d.bray <- calc.distance(phy,method="bray")
  expect_true(is(d.bray,"dist"))
  d.unifrac <- calc.distance(phy,method="unifrac")
  expect_true(is(d.unifrac,"dist"))
  d.taxhorn <- calc.distance(phy,method="taxhorn")
  expect_true(is(d.taxhorn,"dist"))

})





