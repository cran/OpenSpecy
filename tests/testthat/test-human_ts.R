test_that("Make sure timestamps really differ", {
  t1 <- human_ts()
  Sys.sleep(1)
  t2 <- human_ts()
  expect_false(isTRUE(all.equal(t1, t2)))
})
