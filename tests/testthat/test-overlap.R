test_that("check overlap input", {
  f1 <- list(A = 1:2, B = 2:3)
  f2 <- check_overlap_input(f1)
  expect_equal(f2, f1)
})
