#
# This test file has been generated by kwb.test::create_test_files()
# launched by user mrustl on 2021-02-23 15:12:57.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("simulate_treatment_lean() works", {

  f <- kwb.qmra:::simulate_treatment_lean

  expect_error(
    f()
    # Argument "config" fehlt (ohne Standardwert)
  )

})
