message("level_to_probs works")
expect_error(summrt:::level_to_probs(c(.1, .9)))
expect_error(summrt:::level_to_probs(2))
expect_equal(summrt:::level_to_probs(.9)$lower, .05)
expect_equal(summrt:::level_to_probs(.9)$upper, .95)

message("probs_to_char works")
expect_error(summrt:::probs_to_char(c(.1, .9)))
expect_error(summrt:::probs_to_char(2))
expect_error(summrt:::probs_to_char(.5, digits = -1))
expect_error(summrt:::probs_to_char(.5, remove_trailing_zero = 2))
expect_error(summrt:::probs_to_char(.5, remove_trailing_zero = c(TRUE, FALSE)))
expect_error(summrt:::probs_to_char(.5, add_leading_zero = 2))
expect_error(summrt:::probs_to_char(.5, add_leading_zero = c(TRUE, FALSE)))
expect_identical(summrt:::probs_to_char(0.05), "0.05")
expect_identical(summrt:::probs_to_char(0.05, digits = 4L), "0.05")
expect_identical(summrt:::probs_to_char(0.005, digits = 4L), "0.005")


