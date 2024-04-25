# test_that("replace_seq works", {
#
#   expect_equal(
#     object = replace_seq(seq(1000, 7000, 1000)),
#     expected = c("1,000", "", "3,000", "", "5,000", "", "7,000")
#   )
#
#   expect_equal(
#     object = replace_seq(seq(1000, 7000, 1000), big.mark = " "),
#     expected = c("1 000", "", "3 000", "", "5 000", "", "7 000")
#   )
#
#   expect_equal(
#     object = replace_seq(seq(1000, 7000, 1000), replacement = "_"),
#     expected = c("1,000", "_", "3,000", "_", "5,000", "_", "7,000")
#   )
#
#   expect_equal(
#     object = replace_seq(seq(1000, 7000, 1000), keep_nth = 3),
#     expected = c("1,000", "", "", "4,000", "", "", "7,000")
#   )
#
#   expect_equal(
#     object = replace_seq(seq(1000, 7000, 1000), offset = -1),
#     expected = c("", "2,000", "", "4,000", "", "6,000", "")
#   )
#
#   expect_equal(
#     object = replace_seq(seq(1000, 7000, 1000), big.mark = " ", keep_nth = 3),
#     expected = c("1 000", "", "", "4 000", "", "", "7 000")
#   )
#
#   expect_equal(
#     object = replace_seq(seq(1000, 7000, 1000), big.mark = " ", offset = -1),
#     expected = c("", "2 000", "", "4 000", "", "6 000", "")
#   )
#
#   expect_equal(
#     object = replace_seq(seq(1000, 7000, 1000), big.mark = " ", keep_nth = 3, offset = -1),
#     expected = c("", "", "3 000", "", "", "6 000", "")
#   )
#
#   expect_equal(
#     object = replace_seq(LETTERS[1:12]),
#     expected = c("A", "", "C", "", "E", "", "G", "", "I", "", "K", "")
#   )
#
#   expect_equal(
#     object = replace_seq(LETTERS[1:12], keep_nth = 3),
#     expected = c("A", "", "", "D", "", "", "G", "", "", "J", "", "")
#   )
#
#   expect_equal(
#     object = replace_seq(LETTERS[1:12], offset = -1),
#     expected = c("", "B", "", "D", "", "F", "", "H", "", "J", "", "L")
#   )
#
# })
