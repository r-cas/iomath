test_that("hbtex", {
  A <- Ryacas::ysym(matrix(1:9, 3, 3))
  o <- Ryacas::tex(A)
  
  expect_equal(hbtex(A), gsub("(", "[", fixed = TRUE, 
                              gsub(")", "]", fixed = TRUE, o)))
})
