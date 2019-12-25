test_that("hbtex", {
  A <- Ryacas::ysym(matrix(1:9, 3, 3))
  o <- Ryacas::tex(A)
  
  expect_equal(hbtex(A), gsub("(", "[", fixed = TRUE, 
                              gsub(")", "]", fixed = TRUE, o)))
})


test_that("btex", {
  e <- Ryacas::ysym("((2*Sin(2+x))^2 + (a+b)^2)^3")
  o <- Ryacas::tex(e)
  o2 <- btex(e)

  expect_equal(o2, "\\left\\{\\left[2 \\sin \\left(x + 2\\right)\\right] ^{2} + \\left[a + b\\right] ^{2}\\right\\} ^{3}")
})


