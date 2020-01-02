test_that("compare input", {
  expect_error(compare_reply_answer(reply = "2.2", answer = "2.2")) # yac_symbol
})

test_that("compare simple", {
  expect_true(compare_reply_answer(reply = "2.2", 
                                   answer = Ryacas::ysym("2.2"), 
                                   ans_tol = 0.01))
})

test_that("compare ans_tol", {
  expect_true(compare_reply_answer(reply = "2.21", 
                                   answer = Ryacas::ysym("2.2"), 
                                   ans_tol = 0.01))
  
  expect_false(compare_reply_answer(reply = "2.21", 
                                   answer = Ryacas::ysym("2.2"), 
                                   ans_tol = 0.001))
})

test_that("compare single (with x)", {
  expect_true(compare_reply_answer(reply = "2.2*x^2", 
                                   answer = Ryacas::ysym("2.2*x^2"), 
                                   compare_grid = expand.grid(x = seq(-10, 10, length.out = 10))))
  expect_false(compare_reply_answer(reply = "2.3*x^2", 
                                    answer = Ryacas::ysym("2.2*x^2"), 
                                    compare_grid = expand.grid(x = seq(-10, 10, length.out = 10))))
})

test_that("compare single (with x/y/z)", {
  expect_true(compare_reply_answer(reply = "2.2*x^2 + y^3 + z", 
                                   answer = Ryacas::ysym("2.2*x^2 + y^3 + z"), 
                                   compare_grid = expand.grid(x = seq(-10, 10, length.out = 10),
                                                              y = seq(-10, 10, length.out = 10),
                                                              z = seq(-10, 10, length.out = 10))))
  
  expect_false(compare_reply_answer(reply = "2.2*x^2 + 1.1*y^3 + z", 
                                   answer = Ryacas::ysym("2.2*x^2 + y^3 + z"), 
                                   compare_grid = expand.grid(x = seq(-10, 10, length.out = 10),
                                                              y = seq(-10, 10, length.out = 10),
                                                              z = seq(-10, 10, length.out = 10))))
})
  

test_that("compare matrix (with x)", {
  m <- matrix(c("2", "2*x", "3", "1.1*x^2"), 2, 2)
  y <- Ryacas::ysym(m)
  
  expect_true(compare_reply_answer(reply = m, 
                                   answer = y, 
                                   compare_grid = expand.grid(x = seq(-10, 10, length.out = 5))))
  
  m2 <- apply(m, 2, paste0, '+0.0001')
  expect_true(compare_reply_answer(reply = m2, 
                                   answer = y, 
                                   compare_grid = expand.grid(x = seq(-10, 10, length.out = 5))))
  
  m2 <- apply(m, 2, paste0, '+0.01')
  expect_false(compare_reply_answer(reply = m2, 
                                    answer = y, 
                                    compare_grid = expand.grid(x = seq(-10, 10, length.out = 5))))
})

test_that("compare single (with x/y/z)", {
  m <- matrix(c("2+z^3", "2*sin(x)", "3*y^2", "1.1*x^2"), 2, 2)
  y <- Ryacas::ysym(m)
  grd <- expand.grid(x = seq(-10, 10, length.out = 5),
                     y = seq(-10, 10, length.out = 5),
                     z = seq(-10, 10, length.out = 5))
  
  expect_true(compare_reply_answer(reply = m, 
                                   answer = y, 
                                   compare_grid = grd))
  
  m2 <- apply(m, 2, paste0, '+0.0001')
  expect_true(compare_reply_answer(reply = m2, 
                                   answer = y, 
                                   compare_grid = grd))
  
  m2 <- apply(m, 2, paste0, '+0.01')
  expect_false(compare_reply_answer(reply = m2, 
                                    answer = y, 
                                    compare_grid = grd))
})

