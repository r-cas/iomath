y <- list(
  c("2x", "2*x"),
  c("x2", "x2"), # not convention to insert product, could be x^2, x/2, ...
  c("2+x2*y", "2+x2*y"), # not convention to insert product, could be x^2, x/2, ...
  c("2+2x", "2+2*x"),
  c("3+4*xy", "3+4*x*y"),
  c("3+4xy", "3+4*x*y"), 
  c("3+4*x*y", "3+4*x*y"),
  c("xy", "x*y"), 
  c("-x", "-x"),
  c("xy+4", "x*y+4"), 
  c("3+4*xy+3z", "3+4*x*y+3*z"),
  c("3+4xy+3zw", "3+4*x*y+3*z*w"),
  c("3+4*x*y+3zw", "3+4*x*y+3*z*w"),
  c("4.x", "4.*x")
  )

test_that("insert_product_regex", {
  for (i in seq_along(y)) {
    expect_equal(y[[i]][2L], insert_product_regex(y[[i]][1L]), info = paste0("i = ", i))
  }
})

test_that("insert_product_simple", {
  for (i in seq_along(y)) {
    expect_equal(y[[i]][2L], insert_product_simple(y[[i]][1L]), info = paste0("i = ", i))
  }
})

if (FALSE) {
  microbenchmark::microbenchmark(
    regex = lapply(y, function(z) insert_product_regex(z[1L])),
    simple = lapply(y, function(z) insert_product_simple(z[1L])), 
    times = 10
  )
}







y_adv <- list(
  c("2(1+2)", "2*(1+2)"),
  c("x(y)", "x*(y)"),
  
  c("(1+2)x", "(1+2)*x"), 
  c("(1+2)3", "(1+2)3"), # Not allowed; not conventional to do
  
  c("-(2+1x)", "-(2+1*x)"), 
  c("(1+2)(2+1)", "(1+2)*(2+1)"),
  c("x(2+1)+sin(y)", "x*(2+1)+sin(y)"),
  c("sin(2+1)+x(y+1)", "sin(2+1)+x*(y+1)"),
  c("sin(2x+1)", "sin(2*x+1)"),
  
  c("2pi", "2*pi"),
  c("2pi(2+1)", "2*pi*(2+1)"),
  c("2pi^2", "2*pi^2"),
  
  c("3sin(cos(2x)+1)", "3*sin(cos(2*x)+1)"), # allowed for numbers
  c("xsin(1)", "x*s*i*n*(1)") # not allowed for variables
)

# insert_product_advanced("3sin(cos(2x)+1)")
test_that("insert_product_advanced", {
  for (i in seq_along(y)) {
    expect_equal(y[[i]][2L], insert_product_advanced(y[[i]][1L]), info = paste0("y: i = ", i))
  }
  
  for (i in seq_along(y_adv)) {
    expect_equal(y_adv[[i]][2L], 
                 insert_product_advanced(y_adv[[i]][1L]), info = paste0("y_adv: i = ", i))
  }
})


test_that("insert_product_advanced error", {
  expect_error(compare_reply_answer(reply = "3*x*y", 
                                    answer = Ryacas::ysym("3*x*y")))
  
  expect_error(compare_reply_answer(reply = "3*x*y", 
                                    answer = Ryacas::ysym("3*x*y"), 
                                    compare_grid = expand.grid(x = seq(-10, 10, len = 10))))
  
})

y_numeric <- list(
  c("2x", "2*x"),

  c("2+2x", "2+2*x"),
  c("3+4*xy", "3+4*x*y"),
  c("3+4xy", "3+4*x*y"), 
  c("3+4*x*y", "3+4*x*y"),
  c("xy", "x*y"), 
  c("-x", "-x"),
  c("xy+4", "x*y+4"), 
  c("3+4*xy+3z", "3+4*x*y+3*z"),
  c("3+4xy+3zw", "3+4*x*y+3*z*w"),
  c("3+4*x*y+3zw", "3+4*x*y+3*z*w"),
  c("4.x", "4.*x"),
  
  c("2(1+2)", "2*(1+2)"),
  c("x(y)", "x*(y)"),
  
  c("(1+2)x", "(1+2)*x"), 
  
  c("2pi", "2*pi"),
  c("2pi(2+1)", "2*pi*(2+1)"),
  c("2pi^2", "2*pi^2"),
  
  c("-(2+1x)", "-(2+1*x)"), 
  c("(1+2)(2+1)", "(1+2)*(2+1)"),
  c("x(2+1)+sin(y)", "x*(2+1)+sin(y)"),
  c("sin(2+1)+x(y+1)", "sin(2+1)+x*(y+1)"),
  c("sin(2x+1)", "sin(2*x+1)"),
  
  c("3sin(cos(2x)+1)", "3*sin(cos(2*x)+1)")
)


test_that("insert_product_advanced numeric", {
  for (i in seq_along(y_numeric)) {
    res <- compare_reply_answer(reply = y_numeric[[i]][1L], 
                                answer = Ryacas::ysym(y_numeric[[i]][2L]), 
                                compare_grid = expand.grid(x = seq(-10, 10, len = 2), 
                                                           y = seq(-10, 10, len = 2), 
                                                           z = seq(-10, 10, len = 2),
                                                           w = seq(-10, 10, len = 2)))
    expect_true(res, info = paste0("y: i = ", i))
  }
})
