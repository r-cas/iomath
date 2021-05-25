test_that("smoke", {
  expect_equal(2 * 2, safe_eval("2 * 2"))
  expect_equal(4.4, safe_eval("x", vars = list(x = 4.4)))
})

test_that("pi", {
  expect_equal(2 * pi, safe_eval("2 * pi"))
  expect_error(safe_eval("pipi"))
})

test_that("comma", {
  expect_equal(2.2, safe_eval("2,2"))
  expect_error(safe_eval("2,2", replace_comma = FALSE))
})

test_that("variables", {
  expect_equal(2.2*4.2, safe_eval("2,2*x", list(x = 4.2)))
  expect_error(safe_eval("x")) # x has no value
})

test_that("function", {
  expect_error(safe_eval("function(x) 4")) # 'function' not found
})

test_that("vector input fails", {
  expect_error(safe_eval(c("2", "4*x"), list(x = 4)))
})

test_that("multiple statements ignored", {
  expect_equal(2.2, safe_eval("2.2; 4"))
})

test_that("cannot print", {
  expect_error(safe_eval("cat(2.2)"))
  expect_error(safe_eval("print(2.2)"))
})

test_that("repeated function calls", {
    expect_equal(sqrt(1+sqrt(5)), safe_eval("sqrt(1+sqrt(x))", list(x=5)))
    expect_equal(exp(2)+exp(4), safe_eval("exp(2)+exp(4)"))
    expect_error(safe_eval("sqrtsqrt(5)"))
})


test_that("Advanved input", {
  input <- "2*x + sqrt(4*y^2) + sin(z)"
  expr <- parse(text = input)
  
  vals <- expand.grid(x = c(-2, 1, 3, 4.25), 
                      y = c(-2, 1, 3, 4.25), 
                      z = c(-2, 1, 3, 4.25))
  
  for (i in seq_len(nrow(vals))) {
    vars <- list(x = vals$x[i], 
                 y = vals$y[i],
                 z = vals$z[i])
    
    expect_equal(eval(expr, vars), safe_eval(input, vars), 
                 info = paste0("i = ", i, "; ", 
                               paste0(names(vars), " = ", vars, collapse = ", ")))
  }
})

