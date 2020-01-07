# Based on:
# https://stackoverflow.com/questions/18369913/safely-evaluating-arithmetic-expressions-in-r

safe_constants <- c(
  "pi"
)

#' Get safe constants
#' 
#' @export
get_safe_constants <- function() {
  x <- safe_constants
  return(x)
}

safe_functions <- c(
  "(",
  getGroupMembers("Math"),
  getGroupMembers("Arith")
)

get_safe_env_copy <- function() {
  safe_env <- new.env(parent = emptyenv())
  
  for (f in safe_constants) {
    safe_env[[f]] <- get(f, "package:base")
  }
  
  for (f in safe_functions) {
    safe_env[[f]] <- get(f, "package:base")
  }
  
  new_safe_env <- as.environment(as.list(safe_env, all.names = TRUE))
  
  return(new_safe_env)
}

#' Prepare input
#' 
#' @export
prepare_input <- function(x, 
                      replace_comma = TRUE,
                      insert_products = TRUE,
                      allowed_functions = getGroupMembers("Math"),
                      allowed_constants = c("pi")) {
  
  # Remove everything after ";"
  x2 <- gsub("^([^;]*).*$", "\\1", x)
  
  x2 <- tolower(x2)
  
  if (replace_comma) {
    x2 <- gsub(",", ".", x2, fixed = TRUE)
  }
  
  if (insert_products) {
    x2 <- make_products_explicit(x = x2, 
                                 allowed_functions = allowed_functions,
                                 allowed_constants = allowed_constants)
  }
  
  return(x2)
}

#' Safely eval a string as an expression
#' 
#' @param x string to evaluate
#' @param vars list with variable values
#' @param replace_comma Replace ',' with '.' before parsing
#' @param insert_products Make implicit products explicit
#' @inheritParams make_products_explicit
#' 
#' @examples 
#' safe_eval("2+2")
#' 
#' @export
safe_eval <- function(x, 
                      vars = NULL, 
                      replace_comma = TRUE,
                      insert_products = TRUE,
                      allowed_functions = getGroupMembers("Math"),
                      allowed_constants = c("pi")
                      ) {
  
  stopifnot(!is.null(x))
  stopifnot(is.character(x))
  stopifnot(length(x) == 1L)
  
  new_safe_env <- get_safe_env_copy()

  # Assign variables values:
  vars_names <- names(vars)
  for (var_i in seq_along(vars)) {
    new_safe_env[[ vars_names[var_i] ]] <- vars[[var_i]]
  }
  
  x2 <- prepare_input(x, 
                      replace_comma = replace_comma,
                      insert_products = insert_products,
                      allowed_functions = allowed_functions,
                      allowed_constants = allowed_constants)
  
  y <- parse(text = x2)
  y_vars <- all.vars(y)
  y_vars <- setdiff(y_vars, safe_constants)
  
  # Ensure that no new variables enter expression:
  unknown_vars <- setdiff(y_vars, vars_names)
  if (length(unknown_vars) > 0L) {
    msg <- paste0("Unknown variables in expression: ", 
                  paste0("'", unknown_vars, "'", collapse = ", "))
    stop(msg)
  }
  
  res <- eval(substitute(y), env = new_safe_env)
  return(res)
}
