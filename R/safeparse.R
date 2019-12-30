# Based on:
# https://stackoverflow.com/questions/18369913/safely-evaluating-arithmetic-expressions-in-r

get_safe_env_copy <- function() {
  safe_functions <- c(
    getGroupMembers("Math"),
    getGroupMembers("Arith"),
    #  getGroupMembers("Compare"),
    #"<-", "{", "("
    "("
  )
  
  safe_env <- new.env(parent = emptyenv())
  
  for (f in safe_functions) {
    safe_env[[f]] <- get(f, "package:base")
  }
  
  new_safe_env <- as.environment(as.list(safe_env, all.names = TRUE))
  return(new_safe_env)
}

#' Safely eval a string as an expression
#' 
#' @param x string to evaluate
#' @param vars list with variable values
#' @param replace_comma Replace ',' with '.' before parsing
#' 
#' @examples 
#' safe_eval("2+2")
#' 
#' @export
safe_eval <- function(x, vars = NULL, replace_comma = TRUE) {
  stopifnot(!is.null(x))
  stopifnot(is.character(x))
  stopifnot(length(x) == 1L)
  
  new_safe_env <- get_safe_env_copy()

  # Assign variables values:
  vars_names <- names(vars)
  for (var_i in seq_along(vars)) {
    new_safe_env[[ vars_names[var_i] ]] <- vars[[var_i]]
  }
  
  # Remove everything after ";"
  x2 <- gsub("^([^;]*).*$", "\\1", x)
  
  x2 <- tolower(x2)
  
  if (replace_comma) {
    x2 <- gsub(",", ".", x2, fixed = TRUE)
  }
  
  y <- parse(text = x2)
  y_vars <- all.vars(y)
  
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
