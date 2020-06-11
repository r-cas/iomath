single_compare <- function(reply, 
                           answer, 
                           compare_grid = NULL, 
                           replace_comma = TRUE, 
                           ans_tol = 0.01, 
                           insert_products = TRUE,
                           allowed_functions = getGroupMembers("Math")) {
  
  stopifnot(inherits(answer, "yac_symbol") | inherits(answer, "caracas_symbol"))
  
  answer_expr <- if (inherits(answer, "yac_symbol")) {
    if (!requireNamespace("Ryacas", quietly = TRUE)) {
      stop("Ryacas is not available")
    }
    
    Ryacas::as_r(answer)
  } else if (inherits(x, "caracas_symbol")) {
    if (!requireNamespace("caracas", quietly = TRUE)) {
      stop("caracas is not available")
    }
    
    caracas::as_r(answer)
  }
  
  # Expect no variables
  if (is.null(compare_grid)) {
    if (length(all.vars(answer_expr)) > 0L) {
      stop("Variables are present, please include grid values")
    }
    
    val_answer <- eval(answer_expr)
    val_reply <- safe_eval(reply, 
                           replace_comma = replace_comma, 
                           insert_products = insert_products,
                           allowed_functions = allowed_functions
                           )

    if (abs(val_answer - val_reply) > ans_tol) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # compare grid is not NULL
  for (i in seq_len(nrow(compare_grid))) {
    vars <- compare_grid[i, , drop = FALSE]
    
    # Must be first to catch unprovided variables (checked in safe_eval())
    val_reply <- safe_eval(reply, 
                           vars = vars, 
                           replace_comma = replace_comma, 
                           insert_products = insert_products,
                           allowed_functions = allowed_functions)

    val_answer <- eval(answer_expr, envir = vars)
    
    if (abs(val_answer - val_reply) > ans_tol) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' Compare a reply to an answer
#' 
#' The comparison is done via a numerical grid.
#' 
#' @param reply user-provided reply as string/character matrix (will be evaluated safely)
#' @param answer the true answer provided as an `R` expression (or list of list of expressions; first level are columns, then rows)
#' @param compare_grid grid to perform comparisons on; must have columns 
#' @param ans_tol tolerance for comparisons
#' @inheritParams safe_eval
#' 
#' @examples 
#' compare_reply_answer(reply = "2yx^2", 
#'                      answer = expression("2*y*x^2"), 
#'                      compare_grid = expand.grid(x = seq(-10, 10, len = 10), 
#'                                                 y = seq(-10, 10, len = 10)))
#' compare_reply_answer(reply = "2.1yx^2", 
#'                      answer = expression("2*y*x^2"), 
#'                      compare_grid = expand.grid(x = seq(-10, 10, len = 10), 
#'                                                 y = seq(-10, 10, len = 10)))
#' compare_reply_answer(reply = "2yx^2+0.01", 
#'                      answer = expression("2*y*x^2"), 
#'                      compare_grid = expand.grid(x = seq(-10, 10, len = 10), 
#'                                                 y = seq(-10, 10, len = 10)), 
#'                      ans_tol = 1)
#'                      
#' rep <- matrix(c("(1+1)*x", "3*x"), nrow = 1, ncol = 2)
#' ans <- list(list(expression(2*x)), list(expression(3*x)))
#' compare_reply_answer(reply = rep,
#'                      answer = ans,
#'                      compare_grid = expand.grid(x = seq(-10, 10, len = 10)),
#'                      ans_tol = 1)
#' @export
compare_reply_answer <- function(reply, 
                                 answer, 
                                 compare_grid = NULL, 
                                 replace_comma = TRUE, 
                                 ans_tol = 0.01, 
                                 insert_products = TRUE,
                                 allowed_functions = getGroupMembers("Math")) {
  
  stopifnot(inherits(answer, "yac_symbol") | inherits(answer, "caracas_symbol"))
  
  answer_dims <- dim(answer)
  
  if (!isTRUE(all.equal(dim(reply), answer_dims))) {
    return(FALSE)
  }
  
  if (is.null(answer_dims)) {
    # Single 
    
    res <- single_compare(reply = reply,
                          answer = answer, 
                          compare_grid = compare_grid, 
                          replace_comma = replace_comma, 
                          ans_tol = ans_tol, 
                          insert_products = insert_products,
                          allowed_functions = allowed_functions)
    return(res)
  } 
  
  # Else:
  # Matrix:
  # entry-wise comparison
  for (row in seq_len(nrow(reply))) {
    for (col in seq_len(ncol(reply))) {
      entry_reply <- reply[row, col]
      entry_answer <- answer[row, col]
      
      res <- single_compare(reply = entry_reply,
                            answer = entry_answer, 
                            compare_grid = compare_grid, 
                            replace_comma = replace_comma, 
                            ans_tol = ans_tol, 
                            insert_products = insert_products,
                            allowed_functions = allowed_functions)
      
      if (res == FALSE) {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}