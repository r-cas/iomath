#' @importFrom Ryacas as_r
single_compare <- function(reply, 
                           answer, 
                           compare_grid = NULL, 
                           replace_comma = TRUE, 
                           ans_tol = 0.01) {
  
  stopifnot(is(answer, "yac_symbol"))
  
  answer_expr <- Ryacas::as_r(answer)
  
  # Expect no variables
  if (is.null(compare_grid)) {
    val_answer <- eval(answer_expr)
    val_reply <- safe_eval(reply)
    
    if (abs(val_answer - val_reply) > ans_tol) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # compare grid is not NULL
  for (i in seq_len(nrow(compare_grid))) {
    vars <- compare_grid[i, , drop = FALSE]
    
    val_answer <- eval(answer_expr, envir = vars)
    val_reply <- safe_eval(reply, vars = vars)

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
#' @param reply user-provided reply as string
#' @param answer the true answer provided as `yac_symbol`
#' @param compare_grid grid to perform comparisons on; must have columns 
#' @param ans_tol tolerance for comparisons
#' @inheritParams safe_eval
#' 
#' @export
compare_reply_answer <- function(reply, 
                                 answer, 
                                 compare_grid = NULL, 
                                 replace_comma = TRUE, 
                                 ans_tol = 0.01) {
  
  stopifnot(is(answer, "yac_symbol"))
  
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
                          ans_tol = ans_tol)
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
                            ans_tol = ans_tol)
      
      if (res == FALSE) {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}