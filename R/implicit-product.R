# https://stackoverflow.com/questions/11232801/regex-split-numbers-and-letter-groups-without-spaces
# https://stackoverflow.com/questions/9756360/split-character-data-into-numbers-and-letters
# https://www.regular-expressions.info/lookaround.html
insert_product_regex <- function(x) {
  
  re_numchar <- "(?=[A-Za-z])(?<=[0-9.])" ## Only leading numbers with tailing chars
  re_char <- "(?=[A-Za-z])(?<=[A-Za-z])|(?=[A-Za-z])(?<=[A-Za-z])"
  
  y <- x
  y <- gsub(re_char, "\\1*\\2", y, perl = TRUE)
  y <- gsub(re_numchar, "\\1*\\2", y, perl = TRUE)
  
  return(y)
}

#' @importFrom stringi stri_sub
insert_product_simple <- function(x) {
  n <- nchar(x)
  
  if (n <= 1L) {
    return(x)
  }
  
  i <- 2L
  
  # Cannot be for-loop as n may change
  while (i <= n) {
    # Pseudo code:
    # if (char[i] is a variable) {
    #    if (char[i-1] is a number [0-9.] or a variable [A-Za-z]) {
    #      insert "*" between pos i-1 and i
    #    }
    # }
    
    char <- stringi::stri_sub(x, from = i, to = i)
    
    # Variable
    if (grepl("^[A-Za-z]$", char)) {
      prevchar <- stringi::stri_sub(x, from = i-1L, to = i-1L) 
      
      # Note that '.' is included: 4.x === 4.*x (4.0 * x)
      if (grepl("^[A-Za-z0-9.]$", prevchar)) {
        stringi::stri_sub(x, from = i, to = i) <- paste0("*", char)
        n <- n + 1L
      }
    }
    
    i <- i + 1L
  }
  
  return(x)
}



#' @importFrom stringi stri_sub
insert_product_advanced <- function(x, 
                                    allowed_functions = getGroupMembers("Math")) {
  # x <- "x(2+1) + sin(y)"
  # x <- "sin(2+1) + x(y+1)"
  # x <- "2x"
  
  n <- nchar(x)
  
  if (n <= 1L) {
    return(x)
  }
  
  x <- gsub(" ", "", x, fixed = TRUE)
  
  ignore_i_ranges_lst <- lapply(allowed_functions, function(y) {
    res <- gregexpr(y, x, fixed = TRUE)[[1L]]
    
    if (res == -1L) {
      return(NULL)
    }
    
    return(res)
  })
  ignore_i_start <- unlist(lapply(ignore_i_ranges_lst, function(y) {
    if (is.null(y)) {
      return(NULL)
    }
    
    return(as.integer(y))
  }))
  ignore_i_body <- unlist(lapply(ignore_i_ranges_lst, function(y) {
    if (is.null(y)) {
      return(NULL)
    }
    
    return(seq(y + 1L, y + attr(y, "match.length") - 1L))
  }))
  
  i <- 2L
  
  # Cannot be for-loop as n may change
  while (i <= n) {
    # i is hitting a known function ('in' in 'sin')
    if (i %in% ignore_i_body) {
      i <- i + 1L
      next
    }
     
    char <- stringi::stri_sub(x, from = i, to = i)

    if (
        # Known function start
        i %in% ignore_i_start ||
        
        # Variable
        grepl("^[A-Za-z]$", char) || 
        
        # opening bracket
        (grepl("^[(]$", char) && 
           !((i-1) %in% ignore_i_start) && !((i-1) %in% ignore_i_body)) # bracket for function
        ) {
      prevchar <- stringi::stri_sub(x, from = i-1L, to = i-1L) 
      
      # Note that '.' is included: 4.x === 4.*x (4.0 * x)
      if (grepl("^[A-Za-z0-9.)]$", prevchar)) {
        stringi::stri_sub(x, from = i, to = i) <- paste0("*", char)
        n <- n + 1L
        
        ignore_i_start[ignore_i_start > i] <- ignore_i_start[ignore_i_start > i] + 1L
        ignore_i_body[ignore_i_body > i] <- ignore_i_body[ignore_i_body > i] + 1L
      }
    }
    
    i <- i + 1L
  }
  
  return(x)
}

if (FALSE) {
  insert_product_advanced("2x")
  insert_product_advanced("2(1+2)")
  insert_product_advanced("x(y)")
  insert_product_advanced("(1+2)3") # Not allowed
  insert_product_advanced("x(2+1) + sin(y)")
  insert_product_advanced("sin(2+1) + x(y+1)")
  insert_product_advanced("(2x)(y+1)")
}



