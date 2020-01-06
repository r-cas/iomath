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


get_ranges <- function(patterns, haystack, require_open_bracket = TRUE) {
  ignore_i_ranges_lst <- lapply(patterns, function(y) {
    
    #patn <- paste0("^(^|.*[^A-Za-z]+)(", y, ")\\(") # start or something else than characters
    
    patn <- if (require_open_bracket) {
      paste0("^(^|.*[^A-Za-z]+)(", y, ")(\\(.*)") # start or something else than characters
    } else {
      paste0("^(^|.*[^A-Za-z]+)(", y, ")($|[^A-Za-z]+.*)") 
    }
    
    res <- regexec(patn, haystack)
    res <- res[[1L]] 
    
    if (length(res) == 1L && res == -1L) {
      return(NULL)
    }
    
    if (length(res) != 4L) {
      return(NULL)
    }
    
    return(res)
  })
  
  # ignore_i_ranges_lst[allowed_functions %in% c("cos", "sin")]
  ignore_i_start <- unlist(lapply(ignore_i_ranges_lst, function(y) {
    if (is.null(y)) {
      return(NULL)
    }
    
    #return(as.integer(y))
    return(as.integer(y[3L]))
  }))
  # ignore_i_start
  
  ignore_i_body <- unlist(lapply(ignore_i_ranges_lst, function(y) {
    if (is.null(y)) {
      return(NULL)
    }
    
    #return(seq(y + 1L, y + attr(y, "match.length") - 1L))
    return(seq(y[3L] + 1L, y[3L] + attr(y, "match.length")[3L] - 1L))
  }))
  
  return(list(start = ignore_i_start,
              body = ignore_i_body))
  #ignore_i_body
}

#' @importFrom stringi stri_sub
insert_product_advanced <- function(x, 
                                    allowed_functions = getGroupMembers("Math"),
                                    allowed_constants = c("pi")
                                    ) {
  # x <- "x(2+1) + sin(y)"
  # x <- "sin(2+1) + x(y+1)"
  # x <- "2x"
  
  n <- nchar(x)
  
  if (n <= 1L) {
    return(x)
  }
  
  x <- gsub(" ", "", x, fixed = TRUE)
  
  rng_fnc <- get_ranges(allowed_functions, x, require_open_bracket = TRUE)
  rng_fnc
  rng_cnst <- get_ranges(allowed_constants, x, require_open_bracket = FALSE)
  rng_cnst
  
  i <- 2L
  
  # Cannot be for-loop as n may change
  while (i <= n) {
    # i is hitting a known function/constant ('in' in 'sin' / 'i' in 'pi')
    if (i %in% rng_fnc$body ||
        i %in% rng_cnst$body) {
      i <- i + 1L
      next
    }
     
    char <- stringi::stri_sub(x, from = i, to = i)

    if (
        # Known function start
        i %in% rng_fnc$start ||
        
        # Known constant start
        i %in% rng_cnst$start ||
        
        # Variable
        grepl("^[A-Za-z]$", char) || 
        
        # opening bracket
        (grepl("^[(]$", char) && 
           !((i-1) %in% rng_fnc$start) && !((i-1) %in% rng_fnc$body)) # bracket for function
        ) {
      prevchar <- stringi::stri_sub(x, from = i-1L, to = i-1L) 
      
      # Note that '.' is included: 4.x === 4.*x (4.0 * x)
      if (grepl("^[A-Za-z0-9.)]$", prevchar)) {
        stringi::stri_sub(x, from = i, to = i) <- paste0("*", char)
        n <- n + 1L
        
        rng_fnc$start[rng_fnc$start > i] <- rng_fnc$start[rng_fnc$start > i] + 1L
        rng_fnc$body[rng_fnc$body > i] <- rng_fnc$body[rng_fnc$body > i] + 1L
        
        rng_cnst$start[rng_cnst$start > i] <- rng_cnst$start[rng_cnst$start > i] + 1L
        rng_cnst$body[rng_cnst$body > i] <- rng_cnst$body[rng_cnst$body > i] + 1L
      }
    }
    
    i <- i + 1L
  }
  
  return(x)
}

#' Make implicit products explicit
#' 
#' @param x user input
#' @param allowed_functions functions (sequence of characters) that should not have products, e.g. `cos`, `sin`
#' 
#' @examples 
#' make_products_explicit("2x")
#' make_products_explicit("2(1+2)")
#' make_products_explicit("x(y)")
#' make_products_explicit("(1+2)3") # Not allowed by definition
#' make_products_explicit("(1+2)x") # Is allowed
#' make_products_explicit("x(2+1) + sin(y)")
#' make_products_explicit("sin(2+1) + x(y+1)")
#' make_products_explicit("(2x)(y+1)")
#' 
#' @export
make_products_explicit <- function(x, 
                                   allowed_functions = getGroupMembers("Math"),
                                   allowed_constants = c("pi")) {
  
  return(insert_product_advanced(x = x, 
                                 allowed_functions = allowed_functions, 
                                 allowed_constants = allowed_constants))
  
}
