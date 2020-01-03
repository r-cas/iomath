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
