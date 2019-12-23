#' Bracket controlled tex output 
#' 
#' Make brackets to different types.
#' 
#' @param x yac_symbol
#' @param brackets Symbols to use as brackets
#' 
#' @importFrom Ryacas tex
bracketed_tex <- function(x, 
                          brackets = list(
                            c("{", "}"),
                            c("[", "]"),
                            c("(", ")")
                          )) {
  stopifnot(is(x, "yac_symbol"))
  
  o <- Ryacas::tex(x)
  
  stop("TBI")
}

#' Make hard brackets
#' 
#' @inheritParams bracketed_tex
#' 
#' @importFrom Ryacas tex
hbtex <- function(x) {
  stopifnot(is(x, "yac_symbol"))
  
  o <- Ryacas::tex(x)
  o <- gsub("\\left(", "\\left[", o, fixed = TRUE)
  o <- gsub("\\right)", "\\right]", o, fixed = TRUE)

  return(o)
}

