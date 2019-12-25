# Simple parsing; building AST with stack:
# push/pop at "\\left(" (push) / "\\right)"
# Assumes a valid tex formula
#' @importFrom dequer stack pop push
parse_valid_tex_formula <- function(x) {
  lbs <- gregexpr("\\\\left\\(", x)[[1L]]
  rbs <- gregexpr("\\\\right\\)", x)[[1L]]

  stopifnot(length(lbs) == length(rbs))
  
  bs <- rbind(
    data.frame(pos = as.integer(lbs), 
               len = as.integer(attr(lbs, "match.length")), 
               type = "L"),
    data.frame(pos = as.integer(rbs), 
               len = as.integer(attr(rbs, "match.length")), 
               type = "R")
  )
  bs <- bs[order(bs$pos), ]
  rownames(bs) <- NULL
  bs$mate <- NA
  bs$level <- NA
  bs$pair_no <- NA

  s <- dequer::stack()
  pair_no <- 0L
  
  for (i in seq_len(nrow(bs))) {
    ele <- bs[i, ]

    if (ele$type == "L") {
      dequer::push(s, i)
    } else {
      # type == "R"
      pair_no <- pair_no + 1L
      
      j <- dequer::pop(s)
      
      lvl <- length(s) + 1L
      
      bs$mate[i] <- j
      bs$level[i] <- lvl
      bs$pair_no[i] <- pair_no
      
      bs$mate[j] <- i
      bs$level[j] <- lvl
      bs$pair_no[j] <- pair_no
    }
  }
  
  stopifnot(length(s) == 0L)

  return(bs)
}

#' @importFrom stringi stri_sub
replace_brackets <- function(x, 
                             brackets = list(
                               c("\\{", "\\}"),
                               c("[", "]"),
                               c("(", ")"))) {
  tree <- parse_valid_tex_formula(x)
  tree$repl_b_i <- ((tree$level - 1L) %% length(brackets)) + 1L
  tree$repl <- unlist(lapply(seq_len(nrow(tree)),
                      function(i) {
                        ret <- if (tree$type[[i]] == "L") {
                          return(paste0("\\left", 
                                 brackets[[tree$repl_b_i[i]]][1L]))
                        } else if (tree$type[[i]] == "R") {
                          paste0("\\right" ,
                                 brackets[[tree$repl_b_i[i]]][2L])
                        } else {
                          stop("Unexpected")
                        }
                        
                        return(ret)
                      }))
  tree
  
  y <- x
  
  # Starting from post to avoid adjusting pos in remaining
  for (i in rev(seq_len(nrow(tree)))) {
    # i <- 8L
    ele <- tree[i, ]
    ele
    
    #substring(y, ele$pos, ele$pos + ele$len) <- ele$repl
    stringi::stri_sub(y, ele$pos, ele$pos + ele$len) <- ele$repl
  }
  
  return(y)
}

#' Bracket controlled tex output 
#' 
#' Make brackets to different types.
#' 
#' @param x yac_symbol
#' @param brackets Symbols to use as brackets. Reused as necesary.
#' 
#' @importFrom Ryacas tex
#' @export
btex <- function(x, 
                 brackets = list(
                   c("{", "}"),
                   c("[", "]"),
                   c("(", ")")
                 )) {
  stopifnot(is(x, "yac_symbol"))
  
  o <- Ryacas::tex(x)
  o2 <- replace_brackets(o)
  
  return(o2)
}

#' Make hard brackets
#' 
#' @inheritParams btex
#' 
#' @importFrom Ryacas tex
#' @export
hbtex <- function(x) {
  stopifnot(is(x, "yac_symbol"))
  
  o <- Ryacas::tex(x)
  o <- gsub("\\left(", "\\left[", o, fixed = TRUE)
  o <- gsub("\\right)", "\\right]", o, fixed = TRUE)

  return(o)
}






