pretty_print_large_number <- function(num) {
  num_nondec_digits <- floor(log10(num)) + 1
  ind <- floor((num_nondec_digits-1)/3)
  
  a <- ifelse(num_nondec_digits < 1,
              signif(num, digits = 3),
              ifelse(num_nondec_digits < 4,
                     round(num, digits = 2),
                     round(num/(10^(ind*3)), digits = 2))
  )

  return(paste(a, get_suffix(num), sep = ""))
  
}

get_suffix <- function(num) {
  num_nondec_digits <- floor(log10(num)) + 1
  ind <- floor((num_nondec_digits-1)/3)
  suff <- c("", "k", "m", "b")[ind+1]
  return(suff)
}