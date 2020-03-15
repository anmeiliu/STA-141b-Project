pretty_print_large_number <- function(num) {
  string <- ifelse(num < 1000, 
         format(num, digits = 3),
         ifelse(num < 1000000, 
                paste(format(num/1000, trim = TRUE, digits = 2), "k", sep = ""),
                ifelse(num < 1000000000, 
                       paste(format(num/1000000, trim = TRUE, digits = 2), "m", sep = ""),
                       ifelse(num < 1000000000000,
                              paste(format(num/1000000000, trim = TRUE, digits = 2), "b", sep = ""),
                              NA
                       )
                )
         )
  )
  return(trimws(string))
}
