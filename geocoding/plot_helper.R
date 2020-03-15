pretty_print_large_number <- function(num) {
  if (num < 1000) {
    string <- format(num, digits = 3)
  } else if (num < 1000000) {
    string <- paste(format(num/1000, digits = 3), "k", sep = "")
  } else if (num < 1000000000) {
    string <- paste(format(num/1000000, digits = 3), "m", sep = "")
  } else if (num < 1000000000000) {
    string <- paste(format(num/1000000000, digits = 3), "b", sep = "")
  } else {
    string <- "nani"
  }
  return(string)
}
