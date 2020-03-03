library(RSocrata)

# takes a list of params, eg
# list("$select" = "var", "$where" = "var")
# very janky

gen_api_call <- function(params, endpoint = "https://data.cms.gov/resource/97k6-zzx3.json") {
  
  if (!("$order" %in% names(params))) {
    primary_var <- params[["$select"]][1]
    primary_tokens <- unlist(strsplit(primary_var, " "))
    primary_token <- primary_tokens[length(primary_tokens)]
    params[["$order"]] <- primary_token
  }
  
  param_chunks <- ""
  
  for (param in names(params)) {
    param_choices <- paste(params[[param]], collapse = ",")
    param_chunk <- paste(param, param_choices, sep = "=")
    
    if (param_chunks == "") {
      param_chunks <- param_chunk
    } else {
      param_chunks <- paste(param_chunks, param_chunk, sep = "&")
    }
  }
  
  gen_url <- paste(endpoint, param_chunks, sep = "?")
  
  return(URLencode(gen_url))
}

get_api_call <- function(params) {
  app_key <- Sys.getenv("APP_KEY")
  read.socrata(gen_api_call(params), app_key)
}