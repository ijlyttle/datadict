library(dplyr)
library(tidyr)
library(digest)

#' make_dict_entries
#' 
#' generate dictionary entries 
#' 
#' returns a data-frame with n + 2 observations of three variables: name_col, attribute, value,  
#' where n is the number of additional arguments
#'
#' @param data        data.frame to be described
#' @param name_col    string, column name  
#' @param ...         additional entries, i.e. key = "value"
#'
#' @return data.frame
#' @import digest
#' @export
#'  
make_dict_entries <- function(data, name_col, ...){
  
  manual_entries <- list(...)
  
  # validate name_col
  if (!(name_col %in% colnames(data)))
    stop(paste0(name_col, "is not a column in", data, collapse = " "))

  # num_col 
  num_col <- which(colnames(data) == name_col)
  
  vec <- data[[name_col]]
  
  data.frame(
    name_col = rep(name_col, length(manual_entries) + 2),
    num_col = rep(num_col, length(manual_entries) + 2),
    attribute = c("class", "md5_hash", names(manual_entries)),
    value = c(
      class(vec)[[1]],
      digest::digest(vec, algo = "md5"),
      as.character(manual_entries)
    ),
    stringsAsFactors = FALSE
  )
  
}

#' add_dict_entries
#' 
#' generate dictionary entries 
#' 
#' returns a data-frame with n + 2 observations of three variables: name, attribute, value,  
#' where n is the number of additional arguments
#'
#' @param dict        data.frame to be added-to
#' @param data        data.frame to be described
#' @param name_col    string, column name  
#' @param ...         additional entries, i.e. key = "value"
#'
#' @return data.frame
#' @export
#' 
add_dict_entries <- function(dict, data, name_col, ...){
  
  # bind in new entries
  dict <- dict %>%
    rbind(make_dict_entries(data, name_col, ...))
    
  dict
}


#' get_dict_wide 
#' 
#' print out a dictionary
#' 
#' @param dict
#' @param by_num logical (default TRUE)
#' @param print_hash logical (default TRUE)
#' 
#' @return data.frame
#' @import dplyr
#' @import tidyr
#' @export
get_dict_wide <- function(dict, by_num = TRUE, print_hash = TRUE){
  
  dict_wide <- dict %>% spread(attribute, value)
    
  if (by_num) {
    dict_wide <- dict_wide %>% arrange(num_col)
  } else {
    dict_wide <- dict_wide %>% arrange(name_col)
  }
  
  if (!print_hash){
    dict_wide <- dict_wide %>% select(-md5_hash)
  }
  
  dict_wide
}
  
  

#' dict_validate
#'

#' dict_update_hash
#' 