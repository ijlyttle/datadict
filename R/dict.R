library(dplyr)
library(tidyr)
library(assertthat)
library(digest)

#' make_entries_data
#' 
#' sample text
#'
#' @param data        data.frame to be described
#'
#' @return data.frame
#' @export
#' @import dplyr
#' @import digest
#' @import assertthat
#' @import tidyr
make_entries_data <- function(data){
  
  # assert that data is a data.frame
  assert_that(is.data.frame(data))

  dict <- 
    data.frame(
      name_col = colnames(data),
      num_col = seq(1, ncol(data)),
      class = vapply(data, class, ""),
      md5_hash = vapply(data, digest::digest, "", algo = "md5"),
      stringsAsFactors = FALSE
    )
 
  dict
  
}

#' make_entries_dict
#' 
#' sample_text
#'
#' @param dict   data.frame with dictionary entries, must have column name_col
#' @param data   data.frame to be described
#'
#' @return data.frame
#' @export
make_entries_dict <- function(dict, data){
  
  # assert that data, dict are data.frames, df has a column "name_col"
  assert_that(is.data.frame(data))
  assert_that(is.data.frame(dict))
  assert_that(has_name(dict, "name_col"))
  
  name_col_data <- colnames(data)
  name_col_df <- dict$name_col
  
  name_col_df_good <- name_col_df[(name_col_df %in% name_col_data)]
  name_col_df_bad <- name_col_df[!(name_col_df %in% name_col_data)]
  
  if (length(name_col_df_bad) > 0){
    warning("droppping dictionary entries not in data: ", name_col_df_bad)
  }
  
  # select only good colunms
  dict <- dict %>% filter(name_col %in% name_col_df_good)
  
  dict
}

#' trim_entries
#'
#' Trim the entries in a dictionary according to which columns are still in a
#' data-frame. Could be useful after a select.
#' 
trim_entries <- function(dict, data){
  dict <- dict %>%
    filter(name_col %in% colnames(data))
} 

#' get_entries 
#' 
#' print out a dictionary
#' 
#' @param dict data.frame, dictionary 
#' @param by_num logical (default TRUE)
#' @param print_hash logical (default TRUE)
#' 
#' @return data.frame
#' @import dplyr
#' @import tidyr
#' @export
get_entries <- function(dict, by_num = TRUE, print_hash = TRUE){
  
  if (by_num) {
    dict <- dict %>% arrange(num_col)
  } else {
    dict <- dict %>% arrange(name_col)
  }
  
  if (!print_hash){
    dict <- dict %>% select(-md5_hash)
  }
  
  dict
}
  
  

#' dict_validate
#'

#' dict_update_hash
#' 