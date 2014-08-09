context("Make entries")
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

not_data_frame <- "not data frame"
data_test <- data.frame(a = 1, b = "test", stringsAsFactors = FALSE)
dict_test <- data.frame(
  name_col = c("a", "b"),
  name_long = c("letter a", "letter b"),
  stringsAsFactors = FALSE
) 

test_that("make entries using data", {
  dict_wide <- 
    make_entries_data(data_test) 

  expect_error(.make_dict_entries_from_data(not_data_frame))
  expect_equal(dict_wide$name_col, c("a", "b"))
  expect_equal(dict_wide$num_col, c(1, 2))
  expect_equal(dict_wide$class, c("numeric", "character"))
  expect_equal(
    dict_wide$md5_hash,
    c("6717f2823d3202449301145073ab8719", "06195aa345d5f8889bc7de7dfef92e30")
  )
})

test_that("make entries using data-frame", {
  dict_wide <- 
    make_entries_dict(dict_test,data_test) 
  
  dict_test_bad <- dict_test
  names(dict_test_bad)[[1]] <- "name_cols" # bad column-name
  
  dict_test_warn <- dict_test %>%
    rbind(data.frame(name_col = "d", name_long = "letter d", stringsAsFactors = FALSE))
  
  expect_error(.make_dict_entries_df(data_test, not_data_frame))
  expect_error(.make_dict_entries_df(not_data_frame, dict_test))
  expect_error(.make_dict_entries_df(data_test, dict_test_bad))
  
  expect_warning(
    dict_warn <- make_entries_dict(dict_test_warn, data_test) 
  )
  
  expect_identical(dict_test, dict_wide)
  expect_identical(dict_warn, dict_wide)
})


