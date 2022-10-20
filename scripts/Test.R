library('rcrossref')
library('tibble')
library('lubridate')
library('ggplot2')
library('dplyr')
library('readr')
library('tidyr')


# These next few lines of code create a set of start and end dates
# Then turns these into a list with pairs
from_pub_date <- seq(as.Date("2012-01-01"), length = 130, by = "months")
until_pub_date <- seq(as.Date("2012-02-01"), length = 130, by = "months") - 1
months <- data.frame(from_pub_date = as.character(from_pub_date), until_pub_date = as.character(until_pub_date), stringsAsFactors = FALSE)
xy.list <- split(months, seq(nrow(months)))
xy.list <- setNames(split(months, seq(nrow(months))), rownames(months))
xy.list

totals <- function(...) {
  x <- cr_works(filter = list(...), .progress = "text")
  return(x$meta$total_results)
}


# This function grabs metrics by month
get_data <- function(from_pub_date, until_pub_date) {
  tot = totals(from_pub_date = from_pub_date, until_pub_date = until_pub_date)
  has_funder = totals(from_pub_date = from_pub_date, until_pub_date = until_pub_date, has_funder = TRUE)
  has_license = totals(from_pub_date = from_pub_date, until_pub_date = until_pub_date, has_license = TRUE)
  has_orcid = totals(from_pub_date = from_pub_date, until_pub_date = until_pub_date, has_orcid = TRUE)
  has_abstract = totals(from_pub_date = from_pub_date, until_pub_date = until_pub_date, has_abstract = TRUE)
  tbl_df(data.frame(tot, has_funder, has_license, has_orcid, has_abstract))
}

# -------------------------------------------

results <- lapply(xy.list, function(x) { get_data(x$from_pub_date, x$until_pub_date) })
res <- bind_rows(results)
res$month <- as.Date(months$from_pub_date)
