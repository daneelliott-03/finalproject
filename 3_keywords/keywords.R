####  KEYWORDS ################################################
library(tidyverse)

source("1_review/reviews.R")
source("2_demographics/demographic_analysis.R")
source("2_demographics/run_analysis.R")


#' @description This function loops through a list of keywords and counts 
#' how many times each one appears in a text column.
#' 
#' @param text_column The column with the reviews
#' @param keywords_list A vector of strings to look for
#' 
#' @return A dataframe showing which words appeared most often.
#' 
#' @note IMPORTANT: Filter the data first! 
#' Only run this on a subset of ratings like NEGATIVE reviews (3 stars or less). 
#' If you run this on 5-star reviews, it may count phrases like "No white cast" 
#' or "Not ashy" as matches, which messes up the results.
count_keywords <- function(text_column, keywords_list) {
  
  # empty placeholders to store our results
  final_words <- c()
  final_counts <- c()
  
  # made the input text all lower 
  clean_text <- str_to_lower(text_column)
  
  # loop through each keyword one by one, counting how mam how many times it 
  # appears in the whole column
  for (k in keywords_list) {
    # need to include \\b so paste doesn't also grab fragments within words
    # e.g., ash in crash
    pattern <- paste0("\\b", k, "\\b")
    # count of the # of time the word appears
    total <- sum(str_count(clean_text, pattern), na.rm = TRUE)
    
    final_words <- c(final_words, k)
    final_counts <- c(final_counts, total)}
  # the collected keyword count pairs are compiled into a data frame
  results <- data.frame(word = final_words, n = final_counts)
  return(results %>% arrange(desc(n)))}


#' @description Run a two-sample t-test comparing mean ratings between
#' two user groups for a selected set of products.
#'
#' @param data A dataframe with at least rating, product_id, and group_col.
#' @param target_ids A vector of product_ids to include.
#' @param group_col A string giving the column name that defines groups
#' @param group_a The reference group label
#' @param group_b The comparison group label
#'
#' @return A one row dataframe with the rating gap (group_b - group_a),
#'   p-value, significance flag, and group means
run_disparity_test <- function(data, target_ids, group_col, group_a, group_b) {
  test_data <- data %>% filter(product_id %in% target_ids,
      .data[[group_col]] %in% c(group_a, group_b))
  
  if (nrow(test_data) < 10) {
    return("not enough data")}
  
  # runs two-sample t test on ratings for group_a vs group_b
  res <- t.test(test_data$rating[test_data[[group_col]] == group_a],
    test_data$rating[test_data[[group_col]] == group_b])
  
  # res$estimate is a length 2 named vector with the mean of each group
  mean_a <- res$estimate[1]
  mean_b <- res$estimate[2]
  
  data.frame(gap = mean_b - mean_a, 
    p_value = res$p.value,
    significant = res$p.value < 0.05,
    mean_group_a = mean_a,
    mean_group_b = mean_b,
    n_total = nrow(test_data))}


#' @description  This function uses a set of keywords to flag products whose
#' reviews mention those terms, and then runs a two sample t test that compares
#' mean ratings between two user groups on just those flagged products.
#' 
#' @param data A dataframe, requires a numeric rating column
#' @param keyword_vec Character vector of keywords/phrases to search for
#' @param category Character vector of secondary_category values to include
#' @param group_col A string specifying the column name to group by
#' @param group_a The reference group
#' @param group_b The comparison group 
#' 
#' @return A dataframe with the gap, p-value, and a significance flag.
run_keyword_disparity <- function(data, keyword_vec, category,
                                  group_col, group_a, group_b) {
  sub <- data %>%
    filter(secondary_category %in% category,
      !is.na(review_text),
      !is.na(.data[[group_col]]))
  # turns the vector of keywords into one big regex pattern
  # using \\b so we only match whole words/phrases
  # e.g. "ashy" but not "flashy"
  pattern <- paste0("\\b(", paste(keyword_vec, collapse = "|"), ")\\b")
  
  # find product_ids where at least one review in this subset
  # mentions any of the keywords
  keyword_ids <- sub %>%
    filter(str_detect(str_to_lower(review_text), pattern)) %>%
    distinct(product_id) %>%
    pull(product_id)
  
  # now can run the t-test helper just on flagged products
  # comparing the two groups passed in
  run_disparity_test(data = sub,
    target_ids = keyword_ids,
    group_col = group_col,
    group_a = group_a,
    group_b = group_b)}
