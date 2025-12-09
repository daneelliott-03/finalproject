####DEMOGRAPHIC ANALYSIS##################################################################
source("1_review/reviews.R")

library(tidyverse)
library(kableExtra)

#' @description
#' Computes summary rating statistics across levels of a demographic variable
#' within one or more grouping variables (e.g., products or categories).
#' @param df
#' @param demographic_var A length 1 character string
#' The demographic variable used to compare (skin type and skin tone)
#' @param grouping_var character vector of one or more column names 
#' @return Data frame.
#' Output with summary statistics for each grouping_var x demographic_var combo

summarize_ratings_by_demographic <- function(df, demographic_var, grouping_var) {
  df_sub <- df[!is.na(df[[demographic_var]]), ]
  df_sub %>% group_by(across(all_of(c(grouping_var, demographic_var)))) %>%
    summarize(n_reviews = n(),
      mean_rating = mean(rating, na.rm=TRUE),
      sd_rating = sd(rating, na.rm=TRUE),
      p_low = mean(rating <= 2, na.rm=TRUE),
      p_high = mean(rating == 5, na.rm=TRUE),
      .groups = "drop")}

#' @description
#' Determine rating gaps within products across demographics.
#' First finds products whose ratings vary substantially across demographics.
#' Then compute max-min differences in group means for each product
#' @param df
#' @param demographic_var the name of an existing column in df
#' Demographic variable used to compare
#' @param filter_vec  Restricts to only the relevant categories
#' @param min_reviews Minimum number of reviews required for a product
#' @return df
#' Output of summarized within product rating gaps
compute_within_product_gaps <- function(df, demographic_var, filter_vec, min_reviews = 20, min_group_reviews = 8) {
  df_sub <- df %>%
    filter(filter_vec, !is.na(.data[[demographic_var]]))
  
  group_means <- df_sub %>%
    group_by(product_id, product_name, .data[[demographic_var]]) %>%
    summarize(mean_rating = mean(rating, na.rm = TRUE),
      n_reviews = n(),
      .groups = "drop") %>%
    filter(n_reviews >= min_group_reviews)
  
  product_gaps <- group_means %>%
    group_by(product_id, product_name) %>%
    summarize(n_reviews_total = sum(n_reviews),
      n_groups = n(),
      max_mean = max(mean_rating),
      min_mean = min(mean_rating),
      gap = max_mean - min_mean,
      max_group = .data[[demographic_var]][which.max(mean_rating)],
      min_group = .data[[demographic_var]][which.min(mean_rating)],
      .groups = "drop") %>%
    filter(n_reviews_total >= min_reviews,
      n_groups >= 2)
  
  product_gaps}

# variability at the product level with the products that drive divisive 
# consumer sentiment
variability_summary <- df %>%
  group_by(product_id, product_name, brand_name, primary_category) %>%
  summarise(n_reviews = n(),
            mean_rating = mean(rating, na.rm = TRUE),
            sd_rating = sd(rating, na.rm = TRUE),
            p_extreme = mean(rating == 1 | rating == 5, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(sd_rating))

#' @description
#' Computes summary rating statistics across levels of a demographic variable
#' within one or more grouping variables (e.g., products or categories).
#' @param df
#' @param demographic_var A length 1 character string
#' The demographic variable used to compare (skin type and skin tone)
#' @param grouping_var character vector of one or more column names 
#' @return Data frame.
#' Output with summary statistics for each grouping_var x demographic_var combo

summarize_ratings_by_demographic <- function(df, demographic_var, grouping_var) {
  df_sub <- df[!is.na(df[[demographic_var]]), ]
  df_sub %>% group_by(across(all_of(c(grouping_var, demographic_var)))) %>%
    summarize(n_reviews = n(),
      mean_rating = mean(rating, na.rm=TRUE),
      sd_rating = sd(rating, na.rm=TRUE),
      p_low = mean(rating <= 2, na.rm=TRUE),
      p_high = mean(rating == 5, na.rm=TRUE),
      .groups = "drop")}

#' @description
#' Determine rating gaps within products across demographics.
#' First finds products whose ratings vary substantially across demographics.
#' Then compute max-min differences in group means for each product
#' @param df
#' @param demographic_var the name of an existing column in df
#' Demographic variable used to compare
#' @param filter_vec  Restricts to only the relevant categories
#' @param min_reviews Minimum number of reviews required for a product
#' @return df
#' Output of summarized within product rating gaps
compute_within_product_gaps <- function(df, demographic_var, filter_vec, min_reviews = 20, min_group_reviews = 8) {
  df_sub <- df %>%
    filter(filter_vec, !is.na(.data[[demographic_var]]))
  
  group_means <- df_sub %>%
    group_by(product_id, product_name, .data[[demographic_var]]) %>%
    summarize(mean_rating = mean(rating, na.rm = TRUE),
      n_reviews = n(),
      .groups = "drop") %>%
    filter(n_reviews >= min_group_reviews)
  
  product_gaps <- group_means %>%
    group_by(product_id, product_name) %>%
    summarize(n_reviews_total = sum(n_reviews),
      n_groups = n(),
      max_mean = max(mean_rating),
      min_mean = min(mean_rating),
      gap = max_mean - min_mean,
      max_group = .data[[demographic_var]][which.max(mean_rating)],
      min_group = .data[[demographic_var]][which.min(mean_rating)],
      .groups = "drop") %>%
    filter(n_reviews_total >= min_reviews,
      n_groups >= 2)
  
  product_gaps}
