source("reviews.R")

#' @description
#' Calculates summary statistics across levels of a particular demographic
#' grouped by a particular product
#' @param df
#' @param demographic_var Character.
#' The demographic variable used to compare (skin type and skin tone)
#' @param grouping_var Character or vector.
#' Used to group results (e.g. product_id, product_name, secondary_category)
#' @return Data frame.
#' Output with summary statistics for each grouping_var x demographic_var combo

summarize_ratings_by_demographic <- function(df, demographic_var, grouping_var) {
  df_sub <- df[!is.na(df[[demographic_var]]), ]
  df_sub %>% 
    group_by(across(all_of(c(grouping_var, demographic_var)))) %>%
    summarize(
      n_reviews = n(),
      mean_rating = mean(rating, na.rm=TRUE),
      sd_rating = sd(rating, na.rm=TRUE),
      p_low = mean(rating <= 2, na.rm=TRUE),
      p_high = mean(rating == 5, na.rm=TRUE),
      .groups = "drop")
}

#' @description
#' Determine rating gaps within products across demographics.
#' First finds products whose ratings vary substantially across demographics.
#' Then compute max-min differences in group means for each product
#' @param df
#' @param demographic_var Character
#' Demographic variable used to compare
#' @param filter_vec Vector
#' Restricts to only the relevant categories
#' @param min_reviews Minimum number of reviews required for a product
#' @return df
#' Output of summarized within product rating gaps

compute_within_product_gaps <- function(df, demographic_var, filter_vec, min_reviews = 20) {
  df_sub <- df %>%
    filter(
      filter_vec,
      !is.na(.data[[demographic_var]])
    )
  
  group_means <- df_sub %>%
    group_by(product_id, product_name, .data[[demographic_var]]) %>%
    summarize(
      mean_rating = mean(rating, na.rm = TRUE),
      n_reviews = n(),
      .groups = "drop"
    )
  
  product_gaps <- group_means %>%
    group_by(product_id, product_name) %>%
    summarize(
      n_reviews_total = sum(n_reviews),
      n_groups = n(),
      max_mean = max(mean_rating),
      min_mean = min(mean_rating),
      gap = max_mean - min_mean,
      .groups = "drop"
    ) %>%
    filter(
      n_reviews_total >= min_reviews,
      n_groups >= 2
    )
  
  product_gaps
}

## Analysis by skin-type:

# start with relevant categories
skin_type_categories <- c("Moisturizers", 
                          "Treatments", 
                          "Cleansers",
                          "Masks", 
                          "Sunscreen")

filter_skin_type <- df$secondary_category %in% skin_type_categories

# summaries by skin type with respect to categories
skin_type_by_category <- summarize_ratings_by_demographic(df, 
                                                          demographic_var = "skin_type",
                                                          grouping_var = "secondary_category")
# summaries by skin type with respect to products
skin_type_by_product <- summarize_ratings_by_demographic(df, 
                                                         demographic_var = "skin_type",
                                                         grouping_var = c("product_id", "product_name"))
# rating gaps within each product
skin_type_gaps <- compute_within_product_gaps(
  df,
  demographic_var = "skin_type",
  filter_vec = filter_skin_type,
  min_reviews = 20
)

head(skin_type_by_category)
head(skin_type_by_product)
head(skin_type_gaps)


# By skin-tone

# start with relevant categories
skin_tone_categories <- c("Self Tanners", 
                          "Sunscreen")

filter_skin_tone <- df$secondary_category %in% skin_tone_categories

# summaries by skin tone with respect to categories
skin_tone_by_category <- summarize_ratings_by_demographic(df, 
                                                          demographic_var = "skin_tone",
                                                          grouping_var = "secondary_category")
# summaries by skin tone with respect to products
skin_tone_by_product <- summarize_ratings_by_demographic(df, 
                                                         demographic_var = "skin_tone", 
                                                         grouping_var = c("product_id", "product_name"))
# rating gaps within each product
skin_tone_gaps <- compute_within_product_gaps(
  df,
  demographic_var = "skin_tone",
  filter_vec = filter_skin_tone,
  min_reviews = 20
)

head(skin_tone_by_category)
head(skin_tone_by_product)
head(skin_tone_gaps)