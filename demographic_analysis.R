
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

# By skin-type:

skin_type_categories <- c("Moisturizers", 
                          "Treatments", 
                          "Cleansers",
                          "Masks", 
                          "Sunscreen")

filter_skin_type <- df$secondary_category %in% skin_type_categories

skin_type_by_category <- summarize_ratings_by_demographic(df, 
                                                          demographic_var = "skin_type",
                                                          grouping_var = "secondary_category")

skin_type_by_product <- summarize_ratings_by_demographic(df, 
                                                         demographic_var = "skin_type",
                                                         grouping_var = c("product_id", "product_name"))

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

skin_tone_categories <- c("Self Tanners", 
                          "Sunscreen")

filter_skin_tone <- df$secondary_category %in% skin_tone_categories

skin_tone_by_category <- summarize_ratings_by_demographic(df, 
                                                          demographic_var = "skin_tone",
                                                          grouping_var = "secondary_category")

skin_tone_by_product <- summarize_ratings_by_demographic(df, 
                                                         demographic_var = "skin_tone", 
                                                         grouping_var = c("product_id", "product_name"))

skin_tone_gaps <- compute_within_product_gaps(
  df,
  demographic_var = "skin_tone",
  filter_vec = filter_skin_tone,
  min_reviews = 20
)

head(skin_tone_by_category)
head(skin_tone_by_product)
head(skin_tone_gaps)