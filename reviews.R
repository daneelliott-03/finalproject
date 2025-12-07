library(tidyverse)

products <- read_csv("/Users/dane75862/Desktop/final project/product_info.csv",
                     show_col_types = FALSE)

reviews_0_250 <- read_csv("/Users/dane75862/Desktop/final project/reviews_0-250.csv",
                             show_col_types = FALSE)
reviews_250_500 <- read_csv("/Users/dane75862/Desktop/final project/reviews_250-500.csv",
                             show_col_types = FALSE)
reviews_500_750 <- read_csv("/Users/dane75862/Desktop/final project/reviews_500-750.csv",
                             show_col_types = FALSE)
reviews_750_1250 <- read_csv("/Users/dane75862/Desktop/final project/reviews_750-1250.csv",
                             show_col_types = FALSE)
reviews_1250_end <- read_csv("/Users/dane75862/Desktop/final project/reviews_1250-end.csv",
                             show_col_types = FALSE)

reviews_0_250$author_id <- as.character(reviews_0_250$author_id)
reviews_250_500$author_id <- as.character(reviews_250_500$author_id)
reviews_500_750$author_id <- as.character(reviews_500_750$author_id)
reviews_750_1250$author_id <- as.character(reviews_750_1250$author_id)
reviews_1250_end$author_id <- as.character(reviews_1250_end$author_id)

# reviews stakced
reviews <- bind_rows(
  reviews_0_250,
  reviews_250_500,
  reviews_500_750,
  reviews_750_1250,
  reviews_1250_end
)



merge_products_reviews <- function(products, reviews) {
  products_small <- products %>%
    select(
      product_id,
      primary_category,
      secondary_category,
      tertiary_category,
      size,
      variation_type,
      variation_value,
      variation_desc,
      highlights,
      limited_edition,
      new,
      online_only,
      out_of_stock,
      sephora_exclusive
    )
  
  df <- reviews %>%
    left_join(products_small, by = "product_id")
  
  df
}


add_category_variables <- function(df) {
  
  df %>%
    mutate(
      major_category = case_when(
        str_detect(primary_category, regex("skincare",  ignore_case = TRUE)) ~ "Skincare",
        str_detect(primary_category, regex("makeup", ignore_case = TRUE)) ~ "Makeup",
        str_detect(primary_category, regex("hair",ignore_case = TRUE)) ~ "Hair",
        str_detect(primary_category, regex("fragrance", ignore_case = TRUE)) ~ "Fragrance",
        TRUE ~ "Other"
      ),
      
      is_complexion = case_when(str_detect(secondary_category, regex("foundation|concealer|bb cream|tint", 
                                             ignore_case = TRUE)) ~ TRUE,
        TRUE ~ FALSE
      ))}

summarize_ratings_by_category <- function(df) {
  
  df %>%
    group_by(major_category) %>%
    summarise(
      n_reviews = n(),
      mean_rating = mean(rating, na.rm = TRUE),
      sd_rating = sd(rating,na.rm = TRUE),
      p_low = mean(rating <= 2, na.rm = TRUE),
      p_high = mean(rating == 5, na.rm = TRUE),
      iqr_rating = IQR(rating,  na.rm = TRUE)
    ) %>%
    arrange(desc(n_reviews))
}

summarize_product_variability <- function(df) {
  
  df %>%
    group_by(product_id, product_name, brand_name, major_category) %>%
    summarise(
      n_reviews = n(),
      mean_rating = mean(rating, na.rm = TRUE),
      sd_rating = sd(rating, na.rm = TRUE),
      p_extreme = mean(rating == 1 | rating == 5, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(sd_rating))
}

### Use
df <- merge_products_reviews(products, reviews)
df <- add_category_variables(df)
cat_summary <- summarize_ratings_by_category(df)
product_var <- summarize_product_variability(df)

df
cat_summary
product_var