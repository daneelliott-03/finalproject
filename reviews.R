####REVIEWS##################################################################
library(tidyverse)

# product metadata
products <- read_csv("/Users/dane75862/Desktop/final project/product_info.csv",
                     show_col_types = FALSE)
# review datasets
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

# needed to standardize ID column types
reviews_0_250$author_id <- as.character(reviews_0_250$author_id)
reviews_250_500$author_id <- as.character(reviews_250_500$author_id)
reviews_500_750$author_id <- as.character(reviews_500_750$author_id)
reviews_750_1250$author_id <- as.character(reviews_750_1250$author_id)
reviews_1250_end$author_id <- as.character(reviews_1250_end$author_id)

# binded all reviews together
reviews <- bind_rows(
  reviews_0_250,
  reviews_250_500,
  reviews_500_750,
  reviews_750_1250,
  reviews_1250_end
)


# potentially relevant product features
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
  
# reviews joined with product metadata
df <- reviews %>%
    left_join(products_small, by = "product_id")

# trying to combine more granular skintones into five buckets
df <- df %>%
  mutate(
    skin_tone_bucket = case_when(
      skin_tone %in% c("fair", "porcelain", "fairLight") ~ "Fair",
      skin_tone %in% c("light", "lightMedium") ~ "Light",
      skin_tone %in% c("medium", "mediumTan", "olive") ~ "Medium",
      skin_tone %in% c("tan") ~ "Tan",
      skin_tone %in% c("deep", "rich", "dark") ~ "Deep",
      TRUE ~ NA_character_
    )
  )

# summary table assessing rating distributions across secondary categories. 
# has mean, sd, IQR, as well as proportion of high and low ratings
cat_summary <- df %>%
    group_by(secondary_category) %>%
    summarise(
      n_reviews = n(),
      mean_rating = mean(rating, na.rm = TRUE),
      sd_rating = sd(rating,na.rm = TRUE),
      p_low = mean(rating <= 2, na.rm = TRUE),
      p_high = mean(rating == 5, na.rm = TRUE),
      iqr_rating = IQR(rating,  na.rm = TRUE)
    ) %>%
    arrange(desc(n_reviews))
  
# variability at the product level with the products that drive divisive 
# consumer sentiment
variability_summary <- df %>%
    group_by(product_id, product_name, brand_name, primary_category) %>%
    summarise(
      n_reviews = n(),
      mean_rating = mean(rating, na.rm = TRUE),
      sd_rating = sd(rating, na.rm = TRUE),
      p_extreme = mean(rating == 1 | rating == 5, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(sd_rating))

df
cat_summary
variability_summary