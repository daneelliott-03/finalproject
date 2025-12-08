<<<<<<< HEAD
=======
library(tidyverse)
library(kableExtra)

source("reviews.R")
source("demographic_analysis.R")
source("similarpersonarecs.R")
source("regression.R")

# created this to show how far apart types are within categories overall and not
# within one product

min_cell_n <- 20

skin_type_cat_spread <- skin_type_by_category %>%
  filter(n_reviews >= min_cell_n) %>%   
  group_by(secondary_category) %>%
  summarise(
    n_reviews_total = sum(n_reviews),
    n_skin_types = n(),
    max_mean = max(mean_rating, na.rm = TRUE),
    min_mean = min(mean_rating, na.rm = TRUE),
    gap_mean = max_mean - min_mean,
    .groups = "drop"
  ) %>%
  arrange(desc(gap_mean))

kable(
  skin_type_cat_spread,
  caption = "Spread in mean ratings across skin types by category."
)

# how big is the spread across tone sin this category using 
# category level averages
skin_tone_cat_spread <- skin_tone_by_category %>%
  filter(n_reviews >= min_cell_n) %>%   
  group_by(secondary_category) %>%
  summarise(
    n_reviews_total = sum(n_reviews),
    n_skin_tones = n(), 
    max_mean = max(mean_rating, na.rm = TRUE),
    min_mean = min(mean_rating, na.rm = TRUE),
    gap_mean = max_mean - min_mean,
    .groups = "drop"
  ) %>%
  arrange(desc(gap_mean))

kable(
  skin_tone_cat_spread,
  caption = "Spread in mean ratings across skin tones by category."
)

# Skin type stability per product
skin_type_stability <- skin_type_gaps %>%
  mutate(
    stability_skin_type = case_when(
      gap < 0.25 ~ "Stable",
      gap > 0.75 ~ "Volatile",
      TRUE ~ "Moderate"
    )
  ) %>%
  select(product_id, product_name, stability_skin_type, gap_skin_type = gap)

skin_tone_stability <- skin_tone_gaps %>%
  mutate(
    stability_skin_tone = case_when(
      gap < 0.25 ~ "Stable",
      gap > 0.75 ~ "Volatile",
      TRUE ~ "Moderate"
    )
  ) %>%
  select(product_id, product_name, stability_skin_tone, gap_skin_tone = gap)

product_cats <- df %>%
  select(product_id, secondary_category) %>%
  distinct()
# how many products, average product level gap, and proportion volatile vs stable

skin_type_cat_vol <- skin_type_stability %>%
  left_join(product_cats, by = "product_id") %>%
  group_by(secondary_category) %>%
  summarise(
    n_products = n(),
    mean_gap = mean(gap_skin_type, na.rm = TRUE),
    prop_volatile = mean(stability_skin_type == "Volatile"),
    prop_stable = mean(stability_skin_type == "Stable"),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_volatile))

kable(
  skin_type_cat_vol,
  caption = "Volatility across skin types by category (product-level gaps)."
)

# skin type category level spread and volatility
skin_type_category_summary <- skin_type_cat_spread %>%
  left_join(
    skin_type_cat_vol %>%
      select(secondary_category, prop_volatile, prop_stable),
    by = "secondary_category"
  ) %>%
  filter(secondary_category %in% skin_type_categories)

kable(
  skin_type_category_summary,
  caption = "Rating spread and volatility across skin types by category."
)

# within this category, how many individual products have large gaps across tones
skin_tone_cat_vol <- skin_tone_stability %>%
  left_join(product_cats, by = "product_id") %>%
  group_by(secondary_category) %>%
  summarise(
    n_products = n(),
    mean_gap = mean(gap_skin_tone, na.rm = TRUE),
    prop_volatile = mean(stability_skin_tone == "Volatile"),
    prop_stable = mean(stability_skin_tone == "Stable"),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_volatile))

# spread and volatility for skin tone
skin_tone_category_summary <- skin_tone_cat_spread %>%
  left_join(
    skin_tone_cat_vol %>%
      select(secondary_category, prop_volatile, prop_stable),
    by = "secondary_category"
  ) %>% 
  filter(secondary_category %in% skin_tone_categories)

kable(
  skin_tone_category_summary,
  caption = "Rating spread and volatility across skin tones by category."
)

# here a gap of 4 is saying for one skin type with over 20 reviews, one skin type's 
# mean is near 5 and another is near 1
top_volatile_skin_type <- skin_type_stability %>%
  left_join(product_cats, by = "product_id") %>%
  filter(
    secondary_category %in% skin_type_categories,
    stability_skin_type == "Volatile"
  ) %>%
  arrange(desc(gap_skin_type)) %>%
  slice_head(n = 20)

top_volatile_skin_tone <- skin_tone_stability %>%
  left_join(product_cats, by = "product_id") %>%
  filter(
    secondary_category %in% skin_tone_categories,
    stability_skin_tone == "Volatile"
  ) %>%
  arrange(desc(gap_skin_tone)) %>%
  slice_head(n = 20)

kable(top_volatile_skin_type, caption = "Most volatile products across skin types.")
kable(top_volatile_skin_tone, caption = "Most volatile products across skin tones.")
>>>>>>> c43af84ab1bac4d782560a4058ead458e6cf3cde
