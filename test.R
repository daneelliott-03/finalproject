library(tidyverse)
source("reviews.R")
source("demographic_analysis.R")
source("keywords.R")

df_test <- tibble(product_id = c(1, 1, 1, 2, 2, 2,3, 3, 3, 3, 3, 4, 4, 4),
  product_name = c("Prod A", "Prod A", "Prod A",
    "Prod B", "Prod B", "Prod B",
    "Prod C", "Prod C", "Prod C", "Prod C", "Prod C",
    "Prod D", "Prod D", "Prod D"),
  secondary_category = c("Sunscreen", "Sunscreen", "Sunscreen",
    "Cleansers", "Cleansers", "Cleansers",
    "Moisturizers", "Moisturizers", "Moisturizers", "Moisturizers", "Moisturizers",
    "Treatments", "Treatments", "Treatments"),
  rating = c(5, 2, 3, 4, 1, 3, 5, 3, 2, 4, 1, 2, 3, 4),   
  review_text = c("Loved it, no white cast at all",
    "Too chalky and purple terrible white cast",
    "Greasy and a bit ashy on my skin",
    "very drying cleanser and made my face tight",
    "Too drying caused breakouts and acne",
    "Slightly drying but still okay overall",
    "nice but a little greasy and shiny",
    "Left me oily and shiny all day",
    "Broke me out with pimples and zits",
    "Very greasy finish and shiny T-zone",
    "Made my dry skin greasy and shiny",
    "Strong active treatment, more bREAKouts at first",
    "Another treatment caused breakouts and zits",
    "Initial breakouts and pimples improved later"),
  skin_type = c("oily", "dry", "oily",
    "oily", "dry", "oily",
    "oily", "dry", "normal", "oily", "dry",
    "oily", "normal", "normal"),
  skin_tone_bucket = c("Fair", "Deep", "Fair",
    "Fair", "Deep", "Fair",
    "Fair", "Deep", "Deep", "Fair", "Deep",
    "Fair", "Fair", "Fair"))

df_test <- df_test %>% slice(rep(1:n(), each = 10))

## SUMMARIZE_RATINGS_BY_DEMOGRAPHIC
test_skin_type_cat_small <- summarize_ratings_by_demographic(df_test,
  demographic_var = "skin_type",
  grouping_var = "secondary_category")

test_skin_type_cat_small
summary(test_skin_type_cat_small$mean_rating)

test_skin_tone_cat_small <- summarize_ratings_by_demographic(df_test,
  demographic_var = "skin_tone_bucket",
  grouping_var = "secondary_category")

test_skin_tone_cat_small
summary(test_skin_tone_cat_small$mean_rating)

## COMPUTE_WITHIN_PRODUCT_GAPS
test_gaps_skin_tone_small <- compute_within_product_gaps(
  df = df_test,
  demographic_var = "skin_tone_bucket",
  filter_vec = !is.na(df_test$secondary_category),
  min_reviews = 1,
  min_group_reviews = 1)

test_gaps_skin_tone_small

test_gaps_skin_type_small <- compute_within_product_gaps(
  df = df_test,
  demographic_var = "skin_type",
  filter_vec = !is.na(df_test$secondary_category),
  min_reviews = 1,
  min_group_reviews = 1)

test_gaps_skin_type_small
##COUNT_KEYWORDS
keywords_test <- c("white cast", "ashy", "greasy", "drying", "breakouts")

kw_counts_small <- count_keywords(df_test$review_text, keywords_test)
kw_counts_small

problem_keywords <- c("white cast", "ashy", "grey", "gray", "chalky", "purple")

neg_sunscreen_small <- df_test %>%
  filter(secondary_category == "Sunscreen",
    rating <= 3,
    !is.na(review_text))

kw_neg_sunscreen_small <- count_keywords(neg_sunscreen_small$review_text,
  problem_keywords)

kw_neg_sunscreen_small

## RUN_KEYWORD_DISPARITY
kw_disp_drying_cleanser_small <- run_keyword_disparity(data = df_test,
  keyword_vec = "drying",
  category = "Cleansers",
  group_col = "skin_type",
  group_a = "oily",
  group_b = "dry")

kw_disp_drying_cleanser_small

# TEST 2: white cast / ashy for sunscreen + moisturizers
kw_disp_tone_small <- run_keyword_disparity(data = df_test,
  keyword_vec = c("white cast", "ashy", "grey", "gray", "chalky", "purple"),
  category = c("Sunscreen", "Moisturizers"),
  group_col = "skin_tone_bucket",
  group_a = "Fair",
  group_b = "Deep")

kw_disp_tone_small

# TEST 3: greasy/oily/shiny for moisturizers
kw_disp_greasy_moist_small <- run_keyword_disparity(data = df_test,
  keyword_vec = c("greasy", "oily", "shiny"),
  category = "Moisturizers",
  group_col = "skin_type",
  group_a = "oily",
  group_b = "dry")

kw_disp_greasy_moist_small

# TEST 4: breakouts for treatments 
kw_disp_breakouts_treat_small <- run_keyword_disparity(data = df_test,
  keyword_vec = c("breakout", "breakouts", "acne", "pimples", "zits"),
  category = "Treatments",
  group_col = "skin_type",
  group_a = "oily",
  group_b = "normal")

kw_disp_breakouts_treat_small