library(tidyverse)

source("1_review/reviews.R")
source("2_demographics/demographic_analysis.R")
source("2_demographics/sephora_run_analysis")
source("3_keywords/keywords.R")
source("4_recommendation_models/recommendation_system.R")

df_test <- tibble(product_id = c(1, 1, 1, 2, 2, 2,3, 3, 3, 3, 3, 4, 4, 4),
  author_id = c(1,2,3,1,2,3,4,5,6,4,5,7,8,9),
  brand_name = c("Brand A", "Brand A", "Brand A",
                 "Brand B", "Brand B", "Brand B",
                 "Brand C", "Brand C", "Brand C", "Brand C", "Brand C",
                 "Brand D", "Brand D", "Brand D"),
  price_usd = c(20,20,20,15,15,15,30,30,30,30,30,25,25,25),
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

df_test <- df_test %>% slice(rep(1:n(), each = 50))

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
  group_b = "dry",
  min_per_group = 10)

kw_disp_drying_cleanser_small

# TEST 2: white cast / ashy for sunscreen + moisturizers
kw_disp_tone_small <- run_keyword_disparity(data = df_test,
  keyword_vec = c("white cast", "ashy", "grey", "gray", "chalky", "purple"),
  category = c("Sunscreen", "Moisturizers"),
  group_col = "skin_tone_bucket",
  group_a = "Fair",
  group_b = "Deep",
  min_per_group = 10)

kw_disp_tone_small

# TEST 3: greasy/oily/shiny for moisturizers
kw_disp_greasy_moist_small <- run_keyword_disparity(data = df_test,
  keyword_vec = c("greasy", "oily", "shiny"),
  category = "Moisturizers",
  group_col = "skin_type",
  group_a = "oily",
  group_b = "dry",
  min_per_group = 10)

kw_disp_greasy_moist_small

# TEST 4: breakouts for treatments 
kw_disp_breakouts_treat_small <- run_keyword_disparity(data = df_test,
  keyword_vec = c("breakout", "breakouts", "acne", "pimples", "zits"),
  category = "Treatments",
  group_col = "skin_type",
  group_a = "oily",
  group_b = "normal",
  min_per_group = 10)

kw_disp_breakouts_treat_small

## BUILD_PERSONA_VEC

# TEST 1
users_vector_test <- tibble( # fake user vector testing
  author_id = "user1",
  skin_type_oily = 0,
  skin_type_dry = 0,
  skin_tone_bucket_Fair = 0,
  skin_tone_bucket_Deep = 0
)
test_user1 <- build_persona_vec(
  user_vectors = users_vector_test,
  skin_type = "oily",
  skin_tone = "Deep"
)
test_user1

# TEST 2

users_vector_test2 <- tibble( # fake user vector testing
  author_id = "user2",
  skin_type_oily = 0,
  skin_type_dry = 0,
  skin_type_combination = 0,
  skin_tone_bucket_Fair = 0,
  skin_tone_bucket_Medium = 0,
  skin_tone_bucket_Deep = 0
)

test_user2 <- build_persona_vec(
  user_vectors = users_vector_test2,
  skin_type = "dry",
  skin_tone = "Fair"
)
test_user2

# TEST 3
user_vectors_test3 <- tibble( 
  author_id = "user3", 
  skin_type_oily = 0, 
  skin_type_dry = 0, 
  skin_type_combination = 0, 
  skin_tone_bucket_Fair = 0, 
  skin_tone_bucket_Medium = 0, 
  skin_tone_bucket_Deep = 0 
)

test_user_3 <- build_persona_vec(
  user_vectors = user_vectors_test3, 
  skin_type = "normal", # NOT a real column 
  skin_tone = "Tan" # NOT a real column 
) 
test_user_3




