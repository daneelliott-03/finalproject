####EXAMPLES################################################

source("reviews.R")
source("demographic_analysis.R")
source("keywords.R")
source("recommendation_system.R")

### SUMMARIZE_RATINGS_BY_DEMOGRAPHIC TEST
test_skin_type_cat <- summarize_ratings_by_demographic(df,
  demographic_var = "skin_type",
  grouping_var = "secondary_category"
)

head(test_skin_type_cat)
summary(test_skin_type_cat$mean_rating)

test_skin_tone_cat <- summarize_ratings_by_demographic(df,
  demographic_var = "skin_tone",
  grouping_var = "secondary_category"
)

head(test_skin_tone_cat)
summary(test_skin_tone_cat$mean_rating)


### COMPUTE_WITHIN_PRODUCT_GAPS TEST

# TEST 1: Identify products with large rating gaps across skin types
filter_skin_type <- df$secondary_category %in% c(
  "Moisturizers", "Treatments", "Cleansers", "Masks", "Sunscreen")

compute_within_product_gaps(
  df,
  demographic_var = "skin_type",
  filter_vec = filter_skin_type,
  min_reviews = 20
)

# TEST 2: Identify sunscreen products with large gaps across skin tone buckets
filter_skin_tone <- df$secondary_category == "Sunscreen"

compute_within_product_gaps(
  df,
  demographic_var = "skin_tone",
  filter_vec = filter_skin_tone,
  min_reviews = 20
)

### COUNT_KEYWORDS TEST

## TEST 1: counting keywords

keywords_test <- c("white cast", "ashy", "greasy")

kw_counts_all <- count_keywords(df$review_text, keywords_test)

kw_counts_all

## TEST 2: manual check for "white cast"
kw_white <- count_keywords(df$review_text, c("white cast"))
kw_white
# kw_white$n should be the total number of times "white cast" appears
# across all reviews (not just the number of reviews)
pattern_white <- "\\bwhite cast\\b"

manual_white <- df %>%
  mutate(
    text_lower = str_to_lower(review_text),
    hits_white = str_count(text_lower, pattern_white)
  ) %>%
  summarise(total_hits_white = sum(hits_white, na.rm = TRUE))

manual_white

kw_white$n 
manual_white$total_hits_white 


## TEST 3: count_keywords on NEGATIVE Sunscreen reviews

problem_keywords <- c("white cast", "ashy", "grey", "gray", "chalky", "purple")

neg_sunscreen <- df %>%
  filter(secondary_category == "Sunscreen",
    rating <= 3,
    !is.na(review_text))

# Run your keyword function on that exact subset
kw_neg_sunscreen <- count_keywords(neg_sunscreen$review_text, problem_keywords)

kw_neg_sunscreen


## RUN_DISPARITY_TEST

# TEST 1: drying for cleansers 
run_keyword_disparity(data = df,
  keyword_vec = "drying",
  category = "Cleansers",
  group_col = "skin_type",
  group_a = "oily",
  group_b = "dry")

# TEST 2: skincare complaints
run_keyword_disparity(data = df,
  keyword_vec = c("white cast", "ashy", "grey", "gray", "chalky", "purple"),
  category = c("Sunscreen"),
  group_col = "skin_tone_bucket",
  group_a = "Fair",
  group_b = "Medium")

# TEST 3: shiny after moisturize 
run_keyword_disparity(data = df,
  keyword_vec = c("greasy", "oily", "shiny"),
  category = c("Moisturizers"),
  group_col = "skin_type",
  group_a = "oily",
  group_b = "dry")

# TEST 4: breakouts after treatment
run_keyword_disparity(data = df,
  keyword_vec = c("breakout", "breakouts", "acne", "pimples", "zits"),
  category = c("Treatments"),
  group_col = "skin_type",
  group_a = "oily",
  group_b = "normal")


## TESTS FOR RECOMMENDATION SYSTEM

# BUILD_PERSONA_VEC

# Example 1: Build a persona for an oily + Medium skin tone user
persona1 <- build_persona_vec(
  user_vectors = user_vectors,
  skin_type = "oily",
  skin_tone = "Medium"
)

persona1

# Example 2: Build a persona for a dry + Deep skin tone user
persona2 <- build_persona_vec(
  user_vectors,
  skin_type = "dry",
  skin_tone = "Deep"
)

persona2

# Example 3: Check which demographic columns will be activated
build_persona_vec(
  user_vectors,
  skin_type = "combination",
  skin_tone = "Fair"
)

# RECOMMEND SIMILAR PRODUCTS

# TEST 1:
recommend_similar_products(df_rel, user_vectors,
                           persona_skin_type = "combination",
                           persona_skin_tone = "Deep",
                           price_min = 0,
                           price_max = 20,
                           category = "Moisturizers",
                           n_recs = 5
)

# TEST 2:
recommend_similar_products(df_rel, user_vectors,
                           persona_skin_type = "oily",
                           persona_skin_tone = "Fair",
                           price_min = 0,
                           price_max = 50,
                           category = "Sunscreen",
                           n_recs = 5
)

# TEST 3:
recommend_similar_products(df_rel, user_vectors,
                           persona_skin_type = "dry",
                           persona_skin_tone = "Medium",
                           price_min = 0,
                           price_max = 15,
                           category = "Cleansers",
                           n_recs = 5
)

# TEST 4:
recommend_similar_products(df_rel, user_vectors,
                           persona_skin_type = "normal",
                           persona_skin_tone = "Light",
                           price_min = 0,
                           price_max = 30,
                           category = "Cleansers",
                           n_recs = 5
)

# TEST 5:
recommend_similar_products(df_rel, user_vectors,
                           persona_skin_type = "combination",
                           persona_skin_tone = "Tan",
                           price_min = 0,
                           price_max = 100,
                           category = "Cleansers",
                           n_recs = 5
)



