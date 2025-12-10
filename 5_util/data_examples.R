####EXAMPLES################################################

source("1_review/reviews.R")
source("2_demographics/demographic_analysis.R")
source("2_demographics/sephora_run_analysis.R")
source("3_keywords/keywords.R")
source("4_recommendation_models/recommendation_system.R")

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
  mutate(text_lower = str_to_lower(review_text),
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
                      group_b = "dry",
                      min_per_group = 10)

# TEST 2: skincare complaints
run_keyword_disparity(data = df,
                      keyword_vec = c("white cast", "ashy", "grey", "gray", "chalky", "purple"),
                      category = c("Sunscreen"),
                      group_col = "skin_tone_bucket",
                      group_a = "Fair",
                      group_b = "Medium",
                      min_per_group = 10)

# TEST 3: shiny after moisturize 
run_keyword_disparity(data = df,
                      keyword_vec = c("greasy", "oily", "shiny"),
                      category = c("Moisturizers"),
                      group_col = "skin_type",
                      group_a = "oily",
                      group_b = "dry",
                      min_per_group = 10)

# TEST 4: breakouts after treatment
run_keyword_disparity(data = df,
                      keyword_vec = c("breakout", "breakouts", "acne", "pimples", "zits"),
                      category = c("Treatments"),
                      group_col = "skin_type",
                      group_a = "oily",
                      group_b = "normal",
                      min_per_group = 10)


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
                           n_recs = 5)
