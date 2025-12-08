####EXAMPLES################################################

source("reviews.R")
source("demographic_analysis.R")
source("keywords.R")

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
  category = c("Sunscreen", "Self Tanners", "Moisturizers"),
  group_col = "skin_tone_bucket",
  group_a = "Fair",
  group_b = "Deep")

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

