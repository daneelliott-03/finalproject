source("1_review/reviews.R")
source("2_demographics/demographic_analysis.R")
source("2_demographics/run_analysis.R")

relevant_cats <- c(skin_type_categories, skin_tone_categories)

# filter to only skincare categories where demographic matters
df_rel <- df %>%
  filter(secondary_category %in% relevant_cats)

# create skin tone buckets for more simple groupings
df_rel <- df %>%
  filter(secondary_category %in% c(
    "Moisturizers","Treatments","Cleansers",
    "Masks","Sunscreen","Self Tanners")) %>%
  mutate(skin_tone_bucket = case_when(
    skin_tone %in% c("fair", "porcelain", "fairLight") ~ "Fair",
    skin_tone %in% c("light", "lightMedium") ~ "Light",
    skin_tone %in% c("medium", "mediumTan", "olive") ~ "Medium",
    skin_tone %in% c("tan") ~ "Tan",
    skin_tone %in% c("deep", "rich", "dark") ~ "Deep",
    TRUE ~ NA_character_
  ),
  high_rating = rating >=4
  )

# predict probability of a user granting a high rating (4+)
glm_interact <- glm(
  high_rating ~ 
    skin_type * secondary_category +
    skin_tone_bucket * secondary_category +
    price_usd,
  data = df_rel,
  family = binomial
)
summary(glm_interact)