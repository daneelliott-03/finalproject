library(dplyr)
library(ggplot2)

source("reviews.R")
source("demographic_analysis.R")
source("similarpersonarecs.R")

# initial visualizations displaying average ratings across different products 
# and skin tones / types

############### SKIN TONE ############### 

# Filter sunscreen reviews
sunscreen_tone_stats <- df_rel %>%
  filter(secondary_category == "Sunscreen",
         !is.na(skin_tone_bucket)) %>%
  group_by(skin_tone_bucket) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    skin_tone_bucket = factor(
      skin_tone_bucket,
      levels = c("Fair", "Light", "Medium", "Tan", "Deep")))

# Plot
ggplot(sunscreen_tone_stats, aes(x = skin_tone_bucket, y = mean_rating, fill = skin_tone_bucket)) +
  geom_col(alpha = 0.8, color = "white") +
  geom_text(aes(label = round(mean_rating, 2)), vjust = -0.6, size = 3) +
  ylim(0, 5) +
  labs(
    title = "Average Sunscreen Rating Across Skin Tones",
    x = "Skin Tone",
    y = "Mean Rating (1–5)"
  ) +
  theme_minimal(base_size = 14)


# TANNER
tanner_tone_stats <- df_rel %>%
  filter(secondary_category == "Self Tanners",
         !is.na(skin_tone_bucket)) %>%
  group_by(skin_tone_bucket) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    skin_tone_bucket = factor(
      skin_tone_bucket,
      levels = c("Fair", "Light", "Medium", "Tan", "Deep")))

ggplot(tanner_tone_stats, aes(x = skin_tone_bucket, y = mean_rating, fill = skin_tone_bucket)) +
  geom_col(alpha = 0.8, color = "white") +
  geom_text(aes(label = round(mean_rating, 2)), vjust = -0.6, size = 3) +
  ylim(0, 5) +
  labs(
    title = "Average Self Tanner Rating Across Skin Tones",
    x = "Skin Tone",
    y = "Mean Rating (1–5)") +
  theme_minimal(base_size = 14)


############### SKIN TYPE ############### 

# MOISTURIZERS
moisturizer_stats <- df_rel %>%
  filter(secondary_category == "Moisturizers",
         !is.na(skin_type)) %>%
  group_by(skin_type) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    .groups = "drop")

ggplot(moisturizer_stats, aes(x = skin_type, y = mean_rating, fill = skin_type)) +
  geom_col(alpha = 0.8, color = "white") +
  geom_text(aes(label = round(mean_rating, 2)), vjust = -0.6, size = 3) +
  ylim(0, 5) +
  labs(
    title = "Average Rating by Skin Type — Moisturizers",
    x = "Skin Type",
    y = "Mean Rating (1–5)"
  ) +
  theme_minimal(base_size = 14)


# TREATMENTS
treatment_stats <- df_rel %>%
  filter(secondary_category == "Treatments",
         !is.na(skin_type)) %>%
  group_by(skin_type) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    .groups = "drop")

ggplot(treatment_stats, aes(x = skin_type, y = mean_rating, fill = skin_type)) +
  geom_col(alpha = 0.8, color = "white") +
  geom_text(aes(label = round(mean_rating, 2)), vjust = -0.6, size = 3) +
  ylim(0, 5) +
  labs(
    title = "Average Rating by Skin Type — Treatments",
    x = "Skin Type",
    y = "Mean Rating (1–5)"
  ) +
  theme_minimal(base_size = 14)


# CLEANSERS
cleanser_stats <- df_rel %>%
  filter(secondary_category == "Cleansers",
         !is.na(skin_type)) %>%
  group_by(skin_type) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    .groups = "drop")

ggplot(cleanser_stats, aes(x = skin_type, y = mean_rating, fill = skin_type)) +
  geom_col(alpha = 0.8, color = "white") +
  geom_text(aes(label = round(mean_rating, 2)), vjust = -0.6, size = 3) +
  ylim(0, 5) +
  labs(
    title = "Average Rating by Skin Type — Cleansers",
    x = "Skin Type",
    y = "Mean Rating (1–5)"
  ) +
  theme_minimal(base_size = 14)


# MASKS
mask_stats <- df_rel %>%
  filter(secondary_category == "Masks",
         !is.na(skin_type)) %>%
  group_by(skin_type) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    .groups = "drop")

ggplot(mask_stats, aes(x = skin_type, y = mean_rating, fill = skin_type)) +
  geom_col(alpha = 0.8, color = "white") +
  geom_text(aes(label = round(mean_rating, 2)), vjust = -0.6, size = 3) +
  ylim(0, 5) +
  labs(
    title = "Average Rating by Skin Type — Masks",
    x = "Skin Type",
    y = "Mean Rating (1–5)") +
  theme_minimal(base_size = 14)


# SUNSCREEN (for skin type)

sunscreen_stats_2 <- df_rel %>%
  filter(secondary_category == "Sunscreen",
         !is.na(skin_type)) %>%
  group_by(skin_type) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    .groups = "drop")

ggplot(sunscreen_stats_2, aes(x = skin_type, y = mean_rating, fill = skin_type)) +
  geom_col(alpha = 0.8, color = "white") +
  geom_text(aes(label = round(mean_rating, 2)), vjust = -0.6, size = 3) +
  ylim(0, 5) +
  labs(
    title = "Average Rating by Skin Type — Sunscreen",
    x = "Skin Type",
    y = "Mean Rating (1–5)"
  ) +
  theme_minimal(base_size = 14)