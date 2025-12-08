## graph to show summary for volatility information 
# (% of reviews with gaps over .75 for skin tone)
skin_type_cat_vol_long <- skin_type_cat_vol %>%
  mutate(prop_moderate = 1 - prop_volatile - prop_stable) %>%
  select(secondary_category,
    prop_stable,
    prop_moderate,
    prop_volatile) %>%
  pivot_longer(cols = starts_with("prop_"),
    names_to = "stability",
    values_to = "prop") %>%
  mutate(stability = recode(stability,
      prop_stable = "Stable",
      prop_moderate = "Moderate",
      prop_volatile = "Volatile"))

ggplot(skin_type_cat_vol_long,
  aes(x = secondary_category, y = prop, fill = stability)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Stable" = "#DEEBF7", 
    "Moderate" = "#9ECAE1",
    "Volatile" = "#08519C"  )) +
  theme_minimal() +
  labs(title = "Within each category, share of products across skin types",
    x = NULL,
    y = "Proportion of Products",
    z = "Product Type")


skin_tone_cat_vol_long <- skin_tone_cat_vol %>%
  mutate(prop_moderate = 1 - prop_volatile - prop_stable) %>%
  select(secondary_category,
    prop_stable,
    prop_moderate,
    prop_volatile) %>%
  pivot_longer(
    cols = starts_with("prop_"),
    names_to  = "stability",
    values_to = "prop") %>%
  mutate(stability = recode(stability,
      prop_stable = "Stable",
      prop_moderate = "Moderate",
      prop_volatile = "Volatile"))

ggplot(skin_tone_cat_vol_long,
  aes(x = secondary_category, y = prop, fill = stability)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Stable" = "#FEE5D9", 
    "Moderate" = "#FCAE91",
    "Volatile" = "#CB181D")) +
  theme_minimal() +
  labs(title = "Within each category, share of products that are across skin tones",
    x = NULL,
    y = "Proportion of Products",
    fill = "Stability")
