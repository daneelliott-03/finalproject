
dir.create("visuals")

# top_volatile_skin_tone is the subset of 
# products with large rating gaps across tones
volatile_ids <- top_volatile_skin_tone$product_id[1:5]

volatile_details <- skin_tone_by_product %>%
  filter(product_id %in% volatile_ids)

volatile_ids <- top_volatile_skin_tone$product_id[1:5]

volatile_details <- skin_tone_by_product %>%
  filter(product_id %in% volatile_ids) %>%
  filter(n_reviews >= 5) 
volatile_ids <- top_volatile_skin_tone$product_id[1:10]

volatile_details <- skin_tone_by_product %>%
  filter(product_id %in% volatile_ids, n_reviews >= 5)

keywords <- c("white cast", "ashy", "grey", "gray", "ghost", "purple", "chalky")

deep_keyword_stats <- df %>%
  filter(product_id %in% volatile_ids,
    skin_tone_bucket == "Deep"
  ) %>%
  pull(review_text) %>%             
  count_keywords(keywords) %>%           
  mutate(group = "Deep Skin")

fair_keyword_stats <- df %>%
  filter(product_id %in% volatile_ids,
    skin_tone_bucket == "Fair"
  ) %>%
  pull(review_text) %>%
  count_keywords(keywords) %>%
  mutate(group = "Fair Skin")

comparison_stats <- bind_rows(deep_keyword_stats, fair_keyword_stats)

word_occurence <- ggplot(comparison_stats, aes(x = word, y = n, fill = group)) +
  geom_col() +
  scale_fill_manual(values = c("Fair Skin" = "#F9E4D4", "Deep Skin" = "#3B2219")) +
  theme_minimal() +
  labs(title = "Disparity in User Experience",
    y = "Count of Mentions",
    x = NULL,
    fill = "User Group")


ggsave(file.path("visuals", "word_occurence.png"), plot = word_occurence, width=6, height=4)


