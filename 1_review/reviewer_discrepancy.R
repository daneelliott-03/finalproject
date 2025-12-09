source("1_review/reviews.R")

skin_palette <- c("Fair" = "#F9E4D4",  
                  "Light" = "#EFB99F",  
                  "Medium" = "#BD8865",  
                  "Tan" = "#8F5E3D",  
                  "Deep" = "#3B2219")

representation_skin <- ggplot(skin_tone_by_category,
       aes(x= factor(skin_tone_bucket, levels = c("Fair", "Light", "Medium", "Tan", "Deep")),
           y = n_reviews,
           fill = skin_tone_bucket)) +
  geom_col() +
  facet_wrap(~ secondary_category, scales = "free_y") +
  scale_fill_manual(values = skin_palette) +
  theme_minimal() +
  labs(title = "Reviewer Representation by Skin Tone",
       subtitle = "Deep skin tones are underrepresented in reviews",
       x = "Skin Tone",
       y = "Number of Reviews") +
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none")

rating_dist_all <- df %>%
  filter(!is.na(skin_tone_bucket)) %>%      
  count(skin_tone_bucket, rating) %>%       
  group_by(skin_tone_bucket) %>%
  mutate(prop = n / sum(n))       

star_ratings <- ggplot(rating_dist_all,
       aes(x = factor(rating), y = prop, fill = skin_tone_bucket)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = skin_palette) +
  theme_minimal() +
  labs(title = "Star Ratings by Skin Tone",
       subtitle = "Distribution of review scores across all skin tone groups",
       x = "Star Rating",
       y = "Proportion of Reviews",
       fill = "Skin Tone")

ggsave("1_review/representation_skin.png", 
       plot = representation_skin, 
       width = 6, height = 4, bg="white")

ggsave("1_review/star_ratings.png", 
       plot = star_ratings, 
       width = 6, height = 4, bg="white")