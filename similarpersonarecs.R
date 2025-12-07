# ---------------------
make_recs <- function(df,
                      persona_skin_type = NULL,
                      persona_skin_tone = NULL,
                      categories = skin_type_categories,
                      min_reviews = 5,
                      top_n = 10) {
  # ensuring the function isn't giving recommendations outside the domain
  df_sub <- df %>%
    filter(
      major_category == "Skincare",
      secondary_category %in% categories,
      !is.na(rating)
    )
  # restricts the data so the recommendation system only looks at 
  # reviews from people similar to the persona.
  if (!is.null(persona_skin_type)) {
    df_sub <- df_sub %>%
      filter(
        tolower(.data$skin_type) == tolower(persona_skin_type)
      )
  }
  
  if (!is.null(persona_skin_tone)) {
    df_sub <- df_sub %>%
      filter(
        tolower(.data$skin_tone) == tolower(persona_skin_tone)
      )
  }
  ## grouping and aggregating ratings
  df_sub %>%
    group_by(product_id, product_name, brand_name, secondary_category) %>%
    summarise(
      n_reviews = n(),
      mean_rating = mean(rating, na.rm = TRUE),
      p_high = mean(rating == 5, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # ranking process
    filter(n_reviews >= min_reviews) %>%
    arrange(desc(mean_rating), desc(n_reviews)) %>%
    slice_head(n = top_n)
}


persona1 <- make_recs(df,
                      persona_skin_type = "oily",
                      persona_skin_tone = "light",
                      min_reviews = 20)
persona2 <- make_recs(df,
                      persona_skin_type = "dry",
                      persona_skin_tone = "medium",
                      min_reviews = 20)

persona3 <- make_recs(df,
                      persona_skin_type = "combination",
                      persona_skin_tone = "deep",
                      min_reviews = 20)

persona1_labeled <- persona1 %>%
  mutate(persona = "Oily, Light") %>%
  select(persona, product_name, brand_name, secondary_category,
    n_reviews, mean_rating, p_high
  )

persona2_labeled <- persona2 %>%
  mutate(persona = "Dry, Medium") %>%
  select( persona,product_name, brand_name, secondary_category,
    n_reviews, mean_rating, p_high
  )

persona3_labeled <- persona3 %>%
  mutate(persona = "Combination, Deep") %>%
  select(persona, product_name, brand_name, secondary_category,
    n_reviews, mean_rating,p_high
  )

persona_recs_table <- bind_rows(
  persona1_labeled,
  persona2_labeled,
  persona3_labeled
)

kable(
  persona_recs_table,
  caption = "Top 10 product recommendations for three example personas."
)

