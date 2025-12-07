library(dplyr)
library(fastDummies)
library(tidyr)
library(lsa)

source("reviews.R")
source("demographic_analysis.R")
source("similarpersonarecs.R")
source("regression.R")
source("stability.R")

# include only relevant products and utilize skin tone bucket
df_rel <- df %>%
  filter(secondary_category %in% c(
    "Moisturizers","Treatments","Cleansers",
    "Masks","Sunscreen","Self Tanners")) %>%
  mutate(
    skin_tone_bucket = case_when(
      skin_tone %in% c("fair", "porcelain", "fairLight") ~ "Fair",
      skin_tone %in% c("light", "lightMedium") ~ "Light",
      skin_tone %in% c("medium", "mediumTan", "olive") ~ "Medium",
      skin_tone %in% c("tan") ~ "Tan",
      skin_tone %in% c("deep", "rich", "dark") ~ "Deep",
      TRUE ~ NA_character_
    )
  )

# user demographic dummy matrix
user_demo <- df_rel %>%
  select(author_id, skin_type, skin_tone_bucket) %>%
  distinct(author_id, .keep_all = TRUE) %>%   # 1 row per user
  fastDummies::dummy_cols(
    select_columns = c("skin_type", "skin_tone_bucket"),
    remove_selected_columns = TRUE
  )

# category preference dummy matrix
cat_pref <- df_rel %>%
  group_by(author_id, secondary_category) %>%
  summarise(n = n(), .groups = "drop") %>% # counts of reviews
  group_by(author_id) %>%
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>%
  select(author_id, secondary_category, freq) %>%
  tidyr::pivot_wider(
    names_from = secondary_category,
    values_from = freq,
    values_fill = 0
  )

# Final user vectors matrix (demographics + preference)
user_vectors <- user_demo %>%
  inner_join(cat_pref, by = "author_id") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

#' @description: Persona Function
#' Create a fake persona vector with same structure as user vectors
#' @param user_vectors
#' Full user feature matrix
#' @param skin_type Persons skin type as a string
#' @param skin_tone Persons skin tone as a string
#' @return Data frame
#' One row dataframe displaying the persona as a feature vector

build_persona_vec <- function(user_vectors, skin_type, skin_tone) {
  vec <- user_vectors %>% slice(1) %>% mutate(across(everything(), ~0))
  
  st_col <- paste0("skin_type_", tolower(skin_type)) # form column names
  tone_col <- paste0("skin_tone_bucket_", skin_tone)
  
  if (st_col %in% names(vec)) vec[[st_col]] <- 1
  if (tone_col %in% names(vec)) vec[[tone_col]] <- 1
  
  return(vec)
}

#' @description: Recommendation function
#' Identifies top most similar user to a specific persona then recommends the
#' products those users rated highly. Filtered by category and price.
#' @param df_rel filtered dataset with relevant products/categories
#' @param user_vectors 
#' @param persona_skin_type user skin type
#' @param persona_skin_tone user skin tone
#' @param category product type/category (e.g. moisturizer)
#' @param price_min minimum price for product
#' @param price_max maximum price for product
#' @param top_k_users number of users extracted for the similarity matrix
#' @param n_recs number of recommendations desired
#' @param min_reviews minimum number of reviews needed for product to count
#' @return Tibble
#' Displays recommended products for a particular demographic with respect to
#' price and the specific product.

recommend_similar_products <- function(df_rel,
                                       user_vectors,
                                       persona_skin_type,
                                       persona_skin_tone,
                                       category = NULL,
                                       price_min = 0,
                                       price_max = Inf,
                                       top_k_users = 500,
                                       n_recs = 10,
                                       min_reviews = 5) {
  
  # persona vector
  persona_vec <- build_persona_vec(
    user_vectors,
    persona_skin_type,
    persona_skin_tone
  )
  
  # cosine similarity
  mat <- as.matrix(user_vectors %>% select(-author_id))
  persona_mat <- as.numeric(persona_vec %>% select(-author_id))
  sim_scores <- apply(mat, 1, cosine, y = persona_mat)
  
  # top similar users
  top_users <- user_vectors$author_id[
    order(sim_scores, decreasing = TRUE)][1:top_k_users]
  
  # filter reviews from similar users
  df_sub <- df_rel %>%
    filter(author_id %in% top_users,
           price_usd >= price_min,
           price_usd <= price_max)
  
  if (!is.null(category)) {
    df_sub <- df_sub %>%
      filter(secondary_category == category)
  }
  
  # calculate recommendations
  recs <- df_sub %>%
    group_by(product_id, product_name, brand_name, price_usd) %>%
    summarise(
      n_reviews = n(),
      mean_rating = mean(rating),
      .groups = "drop"
    ) %>%
    filter(n_reviews >= min_reviews) %>%
    arrange(desc(mean_rating), desc(n_reviews)) %>%
    slice_head(n = n_recs)
  
  return(recs)
}


recommend_similar_products(df_rel, user_vectors,
                           persona_skin_type = "combination",
                           persona_skin_tone = "Deep",
                           price_min = 0,
                           price_max = 20,
                           category = "Moisturizers",
                           n_recs = 5
)

recommend_similar_products(df_rel, user_vectors,
                           persona_skin_type = "oily",
                           persona_skin_tone = "Fair",
                           price_min = 0,
                           price_max = 50,
                           category = "Sunscreen",
                           n_recs = 5
)

recommend_similar_products(df_rel, user_vectors,
                           persona_skin_type = "dry",
                           persona_skin_tone = "Medium",
                           price_min = 0,
                           price_max = 15,
                           category = "Cleansers",
                           n_recs = 5
)