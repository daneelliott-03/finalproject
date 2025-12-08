## Final Project: Product Recommendation System

**Author:** Dane Elliott & Sophie Gideon 
**Date:** 12/10/2025 
**Purpose:** To build and test a workflow for understanding how skincare product
ratings differ across demographics and for generating personalized product recommendations.

This project Analyzes basic rating patterns across skin tones and types and builds a 
personalized recommendation system that uses demographic matching and cosine 
similarity to recommend the best products for a specific user profile.

--- 
## Quick Start 

```r
1. Install required packages:

   install.packages(c("tidyverse", "dplyr", "ggplot2" kableExtra", "fastDummies"))

2. Load the code

source("reviews.R")
source("demographic_analysis.R")
source("keywords.R")
source("recommendation_system.R")
source("sephora_data_project_example.R")


3. Download data
products <- read_csv("product_info.csv")
reviews_0_250 <- read_csv("reviews_0-250.csv")
reviews_250_500 <- read_csv("reviews_250-500.csv")
reviews_500_750 <- read_csv("reviews_500-750.csv")
reviews_750_1250 <- read_csv("reviews_750-1250.csv")
reviews_1250_end <- read_csv("reviews_1250-end.csv")

4. Run the main pipeline

recommend_similar_products(df_rel, user_vectors,
                           persona_skin_type = "combination",
                           persona_skin_tone = "Deep",
                           price_min = 0,
                           price_max = 20,
                           category = "Moisturizers",
                           n_recs = 5
)

```

---
## Repo Structure 
Final Project: Product Recommendation System/ 
├── reviews.R # Merging reviews and products; adding category variables
├── demographic_analysis.R # Summarize_ratings_by_demographic, rate stability
├── keywords.R # tracks issues across groups using keywords
├── recommendation_system.R # Cosine similarity computation for final recs
├── sephora_data_project_example.R # visuals and example cases
├── testing.R
└── README.md # Project Overview (this file)

---
## Modules 

# 1. reviews.R 
Data Preparation
* Merges review data with product metadata
* Adds category variables (major_category, is_complexion) 
* Functions needed to summarizes ratings by product and category

# 2. demographic_analysis.R 
Demographic Rating Comparions
* summarize_ratings_by_demographic()
  * Calculates mean, sd, low/high ratings by skin tone and type
* compute_within_product_gaps()
  * Measures rating gaps across demograpgic groups within each product
Rating Stability Across Demographics
* Computes rating across skin types and tones at the category level
* Classifies products as stable, moderate, or volatile based on rating gaps
  * Finds products with biggest gaps

# 3. keywords.R
Demographic Disparity Analysis
*Identifies products with the largest rating gaps between demographic groups
* Extracts reviews for “volatile” products to understand why there are gaps.
* Uses a keyword text analysis to compare review language across groups 
  * e.g., “white cast,” “ashy,” “grey,” “chalky”

# 4. recommendation_system.R
Predictive Model
* Logistic regression to estimte probability of giving a high rating 
* Interaction between demographic and product category
Final Similarity Recommendation Pipeline
* Forms demographic and behavior user vectors
* Computes cosine similarity to find users most similar to a persona
* Collects top user ratings to generate personalized recs
* Allows for filtering by category (e.g. moisturizer), price (0-40usd), and min reviews

# 5. sephora_data_project_example.R
*Basic visualizations of rating averages in the initial dataset.
*disparity_reviews.R - graph displaying keywords
*User examples of each function.

--- 
## Testing 

If you run source("testing.R") and get no errors, the basic logic is working. 

--- 
## Significance 

Beauty and skincare products are used by a wide variety of people, but most
products are marketed broadly without consideration for different skin tones
and skin types. This project analyzes various Sephora products and their reviews,
focusing on factors such as skin tone, skin type, and product category in order 
to understand how ratings vary across demographics. By accounting for these 
characteristics, the pipeline is able to determine the most optimal products for 
each user while also allowing for price adjustment.
