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

   install.packages(c("tidyverse", "dplyr", "ggplot2", "kableExtra", "fastDummies"))

2. Load the code

source("1_review/reviews.R")
source("1_review/sephora_reviewer_discrepancy.R")

source("2_demographics/demographic_analysis.R")
source("2_demographics/sephora_run_analysis.R")
source("2_demographics/sephora_visual_disparity_analysis.R") # sub file

source("3_keywords/keywords.R")
source("3_keywords/sephora_keywords_example_visuals.R") # sub file

source("4_recommendation_models/regression.R")
source("4_recommendation_models/recommendation_system.R")

source("5_util/data_examples.R")
source("5_util/test.R")


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
├── 1_review # Merging reviews and products; standardize variables
├── 2_demographics # analyzes rating differences across skin tones/types; identifies product/category level gaps
├── 3_keywords # extracts key word patterns and tests rating disparities
├── recommendation_system.R # computes cosine similarity computation for final recs
├── 5_utils # example outputs and testing
└── README.md # Project Overview (this file)

---
## Modules 

# 1_review
reviews.R
* Merges review data with product metadata
* Standardize and select certain variables

# 2_demographics
demographic_analysis.R & sephora_run_analysis.R
* Computes rating summaries by demographic group across categories and products
* Identifies products with large rating gaps across demographics
* Classifies products as stable, moderate or volatile based on demographic rating differences

# 3_keywords
keywords.R
* Determines how often each keyword appears in review text
* Conducts a t-test comparing mean ratings between demographic groups
* Flags products whose reviews contain specific keywords
* Determine whether specific groups tend to rate keyword associated products differently
  * e.g., “white cast,” “ashy,” “grey,” “chalky”

# 4_recommendation_models
regression.R & recommendation_system.R
* Logistic regression to estimte probability of giving a high rating 
* Forms demographic and behavior user vectors
* Computes cosine similarity to find users most similar to a persona
* Collects top user ratings to generate personalized recs
* Allows for filtering by category (e.g. moisturizer), price (0-40usd), and min reviews

# 5_util
data_examples.R & tests.R
* User examples and tests of each function.

--- 
## Significance 

Beauty and skincare products are used by a wide variety of people, but most
products are marketed broadly without consideration for different skin tones
and skin types. This project analyzes various Sephora products and their reviews,
focusing on factors such as skin tone, skin type, and product category in order 
to understand how ratings vary across demographics. By accounting for these 
characteristics, the pipeline is able to determine the most optimal products for 
each user while also allowing for price adjustment.
