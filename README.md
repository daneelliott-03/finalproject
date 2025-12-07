## Final Project: Product Recommendation System

**Author:** Dane Elliott & Sophie Gideon 
**Date:** 12/10/2025 
**Purpose:** To build and test a workflow for estimating bike share
station demand, simulate daily trips, and optimizing bike allocations. 

--- 
## Quick Start 

Install required packages:
```r
   install.packages(c("tidyverse", "dplyr", "kableExtra", "fastDummies"))

2. Load the code

source("reviews.R")
source("demographic_analysis.R")
source("similarpersonarecs.R")
source("regression.R")
source("stability.R")
source("recommendation_system.R")


3. Prepare data
products <- read_csv("product_info.csv")
reviews_0_250 <- read_csv("reviews_0-250.csv")
reviews_250_500 <- read_csv("reviews_250-500.csv")
reviews_500_750 <- read_csv("reviews_500-750.csv")
reviews_750_1250 <- read_csv("reviews_750-1250.csv")
reviews_1250_end <- read_csv("reviews_1250-end.csv")

df <- merge_product_reviews(products, reviews)
df <- add_category_variables(df)


4. Run the pipeline


```

---
## Repo Structure 
Final Project: Product Recommendation System/ 
├── reviews.R # Merging reviews and products; adding category variables
├── demographic_analysis.R # Summarize_ratings_by_demographic, compute gaps
├── similarpersonarecs.R # Simple filtered top N persona recommendations
├── regression.R # Logistic regression for rating probabilities
├── stability.R #  
├── recommendation_system.R # Cosine similarity computation for final recs
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

# 3. similarpersonarecs.R
Baseline Recommendations
* Filters by skin type, tone, and category
* Produces top products for a given demographic

# 4. regression.R
Predictive Model
* Logistic regression to estimte probability of giving a high rating 
* Interaction between demographic and product category

# 5. stability.R
Rating Stability Across Demographics
* Computes rating across skin types and tones at the category level
* Classifies products as stable, moderate, or volatile based on rating gaps
  * Finds products with biggest gaps

# 6. recommendation_system.R
Final Similarity Recommendation Pipeline
* Forms demographic and behavior user vectors
* Computes cosine similarity to find users most similar to a persona
* Collects top user ratings to generate personalized recs
* Allows for filtering by category (e.g. moisturizer), price (0-40usd), and min reviews

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
