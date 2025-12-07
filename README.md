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
Overview 
# Estimation 

estimate_arrival_rates(data) 
converts raw trip logs into hourly rate estimates (mu_hat) between station pairs 

# Simulation 

Extract_hour(estimates, origin, dest) 
pulls out the hourly rate vector for a station pair. 

Sim_station_pair(hourly_rates, origin, dest) 
takes the hourly rates for one origin–destination pair and generates 
a realistic set of random trip times for that pair 

Sim_full_day(estimates) 
runs through allstation pairs and returns a combined day of demand.

# Optimization 

Sim_trips_one_day(demand_day, placement) 
marks each trip as successful or failed given starting bike counts. 

evaluate_placement(demand_list, placement) 
computes average failed trips per day. 

optimize_bikes_greedy(demand_list, estimates, total_bikes) 
uses a greedy method to allocate a fixed fleet of bikes across stations to 
minimize failures 

--- 
## Testing 

If you run source("testing.R") and get no errors, the basic logic is working. 

--- 
## Significance 

Bike share systems face variable demand across stations and hours. 
This project brings together demand estimation, simulation of realistic events, 
and optimizing limited resources to assist with meeting demand. 