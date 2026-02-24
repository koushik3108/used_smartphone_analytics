# used_smartphone_analytics
# 📱 Used Smartphone Price Analytics  
## Exploring Key Drivers of Resale Value  

**Author:** Sai Koushik Soma   

---

## 📌 Project Overview

The resale smartphone market is rapidly expanding as consumers frequently upgrade their devices. However, pricing in the secondary market is often inconsistent and subjective.

This project develops a **data-driven pricing framework** to:

- Predict the resale price of used smartphones  
- Identify key drivers influencing price retention  
- Improve pricing transparency  
- Reduce overpricing and underpricing risk  

The goal is to transform subjective resale pricing into a **standardized, analytics-driven process**.

---

## 🎯 Business Problem

Inconsistent resale pricing creates problems for both sellers and buyers:

- Sellers risk overpricing (unsold inventory)  
- Sellers risk underpricing (lost profit)  
- Buyers lack transparency in feature-to-price relationships  
- Market inefficiencies reduce trust and transaction volume  

A predictive pricing system is needed to improve fairness, profitability, and confidence.

---

## 🎯 Business Goals

✔ Reduce prediction error by at least 10–15%  
✔ Increase seller profitability  
✔ Improve pricing transparency  
✔ Standardize pricing across similar devices  
✔ Justify ≥ 80% of pricing decisions using data  

---

## 📊 Dataset Information

- **3,454 smartphones**
- 15 features
- Target variable: `normalized_used_price`

### 🔹 Key Features

**Device Characteristics**
- device_brand
- os
- screen_size
- weight

**Technical Specifications**
- rear_camera_mp
- front_camera_mp
- internal_memory
- ram
- battery

**Usage & Time Factors**
- release_year
- days_used

**Network Compatibility**
- 4g
- 5g

**Pricing**
- normalized_new_price
- normalized_used_price (Target)

---

## 🧹 Data Preprocessing

✔ No duplicate records  
✔ 202 original missing values  
✔ 775 out-of-range values flagged  
✔ 39 legitimate zero front-camera values retained  

### Cleaning Steps:
- Applied realistic range rules
- Converted out-of-range values to NA
- Applied **KNN imputation (k = 5)**
- Encoded categorical variables as factors
- Verified no remaining missing values

---

## 📊 Exploratory Insights

### 🔹 Strongest Numeric Predictors (Pearson Correlation)

| Predictor | Correlation with Used Price |
|------------|----------------------------|
| normalized_new_price | 0.834 |
| screen_size | 0.676 |
| battery | 0.615 |
| weight | 0.609 |
| front_camera_mp | 0.608 |
| rear_camera_mp | 0.578 |
| release_year | 0.510 |
| days_used | -0.358 |

### 🔹 Significant Categorical Predictors (ANOVA)

- 4G Support (p < 1e-272)
- Device Brand (p < 1e-207)
- OS (p < 1e-117)
- 5G Support (p < 1e-78)

### 🔹 Multicollinearity Check

All VIF values < 5 → No severe multicollinearity  
Dimensionality reduction (PCA) not required.

---

## 🤖 Models Tested

- Linear Regression (Baseline)
- Decision Tree
- Random Forest
- K-Nearest Neighbors (KNN)

---

## 📈 Model Performance

### 🔹 Validation & Holdout Performance (R²)

| Model | Validation R² | Holdout R² |
|--------|---------------|------------|
| **Linear Regression** | **0.824** | **0.805** |
| Random Forest | 0.819 | 0.807 |
| KNN | 0.733 | 0.736 |
| Decision Tree | 0.707 | 0.686 |

---

## 🏆 Final Model Selection

### ✅ Linear Regression
- Most interpretable
- Explains >80% of variance
- Stable residual distribution
- Business-friendly

### ✅ Random Forest
- Captures nonlinear interactions
- Robust performance
- Strong feature importance insights

Decision Tree and KNN showed weaker generalization.

---

## 🔍 Key Findings

1. **New price is the strongest predictor**  
2. Larger screens, higher battery, and better cameras increase resale value  
3. Newer devices retain more value  
4. More usage days reduce resale price  
5. Brand and 4G/5G capability significantly influence pricing  

---

## 🚀 Business Impact

This pricing model enables:

- Objective resale valuation
- Reduced pricing variance
- Higher seller profitability
- Increased buyer trust
- Transparent feature-to-price explanation

The framework improves efficiency in the resale smartphone market.

---

## 🛠️ Tech Stack

- R
- Linear Regression
- Random Forest
- Decision Trees
- KNN Regression
- ANOVA
- Pearson Correlation
- VIF Analysis
- KNN Imputation
- ggplot2 Visualizations

---

## 📌 Conclusion

This project successfully transforms subjective smartphone resale pricing into a data-driven predictive system.

By combining:

Strong statistical validation

Robust machine learning models

Transparent regression analysis

Practical business insights

The solution enhances pricing accuracy, builds trust, and supports profitability in the resale smartphone market.
