# TransactionAnomalyR

Real-time credit card fraud detection using the Kaggle Credit Card Fraud dataset.
Implements 7 statistical/ML techniques from scratch in R.

## Setup

### 1. Get the Kaggle dataset
Download `creditcard.csv` from:
https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud

Place it here:
```
TransactionAnomalyR/
└── data-raw/
    └── creditcard.csv   ← PUT FILE HERE
```

### 2. Install dependencies
```r
install.packages(c("ggplot2", "shiny", "shinydashboard",
                   "DT", "plotly", "dplyr"))
```

### 3. Install package
```r
install.packages("path/to/TransactionAnomalyR_1.0.0.tar.gz",
                 repos = NULL, type = "source")
library(TransactionAnomalyR)
```

---

## Quick Start

```r
library(TransactionAnomalyR)

# Step 1: Load real Kaggle data
data <- load_creditcard()           # reads data-raw/creditcard.csv
# Or with custom path:
data <- load_creditcard(path = "~/Downloads/creditcard.csv")

# Step 2: Run full pipeline (all 7 techniques)
pipeline <- full_pipeline(data)

# Step 3: Launch Shiny dashboard
launch_dashboard(pipeline)
```

---

## Functions

| Function | Technique | Description |
|---|---|---|
| `load_creditcard()` | — | Load & sample Kaggle CSV |
| `preprocess_kaggle()` | Feature Engineering | Normalize V1-V28 + Amount + hour |
| `pca_compress()` | **PCA** | Reduce to key components |
| `em_fraud_model()` | **EM Algorithm** | Soft fraud/normal mixture |
| `gradient_descent_scorer()` | **Gradient Descent** | Weighted logistic regression |
| `knn_anomaly_score()` | **kNN** | Anomaly dissimilarity score |
| `bootstrap_threshold()` | **Bootstrap** | Optimal threshold + 95% CI |
| `clt_amount_ci()` | **CLT** | Amount confidence intervals |
| `cross_validate_model()` | **Cross-Validation** | AUC, F1, Recall |
| `full_pipeline()` | All 7 | Run everything at once |
| `launch_dashboard()` | — | Shiny dashboard |

---

## Step-by-step Usage

```r
data  <- load_creditcard(sample_size = 10000)
prep  <- preprocess_kaggle(data)
pca_r <- pca_compress(prep, variance_threshold = 0.95)
em_r  <- em_fraud_model(pca_r)
gd_r  <- gradient_descent_scorer(pca_r, n_epochs = 200)
knn_r <- knn_anomaly_score(pca_r, k = 10)
bt_r  <- bootstrap_threshold(gd_r$fraud_scores, gd_r$labels, B = 150)
clt_r <- clt_amount_ci(prep$raw_amount, prep$labels)
cv_r  <- cross_validate_model(prep, k_folds = 5)

# Plots
print(plot_fraud_space(pca_r, em_r))
print(plot_roc_curve(gd_r$fraud_scores, gd_r$labels))
print(plot_confusion_matrix(gd_r$fraud_scores, gd_r$labels, threshold = bt_r$optimal_threshold))
print(plot_amount_distribution(prep$raw_amount, prep$labels, clt_r))
```

---

## Dataset

- **Source:** Kaggle — ULB Machine Learning Group
- **Size:** 284,807 transactions, 492 fraud (0.172%)
- **Features:** V1–V28 (PCA-anonymized), Amount, Time
- **Period:** September 2013, European cardholders

**Reference:** Dal Pozzolo et al. (2015). *Calibrating Probability with Undersampling for Unbalanced Classification.* IEEE CIDM.
# TransactionAnomalyR
