# TransactionAnomalyR 1.0.0

## Initial Release

* `load_creditcard()` — robust CSV loader with automatic path detection
* `preprocess_kaggle()` — z-score normalisation + cyclical hour encoding
* `pca_compress()` — variance-threshold PCA compression
* `em_fraud_model()` — numerically stable EM Gaussian mixture model
* `gradient_descent_scorer()` — mini-batch logistic regression with class weighting
* `knn_anomaly_score()` — kNN anomaly scoring in PCA space
* `bootstrap_threshold()` — bootstrap F1-optimal threshold with CLT CI
* `clt_amount_ci()` — CLT confidence intervals for transaction amounts
* `cross_validate_model()` — k-fold cross-validation (no leakage)
* `full_pipeline()` — one-call complete pipeline
* `train_test_split()` — stratified train/test split
* `train_model()` / `predict_on_test()` / `print_test_results()` — clean train-evaluate workflow
* `plot_fraud_space()`, `plot_roc_curve()`, `plot_confusion_matrix()`, `plot_amount_distribution()` — ggplot2 visualisations
* `launch_dashboard()` — full Shiny dashboard with interactive controls
