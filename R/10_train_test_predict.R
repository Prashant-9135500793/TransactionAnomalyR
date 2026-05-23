#' Split Data into Train and Test Sets (Stratified)
#'
#' @description
#' Splits the loaded creditcard data into training and test sets using
#' stratified sampling - ensures both sets maintain the same fraud ratio.
#' The model is trained ONLY on training data, then evaluated on test data
#' which the model has never seen.
#'
#' @param data A data frame from \code{\link{load_creditcard}}.
#' @param train_ratio Numeric. Proportion for training. Default 0.8 (80/20 split).
#' @param seed Integer. Random seed for reproducibility. Default 42.
#'
#' @return A list with:
#' \describe{
#'   \item{train}{Training data frame (80% of data)}
#'   \item{test}{Test data frame (20% of data - never seen by model)}
#'   \item{train_idx}{Row indices of training set}
#'   \item{test_idx}{Row indices of test set}
#' }
#'
#' @examples
#' \dontrun{
#' data   <- load_creditcard()
#' splits <- train_test_split(data, train_ratio = 0.8)
#' cat("Train rows:", nrow(splits$train), "\n")
#' cat("Test rows:",  nrow(splits$test),  "\n")
#' cat("Train fraud rate:", mean(splits$train$is_fraud), "\n")
#' cat("Test  fraud rate:", mean(splits$test$is_fraud),  "\n")
#' }
#'
#' @export
train_test_split <- function(data, train_ratio = 0.8, seed = 42) {

  set.seed(seed)

  # Stratified split: sample separately from fraud and normal
  fraud_idx  <- which(data$is_fraud == 1)
  normal_idx <- which(data$is_fraud == 0)

  n_fraud_train  <- round(length(fraud_idx)  * train_ratio)
  n_normal_train <- round(length(normal_idx) * train_ratio)

  train_fraud  <- sample(fraud_idx,  n_fraud_train)
  train_normal <- sample(normal_idx, n_normal_train)
  train_idx    <- sort(c(train_fraud, train_normal))
  test_idx     <- setdiff(seq_len(nrow(data)), train_idx)

  train <- data[train_idx, ]
  test  <- data[test_idx,  ]
  rownames(train) <- NULL
  rownames(test)  <- NULL

  message(sprintf(
    "Train: %d rows | %d fraud (%.3f%%) | %d normal",
    nrow(train), sum(train$is_fraud == 1),
    mean(train$is_fraud) * 100, sum(train$is_fraud == 0)
  ))
  message(sprintf(
    "Test:  %d rows | %d fraud (%.3f%%) | %d normal",
    nrow(test), sum(test$is_fraud == 1),
    mean(test$is_fraud) * 100, sum(test$is_fraud == 0)
  ))

  list(train     = train,
       test      = test,
       train_idx = train_idx,
       test_idx  = test_idx)
}


#' Train Model on Training Data Only
#'
#' @description
#' Runs the full pipeline (PCA + EM + GD + kNN + Bootstrap + CLT + CV)
#' exclusively on the training set. The returned model object is then
#' used to predict on the unseen test set via \code{\link{predict_on_test}}.
#'
#' @param train_data Training data frame from \code{\link{train_test_split}}.
#' @param n_epochs Integer. GD epochs. Default 200.
#' @param bootstrap_B Integer. Bootstrap resamples. Default 150.
#' @param k_folds Integer. CV folds (on training data only). Default 5.
#' @param variance_threshold Numeric. PCA variance. Default 0.95.
#' @param verbose Logical. Print progress. Default TRUE.
#'
#' @return A trained pipeline list (same as \code{\link{full_pipeline}}).
#'
#' @examples
#' \dontrun{
#' data    <- load_creditcard()
#' splits  <- train_test_split(data)
#' trained <- train_model(splits$train)
#' results <- predict_on_test(trained, splits$test)
#' }
#'
#' @export
train_model <- function(train_data,
                         n_epochs           = 200,
                         bootstrap_B        = 150,
                         k_folds            = 5,
                         variance_threshold = 0.95,
                         verbose            = TRUE) {

  if (verbose) message("\n=== Training on TRAIN set only ===")
  full_pipeline(train_data,
                n_epochs           = n_epochs,
                bootstrap_B        = bootstrap_B,
                k_folds            = k_folds,
                variance_threshold = variance_threshold,
                verbose            = verbose)
}


#' Predict Fraud on Unseen Test Data
#'
#' @description
#' Applies a trained pipeline to the test set - data the model has NEVER seen.
#' Uses the training PCA model to project test features, then the trained
#' GD weights to score each test transaction. Evaluates with full metrics.
#'
#' @param trained_pipeline A pipeline from \code{\link{train_model}}.
#' @param test_data Test data frame from \code{\link{train_test_split}}.
#'
#' @return A list with:
#' \describe{
#'   \item{fraud_scores}{Predicted fraud probabilities for each test row}
#'   \item{predictions}{Binary predictions (0/1) at optimal threshold}
#'   \item{true_labels}{True labels from test data}
#'   \item{threshold}{Bootstrap threshold from training}
#'   \item{accuracy}{Test set accuracy}
#'   \item{precision}{Test set precision}
#'   \item{recall}{Test set recall (sensitivity)}
#'   \item{f1}{Test set F1 score}
#'   \item{auc}{Test set AUC-ROC}
#'   \item{confusion_matrix}{Data frame with TP, FP, TN, FN}
#'   \item{flagged_transactions}{Test rows flagged as fraud with scores}
#'   \item{clt_flags}{Transactions also flagged by CLT amount test}
#' }
#'
#' @examples
#' \dontrun{
#' data    <- load_creditcard()
#' splits  <- train_test_split(data)
#' trained <- train_model(splits$train)
#' results <- predict_on_test(trained, splits$test)
#' print_test_results(results)
#' }
#'
#' @export
predict_on_test <- function(trained_pipeline, test_data) {

  message("\n=== Predicting on TEST set (unseen data) ===")

  pipe  <- trained_pipeline
  prep  <- preprocess_kaggle(test_data)

  # Project test features using TRAINING PCA - no leakage
  # Use direct rotation multiply (bypasses predict.prcomp name-matching issues)
  rotation    <- pipe$pca$pca_model$rotation
  k           <- pipe$pca$n_components
  test_scores <- (prep$features %*% rotation)[, seq_len(k), drop = FALSE]

  # Score using TRAINING GD weights
  sigmoid <- function(z) 1 / (1 + exp(-pmin(pmax(z, -500), 500)))
  fraud_scores <- sigmoid(as.numeric(test_scores %*% pipe$gd$weights) +
                            pipe$gd$bias)

  # Apply bootstrap threshold from training
  threshold   <- pipe$bootstrap$optimal_threshold
  predictions <- as.integer(fraud_scores >= threshold)
  true_labels <- prep$labels

  # Confusion matrix
  tp <- sum(predictions == 1 & true_labels == 1)
  fp <- sum(predictions == 1 & true_labels == 0)
  tn <- sum(predictions == 0 & true_labels == 0)
  fn <- sum(predictions == 0 & true_labels == 1)

  precision <- tp / (tp + fp + 1e-10)
  recall    <- tp / (tp + fn + 1e-10)
  f1        <- 2 * precision * recall / (precision + recall + 1e-10)
  accuracy  <- (tp + tn) / (tp + fp + tn + fn + 1e-10)

  # AUC-ROC on test set
  pos <- fraud_scores[true_labels == 1]
  neg <- fraud_scores[true_labels == 0]
  auc <- mean(outer(pos, neg, ">") + 0.5 * outer(pos, neg, "=="))

  # CLT amount check on flagged transactions
  flagged_idx <- which(predictions == 1)
  clt_r       <- clt_amount_ci(prep$raw_amount, true_labels)
  z_scores    <- (test_data$amount - clt_r$normal_mean) / clt_r$normal_sd
  clt_flags   <- which(abs(z_scores) > 1.96)

  # Build flagged transactions table - include ALL V1-V28 columns
  all_cols   <- c("time", "amount", paste0("V", 1:28), "is_fraud")
  flagged_df <- test_data[flagged_idx, ]  # keep ALL columns V1-V28
  flagged_df$fraud_score    <- round(fraud_scores[flagged_idx], 4)
  flagged_df$predicted      <- 1L
  flagged_df$correct        <- as.integer(flagged_df$is_fraud == 1)
  flagged_df <- flagged_df[order(-flagged_df$fraud_score), ]
  rownames(flagged_df) <- NULL

  message(sprintf("Test Accuracy  : %.4f", accuracy))
  message(sprintf("Test Precision : %.4f", precision))
  message(sprintf("Test Recall    : %.4f", recall))
  message(sprintf("Test F1        : %.4f", f1))
  message(sprintf("Test AUC-ROC   : %.4f", auc))
  message(sprintf("Threshold used : %.3f  [Bootstrap CI: %.3f - %.3f]",
                  threshold,
                  pipe$bootstrap$ci_lower,
                  pipe$bootstrap$ci_upper))
  message(sprintf("Flagged as fraud : %d transactions", length(flagged_idx)))
  message(sprintf("True fraud in test: %d", sum(true_labels == 1)))
  message(sprintf("Correctly caught  : %d (%.1f%%)", tp,
                  tp / (sum(true_labels == 1) + 1e-10) * 100))

  list(
    fraud_scores         = fraud_scores,
    predictions          = predictions,
    true_labels          = true_labels,
    threshold            = threshold,
    accuracy             = accuracy,
    precision            = precision,
    recall               = recall,
    f1                   = f1,
    auc                  = auc,
    confusion_matrix     = data.frame(TP=tp, FP=fp, TN=tn, FN=fn),
    flagged_transactions = flagged_df,
    clt_flags            = clt_flags,
    clt_result           = clt_r,
    n_test               = nrow(test_data),
    n_fraud_test         = sum(true_labels == 1),
    n_normal_test        = sum(true_labels == 0)
  )
}


#' Print a Clean Test Results Summary
#'
#' @description
#' Prints a formatted summary of test set performance after
#' \code{\link{predict_on_test}}.
#'
#' @param results A list returned by \code{\link{predict_on_test}}.
#'
#' @examples
#' \dontrun{
#' data    <- load_creditcard()
#' splits  <- train_test_split(data)
#' trained <- train_model(splits$train)
#' results <- predict_on_test(trained, splits$test)
#' print_test_results(results)
#' }
#'
#' @export
print_test_results <- function(results) {
  cat("\n")
  cat("============================================\n")
  cat("   TEST SET RESULTS (Unseen Data)\n")
  cat("============================================\n")
  cat(sprintf("  Test rows          : %d\n",   results$n_test))
  cat(sprintf("  True fraud cases   : %d\n",   results$n_fraud_test))
  cat(sprintf("  True normal cases  : %d\n",   results$n_normal_test))
  cat("--------------------------------------------\n")
  cat(sprintf("  Threshold used     : %.3f\n",  results$threshold))
  cat("--------------------------------------------\n")
  cat(sprintf("  AUC-ROC            : %.4f\n",  results$auc))
  cat(sprintf("  Accuracy           : %.4f\n",  results$accuracy))
  cat(sprintf("  Precision          : %.4f\n",  results$precision))
  cat(sprintf("  Recall             : %.4f\n",  results$recall))
  cat(sprintf("  F1 Score           : %.4f\n",  results$f1))
  cat("--------------------------------------------\n")
  cm <- results$confusion_matrix
  cat(sprintf("  True  Positives    : %d\n",    cm$TP))
  cat(sprintf("  False Positives    : %d\n",    cm$FP))
  cat(sprintf("  True  Negatives    : %d\n",    cm$TN))
  cat(sprintf("  False Negatives    : %d\n",    cm$FN))
  cat("--------------------------------------------\n")
  cat(sprintf("  Fraud caught       : %d / %d  (%.1f%%)\n",
              cm$TP, results$n_fraud_test,
              cm$TP / (results$n_fraud_test + 1e-10) * 100))
  cat(sprintf("  False alarms       : %d / %d  (%.2f%%)\n",
              cm$FP, results$n_normal_test,
              cm$FP / (results$n_normal_test + 1e-10) * 100))
  cat("============================================\n")
  cat("\nTop 10 highest-risk flagged transactions:\n")
  print(head(results$flagged_transactions[,
             c("amount","V1","V14","fraud_score","is_fraud","correct")], 10))
}
