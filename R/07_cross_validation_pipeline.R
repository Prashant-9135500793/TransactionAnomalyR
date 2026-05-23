#' K-Fold Cross-Validation for Fraud Detection
#'
#' @description
#' Evaluates the full PCA + Gradient Descent pipeline using k-fold
#' cross-validation. Training is done on k-1 folds and evaluated on the
#' held-out fold, repeated k times. Reports mean Accuracy, Precision,
#' Recall, F1, and AUC-ROC across all folds.
#'
#' @param prep A list returned by \code{\link{preprocess_kaggle}}.
#' @param k_folds Integer. Number of folds. Default 5.
#' @param n_epochs Integer. Gradient descent epochs per fold. Default 100.
#' @param variance_threshold Numeric. PCA variance threshold per fold. Default 0.90.
#'
#' @return A list with:
#' \describe{
#'   \item{mean_accuracy}{Mean accuracy across folds}
#'   \item{mean_precision}{Mean precision across folds}
#'   \item{mean_recall}{Mean recall (sensitivity)}
#'   \item{mean_f1}{Mean F1 score}
#'   \item{mean_auc}{Mean AUC-ROC}
#'   \item{fold_results}{Data frame with per-fold metrics}
#' }
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard(sample_size = 5000)
#' prep  <- preprocess_kaggle(data)
#' cv_r  <- cross_validate_model(prep, k_folds = 5, n_epochs = 100)
#' cat("Mean AUC:", round(cv_r$mean_auc, 4), "\n")
#' print(cv_r$fold_results)
#' }
#'
#' @export
cross_validate_model <- function(prep,
                                  k_folds            = 5,
                                  n_epochs           = 100,
                                  variance_threshold = 0.90) {

  if (!is.list(prep) || is.null(prep$features)) {
    stop("Input must be the list returned by preprocess_kaggle().")
  }

  n     <- nrow(prep$features)
  folds <- cut(sample(seq_len(n)), breaks = k_folds, labels = FALSE)

  sigmoid <- function(z) 1 / (1 + exp(-pmin(pmax(z, -500), 500)))

  auc_fn <- function(scores, labs) {
    pos <- scores[labs == 1]; neg <- scores[labs == 0]
    if (!length(pos) || !length(neg)) return(0.5)
    mean(outer(pos, neg, ">") + 0.5 * outer(pos, neg, "=="))
  }

  fold_results <- data.frame(
    fold      = seq_len(k_folds),
    accuracy  = NA_real_,
    precision = NA_real_,
    recall    = NA_real_,
    f1        = NA_real_,
    auc       = NA_real_
  )

  for (f in seq_len(k_folds)) {
    tr_i <- which(folds != f); te_i <- which(folds == f)

    tr <- list(features   = prep$features[tr_i, , drop = FALSE],
               labels     = prep$labels[tr_i],
               raw_amount = prep$raw_amount[tr_i])
    te <- list(features   = prep$features[te_i, , drop = FALSE],
               labels     = prep$labels[te_i],
               raw_amount = prep$raw_amount[te_i])

    # Train PCA on training fold only
    pca_tr <- pca_compress(tr, variance_threshold = variance_threshold)
    gd_tr  <- gradient_descent_scorer(pca_tr, n_epochs = n_epochs)

    # Project test fold using training PCA - direct rotation multiply
    rotation  <- pca_tr$pca_model$rotation
    k_tr      <- pca_tr$n_components
    te_scores <- (te$features %*% rotation)[, seq_len(k_tr), drop = FALSE]
    probs <- sigmoid(as.numeric(te_scores %*% gd_tr$weights) + gd_tr$bias)
    preds <- as.integer(probs >= 0.5)
    l     <- te$labels

    tp <- sum(preds == 1 & l == 1); fp <- sum(preds == 1 & l == 0)
    tn <- sum(preds == 0 & l == 0); fn <- sum(preds == 0 & l == 1)
    pr <- tp / (tp + fp + 1e-10); re <- tp / (tp + fn + 1e-10)

    fold_results$accuracy[f]  <- (tp + tn) / (tp + fp + tn + fn + 1e-10)
    fold_results$precision[f] <- pr
    fold_results$recall[f]    <- re
    fold_results$f1[f]        <- 2 * pr * re / (pr + re + 1e-10)
    fold_results$auc[f]       <- auc_fn(probs, l)

    message(sprintf("Fold %d/%d - Accuracy: %.3f | F1: %.3f | AUC: %.3f",
                    f, k_folds,
                    fold_results$accuracy[f],
                    fold_results$f1[f],
                    fold_results$auc[f]))
  }

  list(
    mean_accuracy  = mean(fold_results$accuracy,  na.rm = TRUE),
    mean_precision = mean(fold_results$precision, na.rm = TRUE),
    mean_recall    = mean(fold_results$recall,    na.rm = TRUE),
    mean_f1        = mean(fold_results$f1,        na.rm = TRUE),
    mean_auc       = mean(fold_results$auc,       na.rm = TRUE),
    fold_results   = fold_results
  )
}


#' Run the Full Fraud Detection Pipeline
#'
#' @description
#' Convenience wrapper that runs all 7 steps in sequence:
#' preprocess → PCA → EM → Gradient Descent → kNN → Bootstrap → CLT → Cross-Validation.
#'
#' @param data A data frame from \code{\link{load_creditcard}}.
#' @param n_epochs Integer. Gradient descent epochs. Default 200.
#' @param bootstrap_B Integer. Bootstrap resamples. Default 150.
#' @param k_folds Integer. Cross-validation folds. Default 5.
#' @param variance_threshold Numeric. PCA variance threshold. Default 0.95.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return A named list with all pipeline outputs:
#'   \code{prep}, \code{pca}, \code{em}, \code{gd}, \code{knn},
#'   \code{bootstrap}, \code{clt}, \code{cv}.
#'
#' @examples
#' \dontrun{
#' data     <- load_creditcard(sample_size = 5000)
#' pipeline <- full_pipeline(data)
#' cat("AUC:", pipeline$cv$mean_auc, "\n")
#' launch_dashboard(pipeline)
#' }
#'
#' @export
full_pipeline <- function(data,
                           n_epochs           = 200,
                           bootstrap_B        = 150,
                           k_folds            = 5,
                           variance_threshold = 0.95,
                           verbose            = TRUE) {

  if (verbose) message("\n=== TransactionAnomalyR: Full Pipeline ===")

  if (verbose) message("Step 1/7: Preprocessing features...")
  prep  <- preprocess_kaggle(data)

  if (verbose) message("Step 2/7: PCA compression...")
  pca_r <- pca_compress(prep, variance_threshold = variance_threshold)

  if (verbose) message("Step 3/7: EM mixture model...")
  em_r  <- em_fraud_model(pca_r)

  if (verbose) message("Step 4/7: Gradient Descent scorer...")
  gd_r  <- gradient_descent_scorer(pca_r, n_epochs = n_epochs)

  if (verbose) message("Step 5/7: kNN anomaly scoring...")
  knn_r <- knn_anomaly_score(pca_r)

  if (verbose) message("Step 6/7: Bootstrap threshold + CLT...")
  boot_r <- bootstrap_threshold(gd_r$fraud_scores, gd_r$labels, B = bootstrap_B)
  clt_r  <- clt_amount_ci(prep$raw_amount, prep$labels)

  if (verbose) message("Step 7/7: Cross-validation...")
  cv_r  <- cross_validate_model(prep, k_folds = k_folds, n_epochs = n_epochs,
                                  variance_threshold = variance_threshold)

  if (verbose) {
    message("\n========== RESULTS ==========")
    message(sprintf("Dataset   : %d rows, %.3f%% fraud", nrow(data), mean(data$is_fraud)*100))
    message(sprintf("PCA comps : %d", pca_r$n_components))
    message(sprintf("Threshold : %.3f  [%.3f, %.3f]",
                    boot_r$optimal_threshold, boot_r$ci_lower, boot_r$ci_upper))
    message(sprintf("AUC-ROC   : %.4f", cv_r$mean_auc))
    message(sprintf("F1 Score  : %.4f", cv_r$mean_f1))
    message(sprintf("Recall    : %.4f", cv_r$mean_recall))
  }

  list(prep      = prep,
       pca       = pca_r,
       em        = em_r,
       gd        = gd_r,
       knn       = knn_r,
       bootstrap = boot_r,
       clt       = clt_r,
       cv        = cv_r,
       data      = data)
}
