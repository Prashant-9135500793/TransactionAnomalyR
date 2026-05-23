#' kNN Anomaly Scoring
#'
#' @description
#' Computes an anomaly score for each transaction using k-Nearest Neighbors.
#' Each transaction's score is the mean Euclidean distance to its k nearest
#' neighbours in PCA space. High scores = dissimilar from neighbours = anomalous.
#'
#' For performance on large datasets, a random sample of up to 2000 points
#' is used.
#'
#' @param pca_result A list returned by \code{\link{pca_compress}}.
#' @param k Integer. Number of nearest neighbours. Default 10.
#' @param max_sample Integer. Maximum points to use (for speed). Default 2000.
#'
#' @return A list with:
#' \describe{
#'   \item{dissimilarity}{Numeric vector: anomaly scores normalised to [0,1]}
#'   \item{labels}{True labels for sampled points}
#'   \item{sampled_idx}{Indices of sampled points in original dataset}
#' }
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard()
#' prep  <- preprocess_kaggle(data)
#' pca_r <- pca_compress(prep)
#' knn_r <- knn_anomaly_score(pca_r, k = 10)
#' boxplot(knn_r$dissimilarity ~ knn_r$labels)
#' }
#'
#' @export
knn_anomaly_score <- function(pca_result, k = 10, max_sample = 2000) {

  if (!is.list(pca_result) || is.null(pca_result$scores)) {
    stop("Input must be the list returned by pca_compress().")
  }

  X      <- pca_result$scores
  labels <- pca_result$labels
  n_use  <- min(nrow(X), max_sample)

  set.seed(42)
  idx <- sample(seq_len(nrow(X)), n_use)
  Xs  <- X[idx, , drop = FALSE]
  ys  <- labels[idx]

  scores <- sapply(seq_len(n_use), function(i) {
    dists  <- sqrt(rowSums(sweep(Xs[-i, , drop = FALSE], 2, Xs[i, ], "-")^2))
    mean(sort(dists)[seq_len(min(k, length(dists)))])
  })

  scores_norm <- (scores - min(scores)) / (max(scores) - min(scores) + 1e-10)

  list(
    dissimilarity = scores_norm,
    labels        = ys,
    sampled_idx   = idx
  )
}


#' Bootstrap Threshold Estimation
#'
#' @description
#' Uses bootstrap resampling to find the optimal decision threshold that
#' maximises the F1-score, and constructs a 95\% confidence interval around
#' it using the Central Limit Theorem.
#'
#' @param fraud_scores Numeric vector of predicted fraud probabilities.
#' @param labels Integer vector of true labels (0/1).
#' @param B Integer. Number of bootstrap resamples. Default 150.
#' @param alpha Numeric. Significance level for CI. Default 0.05.
#'
#' @return A list with:
#' \describe{
#'   \item{optimal_threshold}{Bootstrap mean of the best threshold}
#'   \item{ci_lower}{Lower bound of confidence interval}
#'   \item{ci_upper}{Upper bound of confidence interval}
#'   \item{bootstrap_thresholds}{All B threshold estimates}
#'   \item{false_positive_rate}{FPR at the optimal threshold}
#' }
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard()
#' prep  <- preprocess_kaggle(data)
#' pca_r <- pca_compress(prep)
#' gd_r  <- gradient_descent_scorer(pca_r)
#' bt    <- bootstrap_threshold(gd_r$fraud_scores, gd_r$labels, B = 100)
#' cat("Threshold:", bt$optimal_threshold, "\n")
#' cat("95% CI: [", bt$ci_lower, ",", bt$ci_upper, "]\n")
#' }
#'
#' @export
bootstrap_threshold <- function(fraud_scores, labels, B = 150, alpha = 0.05) {

  if (length(fraud_scores) != length(labels)) {
    stop("fraud_scores and labels must have the same length.")
  }

  find_best_t <- function(s, l) {
    best_f1 <- -1; best_t <- 0.5
    for (t in seq(0.1, 0.9, by = 0.05)) {
      p  <- as.integer(s >= t)
      tp <- sum(p == 1 & l == 1)
      fp <- sum(p == 1 & l == 0)
      fn <- sum(p == 0 & l == 1)
      pr <- tp / (tp + fp + 1e-10)
      re <- tp / (tp + fn + 1e-10)
      f1 <- 2 * pr * re / (pr + re + 1e-10)
      if (f1 > best_f1) { best_f1 <- f1; best_t <- t }
    }
    best_t
  }

  n  <- length(fraud_scores)
  bt <- sapply(seq_len(B), function(b) {
    i <- sample(n, n, replace = TRUE)
    find_best_t(fraud_scores[i], labels[i])
  })

  opt    <- mean(bt)
  se     <- stats::sd(bt) / sqrt(B)
  z_crit <- stats::qnorm(1 - alpha / 2)
  fpr    <- sum(as.integer(fraud_scores >= opt) == 1 & labels == 0) /
              (sum(labels == 0) + 1e-10)

  list(
    optimal_threshold    = opt,
    ci_lower             = opt - z_crit * se,
    ci_upper             = opt + z_crit * se,
    bootstrap_thresholds = bt,
    false_positive_rate  = fpr
  )
}
