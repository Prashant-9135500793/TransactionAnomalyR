#' EM Algorithm for Fraud Mixture Model
#'
#' @description
#' Fits a Gaussian Mixture Model using the Expectation-Maximization algorithm.
#' Numerically stable implementation - handles high-dimensional PCA spaces
#' from real Kaggle data without NaN errors.
#'
#' @param pca_result A list returned by \code{\link{pca_compress}}.
#' @param max_iter Integer. Maximum EM iterations. Default 80.
#' @param tol Numeric. Convergence tolerance. Default 1e-4.
#'
#' @return A list with fraud_probs, normal_probs, log_likelihood_history, n_iter.
#'
#' @export
em_fraud_model <- function(pca_result, max_iter = 80, tol = 1e-4) {

  if (!is.list(pca_result) || is.null(pca_result$scores)) {
    stop("Input must be the list returned by pca_compress().")
  }

  X <- pca_result$scores
  n <- nrow(X)
  d <- ncol(X)
  y <- pca_result$labels

  # Initialise parameters from true labels (warm start)
  pi_f  <- mean(y)
  mu_n  <- colMeans(X[y == 0, , drop = FALSE])
  mu_f  <- colMeans(X[y == 1, , drop = FALSE])

  # Use per-class variance, with generous floor to prevent collapse
  var_n <- apply(X[y == 0, , drop = FALSE], 2, stats::var)
  var_f <- apply(X[y == 1, , drop = FALSE], 2, stats::var)
  sig_n <- pmax(var_n, 0.01)
  sig_f <- pmax(var_f, 0.01)

  ll_history <- numeric(max_iter)
  n_iter     <- max_iter

  # Safe log-sum-exp for two values: log(exp(a) + exp(b))
  log_sum_exp2 <- function(a, b) {
    m <- pmax(a, b)
    m + log(exp(a - m) + exp(b - m))
  }

  for (iter in seq_len(max_iter)) {

    # === E-STEP (fully numerically stable) ===
    # Compute log p(x | component) for each component independently per dimension
    log_lik_n <- matrix(0, n, d)
    log_lik_f <- matrix(0, n, d)
    for (j in seq_len(d)) {
      log_lik_n[, j] <- stats::dnorm(X[, j], mu_n[j], sqrt(sig_n[j]), log = TRUE)
      log_lik_f[, j] <- stats::dnorm(X[, j], mu_f[j], sqrt(sig_f[j]), log = TRUE)
    }

    # Sum log-likelihoods across dimensions
    ll_n <- rowSums(log_lik_n)   # log p(x | normal)
    ll_f <- rowSums(log_lik_f)   # log p(x | fraud)

    # Clamp to prevent -Inf blowing up
    ll_n <- pmax(ll_n, -500)
    ll_f <- pmax(ll_f, -500)

    # Log unnormalised responsibilities
    log_rn <- log(pmax(1 - pi_f, 1e-10)) + ll_n
    log_rf <- log(pmax(pi_f,     1e-10)) + ll_f

    # Log denominator using stable log-sum-exp
    log_denom <- log_sum_exp2(log_rn, log_rf)

    # Responsibilities - clamp to valid range
    r_n <- pmax(pmin(exp(log_rn - log_denom), 1 - 1e-10), 1e-10)
    r_f <- 1 - r_n

    # Log-likelihood - replace any NaN/Inf with previous value
    ll_val <- sum(log_denom)
    if (!is.finite(ll_val)) ll_val <- if (iter > 1) ll_history[iter - 1] else -1e10
    ll_history[iter] <- ll_val

    # === M-STEP ===
    N_n <- sum(r_n) + 1e-10
    N_f <- sum(r_f) + 1e-10
    pi_f  <- N_f / n

    mu_n  <- colSums(r_n * X) / N_n
    mu_f  <- colSums(r_f * X) / N_f

    sig_n <- pmax(colSums(r_n * (X - matrix(mu_n, n, d, byrow = TRUE))^2) / N_n, 0.01)
    sig_f <- pmax(colSums(r_f * (X - matrix(mu_f, n, d, byrow = TRUE))^2) / N_f, 0.01)

    # Convergence check - safe for NaN
    if (iter > 1) {
      delta <- abs(ll_history[iter] - ll_history[iter - 1])
      if (is.finite(delta) && delta < tol) {
        n_iter <- iter
        message(sprintf("EM converged in %d iterations.", n_iter))
        break
      }
    }
  }

  # Make sure r_f = fraud component (higher mean absolute PC values = fraud)
  if (mean(abs(mu_n)) > mean(abs(mu_f))) {
    tmp <- r_n; r_n <- r_f; r_f <- tmp
  }

  list(
    fraud_probs            = as.numeric(r_f),
    normal_probs           = as.numeric(r_n),
    log_likelihood_history = ll_history[seq_len(n_iter)],
    n_iter                 = n_iter,
    labels                 = pca_result$labels,
    raw_amount             = pca_result$raw_amount,
    pca_scores             = X
  )
}
