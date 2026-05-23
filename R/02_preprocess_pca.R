#' Preprocess Kaggle Credit Card Transaction Data
#'
#' @description
#' Prepares the raw Kaggle dataset for modeling. Extracts features V1-V28,
#' Amount, and cyclically-encoded hour features from Time. Applies z-score
#' normalization so all features are on the same scale.
#'
#' @param data A data frame returned by \code{\link{load_creditcard}}.
#'
#' @return A list with:
#' \describe{
#'   \item{features}{Normalized numeric matrix (rows = transactions, cols = features)}
#'   \item{labels}{Integer vector: 0 = normal, 1 = fraud}
#'   \item{raw_amount}{Original transaction amounts (for CLT analysis)}
#'   \item{scale_params}{List of means and sds used for normalization}
#'   \item{feature_names}{Character vector of feature column names}
#' }
#'
#' @examples
#' \dontrun{
#' data <- load_creditcard()
#' prep <- preprocess_kaggle(data)
#' dim(prep$features)
#' }
#'
#' @export
preprocess_kaggle <- function(data) {

  required <- c(paste0("V", 1:28), "amount", "time", "is_fraud")
  missing  <- setdiff(required, colnames(data))
  if (length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "),
         "\nMake sure you loaded data using load_creditcard().")
  }

  # Cyclical encoding of hour-of-day from Time (seconds)
  data$hour_sin <- sin(2 * pi * (data$time %% 86400) / 86400)
  data$hour_cos <- cos(2 * pi * (data$time %% 86400) / 86400)

  feat_cols <- c(paste0("V", 1:28), "amount", "hour_sin", "hour_cos")
  X         <- as.matrix(data[, feat_cols])

  # Z-score normalization
  col_means <- colMeans(X)
  col_sds   <- apply(X, 2, stats::sd)
  col_sds[col_sds == 0] <- 1

  X_scaled <- sweep(sweep(X, 2, col_means, "-"), 2, col_sds, "/")

  list(
    features      = X_scaled,
    labels        = as.integer(data$is_fraud),
    raw_amount    = data$amount,
    scale_params  = list(means = col_means, sds = col_sds),
    feature_names = feat_cols
  )
}


#' PCA Compression of Transaction Features
#'
#' @description
#' Applies Principal Component Analysis to reduce the 30-dimensional feature
#' space. Although V1-V28 are already PCA-transformed by Kaggle, applying
#' PCA again on the full feature set (including Amount and hour features)
#' finds a more compact representation optimal for our classifiers.
#'
#' @param prep A list returned by \code{\link{preprocess_kaggle}}.
#' @param variance_threshold Numeric. Retain components explaining at least
#'   this proportion of total variance. Default 0.95.
#'
#' @return A list with:
#' \describe{
#'   \item{scores}{PCA score matrix (n x n_components)}
#'   \item{pca_model}{Fitted \code{prcomp} object for projecting new data}
#'   \item{variance_explained}{Per-component variance proportions}
#'   \item{cumulative_variance}{Cumulative variance proportions}
#'   \item{n_components}{Number of components retained}
#'   \item{labels}{Fraud labels passed through}
#'   \item{raw_amount}{Amounts passed through}
#' }
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard()
#' prep  <- preprocess_kaggle(data)
#' pca_r <- pca_compress(prep, variance_threshold = 0.95)
#' cat("Components retained:", pca_r$n_components, "\n")
#' }
#'
#' @export
pca_compress <- function(prep, variance_threshold = 0.95) {

  if (!is.list(prep) || is.null(prep$features)) {
    stop("Input must be the list returned by preprocess_kaggle().")
  }

  pca_fit <- stats::prcomp(prep$features, center = FALSE, scale. = FALSE)
  var_exp <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
  cum_var <- cumsum(var_exp)
  n_comp  <- max(2, which(cum_var >= variance_threshold)[1])

  message(sprintf("PCA: retained %d components explaining %.1f%% of variance.",
                  n_comp, cum_var[n_comp] * 100))

  list(
    scores              = pca_fit$x[, seq_len(n_comp), drop = FALSE],
    pca_model           = pca_fit,
    variance_explained  = var_exp,
    cumulative_variance = cum_var,
    n_components        = n_comp,
    labels              = prep$labels,
    raw_amount          = prep$raw_amount
  )
}
