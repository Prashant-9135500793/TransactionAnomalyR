#' Logistic Regression Fraud Scorer via Gradient Descent
#'
#' @description
#' Trains a logistic regression classifier using mini-batch gradient descent
#' on the PCA-compressed transaction features. Uses class-weighted loss to
#' handle the severe class imbalance in the Kaggle dataset (0.17\% fraud).
#'
#' The model minimises the weighted binary cross-entropy loss:
#' \deqn{L = -\frac{1}{n}\sum w_i [y_i \log(\hat{y}_i) + (1-y_i)\log(1-\hat{y}_i)] + \frac{\lambda}{2}\|w\|^2}
#'
#' @param pca_result A list returned by \code{\link{pca_compress}}.
#' @param n_epochs Integer. Number of passes over the data. Default 200.
#' @param learning_rate Numeric. Step size. Default 0.01.
#' @param lambda Numeric. L2 regularization strength. Default 0.01.
#' @param batch_size Integer. Mini-batch size. Default 256.
#'
#' @return A list with:
#' \describe{
#'   \item{fraud_scores}{Numeric vector: predicted P(fraud) in [0,1]}
#'   \item{weights}{Trained weight vector (length = n_components)}
#'   \item{bias}{Trained bias scalar}
#'   \item{loss_history}{Loss at each epoch}
#'   \item{labels}{True labels passed through}
#'   \item{raw_amount}{Amounts passed through}
#' }
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard()
#' prep  <- preprocess_kaggle(data)
#' pca_r <- pca_compress(prep)
#' gd_r  <- gradient_descent_scorer(pca_r, n_epochs = 200)
#' summary(gd_r$fraud_scores)
#' plot(gd_r$loss_history, type = "l", xlab = "Epoch", ylab = "Loss")
#' }
#'
#' @export
gradient_descent_scorer <- function(pca_result,
                                     n_epochs      = 200,
                                     learning_rate = 0.01,
                                     lambda        = 0.01,
                                     batch_size    = 256) {

  if (!is.list(pca_result) || is.null(pca_result$scores)) {
    stop("Input must be the list returned by pca_compress().")
  }

  X <- pca_result$scores
  y <- pca_result$labels
  n <- nrow(X)
  d <- ncol(X)

  set.seed(42)
  w <- stats::rnorm(d, 0, 0.01)
  b <- 0

  sigmoid <- function(z) 1 / (1 + exp(-pmin(pmax(z, -500), 500)))

  # Class weights: upweight the rare fraud class
  w_pos     <- sum(y == 0) / (sum(y == 1) + 1e-10)
  loss_hist <- numeric(n_epochs)

  for (ep in seq_len(n_epochs)) {
    # Mini-batch sampling
    idx <- sample(seq_len(n), min(batch_size, n))
    Xb  <- X[idx, , drop = FALSE]
    yb  <- y[idx]

    # Forward pass
    yhat  <- sigmoid(as.numeric(Xb %*% w) + b)
    wts   <- ifelse(yb == 1, w_pos, 1.0)

    # Weighted cross-entropy + L2 regularization
    loss_hist[ep] <- -mean(wts * (yb * log(yhat + 1e-10) +
                                   (1 - yb) * log(1 - yhat + 1e-10))) +
                      (lambda / 2) * sum(w^2)

    # Backward pass
    dz <- wts * (yhat - yb)
    dw <- as.numeric(t(Xb) %*% dz) / length(idx) + lambda * w
    db <- mean(dz)

    # Update - keep w as a plain numeric vector (prevents d×1 matrix drift)
    w <- as.numeric(w - learning_rate * dw)
    b <- b - learning_rate * db
  }

  message(sprintf("Gradient Descent: final loss = %.4f after %d epochs.",
                  tail(loss_hist, 1), n_epochs))

  list(
    fraud_scores = sigmoid(as.numeric(X %*% w) + b),
    weights      = as.numeric(w),
    bias         = b,
    loss_history = loss_hist,
    labels       = y,
    raw_amount   = pca_result$raw_amount
  )
}
