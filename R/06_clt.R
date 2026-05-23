#' CLT-Based Confidence Intervals for Transaction Amounts
#'
#' @description
#' Applies the Central Limit Theorem to construct confidence intervals for
#' the mean transaction amount in each class (normal vs fraud). Can also
#' flag new transaction amounts as statistically unusual using z-scores.
#'
#' By CLT: for large n, the sample mean X_bar ~ Normal(mu, sigma^2/n)
#' So a 95% CI is: [X_bar - 1.96*(sigma/sqrt(n)),  X_bar + 1.96*(sigma/sqrt(n))]
#'
#' @param raw_amount Numeric vector of transaction amounts.
#' @param labels Integer vector of true labels (0 = normal, 1 = fraud).
#' @param confidence_level Numeric. Confidence level. Default 0.95.
#' @param new_amounts Numeric vector. New amounts to test. Default NULL.
#'
#' @return A list with:
#' \describe{
#'   \item{normal_ci}{Numeric vector: c(lower, mean, upper) for normal transactions}
#'   \item{fraud_ci}{Numeric vector: c(lower, mean, upper) for fraud transactions}
#'   \item{normal_mean}{Mean amount for normal transactions}
#'   \item{normal_sd}{Standard deviation for normal transactions}
#'   \item{fraud_mean}{Mean amount for fraud transactions}
#'   \item{fraud_sd}{Standard deviation for fraud transactions}
#'   \item{z_scores}{Z-scores for new_amounts (if provided)}
#'   \item{amount_flags}{Logical vector: TRUE if amount is statistically unusual}
#'   \item{p_values}{Two-tailed p-values for new_amounts}
#' }
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard()
#' prep  <- preprocess_kaggle(data)
#' clt_r <- clt_amount_ci(prep$raw_amount, prep$labels,
#'                         new_amounts = c(10, 500, 5000))
#' cat("Normal mean:", clt_r$normal_mean, "\n")
#' cat("Fraud  mean:", clt_r$fraud_mean,  "\n")
#' print(clt_r$normal_ci)
#' print(clt_r$amount_flags)
#' }
#'
#' @export
clt_amount_ci <- function(raw_amount,
                           labels,
                           confidence_level = 0.95,
                           new_amounts      = NULL) {

  normal_amt <- raw_amount[labels == 0]
  fraud_amt  <- raw_amount[labels == 1]

  # CLT: confidence interval for the mean
  # Returns plain numeric vector (no names) - avoids plotly rendering bugs
  ci_fn <- function(x, conf) {
    n    <- length(x)
    xbar <- mean(x)
    se   <- stats::sd(x) / sqrt(n)
    z    <- stats::qnorm(1 - (1 - conf) / 2)
    as.numeric(c(xbar - z * se, xbar, xbar + z * se))
  }

  result <- list(
    normal_ci   = ci_fn(normal_amt, confidence_level),
    fraud_ci    = ci_fn(fraud_amt,  confidence_level),
    normal_mean = as.numeric(mean(normal_amt)),
    normal_sd   = as.numeric(stats::sd(normal_amt)),
    fraud_mean  = as.numeric(mean(fraud_amt)),
    fraud_sd    = as.numeric(stats::sd(fraud_amt))
  )

  if (!is.null(new_amounts)) {
    z_crit  <- stats::qnorm(1 - (1 - confidence_level) / 2)
    z_scores <- (new_amounts - result$normal_mean) / result$normal_sd
    result$z_scores     <- z_scores
    result$amount_flags <- abs(z_scores) > z_crit
    result$p_values     <- 2 * (1 - stats::pnorm(abs(z_scores)))
  }

  result
}
