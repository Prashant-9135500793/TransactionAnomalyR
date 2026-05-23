#' Load Kaggle Credit Card Fraud Dataset
#'
#' @export
load_creditcard <- function(path = NULL, sample_size = 10000, seed = 42) {

  # Try every possible location the file could be
  # Uses only relative paths and R built-ins - works on ANY computer
  possible <- c(
    # Explicit path passed in (e.g. from Shiny UI)
    if (!is.null(path) && nchar(path) > 0) path else character(0),
    # When user does setwd("~/Downloads") and installs from there
    file.path(getwd(), "TransactionAnomalyR", "data-raw", "creditcard.csv"),
    # When working directory IS the TransactionAnomalyR folder
    file.path(getwd(), "data-raw", "creditcard.csv"),
    # When working directory is inside R/ subfolder
    file.path(getwd(), "..", "data-raw", "creditcard.csv"),
    # system.file for properly installed packages
    system.file("data-raw", "creditcard.csv", package = "TransactionAnomalyR")
  )

  resolved <- NULL
  for (p in possible) {
    if (nchar(p) > 0 && file.exists(p)) {
      resolved <- normalizePath(p)
      break
    }
  }
  path <- resolved

  if (is.null(path)) {
    # Tell the user exactly where to put the file based on their actual getwd()
    stop(
      "\ncreditcard.csv not found!\n\n",
      "Put creditcard.csv here:\n",
      "  ", file.path(getwd(), "TransactionAnomalyR", "data-raw"), "\n\n",
      "Then run load_creditcard() again."
    )
  }

  message("Loading: ", path)
  raw <- read.csv(path)

  colnames(raw)[colnames(raw) == "Class"]  <- "is_fraud"
  colnames(raw)[colnames(raw) == "Amount"] <- "amount"
  colnames(raw)[colnames(raw) == "Time"]   <- "time"

  set.seed(seed)
  fraud_idx  <- which(raw$is_fraud == 1)
  normal_idx <- which(raw$is_fraud == 0)
  n_normal   <- min(length(normal_idx), sample_size)
  idx        <- c(fraud_idx, sample(normal_idx, n_normal))
  data       <- raw[sample(idx), ]
  rownames(data) <- NULL

  message(sprintf("Loaded %d transactions: %d fraud (%.3f%%), %d normal",
    nrow(data), sum(data$is_fraud == 1),
    mean(data$is_fraud) * 100, sum(data$is_fraud == 0)))

  data
}
