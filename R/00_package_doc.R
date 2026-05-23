#' TransactionAnomalyR: Real-Time Credit Card Fraud Detection
#'
#' @description
#' A complete R package for credit card fraud detection using the Kaggle
#' Credit Card Fraud Detection dataset. Implements seven statistical and
#' machine learning algorithms from scratch:
#'
#' \enumerate{
#'   \item \strong{PCA} - dimensionality reduction (\code{\link{pca_compress}})
#'   \item \strong{EM Algorithm} - Gaussian mixture model (\code{\link{em_fraud_model}})
#'   \item \strong{Gradient Descent} - logistic regression (\code{\link{gradient_descent_scorer}})
#'   \item \strong{kNN Anomaly Scoring} - unsupervised detection (\code{\link{knn_anomaly_score}})
#'   \item \strong{Bootstrap} - threshold estimation (\code{\link{bootstrap_threshold}})
#'   \item \strong{CLT} - confidence intervals (\code{\link{clt_amount_ci}})
#'   \item \strong{Cross-Validation} - model evaluation (\code{\link{cross_validate_model}})
#' }
#'
#' @section Quick start:
#' \preformatted{
#' library(TransactionAnomalyR)
#'
#' # 1. Load dataset (place creditcard.csv in data-raw/)
#' data <- load_creditcard()
#'
#' # 2. Train on 80%, evaluate on 20% (no data leakage)
#' splits  <- train_test_split(data)
#' trained <- train_model(splits$train)
#' results <- predict_on_test(trained, splits$test)
#' print_test_results(results)
#'
#' # 3. Or run everything at once
#' pipeline <- full_pipeline(data)
#'
#' # 4. Launch interactive Shiny dashboard
#' launch_dashboard()
#' }
#'
#' @section Dataset:
#' Download \code{creditcard.csv} from
#' \url{https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud}
#' and place it in the \code{data-raw/} subfolder of your working directory.
#'
#' @author Prashant Shekhar \email{prashantsh22@@iitk.ac.in}
#'
#' @docType package
#' @name TransactionAnomalyR
"_PACKAGE"
