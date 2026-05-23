#' Plot PCA Fraud Space
#'
#' @description
#' Scatter plot of transactions in PCA space (PC1 vs PC2), coloured by EM fraud
#' probability. Red = high fraud probability, green = low.
#'
#' @param pca_result List from \code{\link{pca_compress}}.
#' @param em_result List from \code{\link{em_fraud_model}}.
#' @param max_points Integer. Max points to plot (for speed). Default 3000.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard()
#' prep  <- preprocess_kaggle(data)
#' pca_r <- pca_compress(prep)
#' em_r  <- em_fraud_model(pca_r)
#' print(plot_fraud_space(pca_r, em_r))
#' }
#'
#' @export
plot_fraud_space <- function(pca_result, em_result, max_points = 3000) {

  n   <- nrow(pca_result$scores)
  idx <- if (n > max_points) sample(n, max_points) else seq_len(n)

  df <- data.frame(
    PC1  = pca_result$scores[idx, 1],
    PC2  = pca_result$scores[idx, 2],
    prob = em_result$fraud_probs[idx],
    true = factor(pca_result$labels[idx], levels = c(0, 1),
                  labels = c("Normal", "Fraud"))
  )
  var_exp <- round(pca_result$variance_explained[1:2] * 100, 1)

  ggplot2::ggplot(df, ggplot2::aes(x = PC1, y = PC2,
                                    color = prob, shape = true)) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::scale_color_gradient(low = "#2ecc71", high = "#e74c3c",
                                   name = "P(Fraud)\nEM Model") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title    = "PCA Space - EM Fraud Probabilities (Kaggle Data)",
      subtitle = "Each point is a real transaction",
      x        = paste0("PC1 (", var_exp[1], "% variance)"),
      y        = paste0("PC2 (", var_exp[2], "% variance)"),
      shape    = "True Label"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13))
}


#' Plot ROC Curve
#'
#' @description
#' Receiver Operating Characteristic curve showing the trade-off between
#' True Positive Rate (Recall) and False Positive Rate across thresholds.
#' Area Under Curve (AUC) is displayed in the subtitle.
#'
#' @param fraud_scores Numeric vector of predicted fraud probabilities.
#' @param labels Integer vector of true labels (0/1).
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard()
#' prep  <- preprocess_kaggle(data)
#' pca_r <- pca_compress(prep)
#' gd_r  <- gradient_descent_scorer(pca_r)
#' print(plot_roc_curve(gd_r$fraud_scores, gd_r$labels))
#' }
#'
#' @export
plot_roc_curve <- function(fraud_scores, labels) {

  thresholds <- seq(0, 1, by = 0.02)
  roc_df <- do.call(rbind, lapply(thresholds, function(t) {
    p <- as.integer(fraud_scores >= t)
    data.frame(
      tpr = sum(p == 1 & labels == 1) / (sum(labels == 1) + 1e-10),
      fpr = sum(p == 1 & labels == 0) / (sum(labels == 0) + 1e-10)
    )
  }))

  roc_s <- roc_df[order(roc_df$fpr), ]
  auc   <- abs(sum(diff(roc_s$fpr) *
                     (head(roc_s$tpr, -1) + tail(roc_s$tpr, -1)) / 2))

  ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr)) +
    ggplot2::geom_line(color = "#e74c3c", linewidth = 1.2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = tpr),
                          fill = "#e74c3c", alpha = 0.15) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                          linetype = "dashed", color = "grey50") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title    = "ROC Curve - Fraud Detection (Kaggle Data)",
      subtitle = sprintf("AUC = %.4f", auc),
      x        = "False Positive Rate (1 - Specificity)",
      y        = "True Positive Rate (Sensitivity / Recall)"
    ) +
    ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 13),
                   plot.subtitle = ggplot2::element_text(size = 12, color = "grey40"))
}


#' Plot Confusion Matrix
#'
#' @description
#' Heatmap of the confusion matrix showing TP, FP, TN, FN counts at a
#' given decision threshold.
#'
#' @param fraud_scores Numeric vector of predicted fraud probabilities.
#' @param labels Integer vector of true labels (0/1).
#' @param threshold Numeric. Decision threshold. Default 0.5.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard()
#' prep  <- preprocess_kaggle(data)
#' pca_r <- pca_compress(prep)
#' gd_r  <- gradient_descent_scorer(pca_r)
#' print(plot_confusion_matrix(gd_r$fraud_scores, gd_r$labels, threshold = 0.4))
#' }
#'
#' @export
plot_confusion_matrix <- function(fraud_scores, labels, threshold = 0.5) {

  preds <- as.integer(fraud_scores >= threshold)

  cm_df <- data.frame(
    Actual    = factor(c("Normal","Normal","Fraud","Fraud"),
                       levels = c("Normal","Fraud")),
    Predicted = factor(c("Normal","Fraud","Normal","Fraud"),
                       levels = c("Normal","Fraud")),
    Count     = c(sum(preds==0 & labels==0),
                  sum(preds==1 & labels==0),
                  sum(preds==0 & labels==1),
                  sum(preds==1 & labels==1)),
    Label     = c("True\nNegative","False\nPositive",
                  "False\nNegative","True\nPositive")
  )

  ggplot2::ggplot(cm_df, ggplot2::aes(x = Predicted, y = Actual, fill = Count)) +
    ggplot2::geom_tile(color = "white", linewidth = 2) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Label, "\n", Count)),
                        size = 4.5, fontface = "bold") +
    ggplot2::scale_fill_gradient(low = "#ecf0f1", high = "#e74c3c") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title    = "Confusion Matrix",
      subtitle = sprintf("Threshold = %.2f", threshold)
    ) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(face = "bold", size = 13),
                   axis.text  = ggplot2::element_text(size = 11, face = "bold"))
}


#' Plot Transaction Amount Distribution
#'
#' @description
#' Histogram of transaction amounts (log scale) for normal vs fraudulent
#' transactions, with CLT mean lines overlaid.
#'
#' @param raw_amount Numeric vector of transaction amounts.
#' @param labels Integer vector of true labels (0/1).
#' @param clt_result Optional list from \code{\link{clt_amount_ci}}.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' data  <- load_creditcard()
#' prep  <- preprocess_kaggle(data)
#' clt_r <- clt_amount_ci(prep$raw_amount, prep$labels)
#' print(plot_amount_distribution(prep$raw_amount, prep$labels, clt_r))
#' }
#'
#' @export
plot_amount_distribution <- function(raw_amount, labels, clt_result = NULL) {

  df <- data.frame(
    amount = raw_amount + 0.01,
    label  = factor(labels, levels = c(0, 1), labels = c("Normal","Fraud"))
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = amount, fill = label)) +
    ggplot2::geom_histogram(bins = 60, alpha = 0.65, position = "identity") +
    ggplot2::scale_x_log10() +
    ggplot2::scale_fill_manual(values = c("Normal" = "#3498db", "Fraud" = "#e74c3c")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Transaction Amount Distribution (Kaggle Real Data)",
      x     = "Amount in EUR (log scale)",
      y     = "Count",
      fill  = ""
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13))

  if (!is.null(clt_result)) {
    p <- p +
      ggplot2::geom_vline(xintercept = clt_result$normal_mean,
                           color = "#2980b9", linetype = "dashed", linewidth = 1) +
      ggplot2::geom_vline(xintercept = clt_result$fraud_mean,
                           color = "#c0392b", linetype = "dashed", linewidth = 1)
  }
  p
}
