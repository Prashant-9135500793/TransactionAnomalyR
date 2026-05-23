#' Launch TransactionAnomalyR Shiny Dashboard
#'
#' @description
#' Opens an interactive Shiny dashboard showing predictions on the TEST SET.
#' Pass all three objects returned from the train/test workflow for full results.
#'
#' @param pipeline A trained pipeline from \code{\link{train_model}}.
#' @param splits   The train/test split from \code{\link{train_test_split}}.
#' @param test_results Test predictions from \code{\link{predict_on_test}}.
#' @param port Integer. Shiny port. Default 3838.
#' @param launch.browser Logical. Open browser. Default TRUE.
#'
#' @examples
#' \dontrun{
#' data         <- load_creditcard()
#' splits       <- train_test_split(data)
#' trained      <- train_model(splits$train)
#' test_results <- predict_on_test(trained, splits$test)
#' launch_dashboard(trained, splits, test_results)
#' }
#'
#' @export
launch_dashboard <- function(pipeline = NULL, splits = NULL, test_results = NULL, port = 3838, launch.browser = TRUE) {

  ui <- shinydashboard::dashboardPage(
    skin = "red",

    shinydashboard::dashboardHeader(
      title      = "TransactionAnomalyR - Test Set Predictions",
      titleWidth = 420
    ),

    shinydashboard::dashboardSidebar(
      width = 270,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Dashboard",         tabName="dash",  icon=shiny::icon("tachometer-alt")),
        shinydashboard::menuItem("Train vs Test Data",tabName="split", icon=shiny::icon("columns")),
        shinydashboard::menuItem("Test Predictions",  tabName="test",  icon=shiny::icon("vial")),
        shinydashboard::menuItem("Train Models",      tabName="train", icon=shiny::icon("cogs")),
        shinydashboard::menuItem("Visualizations",    tabName="viz",   icon=shiny::icon("chart-bar")),
        shinydashboard::menuItem("Score New TX",      tabName="score", icon=shiny::icon("search-dollar"))
      ),
      shiny::hr(),
      shiny::tags$h5("Load Kaggle Data", style="color:white;padding-left:15px;font-weight:bold;"),
      shiny::textInput("csv_path", "Path to creditcard.csv", "data-raw/creditcard.csv"),
      shiny::sliderInput("sample_size", "Normal samples", 2000, 30000, 10000, 1000),
      shiny::sliderInput("train_ratio", "Train/Test Split", 0.6, 0.9, 0.8, 0.05,
                          post=" train"),
      shiny::actionButton("btn_load", "Load creditcard.csv",
                           icon=shiny::icon("folder-open"),
                           class="btn-info btn-block",
                           style="margin:5px 15px;width:calc(100% - 30px);font-weight:bold;"),
      shiny::hr(),
      shiny::tags$h5("Model Settings", style="color:white;padding-left:15px;font-weight:bold;"),
      shiny::sliderInput("n_epochs",  "GD Epochs",     50, 300, 200, 25),
      shiny::sliderInput("pca_var",   "PCA Variance",  0.80, 0.99, 0.95, 0.01),
      shiny::sliderInput("k_folds",   "CV Folds",      3, 10, 5, 1),
      shiny::sliderInput("boot_B",    "Bootstrap B",   50, 300, 150, 25),
      shiny::actionButton("btn_train", "Train on TRAIN set & Predict on TEST",
                           icon=shiny::icon("play"),
                           class="btn-danger btn-block",
                           style="margin:5px 15px;width:calc(100% - 30px);font-weight:bold;font-size:13px;")
    ),

    shinydashboard::dashboardBody(
      shiny::tags$head(shiny::tags$style(shiny::HTML(
        ".content-wrapper{background:#f4f6f9}
         .box{border-radius:8px;box-shadow:0 2px 8px rgba(0,0,0,.1)}
         .train-badge{background:#2980b9;color:white;padding:3px 10px;border-radius:4px;font-size:12px;font-weight:bold;}
         .test-badge{background:#e74c3c;color:white;padding:3px 10px;border-radius:4px;font-size:12px;font-weight:bold;}"
      ))),

      shinydashboard::tabItems(

        # ============================================================
        # TAB 1: DASHBOARD - live-updating after every train run
        # ============================================================
        shinydashboard::tabItem(tabName="dash",

          # ---- Status bar: shows training state + last-run timestamp ----
          shiny::fluidRow(
            shiny::column(12, shiny::uiOutput("dash_status_bar"))
          ),

          # ---- Settings used for last run ----
          shiny::fluidRow(
            shiny::column(12, shiny::uiOutput("dash_settings_bar"))
          ),

          # ---- Row 1: dataset counts ----
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("vb_total",  width=3),
            shinydashboard::valueBoxOutput("vb_fraud",  width=3),
            shinydashboard::valueBoxOutput("vb_normal", width=3),
            shinydashboard::valueBoxOutput("vb_ratio",  width=3)
          ),

          # ---- Row 2: model performance metrics ----
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("vb_auc",    width=3),
            shinydashboard::valueBoxOutput("vb_f1",     width=3),
            shinydashboard::valueBoxOutput("vb_recall", width=3),
            shinydashboard::valueBoxOutput("vb_thresh", width=3)
          ),

          # ---- Row 3: NEW - live summary panel ----
          shiny::fluidRow(
            shiny::column(12, shiny::uiOutput("dash_live_summary"))
          ),

          # ---- Row 4: plots ----
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("Fraud Score Distribution - TEST SET",
                                shiny::tags$span(class="test-badge", style="margin-left:8px","TEST DATA")),
              status="danger", solidHeader=TRUE, width=6,
              plotly::plotlyOutput("plt_scores", height="300px")),
            shinydashboard::box(
              title=shiny::span("Transaction Amount - TEST SET",
                                shiny::tags$span(class="test-badge", style="margin-left:8px","TEST DATA")),
              status="primary", solidHeader=TRUE, width=6,
              plotly::plotlyOutput("plt_amt", height="300px"))
          ),

          # ---- Row 5: high-risk table ----
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("High-Risk Transactions Flagged on TEST SET",
                                shiny::tags$span(class="test-badge", style="margin-left:8px","TEST DATA")),
              status="warning", solidHeader=TRUE, width=12,
              DT::DTOutput("tbl_risk"))
          )
        ),

        # ============================================================
        # TAB 2: TRAIN vs TEST SPLIT
        # ============================================================
        shinydashboard::tabItem(tabName="split",
          shiny::fluidRow(
            shinydashboard::box(
              title="Train / Test Split Overview", status="primary",
              solidHeader=TRUE, width=12,
              shiny::uiOutput("split_info_ui")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("TRAIN SET Data", shiny::tags$span(class="train-badge","TRAIN")),
              status="primary", solidHeader=TRUE, width=6,
              DT::DTOutput("tbl_train")),
            shinydashboard::box(
              title=shiny::span("TEST SET Data (Unseen)", shiny::tags$span(class="test-badge","TEST")),
              status="danger", solidHeader=TRUE, width=6,
              DT::DTOutput("tbl_test"))
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title="Class Distribution: Train vs Test",
              status="success", solidHeader=TRUE, width=6,
              plotly::plotlyOutput("plt_split_dist", height="300px")),
            shinydashboard::box(
              title="Amount Distribution: Train vs Test",
              status="warning", solidHeader=TRUE, width=6,
              plotly::plotlyOutput("plt_split_amt", height="300px"))
          )
        ),

        # ============================================================
        # TAB 3: TEST PREDICTIONS - main results tab
        # ============================================================
        shinydashboard::tabItem(tabName="test",
          shiny::fluidRow(
            shiny::column(12,
              shiny::div(
                style="background:#e8f8f5;border:2px solid #27ae60;border-radius:8px;padding:14px 18px;margin-bottom:14px;",
                shiny::tags$h4(shiny::tags$b("Test Set Performance"),
                               style="color:#1e8449;margin:0 0 8px 0;"),
                shiny::uiOutput("test_metrics_ui")
              )
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("Confusion Matrix - TEST SET",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="warning", solidHeader=TRUE, width=5,
              shiny::uiOutput("confusion_ui")),
            shinydashboard::box(
              title=shiny::span("ROC Curve - TEST SET",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="primary", solidHeader=TRUE, width=7,
              plotly::plotlyOutput("plt_test_roc", height="300px"))
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("Score Distribution - TEST SET",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="danger", solidHeader=TRUE, width=6,
              plotly::plotlyOutput("plt_test_scores", height="280px")),
            shinydashboard::box(
              title=shiny::span("CLT Amount Analysis - TEST SET",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="success", solidHeader=TRUE, width=6,
              plotly::plotlyOutput("plt_test_clt", height="280px"))
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("All Test Transactions with Predictions",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="info", solidHeader=TRUE, width=12,
              shiny::p(shiny::tags$b("Predicted = 1"), " means model flagged as fraud. ",
                       shiny::tags$b("is_fraud = 1"), " is the true label from Kaggle."),
              DT::DTOutput("tbl_test_preds"))
          )
        ),

        # ============================================================
        # TAB 4: TRAINING
        # ============================================================
        shinydashboard::tabItem(tabName="train",
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("Training Log",
                                shiny::tags$span(class="train-badge","TRAIN DATA ONLY")),
              status="success", solidHeader=TRUE, width=6,
              shiny::verbatimTextOutput("log")),
            shinydashboard::box(
              title="Cross-Validation on TRAIN SET", status="primary",
              solidHeader=TRUE, width=6,
              shiny::fluidRow(
                shinydashboard::infoBoxOutput("ib1",width=6),
                shinydashboard::infoBoxOutput("ib2",width=6)),
              shiny::fluidRow(
                shinydashboard::infoBoxOutput("ib3",width=6),
                shinydashboard::infoBoxOutput("ib4",width=6)),
              shiny::fluidRow(
                shinydashboard::infoBoxOutput("ib5",width=6),
                shinydashboard::infoBoxOutput("ib6",width=6))
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(title="GD Loss Curve (Train)",   status="warning",solidHeader=TRUE,width=6,
                                 plotly::plotlyOutput("plt_loss",   height="270px")),
            shinydashboard::box(title="PCA Variance Explained",  status="info",   solidHeader=TRUE,width=6,
                                 plotly::plotlyOutput("plt_pca",    height="270px"))
          ),
          shiny::fluidRow(
            shinydashboard::box(title="EM Log-Likelihood (Train)",status="danger", solidHeader=TRUE,width=6,
                                 plotly::plotlyOutput("plt_em",    height="270px")),
            shinydashboard::box(title="Bootstrap Threshold Dist.",status="success",solidHeader=TRUE,width=6,
                                 plotly::plotlyOutput("plt_boot",  height="270px"))
          )
        ),

        # ============================================================
        # TAB 5: VISUALIZATIONS - all on test set
        # ============================================================
        shinydashboard::tabItem(tabName="viz",
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("PCA Space - TEST SET (EM Fraud Probs)",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="danger", solidHeader=TRUE, width=6,
              plotly::plotlyOutput("plt_pca_space", height="380px")),
            shinydashboard::box(
              title=shiny::span("ROC Curve - TEST SET",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="primary", solidHeader=TRUE, width=6,
              plotly::plotlyOutput("plt_roc", height="380px"))
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("Confusion Matrix - TEST SET",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="warning", solidHeader=TRUE, width=6,
              shiny::sliderInput("cm_t","Threshold",0.1,0.9,0.5,0.05),
              plotly::plotlyOutput("plt_cm", height="280px")),
            shinydashboard::box(
              title=shiny::span("CLT Amount CI - TEST SET",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="success", solidHeader=TRUE, width=6,
              plotly::plotlyOutput("plt_clt", height="300px"))
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title=shiny::span("kNN Dissimilarity - TEST SET",
                                shiny::tags$span(class="test-badge","TEST DATA")),
              status="info", solidHeader=TRUE, width=12,
              plotly::plotlyOutput("plt_knn", height="270px"))
          )
        ),

        # ============================================================
        # TAB 6: SCORE NEW TX
        # ============================================================
        shinydashboard::tabItem(tabName="score",
          shiny::fluidRow(
            shinydashboard::box(
              title="Enter New Transaction Features",
              status="primary", solidHeader=TRUE, width=5,
              shiny::p("Copy a row from Test Predictions tab and paste all V1-V28 values:"),

              # ---- Auto-fill row ----
              shiny::fluidRow(
                shiny::column(8,
                  shiny::selectInput("fill_type", NULL,
                    choices = c("First FRAUD in test","First NORMAL in test",
                                "Highest risk in test","Random test row"),
                    width = "100%")
                ),
                shiny::column(4,
                  shiny::actionButton("btn_autofill", "Auto-fill",
                    icon=shiny::icon("magic"), class="btn-info btn-block",
                    style="margin-top:0px;font-weight:bold;width:100%;")
                )
              ),
              shiny::hr(style="margin:6px 0;"),

              # ---- Amount + all V1-V28 ----
              shiny::numericInput("tx_amt","Amount (EUR)",100,0,100000),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v1", "V1", -1.36,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v2", "V2", -0.07,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v3", "V3",  2.54,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v4", "V4",  1.38,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v5", "V5",  0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v6", "V6",  0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v7", "V7",  0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v8", "V8",  0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v9", "V9",  0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v10","V10", 0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v11","V11", 0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v12","V12", 0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v13","V13", 0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v14","V14 *",-2.5,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v15","V15", 0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v16","V16", 0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v17","V17 *",-0.99,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v18","V18", 0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v19","V19", 0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v20","V20", 0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v21","V21", 0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v22","V22", 0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v23","V23", 0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v24","V24", 0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v25","V25", 0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v26","V26", 0,-50,50,0.01))),
              shiny::fluidRow(
                shiny::column(6,shiny::numericInput("tx_v27","V27", 0,-50,50,0.01)),
                shiny::column(6,shiny::numericInput("tx_v28","V28", 0,-50,50,0.01))),
              shiny::p(shiny::em("* V14 and V17 are most predictive of fraud"),
                       style="font-size:11px;color:#888;margin:2px 0;"),
              shiny::br(),
              shiny::actionButton("btn_score","SCORE THIS TRANSACTION",
                icon=shiny::icon("search"),
                class="btn btn-danger btn-block btn-lg",
                style="font-weight:bold;")
            ),
            shinydashboard::box(
              title="Risk Assessment (Model trained on TRAIN SET)",
              status="danger", solidHeader=TRUE, width=7,
              shiny::uiOutput("score_out"),
              shiny::br(),
              plotly::plotlyOutput("plt_gauge", height="270px")
            )
          )
        )
      )
    )
  )

  # ================================================================
  # SERVER
  # ================================================================
  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(
      data          = if (!is.null(pipeline)) pipeline$data    else NULL,
      splits        = splits,
      prep          = if (!is.null(pipeline) && !is.null(splits)) preprocess_kaggle(splits$train) else NULL,
      pipe          = pipeline,
      test_res      = test_results,
      loaded        = !is.null(splits),
      trained       = !is.null(test_results),
      training_now  = FALSE,                         # TRUE while btn_train is running
      last_trained  = if (!is.null(test_results)) Sys.time() else NULL,
      last_settings = NULL                           # snapshot of slider values at last train
    )

    # ---- LOAD DATA ----
    shiny::observeEvent(input$btn_load, {
      shiny::withProgress(message="Loading creditcard.csv...", {
        tryCatch({
          path <- input$csv_path
          for (p in c(path,
                      file.path(getwd(),"data-raw","creditcard.csv"),
                      file.path(getwd(),"TransactionAnomalyR","data-raw","creditcard.csv"),
                      "~/Downloads/TransactionAnomalyR/data-raw/creditcard.csv")) {
            if (file.exists(p)) { path <- p; break }
          }
          shiny::setProgress(0.4, message="Reading CSV...")
          rv$data   <- load_creditcard(path=path, sample_size=input$sample_size)
          shiny::setProgress(0.7, message="Splitting train/test...")
          rv$splits <- train_test_split(rv$data, train_ratio=input$train_ratio)
          rv$loaded <- TRUE; rv$trained <- FALSE
          rv$pipe   <- NULL; rv$test_res <- NULL
          shiny::showNotification(
            sprintf("Loaded %d rows - Train: %d | Test: %d",
                    nrow(rv$data), nrow(rv$splits$train), nrow(rv$splits$test)),
            type="message", duration=5)
        }, error=function(e)
          shiny::showNotification(paste("Error:", e$message), type="error", duration=8))
      })
    })

    # ---- TRAIN + PREDICT ----
    # Always re-samples fresh data using current slider values before training.
    # This means sample_size and train_ratio changes take effect immediately
    # on the next Train click - no need to click Load again.
    shiny::observeEvent(input$btn_train, {
      shiny::req(rv$loaded)
      rv$training_now <- TRUE
      shiny::withProgress(message="Training on TRAIN SET...", {
        tryCatch({

          # ── Step 0: re-sample data with CURRENT sample_size + train_ratio ──
          # Use a random seed based on current time so each run with different
          # sample_size produces genuinely different (not deterministic) data.
          shiny::setProgress(0.02, message="Re-sampling data with current settings...")
          current_seed <- as.integer(Sys.time()) %% 100000L

          # Re-load from the raw CSV with the new sample_size
          path <- input$csv_path
          for (p in c(path,
                      file.path(getwd(),"data-raw","creditcard.csv"),
                      file.path(getwd(),"TransactionAnomalyR","data-raw","creditcard.csv"),
                      "~/Downloads/TransactionAnomalyR/data-raw/creditcard.csv")) {
            if (file.exists(p)) { path <- p; break }
          }
          fresh_data <- load_creditcard(
            path        = path,
            sample_size = input$sample_size,
            seed        = current_seed          # different seed = different sample
          )
          fresh_splits <- train_test_split(
            fresh_data,
            train_ratio = input$train_ratio,
            seed        = current_seed
          )

          # Update rv so all other tabs (Split, Test Predictions, etc.) stay in sync
          rv$data   <- fresh_data
          rv$splits <- fresh_splits

          shiny::setProgress(0.05, message="PCA on train set...")
          pca_r <- pca_compress(preprocess_kaggle(rv$splits$train),
                                variance_threshold=input$pca_var)

          shiny::setProgress(0.20, message="EM Algorithm...")
          em_r  <- em_fraud_model(pca_r)

          shiny::setProgress(0.35, message="Gradient Descent...")
          gd_r  <- gradient_descent_scorer(pca_r, n_epochs=input$n_epochs)

          shiny::setProgress(0.50, message="kNN anomaly scoring...")
          knn_r <- knn_anomaly_score(pca_r)

          shiny::setProgress(0.60, message="Bootstrap threshold...")
          bt_r  <- bootstrap_threshold(gd_r$fraud_scores, gd_r$labels, B=input$boot_B)

          shiny::setProgress(0.68, message="CLT analysis...")
          prep_tr <- preprocess_kaggle(rv$splits$train)
          clt_r   <- clt_amount_ci(prep_tr$raw_amount, prep_tr$labels)

          shiny::setProgress(0.75, message="Cross-validation on train set...")
          cv_r  <- cross_validate_model(prep_tr, k_folds=input$k_folds,
                                         n_epochs=input$n_epochs)

          rv$pipe <- list(pca=pca_r, em=em_r, gd=gd_r, knn=knn_r,
                           bootstrap=bt_r, clt=clt_r, cv=cv_r,
                           data=rv$splits$train,
                           scale_params=prep_tr$scale_params)
          rv$prep <- prep_tr

          shiny::setProgress(0.85, message="Predicting on TEST SET...")
          rv$test_res <- predict_on_test(rv$pipe, rv$splits$test)

          rv$trained       <- TRUE
          rv$training_now  <- FALSE
          rv$last_trained  <- Sys.time()
          rv$last_settings <- list(
            epochs     = input$n_epochs,
            pca_var    = input$pca_var,
            k_folds    = input$k_folds,
            boot_B     = input$boot_B,
            train_ratio= input$train_ratio,
            sample_size= input$sample_size
          )
          shiny::setProgress(1, message="Done!")
          shiny::showNotification(
            sprintf("Done! Samples=%d | AUC=%.4f | F1=%.4f | Recall=%.1f%%",
                    nrow(fresh_data),
                    rv$test_res$auc, rv$test_res$f1, rv$test_res$recall*100),
            type="message", duration=6)
        }, error=function(e) {
          rv$training_now <- FALSE
          shiny::showNotification(paste("Error:", e$message), type="error", duration=8)
        })
      })
    })

    # ================================================================
    # DASHBOARD - live reactive outputs
    # ================================================================

    # ---- Status bar: training / idle / ready ----
    output$dash_status_bar <- shiny::renderUI({
      if (rv$training_now) {
        shiny::div(
          style="background:#f39c12;color:white;padding:10px 16px;border-radius:8px;margin-bottom:8px;font-weight:bold;",
          shiny::icon("spinner"), " Training in progress - results will update automatically when complete..."
        )
      } else if (rv$trained) {
        ts <- if (!is.null(rv$last_trained))
          format(rv$last_trained, "%H:%M:%S") else "-"
        shiny::div(
          style="background:#1a252f;color:white;padding:10px 16px;border-radius:8px;margin-bottom:8px;",
          shiny::tags$b(shiny::icon("check-circle"), " Dashboard updated - last trained at ", ts),
          shiny::tags$span(
            style="margin-left:16px;font-size:13px;",
            if (rv$loaded) sprintf(
              "Train: %d rows (%d fraud)  |  Test: %d rows (%d fraud)",
              nrow(rv$splits$train), sum(rv$splits$train$is_fraud),
              nrow(rv$splits$test),  sum(rv$splits$test$is_fraud)
            ) else ""
          )
        )
      } else if (rv$loaded) {
        shiny::div(
          style="background:#2980b9;color:white;padding:10px 16px;border-radius:8px;margin-bottom:8px;font-weight:bold;",
          shiny::icon("info-circle"),
          sprintf(" Data loaded - Train: %d rows | Test: %d rows - click Train to see results",
                  nrow(rv$splits$train), nrow(rv$splits$test))
        )
      } else {
        shiny::div(
          style="background:#7f8c8d;color:white;padding:10px 16px;border-radius:8px;margin-bottom:8px;font-weight:bold;",
          shiny::icon("info-circle"), " Load creditcard.csv from the sidebar, then click Train."
        )
      }
    })

    # ---- Settings bar: shows exactly which settings produced the current results ----
    output$dash_settings_bar <- shiny::renderUI({
      shiny::req(rv$trained, !is.null(rv$last_settings))
      s <- rv$last_settings

      # Check each setting individually so we can give a precise message
      sample_changed <- s$sample_size != input$sample_size
      ratio_changed  <- s$train_ratio != input$train_ratio
      model_changed  <- (s$epochs  != input$n_epochs ||
                         s$pca_var != input$pca_var  ||
                         s$k_folds != input$k_folds  ||
                         s$boot_B  != input$boot_B)
      any_changed <- sample_changed || ratio_changed || model_changed

      # Build specific change messages
      change_parts <- c(
        if (sample_changed) sprintf("Samples: %d → %d", s$sample_size, input$sample_size),
        if (ratio_changed)  sprintf("Split: %.0f%% → %.0f%%", s$train_ratio*100, input$train_ratio*100),
        if (model_changed)  "Model settings changed"
      )

      bg  <- if (any_changed) "#fadbd8;border:2px solid #e74c3c;" else "#eafaf1;border:2px solid #27ae60;"
      msg <- if (any_changed)
        paste0(" ⚠  Changed since last run (", paste(change_parts, collapse=" | "),
               ") - click Train to update results with new data.")
      else
        " ✓ Dashboard reflects current settings - results are up to date."

      shiny::div(
        style=paste0("background:", bg, "border-radius:8px;padding:8px 14px;margin-bottom:10px;font-size:13px;"),
        shiny::tags$b(msg),
        shiny::br(),
        shiny::tags$span(style="color:#555;font-size:12px;",
          sprintf("Last run used: %d samples | %.0f%% train | Epochs=%d | PCA=%.2f | Folds=%d | Boot B=%d",
                  s$sample_size, s$train_ratio*100, s$epochs, s$pca_var, s$k_folds, s$boot_B))
      )
    })

    # ---- Live summary panel: all key metrics in one place ----
    output$dash_live_summary <- shiny::renderUI({
      if (!rv$trained) return(NULL)
      r  <- rv$test_res
      cm <- r$confusion_matrix
      p  <- rv$pipe
      shiny::div(
        style="background:white;border-radius:8px;padding:16px 20px;margin-bottom:10px;box-shadow:0 2px 8px rgba(0,0,0,.1);",
        shiny::tags$h4(shiny::tags$b("Model Results Summary"), style="margin:0 0 12px 0;color:#1a252f;"),
        shiny::fluidRow(
          # Column 1 - Test set performance
          shiny::column(3,
            shiny::div(style="border-right:1px solid #eee;padding-right:12px;",
              shiny::tags$p(style="font-size:11px;color:#888;font-weight:bold;margin:0;", "TEST SET PERFORMANCE"),
              shiny::tags$table(style="width:100%;font-size:14px;",
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "AUC-ROC"),
                  shiny::tags$td(style="font-weight:bold;color:#8e44ad;text-align:right;",
                    round(r$auc, 4))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "F1 Score"),
                  shiny::tags$td(style="font-weight:bold;color:#e74c3c;text-align:right;",
                    round(r$f1, 4))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "Recall"),
                  shiny::tags$td(style="font-weight:bold;color:#27ae60;text-align:right;",
                    sprintf("%.2f%%", r$recall * 100))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "Precision"),
                  shiny::tags$td(style="font-weight:bold;color:#2980b9;text-align:right;",
                    sprintf("%.2f%%", r$precision * 100))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "Accuracy"),
                  shiny::tags$td(style="font-weight:bold;color:#e67e22;text-align:right;",
                    sprintf("%.2f%%", r$accuracy * 100)))
              )
            )
          ),
          # Column 2 - Confusion matrix counts
          shiny::column(3,
            shiny::div(style="border-right:1px solid #eee;padding:0 12px;",
              shiny::tags$p(style="font-size:11px;color:#888;font-weight:bold;margin:0;", "CONFUSION MATRIX"),
              shiny::tags$table(style="width:100%;font-size:14px;",
                shiny::tags$tr(
                  shiny::tags$td(style="color:#27ae60;padding:3px 0;", "True Positives (caught)"),
                  shiny::tags$td(style="font-weight:bold;color:#27ae60;text-align:right;", cm$TP)),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#e74c3c;padding:3px 0;", "False Negatives (missed)"),
                  shiny::tags$td(style="font-weight:bold;color:#e74c3c;text-align:right;", cm$FN)),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#e67e22;padding:3px 0;", "False Positives (false alarm)"),
                  shiny::tags$td(style="font-weight:bold;color:#e67e22;text-align:right;", cm$FP)),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "True Negatives"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;", cm$TN)),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#16a085;padding:3px 0;font-weight:bold;", "Fraud caught"),
                  shiny::tags$td(style="font-weight:bold;color:#16a085;text-align:right;",
                    sprintf("%d / %d (%.1f%%)", cm$TP, r$n_fraud_test,
                            cm$TP / max(r$n_fraud_test, 1) * 100)))
              )
            )
          ),
          # Column 3 - Model settings used
          shiny::column(3,
            shiny::div(style="border-right:1px solid #eee;padding:0 12px;",
              shiny::tags$p(style="font-size:11px;color:#888;font-weight:bold;margin:0;", "MODEL SETTINGS USED"),
              shiny::tags$table(style="width:100%;font-size:14px;",
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "PCA components"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;", p$pca$n_components)),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "PCA variance"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;",
                    sprintf("%.1f%%", p$pca$cumulative_variance[p$pca$n_components]*100))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "GD final loss"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;",
                    sprintf("%.4f", tail(p$gd$loss_history,1)))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "EM iterations"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;", p$em$n_iter)),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "Bootstrap threshold"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;",
                    sprintf("%.3f", p$bootstrap$optimal_threshold)))
              )
            )
          ),
          # Column 4 - CV results on train set
          shiny::column(3,
            shiny::div(style="padding-left:12px;",
              shiny::tags$p(style="font-size:11px;color:#888;font-weight:bold;margin:0;", "CROSS-VALIDATION (TRAIN)"),
              shiny::tags$table(style="width:100%;font-size:14px;",
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "CV AUC"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;",
                    round(p$cv$mean_auc, 4))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "CV F1"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;",
                    round(p$cv$mean_f1, 4))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "CV Recall"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;",
                    sprintf("%.2f%%", p$cv$mean_recall*100))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "CV Accuracy"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;",
                    sprintf("%.2f%%", p$cv$mean_accuracy*100))),
                shiny::tags$tr(
                  shiny::tags$td(style="color:#555;padding:3px 0;", "Folds run"),
                  shiny::tags$td(style="font-weight:bold;text-align:right;",
                    nrow(p$cv$fold_results)))
              )
            )
          )
        )
      )
    })

    # ---- VALUE BOXES - fully reactive, update on every rv$test_res change ----
    output$vb_total <- shinydashboard::renderValueBox({
      if      (rv$training_now) shinydashboard::valueBox("...", "Training...",          icon=shiny::icon("spinner"),            color="yellow")
      else if (rv$trained)      shinydashboard::valueBox(format(rv$test_res$n_test,big.mark=","), "Test Set Rows", icon=shiny::icon("vial"),   color="blue")
      else if (rv$loaded)       shinydashboard::valueBox("Load→Train","Test Set Rows",  icon=shiny::icon("vial"),                color="blue")
      else                      shinydashboard::valueBox("Load Data", "Test Set Rows",  icon=shiny::icon("vial"),                color="blue")
    })
    output$vb_fraud <- shinydashboard::renderValueBox({
      if      (rv$training_now) shinydashboard::valueBox("...", "True Fraud in Test",   icon=shiny::icon("spinner"),            color="yellow")
      else if (rv$trained)      shinydashboard::valueBox(rv$test_res$n_fraud_test, "True Fraud in Test", icon=shiny::icon("exclamation-triangle"), color="red")
      else                      shinydashboard::valueBox("-",   "True Fraud in Test",   icon=shiny::icon("exclamation-triangle"),color="red")
    })
    output$vb_normal <- shinydashboard::renderValueBox({
      if      (rv$training_now) shinydashboard::valueBox("...", "Normal in Test",       icon=shiny::icon("spinner"),            color="yellow")
      else if (rv$trained)      shinydashboard::valueBox(format(rv$test_res$n_normal_test,big.mark=","), "Normal in Test", icon=shiny::icon("check-circle"), color="green")
      else                      shinydashboard::valueBox("-",   "Normal in Test",       icon=shiny::icon("check-circle"),       color="green")
    })
    output$vb_ratio <- shinydashboard::renderValueBox({
      if      (rv$training_now) shinydashboard::valueBox("...", "Fraud Caught (Test)",  icon=shiny::icon("spinner"),            color="yellow")
      else if (rv$trained)      shinydashboard::valueBox(sprintf("%d / %d", rv$test_res$confusion_matrix$TP, rv$test_res$n_fraud_test), "Fraud Caught (Test)", icon=shiny::icon("shield-alt"), color="orange")
      else                      shinydashboard::valueBox("-",   "Fraud Caught (Test)",  icon=shiny::icon("shield-alt"),         color="orange")
    })
    output$vb_auc <- shinydashboard::renderValueBox({
      if      (rv$training_now) shinydashboard::valueBox("...", "AUC-ROC (Test Set)",   icon=shiny::icon("spinner"),            color="yellow")
      else if (rv$trained)      shinydashboard::valueBox(round(rv$test_res$auc,4),  "AUC-ROC (Test Set)",  icon=shiny::icon("chart-line"), color="purple")
      else                      shinydashboard::valueBox("Train first","AUC-ROC",        icon=shiny::icon("chart-line"),         color="purple")
    })
    output$vb_f1 <- shinydashboard::renderValueBox({
      if      (rv$training_now) shinydashboard::valueBox("...", "F1 Score (Test Set)",  icon=shiny::icon("spinner"),            color="yellow")
      else if (rv$trained)      shinydashboard::valueBox(round(rv$test_res$f1,4),    "F1 Score (Test Set)",   icon=shiny::icon("bullseye"),   color="red")
      else                      shinydashboard::valueBox("Train first","F1 Score",       icon=shiny::icon("bullseye"),           color="red")
    })
    output$vb_recall <- shinydashboard::renderValueBox({
      if      (rv$training_now) shinydashboard::valueBox("...", "Recall (Test Set)",    icon=shiny::icon("spinner"),            color="yellow")
      else if (rv$trained)      shinydashboard::valueBox(sprintf("%.1f%%",rv$test_res$recall*100), "Recall (Test Set)", icon=shiny::icon("redo"), color="yellow")
      else                      shinydashboard::valueBox("Train first","Recall",          icon=shiny::icon("redo"),              color="yellow")
    })
    output$vb_thresh <- shinydashboard::renderValueBox({
      if      (rv$training_now) shinydashboard::valueBox("...", "Bootstrap Threshold",  icon=shiny::icon("spinner"),            color="yellow")
      else if (rv$trained)      shinydashboard::valueBox(round(rv$test_res$threshold,3), "Bootstrap Threshold", icon=shiny::icon("sliders-h"), color="teal")
      else                      shinydashboard::valueBox("Train first","Threshold",       icon=shiny::icon("sliders-h"),         color="teal")
    })

    # ---- DASHBOARD PLOTS (test set) ----
    output$plt_scores <- plotly::renderPlotly({
      shiny::req(rv$trained)
      df <- data.frame(
        s = rv$test_res$fraud_scores,
        l = factor(rv$test_res$true_labels, 0:1, c("Normal","Fraud")))
      plotly::ggplotly(
        ggplot2::ggplot(df, ggplot2::aes(x=s, fill=l)) +
          ggplot2::geom_histogram(bins=60, alpha=0.7, position="identity") +
          ggplot2::geom_vline(xintercept=rv$test_res$threshold,
                               linetype="dashed", linewidth=1.2, color="black") +
          ggplot2::scale_fill_manual(values=c(Normal="#3498db", Fraud="#e74c3c")) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title="TEST SET: Fraud Score Distribution",
                        subtitle=sprintf("Dashed line = threshold %.3f (from Bootstrap on TRAIN)",
                                         rv$test_res$threshold),
                        x="Fraud Score", y="Count", fill=""))
    })

    output$plt_amt <- plotly::renderPlotly({
      shiny::req(rv$trained)
      df <- data.frame(
        a = rv$splits$test$amount + 0.01,
        l = factor(rv$splits$test$is_fraud, 0:1, c("Normal","Fraud")))
      plotly::ggplotly(
        ggplot2::ggplot(df, ggplot2::aes(x=a, fill=l)) +
          ggplot2::geom_histogram(bins=60, alpha=0.65, position="identity") +
          ggplot2::scale_x_log10() +
          ggplot2::scale_fill_manual(values=c(Normal="#3498db", Fraud="#e74c3c")) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title="TEST SET: Transaction Amount Distribution",
                        x="Amount EUR (log)", y="Count", fill=""))
    })

    output$tbl_risk <- DT::renderDT({
      shiny::req(rv$trained)
      df <- rv$test_res$flagged_transactions
      if (!nrow(df)) return(data.frame(Message="No fraud flagged."))
      # Show ALL columns including V1-V28
      show_cols <- c("time","amount",paste0("V",1:28),"is_fraud","fraud_score","correct")
      show_cols <- intersect(show_cols, colnames(df))
      DT::datatable(head(df[, show_cols], 20),
                    options=list(scrollX=TRUE, pageLength=10,
                                 columnDefs=list(list(width="80px",targets="_all"))),
                    rownames=FALSE,
                    caption="Transactions flagged as FRAUD on the TEST SET") |>
        DT::formatStyle("is_fraud",
          backgroundColor=DT::styleEqual(c(0,1), c("#fadbd8","#d5f5e3"))) |>
        DT::formatStyle("correct",
          backgroundColor=DT::styleEqual(c(0,1), c("#fadbd8","#d5f5e3"))) |>
        DT::formatStyle("fraud_score",
          background=DT::styleColorBar(c(0,1),"#e74c3c"), backgroundSize="100% 90%")
    })

    # ---- SPLIT TAB ----
    output$split_info_ui <- shiny::renderUI({
      shiny::req(rv$loaded)
      tr <- rv$splits$train; te <- rv$splits$test
      shiny::tags$div(
        shiny::fluidRow(
          shiny::column(6,
            shiny::div(style="background:#d6eaf8;border-radius:8px;padding:14px;margin:4px;",
              shiny::tags$h4("TRAIN SET", style="color:#1a5276;margin:0 0 8px 0;"),
              shiny::tags$p(sprintf("Rows: %d",       nrow(tr))),
              shiny::tags$p(sprintf("Fraud: %d (%.3f%%)", sum(tr$is_fraud), mean(tr$is_fraud)*100)),
              shiny::tags$p(sprintf("Normal: %d",     sum(tr$is_fraud==0))),
              shiny::tags$p(shiny::tags$b("Used to: train PCA, EM, GD, Bootstrap, CV"))
            )
          ),
          shiny::column(6,
            shiny::div(style="background:#fadbd8;border-radius:8px;padding:14px;margin:4px;",
              shiny::tags$h4("TEST SET", style="color:#922b21;margin:0 0 8px 0;"),
              shiny::tags$p(sprintf("Rows: %d",       nrow(te))),
              shiny::tags$p(sprintf("Fraud: %d (%.3f%%)", sum(te$is_fraud), mean(te$is_fraud)*100)),
              shiny::tags$p(sprintf("Normal: %d",     sum(te$is_fraud==0))),
              shiny::tags$p(shiny::tags$b("Used to: evaluate ONLY - never seen during training"))
            )
          )
        )
      )
    })

    output$tbl_train <- DT::renderDT({
      shiny::req(rv$loaded)
      cols <- c("time","amount","V1","V2","V3","V14","is_fraud")
      DT::datatable(rv$splits$train[, intersect(cols, colnames(rv$splits$train))],
                    options=list(scrollX=TRUE, pageLength=8), rownames=FALSE) |>
        DT::formatStyle("is_fraud", backgroundColor=DT::styleEqual(c(0,1),c("#d5f5e3","#fadbd8")))
    })

    output$tbl_test <- DT::renderDT({
      shiny::req(rv$loaded)
      cols <- c("time","amount","V1","V2","V3","V14","is_fraud")
      DT::datatable(rv$splits$test[, intersect(cols, colnames(rv$splits$test))],
                    options=list(scrollX=TRUE, pageLength=8), rownames=FALSE) |>
        DT::formatStyle("is_fraud", backgroundColor=DT::styleEqual(c(0,1),c("#d5f5e3","#fadbd8")))
    })

    output$plt_split_dist <- plotly::renderPlotly({
      shiny::req(rv$loaded)
      df <- data.frame(
        Set   = c("Train","Train","Test","Test"),
        Class = c("Normal","Fraud","Normal","Fraud"),
        Count = c(sum(rv$splits$train$is_fraud==0), sum(rv$splits$train$is_fraud==1),
                  sum(rv$splits$test$is_fraud==0),  sum(rv$splits$test$is_fraud==1))
      )
      plotly::ggplotly(
        ggplot2::ggplot(df, ggplot2::aes(x=Set, y=Count, fill=Class)) +
          ggplot2::geom_bar(stat="identity", position="dodge", alpha=0.8) +
          ggplot2::scale_fill_manual(values=c(Normal="#3498db", Fraud="#e74c3c")) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title="Class counts: Train vs Test", x="", y="Count"))
    })

    output$plt_split_amt <- plotly::renderPlotly({
      shiny::req(rv$loaded)
      df <- rbind(
        data.frame(amt=rv$splits$train$amount+0.01, set="Train"),
        data.frame(amt=rv$splits$test$amount+0.01,  set="Test")
      )
      plotly::ggplotly(
        ggplot2::ggplot(df, ggplot2::aes(x=amt, fill=set)) +
          ggplot2::geom_histogram(bins=50, alpha=0.6, position="identity") +
          ggplot2::scale_x_log10() +
          ggplot2::scale_fill_manual(values=c(Train="#2980b9", Test="#e74c3c")) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title="Amount distribution: Train vs Test",
                        x="Amount EUR (log)", y="Count", fill=""))
    })

    # ---- TEST PREDICTIONS TAB ----
    output$test_metrics_ui <- shiny::renderUI({
      shiny::req(rv$trained)
      r  <- rv$test_res
      cm <- r$confusion_matrix
      shiny::fluidRow(
        shiny::column(2, shiny::div(style="text-align:center",
          shiny::tags$div(style="font-size:26px;font-weight:bold;color:#8e44ad", round(r$auc,4)),
          shiny::tags$div(style="font-size:12px;color:#666","AUC-ROC"))),
        shiny::column(2, shiny::div(style="text-align:center",
          shiny::tags$div(style="font-size:26px;font-weight:bold;color:#e74c3c", round(r$f1,4)),
          shiny::tags$div(style="font-size:12px;color:#666","F1 Score"))),
        shiny::column(2, shiny::div(style="text-align:center",
          shiny::tags$div(style="font-size:26px;font-weight:bold;color:#27ae60", sprintf("%.1f%%",r$recall*100)),
          shiny::tags$div(style="font-size:12px;color:#666","Recall"))),
        shiny::column(2, shiny::div(style="text-align:center",
          shiny::tags$div(style="font-size:26px;font-weight:bold;color:#2980b9", sprintf("%.1f%%",r$precision*100)),
          shiny::tags$div(style="font-size:12px;color:#666","Precision"))),
        shiny::column(2, shiny::div(style="text-align:center",
          shiny::tags$div(style="font-size:26px;font-weight:bold;color:#e67e22", sprintf("%.1f%%",r$accuracy*100)),
          shiny::tags$div(style="font-size:12px;color:#666","Accuracy"))),
        shiny::column(2, shiny::div(style="text-align:center",
          shiny::tags$div(style="font-size:26px;font-weight:bold;color:#16a085",
                          sprintf("%d/%d",cm$TP, r$n_fraud_test)),
          shiny::tags$div(style="font-size:12px;color:#666","Fraud Caught")))
      )
    })

    output$confusion_ui <- shiny::renderUI({
      shiny::req(rv$trained)
      cm <- rv$test_res$confusion_matrix
      shiny::tags$div(
        shiny::tags$p(style="font-size:12px;color:#666;margin-bottom:8px;",
                      "Rows = True label | Cols = Predicted label"),
        shiny::div(style="display:grid;grid-template-columns:1fr 1fr;gap:8px;",
          shiny::div(style="background:#d5f5e3;border-radius:8px;padding:18px;text-align:center;",
            shiny::tags$div(style="font-size:11px;color:#1e8449;","True Negative"),
            shiny::tags$div(style="font-size:32px;font-weight:bold;color:#1e8449;", cm$TN),
            shiny::tags$div(style="font-size:10px;color:#888;","Normal → Normal")),
          shiny::div(style="background:#fde8e8;border-radius:8px;padding:18px;text-align:center;",
            shiny::tags$div(style="font-size:11px;color:#a93226;","False Positive"),
            shiny::tags$div(style="font-size:32px;font-weight:bold;color:#a93226;", cm$FP),
            shiny::tags$div(style="font-size:10px;color:#888;","Normal → Fraud")),
          shiny::div(style="background:#fef9e7;border-radius:8px;padding:18px;text-align:center;",
            shiny::tags$div(style="font-size:11px;color:#b7770d;","False Negative"),
            shiny::tags$div(style="font-size:32px;font-weight:bold;color:#b7770d;", cm$FN),
            shiny::tags$div(style="font-size:10px;color:#888;","Fraud → Normal")),
          shiny::div(style="background:#d5f5e3;border-radius:8px;padding:18px;text-align:center;",
            shiny::tags$div(style="font-size:11px;color:#1e8449;","True Positive"),
            shiny::tags$div(style="font-size:32px;font-weight:bold;color:#1e8449;", cm$TP),
            shiny::tags$div(style="font-size:10px;color:#888;","Fraud → Fraud"))
        )
      )
    })

    output$plt_test_roc <- plotly::renderPlotly({
      shiny::req(rv$trained)
      plotly::ggplotly(plot_roc_curve(rv$test_res$fraud_scores, rv$test_res$true_labels))
    })

    output$plt_test_scores <- plotly::renderPlotly({
      shiny::req(rv$trained)
      df <- data.frame(
        s = rv$test_res$fraud_scores,
        l = factor(rv$test_res$true_labels, 0:1, c("Normal","Fraud")))
      plotly::ggplotly(
        ggplot2::ggplot(df, ggplot2::aes(x=s, fill=l)) +
          ggplot2::geom_histogram(bins=50, alpha=0.7, position="identity") +
          ggplot2::geom_vline(xintercept=rv$test_res$threshold,
                               linetype="dashed", linewidth=1.2) +
          ggplot2::scale_fill_manual(values=c(Normal="#3498db", Fraud="#e74c3c")) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title="TEST SET: Score Distribution",
                        x="Fraud Score", y="Count", fill=""))
    })

    output$plt_test_clt <- plotly::renderPlotly({
      shiny::req(rv$trained)
      cl <- rv$test_res$clt_result
      df <- data.frame(
        g  = c("Normal","Fraud"),
        m  = c(as.numeric(cl$normal_mean), as.numeric(cl$fraud_mean)),
        lo = c(as.numeric(cl$normal_ci[1]), as.numeric(cl$fraud_ci[1])),
        hi = c(as.numeric(cl$normal_ci[3]), as.numeric(cl$fraud_ci[3]))
      )
      plotly::ggplotly(
        ggplot2::ggplot(df, ggplot2::aes(x=g, y=m, fill=g)) +
          ggplot2::geom_bar(stat="identity", alpha=0.8, width=0.5) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=lo, ymax=hi),
                                  width=0.2, linewidth=1.5) +
          ggplot2::scale_fill_manual(values=c(Normal="#3498db", Fraud="#e74c3c")) +
          ggplot2::theme_minimal() + ggplot2::theme(legend.position="none") +
          ggplot2::labs(title="TEST SET: CLT Mean Amount ± 95% CI",
                        x="", y="Mean Amount (EUR)"))
    })

    output$tbl_test_preds <- DT::renderDT({
      shiny::req(rv$trained)
      # Show all V1-V28 columns
      all_cols <- c("time","amount",paste0("V",1:28),"is_fraud")
      df <- rv$splits$test[, intersect(all_cols, colnames(rv$splits$test))]
      df$fraud_score <- round(rv$test_res$fraud_scores, 4)
      df$predicted   <- rv$test_res$predictions
      df$correct     <- as.integer(df$is_fraud == df$predicted)
      df <- df[order(-df$fraud_score), ]
      DT::datatable(df, options=list(scrollX=TRUE, pageLength=15,
                                     columnDefs=list(list(width="80px",targets="_all"))),
                    rownames=FALSE,
                    caption="Full test set - sorted by fraud score descending") |>
        DT::formatStyle("predicted",
          backgroundColor=DT::styleEqual(c(0,1), c("#d5f5e3","#fadbd8"))) |>
        DT::formatStyle("correct",
          backgroundColor=DT::styleEqual(c(0,1), c("#fadbd8","#d5f5e3"))) |>
        DT::formatStyle("fraud_score",
          background=DT::styleColorBar(c(0,1),"#e74c3c"), backgroundSize="100% 90%")
    })

    # ---- TRAINING TAB ----
    output$log <- shiny::renderText({
      if (!rv$loaded)  return("Step 1: Load creditcard.csv\nStep 2: Click 'Train on TRAIN set & Predict on TEST'")
      if (!rv$trained) return(sprintf(
        "Data loaded!\nTrain: %d rows | %d fraud\nTest:  %d rows | %d fraud\n\nClick the train button to run all 7 models.",
        nrow(rv$splits$train), sum(rv$splits$train$is_fraud),
        nrow(rv$splits$test),  sum(rv$splits$test$is_fraud)))
      cv <- rv$pipe$cv; bt <- rv$pipe$bootstrap; tr <- rv$test_res
      paste0(
        "===== TRAIN SET - Model Training =====\n",
        sprintf("Train rows : %d  (%.3f%% fraud)\n", nrow(rv$splits$train), mean(rv$splits$train$is_fraud)*100),
        sprintf("PCA comps  : %d  (%.1f%% variance)\n", rv$pipe$pca$n_components,
                rv$pipe$pca$cumulative_variance[rv$pipe$pca$n_components]*100),
        sprintf("EM iters   : %d\n", rv$pipe$em$n_iter),
        sprintf("GD loss    : %.4f\n\n", tail(rv$pipe$gd$loss_history,1)),
        "--- CV on TRAIN SET ---\n",
        sprintf("Accuracy   : %.4f\n", cv$mean_accuracy),
        sprintf("F1         : %.4f\n", cv$mean_f1),
        sprintf("AUC-ROC    : %.4f\n\n", cv$mean_auc),
        "===== TEST SET - Prediction Results =====\n",
        sprintf("Test rows  : %d  (%d fraud)\n", tr$n_test, tr$n_fraud_test),
        sprintf("Threshold  : %.3f [%.3f, %.3f]\n", bt$optimal_threshold, bt$ci_lower, bt$ci_upper),
        sprintf("AUC-ROC    : %.4f\n", tr$auc),
        sprintf("F1 Score   : %.4f\n", tr$f1),
        sprintf("Recall     : %.4f\n", tr$recall),
        sprintf("Precision  : %.4f\n", tr$precision),
        sprintf("Fraud caught: %d / %d  (%.1f%%)\n", tr$confusion_matrix$TP, tr$n_fraud_test,
                tr$confusion_matrix$TP/(tr$n_fraud_test+1e-10)*100)
      )
    })

    ib <- function(lbl, val, ico, col)
      shinydashboard::infoBox(lbl, val, icon=shiny::icon(ico), color=col, fill=TRUE)
    output$ib1 <- shinydashboard::renderInfoBox(ib("CV Accuracy",  if(rv$trained)sprintf("%.2f%%",rv$pipe$cv$mean_accuracy*100) else"-","check","green"))
    output$ib2 <- shinydashboard::renderInfoBox(ib("CV Precision", if(rv$trained)sprintf("%.2f%%",rv$pipe$cv$mean_precision*100)else"-","crosshairs","blue"))
    output$ib3 <- shinydashboard::renderInfoBox(ib("CV Recall",    if(rv$trained)sprintf("%.2f%%",rv$pipe$cv$mean_recall*100)   else"-","redo","orange"))
    output$ib4 <- shinydashboard::renderInfoBox(ib("CV F1",        if(rv$trained)round(rv$pipe$cv$mean_f1,4)  else"-","balance-scale","purple"))
    output$ib5 <- shinydashboard::renderInfoBox(ib("CV AUC",       if(rv$trained)round(rv$pipe$cv$mean_auc,4) else"-","chart-area","red"))
    output$ib6 <- shinydashboard::renderInfoBox(ib("Threshold",    if(rv$trained)round(rv$pipe$bootstrap$optimal_threshold,3)else"-","sliders-h","teal"))

    output$plt_loss <- plotly::renderPlotly({
      shiny::req(rv$trained)
      df <- data.frame(e=seq_along(rv$pipe$gd$loss_history), l=rv$pipe$gd$loss_history)
      plotly::ggplotly(ggplot2::ggplot(df,ggplot2::aes(x=e,y=l))+
        ggplot2::geom_line(color="#e74c3c",linewidth=1)+ggplot2::theme_minimal()+
        ggplot2::labs(title="GD Loss (TRAIN SET)",x="Epoch",y="Loss"))
    })
    output$plt_pca <- plotly::renderPlotly({
      shiny::req(rv$trained)
      cv2 <- rv$pipe$pca$cumulative_variance
      df  <- data.frame(pc=seq_along(cv2), v=cv2*100)
      plotly::ggplotly(ggplot2::ggplot(df,ggplot2::aes(x=pc,y=v))+
        ggplot2::geom_line(color="#3498db",linewidth=1)+ggplot2::geom_point(color="#3498db",size=2)+
        ggplot2::geom_vline(xintercept=rv$pipe$pca$n_components,linetype="dashed",color="red")+
        ggplot2::theme_minimal()+ggplot2::labs(title="PCA Variance",x="Components",y="%"))
    })
    output$plt_em <- plotly::renderPlotly({
      shiny::req(rv$trained)
      ll <- rv$pipe$em$log_likelihood_history
      df <- data.frame(i=seq_along(ll), ll=ll)
      plotly::ggplotly(ggplot2::ggplot(df,ggplot2::aes(x=i,y=ll))+
        ggplot2::geom_line(color="#9b59b6",linewidth=1)+ggplot2::theme_minimal()+
        ggplot2::labs(title="EM Log-Likelihood (TRAIN)",x="Iter",y="Log-Likelihood"))
    })
    output$plt_boot <- plotly::renderPlotly({
      shiny::req(rv$trained)
      bt <- rv$pipe$bootstrap
      df <- data.frame(t=bt$bootstrap_thresholds)
      plotly::ggplotly(ggplot2::ggplot(df,ggplot2::aes(x=t))+
        ggplot2::geom_histogram(bins=25,fill="#27ae60",alpha=0.7,color="white")+
        ggplot2::geom_vline(xintercept=bt$optimal_threshold,color="red",linetype="dashed",linewidth=1.2)+
        ggplot2::geom_vline(xintercept=bt$ci_lower,color="orange",linetype="dotted")+
        ggplot2::geom_vline(xintercept=bt$ci_upper,color="orange",linetype="dotted")+
        ggplot2::theme_minimal()+
        ggplot2::labs(title="Bootstrap Thresholds (from TRAIN SET)",x="Threshold",y="Count"))
    })

    # ---- VISUALIZATIONS TAB (test set) ----
    output$plt_pca_space <- plotly::renderPlotly({
      shiny::req(rv$trained)
      # Project test set into PCA space for visualisation
      test_prep <- preprocess_kaggle(rv$splits$test)
      rotation  <- rv$pipe$pca$pca_model$rotation
      test_pc   <- (test_prep$features %*% rotation)[, 1:2, drop = FALSE]
      n <- nrow(test_pc)
      idx <- if(n>2000) sample(n,2000) else seq_len(n)
      df <- data.frame(
        PC1  = test_pc[idx,1],
        PC2  = test_pc[idx,2],
        prob = rv$test_res$fraud_scores[idx],
        true = factor(test_prep$labels[idx], 0:1, c("Normal","Fraud")))
      plotly::ggplotly(
        ggplot2::ggplot(df, ggplot2::aes(x=PC1,y=PC2,color=prob,shape=true)) +
          ggplot2::geom_point(alpha=0.6,size=1.5) +
          ggplot2::scale_color_gradient(low="#2ecc71",high="#e74c3c",name="P(Fraud)") +
          ggplot2::theme_minimal() +
          ggplot2::labs(title="TEST SET: PCA Space - Predicted Fraud Probabilities",shape=""))
    })
    output$plt_roc <- plotly::renderPlotly({
      shiny::req(rv$trained)
      plotly::ggplotly(plot_roc_curve(rv$test_res$fraud_scores, rv$test_res$true_labels))
    })
    output$plt_cm <- plotly::renderPlotly({
      shiny::req(rv$trained)
      plotly::ggplotly(
        plot_confusion_matrix(rv$test_res$fraud_scores, rv$test_res$true_labels, input$cm_t))
    })
    output$plt_clt <- plotly::renderPlotly({
      shiny::req(rv$trained)
      cl <- rv$test_res$clt_result
      df <- data.frame(
        g  = c("Normal","Fraud"),
        m  = c(as.numeric(cl$normal_mean), as.numeric(cl$fraud_mean)),
        lo = c(as.numeric(cl$normal_ci[1]), as.numeric(cl$fraud_ci[1])),
        hi = c(as.numeric(cl$normal_ci[3]), as.numeric(cl$fraud_ci[3]))
      )
      plotly::ggplotly(
        ggplot2::ggplot(df, ggplot2::aes(x=g,y=m,fill=g)) +
          ggplot2::geom_bar(stat="identity",alpha=0.8,width=0.5) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=lo,ymax=hi),width=0.2,linewidth=1.5) +
          ggplot2::scale_fill_manual(values=c(Normal="#3498db",Fraud="#e74c3c")) +
          ggplot2::theme_minimal()+ggplot2::theme(legend.position="none") +
          ggplot2::labs(title="TEST SET: CLT Amount ± 95% CI",x="",y="Mean EUR"))
    })
    output$plt_knn <- plotly::renderPlotly({
      shiny::req(rv$trained)
      kn <- rv$pipe$knn
      df <- data.frame(d=kn$dissimilarity,
                       l=factor(kn$labels,0:1,c("Normal","Fraud")))
      plotly::ggplotly(
        ggplot2::ggplot(df,ggplot2::aes(x=d,fill=l))+
          ggplot2::geom_histogram(bins=50,alpha=0.7,position="identity")+
          ggplot2::scale_fill_manual(values=c(Normal="#3498db",Fraud="#e74c3c"))+
          ggplot2::theme_minimal()+
          ggplot2::labs(title="kNN Dissimilarity (from TRAIN SET)",
                        x="Anomaly Score",y="Count",fill=""))
    })

    # ---- SCORE NEW TX ----
    # Auto-fill from a real test row
    shiny::observeEvent(input$btn_autofill, {
      shiny::req(rv$trained)
      test   <- rv$splits$test
      scores <- rv$test_res$fraud_scores
      row <- switch(input$fill_type,
        "First FRAUD in test"    = test[which(test$is_fraud == 1)[1], ],
        "First NORMAL in test"   = test[which(test$is_fraud == 0)[1], ],
        "Highest risk in test"   = test[which.max(scores), ],
        "Random test row"        = test[sample(nrow(test), 1), ],
        test[1, ]
      )
      shiny::updateNumericInput(session, "tx_amt", value = round(row$amount, 4))
      for (i in 1:28) {
        shiny::updateNumericInput(session, paste0("tx_v", i),
                                   value = round(row[[paste0("V", i)]], 4))
      }
      shiny::showNotification(
        sprintf("Filled from test row - True label: %s",
                if (row$is_fraud == 1) "FRAUD" else "NORMAL"),
        type = if (row$is_fraud == 1) "warning" else "message", duration = 4)
    })

    shiny::observeEvent(input$btn_score, {
      shiny::req(rv$trained)
      tryCatch({

        # ---- Guard: make sure all required pieces are present ----
        if (is.null(rv$pipe))                    stop("No trained model found. Please train first.")
        if (is.null(rv$pipe$pca))                stop("PCA model missing from pipeline.")
        if (is.null(rv$pipe$gd))                 stop("GD model missing from pipeline.")
        if (is.null(rv$pipe$bootstrap))          stop("Bootstrap result missing.")
        if (is.null(rv$pipe$clt))                stop("CLT result missing.")

        # ---- Resolve scale_params: prefer pipe copy, fall back to rv$prep ----
        sp <- if (!is.null(rv$pipe$scale_params)) {
          rv$pipe$scale_params
        } else if (!is.null(rv$prep) && !is.null(rv$prep$scale_params)) {
          rv$prep$scale_params
        } else {
          stop("Scale parameters not found. Please re-train the model.")
        }

        feat_names <- c(paste0("V", 1:28), "amount", "hour_sin", "hour_cos")

        # ---- Step 1: build raw value vector (31 elements) ----
        # Read ALL V1-V28 directly from their UI inputs.
        # Each input is always present in the UI, so no NULLs possible.
        raw_vals <- setNames(rep(0, 31L), feat_names)
        raw_vals["V1"]       <- as.numeric(input$tx_v1)
        raw_vals["V2"]       <- as.numeric(input$tx_v2)
        raw_vals["V3"]       <- as.numeric(input$tx_v3)
        raw_vals["V4"]       <- as.numeric(input$tx_v4)
        raw_vals["V5"]       <- as.numeric(input$tx_v5)
        raw_vals["V6"]       <- as.numeric(input$tx_v6)
        raw_vals["V7"]       <- as.numeric(input$tx_v7)
        raw_vals["V8"]       <- as.numeric(input$tx_v8)
        raw_vals["V9"]       <- as.numeric(input$tx_v9)
        raw_vals["V10"]      <- as.numeric(input$tx_v10)
        raw_vals["V11"]      <- as.numeric(input$tx_v11)
        raw_vals["V12"]      <- as.numeric(input$tx_v12)
        raw_vals["V13"]      <- as.numeric(input$tx_v13)
        raw_vals["V14"]      <- as.numeric(input$tx_v14)
        raw_vals["V15"]      <- as.numeric(input$tx_v15)
        raw_vals["V16"]      <- as.numeric(input$tx_v16)
        raw_vals["V17"]      <- as.numeric(input$tx_v17)
        raw_vals["V18"]      <- as.numeric(input$tx_v18)
        raw_vals["V19"]      <- as.numeric(input$tx_v19)
        raw_vals["V20"]      <- as.numeric(input$tx_v20)
        raw_vals["V21"]      <- as.numeric(input$tx_v21)
        raw_vals["V22"]      <- as.numeric(input$tx_v22)
        raw_vals["V23"]      <- as.numeric(input$tx_v23)
        raw_vals["V24"]      <- as.numeric(input$tx_v24)
        raw_vals["V25"]      <- as.numeric(input$tx_v25)
        raw_vals["V26"]      <- as.numeric(input$tx_v26)
        raw_vals["V27"]      <- as.numeric(input$tx_v27)
        raw_vals["V28"]      <- as.numeric(input$tx_v28)
        raw_vals["amount"]   <- as.numeric(input$tx_amt)
        raw_vals["hour_sin"] <- sin(2 * pi * (43200 %% 86400) / 86400)
        raw_vals["hour_cos"] <- cos(2 * pi * (43200 %% 86400) / 86400)

        # Validate no NAs crept in
        if (any(is.na(raw_vals))) stop("One or more input fields is empty or non-numeric.")

        # ---- Step 2: z-score normalise using TRAINING scale params ----
        tr_means <- as.numeric(sp$means[feat_names])
        tr_sds   <- as.numeric(sp$sds[feat_names])
        tr_sds[tr_sds == 0] <- 1   # safety: avoid /0
        scaled_vals <- (as.numeric(raw_vals[feat_names]) - tr_means) / tr_sds
        # scaled_vals: plain numeric vector, length 31

        # ---- Step 3: manual PCA projection (avoids predict.prcomp entirely) ----
        rotation <- rv$pipe$pca$pca_model$rotation   # matrix 31 x 31
        k        <- as.integer(rv$pipe$pca$n_components)
        if (k < 1L || k > nrow(rotation)) stop(sprintf("Invalid n_components: %d", k))
        # Drop all dimnames before multiply to prevent any name-mismatch errors
        rot_k       <- unname(rotation[, seq_len(k), drop = FALSE])  # 31 x k
        sv_mat      <- matrix(unname(scaled_vals), nrow = 1L)         # 1  x 31
        pc_scores   <- sv_mat %*% rot_k                               # 1  x k

        # ---- Step 4: logistic score with GD weights ----
        w  <- as.numeric(rv$pipe$gd$weights)   # length k
        b  <- as.numeric(rv$pipe$gd$bias)      # scalar
        if (length(w) != k) stop(sprintf(
          "Weight length %d != PCA components %d. Re-train the model.", length(w), k))
        sig <- function(z) 1 / (1 + exp(-pmin(pmax(z, -500), 500)))
        sc  <- as.numeric(sig(drop(pc_scores %*% w) + b))   # scalar in [0,1]

        th  <- as.numeric(rv$pipe$bootstrap$optimal_threshold)
        zs  <- (as.numeric(input$tx_amt) - as.numeric(rv$pipe$clt$normal_mean)) /
                 max(as.numeric(rv$pipe$clt$normal_sd), 1e-10)

        output$score_out <- shiny::renderUI({
          col <- if(sc>=th)"#e74c3c" else "#27ae60"
          lab <- if(sc>=th)"HIGH RISK - FRAUD SUSPECTED" else "LOW RISK - NORMAL"
          shiny::tagList(
            shiny::div(style=paste0("font-size:24px;font-weight:bold;color:",col,
              ";text-align:center;padding:16px;background:#f8f9fa;border-radius:8px;"),lab),
            shiny::br(),
            shiny::div(style="text-align:center;font-size:16px;line-height:2.2;",
              shiny::strong(sprintf("Fraud Probability: %.2f%%", sc*100)), shiny::br(),
              sprintf("Threshold: %.3f [Bootstrap CI: %.3f – %.3f]",
                      th, rv$pipe$bootstrap$ci_lower, rv$pipe$bootstrap$ci_upper),
              shiny::br(),
              shiny::p(style="font-size:12px;color:#888;",
                       "Model trained on TRAIN SET only - this score is from the trained model"),
              if(abs(zs)>1.96)
                shiny::span(style="color:#e67e22;font-weight:bold;",
                             sprintf("CLT: Amount EUR %.2f is unusual (z=%.2f)", input$tx_amt, zs))
              else
                shiny::span(style="color:#27ae60;",
                             sprintf("CLT: Amount EUR %.2f is normal (z=%.2f)", input$tx_amt, zs))
            )
          )
        })
        output$plt_gauge <- plotly::renderPlotly({
          plotly::plot_ly(type="indicator",mode="gauge+number+delta",
            value=round(sc*100,2),
            title=list(text="Fraud Risk Score (%)",font=list(size=18)),
            delta=list(reference=th*100),
            gauge=list(axis=list(range=list(0,100)),
              bar=list(color=if(sc>=th)"#e74c3c" else "#27ae60"),
              steps=list(list(range=c(0,th*100),color="#d5f5e3"),
                         list(range=c(th*100,100),color="#fadbd8")),
              threshold=list(line=list(color="black",width=4),
                             thickness=0.8,value=th*100)))
        })
      }, error=function(e)
        shiny::showNotification(paste("Scoring error:", e$message), type="error"))
    })
  }

  shiny::shinyApp(ui=ui, server=server,
                   options=list(port=port, launch.browser=launch.browser))
}
