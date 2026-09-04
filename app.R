required_pkgs <- c("shiny", "ggplot2", "car", "lmtest", "sandwich")

missing <- required_pkgs[!vapply(required_pkgs,
                                 requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
  missing <- required_pkgs[!vapply(required_pkgs,
                                   requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Could not install the required packages: ",
         paste(missing, collapse = ", "))
  }
}

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(car)
  library(lmtest)
  library(sandwich)
})

options(warn = -1)

ui <- fluidPage(
  titlePanel("Regression Diagnostics Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file",
                accept = c(".csv", ".txt", "text/csv")),
      uiOutput("dv_ui"),
      uiOutput("iv_ui"),
      checkboxInput("robust", "Use robust (HC1) standard errors", value = FALSE),
      actionButton("run", "Run Regression", class = "btn-primary"),
      br(), br(),
      textOutput("status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 h4("Model Summary"),
                 verbatimTextOutput("summ"),
                 h4("Coefficient Table"),
                 tableOutput("coef_tbl")),
        tabPanel("Residual Plots",
                 plotOutput("resid_fitted"),
                 br(),
                 uiOutput("resid_by_pred_ui")),
        tabPanel("Normality",
                 plotOutput("qq"),
                 verbatimTextOutput("shapiro")),
        tabPanel("Multicollinearity",
                 h4("Variance Inflation Factors (VIF)"),
                 tableOutput("vif_tbl"),
                 p(HTML("Rule of thumb: VIF > 10 indicates serious<br>multicollinearity; VIF > 5 is often flagged."))),
        tabPanel("Heteroskedasticity",
                 h4("Breusch-Pagan Test"),
                 verbatimTextOutput("bptest"),
                 checkboxInput("scale_smooth", "Show trend line", value = TRUE),
                 plotOutput("scale_loc")),
        tabPanel("Influential Observations",
                 h4("Cook's Distance Plot"),
                 plotOutput("cooks"),
                 h4("Influential Observations"),
                 tableOutput("infl_tbl"))
      )
    )
  )
)

server <- function(input, output, session) {

  clean_csv <- function(path) {
    lines <- readLines(path, warn = FALSE, skipNul = TRUE)
    lines <- sub("^\ufeff", "", lines)
    con <- textConnection(lines)
    on.exit(close(con))
    df <- read.table(con, sep = ",", header = TRUE,
                     check.names = FALSE, stringsAsFactors = FALSE,
                     colClasses = "character", fill = TRUE, quote = "\"")
    df
  }

  clean_numeric_vec <- function(x) {
    x <- trimws(x)
    x[x %in% c("*****", "******", "(X)", "(x)", "(D)", "(N)", "(S)",
               "-", "N", "NA", "", "+", "(F)", "(B)")] <- NA
    x <- gsub("\\$|,|\\+$|%|\\s", "", x)
    x <- gsub("^\u00b1|\u00b1", "", x)
    out <- suppressWarnings(as.numeric(x))
    out
  }

  data <- reactive({
    req(input$file)
    df <- clean_csv(input$file$datapath)
    if (ncol(df) == 0) stop("No columns found in the CSV.")

    out <- lapply(df, function(col) {
      nums <- clean_numeric_vec(col)
      if (all(is.na(nums))) return(NULL)
      nums
    })
    keep <- !vapply(out, is.null, logical(1))
    df <- as.data.frame(out[keep], stringsAsFactors = FALSE)
    names(df) <- names(out)[keep]

    df <- df[, vapply(df, function(x) sum(!is.na(x)) > 0, logical(1))]
    df <- df[, vapply(df, function(x) length(unique(x)) > 1, logical(1))]
    if (ncol(df) < 1) {
      stop("No numeric columns found in the CSV. Census files often need ",
           "the columns to contain parseable numbers (after stripping $, commas, ",
           "percent signs, and annotation codes).")
    }
    names(df) <- make.names(names(df), unique = TRUE)
    df
  })

  output$dv_ui <- renderUI({
    req(data())
    selectInput("dv", "Dependent variable", choices = names(data()))
  })

  output$iv_ui <- renderUI({
    req(data(), input$dv)
    choices <- setdiff(names(data()), input$dv)
    selectInput("iv", "Independent variables",
                choices = choices, multiple = TRUE,
                selected = choices[1])
  })

  model <- eventReactive(input$run, {
    req(data(), input$dv, input$iv)
    validate(need(length(input$iv) >= 1, "Select at least one independent variable."))
    fml <- as.formula(paste(input$dv, "~", paste(input$iv, collapse = " + ")))
    lm(fml, data = data())
  })

  output$status <- renderText({
    if (input$run == 0) "Upload a CSV and click Run Regression."
    else "Model fitted successfully."
  })

  output$summ <- renderPrint({
    req(model())
    m <- model()
    cat("R-squared:      ", round(summary(m)$r.squared, 4), "\n")
    cat("Adj R-squared:  ", round(summary(m)$adj.r.squared, 4), "\n")
    cat("F-statistic:    ", summary(m)$fstatistic[1], "\n")
    cat("Observations:   ", nobs(m), "\n")
    cat("\n")
    print(summary(m)$call)
  })

  output$coef_tbl <- renderTable({
    req(model())
    m <- model()
    if (input$robust) {
      ct <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
      co <- ct[, 1]
      se <- ct[, 2]
      tval <- ct[, 3]
      pval <- ct[, 4]
    } else {
      co <- coef(m)
      se <- summary(m)$coefficients[, 2]
      tval <- summary(m)$coefficients[, 3]
      pval <- summary(m)$coefficients[, 4]
    }
    data.frame(
      Term = names(co),
      Estimate = round(co, 4),
      Std.Error = round(se, 4),
      t.value = round(tval, 4),
      p.value = format.pval(pval, digits = 3)
    )
  }, rownames = FALSE)

  res_data <- reactive({
    m <- model()
    data.frame(
      fitted = fitted(m),
      residuals = residuals(m),
      observed = m$model[[1]],
      pred = m$model[input$iv]
    )
  })

  output$resid_fitted <- renderPlot({
    rd <- res_data()
    ggplot(rd, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(se = FALSE, color = "blue") +
      labs(x = "Fitted values", y = "Residuals",
           title = "Residuals vs Fitted")
  })

  output$resid_by_pred_ui <- renderUI({
    req(model())
    lapply(input$iv, function(v) {
      plotOutput(outputId = paste0("resid_", v))
    })
  })

  lapply_not_used <- NULL

  observe({
    req(model(), input$iv)
    m <- model()
    for (v in input$iv) {
      local({
        var <- v
        output[[paste0("resid_", var)]] <- renderPlot({
          pv <- m$model[[var]]
          ggplot(data.frame(p = pv, r = residuals(m)),
                 aes(x = p, y = r)) +
            geom_point(alpha = 0.6) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            geom_smooth(se = FALSE, color = "blue") +
            labs(x = var, y = "Residuals",
                 title = paste("Residuals vs", var))
        })
      })
    }
  })

  output$qq <- renderPlot({
    m <- model()
    r <- residuals(m)
    df <- data.frame(sample = sort(r))
    ggplot(df, aes(sample = sample)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = "Q-Q Plot of Residuals",
           x = "Theoretical Quantiles",
           y = "Sample Quantiles") +
      theme_minimal()
  })

  output$shapiro <- renderPrint({
    m <- model()
    st <- shapiro.test(residuals(m))
    cat("Shapiro-Wilk normality test\n")
    cat("W =", st$statistic, " p-value =", st$p.value, "\n")
    if (st$p.value < 0.05) {
      cat("Small p-value suggests residuals are NOT normally distributed.\n")
    } else {
      cat("No strong evidence against normality.\n")
    }
  })

  output$vif_tbl <- renderTable({
    req(model())
    if (length(input$iv) < 2) {
      return(data.frame(Note = paste(
        "VIF requires at least 2 independent variables.",
        "Add a second predictor to check for multicollinearity.")))
    }
    v <- car::vif(model())
    data.frame(Variable = names(v), VIF = round(as.numeric(v), 3))
  }, rownames = FALSE)

  output$bptest <- renderPrint({
    req(model())
    bt <- bptest(model())
    print(bt)
    if (bt$p.value < 0.05) {
      cat("\nSmall p-value suggests heteroskedasticity.\n")
    } else {
      cat("\nNo strong evidence of heteroskedasticity.\n")
    }
  })

  output$scale_loc <- renderPlot({
    rd <- res_data()
    rd$sqrt_abs_resid <- sqrt(abs(rd$residuals))
    p <- ggplot(rd, aes(x = fitted, y = sqrt_abs_resid)) +
      geom_point(alpha = 0.6) +
      labs(x = "Fitted values",
           y = expression(sqrt("|standardized residuals|")),
           title = "Scale-Location Plot")
    if (isTRUE(input$scale_smooth)) {
      p <- p + geom_smooth(se = FALSE, color = "blue")
    }
    p
  })

  infl <- reactive({
    m <- model()
    cd <- cooks.distance(m)
    n <- nobs(m)
    thresh <- 4 / n
    infl_idx <- which(cd > thresh)
    data.frame(
      Observation = infl_idx,
      Cooks_Distance = round(cd[infl_idx], 4),
      Threshold = thresh
    )
  })

  output$cooks <- renderPlot({
    m <- model()
    cd <- cooks.distance(m)
    idx <- seq_along(cd)
    ggplot(data.frame(obs = idx, cd = cd), aes(x = obs, y = cd)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 4 / length(cd), linetype = "dashed", color = "red") +
      labs(x = "Observation index", y = "Cook's distance",
           title = "Cook's Distance") +
      annotate("text", x = Inf, y = 4 / length(cd), hjust = 1, vjust = -0.5,
               label = paste("Threshold:", round(4 / length(cd), 3)))
  })

  output$infl_tbl <- renderTable({
    req(infl())
    d <- infl()
    if (nrow(d) == 0) {
      return(data.frame(Observation = "None", Cooks_Distance = "No influential observations above threshold"))
    }
    d
  }, rownames = FALSE)
}

shinyApp(ui, server)
