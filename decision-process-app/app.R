# app.R — Decision Space v3 (vertical sliders + aspect inclusion toggle)

if (!requireNamespace("shiny")) install.packages("shiny")
if (!requireNamespace("bslib")) install.packages("bslib")
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("dplyr")) install.packages("dplyr")
if (!requireNamespace("tidyr")) install.packages("tidyr")
if (!requireNamespace("purrr")) install.packages("purrr")
if (!requireNamespace("shinyjs")) install.packages("shinyjs")
if (!requireNamespace("scales")) install.packages("scales")

library(shiny); library(bslib); library(ggplot2)
library(dplyr); library(tidyr); library(purrr); library(shinyjs); library(scales)

# Color-blind friendly and fun palette (Okabe-Ito inspired)
COLORS <- list(
  orange = "#E69F00",
  skyblue = "#56B4E9",
  green = "#009E73",
  yellow = "#F0E442",
  blue = "#0072B2",
  red = "#D55E00",
  pink = "#CC79A7",
  purple = "#9370DB"
)

# ---------- helpers ----------
map_importance_to_ab <- function(v) {
  # v in [0,1]; L=(1,20) -> H=(20,1)
  alpha <- 1 + 19*v
  beta  <- 20 - 19*v
  c(alpha = alpha, beta = beta)
}
map_uncertainty_scale <- function(v) {
  # v ∈ [0,1]; 0 = tight, 1 = loose
  # Log interpolation for wider uncertainty range
  tight <- 400   # very tight
  loose <- 0.02  # very loose
  exp(log(tight) + (log(loose) - log(tight)) * v)
}
beta_draws <- function(alpha, beta, n=5000) rbeta(n, alpha, beta)

aspect_module_ui <- function(id, idx, choiceA_name, choiceB_name) {
  ns <- NS(id)
  card(
    card_header(
      div(style = "display: flex; justify-content: space-between; align-items: center;",
        span(paste("Decision Aspect", idx)),
        checkboxInput(ns("include"), "Include in analysis", value = TRUE, width = "auto")
      )
    ),
    # Main content container with conditional styling
    div(id = ns("content_wrapper"),
      # Name input
      textInput(ns("name"), "Name of aspect", value = paste("Aspect", idx)),

      # Aspect Importance group
      tags$h5("Aspect Importance"),
      div(class = "group-box",
        sliderInput(ns("imp"), "Importance", min=0, max=1, value=0.5, step=0.01),
        sliderInput(ns("imp_unc"), "Uncertainty", min=0, max=1, value=0.4, step=0.01)
      ),

      # Presence group with 2x2 grid (reactive choice names in labels)
      tags$h5("Presence"),
      div(class = "group-box",
        layout_column_wrap(
          width = 1/2,
          sliderInput(ns("pres_a"), uiOutput(ns("label_pres_a"), inline = TRUE), min=0, max=1, value=0.6, step=0.01),
          sliderInput(ns("unc_a"), uiOutput(ns("label_unc_a"), inline = TRUE), min=0, max=1, value=0.5, step=0.01),
          sliderInput(ns("pres_b"), uiOutput(ns("label_pres_b"), inline = TRUE), min=0, max=1, value=0.5, step=0.01),
          sliderInput(ns("unc_b"), uiOutput(ns("label_unc_b"), inline = TRUE), min=0, max=1, value=0.5, step=0.01)
        )
      ),

      # Plot and table below sliders (full-width)
      plotOutput(ns("plot"), height = "340px"),
      tableOutput(ns("params"))
    )
  )
}

aspect_module_server <- function(id, choiceA_name, choiceB_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render reactive slider labels
    output$label_pres_a <- renderUI({
      paste(choiceA_name(), "Presence")
    })

    output$label_unc_a <- renderUI({
      paste(choiceA_name(), "Uncertainty")
    })

    output$label_pres_b <- renderUI({
      paste(choiceB_name(), "Presence")
    })

    output$label_unc_b <- renderUI({
      paste(choiceB_name(), "Uncertainty")
    })

    # Observe include toggle and dim UI when excluded
    observe({
      if (input$include) {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').style.opacity = '1';",
          ns("content_wrapper")
        ))
      } else {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').style.opacity = '0.4';",
          ns("content_wrapper")
        ))
      }
    })

    params <- reactive({
      # importance
      imp_ab    <- map_importance_to_ab(input$imp)
      imp_scale <- map_uncertainty_scale(input$imp_unc)
      imp_a     <- imp_ab["alpha"]*imp_scale
      imp_b     <- imp_ab["beta"] *imp_scale
      # presence A
      pa_ab     <- map_importance_to_ab(input$pres_a)
      pa_scale  <- map_uncertainty_scale(input$unc_a)
      pa_a      <- pa_ab["alpha"]*pa_scale
      pa_b      <- pa_ab["beta"] *pa_scale
      # presence B
      pb_ab     <- map_importance_to_ab(input$pres_b)
      pb_scale  <- map_uncertainty_scale(input$unc_b)
      pb_a      <- pb_ab["alpha"]*pb_scale
      pb_b      <- pb_ab["beta"] *pb_scale

      tibble(
        dist  = c("Importance", "Presence A", "Presence B"),
        alpha = c(imp_a, pa_a, pb_a),
        beta  = c(imp_b, pa_b, pb_b)
      )
    })

    draws <- reactive({
      p <- params()
      tibble(
        imp   = beta_draws(p$alpha[1], p$beta[1]),
        presA = beta_draws(p$alpha[2], p$beta[2]),
        presB = beta_draws(p$alpha[3], p$beta[3])
      ) |>
        mutate(scoreA = imp * presA,
               scoreB = imp * presB)
    })

    # vertical facets (one row per distribution)
    output$plot <- renderPlot({
      d <- draws()
      long <- bind_rows(
        transmute(d, value = imp,   dist = "Importance"),
        transmute(d, value = presA, dist = paste("Presence", choiceA_name())),
        transmute(d, value = presB, dist = paste("Presence", choiceB_name()))
      )
      ggplot(long, aes(x = value)) +
        geom_density(fill = "grey60", alpha = 0.35) +
        facet_grid(dist ~ ., scales = "free_y", switch = "y") +
        labs(x = "Value", y = NULL,
             subtitle = "Parameterized Beta distributions (after uncertainty scaling)") +
        theme_minimal(base_size = 12) +
        theme(strip.placement = "outside",
              strip.background = element_blank())
    })

    # lean table: mean + 90% interval WIDTH (95th - 5th)
    output$params <- renderTable({
      d <- draws()
      tibble(
        dist = c("Importance", paste("Presence", choiceA_name()), paste("Presence", choiceB_name())),
        mean = c(mean(d$imp), mean(d$presA), mean(d$presB)),
        width_90 = c(quantile(d$imp,.95) - quantile(d$imp,.05),
                     quantile(d$presA,.95) - quantile(d$presA,.05),
                     quantile(d$presB,.95) - quantile(d$presB,.05))
      ) |>
        mutate(across(-dist, ~round(.x, 3)))
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    list(
      name = reactive(input$name),
      draws = draws,
      include = reactive(input$include)
    )
  })
}

# ---------- UI ----------
theme <- bs_theme(bootswatch = "flatly")
ui <- page_fluid(
  theme = theme,
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
  titlePanel("Decision Space — Two-Choice, Five-Aspect Simulator"),
  layout_sidebar(
    sidebar = sidebar(
      textInput("choiceA", "Choice A name", "Choice A"),
      textInput("choiceB", "Choice B name", "Choice B"),
      actionButton("run", "Recompute", class="btn-primary")
    ),
    navset_card_pill(
      nav_panel("Decision Space",
                lapply(1:5, function(i) aspect_module_ui(paste0("aspect", i), i, NULL, NULL))
      ),
      nav_panel("Report",
                # Decision Summary
                card(
                  card_header("Decision Recommendation", class = "bg-primary"),
                  div(style = "padding: 1rem; font-size: 1.1rem;",
                    uiOutput("decision_summary")
                  )
                ),
                # Overview cards
                layout_column_wrap(
                  width = 1/3,
                  card(uiOutput("header_meanA"), verbatimTextOutput("card_meanA", placeholder = TRUE)),
                  card(uiOutput("header_meanB"), verbatimTextOutput("card_meanB", placeholder = TRUE)),
                  card(card_header("Mean Difference"), verbatimTextOutput("card_meanD", placeholder = TRUE))
                ),
                layout_column_wrap(
                  width = 1/3,
                  card(uiOutput("header_pAgtB"), verbatimTextOutput("card_pAgtB", placeholder = TRUE)),
                  card(uiOutput("header_pBgtA"), verbatimTextOutput("card_pBgtA", placeholder = TRUE)),
                  card(card_header("P(within 5%)"), verbatimTextOutput("card_pWithin5", placeholder = TRUE))
                ),
                # Plots
                card(
                  card_header("Score Distributions"),
                  plotOutput("plot_distributions", height = "300px")
                ),
                card(
                  uiOutput("header_difference"),
                  plotOutput("plot_difference", height = "300px")
                ),
                # Tables
                card(
                  card_header("Advantage Thresholds"),
                  p("Probability that one choice has a substantial advantage over the other:",
                    style = "font-size: 0.9rem; color: #666; margin-bottom: 0.5rem;"),
                  tableOutput("tbl_thresholds")
                ),
                card(
                  card_header("Summary Statistics"),
                  tableOutput("tbl_intervals")
                ),
                # Download buttons
                card(
                  card_header("Export"),
                  p("Download a complete report with all statistics and visualizations:",
                    style = "font-size: 0.9rem; color: #666; margin-bottom: 1rem;"),
                  div(style = "display: flex; gap: 0.5rem;",
                    downloadButton("dl_html", "Download HTML", class = "btn-primary"),
                    downloadButton("dl_pdf", "Download PDF", class = "btn-secondary")
                  ),
                  p(style = "font-size: 0.85rem; color: #999; margin-top: 0.5rem;",
                    "Note: Use 'Open in Browser' for downloads if using RStudio Viewer")
                )
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  # fixed sims; button just re-runs reactives cleanly
  observeEvent(input$run, {}, ignoreInit = TRUE)

  # Reactive choice names with fallbacks
  choiceA_name <- reactive(ifelse(nzchar(input$choiceA), input$choiceA, "Choice A"))
  choiceB_name <- reactive(ifelse(nzchar(input$choiceB), input$choiceB, "Choice B"))

  aspects <- lapply(1:5, function(i) {
    aspect_module_server(paste0("aspect", i), choiceA_name, choiceB_name)
  })
  
  overall_draws <- reactive({
    # Filter to only included aspects
    included_aspects <- aspects[sapply(aspects, function(m) m$include())]

    # If no aspects are included, return NULL
    if (length(included_aspects) == 0) return(NULL)

    mats <- lapply(included_aspects, function(m) m$draws())
    reduce(mats, function(acc, d){
      if (is.null(acc)) return(d %>% select(scoreA, scoreB))
      acc + d %>% select(scoreA, scoreB)
    }, .init = NULL)
  })
  
  # Comprehensive statistics
  report_stats <- reactive({
    od <- overall_draws(); req(od)

    A <- od$scoreA
    B <- od$scoreB
    D <- A - B
    R <- A / pmax(B, 1e-6)  # Clip small denominators

    # Basic stats
    mean_A <- mean(A)
    mean_B <- mean(B)
    mean_D <- mean(D)
    width_A <- quantile(A, 0.95) - quantile(A, 0.05)
    width_B <- quantile(B, 0.95) - quantile(B, 0.05)
    width_D <- quantile(D, 0.95) - quantile(D, 0.05)

    # Probabilities
    p_AgtB <- mean(A > B)
    p_BgtA <- mean(B > A)
    p_within5 <- mean(R >= 0.95 & R <= 1.05)

    # Threshold probabilities (use reactive names)
    thresholds <- c(0.05, 0.10, 0.20, 0.50)
    nameA <- choiceA_name()
    nameB <- choiceB_name()
    threshold_stats <- tibble(
      `Margin` = paste0(thresholds * 100, "%"),
      !!paste0(nameA, " has this advantage") := sapply(thresholds, function(tau) mean(A >= (1 + tau) * B)),
      !!paste0(nameB, " has this advantage") := sapply(thresholds, function(tau) mean(B >= (1 + tau) * A))
    )

    list(
      A = A, B = B, D = D, R = R,
      mean_A = mean_A, mean_B = mean_B, mean_D = mean_D,
      width_A = width_A, width_B = width_B, width_D = width_D,
      p_AgtB = p_AgtB, p_BgtA = p_BgtA, p_within5 = p_within5,
      threshold_stats = threshold_stats
    )
  })

  # Decision Summary
  output$decision_summary <- renderUI({
    stats <- report_stats()

    # Determine winner
    if (stats$p_AgtB > 0.75) {
      strength <- if (stats$p_AgtB > 0.90) "strongly" else "likely"
      winner <- choiceA_name()
      prob <- stats$p_AgtB
      diff <- stats$mean_D
      icon_col <- COLORS$green
    } else if (stats$p_BgtA > 0.75) {
      strength <- if (stats$p_BgtA > 0.90) "strongly" else "likely"
      winner <- choiceB_name()
      prob <- stats$p_BgtA
      diff <- -stats$mean_D
      icon_col <- COLORS$blue
    } else {
      # Too close to call
      return(HTML(sprintf(
        '<div style="color: %s;"><strong>📊 The choices are very close!</strong></div>
        <p>The analysis shows that <strong>%s</strong> and <strong>%s</strong> are within 5%% of each other with <strong>%s</strong> probability.
        The mean difference is only <strong>%.3f points</strong> (90%% interval width: %.3f).</p>
        <p style="color: #666; font-size: 0.95rem;">💡 Consider: Are there other factors not captured in these aspects that might tip the balance?</p>',
        COLORS$yellow,
        choiceA_name(), choiceB_name(),
        scales::percent(stats$p_within5, accuracy = 0.1),
        abs(stats$mean_D), stats$width_D
      )))
    }

    HTML(sprintf(
      '<div style="color: %s;"><strong>✨ %s is %s the better choice!</strong></div>
      <p>Based on your decision aspects, <strong>%s</strong> outperforms with <strong>%s</strong> probability.
      The average advantage is <strong>%.3f points</strong> (90%% interval width: %.3f).</p>
      <p style="color: #666; font-size: 0.95rem;">💡 This recommendation is based on %d included aspect(s). Adjust the sliders or toggle aspects to explore different scenarios.</p>',
      icon_col,
      winner, strength,
      winner,
      scales::percent(prob, accuracy = 0.1),
      abs(diff), stats$width_D,
      length(unlist(sapply(1:5, function(i) if (aspects[[i]]$include()) aspects[[i]]$name() else NULL)))
    ))
  })

  # Dynamic card headers using reactive choice names
  output$header_meanA <- renderUI({
    card_header(paste("Mean", choiceA_name()))
  })

  output$header_meanB <- renderUI({
    card_header(paste("Mean", choiceB_name()))
  })

  output$header_pAgtB <- renderUI({
    card_header(paste0("P(", choiceA_name(), " > ", choiceB_name(), ")"))
  })

  output$header_pBgtA <- renderUI({
    card_header(paste0("P(", choiceB_name(), " > ", choiceA_name(), ")"))
  })

  output$header_difference <- renderUI({
    card_header(paste0("Difference Distribution (", choiceA_name(), " - ", choiceB_name(), ")"))
  })

  # Overview cards
  output$card_meanA <- renderText({
    stats <- report_stats()
    sprintf("%.3f", stats$mean_A)
  })

  output$card_meanB <- renderText({
    stats <- report_stats()
    sprintf("%.3f", stats$mean_B)
  })

  output$card_meanD <- renderText({
    stats <- report_stats()
    sprintf("%.3f\n(90%% width: %.3f)", stats$mean_D, stats$width_D)
  })

  output$card_pAgtB <- renderText({
    stats <- report_stats()
    scales::percent(stats$p_AgtB, accuracy = 0.1)
  })

  output$card_pBgtA <- renderText({
    stats <- report_stats()
    scales::percent(stats$p_BgtA, accuracy = 0.1)
  })

  output$card_pWithin5 <- renderText({
    stats <- report_stats()
    scales::percent(stats$p_within5, accuracy = 0.1)
  })

  # Plots
  output$plot_distributions <- renderPlot({
    stats <- report_stats()
    long <- bind_rows(
      tibble(value = stats$A, choice = choiceA_name()),
      tibble(value = stats$B, choice = choiceB_name())
    )
    ggplot(long, aes(x = value, fill = choice)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = c(COLORS$orange, COLORS$skyblue)) +
      labs(x = "Overall Score", y = "Density", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            legend.text = element_text(size = 12, face = "bold"))
  })

  output$plot_difference <- renderPlot({
    stats <- report_stats()
    ggplot(tibble(D = stats$D), aes(x = D)) +
      geom_density(fill = COLORS$purple, alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = COLORS$red, linewidth = 1.2) +
      annotate("text", x = 0, y = Inf, label = "No difference",
               vjust = 2, hjust = -0.1, color = COLORS$red, size = 4, fontface = "bold") +
      labs(x = paste(choiceA_name(), "-", choiceB_name()), y = "Density") +
      theme_minimal(base_size = 13)
  })

  # Tables
  output$tbl_thresholds <- renderTable({
    stats <- report_stats()
    stats$threshold_stats |>
      mutate(across(-Margin, ~scales::percent(.x, accuracy = 0.1)))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$tbl_intervals <- renderTable({
    stats <- report_stats()
    tibble(
      Choice = c(choiceA_name(), choiceB_name(), paste0("Difference (", choiceA_name(), " - ", choiceB_name(), ")")),
      Mean = c(stats$mean_A, stats$mean_B, stats$mean_D),
      `90% Width` = c(stats$width_A, stats$width_B, stats$width_D)
    ) |> mutate(across(-Choice, ~round(.x, 3)))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # HTML Download Handler
  output$dl_html <- downloadHandler(
    filename = function() {
      paste0("decision_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      # Check if rmarkdown is available
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        showNotification(
          "rmarkdown package required. Install with: install.packages('rmarkdown')",
          type = "error",
          duration = 10
        )
        return(NULL)
      }

      # Get stats
      stats <- report_stats()

      # Get included aspect names
      included_names <- sapply(1:5, function(i) {
        if (aspects[[i]]$include()) aspects[[i]]$name() else NULL
      })
      included_names <- unlist(included_names[!sapply(included_names, is.null)])
      included_text <- paste(included_names, collapse = ", ")

      # Save plots to temp files
      overall_png <- file.path(tempdir(), "overall.png")
      diff_png <- file.path(tempdir(), "diff.png")

      p1 <- ggplot(bind_rows(
        tibble(value = stats$A, choice = choiceA_name()),
        tibble(value = stats$B, choice = choiceB_name())
      ), aes(x = value, fill = choice)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c(COLORS$orange, COLORS$skyblue)) +
        labs(x = "Overall Score", y = "Density", fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "top",
              legend.text = element_text(size = 12, face = "bold"))

      p2 <- ggplot(tibble(D = stats$D), aes(x = D)) +
        geom_density(fill = COLORS$purple, alpha = 0.5) +
        geom_vline(xintercept = 0, linetype = "dashed", color = COLORS$red, linewidth = 1.2) +
        annotate("text", x = 0, y = Inf, label = "No difference",
                 vjust = 2, hjust = -0.1, color = COLORS$red, size = 4, fontface = "bold") +
        labs(x = paste(choiceA_name(), "-", choiceB_name()), y = "Density") +
        theme_minimal(base_size = 13)

      ggsave(overall_png, p1, width = 8, height = 4, dpi = 150)
      ggsave(diff_png, p2, width = 8, height = 4, dpi = 150)

      # Prepare intervals table
      intervals_tbl <- tibble(
        Choice = c(choiceA_name(), choiceB_name(), paste0("Difference (", choiceA_name(), " - ", choiceB_name(), ")")),
        Mean = c(stats$mean_A, stats$mean_B, stats$mean_D),
        `90% Width` = c(stats$width_A, stats$width_B, stats$width_D)
      )

      # Prepare threshold stats table (already formatted for display)
      threshold_tbl <- stats$threshold_stats %>%
        mutate(across(-Margin, ~sprintf("%.1f%%", .x * 100)))

      # Render to temp HTML then copy
      tmp_html <- tempfile(fileext = ".html")
      rmarkdown::render(
        input = "report/report.Rmd",
        output_format = rmarkdown::html_document(self_contained = TRUE, theme = "flatly"),
        output_file = tmp_html,
        params = list(
          choiceA = choiceA_name(),
          choiceB = choiceB_name(),
          mean_A = stats$mean_A,
          mean_B = stats$mean_B,
          mean_D = stats$mean_D,
          width_D = stats$width_D,
          p_AgtB = stats$p_AgtB,
          p_BgtA = stats$p_BgtA,
          p_within5 = stats$p_within5,
          threshold_stats = threshold_tbl,
          intervals_tbl = intervals_tbl,
          img_overall = overall_png,
          img_diff = diff_png,
          included_aspects = included_text
        ),
        envir = new.env(parent = globalenv())
      )

      file.copy(tmp_html, file, overwrite = TRUE)
    }
  )

  # PDF Download Handler
  output$dl_pdf <- downloadHandler(
    filename = function() {
      paste0("decision_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      # Check if rmarkdown is available
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        showNotification(
          "rmarkdown package required. Install with: install.packages('rmarkdown')",
          type = "error",
          duration = 10
        )
        return(NULL)
      }

      # Check if tinytex is available for PDF generation
      if (!requireNamespace("tinytex", quietly = TRUE)) {
        showNotification(
          "PDF export requires tinytex package. Install with: install.packages('tinytex'); tinytex::install_tinytex()",
          type = "error",
          duration = 15
        )
        return(NULL)
      }

      if (!tinytex::is_tinytex()) {
        showNotification(
          "PDF export requires TinyTeX. Install with: tinytex::install_tinytex()",
          type = "error",
          duration = 15
        )
        return(NULL)
      }

      # Get stats
      stats <- report_stats()

      # Get included aspect names
      included_names <- sapply(1:5, function(i) {
        if (aspects[[i]]$include()) aspects[[i]]$name() else NULL
      })
      included_names <- unlist(included_names[!sapply(included_names, is.null)])
      included_text <- paste(included_names, collapse = ", ")

      # Save plots to temp files
      overall_png <- file.path(tempdir(), "overall.png")
      diff_png <- file.path(tempdir(), "diff.png")

      p1 <- ggplot(bind_rows(
        tibble(value = stats$A, choice = choiceA_name()),
        tibble(value = stats$B, choice = choiceB_name())
      ), aes(x = value, fill = choice)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c(COLORS$orange, COLORS$skyblue)) +
        labs(x = "Overall Score", y = "Density", fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "top",
              legend.text = element_text(size = 12, face = "bold"))

      p2 <- ggplot(tibble(D = stats$D), aes(x = D)) +
        geom_density(fill = COLORS$purple, alpha = 0.5) +
        geom_vline(xintercept = 0, linetype = "dashed", color = COLORS$red, linewidth = 1.2) +
        annotate("text", x = 0, y = Inf, label = "No difference",
                 vjust = 2, hjust = -0.1, color = COLORS$red, size = 4, fontface = "bold") +
        labs(x = paste(choiceA_name(), "-", choiceB_name()), y = "Density") +
        theme_minimal(base_size = 13)

      ggsave(overall_png, p1, width = 8, height = 4, dpi = 150)
      ggsave(diff_png, p2, width = 8, height = 4, dpi = 150)

      # Prepare intervals table
      intervals_tbl <- tibble(
        Choice = c(choiceA_name(), choiceB_name(), paste0("Difference (", choiceA_name(), " - ", choiceB_name(), ")")),
        Mean = c(stats$mean_A, stats$mean_B, stats$mean_D),
        `90% Width` = c(stats$width_A, stats$width_B, stats$width_D)
      )

      # Prepare threshold stats table
      threshold_tbl <- stats$threshold_stats %>%
        mutate(across(-Margin, ~sprintf("%.1f%%", .x * 100)))

      # Render to temp PDF then copy
      tmp_pdf <- tempfile(fileext = ".pdf")
      rmarkdown::render(
        input = "report/report.Rmd",
        output_format = rmarkdown::pdf_document(keep_tex = FALSE),
        output_file = tmp_pdf,
        params = list(
          choiceA = choiceA_name(),
          choiceB = choiceB_name(),
          mean_A = stats$mean_A,
          mean_B = stats$mean_B,
          mean_D = stats$mean_D,
          width_D = stats$width_D,
          p_AgtB = stats$p_AgtB,
          p_BgtA = stats$p_BgtA,
          p_within5 = stats$p_within5,
          threshold_stats = threshold_tbl,
          intervals_tbl = intervals_tbl,
          img_overall = overall_png,
          img_diff = diff_png,
          included_aspects = included_text
        ),
        envir = new.env(parent = globalenv())
      )

      file.copy(tmp_pdf, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
