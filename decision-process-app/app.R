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

# ---------- helpers ----------
map_importance_to_ab <- function(v) {
  # v in [0,1]; L=(1,20) -> H=(20,1)
  alpha <- 1 + 19*v
  beta  <- 20 - 19*v
  c(alpha = alpha, beta = beta)
}
map_uncertainty_scale <- function(v) {
  # v in [0,1]; L = ×100  -> H = ×0.25
  100*(1 - v) + 0.25*v
}
beta_draws <- function(alpha, beta, n=5000) rbeta(n, alpha, beta)

aspect_module_ui <- function(id, idx) {
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

      # Importance sliders (vertical stack)
      tags$h6(style = "margin-top: 0.5rem; margin-bottom: 0.25rem;", "Importance"),
      div(style = "margin-bottom: 0.75rem;",
        sliderInput(ns("imp"), "Level", min=0, max=1, value=0.5, step=0.01),
        sliderInput(ns("imp_unc"), "Uncertainty", min=0, max=1, value=0.4, step=0.01)
      ),

      tags$hr(style = "margin: 0.5rem 0;"),

      # Choice A sliders (vertical stack)
      tags$h6(style = "margin-top: 0.5rem; margin-bottom: 0.25rem;", "Choice A"),
      div(style = "margin-bottom: 0.75rem;",
        sliderInput(ns("pres_a"), "Presence", min=0, max=1, value=0.6, step=0.01),
        sliderInput(ns("unc_a"), "Uncertainty", min=0, max=1, value=0.5, step=0.01)
      ),

      # Choice B sliders (vertical stack)
      tags$h6(style = "margin-top: 0.5rem; margin-bottom: 0.25rem;", "Choice B"),
      div(style = "margin-bottom: 0.75rem;",
        sliderInput(ns("pres_b"), "Presence", min=0, max=1, value=0.5, step=0.01),
        sliderInput(ns("unc_b"), "Uncertainty", min=0, max=1, value=0.5, step=0.01)
      ),

      # Plot and table below sliders (full-width)
      plotOutput(ns("plot"), height = "340px"),
      tableOutput(ns("params"))
    )
  )
}

aspect_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
        transmute(d, value = presA, dist = "Presence A"),
        transmute(d, value = presB, dist = "Presence B")
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
        dist = c("Importance","Presence A","Presence B"),
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
  titlePanel("Decision Space — Two-Choice, Five-Aspect Simulator"),
  layout_sidebar(
    sidebar = sidebar(
      textInput("choiceA", "Choice A name", "Choice A"),
      textInput("choiceB", "Choice B name", "Choice B"),
      actionButton("run", "Recompute", class="btn-primary")
    ),
    navset_card_pill(
      nav_panel("Decision Space",
                lapply(1:5, function(i) aspect_module_ui(paste0("aspect", i), i))
      ),
      nav_panel("Report",
                # Overview cards
                layout_column_wrap(
                  width = 1/3,
                  card(card_header("Mean A"), verbatimTextOutput("card_meanA", placeholder = TRUE)),
                  card(card_header("Mean B"), verbatimTextOutput("card_meanB", placeholder = TRUE)),
                  card(card_header("Mean Difference"), verbatimTextOutput("card_meanD", placeholder = TRUE))
                ),
                layout_column_wrap(
                  width = 1/3,
                  card(card_header("P(A > B)"), verbatimTextOutput("card_pAgtB", placeholder = TRUE)),
                  card(card_header("P(B > A)"), verbatimTextOutput("card_pBgtA", placeholder = TRUE)),
                  card(card_header("P(within 5%)"), verbatimTextOutput("card_pWithin5", placeholder = TRUE))
                ),
                # Plots
                card(
                  card_header("Score Distributions"),
                  plotOutput("plot_distributions", height = "300px")
                ),
                card(
                  card_header("Difference Distribution (A - B)"),
                  plotOutput("plot_difference", height = "300px")
                ),
                # Tables
                card(
                  card_header("Threshold Probabilities"),
                  tableOutput("tbl_thresholds")
                ),
                card(
                  card_header("Summary Statistics"),
                  tableOutput("tbl_intervals")
                ),
                # Download button
                card(
                  card_header("Export"),
                  downloadButton("dl_report", "Download PDF Report", class = "btn-primary")
                )
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  # fixed sims; button just re-runs reactives cleanly
  observeEvent(input$run, {}, ignoreInit = TRUE)
  
  aspects <- lapply(1:5, function(i) aspect_module_server(paste0("aspect", i)))
  
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

    # Threshold probabilities
    thresholds <- c(0.05, 0.10, 0.20, 0.50)
    threshold_stats <- tibble(
      tau = thresholds,
      `P(A ≥ (1+τ)B)` = sapply(thresholds, function(tau) mean(A >= (1 + tau) * B)),
      `P(B ≥ (1+τ)A)` = sapply(thresholds, function(tau) mean(B >= (1 + tau) * A))
    )

    list(
      A = A, B = B, D = D, R = R,
      mean_A = mean_A, mean_B = mean_B, mean_D = mean_D,
      width_A = width_A, width_B = width_B, width_D = width_D,
      p_AgtB = p_AgtB, p_BgtA = p_BgtA, p_within5 = p_within5,
      threshold_stats = threshold_stats
    )
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
      tibble(value = stats$A, choice = input$choiceA),
      tibble(value = stats$B, choice = input$choiceB)
    )
    ggplot(long, aes(x = value, fill = choice)) +
      geom_density(alpha = 0.35) +
      labs(x = "Overall Score", y = "Density", fill = NULL) +
      theme_minimal(base_size = 13)
  })

  output$plot_difference <- renderPlot({
    stats <- report_stats()
    ggplot(tibble(D = stats$D), aes(x = D)) +
      geom_density(fill = "steelblue", alpha = 0.35) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      labs(x = paste(input$choiceA, "-", input$choiceB), y = "Density") +
      theme_minimal(base_size = 13)
  })

  # Tables
  output$tbl_thresholds <- renderTable({
    stats <- report_stats()
    stats$threshold_stats |>
      mutate(across(-tau, ~scales::percent(.x, accuracy = 0.1)))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$tbl_intervals <- renderTable({
    stats <- report_stats()
    tibble(
      Choice = c(input$choiceA, input$choiceB),
      Mean = c(stats$mean_A, stats$mean_B),
      `90% Width` = c(stats$width_A, stats$width_B)
    ) |> mutate(across(-Choice, ~round(.x, 3)))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  # PDF Download Handler
  output$dl_report <- downloadHandler(
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

      # Create temp directory for plots
      tempdir_plots <- tempdir()
      plot_dist_path <- file.path(tempdir_plots, "plot_distributions.png")
      plot_diff_path <- file.path(tempdir_plots, "plot_difference.png")

      # Get stats
      stats <- report_stats()

      # Get included aspect names
      included_names <- sapply(1:5, function(i) {
        if (aspects[[i]]$include()) aspects[[i]]$name() else NULL
      })
      included_names <- unlist(included_names[!sapply(included_names, is.null)])
      included_text <- paste(included_names, collapse = ", ")

      # Save plots
      p1 <- ggplot(bind_rows(
        tibble(value = stats$A, choice = input$choiceA),
        tibble(value = stats$B, choice = input$choiceB)
      ), aes(x = value, fill = choice)) +
        geom_density(alpha = 0.35) +
        labs(x = "Overall Score", y = "Density", fill = NULL) +
        theme_minimal(base_size = 13)

      p2 <- ggplot(tibble(D = stats$D), aes(x = D)) +
        geom_density(fill = "steelblue", alpha = 0.35) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
        labs(x = paste(input$choiceA, "-", input$choiceB), y = "Density") +
        theme_minimal(base_size = 13)

      ggsave(plot_dist_path, p1, width = 8, height = 4, dpi = 150)
      ggsave(plot_diff_path, p2, width = 8, height = 4, dpi = 150)

      # Prepare intervals table
      intervals_tbl <- tibble(
        Choice = c(input$choiceA, input$choiceB),
        Mean = c(stats$mean_A, stats$mean_B),
        `90% Width` = c(stats$width_A, stats$width_B)
      )

      # Prepare threshold stats table
      threshold_tbl <- stats$threshold_stats %>%
        mutate(across(-tau, ~sprintf("%.1f%%", .x * 100)))

      # Render the report
      rmarkdown::render(
        input = "report/report.Rmd",
        output_file = file,
        params = list(
          choiceA = input$choiceA,
          choiceB = input$choiceB,
          mean_A = stats$mean_A,
          mean_B = stats$mean_B,
          mean_D = stats$mean_D,
          width_D = stats$width_D,
          p_AgtB = stats$p_AgtB,
          p_BgtA = stats$p_BgtA,
          p_within5 = stats$p_within5,
          threshold_stats = threshold_tbl,
          intervals_tbl = intervals_tbl,
          plot_dist_path = plot_dist_path,
          plot_diff_path = plot_diff_path,
          included_aspects = included_text
        ),
        envir = new.env()
      )
    }
  )
}

shinyApp(ui, server)
