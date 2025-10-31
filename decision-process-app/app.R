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
      nav_panel("Results (visual)",
                card(
                  card_header("Overall score distributions (5,000 sims)"),
                  plotOutput("overall_plot", height = "360px"),
                  tableOutput("overall_tbl")
                )
      ),
      nav_panel("Summary (text)",
                verbatimTextOutput("summary_txt")
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
  
  output$overall_plot <- renderPlot({
    od <- overall_draws(); req(od)
    long <- bind_rows(
      transmute(od, value = scoreA, choice = input$choiceA),
      transmute(od, value = scoreB, choice = input$choiceB)
    )
    ggplot(long, aes(x=value, fill=choice)) +
      geom_density(alpha=.35) +
      labs(x="Overall score (Σ Importance × Presence across 5 aspects)",
           y="Density", fill=NULL) +
      theme_minimal(base_size = 13)
  })
  
  output$overall_tbl <- renderTable({
    od <- overall_draws(); req(od)
    tibble(
      choice = c(input$choiceA, input$choiceB),
      mean   = c(mean(od$scoreA), mean(od$scoreB)),
      width_90 = c(quantile(od$scoreA,.95)-quantile(od$scoreA,.05),
                   quantile(od$scoreB,.95)-quantile(od$scoreB,.05))
    ) |> mutate(across(-choice, ~round(.x,3)))
  }, striped=TRUE, bordered=TRUE, spacing="s")
  
  output$summary_txt <- renderText({
    od <- overall_draws(); req(od)

    # Get included aspect names
    included_names <- sapply(1:5, function(i) {
      if (aspects[[i]]$include()) aspects[[i]]$name() else NULL
    })
    included_names <- unlist(included_names[!sapply(included_names, is.null)])

    pAgtB <- mean(od$scoreA > od$scoreB)
    paste0(
      "Choices: ", input$choiceA, " vs ", input$choiceB, "\n",
      "Included aspects (", length(included_names), "/5): ", paste(included_names, collapse = ", "), "\n\n",
      "Computation (per aspect):\n",
      "  Importance ~ Beta(α,β) from Importance slider, scaled by its Uncertainty;\n",
      "  Presence_A, Presence_B ~ Beta(α,β) from Presence sliders, scaled by their Uncertainty.\n",
      "Scores: Score_A = Σ Importance × Presence_A; Score_B analogously.\n",
      "Sims: 5,000.\n",
      "Result: P(A > B) = ", scales::percent(pAgtB, accuracy=0.1),
      "\nMeans/width_90 shown above."
    )
  })
}

shinyApp(ui, server)
