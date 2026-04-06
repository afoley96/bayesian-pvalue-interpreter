# Bayesian P-value Interpreter
# Shiny Application

library(shiny)

# Load core functions
source("R/sellke_berger.R")
source("R/posterior.R")
source("R/colquhoun.R")
source("R/visualizations.R")

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  
  # Title
  titlePanel("Bayesian P-value Interpreter"),
  
  # Sidebar layout
  sidebarLayout(
    
    # --- Input Panel ---
    sidebarPanel(
      
      h4("Required"),
      
      numericInput(
        inputId = "p_value",
        label = "Observed p-value:",
        value = 0.05,
        min = 0.0001,
        max = 0.999,
        step = 0.01
      ),
      
      sliderInput(
        inputId = "prior",
        label = "Prior probability (effect is real):",
        min = 0.01,
        max = 0.99,
        value = 0.5,
        step = 0.01
      ),
      
      # Quick-select buttons for prior
      fluidRow(
        column(12,
               actionButton("prior_10", "10%", class = "btn-sm"),
               actionButton("prior_30", "30%", class = "btn-sm"),
               actionButton("prior_50", "50%", class = "btn-sm"),
               actionButton("prior_70", "70%", class = "btn-sm"),
               actionButton("prior_90", "90%", class = "btn-sm")
        )
      ),
      
      hr(),
      
      # Optional: Colquhoun method
      h4("Optional (for Colquhoun method)"),
      
      numericInput(
        inputId = "n_per_group",
        label = "Sample size per group:",
        value = NA,
        min = 2,
        step = 1
      ),
      
      numericInput(
        inputId = "effect_size",
        label = "Expected effect size (Cohen's d):",
        value = NA,
        min = 0.01,
        max = 3,
        step = 0.1
      ),
      
      hr(),
      
      # Info
      p(
        style = "font-size: 12px; color: gray;",
        "Sellke-Berger method requires only p-value.",
        br(),
        "Colquhoun method also requires sample size and effect size."
      )
      
    ),
    
    # --- Output Panel ---
    mainPanel(
      
      h3("Results"),
      
      # Main result box
      wellPanel(
        h4("Posterior Probability"),
        textOutput("posterior_text"),
        br(),
        h4("False Positive Risk"),
        textOutput("fpr_text")
      ),
      
      # Bayes factor
      wellPanel(
        h4("Bayes Factor"),
        textOutput("bf_text"),
        textOutput("method_text")
      ),
      
      # Interpretation
      wellPanel(
        h4("Interpretation"),
        textOutput("interpretation_text")
      ),
      
      # Sensitivity table
      h4("Sensitivity: What if your prior was different?"),
      tableOutput("sensitivity_table")
      ,
      
      # Nomogram
      h4("Nomogram: Prior → Evidence → Posterior"),
      plotOutput("nomogram", height = "400px"),
      
      # Icon array
      h4("Visual: If 100 studies reported this p-value..."),
      htmlOutput("icon_array")
      
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # --- Prior quick-select buttons ---
  observeEvent(input$prior_10, { updateSliderInput(session, "prior", value = 0.10) })
  observeEvent(input$prior_30, { updateSliderInput(session, "prior", value = 0.30) })
  observeEvent(input$prior_50, { updateSliderInput(session, "prior", value = 0.50) })
  observeEvent(input$prior_70, { updateSliderInput(session, "prior", value = 0.70) })
  observeEvent(input$prior_90, { updateSliderInput(session, "prior", value = 0.90) })
  
  # --- Reactive: Compute results ---
  results <- reactive({
    
    req(input$p_value, input$prior)
    
    p <- input$p_value
    prior <- input$prior
    n <- input$n_per_group
    d <- input$effect_size
    
    # Check if we can use Colquhoun method
    use_colquhoun <- !is.na(n) && !is.na(d) && n >= 2 && d > 0
    
    if (use_colquhoun) {
      # Colquhoun method
      res <- interpret_pvalue_colquhoun(p, n, d, prior)
      method <- "Colquhoun (exact likelihood ratio)"
    } else {
      # Sellke-Berger method
      res <- interpret_pvalue(p, prior)
      method <- "Sellke-Berger (minimum Bayes factor)"
    }
    
    res$method_name <- method
    res
    
  })
  
  # --- Outputs ---
  
  output$posterior_text <- renderText({
    res <- results()
    paste0(round(res$posterior * 100, 1), "%")
  })
  
  output$fpr_text <- renderText({
    res <- results()
    paste0(round(res$fpr * 100, 1), "%")
  })
  
  output$bf_text <- renderText({
    res <- results()
    paste0(round(res$bayes_factor, 2), " : 1 in favor of effect")
  })
  
  output$method_text <- renderText({
    res <- results()
    paste0("Method: ", res$method_name)
  })
  
  output$interpretation_text <- renderText({
    res <- results()
    p <- res$p_value
    post <- res$posterior
    fpr <- res$fpr
    
    if (post >= 0.9) {
      strength <- "strong"
    } else if (post >= 0.75) {
      strength <- "moderate"
    } else if (post >= 0.5) {
      strength <- "weak"
    } else {
      strength <- "very weak"
    }
    
    paste0(
      "A p-value of ", p, " with your prior of ", round(res$prior * 100), "% ",
      "gives ", strength, " evidence for a real effect. ",
      "There is approximately a ", round(fpr * 100), "% chance ",
      "this is a false positive."
    )
  })
  
  # --- Sensitivity table ---
  output$sensitivity_table <- renderTable({
    
    req(input$p_value)
    
    p <- input$p_value
    priors <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    
    bf <- sellke_berger_bf(p)
    
    data.frame(
      Prior = paste0(priors * 100, "%"),
      Posterior = paste0(round(prior_to_posterior(priors, bf) * 100, 1), "%"),
      FPR = paste0(round((1 - prior_to_posterior(priors, bf)) * 100, 1), "%")
    )
    
  }, align = "c")
  
  # --- Nomogram ---
  output$nomogram <- renderPlot({
    res <- results()
    generate_nomogram(res$prior, res$bayes_factor, res$posterior)
  })
  
  # --- Icon array ---
  output$icon_array <- renderUI({
    res <- results()
    HTML(generate_icon_array(res$posterior))
  })
  
}

# ============================================================================
# RUN
# ============================================================================

shinyApp(ui = ui, server = server)