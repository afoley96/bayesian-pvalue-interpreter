# Bayesian P-value Interpreter
# Shiny Application

library(shiny)
library(ggplot2)

# Load core functions
source("R/sellke_berger.R")
source("R/posterior.R")
source("R/colquhoun.R")
source("R/visualizations.R")

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  
  # Include CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Title
  div(class = "title-panel",
      h2("Bayesian P-value Interpreter"),
      p("Transform p-values into probabilities you can actually interpret")
  ),
  
  # Sidebar layout
  sidebarLayout(
    
    # --- Input Panel ---
    sidebarPanel(
      class = "sidebar-panel",
      
      h4("Step 1: Enter P-value"),
      
      numericInput(
        inputId = "p_value",
        label = NULL,
        value = 0.05,
        min = 0.0001,
        max = 0.999,
        step = 0.001
      ),
      
      h4("Step 2: Set Prior Probability"),
      p(style = "font-size: 13px; color: #666;",
        "How plausible was this effect before the study?"),
      
      sliderInput(
        inputId = "prior",
        label = NULL,
        min = 0.01,
        max = 0.99,
        value = 0.5,
        step = 0.01
      ),
      
      # Quick-select buttons
      div(class = "prior-buttons",
          actionButton("prior_10", "10%", class = "btn-sm btn-default"),
          actionButton("prior_30", "30%", class = "btn-sm btn-default"),
          actionButton("prior_50", "50%", class = "btn-sm btn-default"),
          actionButton("prior_70", "70%", class = "btn-sm btn-default"),
          actionButton("prior_90", "90%", class = "btn-sm btn-default")
      ),
      
      hr(),
      
      h4("Optional: Study Details"),
      p(style = "font-size: 12px; color: #666;",
        "For more precise calculation (Colquhoun method)"),
      
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
      )
      
    ),
    
    # --- Main Panel ---
    mainPanel(
      class = "main-panel",
      
      # Results row
      fluidRow(
        column(6,
               div(class = "result-card positive",
                   h4("Posterior Probability"),
                   div(class = "big-number", textOutput("posterior_text", inline = TRUE)),
                   div(class = "subtitle", "chance effect is real")
               )
        ),
        column(6,
               div(class = "result-card negative",
                   h4("False Positive Risk"),
                   div(class = "big-number", textOutput("fpr_text", inline = TRUE)),
                   div(class = "subtitle", "chance this is a false positive")
               )
        )
      ),
      
      # Bayes factor
      fluidRow(
        column(12,
               div(class = "result-card neutral",
                   h4("Evidence Strength"),
                   div(style = "font-size: 24px;",
                       textOutput("bf_text", inline = TRUE)
                   ),
                   div(class = "method-badge",
                       textOutput("method_text", inline = TRUE)
                   )
               )
        )
      ),
      
      # Interpretation
      div(class = "interpretation-box",
          h4("What does this mean?"),
          textOutput("interpretation_text")
      ),
      
      # Nomogram
      h4(class = "section-header", "Nomogram: Prior → Evidence → Posterior"),
      div(class = "nomogram-container",
          plotOutput("nomogram", height = "350px")
      ),
      
      # Icon array
      h4(class = "section-header", "If 100 studies reported this p-value..."),
      div(class = "icon-array-container",
          htmlOutput("icon_array")
      ),
      
      # Sensitivity table
      h4(class = "section-header", "Sensitivity Analysis"),
      p(style = "color: #666;", "How would different prior beliefs change the conclusion?"),
      tableOutput("sensitivity_table"),
      
      # About section
      h4(class = "section-header", "About This Tool"),
      div(class = "result-card",
          h4("Methods"),
          p("This tool computes the posterior probability that an effect is real, 
                   given an observed p-value and your prior belief. It transforms the 
                   commonly misinterpreted p-value into a more intuitive probability."),
          
          p(strong("Sellke-Berger method:"), " Computes the minimum Bayes factor 
                   (maximum evidence) from a p-value alone, using the bound derived by 
                   Sellke, Bayarri & Berger (2001). This is conservative—actual evidence 
                   may be weaker."),
          
          p(strong("Colquhoun method:"), " When sample size and effect size are 
                   provided, computes the exact likelihood ratio using the approach 
                   described by Colquhoun (2017). This accounts for study-specific power."),
          
          h4("Key References"),
          tags$ul(
            tags$li("Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of p-values 
                            for testing precise null hypotheses. The American Statistician, 55(1), 62-71."),
            tags$li("Colquhoun, D. (2017). The reproducibility of research and the misinterpretation 
                            of p-values. Royal Society Open Science, 4(12), 171085."),
            tags$li("Colquhoun, D. (2019). The False Positive Risk: A proposal concerning what to 
                            do about p-values. The American Statistician, 73(sup1), 192-201."),
            tags$li("Held, L., & Ott, M. (2018). On p-values and Bayes factors. 
                            Annual Review of Statistics and Its Application, 5, 393-419.")
          ),
          
          p(style = "font-size: 13px; color: #666; margin-top: 15px;",
            "This tool applies the diagnostic test framework for interpreting p-values, 
                   as described in Newman & Kohn's 'Evidence-Based Diagnosis'.")
      ),
      
      # Feedback section
      h4(class = "section-header", "Feedback & Suggestions"),
      div(class = "result-card",
          p("We welcome your input on improving this tool."),
          
          h4("Questions to Consider:"),
          tags$ul(
            tags$li("Is the nomogram visualization intuitive?"),
            tags$li("Are the default prior options appropriate?"),
            tags$li("What additional features would be valuable?"),
            tags$li("How could we better explain the methodology?")
          ),
          
          h4("Notes / Comments:"),
          div(style = "background: #f5f5f5; border: 1px dashed #ccc; border-radius: 4px; 
                            padding: 20px; min-height: 100px; color: #999;",
              p("[Space for reviewer comments]")
          ),
          
          h4("Suggested Improvements:"),
          div(style = "background: #f5f5f5; border: 1px dashed #ccc; border-radius: 4px; 
                            padding: 20px; min-height: 100px; color: #999;",
              p("[Space for suggestions]")
          )
      ),
      
      # Footer
      div(class = "footer",
          p("Version 0.1 | April 2026"),
          p("Developed for educational and research purposes")
      )
      
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
      res <- interpret_pvalue_colquhoun(p, n, d, prior)
      method <- "Colquhoun method"
    } else {
      res <- interpret_pvalue(p, prior)
      method <- "Sellke-Berger method"
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
    bf <- res$bayes_factor
    if (bf >= 1) {
      paste0("Bayes Factor: ", round(bf, 2), " : 1 for effect")
    } else {
      paste0("Bayes Factor: 1 : ", round(1/bf, 2), " against effect")
    }
  })
  
  output$method_text <- renderText({
    res <- results()
    res$method_name
  })
  
  output$interpretation_text <- renderText({
    res <- results()
    post <- res$posterior
    fpr <- res$fpr
    p <- res$p_value
    prior_pct <- round(res$prior * 100)
    
    if (post >= 0.95) {
      strength <- "very strong"
      action <- "The evidence strongly supports a real effect."
    } else if (post >= 0.85) {
      strength <- "strong"
      action <- "The evidence supports a real effect, though some uncertainty remains."
    } else if (post >= 0.70) {
      strength <- "moderate"
      action <- "The evidence is suggestive but not conclusive. Consider replication."
    } else if (post >= 0.50) {
      strength <- "weak"
      action <- "The evidence is weak. Interpret with caution."
    } else {
      strength <- "very weak"
      action <- "The evidence does not support a real effect given your prior."
    }
    
    paste0(
      "With a prior of ", prior_pct, "% and p = ", p, ", the evidence is ",
      strength, ". ", action, " ",
      "False positive risk: ", round(fpr * 100), "%."
    )
  })
  
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
  
  # --- Sensitivity table ---
  output$sensitivity_table <- renderTable({
    
    req(input$p_value)
    
    p <- input$p_value
    priors <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    bf <- sellke_berger_bf(p)
    posteriors <- prior_to_posterior(priors, bf)
    
    data.frame(
      `Prior Belief` = paste0(priors * 100, "%"),
      `Posterior Probability` = paste0(round(posteriors * 100, 1), "%"),
      `False Positive Risk` = paste0(round((1 - posteriors) * 100, 1), "%"),
      check.names = FALSE
    )
    
  }, align = "c", striped = TRUE, hover = TRUE, bordered = TRUE)
  
}

# ============================================================================
# RUN
# ============================================================================

shinyApp(ui = ui, server = server)