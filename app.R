# app.R
library(shiny)
library(shinythemes)

# --------------------------
# Helper: simulate S(t)
# --------------------------
simulate_S <- function(t, lambda, mu, n_sim = 5000) {
  # N(t) ~ Poisson(lambda * t)
  N <- rpois(n_sim, lambda * t)
  S <- numeric(n_sim)
  
  for (i in seq_len(n_sim)) {
    if (N[i] > 0) {
      S[i] <- sum(rexp(N[i], rate = mu))
    } else {
      S[i] <- 0
    }
  }
  S
}

# --------------------------
# Helper: simulate one sample path S(t) over [0, t_max]
# --------------------------
simulate_path <- function(t_max, lambda, mu) {
  # simulate jump times for Poisson process
  times <- c()
  t <- 0
  while (TRUE) {
    t <- t + rexp(1, rate = lambda)
    if (t > t_max) break
    times <- c(times, t)
  }
  
  n_jumps <- length(times)
  
  if (n_jumps == 0) {
    # no jumps: S(t) always 0
    return(data.frame(time = c(0, t_max),
                      S = c(0, 0)))
  }
  
  jumps <- rexp(n_jumps, rate = mu)
  S_vals <- cumsum(jumps)
  
  df <- data.frame(time = times, S = S_vals)
  df <- rbind(data.frame(time = 0, S = 0), df)
  df <- rbind(df, data.frame(time = t_max, S = S_vals[n_jumps]))
  df[order(df$time), ]
}

# --------------------------
# UI
# --------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      body {
        background: #f3f6fa;
      }
      .well {
        background-color: #ffffff !important;
        border-radius: 10px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }
      .panel {
        background-color: #ffffff !important;
        border-radius: 10px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }
      .tabbable > .nav-tabs > li > a {
        font-weight: 500;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background-color: #2c3e50 !important;
        color: #ffffff !important;
      }
      .shiny-plot-output {
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 5px;
        background-color: #ffffff;
      }
      #go {
        width: 100%;
      }
    "))
  ),
  
  titlePanel("Compound Poisson Process S(t) with Exponential Jumps"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parameters"),
      numericInput("lambda", "Arrival rate λ:", 
                   value = 1, min = 0.1, step = 0.1),
      numericInput("mu", "Jump rate μ (X ~ Exp(μ)):", 
                   value = 1, min = 0.1, step = 0.1),
      numericInput("nsim", "Number of simulations:", 
                   value = 5000, min = 1000, step = 500),
      
      hr(),
      h4("Custom time t for S(t)"),
      numericInput("t_custom", "Custom time t:", 
                   value = 100, min = 1, step = 1),
      
      hr(),
      h4("Sample path settings"),
      numericInput("tmax", "Max time for sample path:", 
                   value = 1000, min = 10, step = 10),
      
      hr(),
      actionButton("go", "Run Simulation", class = "btn btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histograms at fixed times",
                 br(),
                 p("Histograms of S(t) at fixed times t = 10, 100, 1000, 10000."),
                 fluidRow(
                   column(6, plotOutput("hist10")),
                   column(6, plotOutput("hist100"))
                 ),
                 br(),
                 fluidRow(
                   column(6, plotOutput("hist1000")),
                   column(6, plotOutput("hist10000"))
                 )
        ),
        
        tabPanel("Custom time histogram",
                 br(),
                 p("Use this tab to study S(t) at an arbitrary time t."),
                 plotOutput("histCustom"),
                 br(),
                 verbatimTextOutput("summaryCustom")
        ),
        
        tabPanel("Sample path S(t)",
                 br(),
                 plotOutput("pathPlot"),
                 br(),
                 verbatimTextOutput("pathInfo")
        ),
        
        tabPanel("Theory & Interpretation",
                 br(),
                 h4("Distribution of S(t)"),
                 p("We consider a compound Poisson process"),
                 tags$pre("
S(t) = sum_{i=1}^{N(t)} X_i,   t ≥ 0
"),
                 p("where N(t) is a Poisson process with rate λ (exponential inter-arrival times), 
                   and X_i are i.i.d. Exp(μ) jumps, independent of N(t)."),
                 p("Conditional on N(t) = n, we have S(t) | N(t) = n ~ Gamma(n, μ). 
                   Unconditionally, S(t) has a point mass at zero, P(S(t) = 0) = exp(-λ t),
                   plus a continuous density on (0, ∞) given by a mixture of Gamma distributions."),
                 
                 h4("Moments"),
                 tags$pre("
E[S(t)]   = λ t / μ
Var[S(t)] = 2 λ t / μ^2
"),
                 
                 h4("Parameter sensitivity"),
                 tags$ul(
                   tags$li("Increasing λ (arrival rate): more jumps per unit time.
                           E[S(t)] and Var[S(t)] both increase linearly with λ.
                           Histograms shift to the right and spread out."),
                   tags$li("Increasing μ (jump rate): makes each jump smaller on average (mean jump size = 1/μ).
                           Both E[S(t)] and Var[S(t)] decrease as μ increases.
                           Histograms become more concentrated near smaller values."),
                   tags$li("Increasing t: both mean and variance grow linearly with t, and for large t, 
                           by a CLT argument, the distribution of S(t) becomes approximately normal.")
                 )
        )
      )
    )
  )
)

# --------------------------
# Server
# --------------------------
server <- function(input, output, session) {
  
  # Reactive simulation for all relevant times
  sim_all <- eventReactive(input$go, {
    lambda <- input$lambda
    mu     <- input$mu
    n_sim  <- input$nsim
    
    list(
      S10     = simulate_S(10, lambda, mu, n_sim),
      S100    = simulate_S(100, lambda, mu, n_sim),
      S1000   = simulate_S(1000, lambda, mu, n_sim),
      S10000  = simulate_S(10000, lambda, mu, n_sim),
      Scustom = simulate_S(input$t_custom, lambda, mu, n_sim)
    )
  })
  
  # --------------------------
  # Histograms at fixed times
  # --------------------------
  output$hist10 <- renderPlot({
    sims <- sim_all()
    S <- sims$S10
    lambda <- input$lambda
    mu <- input$mu
    
    hist(S, breaks = 40, probability = TRUE,
         main = "Histogram of S(t) at t = 10",
         xlab = "S(10)",
         col  = "#FAD4D8",      # light red
         border = "#F28B82")    # slightly darker red border
    abline(v = lambda * 10 / mu, lwd = 2, col = "#E53935")
  })
  
  output$hist100 <- renderPlot({
    sims <- sim_all()
    S <- sims$S100
    lambda <- input$lambda
    mu <- input$mu
    
    hist(S, breaks = 40, probability = TRUE,
         main = "Histogram of S(t) at t = 100",
         xlab = "S(100)",
         col  = "#FAD4D8",
         border = "#F28B82")
    abline(v = lambda * 100 / mu, lwd = 2, col = "#E53935")
  })
  
  output$hist1000 <- renderPlot({
    sims <- sim_all()
    S <- sims$S1000
    lambda <- input$lambda
    mu <- input$mu
    
    hist(S, breaks = 40, probability = TRUE,
         main = "Histogram of S(t) at t = 1000",
         xlab = "S(1000)",
         col  = "#FAD4D8",
         border = "#F28B82")
    abline(v = lambda * 1000 / mu, lwd = 2, col = "#E53935")
  })
  
  output$hist10000 <- renderPlot({
    sims <- sim_all()
    S <- sims$S10000
    lambda <- input$lambda
    mu <- input$mu
    
    hist(S, breaks = 40, probability = TRUE,
         main = "Histogram of S(t) at t = 10000",
         xlab = "S(10000)",
         col  = "#FAD4D8",
         border = "#F28B82")
    abline(v = lambda * 10000 / mu, lwd = 2, col = "#E53935")
  })
  
  # --------------------------
  # Custom time histogram
  # --------------------------
  output$histCustom <- renderPlot({
    sims <- sim_all()
    S <- sims$Scustom
    lambda <- input$lambda
    mu <- input$mu
    t  <- input$t_custom
    
    hist(S, breaks = 40, probability = TRUE,
         main = paste("Histogram of S(t) at t =", t),
         xlab = paste0("S(", t, ")"),
         col  = "#FAD4D8",
         border = "#F28B82")
    abline(v = lambda * t / mu, lwd = 2, col = "#E53935")
  })
  
  output$summaryCustom <- renderPrint({
    sims <- sim_all()
    S <- sims$Scustom
    lambda <- input$lambda
    mu <- input$mu
    t  <- input$t_custom
    
    cat("Empirical summary of S(t) at t =", t, ":\n")
    print(summary(S))
    
    cat("\nEmpirical mean  :", mean(S), "\n")
    cat("Empirical var   :", var(S), "\n")
    
    cat("\nTheoretical mean E[S(t)]   =", lambda * t / mu, "\n")
    cat("Theoretical var  Var[S(t)] =", 2 * lambda * t / (mu^2), "\n")
  })
  
  # --------------------------
  # Sample path
  # --------------------------
  path_data <- eventReactive(input$go, {
    simulate_path(input$tmax, input$lambda, input$mu)
  })
  
  output$pathPlot <- renderPlot({
    df <- path_data()
    plot(df$time, df$S, type = "s",
         main = paste0("Sample Path of S(t) up to t = ", input$tmax),
         xlab = "Time t", ylab = "S(t)",
         col  = "#F28B82",  # light-ish red line
         lwd  = 2)
  })
  
  output$pathInfo <- renderPrint({
    lambda <- input$lambda
    mu     <- input$mu
    t_max  <- input$tmax
    
    cat("Sample path information:\n")
    cat("Parameters: λ =", lambda, ", μ =", mu, ", t_max =", t_max, "\n")
    cat("Theoretical E[S(t_max)]   =", lambda * t_max / mu, "\n")
    cat("Theoretical Var[S(t_max)] =", 2 * lambda * t_max / mu^2, "\n")
  })
}

# --------------------------
# Run app
# --------------------------
shinyApp(ui = ui, server = server)
