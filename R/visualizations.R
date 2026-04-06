#' Generate Icon Array HTML
#'
#' Creates an HTML representation of false positive risk using colored dots.
#'
#' @param posterior Posterior probability (0 to 1)
#' @param n_icons Total number of icons (default 100)
#'
#' @return HTML string for rendering
#'
#' @export
generate_icon_array <- function(posterior, n_icons = 100) {
  
  n_positive <- round(posterior * n_icons)
  n_negative <- n_icons - n_positive
  
  # Create icon strings
  positive_icon <- "<span style='color: #2E7D32; font-size: 16px;'>●</span>"
  negative_icon <- "<span style='color: #C62828; font-size: 16px;'>●</span>"
  
  # Build array (10 per row)
  icons <- c(rep(positive_icon, n_positive), rep(negative_icon, n_negative))
  
  # Arrange in rows of 10
  rows <- split(icons, ceiling(seq_along(icons) / 10))
  row_html <- sapply(rows, function(row) {
    paste0("<div style='line-height: 1.4;'>", paste(row, collapse = " "), "</div>")
  })
  
  html <- paste0(
    "<div style='font-family: monospace;'>",
    paste(row_html, collapse = "\n"),
    "</div>",
    "<div style='margin-top: 10px; font-size: 14px;'>",
    "<span style='color: #2E7D32;'>●</span> ", n_positive, " likely real effects &nbsp;&nbsp;",
    "<span style='color: #C62828;'>●</span> ", n_negative, " likely false positives",
    "</div>"
  )
  
  return(html)
}

#' Generate Fagan-style Nomogram
#'
#' Creates a nomogram showing prior -> Bayes factor -> posterior
#'
#' @param prior Prior probability
#' @param bf Bayes factor
#' @param posterior Posterior probability
#'
#' @return A ggplot object
#'
#' @export
generate_nomogram <- function(prior, bf, posterior) {
  
  library(ggplot2)
  
  # Convert probabilities to log-odds for linear scale
  prob_to_logodds <- function(p) {
    log(p / (1 - p))
  }
  
  logodds_to_prob <- function(lo) {
    exp(lo) / (1 + exp(lo))
  }
  
  # Axis positions
  x_prior <- 0
  x_bf <- 0.5
  x_posterior <- 1
  
  # Y positions (log-odds scale)
  y_prior <- prob_to_logodds(prior)
  y_bf <- log(bf) / 2  # Half because it spans the gap
  y_posterior <- prob_to_logodds(posterior)
  
  # Create probability labels for axes
  prob_labels <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 0.99)
  prob_y <- prob_to_logodds(prob_labels)
  
  # BF labels
  bf_labels <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50)
  bf_y <- log(bf_labels) / 2
  
  # Data for axes
  axis_data <- data.frame(
    x = c(rep(x_prior, length(prob_labels)),
          rep(x_bf, length(bf_labels)),
          rep(x_posterior, length(prob_labels))),
    y = c(prob_y, bf_y, prob_y),
    label = c(paste0(prob_labels * 100, "%"),
              as.character(bf_labels),
              paste0(prob_labels * 100, "%"))
  )
  
  # Data for the connecting line
  line_data <- data.frame(
    x = c(x_prior, x_bf, x_posterior),
    y = c(y_prior, y_bf, y_posterior)
  )
  
  # Data for the current point markers
  point_data <- data.frame(
    x = c(x_prior, x_bf, x_posterior),
    y = c(y_prior, y_bf, y_posterior),
    label = c(paste0(round(prior * 100), "%"),
              round(bf, 2),
              paste0(round(posterior * 100), "%"))
  )
  
  # Build plot
  p <- ggplot() +
    
    # Axis lines
    geom_segment(aes(x = 0, xend = 0, y = -4.5, yend = 4.5), color = "gray70", linewidth = 0.5) +
    geom_segment(aes(x = 0.5, xend = 0.5, y = -2.5, yend = 2.5), color = "gray70", linewidth = 0.5) +
    geom_segment(aes(x = 1, xend = 1, y = -4.5, yend = 4.5), color = "gray70", linewidth = 0.5) +
    
    # Tick marks and labels - Prior
    geom_segment(data = data.frame(y = prob_y),
                 aes(x = -0.02, xend = 0.02, y = y, yend = y), color = "gray50") +
    geom_text(data = data.frame(y = prob_y, label = paste0(prob_labels * 100, "%")),
              aes(x = -0.08, y = y, label = label), size = 3, hjust = 1) +
    
    # Tick marks and labels - BF
    geom_segment(data = data.frame(y = bf_y),
                 aes(x = 0.48, xend = 0.52, y = y, yend = y), color = "gray50") +
    geom_text(data = data.frame(y = bf_y, label = bf_labels),
              aes(x = 0.56, y = y, label = label), size = 3, hjust = 0) +
    
    # Tick marks and labels - Posterior
    geom_segment(data = data.frame(y = prob_y),
                 aes(x = 0.98, xend = 1.02, y = y, yend = y), color = "gray50") +
    geom_text(data = data.frame(y = prob_y, label = paste0(prob_labels * 100, "%")),
              aes(x = 1.08, y = y, label = label), size = 3, hjust = 0) +
    
    # Axis titles
    annotate("text", x = 0, y = 5, label = "Prior", fontface = "bold", size = 4) +
    annotate("text", x = 0.5, y = 3, label = "Bayes\nFactor", fontface = "bold", size = 4) +
    annotate("text", x = 1, y = 5, label = "Posterior", fontface = "bold", size = 4) +
    
    # The connecting line
    geom_line(data = line_data, aes(x = x, y = y),
              color = "#1976D2", linewidth = 1.5) +
    
    # Points at each axis
    geom_point(data = point_data, aes(x = x, y = y),
               color = "#1976D2", size = 4) +
    
    # Value labels on points
    geom_label(data = point_data[1, ], aes(x = x - 0.12, y = y, label = label),
               fill = "#1976D2", color = "white", size = 3.5, fontface = "bold") +
    geom_label(data = point_data[2, ], aes(x = x, y = y + 0.4, label = label),
               fill = "#1976D2", color = "white", size = 3.5, fontface = "bold") +
    geom_label(data = point_data[3, ], aes(x = x + 0.12, y = y, label = label),
               fill = "#1976D2", color = "white", size = 3.5, fontface = "bold") +
    
    # Styling
    theme_void() +
    theme(
      plot.margin = margin(20, 40, 20, 40)
    ) +
    coord_cartesian(xlim = c(-0.25, 1.25), ylim = c(-5, 5.5))
  
  return(p)
}