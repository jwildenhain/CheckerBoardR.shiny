#
# Make3DPlotFunctions.R
# Modern, publication-quality visualization system using ggplot2 and plotly.
# Implements Nature, Science, The Economist, and Financial Times design presets.
#

library(ggplot2)
library(plotly)
library(RColorBrewer)
source("SynergyCalculations.R")

# Custom publication theme mappings
get_theme_palette <- function(theme_preset = "Nature", is_divergent = TRUE) {
  # Handle NULL or length-zero parameters safely
  if (is.null(theme_preset) || length(theme_preset) == 0 || theme_preset == "") {
    theme_preset <- "Nature"
  }
  # Returns: list(theme, colors)
  # Theme preset configuration
  if (theme_preset == "Nature") {
    # Nature: clean, monochrome-leaning, black and grey text, rose-red/royal-blue accents
    theme_obj <- theme_bw() + theme(
      text = element_text(family = "sans", color = "#111827"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 9),
      panel.grid.major = element_line(color = "#f3f4f6"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#e5e7eb", fill = NA, size = 1)
    )
    if (is_divergent) {
      colors <- c(low = "#2563eb", mid = "#ffffff", high = "#e11d48") # Blue -> White -> Rose
    } else {
      colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(128)
    }
  } else if (theme_preset == "Science") {
    # Science: pure white, clean serif/sans typography, high contrast, warm green/red accents
    theme_obj <- theme_classic() + theme(
      text = element_text(family = "sans", color = "#000000"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 9),
      axis.line = element_line(color = "#000000", size = 0.8)
    )
    if (is_divergent) {
      colors <- c(low = "#059669", mid = "#ffffff", high = "#dc2626") # Green -> White -> Red
    } else {
      colors <- colorRampPalette(brewer.pal(9, "Viridis"))(128)
    }
  } else if (theme_preset == "The Economist") {
    # The Economist: light-blue background, bold sans-serif, white gridlines, distinctive red banner accent
    theme_obj <- theme_minimal() + theme(
      text = element_text(family = "sans", color = "#0f172a"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#075985"),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 9),
      panel.background = element_rect(fill = "#e4eef2", color = NA),
      plot.background = element_rect(fill = "#e4eef2", color = NA),
      panel.grid.major = element_line(color = "#ffffff", size = 1.2),
      panel.grid.minor = element_blank()
    )
    if (is_divergent) {
      colors <- c(low = "#0369a1", mid = "#ffffff", high = "#be123c") # Blue -> White -> Red
    } else {
      colors <- colorRampPalette(c("#e4eef2", "#0369a1", "#0284c7"))(128)
    }
  } else if (theme_preset == "Financial Times") {
    # Financial Times: signature warm salmon paper background, dark blue/red palette
    theme_obj <- theme_minimal() + theme(
      text = element_text(family = "sans", color = "#262626"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#1a1a1a"),
      axis.title = element_text(size = 11, face = "bold", color = "#333333"),
      axis.text = element_text(size = 9, color = "#404040"),
      panel.background = element_rect(fill = "#fff1e5", color = NA),
      plot.background = element_rect(fill = "#fff1e5", color = NA),
      panel.grid.major = element_line(color = "#e6d9ce", size = 0.8),
      panel.grid.minor = element_blank()
    )
    if (is_divergent) {
      colors <- c(low = "#0f172a", mid = "#ffffff", high = "#991b1b") # Dark Slate -> White -> Crimson
    } else {
      colors <- colorRampPalette(c("#fff1e5", "#991b1b"))(128)
    }
  }
  
  return(list(theme = theme_obj, colors = colors))
}

# Redesigned 2D Heatmap via ggplot2
ggplot_synergy_heatmap <- function(data_list, model_name = "Bliss", orientation = "synergism", theme_preset = "Nature", title = "") {
  # Extract values
  if (model_name == "Data") {
    mat <- data_list$raw_inhibition
    cTitle <- "Inhibition"
    is_div <- FALSE
  } else {
    mat <- data_list[[model_name]]$scores
    cTitle <- paste(model_name, "Score")
    is_div <- TRUE
  }
  
  # Melt the matrix for ggplot
  df <- as.data.frame(mat)
  df$DrugB <- rownames(df)
  df_long <- reshape(df, direction = "long", varying = list(1:(ncol(df)-1)), 
                     v.names = "Score", timevar = "DrugA", 
                     times = colnames(df)[1:(ncol(df)-1)], idvar = "DrugB")
  
  # Ensure factors/numeric sorting
  df_long$DrugA_num <- extract_concentration(df_long$DrugA)
  df_long$DrugB_num <- extract_concentration(df_long$DrugB)
  
  df_long$DrugA <- factor(df_long$DrugA, levels = unique(colnames(mat)))
  df_long$DrugB <- factor(df_long$DrugB, levels = unique(rownames(mat)))
  
  # Safety guards for parameters
  if (is.null(title) || length(title) == 0 || title == "") {
    title_text <- paste(model_name, if (model_name == "Data") "Inhibition Map" else "Synergy Landscape")
  } else {
    title_text <- title
  }
  
  if (is.null(theme_preset) || length(theme_preset) == 0 || theme_preset == "") {
    theme_preset <- "Nature"
  }
  
  if (is.null(orientation) || length(orientation) == 0 || orientation == "") {
    orientation <- "synergism"
  }
  
  theme_cfg <- get_theme_palette(theme_preset, is_divergent = is_div)
  
  # Construct plot
  p <- ggplot(df_long, aes(x = DrugA, y = DrugB, fill = Score)) +
    geom_tile(color = "#ffffff", size = 0.5) +
    geom_text(aes(label = sprintf("%.2f", Score)), color = "#1e293b", size = 3, fontface = "bold") +
    labs(
      title = title_text,
      x = "Drug A Concentration",
      y = "Drug B Concentration",
      fill = cTitle
    ) +
    theme_cfg$theme
  
  # Apply color scale
  if (is_div) {
    # If looking at Antagonism, we reverse the color scaling direction
    if (orientation == "antagonism") {
      p <- p + scale_fill_gradient2(
        low = theme_cfg$colors["high"], 
        mid = theme_cfg$colors["mid"], 
        high = theme_cfg$colors["low"], 
        midpoint = 0
      )
    } else {
      p <- p + scale_fill_gradient2(
        low = theme_cfg$colors["low"], 
        mid = theme_cfg$colors["mid"], 
        high = theme_cfg$colors["high"], 
        midpoint = 0
      )
    }
  } else {
    p <- p + scale_fill_gradientn(colors = theme_cfg$colors)
  }
  
  return(p)
}

# Redesigned 1D Single-Agent Fit curves side-by-side
ggplot_single_agent_fits <- function(data_list, theme_preset = "Nature") {
  conc_A <- data_list$conc_A
  resp_A <- data_list$raw_inhibition[1, ]
  par_A <- data_list$single_fit_A
  
  conc_B <- data_list$conc_B
  resp_B <- data_list$raw_inhibition[, 1]
  par_B <- data_list$single_fit_B
  
  # Grid sequence for smooth fitted line plotting
  grid_A <- seq(0, max(conc_A), length.out = 100)
  grid_B <- seq(0, max(conc_B), length.out = 100)
  
  fit_y_A <- if (!is.null(par_A)) predict_4pl(grid_A, par_A) else approx(x = conc_A, y = resp_A, xout = grid_A, rule = 2)$y
  fit_y_B <- if (!is.null(par_B)) predict_4pl(grid_B, par_B) else approx(x = conc_B, y = resp_B, xout = grid_B, rule = 2)$y
  
  df_pts_A <- data.frame(Concentration = conc_A, Inhibition = resp_A, Agent = "Drug A")
  df_fit_A <- data.frame(Concentration = grid_A, Inhibition = fit_y_A, Agent = "Drug A")
  
  df_pts_B <- data.frame(Concentration = conc_B, Inhibition = resp_B, Agent = "Drug B")
  df_fit_B <- data.frame(Concentration = grid_B, Inhibition = fit_y_B, Agent = "Drug B")
  
  df_pts <- rbind(df_pts_A, df_pts_B)
  df_fit <- rbind(df_fit_A, df_fit_B)
  
  theme_cfg <- get_theme_palette(theme_preset, is_divergent = FALSE)
  
  p <- ggplot() +
    geom_line(data = df_fit, aes(x = Concentration, y = Inhibition, color = Agent), size = 1.2) +
    geom_point(data = df_pts, aes(x = Concentration, y = Inhibition, fill = Agent), shape = 21, size = 3, color = "#000000", stroke = 0.8) +
    facet_wrap(~Agent, scales = "free_x") +
    labs(
      title = "Single-Agent Dose-Response Fitted Curves",
      x = "Concentration",
      y = "Inhibition"
    ) +
    scale_color_manual(values = c("Drug A" = "#2563eb", "Drug B" = "#dc2626")) +
    scale_fill_manual(values = c("Drug A" = "#3b82f6", "Drug B" = "#ef4444")) +
    theme_cfg$theme +
    theme(legend.position = "none")
  
  return(p)
}

# Redesigned 3D surface plotting via Plotly
plotly_synergy_surface <- function(data_list, model_name = "Bliss", theme_preset = "Nature") {
  # Extract values
  if (model_name == "Data") {
    mat <- data_list$raw_inhibition
    cTitle <- "Inhibition"
  } else {
    mat <- data_list[[model_name]]$scores
    cTitle <- paste(model_name, "Score")
  }
  
  # Clean names to extract exact numeric dose levels
  conc_A <- data_list$conc_A
  conc_B <- data_list$conc_B
  
  # Style configuration
  colorscale_choice <- "RdBu"
  bg_color <- "#ffffff"
  text_color <- "#111827"
  grid_color <- "#f3f4f6"
  
  if (theme_preset == "The Economist") {
    bg_color <- "#e4eef2"
    grid_color <- "#ffffff"
    colorscale_choice <- "Viridis"
  } else if (theme_preset == "Financial Times") {
    bg_color <- "#fff1e5"
    grid_color <- "#e6d9ce"
    colorscale_choice <- "Portland"
  }
  
  p <- plot_ly(
    x = ~conc_A, 
    y = ~conc_B, 
    z = ~mat, 
    type = "surface", 
    colorscale = colorscale_choice
  ) %>%
    layout(
      title = list(
        text = paste("3D Synergy Surface -", model_name),
        font = list(family = "sans", size = 16, color = text_color, weight = "bold")
      ),
      scene = list(
        xaxis = list(title = "Drug A Concentration", gridcolor = grid_color, backgroundcolor = bg_color, showbackground = TRUE),
        yaxis = list(title = "Drug B Concentration", gridcolor = grid_color, backgroundcolor = bg_color, showbackground = TRUE),
        zaxis = list(title = cTitle, gridcolor = grid_color, backgroundcolor = bg_color, showbackground = TRUE)
      ),
      paper_bgcolor = bg_color,
      plot_bgcolor = bg_color
    )
  
  return(p)
}

# Keep legacy Base R compatibility signatures so that downloads and simple modes don't crash
myImagePlotReverse <- function(x, ...) {
  # Draw a high-quality base image plot fallback
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  
  layout(matrix(c(1, 2), 2, 1), heights = c(4, 1))
  ColorRamp <- colorRampPalette(brewer.pal(9, "YlOrRd"))(128)
  
  # Reverse Y axis
  reverse <- nrow(x):1
  yLabels <- yLabels[reverse]
  x <- x[reverse, ]
  
  par(mar = c(4, 5, 3, 2))
  image(1:ncol(x), 1:nrow(x), t(x), col = ColorRamp, axes = FALSE, xlab = "", ylab = "")
  axis(1, at = 1:ncol(x), labels = xLabels)
  axis(2, at = 1:nrow(x), labels = yLabels, las = 1)
  
  ColorLevels <- seq(min_val, max_val, length.out = length(ColorRamp))
  par(mar = c(3, 5, 1, 2))
  image(ColorLevels, 1, matrix(ColorLevels, length(ColorLevels), 1), col = ColorRamp, yaxt = "n", xlab = "Synergy Intensity")
  
  layout(1)
  return(max_val)
}

raw_plot <- function(xx, ...) {
  # Standard beautiful 3D persp plot fallback
  z <- as.matrix(xx)
  class(z) <- "numeric"
  
  x <- 1:nrow(z)
  y <- 1:ncol(z)
  
  jet.colors <- colorRampPalette(c("midnightblue", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
  nbcol <- 64
  color <- jet.colors(nbcol)
  
  zfacet <- z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]
  facetcol <- cut(zfacet, nbcol)
  
  persp(x, y, z, col = color[facetcol], phi = 30, theta = -60,
        ticktype = "detailed", d = 5, r = 1, shade = 0.1, expand = 0.6,
        xlab = "Drug A", ylab = "Drug B", zlab = "Inhibition")
}
