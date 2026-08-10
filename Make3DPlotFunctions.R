#
# Make3DPlotFunctions.R
# Modern, publication-quality visualization system using ggplot2 and plotly.
# Implements Nature, Science, The Economist, and Financial Times design presets.
#

library(ggplot2)
library(plotly)
library(RColorBrewer)
# Helper to reactively flip x, y, and z dimensions of matrices for custom views
apply_plot_flips <- function(mat, conc_A, conc_B, flip_x = FALSE, flip_y = FALSE, flip_z = FALSE) {
  if (flip_x) {
    # Reverse columns
    mat <- mat[, ncol(mat):1, drop = FALSE]
    conc_A <- rev(conc_A)
  }
  if (flip_y) {
    # Reverse rows
    mat <- mat[nrow(mat):1, , drop = FALSE]
    conc_B <- rev(conc_B)
  }
  if (flip_z) {
    # Negate scores
    mat <- mat * -1
  }
  return(list(mat = mat, conc_A = conc_A, conc_B = conc_B))
}

# Custom publication theme mappings
get_theme_palette <- function(theme_preset = "Nature", is_divergent = TRUE) {
  # Handle NULL or length-zero parameters safely
  if (is.null(theme_preset) || length(theme_preset) == 0 || theme_preset == "") {
    theme_preset <- "Nature"
  }
  
  # Determine distinctive typography family for each style guide
  font_family <- "sans"
  if (theme_preset == "Nature") {
    font_family <- "Arial"
  } else if (theme_preset == "Science") {
    font_family <- "Helvetica"
  } else if (theme_preset == "The Economist") {
    font_family <- "Trebuchet MS"
  } else if (theme_preset == "Financial Times") {
    font_family <- "Georgia"
  }
  
  # Returns: list(theme, colors, family)
  # Theme preset configuration
  if (theme_preset == "Nature") {
    # Nature: clean, monochrome-leaning, black and grey text, rose-red/royal-blue accents
    theme_obj <- theme_bw() + theme(
      text = element_text(family = font_family, color = "#111827"),
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
      text = element_text(family = font_family, color = "#000000"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 9),
      axis.line = element_line(color = "#000000", size = 0.8)
    )
    if (is_divergent) {
      colors <- c(low = "#059669", mid = "#ffffff", high = "#dc2626") # Green -> White -> Red
    } else {
      colors <- colorRampPalette(c("#440154", "#3b528b", "#21908c", "#5dc963", "#fde725"))(128)
    }
  } else if (theme_preset == "The Economist") {
    # The Economist: light-blue background, bold sans-serif, white gridlines, distinctive red banner accent
    theme_obj <- theme_minimal() + theme(
      text = element_text(family = font_family, color = "#0f172a"),
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
      text = element_text(family = font_family, color = "#262626"),
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
  
  return(list(theme = theme_obj, colors = colors, family = font_family))
}

ggplot_synergy_heatmap <- function(data_list, model_name = "Bliss", orientation = "synergism", theme_preset = "Nature", title = "", flip_x = FALSE, flip_y = FALSE, flip_z = FALSE) {
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
  
  conc_A <- data_list$conc_A
  conc_B <- data_list$conc_B
  
  # Apply reactive flips
  flipped <- apply_plot_flips(mat, conc_A, conc_B, flip_x, flip_y, flip_z)
  mat <- flipped$mat
  
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
  
  # Formatting parameters for Z axis/fill scale if flipped
  scale_formatter <- if (flip_z) abs else identity
  fill_label <- if (flip_z) paste(cTitle, "(Inverted)") else cTitle

  # Construct plot
  p <- ggplot(df_long, aes(x = DrugA, y = DrugB, fill = Score)) +
    geom_tile(color = "#ffffff", size = 0.5) +
    geom_text(aes(label = sprintf("%.2f", ifelse(flip_z, abs(Score), Score))), color = "#1e293b", size = 3, fontface = "bold") +
    labs(
      title = title_text,
      x = "Drug A Concentration",
      y = "Drug B Concentration",
      fill = fill_label
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
        midpoint = 0,
        labels = scale_formatter
      )
    } else {
      p <- p + scale_fill_gradient2(
        low = theme_cfg$colors["low"], 
        mid = theme_cfg$colors["mid"], 
        high = theme_cfg$colors["high"], 
        midpoint = 0,
        labels = scale_formatter
      )
    }
  } else {
    p <- p + scale_fill_gradientn(colors = theme_cfg$colors, labels = scale_formatter)
  }
  
  return(p)
}

# Redesigned 1D Single-Agent Fit curves side-by-side
ggplot_single_agent_fits <- function(data_list, theme_preset = "Nature") {
  conc_A <- data_list$conc_A
  resp_A <- data_list$raw_inhibition[data_list$zero_row, ]
  par_A <- data_list$single_fit_A
  
  conc_B <- data_list$conc_B
  resp_B <- data_list$raw_inhibition[, data_list$zero_col]
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

plotly_synergy_surface <- function(data_list, model_name = "Bliss", theme_preset = "Nature", camera_theta = 45, camera_phi = 30, camera_zoom = 1.8, flip_x = FALSE, flip_y = FALSE, flip_z = FALSE) {
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
  
  conc_A <- data_list$conc_A
  conc_B <- data_list$conc_B
  
  # Apply reactive flips
  flipped <- apply_plot_flips(mat, conc_A, conc_B, flip_x, flip_y, flip_z)
  mat <- flipped$mat
  conc_A <- flipped$conc_A
  conc_B <- flipped$conc_B
  
  # Pull the exact same theme preset configuration as 2D/1D ggplot
  theme_cfg <- get_theme_palette(theme_preset, is_divergent = is_div)
  
  # Set background and text styling
  bg_color <- "#ffffff"
  text_color <- "#111827"
  grid_color <- "#f3f4f6"
  
  if (theme_preset == "The Economist") {
    bg_color <- "#e4eef2"
    grid_color <- "#ffffff"
    text_color <- "#0f172a"
  } else if (theme_preset == "Financial Times") {
    bg_color <- "#fff1e5"
    grid_color <- "#e6d9ce"
    text_color <- "#262626"
  }
  
  # Build Plotly custom colorscale from the active publication theme palette
  if (is_div) {
    colorscale_choice <- list(
      list(0.0, theme_cfg$colors["low"]),
      list(0.5, theme_cfg$colors["mid"]),
      list(1.0, theme_cfg$colors["high"])
    )
  } else {
    # Non-divergent scale
    colorscale_choice <- list(
      list(0.0, theme_cfg$colors[1]),
      list(0.25, theme_cfg$colors[32]),
      list(0.5, theme_cfg$colors[64]),
      list(0.75, theme_cfg$colors[96]),
      list(1.0, theme_cfg$colors[128])
    )
  }
  
  # Generate absolute hover labels to keep numbers positive even when physically inverted
  hover_text <- matrix("", nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      orig_val <- mat[i, j]
      if (flip_z) orig_val <- -orig_val # Reverse negation back to positive for labeling
      hover_text[i, j] <- sprintf(
        "Drug A: %s<br>Drug B: %s<br>%s: %.2f",
        colnames(mat)[j], rownames(mat)[i], cTitle, orig_val
      )
    }
  }
  
  # Convert azimuth (theta) and elevation (phi) to spherical Cartesian eye coordinates for Plotly
  theta_rad <- camera_theta * pi / 180
  phi_rad <- camera_phi * pi / 180
  
  eye_x <- camera_zoom * cos(phi_rad) * sin(theta_rad)
  eye_y <- camera_zoom * cos(phi_rad) * cos(theta_rad)
  eye_z <- camera_zoom * sin(phi_rad)
  
  # Dynamically construct z-axis configuration to display positive labels on invert
  zaxis_config <- list(title = cTitle, gridcolor = grid_color, backgroundcolor = bg_color, showbackground = TRUE)
  if (flip_z) {
    z_min <- min(mat, na.rm = TRUE)
    z_max <- max(mat, na.rm = TRUE)
    ticks <- pretty(c(z_min, z_max), n = 5)
    ticks <- ticks[ticks >= z_min & ticks <= z_max]
    if (length(ticks) > 0) {
      zaxis_config$tickmode <- "array"
      zaxis_config$tickvals <- ticks
      zaxis_config$ticktext <- as.character(abs(ticks))
    }
    zaxis_config$title <- paste(cTitle, "(Inverted - Positive Effect)")
  }

  p <- plot_ly(
    x = 1:ncol(mat), 
    y = 1:nrow(mat), 
    z = ~mat, 
    type = "surface", 
    colorscale = colorscale_choice,
    text = hover_text,
    hoverinfo = "text"
  ) %>%
    layout(
      title = list(
        text = paste("3D Synergy Surface -", model_name),
        font = list(family = "sans", size = 16, color = text_color, weight = "bold")
      ),
      scene = list(
        camera = list(eye = list(x = eye_x, y = eye_y, z = eye_z)),
        xaxis = list(
          title = "Drug A Concentration", 
          gridcolor = grid_color, 
          backgroundcolor = bg_color, 
          showbackground = TRUE,
          tickmode = "array",
          tickvals = 1:ncol(mat),
          ticktext = colnames(mat)
        ),
        yaxis = list(
          title = "Drug B Concentration", 
          gridcolor = grid_color, 
          backgroundcolor = bg_color, 
          showbackground = TRUE,
          tickmode = "array",
          tickvals = 1:nrow(mat),
          ticktext = rownames(mat)
        ),
        zaxis = zaxis_config
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

raw_plot <- function(data_list, model_name = "Bliss", theme_preset = "Nature", theta = -60, phi = 30, flip_x = FALSE, flip_y = FALSE, flip_z = FALSE, ...) {
  # Standard beautiful 3D persp plot fallback
  # Save original par settings and restore on exit to prevent leaking styling to other plots
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  # Extract values matching the active model
  if (model_name == "Data") {
    mat <- data_list$raw_inhibition
    cTitle <- "Inhibition"
    is_div <- FALSE
  } else {
    mat <- data_list[[model_name]]$scores
    cTitle <- paste(model_name, "Score")
    is_div <- TRUE
  }

  # Load theme preset configuration for background colors and font families
  theme_cfg <- get_theme_palette(theme_preset, is_divergent = is_div)
  
  # Determine background color based on the selected publication theme preset
  bg_color <- "#ffffff"
  if (theme_preset == "The Economist") {
    bg_color <- "#e4eef2"
  } else if (theme_preset == "Financial Times") {
    bg_color <- "#fff1e5"
  }
  par(family = "sans", bg = bg_color)
  
  conc_A <- data_list$conc_A
  conc_B <- data_list$conc_B
  
  # Apply reactive flips
  flipped <- apply_plot_flips(mat, conc_A, conc_B, flip_x, flip_y, flip_z)
  mat_flipped <- flipped$mat
  
  # Transpose so that rows of mat_flipped (Drug A) maps to x-axis, and columns (Drug B) to y-axis of persp
  z_t <- t(mat_flipped)
  
  # persp expects strictly increasing x and y coords. We use the indices 1:nrow(z_t) and 1:ncol(z_t)
  x_coords <- 1:nrow(z_t)
  y_coords <- 1:ncol(z_t)
  
  # Revert to legacy color ramp from user's papers (Blue -> Green -> Red/Yellow gradient)
  ColorRamp <- rgb(seq(0, 1, length = 256),
                   seq(0, 1, length = 256),
                   seq(1, 0, length = 256))
  
  nbcol <- length(ColorRamp)
  
  zfacet <- z_t[-1, -1] + z_t[-1, -ncol(z_t)] + z_t[-nrow(z_t), -1] + z_t[-nrow(z_t), -ncol(z_t)]
  
  # Robust custom color scaling to prevent NA levels or cut() failures on identical ranges/negative values
  min_z <- min(zfacet, na.rm = TRUE)
  max_z <- max(zfacet, na.rm = TRUE)
  if (is.na(min_z) || is.na(max_z) || min_z == max_z) {
    facetcol <- rep(1, length(zfacet))
  } else {
    facetcol <- round((zfacet - min_z) / (max_z - min_z) * (nbcol - 1)) + 1
    facetcol[facetcol < 1] <- 1
    facetcol[facetcol > nbcol] <- nbcol
    facetcol[is.na(facetcol)] <- 1
  }
  
  zlab_text <- if (flip_z) paste(cTitle, "(Inverted)") else cTitle
  
  # Format axis labels to indicate if they have been flipped in the perspective
  xlab_text <- if (flip_x) "Drug A Concentration (Flipped)" else "Drug A Concentration"
  ylab_text <- if (flip_y) "Drug B Concentration (Flipped)" else "Drug B Concentration"
  
  # Find strictly formatted bounds for 3D coordinates
  z_min_val <- min(z_t, na.rm = TRUE)
  z_max_val <- max(z_t, na.rm = TRUE)
  if (z_min_val == z_max_val) z_max_val <- z_min_val + 1.0
  
  # Call persp with axes = FALSE and box = TRUE to draw the 3D frame box cleanly
  p_mat <- persp(x_coords, y_coords, z_t, col = ColorRamp[facetcol], phi = phi, theta = theta,
                 d = 5, r = 1, shade = 0.1, expand = 0.6,
                 xlab = "", ylab = "", zlab = "", axes = FALSE, box = TRUE)
                 
  # Project and draw Drug A (X axis) concentrations along the front-bottom edge
  for (i in 1:nrow(z_t)) {
    pt <- trans3d(i, 0.4, z_min_val - 0.08 * (z_max_val - z_min_val), p_mat)
    text(pt$x, pt$y, labels = colnames(mat_flipped)[i], cex = 0.7, adj = c(0.5, 1), xpd = TRUE)
  }
  pt_xlab <- trans3d(mean(x_coords), 0.1, z_min_val - 0.18 * (z_max_val - z_min_val), p_mat)
  text(pt_xlab$x, pt_xlab$y, labels = xlab_text, cex = 0.85, font = 2, xpd = TRUE)
  
  # Project and draw Drug B (Y axis) concentrations along the left-bottom edge
  for (j in 1:ncol(z_t)) {
    pt <- trans3d(0.4, j, z_min_val - 0.08 * (z_max_val - z_min_val), p_mat)
    text(pt$x, pt$y, labels = rownames(mat_flipped)[j], cex = 0.7, adj = c(0.5, 1), xpd = TRUE)
  }
  pt_ylab <- trans3d(0.1, mean(y_coords), z_min_val - 0.18 * (z_max_val - z_min_val), p_mat)
  text(pt_ylab$x, pt_ylab$y, labels = ylab_text, cex = 0.85, font = 2, xpd = TRUE)
  
  # Project and draw Z axis (Inhibition / Score) ticks and labels
  z_ticks <- pretty(c(z_min_val, z_max_val), n = 5)
  z_ticks <- z_ticks[z_ticks >= z_min_val & z_ticks <= z_max_val]
  for (zt in z_ticks) {
    pt <- trans3d(0.4, 0.4, zt, p_mat)
    lbl <- if (flip_z) abs(zt) else zt
    text(pt$x, pt$y, labels = as.character(lbl), cex = 0.7, adj = c(1.2, 0.5), xpd = TRUE)
  }
  pt_zlab <- trans3d(0.2, 0.2, mean(c(z_min_val, z_max_val)), p_mat)
  text(pt_zlab$x, pt_zlab$y, labels = zlab_text, cex = 0.85, font = 2, srt = 90, adj = c(0.5, 1.5), xpd = TRUE)
}
