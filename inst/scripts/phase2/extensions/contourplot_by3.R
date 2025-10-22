# install.packages("fields")  # if needed
library(dplyr)
library(tidyr)
library(ggplot2)
library(metR)
library(fields)
library(tibble)

path_new <- file.path(Sys.getenv("GSED_PHASE2"), "202510", "544_0_ext_by3")
select_me <- readRDS(file.path(path_new, "select_me_include_true.Rds"))

# --- 1) Matrix -> tidy with parsed axes (handles "Inf") -----------------------

m <- select_me["n", 1:9, 1:9]

df <- as.data.frame(m, check.names = FALSE) |>
  rownames_to_column("infit") |>
  pivot_longer(-infit, names_to = "outfit", values_to = "z")

parse_level <- function(x, prefix) {
  s <- sub(paste0("^", prefix, "_"), "", x)
  is_inf <- s == "Inf"
  out <- suppressWarnings(as.numeric(s))
  out[is_inf] <- Inf
  out
}

df <- df |>
  mutate(
    infit_num = parse_level(infit, "infit"),
    outfit_num = parse_level(outfit, "outfit")
  )

# Place Inf one step beyond the max finite value and keep pretty ticks
place_inf <- function(v) {
  v_fin <- sort(unique(v[is.finite(v)]))
  step <- if (length(v_fin) >= 2) min(diff(v_fin)) else 1
  inf_pos <- max(v_fin) + step
  v_plot <- ifelse(is.infinite(v), inf_pos, v)
  list(
    v_plot = v_plot,
    breaks = c(v_fin, inf_pos),
    labels = c(as.character(v_fin), "Inf")
  )
}

xp <- place_inf(df$infit_num)
yp <- place_inf(df$outfit_num)

df <- df |>
  mutate(infit_plot = xp$v_plot, outfit_plot = yp$v_plot)

# --- 2) Fit Thin Plate Spline and predict to a dense grid --------------------

# Tune these:
lambda_val <- 0.002 # ↑ lambda = smoother/stricter (try 0, 0.2, 0.8, 2, 5)
grid_n <- 200 # grid density for smooth contours

# Fit Tps on observed grid points
fit <- Tps(cbind(df$infit_plot, df$outfit_plot), df$z, lambda = lambda_val)

# Prediction grid
grid <- expand.grid(
  infit_plot = seq(min(df$infit_plot), max(df$infit_plot), length.out = grid_n),
  outfit_plot = seq(
    min(df$outfit_plot),
    max(df$outfit_plot),
    length.out = grid_n
  )
)

grid$z <- as.numeric(predict(
  fit,
  as.matrix(grid[, c("infit_plot", "outfit_plot")])
))
# (Optional) clamp extreme values to observed range to avoid edge artifacts
z_range <- range(df$z, na.rm = TRUE)
grid$z <- pmin(pmax(grid$z, z_range[1]), z_range[2])

# --- 3) Plot: filled contours (plus optional lines) --------------------------

p_filled <- ggplot(grid, aes(x = infit_plot, y = outfit_plot, z = z)) +
  geom_point(
    data = data.frame(
      infit_plot = c(1.2, 1.3, 1.4),
      outfit_plot = c(1.3, 1.4, 1.5)
    ),
    aes(x = infit_plot, y = outfit_plot),
    inherit.aes = FALSE,
    size = 1,
    shape = 19,
    colour = "grey40",
    stroke = 1
  ) +
  # Contour lines
  geom_contour(
    breaks = seq(40, 75, 5),
    linewidth = 0.3,
    alpha = 0.6,
    color = "black"
  ) +
  # Add contour labels
  geom_text_contour(
    breaks = seq(40, 75, 5),
    stroke = 0.2, # outline around text
    size = 3,
    color = "black",
    skip = 0 # show all labels; increase to reduce overlap
  ) +
  scale_x_continuous(
    "Infit",
    breaks = xp$breaks,
    labels = xp$labels,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    "Outfit",
    breaks = yp$breaks,
    labels = yp$labels,
    expand = expansion(mult = c(0, 0))
  ) +
  coord_fixed() +
  labs(
    title = "Average number of BSID-III items endorsed",
    fill = NULL
  ) +
  theme_minimal(base_size = 12)

p_filled

# --- SEM Plotting Example -----------------------------------------------------

m <- select_me["sem", 1:9, 1:9]

df <- as.data.frame(m, check.names = FALSE) |>
  rownames_to_column("infit") |>
  pivot_longer(-infit, names_to = "outfit", values_to = "z")

# parse numeric axes from "infit_*" / "outfit_*"
parse_level <- function(x, prefix) {
  s <- sub(paste0("^", prefix, "_"), "", x)
  out <- suppressWarnings(as.numeric(s))
  out
}

df <- df |>
  mutate(
    infit_num = parse_level(infit, "infit"),
    outfit_num = parse_level(outfit, "outfit")
  )

# (No Inf here, but keep the helper to be consistent)
place_inf <- function(v) {
  v_fin <- sort(unique(v[is.finite(v)]))
  step <- if (length(v_fin) >= 2) min(diff(v_fin)) else 1
  inf_pos <- max(v_fin) + step
  list(v_plot = v, breaks = v_fin, labels = as.character(v_fin))
}

xp <- place_inf(df$infit_num)
yp <- place_inf(df$outfit_num)

df <- df |>
  mutate(infit_plot = xp$v_plot, outfit_plot = yp$v_plot)

# --- 2) Smooth surface with Thin Plate Splines -------------------------------
lambda_val <- 0.0013 # increase for stricter/less wiggly contours (e.g., 2, 5)
grid_n <- 200

fit <- Tps(cbind(df$infit_plot, df$outfit_plot), df$z, lambda = lambda_val)

grid <- expand.grid(
  infit_plot = seq(min(df$infit_plot), max(df$infit_plot), length.out = grid_n),
  outfit_plot = seq(
    min(df$outfit_plot),
    max(df$outfit_plot),
    length.out = grid_n
  )
)
grid$z <- as.numeric(predict(
  fit,
  as.matrix(grid[, c("infit_plot", "outfit_plot")])
))
# clamp to observed range to avoid edge artifacts
z_range <- range(df$z, na.rm = TRUE)
grid$z <- pmin(pmax(grid$z, z_range[1]), z_range[2])

# --- 3) Choose nice contour breaks around SEM range --------------------------
# e.g., steps of 0.05 if that suits your reporting
brks <- seq(
  floor(z_range[1] * 20) / 20,
  ceiling(z_range[2] * 20) / 20,
  by = 0.05
)

# --- 4) Plot: labeled contour lines (optionally add filled contours) ---------
p_sem <- ggplot(grid, aes(x = infit_plot, y = outfit_plot, z = z)) +
  # Uncomment next line if you also want a filled background:
  # geom_contour_filled(breaks = brks) +
  geom_point(
    data = data.frame(
      infit_plot = c(1.2, 1.3, 1.4),
      outfit_plot = c(1.3, 1.4, 1.5)
    ),
    aes(x = infit_plot, y = outfit_plot),
    inherit.aes = FALSE,
    size = 1,
    shape = 19,
    colour = "grey40",
    stroke = 1
  ) +
  geom_contour(breaks = brks, linewidth = 0.3, alpha = 0.7, color = "black") +
  geom_text_contour(
    breaks = brks,
    size = 3,
    stroke = 0.5, # outline around text
    color = "black", # text color
    stroke.color = "white", # background fill color
    skip = 0 # show all labels; increase to reduce overlap
  ) +
  scale_x_continuous(
    "Infit",
    breaks = xp$breaks,
    labels = xp$labels,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    "Outfit",
    breaks = yp$breaks,
    labels = yp$labels,
    expand = expansion(mult = c(0, 0))
  ) +
  coord_fixed() +
  labs(
    title = "Average standard error of measurement (SEM)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12)

p_sem

# --- 5) Arrange and save plots -----------------------------------------------
library(patchwork)
p_combined <- p_filled +
  p_sem +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
p_combined
ggsave(
  "contours_include_true.pdf",
  p_combined,
  width = 12,
  height = 6,
  dpi = 300
)
# End of code
