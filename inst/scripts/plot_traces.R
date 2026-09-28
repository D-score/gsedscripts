library(ggplot2)
library(dscore)

# create objects lf, sf, bsid
source("inst/scripts/read_phase1_data.R")
options(dplyr.summarise.inform = FALSE)

plot_traces <- function(df, type = c("a", "d", "ad", "long", "centiles", "a_filter", "d_filter"), minn = 20) {
  type <- match.arg(type)
  ins3 <- switch(df$ins[1L],
                 lf = "gto",
                 sf = "gpa",
                 bsid = "by3")
  if (type %in% c("d", "ad", "long", "centiles", "d_filter")) {
    ds <- df |>
      mutate(age = agedays / 365.25) |>
      dscore()
    df <- bind_cols(df, ds) |>
      mutate(agemonth = a * 12)
  } else {
    df$d <- NA_real_
  }

  if (type %in% c("a", "ad", "long", "centiles", "a_filter")) {
    agg <- df |>
      filter(!is.na(agedays)) |>
      mutate(
        agemos = agedays / 365.25 * 12,
        agegp  = cut(agemos, breaks = c(seq(0, 24, 1), 25:43))
      ) |>
      pivot_longer(cols = starts_with(!! ins3),
                   names_to = "item", values_to = "value",
                   values_drop_na = TRUE) |>
      group_by(item, agegp) |>
      summarise(p = mean(value, na.rm = TRUE),
                n = n(),
                a = mean(agemos, na.rm = TRUE)) |>
      filter(n >= minn)
  }
  if (type %in% c("d", "d_filter")) {
    agg <- df |>
      filter(!is.na(agedays)) |>
      mutate(
        dgp  = cut(d, breaks = seq(10, 90, 2))
      ) |>
      pivot_longer(cols = starts_with(!! ins3),
                   names_to = "item", values_to = "value",
                   values_drop_na = TRUE) |>
      group_by(item, dgp) |>
      summarise(p = mean(value, na.rm = TRUE),
                n = n(),
                d = mean(d, na.rm = TRUE)) |>
      filter(n >= minn)
  }

  nlev <- length(unique(agg$item))
  colors <- rep(RColorBrewer::brewer.pal(n = 8, "Dark2"), 1 + nlev/8)[1:nlev]
  if (type == "a") {
    g <- ggplot(data = agg, aes(x = a, y = p, colour = item)) +
      theme_minimal() +
      theme(legend.position = "none", axis.ticks = element_blank(),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(0, 42, 6), name = "Month",
                         expand = expansion(0.01), limits = c(0, 42)) +
      scale_y_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                         expand = expansion(0.01),
                         minor_breaks = NULL,
                         name = paste0("Proportion Pass (", toupper(df$ins[1L]), " - Phase 1)")) +
      geom_line() +
      geom_point(size = 1)
  }
  if (type == "a_filter") {
    agg_filter <-
      agg |> filter(item %in% c("gpamoc052", "gpamoc103")) |> left_join({get_itemtable(items =  c("gpamoc052", "gpamoc103"))})

    g <- ggplot(data = agg_filter, aes(x = a, y = p, colour = item)) +
      theme_minimal() +
      theme(legend.position = "none", axis.ticks = element_blank(),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(0, 42, 6), name = "Month",
                         expand = expansion(0.01), limits = c(0, 42)) +
      scale_y_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), limits = c(0, 1),
                         expand = expansion(0.01),
                         minor_breaks = NULL,
                         name = paste0("Proportion Pass (", toupper(df$ins[1L]), " - Phase 1)")) +
      geom_line() +
      geom_point(size = 1)
  }
  if (type == "d") {
    g <- ggplot(data = agg, aes(x = d, y = p, colour = item)) +
      theme_minimal() +
      theme(legend.position = "none", axis.ticks = element_blank(),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(10, 80, 10), name = "D-score",
                         expand = expansion(0.01), limits = c(6, 80)) +
      scale_y_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                         expand = expansion(0.01),
                         minor_breaks = NULL,
                         name = paste0("Proportion Pass (", toupper(df$ins[1L]), " - Phase 1)")) +
      geom_line() +
      geom_point(size = 1)
  }
  if (type == "d_filter") {
    agg_filter <-
      agg |> filter(item %in% c("gpamoc052", "gpamoc103")) |> left_join({get_itemtable(items =  c("gpamoc052", "gpamoc103"))})

    g <- ggplot(data = agg_filter, aes(x = d, y = p, colour = item)) +
      theme_minimal() +
      theme(legend.position = "none", axis.ticks = element_blank(),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(10, 80, 10), name = "D-score",
                         expand = expansion(0.01), limits = c(6, 80)) +
      scale_y_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), limits= c(0,1),
                         expand = expansion(0.01),
                         minor_breaks = NULL,
                         name = paste0("Proportion Pass (", toupper(df$ins[1L]), " - Phase 1)")) +
      geom_line() +
      geom_point(size = 1)
  }
  if (type == "ad") {
    col_manual = c("GSED-BGD" = "#D93F46", "GSED-PAK" = "#489033", "GSED-TZA" = "#47A1D8")
    df1 <- slice_sample(df, n = nrow(df), replace = FALSE)
    g <- ggplot(data = df1, aes(x = agemonth, y = d, colour = cohort)) +
      theme_minimal() +
      theme(legend.position = "none", axis.ticks = element_blank(),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_color_manual(values = col_manual) +
      scale_x_continuous(breaks = seq(0, 42, 6), name = "Month",
                         expand = expansion(0.01), limits = c(0, 42)) +
      scale_y_continuous(breaks = seq(10, 80, 10),
                         expand = expansion(0.01), limits = c(0, 80),
                         name = paste0("D-score (", toupper(df$ins[1L]), " - Phase 1)")) +
      geom_point(size = 1)
  }

  if (type == "long") {
    col_manual = c("GSED-BGD" = "#D93F46", "GSED-PAK" = "#489033", "GSED-TZA" = "#47A1D8")
    df1 <- df |>
      group_by(subjid) |>
      filter(n() > 1 & cohort == "GSED-TZA")
    g <- ggplot(data = df1, aes(x = agemonth, y = d, group = subjid, color = cohort)) +
      theme_minimal() +
      theme(legend.position = "none", axis.ticks = element_blank(),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_color_manual(values = col_manual) +
      scale_x_continuous(breaks = seq(0, 42, 6), name = "Month",
                         expand = expansion(0.01), limits = c(0, 42)) +
      scale_y_continuous(breaks = seq(10, 80, 10),
                         expand = expansion(0.01), limits = c(0, 80),
                         name = paste0("D-score (", toupper(df$ins[1L]), " - Phase 1)")) +
      geom_point(size = 1) +
      geom_line(alpha = 0.4)
  }

  if (type == "centiles") {
    r <- get_reference() |>
      mutate(agemos = age * 12) |>
      filter(agemos <= 42)
    chart_green <- rgb(224, 241, 231, maxColorValue = 255)
    chart_grey <- "grey50"
    g <- ggplot(df, aes(x = agemonth, y = mu)) +
      theme_minimal() +
      theme(legend.position = "none", axis.ticks = element_blank(),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 13)) +
    #  annotate("polygon", x = c(r$agemos, rev(r$agemos)),
    #           y = c(r$SDM2, rev(r$SDP2)), alpha = 0.4, fill = chart_green) +
      annotate("polygon", x = c(rev(r$agemos), (r$agemos)),
               y = c(rev(r$SDM2), (r$SDP2)), alpha = 0.4, fill = chart_green) +
      annotate("line", x = r$agemos, y = r$SDM2, lwd = 0.6, alpha = 0.5, color = chart_grey) +
      annotate("line", x = r$agemos, y = r$SDP2, lwd = 0.6, alpha = 0.5, color = chart_grey) +
      annotate("line", x = r$agemos, y = r$SDM1, lwd = 0.3, alpha = 0.5, color = chart_grey) +
      annotate("line", x = r$agemos, y = r$SDP1, lwd = 0.3, alpha = 0.5, color = chart_grey) +
      annotate("line", x = r$agemos, y = r$SD0, lwd = 1, alpha = 0.5, color = chart_grey) +
      scale_x_continuous(breaks = seq(0, 42, 6), name = "Month",
                         expand = expansion(0.01), limits = c(0, 42)) +
      scale_y_continuous(breaks = seq(10, 100, 10),
                         expand = expansion(0.01), limits = c(0, 89),
                         name = paste0("D-score (Model gsed2406)"))
  }


  return(g)
}

plot_traces(sf, type = "a_filter")

path <- path.expand("C:/Users/eekhouti/OneDrive - TNO/Projecten/gsed/presentations/Webinar")
pdf(file.path(path, "traces_sf_a.pdf"), width = 16, height = 9); plot_traces(sf, type = "a"); dev.off()
pdf(file.path(path, "traces_sf_a_filter.pdf"), width = 16, height = 9); plot_traces(sf, type = "a_filter"); dev.off()
pdf(file.path(path, "traces_sf_d.pdf"), width = 16, height = 9); plot_traces(sf, type = "d"); dev.off()
pdf(file.path(path, "traces_sf_d_filter.pdf"), width = 16, height = 9); plot_traces(sf, type = "d_filter"); dev.off()
pdf(file.path(path, "traces_sf_ad.pdf"), width = 9, height = 9); plot_traces(sf, type = "ad"); dev.off()
pdf(file.path(path, "traces_sf_long.pdf"), width = 9, height = 9); plot_traces(sf, type = "long"); dev.off()
pdf(file.path(path, "centiles.pdf"), width = 9, height = 9); plot_traces(sf, type = "centiles"); dev.off()

pdf(file.path(path, "traces_lf_a.pdf"), width = 16, height = 9); plot_traces(lf, type = "a"); dev.off()
pdf(file.path(path, "traces_lf_d.pdf"), width = 16, height = 9); plot_traces(lf, type = "d"); dev.off()
pdf(file.path(path, "traces_lf_ad.pdf"), width = 9, height = 9); plot_traces(lf, type = "ad"); dev.off()
pdf(file.path(path, "traces_lf_long.pdf"), width = 9, height = 9); plot_traces(lf, type = "long"); dev.off()

plot_traces(sf, type = "long")

plot_traces(sf, type = "centiles")
#
# plot_trace_age(bsid, minn = 8)
jpeg(file.path(path, "centiles.jpg"), width = 1800, height = 1800, res = 300); plot_traces(sf, type = "centiles"); dev.off()


#plot trace with data.
g <- plot_traces(sf, type = "centiles")

  df <- data.frame(a = c(1,2,4,6,12,17,20,26),
                   d = c(15,18,25,31, 44, 54, 56, 64)) |>
      mutate(ay = a/12,
             daz = dscore::daz(d, ay, reference_table = get_reference()))
cent_data <-
g +
  geom_point(data = df, aes(x = a, y = d))+
  geom_line(data = df, aes(x = a, y = d))

pdf(file.path(path, "centiles_line.pdf"), width = 9, height = 9); cent_data; dev.off()
jpeg(file.path(path, "centiles_line.jpg"), width = 1800, height = 1800, res = 300); cent_data; dev.off()


#plot in daz plot
r <- data.frame(agemos = c(0, 20, 30, 42),
                     SD0 = 0,
                     SDM1 = -1,
                     SDM2 = -2,
                     SDP1 = 1,
                     SDP2 = 2)

gdz <- ggplot(r, aes(x = agemos, y = SD0)) +
  theme_minimal() +
  theme(legend.position = "none", axis.ticks = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) +
  #  annotate("polygon", x = c(r$agemos, rev(r$agemos)),
  #           y = c(r$SDM2, rev(r$SDP2)), alpha = 0.4, fill = chart_green) +
  annotate("polygon", x = c(rev(r$agemos), (r$agemos)),
           y = c(rev(r$SDM2), (r$SDP2)), alpha = 0.4, fill = chart_green) +
  annotate("line", x = r$agemos, y = r$SDM2, lwd = 0.6, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SDP2, lwd = 0.6, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SDM1, lwd = 0.3, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SDP1, lwd = 0.3, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SD0, lwd = 1, alpha = 0.5, color = chart_grey) +
  scale_x_continuous(breaks = seq(0, 42, 6), name = "Month",
                     expand = expansion(0.01), limits = c(0, 42)) +
  scale_y_continuous(breaks = seq(-5, 5, 1),
                     expand = expansion(0.01), limits = c(-4, 4),
                     name = paste0("DAZ (Model gsed2406)"))

pdf(file.path(path, "centiles_daz.pdf"), width = 9, height = 9); gdz; dev.off()
jpeg(file.path(path, "centiles_daz.jpg"), width = 1800, height = 1800, res = 300); gdz; dev.off()

gdz_data <- gdz +
  geom_point(data = df, aes(x = a, y = daz))+
  geom_line(data = df, aes(x = a, y = daz))

pdf(file.path(path, "centiles_daz_line.pdf"), width = 9, height = 9); gdz_data; dev.off()
jpeg(file.path(path, "centiles_daz_line.jpg"), width = 1800, height = 1800, res = 300); gdz_data; dev.off()

#HAZ
hg <- read.csv(file.path(path, "/refs/lhfa-girls-zscore-expanded-tables(LFA_girls_z_exp).csv"))
hb <- read.csv(file.path(path, "/refs/lhfa-boys-zscore-expanded-tables(LFA_boys_z_exp).csv"))

r <- hg |>
  mutate(agemos = Day/365.25 *12)
chart_green <- rgb(224, 241, 231, maxColorValue = 255)
chart_grey <- "grey50"
haz <- ggplot(r, aes(x = agemos, y = SD0)) +
  theme_minimal() +
  theme(legend.position = "none", axis.ticks = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) +
  #  annotate("polygon", x = c(r$agemos, rev(r$agemos)),
  #           y = c(r$SDM2, rev(r$SDP2)), alpha = 0.4, fill = chart_green) +
  annotate("polygon", x = c(rev(r$agemos), (r$agemos)),
           y = c(rev(r$SD2neg), (r$SD2)), alpha = 0.4, fill = chart_green) +
  annotate("line", x = r$agemos, y = r$SD2neg, lwd = 0.6, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SD2, lwd = 0.6, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SD1neg, lwd = 0.3, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SD1, lwd = 0.3, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SD0, lwd = 1, alpha = 0.5, color = chart_grey) +
  scale_x_continuous(breaks = seq(0, 60, 6), name = "Month",
                     expand = expansion(0.01), limits = c(0, 60)) +
  scale_y_continuous(breaks = seq(45, 150, 5),
                     expand = expansion(0.01), limits = c(40, 125),
                     name = paste0("HAZ (Height for age WHO reference for GIRLS)"))
haz
pdf(file.path(path, "haz.pdf"), width = 9, height = 9); haz; dev.off()
jpeg(file.path(path, "haz.jpg"), width = 1800, height = 1800, res = 300); haz; dev.off()


#WAZ
wg <- read.csv(file.path(path, "/refs/wfa-girls-zscore-expanded-tables(WFA_girls_z_exp).csv"))

r <- wg |>
  mutate(agemos = Day/365.25 *12)
chart_green <- rgb(224, 241, 231, maxColorValue = 255)
chart_grey <- "grey50"
waz <- ggplot(r, aes(x = agemos, y = SD0)) +
  theme_minimal() +
  theme(legend.position = "none", axis.ticks = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) +
  #  annotate("polygon", x = c(r$agemos, rev(r$agemos)),
  #           y = c(r$SDM2, rev(r$SDP2)), alpha = 0.4, fill = chart_green) +
  annotate("polygon", x = c(rev(r$agemos), (r$agemos)),
           y = c(rev(r$SD2neg), (r$SD2)), alpha = 0.4, fill = chart_green) +
  annotate("line", x = r$agemos, y = r$SD2neg, lwd = 0.6, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SD2, lwd = 0.6, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SD1neg, lwd = 0.3, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SD1, lwd = 0.3, alpha = 0.5, color = chart_grey) +
  annotate("line", x = r$agemos, y = r$SD0, lwd = 1, alpha = 0.5, color = chart_grey) +
  scale_x_continuous(breaks = seq(0, 60, 6), name = "Month",
                     expand = expansion(0.01), limits = c(0, 60)) +
  scale_y_continuous(breaks = seq(2, 30, 2),
                     expand = expansion(0.01), limits = c(0, 32),
                     name = paste0("WAZ (Weight for age WHO reference for GIRLS)"))
waz
pdf(file.path(path, "waz.pdf"), width = 9, height = 9); waz; dev.off()
jpeg(file.path(path, "waz.jpg"), width = 1800, height = 1800, res = 300); waz; dev.off()
