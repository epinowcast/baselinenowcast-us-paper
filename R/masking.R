#' Clean the DPH percent of ED visit data for the masking comparison
#'
#' @param pct_data Data on percent of ED visits due to each syndrome, with
#'   the same-week preliminary percent (`Current Percent`), real-time DPH
#'   nowcast percent and 95% interval, and final percent.
#' @param pathogen_i Character string indicating the syndrome to keep.
#'
#' @returns Data frame with one row per reference week containing the
#'   same-week preliminary, nowcast and final percent of ED visits.
#' @importFrom dplyr mutate filter arrange distinct select
#' @importFrom lubridate dmy
#' @autoglobal
clean_pct_data <- function(pct_data, pathogen_i = "bar") {
  pct_data_clean <- pct_data |>
    mutate(
      reference_date = dmy(reference_date),
      nowcast_date = dmy(nowcast_date)
    ) |>
    filter(
      pathogen == pathogen_i,
      nowcast_date == reference_date
    ) |>
    arrange(reference_date) |>
    # Keep a single row per week where the source has duplicates
    distinct(reference_date, .keep_all = TRUE) |>
    select(
      reference_date,
      prelim_pct = `Current Percent`,
      nowcast_pct = `Prediction Percent`,
      nowcast_lower_pct = `Prediction Low Percent`,
      nowcast_upper_pct = `Prediction High Percent`,
      final_pct = `Final Percent`
    )
  return(pct_data_clean)
}

#' Determine weekly masking status from a threshold rule
#'
#' Masking turns on in the first week that `on_value` is above the threshold
#'   and turns off once `off_value` has been below the threshold for
#'   `n_weeks_off` consecutive weeks. Weeks with missing values leave the
#'   masking status unchanged.
#'
#' @param on_value Numeric vector, ordered by week, compared to the threshold
#'   to turn masking on.
#' @param off_value Numeric vector, ordered by week, compared to the threshold
#'   to turn masking off.
#' @param threshold Numeric threshold, on the same scale as the values.
#' @param n_weeks_off Integer number of consecutive weeks below the threshold
#'   required to turn masking off. Default is 2.
#'
#' @returns Logical vector indicating whether masking is on in each week.
get_masking_status <- function(on_value,
                               off_value,
                               threshold,
                               n_weeks_off = 2) {
  masking <- FALSE
  weeks_below <- 0
  status <- logical(length(on_value))
  for (i in seq_along(on_value)) {
    if (!masking) {
      if (isTRUE(on_value[i] > threshold)) {
        masking <- TRUE
        weeks_below <- 0
      }
    } else if (!is.na(off_value[i])) {
      weeks_below <- ifelse(off_value[i] < threshold, weeks_below + 1, 0)
      if (weeks_below >= n_weeks_off) {
        masking <- FALSE
      }
    }
    status[i] <- masking
  }
  return(status)
}

#' Add masking status under the nowcast and preliminary data rules
#'
#' @param pct_data_clean Output of `clean_pct_data()`.
#' @param threshold Masking threshold as a proportion of ED visits.
#' @param n_weeks_off Integer number of consecutive weeks below the threshold
#'   required to turn masking off.
#'
#' @returns `pct_data_clean` with logical columns `masking_nowcast` and
#'   `masking_prelim`.
#' @importFrom dplyr mutate
#' @autoglobal
add_masking_status <- function(pct_data_clean,
                               threshold,
                               n_weeks_off = 2) {
  masking_data <- pct_data_clean |>
    mutate(
      # Nowcast: on when the point estimate is above the threshold, off when
      # the upper bound of the 95% interval is below it
      masking_nowcast = get_masking_status(
        on_value = nowcast_pct,
        off_value = nowcast_upper_pct,
        threshold = threshold,
        n_weeks_off = n_weeks_off
      ),
      # Preliminary data: on and off based on the preliminary percent alone
      masking_prelim = get_masking_status(
        on_value = prelim_pct,
        off_value = prelim_pct,
        threshold = threshold,
        n_weeks_off = n_weeks_off
      )
    )
  return(masking_data)
}

#' Plot masking status under nowcast and preliminary data rules
#'
#' @param masking_data Output of `add_masking_status()`.
#' @param threshold Masking threshold as a proportion of ED visits.
#' @param fig_file_name Character string for output filename (without
#'   extension). If NULL, the figure is not saved.
#' @param fig_file_dir Character string for output directory.
#'
#' @returns ggplot object with one panel for the nowcast rule and one for the
#'   preliminary data rule.
#' @importFrom ggplot2 ggplot aes geom_rect geom_hline geom_line geom_ribbon
#'   facet_wrap scale_fill_manual scale_color_manual scale_x_date labs theme
#'   guides guide_legend ggsave element_text
#' @importFrom dplyr bind_rows mutate filter
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @autoglobal
get_plot_masking <- function(masking_data,
                             threshold,
                             fig_file_name = NULL,
                             fig_file_dir = file.path("output", "figs", "supp")) { # nolint
  panel_levels <- c("A. DPH nowcast", "B. Preliminary data")
  nowcast_panel <- mutate(
    masking_data,
    panel = panel_levels[1],
    series = "DPH nowcast",
    value = nowcast_pct,
    lower = nowcast_lower_pct,
    upper = nowcast_upper_pct,
    masking = masking_nowcast
  )
  prelim_panel <- mutate(
    masking_data,
    panel = panel_levels[2],
    series = "Preliminary data",
    value = prelim_pct,
    lower = NA_real_,
    upper = NA_real_,
    masking = masking_prelim
  )
  plot_data <- bind_rows(nowcast_panel, prelim_panel) |>
    mutate(panel = factor(panel, levels = panel_levels))
  model_colors <- plot_components()$model_colors
  nowcast_color <- model_colors[["DPH method (real-time)"]]
  prelim_color <- model_colors[["preliminary data"]]
  date_breaks <- seq(
    min(masking_data$reference_date),
    max(masking_data$reference_date),
    by = "2 weeks"
  )

  p <- ggplot(plot_data) +
    geom_rect(
      data = filter(plot_data, masking),
      aes(
        xmin = reference_date - 3.5, xmax = reference_date + 3.5,
        ymin = -Inf, ymax = Inf, fill = "Masking on"
      ),
      alpha = 0.6
    ) +
    geom_hline(yintercept = 100 * threshold, linetype = "dashed") +
    geom_ribbon(
      data = filter(plot_data, !is.na(lower)),
      aes(x = reference_date, ymin = 100 * lower, ymax = 100 * upper),
      fill = nowcast_color, alpha = 0.3
    ) +
    geom_line(aes(
      x = reference_date, y = 100 * final_pct,
      color = "Final data"
    )) +
    geom_line(aes(x = reference_date, y = 100 * value, color = series)) +
    facet_wrap(~panel, ncol = 1) +
    scale_fill_manual(values = c(`Masking on` = "#fdae6b"), name = "") +
    scale_color_manual(
      values = c(
        `DPH nowcast` = nowcast_color,
        `Preliminary data` = prelim_color,
        `Final data` = "black"
      ),
      breaks = c("DPH nowcast", "Preliminary data", "Final data"),
      name = ""
    ) +
    scale_x_date(breaks = date_breaks, date_labels = "%b %d") +
    get_plot_theme(dates = TRUE) +
    theme(
      legend.position = "top",
      strip.text = element_text(hjust = 0, face = "bold")
    ) +
    guides(
      color = guide_legend(order = 1),
      fill = guide_legend(order = 2)
    ) +
    labs(
      title = "2025-2026 season",
      x = "",
      y = "Same week % of ED visits due to BAR"
    )

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.png")),
      width = 10,
      height = 9,
      dpi = 600
    )
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.tiff")),
      device = "tiff",
      dpi = 600,
      compression = "lzw",
      type = "cairo",
      width = 10,
      height = 9
    )
  }
  return(p)
}
