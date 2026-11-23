#' Add the real-time denominator and baselinenowcast percents to the DPH
#'   percent of ED visit data
#'
#' The published DPH percents divide visit counts by the total number of ED
#'   visits available in real time, which is itself incomplete, rather than
#'   by the final total (`statewide_total`). This backs out that real-time
#'   denominator from the DPH predicted count and percent, checks it against
#'   the denominator implied by the preliminary count and percent, and uses it
#'   to express the baselinenowcast nowcasts as a percent of ED visits.
#'
#' @param pct_data Data on percent of ED visits due to each syndrome, with the
#'   real-time DPH nowcast count (`0.5`, `0.025`, `0.975`) and percent
#'   (`Prediction Percent`), and the preliminary count (`initial_count`) and
#'   percent (`Current Percent`). `nowcast_date` is the last day (Saturday) of
#'   the most recent week.
#' @param state_nowcasts Data frame of state-level nowcasts with columns
#'   `model`, `pathogen`, `reference_date`, `nowcast_date` (the Wednesday the
#'   nowcast was made), `quantile_level` and `quantile_value`.
#' @param bnc_model Character string indicating the baselinenowcast model to
#'   use. Default is `"baselinenowcast"`.
#'
#' @returns `pct_data` with dates parsed and added columns
#'   `realtime_denominator`, `prelim_denominator`, `denominator_ratio`
#'   (preliminary over real-time denominator), the baselinenowcast median and
#'   95% interval counts (`bnc_0.5`, `bnc_0.025`, `bnc_0.975`) and percents
#'   (`bnc Prediction Percent`, `bnc Prediction Low Percent`,
#'   `bnc Prediction High Percent`). baselinenowcast columns are missing for
#'   horizons that were not nowcast.
#' @importFrom dplyr mutate filter select left_join if_else
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate dmy
#' @autoglobal
add_realtime_denominator <- function(pct_data,
                                     state_nowcasts,
                                     bnc_model = "baselinenowcast") {
  bnc_wide <- state_nowcasts |>
    filter(model == bnc_model) |>
    # Match the DPH file, where nowcast_date is the end of the most recent
    # week (the Saturday before the Wednesday nowcast date)
    mutate(nowcast_week_end = nowcast_date - 4) |>
    select(
      pathogen, reference_date, nowcast_week_end,
      quantile_level, quantile_value
    ) |>
    pivot_wider(
      names_from = quantile_level,
      values_from = quantile_value,
      names_prefix = "bnc_"
    )

  pct_data_rt <- pct_data |>
    mutate(
      reference_date = dmy(reference_date),
      nowcast_date = dmy(nowcast_date),
      realtime_denominator = if_else(
        `Prediction Percent` > 0, `0.5` / `Prediction Percent`, NA_real_
      ),
      prelim_denominator = if_else(
        `Current Percent` > 0, initial_count / `Current Percent`, NA_real_
      ),
      denominator_ratio = prelim_denominator / realtime_denominator
    ) |>
    left_join(
      bnc_wide,
      by = c(
        "pathogen", "reference_date",
        nowcast_date = "nowcast_week_end"
      )
    ) |>
    mutate(
      `bnc Prediction Percent` = `bnc_0.5` / realtime_denominator,
      `bnc Prediction Low Percent` = `bnc_0.025` / realtime_denominator,
      `bnc Prediction High Percent` = `bnc_0.975` / realtime_denominator
    )
  return(pct_data_rt)
}

#' Check the real-time denominator against the preliminary percent
#'
#' @param pct_data_rt Output of `add_realtime_denominator()`.
#' @param pathogen_i Character string indicating the syndrome to check.
#' @param tolerance Relative difference above which a week is flagged.
#'   Default is 0.01 (1%).
#'
#' @returns Data frame with one row per same-week reference week, the
#'   relative difference between the denominators implied by the preliminary
#'   and predicted percents, and whether it exceeds `tolerance`.
#' @importFrom dplyr filter mutate select arrange
#' @autoglobal
check_realtime_denominator <- function(pct_data_rt,
                                       pathogen_i = "bar",
                                       tolerance = 0.01) {
  denominator_check <- pct_data_rt |>
    filter(pathogen == pathogen_i, nowcast_date == reference_date) |>
    arrange(reference_date) |>
    mutate(
      relative_difference = denominator_ratio - 1,
      exceeds_tolerance = abs(relative_difference) > tolerance
    ) |>
    select(
      reference_date, realtime_denominator, prelim_denominator,
      relative_difference, exceeds_tolerance
    )
  return(denominator_check)
}

#' Clean the DPH percent of ED visit data for the masking comparison
#'
#' @param pct_data Data on percent of ED visits due to each syndrome, with
#'   the same-week preliminary percent (`Current Percent`), real-time DPH
#'   nowcast percent and 95% interval, and final percent.
#' @param pathogen_i Character string indicating the syndrome to keep.
#'
#' @returns Data frame with one row per reference week containing the
#'   same-week preliminary, nowcast and final percent of ED visits, and the
#'   baselinenowcast percent when `pct_data` contains it.
#' @importFrom dplyr mutate filter arrange distinct select any_of
#' @importFrom lubridate dmy
#' @autoglobal
clean_pct_data <- function(pct_data, pathogen_i = "bar") {
  if (is.character(pct_data$reference_date)) {
    pct_data <- mutate(
      pct_data,
      reference_date = dmy(reference_date),
      nowcast_date = dmy(nowcast_date)
    )
  }
  pct_data_clean <- pct_data |>
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
      final_pct = `Final Percent`,
      bnc_pct = any_of("bnc Prediction Percent"),
      bnc_lower_pct = any_of("bnc Prediction Low Percent"),
      bnc_upper_pct = any_of("bnc Prediction High Percent")
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
#' @returns `pct_data_clean` with logical columns `masking_nowcast`,
#'   `masking_bnc` (missing if `pct_data_clean` has no baselinenowcast
#'   percent) and `masking_prelim`.
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
      # baselinenowcast (when available): same rule as the DPH nowcast
      masking_bnc = if ("bnc_pct" %in% names(pct_data_clean)) {
        get_masking_status(
          on_value = bnc_pct,
          off_value = bnc_upper_pct,
          threshold = threshold,
          n_weeks_off = n_weeks_off
        )
      } else {
        NA
      },
      # Preliminary data: on and off based on the preliminary percent alone
      masking_prelim = get_masking_status(
        on_value = prelim_pct,
        off_value = prelim_pct,
        threshold = threshold,
        n_weeks_off = n_weeks_off
      ),
      # Final data: the same rule as the preliminary data, applied to the
      # final percent, as a reference for when masking would ideally be on
      masking_final = get_masking_status(
        on_value = final_pct,
        off_value = final_pct,
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
#' @param include_bnc Boolean indicating whether to add a panel for the
#'   baselinenowcast nowcast rule. Requires `masking_data` to contain the
#'   baselinenowcast percent. Default is FALSE.
#' @param fig_file_name Character string for output filename (without
#'   extension). If NULL, the figure is not saved.
#' @param fig_file_dir Character string for output directory.
#'
#' @returns ggplot object with one panel for each nowcast rule and one for
#'   the preliminary data rule.
#' @importFrom ggplot2 ggplot aes geom_rect geom_hline geom_line geom_ribbon
#'   facet_wrap scale_fill_manual scale_color_manual scale_x_date labs theme
#'   guides guide_legend ggsave element_text
#' @importFrom dplyr bind_rows mutate filter
#' @importFrom fs dir_create
#' @importFrom stats setNames
#' @importFrom glue glue
#' @autoglobal
get_plot_masking <- function(masking_data,
                             threshold,
                             include_bnc = FALSE,
                             fig_file_name = NULL,
                             fig_file_dir = file.path("output", "figs", "supp")) { # nolint
  panel_levels <- c("A. DPH multiplier method nowcast", "B. Preliminary data")
  nowcast_panel <- mutate(
    masking_data,
    panel = panel_levels[1],
    series = "DPH multiplier method nowcast",
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
  plot_data <- bind_rows(nowcast_panel, prelim_panel)
  model_colors <- plot_components()$model_colors
  nowcast_color <- model_colors[["DPH method (real-time)"]]
  prelim_color <- model_colors[["preliminary data"]]
  bnc_color <- model_colors[["baselinenowcast"]]
  series_colors <- c(
    `DPH multiplier method nowcast` = nowcast_color,
    `Preliminary data` = prelim_color,
    `Final data` = "black"
  )
  if (isTRUE(include_bnc)) {
    # baselinenowcast first, then the DPH method, then preliminary data
    panel_levels <- c(
      "A. baselinenowcast nowcast", "B. DPH multiplier method nowcast",
      "C. Preliminary data"
    )
    bnc_panel <- mutate(
      masking_data,
      panel = panel_levels[1],
      series = "baselinenowcast nowcast",
      value = bnc_pct,
      lower = bnc_lower_pct,
      upper = bnc_upper_pct,
      masking = masking_bnc
    )
    plot_data <- bind_rows(
      bnc_panel,
      mutate(nowcast_panel, panel = panel_levels[2]),
      mutate(prelim_panel, panel = panel_levels[3])
    )
    series_colors <- c(
      `baselinenowcast nowcast` = bnc_color, series_colors
    )
  }
  plot_data <- mutate(plot_data,
    panel = factor(panel, levels = panel_levels),
    ribbon_color = ifelse(
      series == "baselinenowcast nowcast", bnc_color, nowcast_color
    )
  )
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
      aes(
        x = reference_date, ymin = 100 * lower, ymax = 100 * upper,
        fill = I(ribbon_color)
      ),
      alpha = 0.3
    ) +
    geom_line(aes(
      x = reference_date, y = 100 * final_pct,
      color = "Final data"
    )) +
    geom_line(aes(x = reference_date, y = 100 * value, color = series)) +
    facet_wrap(~panel, ncol = 1) +
    scale_fill_manual(values = c(`Masking on` = "#fdae6b"), name = "") +
    scale_color_manual(
      values = series_colors,
      breaks = names(series_colors),
      name = ""
    ) +
    scale_x_date(breaks = date_breaks, date_labels = "%b %d") +
    get_plot_theme(dates = TRUE) +
    theme(
      legend.position = "top",
      strip.text = element_text(hjust = 0, face = "bold")
    ) +
    guides(
      color = guide_legend(order = 1, nrow = if (isTRUE(include_bnc)) 2 else 1),
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
      height = if (isTRUE(include_bnc)) 12 else 9,
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
      height = if (isTRUE(include_bnc)) 12 else 9
    )
  }
  return(p)
}

#' Get the weeks in which masking turns on and off
#'
#' @param masking_data Output of `add_masking_status()`.
#'
#' @returns Data frame with one row per change in masking status, with columns
#'   `rule` ("baselinenowcast", "DPH method (real-time)" or
#'   "preliminary data"), `reference_date` (the week in which the status
#'   changes) and `change` ("Masking on" or "Masking off").
#' @importFrom dplyr arrange mutate filter select bind_rows lag
#' @autoglobal
get_masking_changes <- function(masking_data) {
  rules <- c(
    masking_bnc = "baselinenowcast",
    masking_nowcast = "DPH method (real-time)",
    masking_prelim = "preliminary data"
  )
  masking_data <- arrange(masking_data, reference_date)
  changes <- lapply(names(rules), function(col) {
    status <- masking_data[[col]]
    if (all(is.na(status))) {
      return(NULL)
    }
    prev_status <- lag(status, default = FALSE)
    out <- masking_data |>
      mutate(status = status, prev_status = prev_status) |>
      filter(status != prev_status) |>
      mutate(
        rule = rules[[col]],
        change = ifelse(status, "Masking on", "Masking off")
      ) |>
      select(rule, reference_date, change)
    return(out)
  })
  return(bind_rows(changes))
}

#' Plot same-week nowcasts of the percent of ED visits with masking changes
#'
#' One panel per nowcasting method, each showing the same-week nowcast and 95%
#'   prediction interval, the preliminary and final percent, the masking
#'   threshold, and vertical lines for the weeks in which masking would turn
#'   on and off under the nowcast and preliminary data rules.
#'
#' @param masking_data Output of `add_masking_status()`, including the
#'   baselinenowcast percent.
#' @param threshold Masking threshold as a proportion of ED visits.
#' @param title Character string for the plot title.
#' @param fig_file_name Character string for output filename (without
#'   extension). If NULL, the figure is not saved.
#' @param fig_file_dir Character string for output directory.
#'
#' @returns ggplot object
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline geom_line geom_ribbon
#'   facet_wrap scale_fill_manual scale_color_manual scale_linetype_manual
#'   scale_x_date labs theme guides guide_legend ggsave element_rect labeller
#' @importFrom dplyr bind_rows mutate filter transmute
#' @importFrom fs dir_create
#' @importFrom stats setNames
#' @importFrom glue glue
#' @autoglobal
get_plot_masking_nowcasts <- function(masking_data,
                                      threshold,
                                      title = "Broad Acute Respiratory, same-week nowcasts", # nolint
                                      fig_file_name = NULL,
                                      fig_file_dir = file.path("output", "figs")) { # nolint
  models <- c("baselinenowcast", "DPH method (real-time)")
  nowcasts <- bind_rows(
    transmute(
      masking_data, reference_date,
      model = models[1],
      value = bnc_pct, lower = bnc_lower_pct, upper = bnc_upper_pct,
      prelim_pct, final_pct
    ),
    transmute(
      masking_data, reference_date,
      model = models[2],
      value = nowcast_pct, lower = nowcast_lower_pct,
      upper = nowcast_upper_pct,
      prelim_pct, final_pct
    )
  ) |>
    mutate(model = factor(model, levels = models))

  changes <- get_masking_changes(masking_data)
  # Each panel shows its own nowcast rule and the preliminary data rule
  change_lines <- bind_rows(
    lapply(models, function(m) {
      model_changes <- filter(changes, rule %in% c(m, "preliminary data"))
      return(mutate(model_changes, model = m))
    })
  ) |>
    mutate(
      model = factor(model, levels = models),
      rule_type = ifelse(
        rule == "preliminary data",
        "Preliminary data", "Nowcast"
      )
    )

  plot_colors <- plot_components()$model_colors
  threshold_label <- glue("Masking threshold ({100 * threshold}%)")

  p <- ggplot(nowcasts) +
    geom_hline(
      aes(
        yintercept = 100 * threshold,
        linetype = threshold_label
      )
    ) +
    geom_vline(
      data = change_lines,
      aes(xintercept = reference_date, color = rule, linetype = rule_type),
      linewidth = 1
    ) +
    geom_ribbon(
      aes(
        x = reference_date, ymin = 100 * lower, ymax = 100 * upper,
        fill = model
      ),
      alpha = 0.3
    ) +
    geom_line(
      aes(
        x = reference_date, y = 100 * prelim_pct,
        color = "preliminary data"
      ),
      linewidth = 1
    ) +
    geom_line(
      aes(x = reference_date, y = 100 * final_pct, color = "Final data"),
      linewidth = 1
    ) +
    geom_line(
      aes(x = reference_date, y = 100 * value, color = model),
      linewidth = 1
    ) +
    facet_wrap(~model,
      ncol = 1,
      labeller = labeller(
        model = function(x) format_model_label(x, wrap = FALSE)
      )
    ) +
    scale_color_manual(
      values = c(plot_colors, `Final data` = "black"),
      labels = function(x) {
        return(sub(
          "^preliminary data$", "Preliminary data",
          format_model_label(x)
        ))
      },
      breaks = c(models, "preliminary data", "Final data"),
      name = ""
    ) +
    scale_fill_manual(
      values = plot_colors,
      labels = format_model_label,
      guide = "none"
    ) +
    scale_linetype_manual(
      values = setNames(
        c("dotted", "solid", "dashed"),
        c(threshold_label, "Nowcast", "Preliminary data")
      ),
      breaks = c(threshold_label, "Nowcast", "Preliminary data"),
      labels = c(
        threshold_label,
        "Masking on or off week (nowcast)",
        "Masking on or off week (preliminary data)"
      ),
      name = ""
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    get_plot_theme() +
    theme(
      strip.background = element_rect(color = NA, fill = NA),
      legend.position = "top"
    ) +
    guides(
      color = guide_legend(nrow = 2),
      linetype = guide_legend(nrow = 3, override.aes = list(color = "black"))
    ) +
    labs(
      title = title,
      x = "",
      y = "% of ED visits due to BAR"
    )

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.png")),
      width = 16,
      height = 10,
      dpi = 600
    )
  }
  return(p)
}

#' Plot masking status over time for each rule as a timeline strip
#'
#' One row per rule (baselinenowcast, DPH multiplier method, preliminary
#'   data and final data), with a bar for each week in which masking would be
#'   in place.
#'
#' @param masking_data Output of `add_masking_status()`, including the
#'   baselinenowcast masking status.
#' @param date_limits Date vector of length two giving the x-axis limits, to
#'   align the strip with a panel above it. Default is NULL (data range).
#'
#' @returns ggplot object
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_manual
#'   scale_y_discrete scale_x_date labs theme element_blank
#' @importFrom dplyr bind_rows transmute filter mutate
#' @autoglobal
get_plot_masking_timeline <- function(masking_data, date_limits = NULL) {
  rules <- c(
    masking_bnc = "baselinenowcast",
    masking_nowcast = "DPH method (real-time)",
    masking_prelim = "preliminary data",
    masking_final = "Final data"
  )
  timeline <- bind_rows(lapply(names(rules), function(col) {
    return(transmute(masking_data, reference_date,
      rule = rules[[col]], masking = .data[[col]]
    ))
  })) |>
    filter(masking) |>
    mutate(rule = factor(rule, levels = rev(rules)))
  plot_colors <- plot_components()$model_colors
  rule_labels <- function(x) {
    return(sub(
      "^preliminary data$", "Preliminary data",
      format_model_label(x, wrap = FALSE)
    ))
  }

  # Label each row inside the panel, before masking starts, rather than on
  # the y axis, so long labels do not shift the alignment of other panels
  label_date <- if (is.null(date_limits)) {
    min(masking_data$reference_date)
  } else {
    date_limits[1]
  }
  row_labels <- data.frame(
    rule = factor(rules, levels = rev(rules)),
    reference_date = label_date
  )

  p <- ggplot(timeline) +
    geom_tile(
      aes(x = reference_date, y = rule, fill = rule),
      width = 7, height = 0.7
    ) +
    geom_text(
      data = row_labels,
      aes(x = reference_date, y = rule, label = rule_labels(rule)),
      hjust = 0, size = 8
    ) +
    scale_fill_manual(
      values = c(plot_colors, `Final data` = "black"), guide = "none"
    ) +
    # No y-axis labels: rows are labelled inside the panel
    scale_y_discrete(breaks = NULL, drop = FALSE) +
    scale_x_date(
      date_breaks = "1 month", date_labels = "%b %Y",
      limits = date_limits
    ) +
    get_plot_theme() +
    labs(x = "", y = "", title = "Masking at public health hospitals") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  return(p)
}
