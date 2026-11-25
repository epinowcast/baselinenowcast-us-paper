#' Get bar charts of WIS breakdown by pathogen and model
#'
#' @param scores_su Data.frame of scores by pathogen, nowcast date, model,
#'   and reference date
#' @param remove_legend Boolean indicating whether or not to remove the legend,
#'   default is FALSE
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 geom_bar scale_alpha_manual facet_wrap scale_fill_manual
#'  scale_alpha_manual guides guide_legend xlab ylab theme element_blank
#'  scale_y_discrete scale_x_continuous expansion
#' @importFrom scoringutils summarise_scores
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr distinct pull
#' @returns ggplot object
#' @autoglobal
get_bar_chart_scores <- function(scores_su,
                                 remove_legend = FALSE) {
  summary_scores <- scores_su |>
    summarise_scores(by = c("model", "pathogen_name", "pathogen")) |>
    pivot_longer(cols = c("overprediction", "underprediction", "dispersion")) |>
    mutate(
      name = factor(name, levels = c(
        "overprediction",
        "dispersion",
        "underprediction"
      ))
    )
  plot_comps <- plot_components()
  # Horizontal bars, with the first model at the top. Models are identified
  # by colour, so the model axis is left unlabelled.
  p <- ggplot(summary_scores) +
    geom_bar(
      aes(
        x = value, y = model, fill = model,
        alpha = name
      ),
      stat = "identity",
      position = "stack"
    ) +
    facet_wrap(~pathogen_name, scales = "free_x") +
    get_plot_theme(dates = FALSE) +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.05)),
      n.breaks = 3
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors,
      labels = format_model_label
    ) +
    scale_alpha_manual(
      name = "WIS components",
      values = plot_comps$score_alpha,
      # Same order as the bars are stacked, from left to right
      breaks = c("underprediction", "dispersion", "overprediction")
    ) +
    guides(
      alpha = guide_legend(
        title.position = "top",
        override.aes = list(fill = "gray20"),
        nrow = 1
      ),
      fill = "none"
    ) +
    xlab("WIS") +
    ylab("") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank()
    )

  if (isTRUE(remove_legend)) {
    p <- p + guides(
      alpha = "none",
      fill = "none"
    )
  }

  return(p)
}


#' Get a plot of WIS by horizon for each pathogen
#'
#' Mean WIS of each model at each horizon, split into its components
#'   (overprediction, dispersion and underprediction), for each pathogen.
#'   Bars for each model are shown side by side at each horizon.
#'
#' @param scores_su Data.frame of scores by pathogen, nowcast date, model,
#'   and reference date
#' @param bar_width Numeric width of each model's bar. Default is `NULL`,
#'   which splits a width of 0.8 between the models.
#' @param fig_file_name Character string indicating name of the figure. If
#'   `NULL` (default), the figure is not saved.
#' @param fig_file_dir Character string indicating the filepath
#' @importFrom scoringutils summarise_scores
#' @importFrom dplyr mutate group_by ungroup arrange
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_rect facet_wrap scale_fill_manual
#'   scale_alpha_manual scale_x_continuous guides guide_legend xlab ylab
#'   ggsave
#' @importFrom fs dir_create
#' @importFrom glue glue
#' @returns ggplot object
#' @autoglobal
get_plot_wis_by_horizon <- function(scores_su,
                                    bar_width = NULL,
                                    fig_file_name = NULL,
                                    fig_file_dir = file.path(
                                      "output", "figs", "supp"
                                    )) {
  models <- sort(unique(scores_su$model))
  if (is.null(bar_width)) {
    bar_width <- 0.8 / length(models)
  }
  # Offset of each model's bar from the horizon, so bars sit side by side
  offsets <- (seq_along(models) - (length(models) + 1) / 2) * bar_width

  wis_components <- scores_su |>
    mutate(
      horizon = as.integer(floor((nowcast_date - reference_date) / 7))
    ) |>
    summarise_scores(by = c("model", "pathogen_name", "horizon")) |>
    pivot_longer(cols = c("underprediction", "dispersion", "overprediction")) |>
    mutate(name = factor(name, levels = c(
      "underprediction", "dispersion", "overprediction"
    ))) |>
    # Stack the components: underprediction at the bottom, as in the bar
    # charts of overall WIS
    arrange(pathogen_name, horizon, model, name) |>
    group_by(pathogen_name, horizon, model) |>
    mutate(
      ymax = cumsum(value),
      ymin = ymax - value,
      x = -horizon + offsets[match(model, models)]
    ) |>
    ungroup()

  plot_comps <- plot_components()
  p <- ggplot(wis_components) +
    geom_rect(aes(
      xmin = x - bar_width / 2, xmax = x + bar_width / 2,
      ymin = ymin, ymax = ymax,
      fill = model, alpha = name
    )) +
    facet_wrap(~pathogen_name, nrow = 1, scales = "free_y") +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors,
      labels = format_model_label
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_comps$score_alpha
    ) +
    scale_x_continuous(breaks = -3:0) +
    guides(alpha = guide_legend(override.aes = list(fill = "gray30"))) +
    get_plot_theme(dates = FALSE) +
    xlab("Horizon (weeks)") +
    ylab("WIS")

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = p,
      filename = file.path(fig_file_dir, glue("{fig_file_name}.png")),
      width = 20,
      height = 6,
      dpi = 600
    )
  }
  return(p)
}


#' Get a plot illustrating nowcasts at certain dates
#'
#'
#' @param nowcasts Dataframe of the combined quantiles across
#'    horizons and nowcast dates
#' @param all_data Weekly data by reference and report date for all pathogens
#' @param max_delay Integer indicating the maximum delay, used to create the
#'   evaluation data.
#' @param nowcast_dates_to_plot Vector of character strings of the dates you
#'   wish to plot, default is `NULL` which will plot all of them
#' @param facet Boolean indicating whether or not to make separate facets
#'    of each model
#' @param fig_name_suffix Character string indicating fig file suffix,
#'   default is `"comp"`.
#' @param fig_file_dir Character string indicating the filepath
#' @param reference_date_range Vector of two dates. If given, only reference
#'   dates in this range are shown. Default is `NULL`, which shows all
#'   reference dates nowcasted.
#' @importFrom glue glue
#' @importFrom ggplot2 aes ggplot ggtitle xlab ylab geom_line geom_ribbon
#'    facet_wrap scale_color_manual scale_fill_manual guide_legend
#'     scale_linetype_manual
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#' @returns ggplot object
#' @autoglobal
get_plot_nowcasts_vs_data <- function(nowcasts,
                                      all_data,
                                      max_delay,
                                      pathogen_i,
                                      nowcast_dates_to_plot = NULL,
                                      facet = FALSE,
                                      fig_name_suffix = "comp",
                                      fig_file_dir = file.path("output", "figs", "supp"), # nolint
                                      reference_date_range = NULL) {
  # Show final data from the earliest reference date nowcasted
  nowcast_date_range <- c(
    min(nowcasts$reference_date),
    max(nowcasts$nowcast_date)
  )

  nc <- nowcasts |>
    filter(
      is.null(nowcast_dates_to_plot) |
        nowcast_date %in% c(nowcast_dates_to_plot),
      pathogen == pathogen_i
    ) |>
    mutate(nowcast_date_model = glue("{nowcast_date}-{model}")) |>
    pivot_wider(
      id_cols = c(
        "reference_date", "pathogen", "nowcast_date",
        "final_count", "initial_count", "model",
        "nowcast_date_model"
      ),
      names_from = quantile_level,
      values_from = quantile_value,
      names_prefix = "q_"
    )
  data_only <- all_data |>
    filter(
      delay <= max_delay,
      pathogen == pathogen_i
    ) |>
    group_by(pathogen, end_of_week_reference_date) |>
    summarise(final_count = sum(count)) |>
    filter(
      end_of_week_reference_date <= nowcast_date_range[2],
      end_of_week_reference_date >= nowcast_date_range[1]
    )
  if (!is.null(reference_date_range)) {
    nc <- filter(
      nc,
      reference_date >= min(reference_date_range),
      reference_date <= max(reference_date_range)
    )
    data_only <- filter(
      data_only,
      end_of_week_reference_date >= min(reference_date_range),
      end_of_week_reference_date <= max(reference_date_range)
    )
  }
  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)

  nc <- mutate(nc, pathogen_name = !!pathogen_name)
  plot_comps <- plot_components()
  p <- ggplot() +
    geom_line(
      data = nc,
      aes(
        x = reference_date, y = `q_0.5`,
        color = model, group = nowcast_date_model
      )
    ) +
    facet_wrap(~model,
      ncol = 1,
      labeller = labeller(
        model = function(x) format_model_label(x, wrap = FALSE)
      )
    ) +
    geom_line(
      data = nc,
      aes(
        x = reference_date, y = initial_count,
        group = nowcast_date,
        linetype = "Data as of nowcast date"
      ),
      color = "gray",
      linewidth = 1
    ) +
    geom_vline(
      data = nc,
      aes(
        xintercept = nowcast_date,
        linetype = "Date of nowcast"
      ),
      color = "navy"
    ) +
    geom_ribbon(
      data = nc,
      aes(
        x = reference_date,
        ymin = `q_0.025`,
        ymax = `q_0.975`, fill = model,
        group = nowcast_date_model,
        alpha = "95%"
      )
    ) +
    geom_line(
      data = data_only,
      aes(
        x = end_of_week_reference_date, y = final_count,
        linetype = "Final observed values"
      ),
      color = "black", linewidth = 1
    ) +
    get_plot_theme(dates = TRUE) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    ggtitle(glue("{pathogen_name}")) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors,
      labels = format_model_label
    ) +
    # Add scale for the reference lines
    scale_linetype_manual(
      name = "Observed data",
      values = c(
        "Final observed values" = "solid",
        "Data as of nowcast date" = "solid",
        "Date of nowcast" = "dashed"
      ),
      breaks = c(
        "Final observed values",
        "Data as of nowcast date",
        "Date of nowcast"
      ),
      guide = guide_legend(
        override.aes = list(
          color = c(
            "Final observed values" = "black",
            "Data as of nowcast date" = "gray",
            "Date of nowcast" = "navy"
          ),
          linewidth = 1
        )
      )
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors,
      labels = format_model_label
    ) +
    scale_alpha_manual(
      name = "Prediction intervals",
      values = c(
        "95%" = 0.2,
        "50%" = 0.4
      ),
      guide = guide_legend(
        override.aes = list(
          alpha = c(
            "95%" = 0.2,
            "50%" = 0.4
          )
        )
      )
    ) +
    xlab("") +
    ylab(glue("ED visits")) +
    guides(
      color = guide_legend(title.position = "top"),
      fill = guide_legend(title.position = "top"),
      linetype = guide_legend(
        title.position = "top",
        nrow = 3
      ),
      alpha = guide_legend(title.position = "top")
    )

  if (isTRUE(facet)) {
    p <- p + facet_wrap(~model,
      ncol = 1,
      labeller = labeller(
        model = function(x) format_model_label(x, wrap = FALSE)
      )
    )
  }

  dir_create(fig_file_dir)

  fig_file_name <- glue::glue("{pathogen_i}_state_nowcast_comp_{fig_name_suffix}") # nolint
  ggsave(
    plot = p,
    filename = file.path(
      fig_file_dir,
      glue("{fig_file_name}.tiff")
    ),
    device = "tiff",
    dpi = 600,
    compression = "lzw",
    type = "cairo",
    width = 24,
    height = 12
  )
  ggsave(
    plot = p,
    filename = file.path(
      fig_file_dir,
      glue("{fig_file_name}.png")
    ),
    width = 24,
    height = 12,
    dpi = 600
  )


  return(p)
}

#' Make state nowcast comparison figure
#'
#' The nowcast panel (with the optional masking strip) and the WIS bar
#'   charts are combined as two blocks, each with its own legends: the model,
#'   prediction interval and observed data legends sit below the nowcasts
#'   (between the nowcasts and the masking strip, if included) and the WIS
#'   components legend sits below the bar charts.
#'
#' @param nowcasts_vs_data plot A
#' @param bar_chart_scores1 plot B
#' @param bar_chart_scores2 plot C
#' @param bar_chart_scores3 plot D
#' @param bar_chart_scores4 plot E
#' @param masking_timeline Optional plot of masking status over time, from
#'   `get_plot_masking_timeline()`, placed directly below `nowcasts_vs_data`
#'   (which then loses its date labels). Default is NULL (not included).
#' @param fig_file_name name of figure
#' @param fig_file_dir filepath to save figure
#'
#' @returns patchwork fig
#' @importFrom patchwork plot_annotation plot_layout wrap_plots
#' @importFrom fs dir_create
#' @autoglobal
make_state_nowcast_comp_fig <- function(
  nowcasts_vs_data,
  bar_chart_scores1,
  bar_chart_scores2,
  bar_chart_scores3,
  bar_chart_scores4,
  masking_timeline = NULL,
  fig_file_name = NULL,
  fig_file_dir = file.path("output", "figs")
) {
  fig_theme <- comp_fig_theme(size = 24)
  # The shared theme is added to each panel before its own changes, so that
  # it does not switch back on axis text a panel has removed
  nowcasts_vs_data <- nowcasts_vs_data + fig_theme
  top_panels <- nowcasts_vs_data +
    theme(axis.text.x = element_text(size = 24))
  top_heights <- 1
  top_guides <- "collect"
  if (!is.null(masking_timeline)) {
    # Masking strip shares the nowcast panel's dates, so the dates are shown
    # only once, below the strip
    top_panels <- (nowcasts_vs_data +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())) /
      (masking_timeline + fig_theme + theme(
        axis.text.x = element_text(size = 24),
        axis.text.y = element_blank(),
        axis.line.y = element_blank()
      ))
    top_heights <- c(3, 1)
    # Keep the nowcast legends with the nowcast panel, between it and the
    # masking strip
    top_guides <- "keep"
  }
  top_panels <- top_panels +
    plot_layout(
      heights = top_heights, axes = "collect_x", guides = top_guides
    ) &
    theme(legend.position = "bottom")

  wis_panels <- get_wis_bar_panels(
    list(
      bar_chart_scores1, bar_chart_scores2,
      bar_chart_scores3, bar_chart_scores4
    ),
    ncol = 4,
    fig_theme = fig_theme
  )

  fig <- wrap_plots(top_panels, wis_panels, ncol = 1) +
    plot_layout(heights = c(if (is.null(masking_timeline)) 3 else 4, 1)) +
    plot_annotation(tag_levels = "A")

  if (!is.null(fig_file_name)) {
    dir_create(fig_file_dir)
    ggsave(
      plot = fig,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.tiff")
      ),
      device = "tiff",
      dpi = 600,
      compression = "lzw",
      type = "cairo",
      width = 22,
      height = 22
    )
    ggsave(
      plot = fig,
      filename = file.path(
        fig_file_dir,
        glue("{fig_file_name}.png")
      ),
      width = 22,
      height = 22,
      dpi = 600
    )
  }

  return(fig)
}

#' Shared theme for the nowcast comparison figures (Figs 3 and 4)
#'
#' @param size Numeric font size for axis numbers and strip text. Axis titles,
#'   legend text and panel tags are scaled up from this.
#'
#' @returns a theme object
#' @importFrom ggplot2 theme element_text
comp_fig_theme <- function(size = 24) {
  return(
    larger_axis_text(size = size) +
      theme(
        axis.title = element_text(size = size + 6),
        strip.text = element_text(size = size),
        plot.title = element_text(size = size + 4),
        legend.text = element_text(size = size + 4),
        legend.title = element_text(size = size + 6),
        legend.justification = "center",
        # Panel letters the same size as the axis titles, in the margin so
        # they do not overlap them
        plot.tag = element_text(size = size + 6, face = "bold"),
        plot.tag.location = "margin",
        plot.tag.position = "topleft"
      )
  )
}

#' Combine WIS bar charts into one block with its own legend
#'
#' The WIS components legend is collected below the bar charts, so that it
#'   is kept apart from the prediction interval legend of the nowcast panels.
#'
#' @param bar_charts List of bar charts from `get_bar_chart_scores()`.
#' @param ncol Integer number of columns of bar charts.
#' @param fig_theme Theme applied to every bar chart.
#'
#' @returns patchwork of the bar charts
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom ggplot2 theme element_blank element_text margin
get_wis_bar_panels <- function(bar_charts, ncol, fig_theme) {
  return(
    wrap_plots(bar_charts, ncol = ncol) +
      plot_layout(axes = "collect", guides = "collect") &
      fig_theme &
      theme(
        # Models are identified by colour, as in the nowcast panels
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = fig_theme$axis.text.y$size),
        axis.line.y = element_blank(),
        # Room on the right so the last tick label of one chart does not run
        # into the first tick label of the next
        plot.margin = margin(5.5, 30, 5.5, 5.5),
        legend.position = "bottom"
      )
  )
}

#' State-level WIS over time + final data, faceted by pathogen
#'
#' @param all_data Weekly data by reference and report date for all pathogens
#' @param scores Data.frame of scores by pathogen, nowcast date, model,
#'   and reference date
#' @param nowcasts Dataframe of the combined quantiles across
#'    horizons and nowcast dates
#' @param max_delay Integer indicating the maximum delay, used to create the
#'   evaluation data.
#' @param fig_file_name Character string indicating name of fig
#' @param fig_file_dir Character string indicating the filepath
#'
#' @returns patchwork fig
#' @importFrom patchwork plot_annotation plot_layout
#' @importFrom fs dir_create
#' @importFrom dplyr filter summarise group_by
#' @autoglobal
get_state_wis_over_time_plot <- function(all_data,
                                         scores,
                                         nowcasts,
                                         max_delay,
                                         fig_file_name = NULL,
                                         fig_file_dir = file.path("output", "figs", "supp")) { # nolint
  nowcast_date_range <- c(
    min(nowcasts$nowcast_date),
    max(nowcasts$nowcast_date)
  )

  data_only <- all_data |>
    filter(
      delay <= max_delay
    ) |>
    group_by(pathogen, pathogen_name, end_of_week_reference_date) |>
    summarise(final_count = sum(count)) |>
    filter(
      end_of_week_reference_date <= nowcast_date_range[2],
      end_of_week_reference_date >= nowcast_date_range[1]
    )

  scores_t <- scores |>
    summarise_scores(by = c(
      "model", "nowcast_date",
      "pathogen_name", "pathogen"
    ))
  plot_comps <- plot_components()
  p1 <- ggplot(scores_t) +
    geom_line(
      aes(
        x = nowcast_date, y = wis, color = model
      )
    ) +
    facet_wrap(~pathogen_name, nrow = 4, scales = "free_y") +
    get_plot_theme(dates = TRUE) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors,
      labels = format_model_label
    ) +
    guides(
      color = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 1
      )
    ) +
    xlab("") +
    ylab("WIS")

  p2 <- ggplot(data_only) +
    geom_line(
      aes(
        x = end_of_week_reference_date, y = final_count
      )
    ) +
    facet_wrap(~pathogen_name, nrow = 4, scales = "free_y") +
    get_plot_theme(dates = TRUE) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    xlab("") +
    ylab("ED visits")

  fig_layout <- "
  AB
  "

  fig <- p1 + p2 +
    plot_layout(
      design = fig_layout,
      axes = "collect",
      guides = "collect"
    ) +
    plot_annotation(
      theme = theme(
        legend.position = "top",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(size = 20),
        legend.justification = "center",
        plot.tag = element_text(size = 20)
      )
    )
  dir_create(fig_file_dir)
  ggsave(
    plot = fig,
    filename = file.path(
      fig_file_dir,
      glue("{fig_file_name}.tiff")
    ),
    device = "tiff",
    dpi = 600,
    compression = "lzw",
    type = "cairo",
    width = 24,
    height = 12
  )
  ggsave(
    plot = fig,
    filename = file.path(
      fig_file_dir,
      glue("{fig_file_name}.png")
    ),
    width = 12,
    height = 12,
    dpi = 600
  )
  return(fig)
}


#' Bar chart of 95th percent coverage for each pathogen and model
#'
#' @param coverage Dataframe of 95% interval coverage by model and pathogen
#' @param title Title of plot
#' @param fig_file_name Character string indicating name of fig
#' @param fig_file_dir Character string indicating the filepathir
#' @autoglobal
#' @returns ggplot object
#' @importFrom ggplot2 labs
#' @importFrom dplyr n

get_bar_chart_coverage <- function(coverage,
                                   title = NULL,
                                   fig_file_name = NULL,
                                   fig_file_dir = file.path("output", "figs", "supp")) { # nolint

  coverage_wide <- coverage |>
    group_by(model, pathogen_name, pathogen, interval_range) |>
    summarise(interval_coverage = sum(interval_coverage) / n()) |>
    pivot_wider(
      names_from = interval_range,
      values_from = interval_coverage
    )

  plot_comps <- plot_components()
  p <- ggplot(coverage_wide) +
    geom_bar(
      aes(
        x = model, y = `95`,
        fill = model
      ),
      stat = "identity", position = "stack"
    ) +
    facet_wrap(~pathogen_name) +
    get_plot_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(color = NA, fill = NA),
      strip.text = element_text(size = 18),
      legend.position = "top"
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors,
      labels = format_model_label
    ) +
    geom_hline(aes(yintercept = 0.95), linetype = "dashed") +
    labs(
      y = "Interval coverage", x = ""
    ) +
    guides(
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      )
    ) +
    ggtitle(title)
  dir_create(fig_file_dir)
  ggsave(
    plot = p,
    filename = file.path(
      fig_file_dir,
      glue("{fig_file_name}.tiff")
    ),
    device = "tiff",
    dpi = 600,
    compression = "lzw",
    type = "cairo",
    width = 24,
    height = 12
  )
  ggsave(
    plot = p,
    filename = file.path(
      fig_file_dir,
      glue("{fig_file_name}.png")
    ),
    width = 12,
    height = 12,
    dpi = 600
  )
  return(p)
}
