#' Get bar charts of WIS breakdown by pathogen and model
#'
#' @param scores_su Data.frame of scores by pathogen, nowcast date, model,
#'   and reference date
#' @importFrom scoringutils summarise_scores
#' @importFrom ggplot2 geom_bar scale_alpha_manual facet_wrap scale_fill_manual
#'  scale_alpha_manual guides guide_legend xlab ylab theme element_blank
#' @importFrom scoringutils summarise_scores
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr distinct pull
#' @returns ggplot object
#' @autoglobal
get_overall_scores <- function(scores_su) {
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
  p <- ggplot(summary_scores) +
    geom_bar(
      aes(
        x = model, y = value, fill = model,
        alpha = name
      ),
      stat = "identity",
      position = "stack"
    ) +
    facet_wrap(~pathogen_name, scales = "free_y") +
    get_plot_theme() +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    scale_alpha_manual(
      name = "WIS breakdown",
      values = plot_comps$score_alpha
    ) +
    guides(
      # Can used fill = "none" if we want to remove color
      alpha = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      ),
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3
      )
    ) +
    xlab("") +
    ylab("WIS") +
    theme(axis.text.x = element_blank())

  return(p)
}

#' Get a plot illustrating nowcasts at certain dates
#'
#'
#' @param nowcasts Dataframe of the combined quantiles across
#'    horizons and nowcast dates
#' @param max_delay Integer indicating the maximum delay, used to create the
#'   evaluation data.
#' @param nowcast_dates_to_plot Vector of character strings of the dates you
#'   wish to plot, default is `NULL` which will plot all of them
#' @param facet Boolean indicating whether or not to make separate facets
#'    of each model
#' @importFrom glue glue
#' @importFrom ggplot2 aes ggplot ggtitle xlab ylab geom_line geom_ribbon
#'    facet_wrap scale_color_manual scale_fill_manual guide_legend
#'     scale_linetype_manual
#' @importFrom dplyr filter
#' @returns ggplot object
#' @autoglobal
get_plot_nowcasts_vs_data <- function(nowcasts,
                                      all_data,
                                      max_delay,
                                      pathogen_i,
                                      nowcast_dates_to_plot = NULL,
                                      facet = FALSE) {
  nowcast_date_range <- c(
    min(nowcasts$nowcast_date),
    max(nowcasts$nowcast_date)
  )

  nc <- nowcasts |>
    filter(
      is.null(nowcast_dates_to_plot) | nowcast_date %in% c(nowcast_dates_to_plot),
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
  pathogen_name <- all_data |>
    filter(pathogen == pathogen_i) |>
    distinct(pathogen_name) |>
    pull(pathogen_name)
  plot_comps <- plot_components()
  p <- ggplot() +
    geom_line(
      data = nc,
      aes(
        x = reference_date, y = `q_0.5`,
        color = model, group = nowcast_date_model
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
      color = "black"
    ) +
    geom_ribbon(
      data = nc,
      aes(
        x = reference_date,
        ymin = `q_0.25`,
        ymax = `q_0.75`, fill = model,
        group = nowcast_date_model,
        alpha = "50%"
      )
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
        linetype = "Final evaluation data"
      ),
      color = "red", linewidth = 1
    ) +
    get_plot_theme() +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    scale_color_manual(
      name = "Model",
      values = plot_comps$model_colors
    ) +
    # Add scale for the reference lines
    scale_linetype_manual(
      name = "Observed data",
      values = c(
        "Final evaluation data" = "solid",
        "Data as of nowcast date" = "solid",
        "Date of nowcast" = "dashed"
      ),
      breaks = c(
        "Final evaluation data",
        "Data as of nowcast date",
        "Date of nowcast"
      ),
      guide = guide_legend(
        override.aes = list(
          color = c(
            "Final evaluation data" = "red",
            "Data as of nowcast date" = "gray",
            "Date of nowcast" = "black"
          ),
          linewidth = 1
        )
      )
    ) +
    scale_fill_manual(
      name = "Model",
      values = plot_comps$model_colors
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
    ylab(glue::glue("ED visits")) +
    ggtitle(glue::glue("Nowcasted ED visits due to {pathogen_name}")) +
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
    p <- p + facet_wrap(~model)
  }


  return(p)
}
