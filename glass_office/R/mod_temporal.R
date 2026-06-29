# =============================================================================
# MODULE 2 - Temporal Anomaly
# =============================================================================

library(plotly)
library(ggplot2)
library(scales)
library(visNetwork)
library(igraph)

.TEMPORAL_RELEASE_PATTERN <- "civicloom|harborcrest|merger|embargo"

.TEMPORAL_METRICS <- c(
  n_msg = "Message volume",
  n_public = "Public posts",
  release_public = "Release-sensitive public posts",
  risk_share = "Risk share",
  public_share = "Public share",
  active_agents = "Active agents",
  net_sentiment = "Net sentiment"
)

.TEMPORAL_HEATMAP_MEASURES <- c(
  messages = "All messages",
  public = "Public posts",
  release = "Release-sensitive public posts",
  risk = "Risk-flagged messages"
)

.is_release_sensitive <- function(x) {
  !is.na(x) & grepl(.TEMPORAL_RELEASE_PATTERN, x, ignore.case = TRUE)
}

.is_pre_embargo <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%S") < "2046-06-05 18:00:00"
}

.fmt_time <- function(x) {
  format(x, "%b %d %H:%M")
}

.fmt_value <- function(x, metric) {
  if (!length(x)) return(character())
  if (metric %in% c("risk_share", "public_share", "release_share")) {
    out <- scales::percent(x, accuracy = 0.1)
    out[is.na(x)] <- "-"
    return(out)
  }
  out <- if (metric == "net_sentiment") round(x, 1) else round(x, 2)
  out[is.na(x)] <- "-"
  out
}

.plot_bar_width <- function(x) {
  if (dplyr::n_distinct(as.Date(x)) == 1) 60 * 35 else 60 * 60 * 8
}

.stat_card <- function(label, value, note = NULL, accent = "#171B4A") {
  tags$div(
    class = "glass-stat-card",
    style = paste0("border-top-color:", accent, ";"),
    tags$div(class = "glass-stat-value", value),
    tags$div(class = "glass-stat-label", label),
    if (!is.null(note)) tags$div(class = "glass-stat-note", note)
  )
}

.temporal_style <- tags$style(HTML("
  .glass-stat-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
    gap: 12px;
    margin-bottom: 12px;
  }
  .glass-stat-card {
    border: 1px solid #d7dde8;
    border-top: 5px solid #171B4A;
    border-radius: 8px;
    background: #fff;
    padding: 12px 14px;
    min-height: 102px;
  }
  .glass-stat-value {
    font-size: 24px;
    line-height: 1.05;
    font-weight: 800;
    color: #171B4A;
    overflow-wrap: anywhere;
  }
  .glass-stat-label {
    margin-top: 8px;
    font-size: 13px;
    font-weight: 700;
    color: #1f2937;
  }
  .glass-stat-note {
    margin-top: 5px;
    font-size: 12px;
    color: #64748b;
  }
  .glass-callout {
    border-left: 5px solid #287172;
    background: #f7fbfb;
    padding: 12px 14px;
    border-radius: 6px;
    margin-bottom: 10px;
  }
  .glass-callout h4 {
    margin: 0 0 6px 0;
    font-weight: 800;
  }
  .glass-small-muted {
    color: #64748b;
    font-size: 12px;
  }
  .plotly.html-widget-output[id$='-timeline'] {
    height: 430px !important;
    min-height: 430px !important;
    flex: 0 0 430px !important;
  }
  .plotly.html-widget-output[id$='-phase_plot'],
  .plotly.html-widget-output[id$='-channel_heatmap'] {
    height: 390px !important;
    min-height: 390px !important;
    flex: 0 0 390px !important;
  }
  .plotly.html-widget-output[id$='-cluster_heatmap'] {
    height: 440px !important;
    min-height: 440px !important;
    flex: 0 0 440px !important;
  }
  .visNetwork.html-widget-output[id$='-ego_network'] {
    height: 440px !important;
    min-height: 440px !important;
    flex: 0 0 440px !important;
  }
  .datatables.html-widget-output[id$='-events_tbl'],
  .datatables.html-widget-output[id$='-release_tbl'] {
    min-height: 340px !important;
    flex: 0 0 auto !important;
  }
  .datatables.html-widget-output[id$='-events_tbl'] table.dataTable td,
  .datatables.html-widget-output[id$='-release_tbl'] table.dataTable td {
    white-space: nowrap;
    max-width: 460px;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .shiny-html-output[id$='-summary_cards'],
  .shiny-html-output[id$='-temporal_reading'],
  .shiny-html-output[id$='-technique_rationale'] {
    min-height: 96px;
    flex: 0 0 auto !important;
  }
  @media (max-width: 760px) {
    .glass-stat-grid { grid-template-columns: 1fr; }
  }
"))

mod_temporal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    .temporal_style,
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 315,
        position = "left",
        h4("Temporal controls"),
        selectInput(ns("metric"), "Timeline metric",
                    choices = stats::setNames(names(.TEMPORAL_METRICS), .TEMPORAL_METRICS),
                    selected = "release_public"),
        radioButtons(
          ns("focus"), "Time focus",
          c("Shared filter range" = "filter", "Crisis day only" = "crisis"),
          selected = "filter"
        ),
        selectInput(ns("heatmap_measure"), "Agent-channel heatmap",
                    choices = stats::setNames(names(.TEMPORAL_HEATMAP_MEASURES), .TEMPORAL_HEATMAP_MEASURES),
                    selected = "release"),
        checkboxInput(ns("show_baseline"), "Show baseline threshold", TRUE),
        checkboxInput(ns("show_events"), "Overlay external / availability events", TRUE),
        checkboxInput(ns("show_release"), "Mark release-sensitive public posts", TRUE),
        sliderInput(ns("cluster_k"), "Round profile clusters",
                    min = 2, max = 5, value = 3, step = 1),
        uiOutput(ns("round_select_ui")),
        uiOutput(ns("ego_agent_ui")),
        hr(),
        div(
          class = "glass-small-muted",
          "Recommended workflow: find the spike in the timeline, compare it against baseline, inspect the agent-channel route, then verify exact public posts in the evidence table."
        )
      ),
      uiOutput(ns("summary_cards")),
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Forensic reading"),
          uiOutput(ns("temporal_reading"))
        ),
        card(
          card_header("Dual-resolution anomaly timeline"),
          plotlyOutput(ns("timeline"), height = "430px")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Baseline vs crisis contrast"),
          plotlyOutput(ns("phase_plot"), height = "390px")
        ),
        card(
          card_header("Agent x channel route heatmap"),
          plotlyOutput(ns("channel_heatmap"), height = "390px")
        )
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Round-behaviour clustering"),
          plotlyOutput(ns("cluster_heatmap"), height = "440px")
        ),
        card(
          card_header("Selected-round ego context"),
          visNetworkOutput(ns("ego_network"), height = "440px")
        )
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Environment event register"),
          DTOutput(ns("events_tbl"))
        ),
        card(
          card_header("Public release-sensitive evidence"),
          DTOutput(ns("release_tbl"))
        )
      ),
      card(
        card_header("Why these techniques fit this module"),
        tags$div(
          class = "glass-callout",
          tags$h4("Visual analytics rationale"),
          tags$p(
            "The module uses a timeline for event order, a baseline/crisis contrast for anomaly strength, an agent-channel heatmap for operational route discovery, round clustering for behaviour-profile comparison, and a selected-round ego graph for local relationship context. The evidence table keeps every aggregate claim traceable to exact public posts."
          )
        )
      )
    )
  )
}

mod_temporal_server <- function(id, filtered_data, opts) {
  moduleServer(id, function(input, output, session) {

    filtered_focus <- reactive({
      fd <- filtered_data()
      if (identical(input$focus, "crisis")) {
        fd <- fd |>
          dplyr::filter(as.Date(round_hour) == as.Date("2046-06-05"))
      }
      fd
    })

    round_metrics <- reactive({
      fd <- filtered_focus()
      if (!nrow(fd)) return(data.frame())

      prof <- fd |>
        dplyr::group_by(round_hour) |>
        dplyr::summarise(
          is_crisis_day = any(is_crisis_day, na.rm = TRUE),
          n_msg = dplyr::n(),
          n_public = sum(is_public, na.rm = TRUE),
          n_internal = sum(!is_public, na.rm = TRUE),
          release_public = sum(is_public & .is_release_sensitive(content), na.rm = TRUE),
          risk_flagged = sum(risk_flag, na.rm = TRUE),
          risk_share = mean(risk_flag, na.rm = TRUE),
          public_share = mean(is_public, na.rm = TRUE),
          active_agents = dplyr::n_distinct(agent_label),
          active_channels = dplyr::n_distinct(channel),
          .groups = "drop"
        ) |>
        dplyr::arrange(round_hour)

      sent <- if (exists("sentiment_by_round", inherits = TRUE)) {
        sentiment_by_round |> dplyr::select(round_hour, net_sentiment)
      } else {
        round_profile |> dplyr::select(round_hour, net_sentiment)
      }

      prof |>
        dplyr::left_join(sent, by = "round_hour") |>
        dplyr::mutate(
          dplyr::across(
            c(risk_share, public_share, net_sentiment),
            ~ tidyr::replace_na(.x, 0)
          )
        )
    })

    full_round_metrics <- reactive({
      fd <- filtered_data()
      if (!nrow(fd)) return(data.frame())

      fd |>
        dplyr::group_by(round_hour) |>
        dplyr::summarise(
          is_crisis_day = any(is_crisis_day, na.rm = TRUE),
          n_msg = dplyr::n(),
          n_public = sum(is_public, na.rm = TRUE),
          release_public = sum(is_public & .is_release_sensitive(content), na.rm = TRUE),
          risk_flagged = sum(risk_flag, na.rm = TRUE),
          risk_share = mean(risk_flag, na.rm = TRUE),
          public_share = mean(is_public, na.rm = TRUE),
          active_agents = dplyr::n_distinct(agent_label),
          active_channels = dplyr::n_distinct(channel),
          .groups = "drop"
        ) |>
        dplyr::left_join(
          sentiment_by_round |> dplyr::select(round_hour, net_sentiment),
          by = "round_hour"
        ) |>
        dplyr::mutate(
          dplyr::across(
            c(risk_share, public_share, net_sentiment),
            ~ tidyr::replace_na(.x, 0)
          )
        ) |>
        dplyr::arrange(round_hour)
    })

    release_posts <- reactive({
      fd <- filtered_focus()
      if (!nrow(fd)) return(fd[0, , drop = FALSE])
      fd |>
        dplyr::filter(is_public, .is_release_sensitive(content)) |>
        dplyr::arrange(timestamp)
    })

    events_in_range <- reactive({
      prof <- round_metrics()
      if (!nrow(prof)) return(env_events_long[0, , drop = FALSE])
      ev <- env_events_long |>
        dplyr::filter(round_hour %in% prof$round_hour)

      if (exists("rounds_env", inherits = TRUE)) {
        ev <- ev |>
          dplyr::left_join(
            rounds_env |>
              dplyr::select(round_hour, event_headline, stock_price,
                            percent_change, mkt_sentiment),
            by = "round_hour"
          )
      }
      ev |> dplyr::arrange(round_hour, event_type)
    })

    temporal_summary <- reactive({
      fd <- filtered_data()
      prof <- full_round_metrics()
      if (!nrow(fd) || !nrow(prof)) return(NULL)

      rel <- fd |>
        dplyr::filter(is_public, .is_release_sensitive(content)) |>
        dplyr::arrange(timestamp)
      pre_rel <- rel |> dplyr::filter(.is_pre_embargo(timestamp))

      baseline <- prof |> dplyr::filter(!is_crisis_day)
      crisis <- prof |> dplyr::filter(is_crisis_day)

      baseline_mu <- if (nrow(baseline)) mean(baseline$release_public, na.rm = TRUE) else NA_real_
      baseline_sd <- if (nrow(baseline)) stats::sd(baseline$release_public, na.rm = TRUE) else NA_real_
      crisis_max <- if (nrow(crisis)) max(crisis$release_public, na.rm = TRUE) else NA_real_
      z_score <- if (!is.na(baseline_sd) && baseline_sd > 0) {
        (crisis_max - baseline_mu) / baseline_sd
      } else if (!is.na(crisis_max) && crisis_max > 0) {
        Inf
      } else {
        NA_real_
      }

      peak <- if (nrow(prof)) prof[which.max(prof$release_public), , drop = FALSE] else prof
      authors <- rel |> dplyr::count(agent_label, channel, sort = TRUE, name = "Posts")

      list(
        release_all = rel,
        pre_release = pre_rel,
        baseline = baseline,
        crisis = crisis,
        peak = peak,
        authors = authors,
        z_score = z_score
      )
    })

    output$round_select_ui <- renderUI({
      prof <- round_metrics()
      if (!nrow(prof)) return(NULL)
      choices <- stats::setNames(
        as.character(prof$round_hour),
        paste0(.fmt_time(prof$round_hour), " | release=", prof$release_public)
      )
      default <- as.character(prof$round_hour[which.max(prof$release_public)])
      selectInput(session$ns("selected_round"), "Ego context round",
                  choices = choices, selected = default)
    })

    output$ego_agent_ui <- renderUI({
      fd <- filtered_focus()
      agents <- sort(unique(fd$agent_label))
      if (!length(agents)) return(NULL)
      selectInput(session$ns("ego_agent"), "Ego context agent",
                  choices = c("Most active in selected round" = "__auto__", agents),
                  selected = "__auto__")
    })

    output$summary_cards <- renderUI({
      s <- temporal_summary()
      if (is.null(s)) {
        return(div(class = "text-muted", "No messages in the current selection."))
      }

      first_pre <- if (nrow(s$pre_release)) .fmt_time(s$pre_release$timestamp[1]) else "-"
      first_agent <- if (nrow(s$pre_release)) s$pre_release$agent_label[1] else "No pre-embargo public release"
      peak_val <- if (nrow(s$peak)) s$peak$release_public[1] else 0
      peak_note <- if (nrow(s$peak)) .fmt_time(s$peak$round_hour[1]) else "-"
      z_note <- if (is.infinite(s$z_score)) "baseline has zero release posts" else "vs baseline"
      z_value <- if (is.infinite(s$z_score)) "break" else if (is.na(s$z_score)) "-" else paste0(round(s$z_score, 1), " SD")

      tags$div(
        class = "glass-stat-grid",
        .stat_card("Pre-embargo release posts", nrow(s$pre_release), first_pre, "#B6403C"),
        .stat_card("Peak release hour", peak_val, peak_note, "#B6403C"),
        .stat_card("Dominant route", if (nrow(s$authors)) s$authors$agent_label[1] else "-",
                   if (nrow(s$authors)) paste(s$authors$channel[1], s$authors$Posts[1], "posts") else NULL,
                   "#287172"),
        .stat_card("Baseline departure", z_value, z_note, "#D5A536")
      )
    })

    output$temporal_reading <- renderUI({
      s <- temporal_summary()
      if (is.null(s)) {
        return(div(class = "text-muted", "No messages in the current selection."))
      }

      first_pre <- if (nrow(s$pre_release)) {
        paste0(.fmt_time(s$pre_release$timestamp[1]), " by ",
               s$pre_release$agent_label[1], " through ",
               s$pre_release$channel[1])
      } else {
        "No pre-embargo release-sensitive public post in this filter"
      }

      route <- if (nrow(s$authors)) {
        paste0(s$authors$agent_label, " via ", s$authors$channel,
               " (", s$authors$Posts, ")", collapse = "; ")
      } else {
        "No public release-sensitive route in this filter"
      }

      departure <- if (is.infinite(s$z_score)) {
        "a structural break: baseline release count is zero, crisis release count is positive"
      } else if (is.na(s$z_score)) {
        "not computable for this filter"
      } else {
        paste0(round(s$z_score, 2), " standard deviations above baseline")
      }

      tags$div(
        tags$p(
          tags$b("Interpretation. "),
          "The most important temporal signal is not simple message volume. It is the sudden appearance of deal-specific public language before the 6:00 PM embargo lift, concentrated in the final crisis-hour rounds."
        ),
        tags$ul(
          tags$li(tags$b("First pre-embargo public disclosure: "), first_pre, "."),
          tags$li(tags$b("Operational public route: "), route, "."),
          tags$li(tags$b("Baseline comparison: "), departure, "."),
          tags$li(tags$b("Analytical use: "),
                  "click and filter the timeline, heatmap, round cluster profile, and evidence rows to move from anomaly to exact posts.")
        )
      )
    })

    output$timeline <- renderPlotly({
      plot_df <- round_metrics()
      validate(need(nrow(plot_df) > 0, "No messages in the current selection."))

      metric <- input$metric
      plot_df$value <- plot_df[[metric]]
      plot_df$Phase <- ifelse(plot_df$is_crisis_day, "Crisis day", "Baseline")
      plot_df$tooltip <- paste0(
        .fmt_time(plot_df$round_hour),
        "<br>", .TEMPORAL_METRICS[[metric]], ": ", .fmt_value(plot_df$value, metric),
        "<br>messages: ", plot_df$n_msg,
        "<br>public posts: ", plot_df$n_public,
        "<br>release-sensitive public posts: ", plot_df$release_public,
        "<br>active agents: ", plot_df$active_agents
      )
      plot_df$round_label <- .fmt_time(plot_df$round_hour)
      x_levels <- plot_df$round_label

      p <- plot_ly() |>
        add_bars(
          data = plot_df,
          x = ~round_label,
          y = ~value,
          color = ~Phase,
          colors = c("Baseline" = "#6BAED6", "Crisis day" = "#E15759"),
          text = ~tooltip,
          hoverinfo = "text",
          marker = list(line = list(color = "white", width = 1)),
          opacity = 0.78
        ) |>
        add_trace(
          data = plot_df,
          x = ~round_label,
          y = ~value,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#273447", width = 2),
          marker = list(color = "#273447", size = 7),
          text = ~tooltip,
          hoverinfo = "text",
          showlegend = FALSE,
          inherit = FALSE
        )

      if (isTRUE(input$show_baseline)) {
        base_vals <- full_round_metrics() |>
          dplyr::filter(!is_crisis_day) |>
          dplyr::pull(dplyr::all_of(metric))
        if (length(base_vals) && any(!is.na(base_vals))) {
          mu <- mean(base_vals, na.rm = TRUE)
          sig <- stats::sd(base_vals, na.rm = TRUE)
          limit <- if (!is.na(sig) && sig > 0) mu + 2 * sig else mu
          p <- p |>
            add_trace(
              x = x_levels,
              y = rep(limit, length(x_levels)),
              type = "scatter",
              mode = "lines",
              line = list(color = "#111827", width = 1.2, dash = "dot"),
              hoverinfo = "skip",
              name = "Baseline +2sd",
              inherit = FALSE
            )
        }
      }

      if (metric == "net_sentiment") {
        p <- p |>
          add_trace(
            x = x_levels,
            y = rep(0, length(x_levels)),
            type = "scatter",
            mode = "lines",
            line = list(color = "#64748b", width = 1.2, dash = "dot"),
            hoverinfo = "skip",
            name = "Neutral sentiment",
            inherit = FALSE
          )
      }

      y_anchor <- max(plot_df$value, na.rm = TRUE)
      if (!is.finite(y_anchor) || y_anchor <= 0) y_anchor <- 1

      if (isTRUE(input$show_release) && nrow(release_posts())) {
        rel <- release_posts() |>
          dplyr::count(round_hour, name = "Posts") |>
          dplyr::mutate(
            round_label = .fmt_time(round_hour),
            y = y_anchor * 1.08,
            tooltip = paste0(.fmt_time(round_hour), "<br>release posts: ", Posts)
          )
        p <- p |>
          add_markers(
            data = rel,
            x = ~round_label,
            y = ~y,
            marker = list(symbol = "triangle-up", size = 12,
                          color = "#B6403C", line = list(color = "#7A1F1F", width = 1)),
            text = ~tooltip,
            hoverinfo = "text",
            name = "Release markers",
            inherit = FALSE
          )
      }

      if (isTRUE(input$show_events) && isTRUE(opts()$avail_overlay) && nrow(events_in_range())) {
        ev <- events_in_range() |>
          dplyr::count(round_hour, event_type, name = "Events") |>
          dplyr::mutate(
            round_label = .fmt_time(round_hour),
            y = y_anchor * 1.18,
            tooltip = paste0(.fmt_time(round_hour),
                             "<br>events: ", event_type,
                             "<br>count: ", Events)
          )
        p <- p |>
          add_markers(
            data = ev,
            x = ~round_label,
            y = ~y,
            marker = list(symbol = "circle", size = 10,
                          color = "#D5A536", line = list(color = "#8A6A00", width = 1)),
            text = ~tooltip,
            hoverinfo = "text",
            name = "Event markers",
            inherit = FALSE
          )
      }

      yaxis <- list(title = .TEMPORAL_METRICS[[metric]])
      if (metric %in% c("risk_share", "public_share")) {
        yaxis$tickformat <- ".0%"
      }

      p |>
        layout(
          barmode = "overlay",
          yaxis = yaxis,
          xaxis = list(title = "", tickangle = -35,
                       type = "category",
                       categoryorder = "array",
                       categoryarray = x_levels),
          legend = list(orientation = "h", y = 1.08),
          margin = list(l = 65, r = 25, t = 15, b = 85)
        )
    })

    output$phase_plot <- renderPlotly({
      fd <- filtered_data()
      validate(need(nrow(fd) > 0, "No messages in the current selection."))

      phase <- fd |>
        dplyr::mutate(
          Phase = ifelse(is_crisis_day, "Crisis day", "Baseline"),
          release_sensitive = is_public & .is_release_sensitive(content)
        ) |>
        dplyr::group_by(Phase) |>
        dplyr::summarise(
          Rounds = dplyr::n_distinct(round_hour),
          `Messages / round` = dplyr::n() / Rounds,
          `Public posts / round` = sum(is_public, na.rm = TRUE) / Rounds,
          `Release posts / round` = sum(release_sensitive, na.rm = TRUE) / Rounds,
          `Risk share` = mean(risk_flag, na.rm = TRUE),
          `Active agents / round` = dplyr::n_distinct(agent_label) / Rounds,
          .groups = "drop"
        ) |>
        tidyr::pivot_longer(-Phase, names_to = "Metric", values_to = "Value")

      phase$tooltip <- paste0(phase$Phase, "<br>", phase$Metric, ": ", round(phase$Value, 3))
      plot_ly(
        phase,
        x = ~Value,
        y = ~Metric,
        color = ~Phase,
        colors = c("Baseline" = "#6BAED6", "Crisis day" = "#E15759"),
        type = "bar",
        orientation = "h",
        text = ~tooltip,
        hoverinfo = "text",
        opacity = 0.84
      ) |>
        layout(
          barmode = "group",
          xaxis = list(title = "Per-round value / share"),
          yaxis = list(title = "", automargin = TRUE),
          legend = list(orientation = "h", y = 1.08),
          margin = list(l = 165, r = 20, t = 10, b = 45)
        )
    })

    output$channel_heatmap <- renderPlotly({
      fd <- filtered_focus()
      validate(need(nrow(fd) > 0, "No messages in the current selection."))

      measure <- input$heatmap_measure
      fd$value <- if (identical(measure, "public")) {
        as.integer(fd$is_public)
      } else if (identical(measure, "release")) {
        as.integer(fd$is_public & .is_release_sensitive(fd$content))
      } else if (identical(measure, "risk")) {
        as.integer(fd$risk_flag)
      } else {
        rep(1L, nrow(fd))
      }

      hm <- fd |>
        dplyr::group_by(agent_label, channel) |>
        dplyr::summarise(Value = sum(value, na.rm = TRUE), .groups = "drop") |>
        tidyr::complete(agent_label, channel, fill = list(Value = 0))

      label <- .TEMPORAL_HEATMAP_MEASURES[[measure]]
      mat <- xtabs(Value ~ agent_label + channel, data = hm)
      text_mat <- matrix(
        paste0(rep(rownames(mat), times = ncol(mat)),
               "<br>", rep(colnames(mat), each = nrow(mat)),
               "<br>", label, ": ", as.vector(mat)),
        nrow = nrow(mat),
        ncol = ncol(mat)
      )

      plot_ly(
        x = colnames(mat),
        y = rownames(mat),
        z = mat,
        type = "heatmap",
        colorscale = list(c(0, "#F8FAFC"), c(1, "#B6403C")),
        text = text_mat,
        hoverinfo = "text",
        colorbar = list(title = label)
      ) |>
        layout(
          xaxis = list(title = "", tickangle = -35),
          yaxis = list(title = "", automargin = TRUE),
          margin = list(l = 135, r = 15, t = 10, b = 85)
        )
    })

    output$cluster_heatmap <- renderPlotly({
      prof <- full_round_metrics()
      validate(need(nrow(prof) > 2, "At least three rounds are needed for round clustering."))

      features <- prof |>
        dplyr::select(round_hour, is_crisis_day, n_msg, n_public, release_public,
                      risk_share, public_share, active_agents, active_channels,
                      net_sentiment)
      numeric_cols <- c("n_msg", "n_public", "release_public", "risk_share",
                        "public_share", "active_agents", "active_channels",
                        "net_sentiment")

      mat <- as.matrix(features[, numeric_cols, drop = FALSE])
      z <- scale(mat)
      z[is.na(z)] <- 0

      k <- min(input$cluster_k, nrow(z))
      hc <- stats::hclust(stats::dist(z))
      clusters <- stats::cutree(hc, k = k)
      order_idx <- hc$order

      round_labels <- paste0(.fmt_time(features$round_hour[order_idx]),
                             ifelse(features$is_crisis_day[order_idx], " | crisis", " | baseline"))
      feature_labels <- rev(numeric_cols)
      z_heat <- t(z[order_idx, feature_labels, drop = FALSE])
      text_heat <- matrix(
        paste0(
          rep(round_labels, each = length(feature_labels)),
          "<br>", rep(feature_labels, times = length(round_labels)),
          "<br>standardized value: ", round(as.vector(z_heat), 2),
          "<br>Cluster ", rep(clusters[order_idx], each = length(feature_labels))
        ),
        nrow = length(feature_labels),
        ncol = length(round_labels)
      )

      plot_ly(
        x = round_labels,
        y = feature_labels,
        z = z_heat,
        type = "heatmap",
        colorscale = list(c(0, "#2B6CB0"), c(0.5, "#F8FAFC"), c(1, "#B6403C")),
        zmid = 0,
        text = text_heat,
        hoverinfo = "text",
        colorbar = list(title = "Z-score")
      ) |>
        layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "", automargin = TRUE),
          margin = list(l = 125, r = 20, t = 10, b = 145)
        )
    })

    output$ego_network <- renderVisNetwork({
      fd <- filtered_focus()
      validate(need(nrow(fd) > 0, "No messages in the current selection."))

      selected_round <- input$selected_round
      if (is.null(selected_round) || !nzchar(selected_round)) {
        selected_round <- as.character(round_metrics()$round_hour[which.max(round_metrics()$release_public)])
      }
      round_time <- as.POSIXct(selected_round, tz = "UTC")
      round_df <- fd |> dplyr::filter(round_hour == round_time)
      validate(need(nrow(round_df) > 0, "No messages for the selected round."))

      ego <- input$ego_agent
      if (is.null(ego) || identical(ego, "__auto__")) {
        ego <- round_df |> dplyr::count(agent_label, sort = TRUE) |> dplyr::slice(1) |> dplyr::pull(agent_label)
      }

      rec <- recipient_edges |>
        dplyr::left_join(communications |> dplyr::select(message_id, round_hour),
                         by = "message_id") |>
        dplyr::filter(round_hour == round_time, to_recipient != "ALL") |>
        dplyr::transmute(from = from_agent, to = to_recipient, channel, message_type)

      replies <- round_df |>
        dplyr::filter(!is.na(responding_to), nzchar(responding_to)) |>
        dplyr::select(message_id, from = agent_label, responding_to) |>
        dplyr::left_join(
          communications |> dplyr::select(responding_to = message_id, to = agent_label),
          by = "responding_to"
        ) |>
        dplyr::filter(!is.na(to), from != to) |>
        dplyr::mutate(channel = "reply", message_type = "reply") |>
        dplyr::select(from, to, channel, message_type)

      edges <- dplyr::bind_rows(rec, replies) |>
        dplyr::filter(!is.na(from), !is.na(to), from != to)

      if (nrow(edges)) {
        keep <- unique(c(ego, edges$to[edges$from == ego], edges$from[edges$to == ego]))
        edges <- edges |> dplyr::filter(from %in% keep, to %in% keep)
      }

      if (!nrow(edges)) {
        nodes <- data.frame(id = unique(round_df$agent_label),
                            label = unique(round_df$agent_label),
                            group = ifelse(unique(round_df$agent_label) == ego, "selected", "active"),
                            value = 10)
        return(
          visNetwork(nodes, data.frame()) |>
            visNodes(shape = "dot") |>
            visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
        )
      }

      edge_df <- edges |>
        dplyr::count(from, to, channel, name = "weight") |>
        dplyr::mutate(
          arrows = "to",
          title = paste0(from, " -> ", to, "<br>", channel, "<br>messages: ", weight),
          width = pmax(1, weight)
        )

      node_ids <- sort(unique(c(edge_df$from, edge_df$to)))
      node_stats <- round_df |> dplyr::count(agent_label, name = "messages")
      nodes <- data.frame(id = node_ids, label = node_ids, stringsAsFactors = FALSE) |>
        dplyr::left_join(node_stats, by = c("id" = "agent_label")) |>
        dplyr::mutate(
          messages = tidyr::replace_na(messages, 0L),
          group = dplyr::case_when(
            id == ego ~ "selected ego",
            id %in% edge_df$from ~ "sender",
            TRUE ~ "recipient"
          ),
          value = pmax(12, messages * 2),
          title = paste0(id, "<br>messages in round: ", messages)
        )

      visNetwork(nodes, edge_df, width = "100%", height = "100%") |>
        visNodes(shape = "dot", font = list(size = 18)) |>
        visEdges(smooth = list(enabled = TRUE, type = "curvedCW")) |>
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1),
                   nodesIdSelection = TRUE) |>
        visGroups(groupname = "selected ego", color = list(background = "#B6403C", border = "#7A1F1F")) |>
        visGroups(groupname = "sender", color = list(background = "#287172", border = "#1F5556")) |>
        visGroups(groupname = "recipient", color = list(background = "#6BAED6", border = "#2B6CB0")) |>
        visIgraphLayout(layout = "layout_with_fr") |>
        visLegend() |>
        visLayout(randomSeed = 608)
    })

    output$events_tbl <- renderDT({
      ev <- events_in_range() |>
        dplyr::transmute(
          Round = .fmt_time(round_hour),
          Type = event_type,
          Event = event_text,
          Headline = event_headline,
          Market = paste(stock_price, percent_change, mkt_sentiment)
        )

      DT::datatable(ev, rownames = FALSE, selection = "single",
                    options = list(pageLength = 7, scrollX = TRUE,
                                   scrollY = "300px"))
    })

    output$release_tbl <- renderDT({
      rel <- release_posts() |>
        dplyr::transmute(
          Time = .fmt_time(timestamp),
          Phase = ifelse(.is_pre_embargo(timestamp), "Pre-embargo", "Post-embargo"),
          Author = agent_label,
          Channel = channel,
          `Message ID` = message_id,
          Content = content
        )

      DT::datatable(rel, rownames = FALSE, selection = "single",
                    options = list(pageLength = 7, scrollX = TRUE,
                                   scrollY = "300px")) |>
        DT::formatStyle("Phase", target = "row",
                        backgroundColor = DT::styleEqual("Pre-embargo", "#FDECEC"))
    })
  })
}
