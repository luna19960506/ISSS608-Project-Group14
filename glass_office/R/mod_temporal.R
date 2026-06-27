# =============================================================================
# MODULE 2 — Temporal Anomaly           OWNER: Cheng Yuanyuan
# -----------------------------------------------------------------------------
# Only Cheng Yuanyuan edits this file. Do not change the function signatures:
#   mod_temporal_ui(id)
#   mod_temporal_server(id, filtered_data, opts)
#
# Inputs you receive:
#   filtered_data()  reactive -> filtered communications master table
#   opts()           reactive -> list(avail_overlay, public_only,
#                                      time_start, time_end)
#
# Global clean tables you may read directly (defined in app.R):
#   communications, round_profile, rounds_env, env_events_long
#
# What this module delivers (from the proposal):
#   - Interactive timeline (plotly): zoomable, hover-for-value, chosen metric,
#     full two-week view vs hourly drill-down on the crisis day (5 Jun).
#   - Baseline-vs-crisis comparison: makes the departure from normal explicit.
#   - Agent x channel heatmap: who used which channel when.
#   - Environment event overlay (toggle): media events / investigations /
#     agent-unavailability markers — honour opts()$avail_overlay too.
#
# Tip — summarise the filtered slice per round_hour:
#   fd <- filtered_data()
#   prof <- fd |>
#     dplyr::group_by(round_hour) |>
#     dplyr::summarise(n_msg = dplyr::n(),
#                      n_public = sum(is_public),
#                      risk_share = mean(risk_flag, na.rm = TRUE),
#                      .groups = "drop")
#   # overlay events by joining env_events_long on round_hour.
# =============================================================================

library(plotly)
library(ggplot2)
library(scales)

.TEMPORAL_METRICS <- c(
  n_msg = "Message volume",
  n_public = "Public posts",
  release_public = "Release-sensitive public posts",
  risk_share = "Risk share",
  net_sentiment = "Net sentiment"
)

.RELEASE_PATTERN <- "civicloom|harborcrest|merger|embargo"

.is_release_sensitive <- function(x) {
  !is.na(x) & grepl(.RELEASE_PATTERN, x, ignore.case = TRUE)
}

.is_pre_embargo <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%S") < "2046-06-05 18:00:00"
}

.fmt_temporal_value <- function(x, metric) {
  if (length(x) == 0 || is.na(x)) return("—")
  if (metric == "risk_share") return(scales::percent(x, accuracy = 0.1))
  if (metric == "net_sentiment") return(round(x, 1))
  round(x, 2)
}

mod_temporal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 260, position = "right",
        selectInput(ns("metric"), "Metric",
                    c("Message volume" = "n_msg",
                      "Public posts" = "n_public",
                      "Release-sensitive public posts" = "release_public",
                      "Risk share" = "risk_share",
                      "Net sentiment" = "net_sentiment")),
        radioButtons(ns("focus"), "Focus",
                     c("Current shared filter" = "filter",
                       "Crisis day only" = "crisis"),
                     selected = "filter"),
        checkboxInput(ns("show_events"), "Overlay environment events", value = TRUE),
        checkboxInput(ns("show_release"), "Mark release-sensitive posts", value = TRUE),
        hr(),
        helpText("Release-sensitive posts are public messages mentioning CivicLoom, HarborCrest, merger, or embargo.")
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Temporal anomaly timeline"),
          plotlyOutput(ns("timeline"), height = "390px")
        ),
        card(
          card_header("Baseline vs crisis contrast"),
          DTOutput(ns("phase_tbl"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Agent x channel heatmap"),
          plotlyOutput(ns("channel_heatmap"), height = "360px")
        ),
        card(
          card_header("Environment event register"),
          DTOutput(ns("events_tbl"))
        )
      ),
      card(
        card_header("Public release-sensitive evidence"),
        DTOutput(ns("release_tbl"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Computed forensic reading"),
          uiOutput(ns("temporal_reading"))
        ),
        card(
          card_header("Anomaly scorecard"),
          DTOutput(ns("anomaly_tbl"))
        )
      ),
      card(
        card_header("Technical method and visual rationale"),
        tags$p(tags$b("Purpose. "),
               "This module tests whether public release behaviour on 5 June is typical background posting or a measurable break from the pre-crisis communication regime."),
        tags$ul(
          tags$li(tags$b("Cleaning and temporal alignment: "),
                  "message-level records are aggregated by round_hour, the only shared key guaranteed across communications, sentiment, and environment tables. This avoids accidental joins on per-table row order or inconsistent round indexes."),
          tags$li(tags$b("Derived breach indicator: "),
                  "release-sensitive public posts are identified from public-channel content containing CivicLoom, HarborCrest, merger, or embargo. This is necessary because the prepared risk_flag captures general risk language but does not label the actual pre-embargo disclosure posts."),
          tags$li(tags$b("Anomaly method: "),
                  "baseline rounds before 5 June define the normal range for the selected metric. Crisis-day values are compared against that baseline using mean, maximum, and a z-style departure score where baseline variance exists; if baseline variance is zero, any positive release count is treated as a structural break rather than a noisy fluctuation."),
          tags$li(tags$b("Visual analytics fit: "),
                  "the timeline reconstructs event order, the baseline/crisis scorecard quantifies departure from normal behaviour, the agent-channel heatmap identifies the operational route into public channels, the event register overlays external pressure, and the evidence table keeps every aggregate claim traceable to exact public posts.")
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
      if (!nrow(fd)) {
        return(data.frame())
      }

      prof <- fd |>
        dplyr::group_by(round_hour) |>
        dplyr::summarise(
          n_msg = dplyr::n(),
          n_public = sum(is_public, na.rm = TRUE),
          n_internal = sum(!is_public, na.rm = TRUE),
          release_public = sum(is_public & .is_release_sensitive(content), na.rm = TRUE),
          risk_share = mean(risk_flag, na.rm = TRUE),
          public_share = mean(is_public, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(round_hour)

      sentiment <- if (exists("sentiment_by_round", inherits = TRUE)) {
        sentiment_by_round |>
          dplyr::select(round_hour, net_sentiment)
      } else {
        round_profile |>
          dplyr::select(round_hour, net_sentiment)
      }

      prof |>
        dplyr::left_join(sentiment, by = "round_hour") |>
        dplyr::mutate(
          risk_share = dplyr::if_else(is.nan(risk_share), 0, risk_share),
          public_share = dplyr::if_else(is.nan(public_share), 0, public_share),
          net_sentiment = tidyr::replace_na(net_sentiment, 0)
        )
    })

    release_posts <- reactive({
      fd <- filtered_focus()
      if (!nrow(fd)) {
        return(fd[0, , drop = FALSE])
      }

      fd |>
        dplyr::filter(is_public, .is_release_sensitive(content)) |>
        dplyr::arrange(timestamp)
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
          risk_share = mean(risk_flag, na.rm = TRUE),
          public_share = mean(is_public, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::left_join(
          sentiment_by_round |>
            dplyr::select(round_hour, net_sentiment),
          by = "round_hour"
        ) |>
        dplyr::mutate(
          risk_share = dplyr::if_else(is.nan(risk_share), 0, risk_share),
          public_share = dplyr::if_else(is.nan(public_share), 0, public_share),
          net_sentiment = tidyr::replace_na(net_sentiment, 0)
        ) |>
        dplyr::arrange(round_hour)
    })

    temporal_summary <- reactive({
      fd <- filtered_data()
      prof <- full_round_metrics()
      if (!nrow(fd) || !nrow(prof)) return(NULL)

      release_all <- fd |>
        dplyr::filter(is_public, .is_release_sensitive(content)) |>
        dplyr::arrange(timestamp)
      pre_release <- release_all |>
        dplyr::filter(.is_pre_embargo(timestamp))

      baseline <- prof |>
        dplyr::filter(!is_crisis_day)
      crisis <- prof |>
        dplyr::filter(is_crisis_day)

      baseline_release_round_max <- if (nrow(baseline)) max(baseline$release_public, na.rm = TRUE) else NA_real_
      crisis_release_round_max <- if (nrow(crisis)) max(crisis$release_public, na.rm = TRUE) else NA_real_
      baseline_mu <- if (nrow(baseline)) mean(baseline$release_public, na.rm = TRUE) else NA_real_
      baseline_sd <- if (nrow(baseline)) stats::sd(baseline$release_public, na.rm = TRUE) else NA_real_
      z_score <- if (!is.na(baseline_sd) && baseline_sd > 0) {
        (crisis_release_round_max - baseline_mu) / baseline_sd
      } else if (!is.na(crisis_release_round_max) && crisis_release_round_max > 0) {
        Inf
      } else {
        NA_real_
      }

      peak <- if (nrow(prof)) prof[which.max(prof$release_public), , drop = FALSE] else prof
      top_authors <- release_all |>
        dplyr::count(agent_label, sort = TRUE, name = "Posts") |>
        dplyr::slice_head(n = 3)

      list(
        release_all = release_all,
        pre_release = pre_release,
        baseline = baseline,
        crisis = crisis,
        peak = peak,
        top_authors = top_authors,
        baseline_release_round_max = baseline_release_round_max,
        crisis_release_round_max = crisis_release_round_max,
        baseline_mu = baseline_mu,
        baseline_sd = baseline_sd,
        z_score = z_score
      )
    })

    events_in_range <- reactive({
      prof <- round_metrics()
      if (!nrow(prof)) {
        return(
          env_events_long[0, , drop = FALSE] |>
            dplyr::left_join(
              rounds_env |>
                dplyr::select(round_hour, event_headline, social_state,
                              stock_price, percent_change, mkt_sentiment),
              by = "round_hour"
            )
        )
      }

      env_events_long |>
        dplyr::filter(round_hour %in% prof$round_hour) |>
        dplyr::left_join(
          rounds_env |>
            dplyr::select(round_hour, event_headline, social_state,
                          stock_price, percent_change, mkt_sentiment),
          by = "round_hour"
        ) |>
        dplyr::arrange(round_hour, event_type)
    })

    output$timeline <- renderPlotly({
      prof <- round_metrics()
      validate(need(nrow(prof) > 0, "No messages in the current selection."))

      metric <- input$metric
      plot_df <- prof
      plot_df$value <- plot_df[[metric]]
      bar_width <- if (dplyr::n_distinct(as.Date(plot_df$round_hour)) == 1) {
        60 * 35
      } else {
        60 * 60 * 8
      }
      plot_df$tooltip <- paste0(
        format(plot_df$round_hour, "%b %d %H:%M"),
        "<br>", .TEMPORAL_METRICS[[metric]], ": ",
        if (metric == "risk_share") scales::percent(plot_df$value, accuracy = 0.1)
        else plot_df$value,
        "<br>messages: ", plot_df$n_msg,
        "<br>public posts: ", plot_df$n_public,
        "<br>release-sensitive public posts: ", plot_df$release_public
      )

      fill_col <- if (metric == "release_public") "#E15759" else "#4E79A7"
      p <- ggplot(plot_df, aes(x = round_hour, y = value, text = tooltip)) +
        geom_col(width = bar_width, fill = fill_col, alpha = 0.78) +
        geom_line(color = "#2F2F2F", linewidth = 0.55) +
        geom_point(color = "#2F2F2F", size = 1.8) +
        labs(x = NULL, y = .TEMPORAL_METRICS[[metric]]) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 35, hjust = 1))

      if (metric == "risk_share") {
        p <- p + scale_y_continuous(labels = scales::label_percent(accuracy = 1))
      }
      if (metric == "net_sentiment") {
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "#999999")
      }

      baseline_vals <- full_round_metrics() |>
        dplyr::filter(!is_crisis_day) |>
        dplyr::pull(dplyr::all_of(metric))
      if (length(baseline_vals) && any(!is.na(baseline_vals))) {
        baseline_mean <- mean(baseline_vals, na.rm = TRUE)
        baseline_sd <- stats::sd(baseline_vals, na.rm = TRUE)
        baseline_limit <- if (!is.na(baseline_sd) && baseline_sd > 0) {
          baseline_mean + 2 * baseline_sd
        } else {
          baseline_mean
        }
        p <- p +
          geom_hline(yintercept = baseline_limit,
                     linetype = "dotted", color = "#7A869A",
                     linewidth = 0.55)
      }

      y_anchor <- max(plot_df$value, na.rm = TRUE)
      if (!is.finite(y_anchor) || y_anchor == 0) y_anchor <- 1

      if (isTRUE(input$show_release) && nrow(release_posts())) {
        rel <- release_posts() |>
          dplyr::group_by(round_hour) |>
          dplyr::summarise(
            posts = dplyr::n(),
            authors = paste(sort(unique(agent_label)), collapse = ", "),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            y = y_anchor * 1.08,
            tooltip = paste0(format(round_hour, "%b %d %H:%M"),
                             "<br>release-sensitive public posts: ", posts,
                             "<br>authors: ", authors)
        )
        p <- p +
          geom_point(data = rel, aes(x = round_hour, y = y),
                     inherit.aes = FALSE, shape = 24, size = 3.4,
                     fill = "#E15759", color = "#7A1F1F")
      }

      show_events <- isTRUE(input$show_events) && isTRUE(opts()$avail_overlay)
      if (show_events && nrow(events_in_range())) {
        ev <- events_in_range() |>
          dplyr::group_by(round_hour) |>
          dplyr::summarise(
            events = paste(unique(event_type), collapse = ", "),
            details = paste(unique(event_text), collapse = " | "),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            y = y_anchor * 1.18,
            tooltip = paste0(format(round_hour, "%b %d %H:%M"),
                             "<br>events: ", events,
                             "<br>", details)
        )
        p <- p +
          geom_point(data = ev, aes(x = round_hour, y = y),
                     inherit.aes = FALSE, shape = 21, size = 3.2,
                     fill = "#F28E2B", color = "#8A4A00")
      }

      ggplotly(p, tooltip = "text") |>
        layout(legend = list(orientation = "h"),
               margin = list(l = 55, r = 20, t = 10, b = 85))
    })

    output$phase_tbl <- renderDT({
      fd <- filtered_focus()
      if (!nrow(fd)) return(datatable(data.frame()))

      phase <- fd |>
        dplyr::mutate(
          Phase = dplyr::if_else(is_crisis_day,
                                 "Crisis day (5 Jun)",
                                 "Baseline rounds"),
          release_sensitive = is_public & .is_release_sensitive(content)
        ) |>
        dplyr::group_by(Phase) |>
        dplyr::summarise(
          Rounds = dplyr::n_distinct(round_hour),
          Messages = dplyr::n(),
          Public = sum(is_public, na.rm = TRUE),
          `Rel-public` = sum(release_sensitive, na.rm = TRUE),
          Risk = sum(risk_flag, na.rm = TRUE),
          `Public %` = mean(is_public, na.rm = TRUE),
          `Release %` = mean(release_sensitive, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(Phase == "Crisis day (5 Jun)"))

      datatable(phase, rownames = FALSE, selection = "none",
                options = list(dom = "t", pageLength = 3, scrollX = TRUE)) |>
        formatPercentage(c("Public %", "Release %"), 1)
    })

    output$temporal_reading <- renderUI({
      s <- temporal_summary()
      if (is.null(s)) {
        return(div(class = "text-muted", "No messages in the current selection."))
      }

      first_pre <- if (nrow(s$pre_release)) {
        paste0(format(s$pre_release$timestamp[1], "%b %d %H:%M"),
               " by ", s$pre_release$agent_label[1],
               " via ", s$pre_release$channel[1])
      } else {
        "No pre-embargo release-sensitive public post in the current selection"
      }

      author_txt <- if (nrow(s$top_authors)) {
        paste0(s$top_authors$agent_label, " (", s$top_authors$Posts, ")",
               collapse = ", ")
      } else {
        "No release-sensitive public authors in the current selection"
      }

      z_txt <- if (is.infinite(s$z_score)) {
        "structural break: baseline variance is zero but crisis release count is positive"
      } else if (is.na(s$z_score)) {
        "not computable for the current filter"
      } else {
        paste0(round(s$z_score, 2), " SD above baseline")
      }

      tags$div(
        tags$p(tags$b("Interpretation. "),
               "The pattern is best read as a timed public narrative-release cascade, not ordinary public communication. The decisive change is not only message volume; it is the appearance and repetition of deal-specific public language before the 6:00 PM embargo lift."),
        tags$ul(
          tags$li(tags$b("First pre-embargo disclosure: "), first_pre, "."),
          tags$li(tags$b("Pre-embargo release-sensitive posts: "),
                  nrow(s$pre_release), " of ", nrow(s$release_all),
                  " release-sensitive public posts in the selected window."),
          tags$li(tags$b("Peak release hour: "),
                  if (nrow(s$peak)) format(s$peak$round_hour[1], "%b %d %H:%M") else "—",
                  " with ", if (nrow(s$peak)) s$peak$release_public[1] else "—",
                  " release-sensitive public posts."),
          tags$li(tags$b("Dominant public authors: "), author_txt, "."),
          tags$li(tags$b("Baseline departure: "), z_txt, ".")
        )
      )
    })

    output$anomaly_tbl <- renderDT({
      s <- temporal_summary()
      if (is.null(s)) return(datatable(data.frame()))

      baseline_rel <- if (nrow(s$baseline)) sum(s$baseline$release_public, na.rm = TRUE) else 0
      crisis_rel <- if (nrow(s$crisis)) sum(s$crisis$release_public, na.rm = TRUE) else 0
      baseline_public <- if (nrow(s$baseline)) sum(s$baseline$n_public, na.rm = TRUE) else 0
      crisis_public <- if (nrow(s$crisis)) sum(s$crisis$n_public, na.rm = TRUE) else 0

      score <- data.frame(
        Signal = c("Release-sensitive public posts",
                   "Release share of public posts",
                   "Max release posts in one round",
                   "Baseline departure score",
                   "Pre-embargo release posts"),
        Baseline = c(
          baseline_rel,
          if (baseline_public > 0) scales::percent(baseline_rel / baseline_public, accuracy = 0.1) else "—",
          .fmt_temporal_value(s$baseline_release_round_max, "release_public"),
          "reference",
          "0 expected before embargo"
        ),
        Crisis = c(
          crisis_rel,
          if (crisis_public > 0) scales::percent(crisis_rel / crisis_public, accuracy = 0.1) else "—",
          .fmt_temporal_value(s$crisis_release_round_max, "release_public"),
          if (is.infinite(s$z_score)) "structural break" else if (is.na(s$z_score)) "—" else paste0(round(s$z_score, 2), " SD"),
          nrow(s$pre_release)
        )
      )

      datatable(score, rownames = FALSE, selection = "none",
                options = list(dom = "t", pageLength = 5, scrollX = TRUE))
    })

    output$channel_heatmap <- renderPlotly({
      fd <- filtered_focus()
      validate(need(nrow(fd) > 0, "No messages in the current selection."))

      hm <- fd |>
        dplyr::count(agent_label, channel, name = "Messages") |>
        tidyr::complete(agent_label, channel, fill = list(Messages = 0))

      p <- ggplot(hm, aes(x = channel, y = agent_label, fill = Messages,
                          text = paste0(agent_label, "<br>", channel,
                                        "<br>messages: ", Messages))) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_gradient(low = "#F2F4F7", high = "#E15759") +
        labs(x = NULL, y = NULL, fill = "Messages") +
        theme_minimal(base_size = 12) +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(angle = 35, hjust = 1))

      ggplotly(p, tooltip = "text") |>
        layout(margin = list(l = 120, r = 15, t = 10, b = 85))
    })

    output$events_tbl <- renderDT({
      ev <- events_in_range() |>
        dplyr::transmute(
          Round = format(round_hour, "%b %d %H:%M"),
          Type = event_type,
          Event = event_text,
          Headline = event_headline,
          Market = paste(stock_price, percent_change, mkt_sentiment)
        )

      datatable(ev, rownames = FALSE, selection = "none",
                options = list(pageLength = 6, scrollX = TRUE))
    })

    output$release_tbl <- renderDT({
      rel <- release_posts() |>
        dplyr::transmute(
          Time = format(timestamp, "%b %d %H:%M"),
          Phase = dplyr::if_else(.is_pre_embargo(timestamp),
                                 "Pre-embargo", "Post-embargo"),
          Author = agent_label,
          Channel = channel,
          Content = content
        )

      datatable(rel, rownames = FALSE, selection = "none",
                options = list(pageLength = 7, scrollX = TRUE)) |>
        formatStyle("Phase", target = "row",
                    backgroundColor = styleEqual("Pre-embargo", "#FDECEC"))
    })
  })
}
