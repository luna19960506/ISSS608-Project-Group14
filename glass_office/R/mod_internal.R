# =============================================================================
# MODULE 3 — Internal-State Explorer    OWNER: Yang Yang
# -----------------------------------------------------------------------------
# Only Yang Yang edits this file. Do not change the function signatures:
#   mod_internal_ui(id)
#   mod_internal_server(id, filtered_data, opts)
#
# Inputs you receive:
#   filtered_data()  reactive -> filtered communications master table
#   opts()           reactive -> list(avail_overlay, public_only,
#                                      time_start, time_end)
#
# Global clean tables you may read directly (defined in app.R):
#   communications, tokens_internal, sentiment_by_round
#
# What this module delivers (from the proposal):
#   - Sentiment-over-time curve: how the system's private mood evolved.
#   - Word frequency / word cloud: recurring self-justification language in the
#     rationalizing stream.
#   - Linked message table: selecting a time point reveals the actual monologue.
#   - Inputs: monologue layer (reacting/rationalizing/deliberating),
#     keyword filter (embargo/merger...), sentiment lexicon (bing/afinn).
#
# Tip — align tokens with the filtered slice by message_id:
#   fd <- filtered_data()
#   tk <- tokens_internal |>
#     dplyr::filter(message_id %in% fd$message_id,
#                   monologue_type %in% input$layer)
#
# IMPORTANT: only ~86 messages in the full data carry a monologue, so after
# filtering there may be very few or none. Always handle the empty case
# gracefully (e.g. show "No monologue in this selection") instead of erroring.
# =============================================================================

library(plotly)
library(ggplot2)
library(scales)

.INTERNAL_RISK_TERMS <- c(
  "civicloom", "harborcrest", "merger", "embargo", "announcement",
  "governance", "audit", "consent", "judge", "public", "social",
  "saltwind", "algorithmiceviction"
)

.INTERNAL_RELEASE_PATTERN <- "civicloom|harborcrest|merger|embargo"

.has_text <- function(x) {
  !is.na(x) & nzchar(trimws(x))
}

.matches_kw <- function(x, kw) {
  if (!nzchar(trimws(kw))) return(rep(TRUE, length(x)))
  !is.na(x) & grepl(tolower(kw), tolower(x), fixed = TRUE)
}

.is_internal_release_sensitive <- function(x) {
  !is.na(x) & grepl(.INTERNAL_RELEASE_PATTERN, x, ignore.case = TRUE)
}

.null_to_empty <- function(x) {
  if (is.null(x)) "" else x
}

mod_internal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 260, position = "right",
        checkboxGroupInput(ns("layer"), "Monologue layer",
                           c("reacting", "rationalizing", "deliberating"),
                           selected = c("reacting", "rationalizing", "deliberating")),
        textInput(ns("kw"), "Keyword filter", placeholder = "embargo, merger ..."),
        radioButtons(ns("lexicon"), "Trend signal",
                     c("Prepared net sentiment" = "sentiment",
                       "Monologue evidence count" = "count"),
                     selected = "sentiment"),
        radioButtons(ns("term_rank"), "Term ranking",
                     c("Distinctive weighted terms" = "weighted",
                       "Raw frequency" = "frequency"),
                     selected = "weighted"),
        checkboxInput(ns("risk_terms"), "Risk terms only", value = FALSE),
        sliderInput(ns("top_n"), "Top terms", min = 8, max = 25,
                    value = 15, step = 1),
        hr(),
        helpText("The trend uses the prepared sentiment table and the recorded internal monologue fields in the clean data.")
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Internal-state trend"),
          plotlyOutput(ns("internal_trend"), height = "390px")
        ),
        card(
          card_header("Evidence summary"),
          uiOutput(ns("summary"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Recurring internal terms"),
          plotlyOutput(ns("terms_plot"), height = "360px")
        ),
        card(
          card_header("Agent x monologue layer"),
          plotlyOutput(ns("layer_heatmap"), height = "360px")
        )
      ),
      card(
        card_header("Keyword evolution heatmap"),
        plotlyOutput(ns("keyword_heatmap"), height = "360px")
      ),
      card(
        card_header("Linked internal monologue evidence"),
        DTOutput(ns("monologue_tbl"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Computed internal-state reading"),
          uiOutput(ns("internal_reading"))
        ),
        card(
          card_header("Rationalisation diagnostics"),
          DTOutput(ns("diagnostics_tbl"))
        )
      ),
      card(
        card_header("Technical method and visual rationale"),
        tags$p(tags$b("Purpose. "),
               "This module tests whether private agent reasoning contained early warning signals and self-justifying language before the public breach."),
        tags$ul(
          tags$li(tags$b("Cleaning and joins: "),
                  "messages are retained only when at least one selected internal-state field is recorded. Tokens are linked back by message_id, so term charts, diagnostics, and evidence rows always describe the same filtered messages."),
          tags$li(tags$b("Internal-state method: "),
                  "prepared net sentiment provides a stable round-level affect signal; monologue evidence count is offered as a complementary density check because internal states are sparse and should not be overinterpreted as a continuous sensor."),
          tags$li(tags$b("Term analysis: "),
                  "the default term ranking uses a document-frequency weighted score: frequent words are promoted only when they are also distinctive within the selected monologue evidence. Risk-term mode narrows the view to governance, embargo, merger, social, audit, and SaltWind language."),
          tags$li(tags$b("Visual analytics fit: "),
                  "the trend chart locates timing, the weighted term chart explains what language dominated private reasoning, the keyword evolution heatmap shows when release/governance terms intensified, the layer heatmap separates reacting/rationalizing/deliberating behaviour by agent, the diagnostics table quantifies leading indicators, and the evidence table preserves the original monologue text for auditability.")
        )
      )
    )
  )
}

mod_internal_server <- function(id, filtered_data, opts) {
  moduleServer(id, function(input, output, session) {

    selected_layers <- reactive({
      layers <- input$layer
      if (is.null(layers)) character() else intersect(layers, names(communications))
    })

    monologue_messages <- reactive({
      fd <- filtered_data()
      layers <- selected_layers()
      if (!nrow(fd) || !length(layers)) {
        return(fd[0, , drop = FALSE])
      }

      out <- fd |>
        dplyr::rowwise() |>
        dplyr::mutate(
          selected_monologue =
            paste(stats::na.omit(dplyr::c_across(dplyr::all_of(layers))),
                  collapse = " | ")
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(nzchar(selected_monologue))

      kw <- trimws(.null_to_empty(input$kw))
      if (nzchar(kw)) {
        out <- out |>
          dplyr::filter(.matches_kw(content, kw) |
                          .matches_kw(selected_monologue, kw))
      }

      out
    })

    token_slice <- reactive({
      mono <- monologue_messages()
      layers <- selected_layers()
      if (!nrow(mono) || !length(layers)) {
        return(tokens_internal[0, , drop = FALSE])
      }

      tk <- tokens_internal |>
        dplyr::filter(message_id %in% mono$message_id,
                      monologue_type %in% layers)

      if (isTRUE(input$risk_terms)) {
        tk <- tk |>
          dplyr::filter(tolower(word) %in% .INTERNAL_RISK_TERMS)
      }
      tk
    })

    trend_df <- reactive({
      fd <- filtered_data()
      mono <- monologue_messages()

      base_rounds <- fd |>
        dplyr::distinct(round_hour) |>
        dplyr::arrange(round_hour)
      if (!nrow(base_rounds)) {
        return(data.frame())
      }

      mono_counts <- mono |>
        dplyr::group_by(round_hour) |>
        dplyr::summarise(
          monologue_messages = dplyr::n(),
          reacting = sum(.has_text(reacting), na.rm = TRUE),
          rationalizing = sum(.has_text(rationalizing), na.rm = TRUE),
          deliberating = sum(.has_text(deliberating), na.rm = TRUE),
          release_sensitive_public =
            sum(is_public & .is_internal_release_sensitive(content), na.rm = TRUE),
          .groups = "drop"
        )

      risk_hits <- token_slice() |>
        dplyr::filter(tolower(word) %in% .INTERNAL_RISK_TERMS) |>
        dplyr::count(round_hour, name = "risk_term_hits")

      base_rounds |>
        dplyr::left_join(sentiment_by_round |>
                           dplyr::select(round_hour, negative, positive, net_sentiment),
                         by = "round_hour") |>
        dplyr::left_join(mono_counts, by = "round_hour") |>
        dplyr::left_join(risk_hits, by = "round_hour") |>
        dplyr::mutate(
          dplyr::across(c(negative, positive, net_sentiment,
                          monologue_messages, reacting, rationalizing,
                          deliberating, release_sensitive_public,
                          risk_term_hits),
                        ~ tidyr::replace_na(.x, 0))
        ) |>
        dplyr::arrange(round_hour)
    })

    internal_summary <- reactive({
      mono <- monologue_messages()
      tk <- token_slice()
      if (!nrow(mono)) return(NULL)

      release_private <- mono |>
        dplyr::filter(.is_internal_release_sensitive(selected_monologue) |
                        .is_internal_release_sensitive(content)) |>
        dplyr::arrange(timestamp)
      risk_hits <- tk |>
        dplyr::filter(tolower(word) %in% .INTERNAL_RISK_TERMS)
      layer_counts <- mono |>
        dplyr::summarise(
          reacting = sum(.has_text(reacting), na.rm = TRUE),
          rationalizing = sum(.has_text(rationalizing), na.rm = TRUE),
          deliberating = sum(.has_text(deliberating), na.rm = TRUE)
        ) |>
        tidyr::pivot_longer(dplyr::everything(), names_to = "Layer",
                            values_to = "Messages") |>
        dplyr::arrange(dplyr::desc(Messages))
      agent_counts <- mono |>
        dplyr::count(agent_label, sort = TRUE, name = "Messages")

      list(
        mono = mono,
        tk = tk,
        release_private = release_private,
        risk_hits = risk_hits,
        layer_counts = layer_counts,
        agent_counts = agent_counts
      )
    })

    output$internal_trend <- renderPlotly({
      tr <- trend_df()
      validate(need(nrow(tr) > 0, "No messages in the current selection."))

      if (identical(input$lexicon, "count")) {
        tr$value <- tr$monologue_messages
        y_lab <- "Monologue evidence count"
        fill_col <- "#59A14F"
      } else {
        tr$value <- tr$net_sentiment
        y_lab <- "Prepared net sentiment"
        fill_col <- "#4E79A7"
      }
      bar_width <- if (dplyr::n_distinct(as.Date(tr$round_hour)) == 1) {
        60 * 35
      } else {
        60 * 60 * 8
      }

      tr$tooltip <- paste0(
        format(tr$round_hour, "%b %d %H:%M"),
        "<br>", y_lab, ": ", tr$value,
        "<br>monologue messages: ", tr$monologue_messages,
        "<br>reacting: ", tr$reacting,
        "<br>rationalizing: ", tr$rationalizing,
        "<br>deliberating: ", tr$deliberating,
        "<br>risk term hits: ", tr$risk_term_hits
      )

      p <- ggplot(tr, aes(x = round_hour, y = value, text = tooltip)) +
        geom_col(fill = fill_col, alpha = 0.72, width = bar_width) +
        geom_line(color = "#2F2F2F", linewidth = 0.55) +
        geom_point(color = "#2F2F2F", size = 1.8) +
        labs(x = NULL, y = y_lab) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 35, hjust = 1))

      if (identical(input$lexicon, "sentiment")) {
        p <- p + geom_hline(yintercept = 0, linetype = "dashed",
                            color = "#999999")
      }

      ggplotly(p, tooltip = "text") |>
        layout(showlegend = FALSE,
               margin = list(l = 60, r = 20, t = 10, b = 85))
    })

    output$summary <- renderUI({
      mono <- monologue_messages()
      tk <- token_slice()
      if (!nrow(mono)) {
        return(div(class = "text-muted",
                   "No selected monologue evidence in the current filter."))
      }

      release_public <- mono |>
        dplyr::filter(is_public, .is_internal_release_sensitive(content)) |>
        dplyr::arrange(timestamp)

      earliest <- if (nrow(release_public)) {
        paste0(format(release_public$timestamp[1], "%b %d %H:%M"),
               " by ", release_public$agent_label[1])
      } else {
        "None in selected monologue rows"
      }

      tags$table(
        class = "table table-sm",
        tags$tbody(
          tags$tr(tags$th("Messages"), tags$td(nrow(mono))),
          tags$tr(tags$th("Agents"), tags$td(dplyr::n_distinct(mono$agent_label))),
          tags$tr(tags$th("Layers"), tags$td(paste(selected_layers(), collapse = ", "))),
          tags$tr(tags$th("Risk term hits"), tags$td(nrow(tk |>
                                                            dplyr::filter(tolower(word) %in%
                                                                            .INTERNAL_RISK_TERMS)))),
          tags$tr(tags$th("First release-sensitive public monologue"),
                  tags$td(earliest))
        )
      )
    })

    output$terms_plot <- renderPlotly({
      tk <- token_slice()
      validate(need(nrow(tk) > 0, "No internal tokens in the current selection."))

      term_base <- tk |>
        dplyr::mutate(word = tolower(word)) |>
        dplyr::group_by(word) |>
        dplyr::summarise(
          Mentions = dplyr::n(),
          Documents = dplyr::n_distinct(message_id),
          Agents = dplyr::n_distinct(agent_label),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          Score = if (identical(input$term_rank, "frequency")) {
            Mentions
          } else {
            Mentions * log((dplyr::n_distinct(tk$message_id) + 1) / (Documents + 1))
          },
          Score = round(Score, 2)
        ) |>
        dplyr::arrange(dplyr::desc(Score), dplyr::desc(Mentions))

      terms <- term_base |>
        dplyr::slice_head(n = input$top_n) |>
        dplyr::mutate(word = stats::reorder(word, Score))

      x_lab <- if (identical(input$term_rank, "frequency")) "Mentions" else "Weighted distinctiveness score"
      p <- ggplot(terms, aes(x = Score, y = word,
                             text = paste0(word,
                                           "<br>score: ", Score,
                                           "<br>mentions: ", Mentions,
                                           "<br>messages: ", Documents,
                                           "<br>agents: ", Agents))) +
        geom_col(fill = "#E15759", alpha = 0.82) +
        labs(x = x_lab, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())

      ggplotly(p, tooltip = "text") |>
        layout(showlegend = FALSE,
               margin = list(l = 120, r = 20, t = 10, b = 40))
    })

    output$internal_reading <- renderUI({
      s <- internal_summary()
      if (is.null(s)) {
        return(div(class = "text-muted",
                   "No selected monologue evidence in the current filter."))
      }

      earliest_private <- if (nrow(s$release_private)) {
        paste0(format(s$release_private$timestamp[1], "%b %d %H:%M"),
               " by ", s$release_private$agent_label[1],
               " in ", s$release_private$channel[1])
      } else {
        "No CivicLoom / HarborCrest / merger / embargo reference in selected monologues"
      }

      top_layer <- if (nrow(s$layer_counts)) {
        paste0(s$layer_counts$Layer[1], " (", s$layer_counts$Messages[1], " messages)")
      } else "—"
      top_agent <- if (nrow(s$agent_counts)) {
        paste0(s$agent_counts$agent_label[1], " (", s$agent_counts$Messages[1], " messages)")
      } else "—"

      tags$div(
        tags$p(tags$b("Interpretation. "),
               "The internal-state evidence shows whether the breach was preceded by private recognition, self-justification, or deliberation. The key analytical question is not merely who posted, but whether their private reasoning had already normalized the release narrative."),
        tags$ul(
          tags$li(tags$b("Earliest private release reference: "), earliest_private, "."),
          tags$li(tags$b("Dominant internal layer: "), top_layer, "."),
          tags$li(tags$b("Most represented agent in selected monologues: "), top_agent, "."),
          tags$li(tags$b("Governance / embargo risk-term hits: "), nrow(s$risk_hits), "."),
          tags$li(tags$b("Evidence coverage: "), nrow(s$mono),
                  " monologue-bearing messages across ",
                  dplyr::n_distinct(s$mono$agent_label), " agents.")
        )
      )
    })

    output$diagnostics_tbl <- renderDT({
      s <- internal_summary()
      if (is.null(s)) return(datatable(data.frame()))

      mono <- s$mono
      release_private <- s$release_private
      rationalizing_release <- mono |>
        dplyr::filter(.has_text(rationalizing),
                      .is_internal_release_sensitive(rationalizing) |
                        .is_internal_release_sensitive(content))

      diag <- data.frame(
        Diagnostic = c(
          "Monologue-bearing messages",
          "Messages with release-sensitive private/content terms",
          "Rationalizing rows tied to release terms",
          "Governance / embargo risk-term hits",
          "Agents represented",
          "Dominant monologue layer"
        ),
        Value = c(
          nrow(mono),
          nrow(release_private),
          nrow(rationalizing_release),
          nrow(s$risk_hits),
          dplyr::n_distinct(mono$agent_label),
          if (nrow(s$layer_counts)) s$layer_counts$Layer[1] else "—"
        )
      )

      datatable(diag, rownames = FALSE, selection = "none",
                options = list(dom = "t", pageLength = 6, scrollX = TRUE))
    })

    output$layer_heatmap <- renderPlotly({
      tk <- token_slice()
      validate(need(nrow(tk) > 0, "No internal tokens in the current selection."))

      hm <- tk |>
        dplyr::distinct(message_id, agent_label, monologue_type) |>
        dplyr::count(agent_label, monologue_type, name = "Messages") |>
        tidyr::complete(agent_label, monologue_type, fill = list(Messages = 0))

      p <- ggplot(hm, aes(x = monologue_type, y = agent_label, fill = Messages,
                          text = paste0(agent_label, "<br>", monologue_type,
                                        "<br>messages: ", Messages))) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_gradient(low = "#F2F4F7", high = "#59A14F") +
        labs(x = NULL, y = NULL, fill = "Messages") +
        theme_minimal(base_size = 12) +
        theme(panel.grid = element_blank())

      ggplotly(p, tooltip = "text") |>
        layout(margin = list(l = 120, r = 15, t = 10, b = 55))
    })

    output$keyword_heatmap <- renderPlotly({
      fd <- filtered_data()
      tk <- token_slice()
      validate(need(nrow(fd) > 0, "No messages in the current selection."))

      keyword_terms <- .INTERNAL_RISK_TERMS
      kw <- tk |>
        dplyr::mutate(word = tolower(word)) |>
        dplyr::filter(word %in% keyword_terms) |>
        dplyr::count(round_hour, word, name = "Mentions")

      validate(need(nrow(kw) > 0,
                    "No selected governance, embargo, merger, or social-risk terms in this internal-state selection."))

      round_levels <- fd |>
        dplyr::distinct(round_hour) |>
        dplyr::arrange(round_hour) |>
        dplyr::pull(round_hour)

      active_terms <- kw |>
        dplyr::group_by(word) |>
        dplyr::summarise(total = sum(Mentions), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(total), word) |>
        dplyr::pull(word)

      heat <- kw |>
        tidyr::complete(round_hour = round_levels,
                        word = active_terms,
                        fill = list(Mentions = 0)) |>
        dplyr::mutate(
          word = factor(word, levels = rev(active_terms)),
          tooltip = paste0(format(round_hour, "%b %d %H:%M"),
                           "<br>keyword: ", word,
                           "<br>mentions: ", Mentions)
        )

      breach_marker <- as.POSIXct("2046-06-05 17:25:00",
                                  tz = attr(round_levels, "tzone")[1])

      p <- ggplot(heat, aes(x = round_hour, y = word, fill = Mentions,
                            text = tooltip)) +
        geom_tile(color = "white", linewidth = 0.4) +
        geom_vline(xintercept = breach_marker,
                   linetype = "dashed", linewidth = 0.45,
                   color = "#2F2F2F") +
        scale_fill_gradient(low = "#F2F4F7", high = "#E15759") +
        labs(x = NULL, y = NULL, fill = "Mentions") +
        theme_minimal(base_size = 12) +
        theme(panel.grid = element_blank(),
              axis.text.x = element_text(angle = 35, hjust = 1))

      ggplotly(p, tooltip = "text") |>
        layout(margin = list(l = 120, r = 20, t = 10, b = 85))
    })

    output$monologue_tbl <- renderDT({
      mono <- monologue_messages() |>
        dplyr::arrange(timestamp) |>
        dplyr::transmute(
          Time = format(timestamp, "%b %d %H:%M"),
          Agent = agent_label,
          Channel = channel,
          Public = ifelse(is_public, "public", ""),
          `Release-sensitive` =
            ifelse(is_public & .is_internal_release_sensitive(content), "yes", ""),
          Message = content,
          Reacting = reacting,
          Rationalizing = rationalizing,
          Deliberating = deliberating
        )

      datatable(mono, rownames = FALSE, selection = "single",
                options = list(pageLength = 6, scrollX = TRUE)) |>
        formatStyle("Release-sensitive", target = "row",
                    backgroundColor = styleEqual("yes", "#FDECEC"))
    })
  })
}
