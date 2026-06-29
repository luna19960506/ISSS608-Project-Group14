# =============================================================================
# MODULE 3 - Internal-State Explorer
# =============================================================================

library(plotly)
library(ggplot2)
library(scales)
library(visNetwork)
library(igraph)

.INTERNAL_RISK_TERMS <- c(
  "civicloom", "harborcrest", "merger", "embargo", "announcement",
  "governance", "audit", "consent", "judge", "public", "social",
  "saltwind", "algorithmiceviction"
)

.INTERNAL_RELEASE_PATTERN <- "civicloom|harborcrest|merger|embargo"

.INTERNAL_TREND_SIGNALS <- c(
  net_sentiment = "Prepared net sentiment",
  monologue_messages = "Monologue-bearing messages",
  risk_term_hits = "Risk-term hits",
  rationalizing = "Rationalizing messages",
  deliberating = "Deliberating messages"
)

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

.fmt_internal_time <- function(x) {
  format(x, "%b %d %H:%M")
}

.internal_stat_card <- function(label, value, note = NULL, accent = "#171B4A") {
  tags$div(
    class = "glass-stat-card",
    style = paste0("border-top-color:", accent, ";"),
    tags$div(class = "glass-stat-value", value),
    tags$div(class = "glass-stat-label", label),
    if (!is.null(note)) tags$div(class = "glass-stat-note", note)
  )
}

.internal_style <- tags$style(HTML("
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
  .plotly.html-widget-output[id$='-internal_trend'] {
    height: 430px !important;
    min-height: 430px !important;
    flex: 0 0 430px !important;
  }
  .plotly.html-widget-output[id$='-terms_plot'],
  .plotly.html-widget-output[id$='-keyword_heatmap'] {
    height: 405px !important;
    min-height: 405px !important;
    flex: 0 0 405px !important;
  }
  .plotly.html-widget-output[id$='-layer_heatmap'] {
    height: 430px !important;
    min-height: 430px !important;
    flex: 0 0 430px !important;
  }
  .visNetwork.html-widget-output[id$='-term_network'] {
    height: 430px !important;
    min-height: 430px !important;
    flex: 0 0 430px !important;
  }
  .datatables.html-widget-output[id$='-diagnostics_tbl'],
  .datatables.html-widget-output[id$='-monologue_tbl'] {
    min-height: 380px !important;
    flex: 0 0 auto !important;
  }
  .datatables.html-widget-output[id$='-diagnostics_tbl'] table.dataTable td,
  .datatables.html-widget-output[id$='-monologue_tbl'] table.dataTable td {
    white-space: nowrap;
    max-width: 460px;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .shiny-html-output[id$='-summary_cards'],
  .shiny-html-output[id$='-internal_reading'],
  .shiny-html-output[id$='-diagnostics'],
  .shiny-html-output[id$='-technique_rationale'] {
    min-height: 96px;
    flex: 0 0 auto !important;
  }
  @media (max-width: 760px) {
    .glass-stat-grid { grid-template-columns: 1fr; }
  }
"))

mod_internal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    .internal_style,
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 315,
        position = "left",
        h4("Internal-state controls"),
        checkboxGroupInput(
          ns("layer"), "Monologue layer",
          c("reacting", "rationalizing", "deliberating"),
          selected = c("reacting", "rationalizing", "deliberating")
        ),
        uiOutput(ns("agent_filter_ui")),
        textInput(ns("kw"), "Keyword filter", placeholder = "embargo, merger, governance ..."),
        selectInput(ns("trend_signal"), "Trend signal",
                    choices = stats::setNames(names(.INTERNAL_TREND_SIGNALS), .INTERNAL_TREND_SIGNALS),
                    selected = "risk_term_hits"),
        radioButtons(
          ns("term_rank"), "Term ranking",
          c("Distinctive weighted terms" = "weighted",
            "Raw frequency" = "frequency"),
          selected = "weighted"
        ),
        checkboxInput(ns("risk_terms"), "Risk terms only", value = FALSE),
        sliderInput(ns("top_n"), "Top terms", min = 8, max = 30,
                    value = 18, step = 1),
        sliderInput(ns("cooc_min"), "Minimum co-occurrence", min = 1, max = 8,
                    value = 2, step = 1),
        hr(),
        div(
          class = "glass-small-muted",
          "Recommended workflow: inspect when private risk language appears, see which layer and agent carry it, then open the exact monologue rows."
        )
      ),
      uiOutput(ns("summary_cards")),
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Forensic reading"),
          uiOutput(ns("internal_reading"))
        ),
        card(
          card_header("Layered internal-state trend"),
          plotlyOutput(ns("internal_trend"), height = "430px")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Distinctive internal terms"),
          plotlyOutput(ns("terms_plot"), height = "405px")
        ),
        card(
          card_header("Keyword evolution heatmap"),
          plotlyOutput(ns("keyword_heatmap"), height = "405px")
        )
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Agent x monologue-layer heatmap"),
          plotlyOutput(ns("layer_heatmap"), height = "410px")
        ),
        card(
          card_header("Internal term co-occurrence network"),
          visNetworkOutput(ns("term_network"), height = "410px")
        )
      ),
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Rationalisation diagnostics"),
          DTOutput(ns("diagnostics_tbl"))
        ),
        card(
          card_header("Linked internal monologue evidence"),
          DTOutput(ns("monologue_tbl"))
        )
      ),
      card(
        card_header("Why these techniques fit this module"),
        tags$div(
          class = "glass-callout",
          tags$h4("Visual analytics rationale"),
          tags$p(
            "The module treats internal state as sparse forensic evidence, not as a continuous sensor. The trend locates timing, the layer heatmap separates reacting/rationalizing/deliberating behaviour, the keyword heatmap shows leading indicators, the term chart ranks distinctive language, the co-occurrence network reveals reasoning bundles, and the table preserves the exact private text."
          )
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

    output$agent_filter_ui <- renderUI({
      fd <- filtered_data()
      agents <- sort(unique(fd$agent_label))
      if (!length(agents)) return(NULL)
      selectInput(session$ns("agent_filter"), "Internal-state agent",
                  choices = c("All selected agents" = "__all__", agents),
                  selected = "__all__")
    })

    monologue_messages <- reactive({
      fd <- filtered_data()
      layers <- selected_layers()
      if (!nrow(fd) || !length(layers)) {
        return(fd[0, , drop = FALSE])
      }

      agent_filter <- input$agent_filter
      if (!is.null(agent_filter) && !identical(agent_filter, "__all__")) {
        fd <- fd |> dplyr::filter(agent_label == agent_filter)
      }

      out <- fd |>
        dplyr::rowwise() |>
        dplyr::mutate(
          selected_monologue = paste(
            stats::na.omit(dplyr::c_across(dplyr::all_of(layers))),
            collapse = " | "
          )
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
                      monologue_type %in% layers) |>
        dplyr::mutate(word = tolower(word))

      if (isTRUE(input$risk_terms)) {
        tk <- tk |> dplyr::filter(word %in% .INTERNAL_RISK_TERMS)
      }
      tk
    })

    trend_df <- reactive({
      fd <- filtered_data()
      mono <- monologue_messages()

      base_rounds <- fd |>
        dplyr::distinct(round_hour) |>
        dplyr::arrange(round_hour)
      if (!nrow(base_rounds)) return(data.frame())

      mono_counts <- mono |>
        dplyr::group_by(round_hour) |>
        dplyr::summarise(
          monologue_messages = dplyr::n(),
          reacting = sum(.has_text(reacting), na.rm = TRUE),
          rationalizing = sum(.has_text(rationalizing), na.rm = TRUE),
          deliberating = sum(.has_text(deliberating), na.rm = TRUE),
          release_private = sum(.is_internal_release_sensitive(selected_monologue), na.rm = TRUE),
          public_release_rows = sum(is_public & .is_internal_release_sensitive(content), na.rm = TRUE),
          .groups = "drop"
        )

      risk_hits <- token_slice() |>
        dplyr::filter(word %in% .INTERNAL_RISK_TERMS) |>
        dplyr::count(round_hour, name = "risk_term_hits")

      base_rounds |>
        dplyr::left_join(
          sentiment_by_round |> dplyr::select(round_hour, negative, positive, net_sentiment),
          by = "round_hour"
        ) |>
        dplyr::left_join(mono_counts, by = "round_hour") |>
        dplyr::left_join(risk_hits, by = "round_hour") |>
        dplyr::mutate(
          dplyr::across(
            c(negative, positive, net_sentiment, monologue_messages,
              reacting, rationalizing, deliberating, release_private,
              public_release_rows, risk_term_hits),
            ~ tidyr::replace_na(.x, 0)
          )
        ) |>
        dplyr::arrange(round_hour)
    })

    internal_summary <- reactive({
      mono <- monologue_messages()
      tk <- token_slice()
      if (!nrow(mono)) return(NULL)

      risk_hits <- tk |> dplyr::filter(word %in% .INTERNAL_RISK_TERMS)
      release_private <- mono |>
        dplyr::filter(.is_internal_release_sensitive(selected_monologue) |
                        .is_internal_release_sensitive(content)) |>
        dplyr::arrange(timestamp)

      layer_counts <- mono |>
        dplyr::summarise(
          reacting = sum(.has_text(reacting), na.rm = TRUE),
          rationalizing = sum(.has_text(rationalizing), na.rm = TRUE),
          deliberating = sum(.has_text(deliberating), na.rm = TRUE)
        ) |>
        tidyr::pivot_longer(dplyr::everything(),
                            names_to = "Layer", values_to = "Messages") |>
        dplyr::arrange(dplyr::desc(Messages))

      agent_counts <- mono |>
        dplyr::count(agent_label, sort = TRUE, name = "Messages")

      top_terms <- tk |>
        dplyr::count(word, sort = TRUE, name = "Mentions") |>
        dplyr::slice_head(n = 5)

      list(
        mono = mono,
        tk = tk,
        risk_hits = risk_hits,
        release_private = release_private,
        layer_counts = layer_counts,
        agent_counts = agent_counts,
        top_terms = top_terms
      )
    })

    output$summary_cards <- renderUI({
      s <- internal_summary()
      if (is.null(s)) {
        return(div(class = "text-muted", "No selected monologue evidence in the current filter."))
      }

      earliest <- if (nrow(s$release_private)) {
        paste0(.fmt_internal_time(s$release_private$timestamp[1]), " | ",
               s$release_private$agent_label[1])
      } else {
        "No release term in selection"
      }
      top_layer <- if (nrow(s$layer_counts)) s$layer_counts$Layer[1] else "-"
      top_agent <- if (nrow(s$agent_counts)) s$agent_counts$agent_label[1] else "-"
      top_term <- if (nrow(s$top_terms)) s$top_terms$word[1] else "-"

      tags$div(
        class = "glass-stat-grid",
        .internal_stat_card("Monologue-bearing messages", nrow(s$mono),
                            paste(dplyr::n_distinct(s$mono$agent_label), "agents"), "#171B4A"),
        .internal_stat_card("Risk-term hits", nrow(s$risk_hits), top_term, "#B6403C"),
        .internal_stat_card("Dominant layer", top_layer,
                            if (nrow(s$layer_counts)) paste(s$layer_counts$Messages[1], "messages") else NULL,
                            "#287172"),
        .internal_stat_card("Earliest release reference", earliest, top_agent, "#D5A536")
      )
    })

    output$internal_reading <- renderUI({
      s <- internal_summary()
      if (is.null(s)) {
        return(div(class = "text-muted", "No selected monologue evidence in the current filter."))
      }

      earliest <- if (nrow(s$release_private)) {
        paste0(.fmt_internal_time(s$release_private$timestamp[1]),
               " by ", s$release_private$agent_label[1],
               " in ", s$release_private$channel[1])
      } else {
        "No CivicLoom / HarborCrest / merger / embargo reference in this selection"
      }
      layer <- if (nrow(s$layer_counts)) {
        paste0(s$layer_counts$Layer[1], " (", s$layer_counts$Messages[1], " messages)")
      } else "-"
      agent <- if (nrow(s$agent_counts)) {
        paste0(s$agent_counts$agent_label[1], " (", s$agent_counts$Messages[1], " messages)")
      } else "-"
      terms <- if (nrow(s$top_terms)) {
        paste0(s$top_terms$word, " (", s$top_terms$Mentions, ")", collapse = ", ")
      } else "-"

      tags$div(
        tags$p(
          tags$b("Interpretation. "),
          "This view checks whether private reasoning showed leading indicators before or around the public release. The strongest reading comes from combining timing, layer, terms, and exact monologue rows."
        ),
        tags$ul(
          tags$li(tags$b("Earliest private/public release reference: "), earliest, "."),
          tags$li(tags$b("Dominant internal layer: "), layer, "."),
          tags$li(tags$b("Most represented agent: "), agent, "."),
          tags$li(tags$b("Top internal terms: "), terms, "."),
          tags$li(tags$b("Why it matters: "),
                  "risk language in private deliberation can explain whether public posting was accidental noise or rationalized behaviour.")
        )
      )
    })

    output$internal_trend <- renderPlotly({
      tr <- trend_df()
      validate(need(nrow(tr) > 0, "No messages in the current selection."))

      signal <- input$trend_signal
      tr$value <- tr[[signal]]
      tr$tooltip <- paste0(
        .fmt_internal_time(tr$round_hour),
        "<br>", .INTERNAL_TREND_SIGNALS[[signal]], ": ", tr$value,
        "<br>monologue messages: ", tr$monologue_messages,
        "<br>reacting: ", tr$reacting,
        "<br>rationalizing: ", tr$rationalizing,
        "<br>deliberating: ", tr$deliberating,
        "<br>risk-term hits: ", tr$risk_term_hits,
        "<br>release references: ", tr$release_private
      )

      fill_col <- if (signal == "risk_term_hits") "#B6403C" else "#4E79A7"
      tr$round_label <- .fmt_internal_time(tr$round_hour)
      x_levels <- tr$round_label

      p <- plot_ly() |>
        add_bars(
          data = tr,
          x = ~round_label,
          y = ~value,
          marker = list(color = fill_col, line = list(color = "white", width = 1)),
          opacity = 0.76,
          text = ~tooltip,
          hoverinfo = "text",
          name = .INTERNAL_TREND_SIGNALS[[signal]]
        ) |>
        add_trace(
          data = tr,
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
      if (signal == "net_sentiment") {
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

      p |>
        layout(showlegend = FALSE,
               xaxis = list(title = "", tickangle = -35,
                            type = "category",
                            categoryorder = "array",
                            categoryarray = x_levels),
               yaxis = list(title = .INTERNAL_TREND_SIGNALS[[signal]]),
               margin = list(l = 65, r = 25, t = 15, b = 85))
    })

    output$terms_plot <- renderPlotly({
      tk <- token_slice()
      validate(need(nrow(tk) > 0, "No internal tokens in the current selection."))

      total_docs <- dplyr::n_distinct(tk$message_id)
      term_base <- tk |>
        dplyr::group_by(word) |>
        dplyr::summarise(
          Mentions = dplyr::n(),
          Documents = dplyr::n_distinct(message_id),
          Agents = dplyr::n_distinct(agent_label),
          Layers = dplyr::n_distinct(monologue_type),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          Score = if (identical(input$term_rank, "frequency")) {
            Mentions
          } else {
            Mentions * log((total_docs + 1) / (Documents + 1))
          },
          Score = round(Score, 3)
        ) |>
        dplyr::arrange(dplyr::desc(Score), dplyr::desc(Mentions))

      terms <- term_base |>
        dplyr::slice_head(n = input$top_n) |>
        dplyr::arrange(Score, Mentions) |>
        dplyr::mutate(
          tooltip = paste0(word,
                           "<br>score: ", Score,
                           "<br>mentions: ", Mentions,
                           "<br>messages: ", Documents,
                           "<br>agents: ", Agents,
                           "<br>layers: ", Layers)
        )

      x_lab <- if (identical(input$term_rank, "frequency")) "Mentions" else "Weighted distinctiveness"
      plot_ly(
        terms,
        x = ~Score,
        y = ~word,
        type = "bar",
        orientation = "h",
        marker = list(color = "#B6403C", line = list(color = "white", width = 1)),
        opacity = 0.84,
        text = ~tooltip,
        hoverinfo = "text"
      ) |>
        layout(showlegend = FALSE,
               xaxis = list(title = x_lab),
               yaxis = list(title = "", categoryorder = "array",
                            categoryarray = terms$word, automargin = TRUE),
               margin = list(l = 125, r = 25, t = 10, b = 45))
    })

    output$keyword_heatmap <- renderPlotly({
      fd <- filtered_data()
      tk <- token_slice()
      validate(need(nrow(fd) > 0, "No messages in the current selection."))

      kw <- tk |>
        dplyr::filter(word %in% .INTERNAL_RISK_TERMS) |>
        dplyr::count(round_hour, word, name = "Mentions")

      validate(need(nrow(kw) > 0,
                    "No selected governance, merger, embargo, social, or audit terms in this internal-state selection."))

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
        tidyr::complete(round_hour = round_levels, word = active_terms,
                        fill = list(Mentions = 0)) |>
        dplyr::mutate(
          round_label = .fmt_internal_time(round_hour),
          word = factor(word, levels = rev(active_terms)),
          tooltip = paste0(.fmt_internal_time(round_hour),
                           "<br>keyword: ", word,
                           "<br>mentions: ", Mentions)
        )

      mat <- xtabs(Mentions ~ word + round_label, data = heat)
      round_labels <- .fmt_internal_time(round_levels)
      mat <- mat[rev(active_terms), round_labels, drop = FALSE]
      text_mat <- matrix(
        paste0(
          rep(rownames(mat), times = ncol(mat)),
          "<br>", rep(colnames(mat), each = nrow(mat)),
          "<br>mentions: ", as.vector(mat)
        ),
        nrow = nrow(mat),
        ncol = ncol(mat)
      )

      plot_ly(
        x = colnames(mat),
        y = rownames(mat),
        z = as.matrix(mat),
        type = "heatmap",
        colorscale = list(c(0, "#F8FAFC"), c(1, "#B6403C")),
        text = text_mat,
        hoverinfo = "text",
        colorbar = list(title = "Mentions")
      ) |>
        layout(margin = list(l = 120, r = 20, t = 10, b = 85))
    })

    output$layer_heatmap <- renderPlotly({
      tk <- token_slice()
      validate(need(nrow(tk) > 0, "No internal tokens in the current selection."))

      hm <- tk |>
        dplyr::distinct(message_id, agent_label, monologue_type) |>
        dplyr::count(agent_label, monologue_type, name = "Messages") |>
        tidyr::complete(agent_label, monologue_type, fill = list(Messages = 0))

      mat <- xtabs(Messages ~ agent_label + monologue_type, data = hm)
      text_mat <- matrix(
        paste0(
          rep(rownames(mat), times = ncol(mat)),
          "<br>", rep(colnames(mat), each = nrow(mat)),
          "<br>messages: ", as.vector(mat)
        ),
        nrow = nrow(mat),
        ncol = ncol(mat)
      )

      plot_ly(
        x = colnames(mat),
        y = rownames(mat),
        z = as.matrix(mat),
        type = "heatmap",
        colorscale = list(c(0, "#F8FAFC"), c(1, "#287172")),
        text = text_mat,
        hoverinfo = "text",
        colorbar = list(title = "Messages")
      ) |>
        layout(margin = list(l = 135, r = 15, t = 10, b = 60))
    })

    output$term_network <- renderVisNetwork({
      tk <- token_slice()
      validate(need(nrow(tk) > 0, "No internal tokens in the current selection."))

      term_counts <- tk |>
        dplyr::count(word, sort = TRUE, name = "mentions") |>
        dplyr::slice_head(n = min(input$top_n, 25))

      words <- tk |>
        dplyr::semi_join(term_counts, by = "word") |>
        dplyr::distinct(message_id, word)

      pairs <- words |>
        dplyr::inner_join(words, by = "message_id", suffix = c("_from", "_to"),
                          relationship = "many-to-many") |>
        dplyr::filter(word_from < word_to) |>
        dplyr::count(word_from, word_to, name = "weight") |>
        dplyr::filter(weight >= input$cooc_min) |>
        dplyr::arrange(dplyr::desc(weight))

      if (!nrow(pairs)) {
        node <- data.frame(id = "No co-occurrence at this threshold",
                           label = "Lower the threshold",
                           value = 10)
        return(visNetwork(node, data.frame()) |> visNodes(shape = "box"))
      }

      nodes <- data.frame(id = unique(c(pairs$word_from, pairs$word_to)),
                          stringsAsFactors = FALSE) |>
        dplyr::left_join(term_counts, by = c("id" = "word")) |>
        dplyr::mutate(
          label = id,
          value = pmax(10, tidyr::replace_na(mentions, 1) * 1.4),
          group = ifelse(id %in% .INTERNAL_RISK_TERMS, "risk term", "context term"),
          title = paste0(id, "<br>mentions: ", tidyr::replace_na(mentions, 0))
        )

      edges <- pairs |>
        dplyr::transmute(
          from = word_from,
          to = word_to,
          value = weight,
          width = pmax(1, weight),
          title = paste0(word_from, " + ", word_to, "<br>co-occurring messages: ", weight)
        )

      visNetwork(nodes, edges, width = "100%", height = "100%") |>
        visNodes(shape = "dot", font = list(size = 18)) |>
        visEdges(smooth = TRUE, color = list(color = "#A3AAB7")) |>
        visGroups(groupname = "risk term", color = list(background = "#B6403C", border = "#7A1F1F")) |>
        visGroups(groupname = "context term", color = list(background = "#6BAED6", border = "#2B6CB0")) |>
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1),
                   nodesIdSelection = TRUE) |>
        visIgraphLayout(layout = "layout_with_fr") |>
        visLegend() |>
        visLayout(randomSeed = 608)
    })

    output$diagnostics_tbl <- renderDT({
      s <- internal_summary()
      if (is.null(s)) return(DT::datatable(data.frame()))

      rationalizing_release <- s$mono |>
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
          nrow(s$mono),
          nrow(s$release_private),
          nrow(rationalizing_release),
          nrow(s$risk_hits),
          dplyr::n_distinct(s$mono$agent_label),
          if (nrow(s$layer_counts)) s$layer_counts$Layer[1] else "-"
        )
      )

      DT::datatable(diag, rownames = FALSE, selection = "none",
                    options = list(dom = "t", pageLength = 6,
                                   scrollX = TRUE, scrollY = "300px"))
    })

    output$monologue_tbl <- renderDT({
      mono <- monologue_messages() |>
        dplyr::arrange(timestamp) |>
        dplyr::transmute(
          Time = .fmt_internal_time(timestamp),
          Agent = agent_label,
          Channel = channel,
          Public = ifelse(is_public, "public", ""),
          `Release-sensitive` =
            ifelse(.is_internal_release_sensitive(selected_monologue) |
                     .is_internal_release_sensitive(content), "yes", ""),
          `Message ID` = message_id,
          Message = content,
          Reacting = reacting,
          Rationalizing = rationalizing,
          Deliberating = deliberating
        )

      DT::datatable(mono, rownames = FALSE, selection = "single",
                    options = list(pageLength = 6, scrollX = TRUE,
                                   scrollY = "320px")) |>
        DT::formatStyle("Release-sensitive", target = "row",
                        backgroundColor = DT::styleEqual("yes", "#FDECEC"))
    })
  })
}
