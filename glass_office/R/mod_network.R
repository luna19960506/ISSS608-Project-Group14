# =============================================================================
# MODULE 1 — Interaction Network        OWNER: Li Xinyue
# -----------------------------------------------------------------------------
# Only Li Xinyue edits this file. Do not change the function signatures:
#   mod_network_ui(id)
#   mod_network_server(id, filtered_data, opts)
#
# Inputs you receive:
#   filtered_data()  reactive -> filtered communications master table
#                                (one row per message; filtered by the shared
#                                 sidebar: time / agents / channels / public)
#   opts()           reactive -> list(avail_overlay, public_only,
#                                      time_start, time_end)
#
# Global clean tables read here (defined in app.R):
#   communications, recipient_edges, agents
#
# Delivers (proposal):
#   - Interactive visNetwork graph: node size = message volume in selection,
#     colour = seniority, oversight agents (Legal / Platform-Trust / Judge)
#     get a dark border, anonymous-post authors highlighted on toggle.
#   - Edge-type switch: reply network (who answers whom) vs recipient network.
#   - Centrality ranking table (DT): degree / interactions / betweenness —
#     tests whether Judge-Agent sits at the periphery.
#   - Click a node -> that agent's messages in the window.
#   - Evidence drawer: pick a public/anonymous post -> upstream parent message,
#     recipients, and the author's internal monologue.
# =============================================================================

library(visNetwork)
library(igraph)

# ---- static lookups (whole-dataset, filter-independent) ---------------------
.SENIORITY_COL <- c(Senior = "#4E79A7", Junior = "#F28E2B", Compliance = "#59A14F")

mod_network_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 270, position = "right",
        radioButtons(ns("edge_type"), "Edge type",
                     c("Reply (who answers whom)" = "reply",
                       "Recipient (who sends to whom)" = "recipient")),
        materialSwitch(ns("flag_anon"), "Highlight anonymous authors",
                       value = TRUE, status = "danger"),
        sliderInput(ns("min_w"), "Min edge weight", min = 1, max = 10,
                    value = 1, step = 1),
        hr(),
        helpText("Click a node to list that agent's messages. ",
                 "Use the evidence table below to trace a public/anonymous post.")
      ),
      # ---- graph + centrality side by side ----
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Interaction network"),
          visNetworkOutput(ns("net"), height = "440px")
        ),
        card(
          card_header("Centrality ranking"),
          DTOutput(ns("centrality"))
        )
      ),
      # ---- click-through: selected agent's messages ----
      card(
        card_header(textOutput(ns("sel_title"), inline = TRUE)),
        DTOutput(ns("agent_msgs"))
      ),
      # ---- evidence drawer: public/anonymous posts -> provenance ----
      card(
        card_header("Breach-path evidence — pick a public / anonymous post"),
        layout_columns(
          col_widths = c(6, 6),
          DTOutput(ns("public_posts")),
          uiOutput(ns("evidence"))
        )
      )
    )
  )
}

mod_network_server <- function(id, filtered_data, opts) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    agent_labels <- agents$agent_label
    # message_id -> author lookup over the FULL data, so reply parents resolve
    # even when the parent message falls just outside the selected window.
    parent_map <- setNames(communications$agent_label, communications$message_id)

    # ---- nodes: all 7 agents, sized by message volume in the selection -------
    node_df <- reactive({
      fd <- filtered_data()
      vol <- fd |>
        dplyr::count(agent_label, name = "n_msg")
      anon_authors <- fd |>
        dplyr::filter(channel == "anonymous_post") |>
        dplyr::distinct(agent_label) |>
        dplyr::pull(agent_label)

      agents |>
        dplyr::left_join(vol, by = "agent_label") |>
        dplyr::mutate(
          n_msg = tidyr::replace_na(n_msg, 0L),
          is_anon = agent_label %in% anon_authors
        )
    })

    # ---- edges: reply OR recipient, both respecting the current filter -------
    edge_df <- reactive({
      fd <- filtered_data()
      if (input$edge_type == "reply") {
        e <- fd |>
          dplyr::filter(!is.na(responding_to)) |>
          dplyr::transmute(
            to     = agent_label,                       # the replier
            from   = unname(parent_map[responding_to])  # the one replied to
          ) |>
          dplyr::filter(!is.na(from)) |>
          dplyr::count(from, to, name = "weight")
      } else {
        e <- recipient_edges |>
          dplyr::filter(message_id %in% fd$message_id,
                        to_recipient %in% agent_labels) |>
          dplyr::count(from = from_agent, to = to_recipient, name = "weight")
      }
      e |>
        dplyr::filter(from != to, weight >= input$min_w)  # drop self-loops
    })

    # ---- centrality on the directed graph ------------------------------------
    centrality_df <- reactive({
      nodes <- node_df()
      edges <- edge_df()
      g <- igraph::graph_from_data_frame(
        d = if (nrow(edges)) edges else
              data.frame(from = character(), to = character(), weight = numeric()),
        vertices = nodes["agent_label"],
        directed = TRUE
      )
      tibble::tibble(
        agent_label  = igraph::V(g)$name,
        partners     = as.integer(igraph::degree(g, mode = "all")),
        interactions = as.numeric(igraph::strength(g, mode = "all",
                                                   weights = igraph::E(g)$weight)),
        betweenness  = round(igraph::betweenness(g, directed = TRUE), 1)
      ) |>
        dplyr::left_join(nodes |>
                           dplyr::select(agent_label, role, seniority,
                                         is_oversight, n_msg),
                         by = "agent_label") |>
        dplyr::arrange(dplyr::desc(betweenness), dplyr::desc(interactions))
    })

    # ---- network graph -------------------------------------------------------
    output$net <- renderVisNetwork({
      nodes <- node_df()
      edges <- edge_df()

      vis_nodes <- nodes |>
        dplyr::transmute(
          id    = agent_label,
          label = agent_label,
          value = n_msg + 1,                       # size by volume
          group = seniority,
          color.background = unname(.SENIORITY_COL[seniority]),
          color.border = dplyr::case_when(
            isTRUE(input$flag_anon) & is_anon ~ "#E15759",   # anon highlight
            is_oversight                      ~ "#1B1B1B",   # oversight ring
            TRUE                              ~ "#BBBBBB"
          ),
          borderWidth = dplyr::case_when(
            isTRUE(input$flag_anon) & is_anon ~ 4,
            is_oversight                      ~ 3,
            TRUE                              ~ 1
          ),
          title = paste0("<b>", agent_label, "</b><br>",
                         role, " · ", seniority,
                         ifelse(is_oversight, " · oversight", ""),
                         "<br>messages: ", n_msg,
                         ifelse(is_anon, "<br><i>posted anonymously</i>", ""))
        )

      vis_edges <- if (nrow(edges)) {
        edges |>
          dplyr::transmute(from, to, value = weight,
                           title = paste0("weight: ", weight),
                           arrows = "to")
      } else data.frame()

      visNetwork(vis_nodes, vis_edges) |>
        visIgraphLayout(layout = "layout_with_fr") |>
        visNodes(scaling = list(min = 12, max = 46)) |>
        visEdges(color = list(color = "#C9C9C9", highlight = "#E15759"),
                 smooth = list(enabled = TRUE, type = "curvedCW")) |>
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1,
                                           hover = TRUE),
                   nodesIdSelection = FALSE) |>
        visEvents(select = sprintf(
          "function(data){ Shiny.setInputValue('%s', data.nodes); }",
          ns("sel_node"))) |>
        visLegend(addNodes = data.frame(
          label = names(.SENIORITY_COL),
          color = unname(.SENIORITY_COL),
          shape = "dot"), useGroups = FALSE, main = "Seniority")
    })

    # ---- centrality table (Judge row flagged) --------------------------------
    output$centrality <- renderDT({
      df <- centrality_df() |>
        dplyr::transmute(Agent = agent_label, Role = role, Seniority = seniority,
                         Oversight = ifelse(is_oversight, "yes", ""),
                         Msgs = n_msg, Partners = partners,
                         Interactions = interactions, Betweenness = betweenness)
      datatable(df, rownames = FALSE, selection = "none",
                options = list(dom = "t", pageLength = 7, order = list())) |>
        formatStyle("Agent", target = "row",
                    backgroundColor = styleEqual("Judge-Agent", "#FFF3CD"))
    })

    # ---- click a node -> its messages ----------------------------------------
    sel_agent <- reactive({
      sel <- input$sel_node
      if (is.null(sel) || length(sel) == 0) return(NULL)
      sel[[1]]
    })

    output$sel_title <- renderText({
      a <- sel_agent()
      if (is.null(a)) "Click a node to see that agent's messages" else
        paste0("Messages — ", a)
    })

    output$agent_msgs <- renderDT({
      a <- sel_agent()
      if (is.null(a)) return(datatable(data.frame()))
      filtered_data() |>
        dplyr::filter(agent_label == a) |>
        dplyr::arrange(timestamp) |>
        dplyr::transmute(round_hour, channel,
                         public = ifelse(is_public, "public", ""),
                         risk = ifelse(risk_flag, "RISK", ""),
                         content)
    }, options = list(pageLength = 6, scrollX = TRUE))

    # ---- evidence drawer: public/anonymous posts -----------------------------
    public_df <- reactive({
      filtered_data() |>
        dplyr::filter(is_public) |>
        dplyr::arrange(timestamp) |>
        dplyr::mutate(.row = dplyr::row_number())
    })

    output$public_posts <- renderDT({
      public_df() |>
        dplyr::transmute(round_hour, author = agent_label, channel,
                         risk = ifelse(risk_flag, "RISK", ""), content)
    }, selection = "single",
       options = list(pageLength = 6, scrollX = TRUE))

    output$evidence <- renderUI({
      sel <- input$public_posts_rows_selected
      if (is.null(sel) || length(sel) == 0)
        return(div(class = "text-muted",
                   "Select a public / anonymous post on the left to trace it."))
      post <- public_df()[sel, ]

      parent <- if (!is.na(post$responding_to)) {
        communications |>
          dplyr::filter(message_id == post$responding_to) |>
          dplyr::slice(1)
      } else NULL

      recips <- post$recipients[[1]]
      recip_txt <- if (length(recips)) paste(recips, collapse = ", ") else "—"
      mono <- c(post$reacting, post$rationalizing, post$deliberating)
      mono <- mono[!is.na(mono)]
      mono_txt <- if (length(mono)) paste(mono, collapse = " · ") else
        "(no internal monologue recorded)"

      tagList(
        tags$h6(paste0(post$agent_label, " — ", post$channel,
                       " @ ", format(post$round_hour, "%b %d %H:%M"))),
        tags$p(tags$b("Post: "), post$content),
        tags$p(tags$b("Recipients: "), recip_txt),
        if (!is.null(parent) && nrow(parent))
          tags$p(tags$b("In reply to "), parent$agent_label, ": ",
                 tags$i(parent$content))
        else tags$p(tags$b("In reply to: "), "—"),
        tags$hr(),
        tags$p(tags$b("Author's internal monologue:")),
        tags$blockquote(mono_txt)
      )
    })
  })
}
