# =============================================================================
# Inside the Glass Office — Shiny App (shared shell)
# VAST Challenge 2026 · Mini-Challenge 1 · Group 14
# -----------------------------------------------------------------------------
# This is the COMMON SHELL only. The three analysis modules live in their own
# files under R/ and are owned by one person each:
#   R/mod_network.R   — Li Xinyue
#   R/mod_temporal.R  — Cheng Yuanyuan
#   R/mod_internal.R  — Yang Yang
#
# DO NOT edit another person's module file. This app.R (shell) is shared:
# announce in the group chat before changing it.
#
# INTERFACE CONTRACT — all three modules follow this:
#   1. The shared sidebar filters are read ONLY here in the main server.
#      Modules never touch input$flt_* directly.
#   2. filtered_data() returns the filtered communications master table
#      (one row per message). Each module receives it as an argument and
#      derives its own structures from it.
#   3. opts() carries display-level switches (availability overlay /
#      public-only / current time range).
#   4. All cross-table joins use round_hour, NOT the per-table round_idx.
#   5. The data/clean/*.rds schema is append-only.
#   The contract (filtered_data shape + mod_*_server(id, filtered_data, opts)
#   signature) must NOT be changed without all three members agreeing.
#
# Run: open this file from the project root (with data/clean/ beneath it) and
#   click Run App.
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(shinyWidgets)
library(DT)

# ---- Load the module files --------------------------------------------------
# local = TRUE so the module functions are defined in the SAME environment as
# the data objects loaded below. Run App evaluates app.R in its own environment;
# without local = TRUE, source() would put the functions in the global env while
# the data (agents, communications, ...) lives in the app env, causing
# "object 'agents' not found" at runtime.
source("R/mod_network.R",  local = TRUE)
source("R/mod_temporal.R", local = TRUE)
source("R/mod_internal.R", local = TRUE)

# =============================================================================
# 0. Load the data layer (clean tables produced by data_prep.R)
# =============================================================================
clean_dir <- "data/clean"
rd <- function(name) readRDS(file.path(clean_dir, paste0(name, ".rds")))

communications     <- rd("communications")        # master table (recipients list-col)
round_profile      <- rd("round_profile")          # per-round summary
rounds_env         <- rd("rounds_env")             # stock / sentiment / headline
env_events_long    <- rd("env_events_long")        # media / unavailability / deadlines (long)
reply_edges_agent  <- rd("reply_edges_agent")      # agent->agent reply weight edges
recipient_edges    <- rd("recipient_edges")        # sender->recipient (message level)
network_nodes      <- rd("network_nodes")          # nodes (volume / seniority / oversight)
tokens_internal    <- rd("tokens_internal")        # tokenised monologue (stop words removed)
sentiment_by_round <- rd("sentiment_by_round")     # net sentiment per round
agents             <- rd("agents")                 # agent dimension table

# ---- Sidebar choices --------------------------------------------------------
round_levels <- sort(unique(communications$round_hour))            # POSIXct, ascending
round_labels <- format(round_levels, "%b %d  %H:%M")               # slider display
agent_choices   <- sort(unique(communications$agent_label))
channel_choices <- sort(unique(communications$channel))

# label <-> round_hour lookup (modules can use it directly if needed)
label_to_round <- setNames(round_levels, round_labels)

# =============================================================================
# SHARED UI — shared sidebar + three tabs
# =============================================================================
ui <- page_sidebar(
  title = "Inside the Glass Office — Visual Forensics (Group 14)",
  theme = bs_theme(version = 5),
  fillable = FALSE,   # let content flow & scroll normally instead of squashing plots
  # ---- Global layout fixes (applies to all three module tabs) ----
  # bslib's fill system gives widgets min-height:0, and plotly draws before the
  # card height is known, so plots collapse into a thin band. Fix = force a real
  # min-height on the widgets AND fire a window 'resize' after load / on tab
  # switch so plotly redraws to fill its container. No module file changes.
  tags$head(
    tags$style(HTML("
      /* text cards: don't clip long 'reading' text */
      .bslib-card, .card { overflow: visible !important; }
    ")),
    tags$script(HTML("
      function __nudge(){ window.dispatchEvent(new Event('resize')); }
      document.addEventListener('DOMContentLoaded', function(){
        setTimeout(__nudge, 300); setTimeout(__nudge, 1200);
      });
      document.addEventListener('shown.bs.tab', function(){ setTimeout(__nudge, 200); });
    "))
  ),
  sidebar = sidebar(
    title = "Shared filters",
    width = 320,
    sliderTextInput(
      "flt_time", "Time range",
      choices  = round_labels,
      selected = c(round_labels[1], round_labels[length(round_labels)]),
      width = "100%"
    ),
    pickerInput(
      "flt_agents", "Agents",
      choices  = agent_choices, selected = agent_choices,
      multiple = TRUE,
      options  = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 2")
    ),
    pickerInput(
      "flt_channels", "Channels",
      choices  = channel_choices, selected = channel_choices,
      multiple = TRUE,
      options  = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 2")
    ),
    materialSwitch("flt_public", "Public channels only", value = FALSE, status = "danger"),
    materialSwitch("flt_avail",  "Availability overlay",  value = FALSE, status = "warning"),
    hr(),
    helpText("Filters apply globally; all three tabs share the same filtered_data().")
  ),
  navset_card_tab(
    nav_panel("Interaction Network",     mod_network_ui("net")),
    nav_panel("Temporal Anomaly",        mod_temporal_ui("tmp")),
    nav_panel("Internal-State Explorer", mod_internal_ui("int"))
  )
)

# =============================================================================
# SHARED SERVER — compute the filtered_data() contract, dispatch to modules
# =============================================================================
server <- function(input, output, session) {

  # ---- Contract core: filtered message master table (one row per message) ----
  filtered_data <- reactive({
    # 1) Time range: map the slider's two labels back to round_hour, keep all
    #    rounds in between
    sel  <- input$flt_time
    i1   <- match(sel[1], round_labels)
    i2   <- match(sel[2], round_labels)
    keep_rounds <- round_levels[seq(min(i1, i2), max(i1, i2))]

    df <- communications |>
      dplyr::filter(round_hour %in% keep_rounds)

    # 2) agents
    if (!is.null(input$flt_agents))
      df <- df |> dplyr::filter(agent_label %in% input$flt_agents)

    # 3) channels (+ public-only further narrows within the selected channels)
    if (!is.null(input$flt_channels))
      df <- df |> dplyr::filter(channel %in% input$flt_channels)
    if (isTRUE(input$flt_public))
      df <- df |> dplyr::filter(is_public)

    df
  })

  # ---- Display switches / current range, for overlays or titles in modules ----
  opts <- reactive({
    sel <- input$flt_time
    i1  <- match(sel[1], round_labels); i2 <- match(sel[2], round_labels)
    list(
      avail_overlay = isTRUE(input$flt_avail),
      public_only   = isTRUE(input$flt_public),
      time_start    = round_levels[min(i1, i2)],
      time_end      = round_levels[max(i1, i2)]
    )
  })

  # ---- Dispatch to the three modules (uniform interface: id, filtered_data, opts) ----
  mod_network_server ("net", filtered_data, opts)
  mod_temporal_server("tmp", filtered_data, opts)
  mod_internal_server("int", filtered_data, opts)
}

shinyApp(ui, server)
