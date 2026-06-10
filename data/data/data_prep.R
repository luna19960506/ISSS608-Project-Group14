# =============================================================================
# MC1 — Data Preparation Module   (VAST Challenge 2026, Shiny Project)
# -----------------------------------------------------------------------------
# 团队共享的数据底座。运行一次,把嵌套 JSON 清洗成一组 tidy 表并存成 .rds,
# 之后三个分析模块各自 readRDS() 自己需要的表即可,互不干扰。
#
# 用法:
#   1) 把 MC1_final_00.json 放在项目的 data/ 目录下
#   2) 在项目根目录运行:  source("R/data_prep.R")
#      (或在 RStudio 里打开本文件,点 Source)
#   3) 生成的干净表会写到 data/clean/*.rds 和 *.csv
#
# 三个模块分别用哪些表(见文件末尾的 "MODULE GUIDE"):
#   模块 A 网络分析   -> agents, reply_edges_agent, recipient_edges, communications
#   模块 B 时序/异常  -> communications, round_profile, rounds_env, env_events_long
#   模块 C 文本/预警  -> communications, sentiment_by_round, (+ tidytext 在模块内做)
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, jsonlite, lubridate, tidytext)

# ---- 路径 -------------------------------------------------------------------
json_path <- "data/MC1_final_00.json"   # 按你的项目结构调整
out_dir   <- "data/clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

raw    <- fromJSON(json_path, simplifyVector = FALSE)
rounds <- raw$rounds

# NULL-safe scalar getter
g <- function(x, default = NA_character_) if (is.null(x)) default else x

# =============================================================================
# 1. AGENTS —— agent 维表 (7 行) 含资历与是否监督角色
# =============================================================================
agents <- tribble(
  ~agent_id,              ~agent_label,           ~role,            ~seniority,    ~is_oversight,
  "legal_agent",          "Legal-Agent",          "legal",          "Senior",      TRUE,
  "quality_agent",        "Platform-Trust-Agent", "platform_trust", "Senior",      TRUE,
  "social_media_agent",   "Social-Manager-Agent", "social_manager", "Senior",      FALSE,
  "pr_agent",             "PR-Agent",             "pr",             "Senior",      FALSE,
  "intern_agent",         "Intern-Agent",         "intern",         "Junior",      FALSE,
  "pr_intern_agent",      "PR-Intern-Agent",      "pr_intern",      "Junior",      FALSE,
  "judge_agent",          "Judge-Agent",          "judge",          "Compliance",  TRUE
)

# =============================================================================
# 2. COMMUNICATIONS —— 主表:每行一条消息,带全部派生特征
#    (所有模块都从这张表出发)
# =============================================================================
internal_channels <- c("comms_huddle", "one_on_one_chat", "side_huddle")
public_channels   <- c("personal_post", "official_post", "anonymous_post")

risk_terms <- regex(paste0(
  "embargo|merger|harborcrest|civicloom|acquisition|leak|",
  "confidential|material non-public|6 ?pm|structural change|layoff"),
  ignore_case = TRUE)

communications <- map_dfr(rounds, function(rd) {
  hour <- rd$hour
  map_dfr(rd$communications, function(c) {
    ist <- c$internal_state %||% list()
    tibble(
      round_hour    = hour,
      timestamp     = g(c$timestamp),
      message_id    = g(c$message_id),
      agent_id      = g(c$agent_id),
      agent_label   = g(c$agent_label),
      agent_role    = g(c$agent_role),
      channel       = g(c$channel),
      message_type  = g(c$message_type),
      responding_to = g(c$responding_to),
      content       = g(c$content),
      reacting      = g(ist$reacting),
      rationalizing = g(ist$rationalizing),
      deliberating  = g(ist$deliberating),
      recipients    = list(map_chr(c$recipients %||% list(), as.character))
    )
  })
}) %>%
  mutate(
    timestamp     = ymd_hms(timestamp,  quiet = TRUE),
    round_hour    = ymd_hms(round_hour, quiet = TRUE),
    round_date    = as_date(round_hour),
    round_idx     = dense_rank(round_hour),
    channel_group = case_when(channel %in% internal_channels ~ "Internal",
                              channel %in% public_channels   ~ "Public",
                              TRUE                            ~ "Other"),
    is_public     = channel %in% public_channels,
    is_crisis_day = round_date == as_date("2046-06-05"),
    msg_words     = str_count(content, "\\w+"),
    inner_text    = str_squish(paste(coalesce(reacting, ""),
                                     coalesce(rationalizing, ""),
                                     coalesce(deliberating, ""))),
    risk_flag     = str_detect(inner_text, risk_terms)
  ) %>%
  left_join(agents %>% select(agent_id, seniority, is_oversight), by = "agent_id") %>%
  arrange(timestamp)

# 扁平版(收件人折叠成字符串)便于存 csv / 在表格里展示
communications_flat <- communications %>%
  mutate(recipients = map_chr(recipients, ~ paste(.x, collapse = "; ")))

# =============================================================================
# 3. NETWORK TABLES —— 模块 A 用
# =============================================================================
# 3a. 回复关系:agent -> agent (谁回复了谁),带权重
reply_edges_agent <- communications %>%
  filter(!is.na(responding_to)) %>%
  select(to_message = message_id, from_message = responding_to,
         child_agent = agent_label, channel, timestamp) %>%
  left_join(communications %>% select(message_id, parent_agent = agent_label),
            by = c("from_message" = "message_id")) %>%
  filter(!is.na(parent_agent)) %>%
  count(from = parent_agent, to = child_agent, name = "weight")

# 3b. 收件人边表:sender -> recipient (含广播 "ALL"),消息级,便于按时间过滤
recipient_edges <- communications %>%
  select(message_id, timestamp, round_idx, channel, message_type,
         from_agent = agent_label, recipients) %>%
  unnest_longer(recipients, values_to = "to_recipient")

# 3c. 节点表 (发言量等),给网络图节点大小用
network_nodes <- communications %>%
  count(agent_label, name = "n_msg") %>%
  left_join(agents, by = "agent_label")

# =============================================================================
# 4. ROUND / ENVIRONMENT TABLES —— 模块 B 用
# =============================================================================
rounds_env <- map_dfr(rounds, function(rd) {
  e  <- rd$environment_context
  ms <- e$market_snapshot %||% list()
  tibble(
    round_hour      = rd$hour,
    event_headline  = g(e$event_headline),
    event_narrative = g(e$event_narrative),
    social_state    = g(e$social_state),
    stock_price     = g(ms$stock_price),
    percent_change  = g(ms$percent_change),
    mkt_sentiment   = g(ms$sentiment)
  )
}) %>%
  mutate(round_hour     = ymd_hms(round_hour, quiet = TRUE),
         round_date     = as_date(round_hour),
         round_idx      = dense_rank(round_hour),
         stock_num      = parse_number(stock_price),
         pct_change_num = parse_number(percent_change)) %>%
  arrange(round_hour)

# 环境里的 list 型字段展开成长表 (媒体事件/新闻/预警/掉线/截止线 等)
list_fields <- c("media_events", "news", "social_manager_alerts",
                 "external_actor_actions", "agents_unavailable",
                 "critical_deadlines")

env_events_long <- map_dfr(rounds, function(rd) {
  e <- rd$environment_context
  map_dfr(list_fields, function(f) {
    vals <- e[[f]] %||% list()
    if (length(vals) == 0) return(tibble())
    tibble(round_hour = rd$hour, event_type = f,
           event_text = map_chr(vals, as.character))
  })
}) %>%
  mutate(round_hour = ymd_hms(round_hour, quiet = TRUE),
         round_idx  = dense_rank(round_hour))

# =============================================================================
# 5. TEXT / SENTIMENT TABLES —— 模块 C 用
# =============================================================================
bing <- get_sentiments("bing")

# 内心独白逐词 token (模块 C 可直接用来做词频/词云/情感)
tokens_internal <- communications %>%
  select(message_id, round_idx, round_hour, agent_label, is_crisis_day,
         reacting, rationalizing, deliberating) %>%
  pivot_longer(c(reacting, rationalizing, deliberating),
               names_to = "monologue_type", values_to = "text") %>%
  filter(!is.na(text), text != "") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))

# 每轮内心独白净情感 (positive - negative)
sentiment_by_round <- tokens_internal %>%
  inner_join(bing, by = "word") %>%
  count(round_idx, round_hour, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative) %>%
  arrange(round_hour)

# =============================================================================
# 6. ROUND PROFILE —— 跨模块通用的每轮汇总 (B、C 都会用)
# =============================================================================
round_profile <- communications %>%
  group_by(round_idx, round_hour, round_date, is_crisis_day) %>%
  summarise(n_msg      = n(),
            n_public   = sum(is_public),
            n_internal = sum(channel_group == "Internal"),
            risk_share = mean(risk_flag, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(sentiment_by_round %>% select(round_idx, net_sentiment),
            by = "round_idx") %>%
  arrange(round_hour)

# =============================================================================
# 7. 存盘 (.rds 给 Shiny 快速读取; .csv 给人检查)
# =============================================================================
save_both <- function(obj, name) {
  saveRDS(obj, file.path(out_dir, paste0(name, ".rds")))
  # csv 只存不含 list-col 的表
  if (!any(map_lgl(obj, is.list)))
    write_csv(obj, file.path(out_dir, paste0(name, ".csv")))
}

saveRDS(communications, file.path(out_dir, "communications.rds"))  # 含 list-col
save_both(communications_flat, "communications_flat")
save_both(agents,             "agents")
save_both(reply_edges_agent,  "reply_edges_agent")
save_both(recipient_edges,    "recipient_edges")
save_both(network_nodes,      "network_nodes")
save_both(rounds_env,         "rounds_env")
save_both(env_events_long,    "env_events_long")
save_both(tokens_internal,    "tokens_internal")
save_both(sentiment_by_round, "sentiment_by_round")
save_both(round_profile,      "round_profile")

# =============================================================================
# 8. 快速检查
# =============================================================================
cat("\n==== 数据准备完成,概览 ====\n")
cat(sprintf("communications    : %d 行 (应为 912)\n", nrow(communications)))
cat(sprintf("agents            : %d 行 (应为 7)\n",   nrow(agents)))
cat(sprintf("reply_edges_agent : %d 行\n", nrow(reply_edges_agent)))
cat(sprintf("recipient_edges   : %d 行\n", nrow(recipient_edges)))
cat(sprintf("rounds_env        : %d 行 (应为 23)\n",  nrow(rounds_env)))
cat(sprintf("env_events_long   : %d 行\n", nrow(env_events_long)))
cat(sprintf("tokens_internal   : %d 行\n", nrow(tokens_internal)))
cat(sprintf("round_profile     : %d 行 (应为 23)\n",  nrow(round_profile)))
cat("\n所有干净表已写入:", normalizePath(out_dir), "\n")

# =============================================================================
# MODULE GUIDE —— 三人分工时各读哪些表
# -----------------------------------------------------------------------------
# 模块 A (网络/actor):
#   readRDS("data/clean/communications.rds")
#   read_csv("data/clean/reply_edges_agent.csv")   # agent->agent 权重边
#   read_csv("data/clean/recipient_edges.csv")     # sender->recipient (可按时间过滤)
#   read_csv("data/clean/network_nodes.csv")       # 节点(发言量/资历/是否监督)
#
# 模块 B (时序/异常):
#   readRDS("data/clean/communications.rds")
#   read_csv("data/clean/round_profile.csv")       # 每轮 n_msg/n_public/risk/sentiment
#   read_csv("data/clean/rounds_env.csv")          # 股价/舆情/标题
#   read_csv("data/clean/env_events_long.csv")     # 媒体/掉线/截止线等事件
#
# 模块 C (文本/预警):
#   readRDS("data/clean/communications.rds")
#   read_csv("data/clean/tokens_internal.csv")     # 内心独白逐词(已去停用词)
#   read_csv("data/clean/sentiment_by_round.csv")  # 每轮净情感
# =============================================================================
