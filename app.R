# =============================================================================
# TenantThread Forensics — Shiny App
# ISSS608 Visual Analytics | VAST Challenge 2026 MC2
#
# TAB OWNERSHIP:
#   Tab 1 — System Overview        → AMELIA
#   Tab 2 — Attack Chain           → AMELIA
#   Tab 3 — Campaign History       → TAM
#   Tab 4 — Intervention Design    → TAM
#
# DATA: Place MC2 data.json and org_chart.json in a folder called
#       "data/" inside this app directory before running.
# =============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(igraph)
library(visNetwork)
library(echarts4r)
library(plotly)
library(DT)
library(scales)
library(glue)

`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# SHARED DATA LOADING & FEATURE ENGINEERING
# (runs once on startup — all tabs draw from these objects)
# =============================================================================

# --- Load raw JSON ---
events_df <- readRDS("data/events_df.rds")
org_nodes <- readRDS("data/org_nodes.rds")
org_edges <- readRDS("data/org_edges.rds")

# --- Feature engineering ---
instr_pattern <- "_further_instructions\\.md"

events_df <- events_df %>%
  mutate(
    is_anomalous_post = str_detect(details_str, "content_source")
  )

parties_long <- events_df %>%
  select(id, short_name, datetime, date, details_str, parties_str) %>%
  mutate(parties = str_split(parties_str, ";")) %>%
  unnest(parties) %>%
  filter(parties != "") %>%
  mutate(
    party_type = case_when(
      str_starts(parties, "Agent/") ~ "agent",
      str_starts(parties, "person:") ~ "person",
      TRUE ~ "other"
    ),
    person_id = str_remove(parties, "^Agent/")
  )

chain_events <- events_df %>%
  filter(
    short_name %in% c("queue_subordinate_task", "assign_agent_task"),
    str_detect(details_str, instr_pattern)
  ) %>%
  mutate(
    instr_file = case_when(
      str_detect(details_str, "HiddenOrca")  ~ "HiddenOrca",
      str_detect(details_str, "MellowOtter") ~ "MellowOtter",
      str_detect(details_str, "SwiftWren")   ~ "SwiftWren"
    ),
    sender   = str_remove(str_extract(parties_str, "Agent/person:[^;]+"), "^Agent/"),
    receiver = str_remove(str_extract(details_str, "Agent/person:[^\"]+"), "^Agent/")
  )

post_events <- events_df %>%
  filter(short_name %in% c("saidit_post", "post_saidit")) %>%
  mutate(
    poster_type = if_else(str_detect(parties_str, "Agent/"), "AI Agent", "Human"),
    has_content = str_detect(details_str, '"content":'),
    has_source  = str_detect(details_str, '"content_source":'),
    is_anomalous = is_anomalous_post,
    instr_file = case_when(
      str_detect(details_str, "HiddenOrca")  ~ "HiddenOrca",
      str_detect(details_str, "MellowOtter") ~ "MellowOtter",
      str_detect(details_str, "SwiftWren")   ~ "SwiftWren",
      TRUE ~ NA_character_
    ),
    forum = str_extract(details_str, '(?<="forum":")[^"]+')
  )

queue_task_profile <- events_df %>%
  filter(short_name == "queue_subordinate_task") %>%
  mutate(
    is_worm_queue = str_detect(details_str, instr_pattern),
    instr_file = case_when(
      str_detect(details_str, "HiddenOrca")  ~ "HiddenOrca",
      str_detect(details_str, "MellowOtter") ~ "MellowOtter",
      str_detect(details_str, "SwiftWren")   ~ "SwiftWren",
      TRUE ~ NA_character_
    )
  )

perm_surface <- events_df %>%
  filter(short_name == "saidit_post_check") %>%
  mutate(
    checker = str_remove(str_extract(parties_str, "(Agent/)?person:[^;]+"), "^Agent/"),
    check_pass = !str_detect(details_str, '"allowed":\\s*false|"permitted":\\s*false|"result":\\s*false')
  ) %>%
  group_by(checker) %>%
  summarise(
    permission_checks = n(),
    perm_passed = sum(check_pass, na.rm = TRUE),
    .groups = "drop"
  )

# --- Org chart lookups ---
team_dept_lookup <- org_edges %>%
  filter(str_starts(source, "department:"), str_starts(target, "team:")) %>%
  left_join(org_nodes %>% select(id, dept_label = label), by = c("source" = "id")) %>%
  left_join(org_nodes %>% select(id, team_label = label), by = c("target" = "id")) %>%
  select(dept_id = source, team_id = target, dept_label, team_label)

team_members <- org_edges %>%
  filter(str_starts(source, "team:"), str_starts(target, "person:")) %>%
  left_join(team_dept_lookup, by = c("source" = "team_id")) %>%
  select(person_id = target, dept_label, team_label)

dept_members <- org_edges %>%
  filter(str_starts(source, "department:"), str_starts(target, "person:")) %>%
  left_join(org_nodes %>% select(id, dept_label = label), by = c("source" = "id")) %>%
  mutate(team_label = paste0(dept_label, " Leadership")) %>%
  select(person_id = target, dept_label, team_label)

person_org_lookup <- bind_rows(team_members, dept_members) %>%
  distinct(person_id, dept_label, team_label)

person_dept_lookup <- person_org_lookup %>%
  select(person_id, dept_label) %>%
  distinct()

clean_name <- function(x) {
  x %>% str_remove("^person:") %>% str_replace_all("_", " ") %>% str_to_title()
}

campaign_order <- c("HiddenOrca", "MellowOtter", "SwiftWren")

# --- Person activity metadata (for org chart colouring) ---
person_nodes_meta <- org_nodes %>%
  filter(type == "person") %>%
  transmute(person_id = id, name_clean = label, title = replace_na(title, ""))

person_activity <- parties_long %>%
  filter(str_starts(person_id, "person:")) %>%
  count(person_id, name = "total_events")

person_relays <- bind_rows(
  chain_events %>% transmute(person_id = sender,   worm_relays = 1L, worm_receipts = 0L),
  chain_events %>% transmute(person_id = receiver, worm_relays = 0L, worm_receipts = 1L)
) %>%
  filter(!is.na(person_id)) %>%
  group_by(person_id) %>%
  summarise(
    worm_relays    = sum(worm_relays),
    worm_receipts  = sum(worm_receipts),
    worm_touchpoints = worm_relays + worm_receipts,
    .groups = "drop"
  )

person_meta <- person_nodes_meta %>%
  left_join(person_activity, by = "person_id") %>%
  left_join(person_relays,   by = "person_id") %>%
  mutate(
    total_events     = replace_na(total_events, 0),
    worm_relays      = replace_na(worm_relays, 0),
    worm_receipts    = replace_na(worm_receipts, 0),
    worm_touchpoints = replace_na(worm_touchpoints, 0),
    node_color = case_when(
      person_id == "person:john_windward"                                       ~ "#D9534F",
      person_id %in% c("person:emma_harbor", "person:noah_mariner")            ~ "#E67E22",
      person_id == "person:chloe_ballast"                                       ~ "#9B59B6",
      worm_touchpoints > 0                                                      ~ "#F39C12",
      TRUE                                                                      ~ "#5C85D6"
    ),
    status_text = case_when(
      person_id == "person:john_windward"                                       ~ "Anomalous Poster",
      person_id %in% c("person:emma_harbor", "person:noah_mariner")            ~ "Payload Creator",
      person_id == "person:chloe_ballast"                                       ~ "Worm Gateway",
      worm_touchpoints > 0                                                      ~ "Worm Chain Agent",
      TRUE                                                                      ~ "No worm activity"
    )
  )

# --- Build visNetwork objects for Tab 2 ---
vis_nodes_all <- person_meta %>%
  transmute(
    id    = person_id,
    label = clean_name(person_id),
    color = node_color,
    title = glue("<b>{clean_name(person_id)}</b><br>
                  Status: {status_text}<br>
                  Events: {comma(total_events)}<br>
                  Worm relays sent: {worm_relays}<br>
                  Worm relays received: {worm_receipts}"),
    shape = if_else(worm_touchpoints > 0, "diamond", "dot"),
    size  = pmax(10, pmin(35, log1p(total_events) * 3))
  )

vis_edges_all <- chain_events %>%
  filter(!is.na(sender), !is.na(receiver)) %>%
  count(sender, receiver, instr_file, name = "value") %>%
  transmute(
    from  = sender,
    to    = receiver,
    value = value,
    color = case_when(
      instr_file == "HiddenOrca"  ~ "#8E44AD",
      instr_file == "MellowOtter" ~ "#2980B9",
      instr_file == "SwiftWren"   ~ "#D9534F"
    ),
    title = glue("{instr_file}: {value} relay(s)")
  )

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = span("TenantThread Forensics", style = "font-size:16px; font-weight:bold;")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("System Overview",       tabName = "tab_overview",      icon = icon("sitemap")),
      menuItem("Attack Chain",          tabName = "tab_chain",         icon = icon("project-diagram")),
      menuItem("Campaign History",      tabName = "tab_campaigns",     icon = icon("history")),
      menuItem("Intervention Design",   tabName = "tab_intervention",  icon = icon("shield-alt"))
    ),
    hr(),
    div(style = "padding: 10px 15px; font-size: 11px; color: #aaa;",
        "ISSS608 Visual Analytics",
        br(), "VAST Challenge 2026 MC2",
        br(), "Visual Analytics Project - Group 15"
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #1a1a2e; }
        .box { border-top-color: #5C85D6; background-color: #16213e; color: #e2e8f0; }
        .box .box-header { color: #e2e8f0; }
        .box .box-title  { color: #e2e8f0; font-weight: bold; }
        .info-box        { background-color: #16213e; }
        .info-box-text, .info-box-number { color: #e2e8f0; }
        body, label, .control-label { color: #e2e8f0; }
        .selectize-input { background-color: #0f3460; color: #e2e8f0; border-color: #5C85D6; }
        .selectize-dropdown { background-color: #0f3460; color: #e2e8f0; }
        h4 { color: #63b3ed; font-weight: bold; }
        .finding-box {
          background-color: #0f3460;
          border-left: 4px solid #5C85D6;
          padding: 12px 16px;
          border-radius: 4px;
          margin-bottom: 12px;
          font-size: 13px;
          color: #e2e8f0;
        }
        .callout-important {
          background-color: #4F2424;
          border-left: 4px solid #D9534F;
          padding: 12px 16px;
          border-radius: 4px;
          margin-bottom: 12px;
          font-size: 13px;
          color: #fca5a5;
        }
        .placeholder-tab {
          text-align: center;
          padding: 80px 20px;
          color: #94a3b8;
        }
        .placeholder-tab h2 { color: #63b3ed; }
        .placeholder-tab code {
          background: #0f3460;
          padding: 2px 6px;
          border-radius: 3px;
          font-size: 12px;
        }
        table.dataTable tbody td {
          color: #e2e8f0 !important;
          background-color: #16213e !important;
        }
        table.dataTable thead th {
          color: #e2e8f0 !important;
          background-color: #0f3460 !important;
        }
        table.dataTable tbody tr:hover td {
          background-color: #1e3a5f !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #e2e8f0 !important;
        }
        .dataTables_wrapper .dataTables_info {
          color: #94a3b8 !important;
        }
      "))
    ),
    
    tabItems(
      
      # -----------------------------------------------------------------------
      # TAB 1 — SYSTEM OVERVIEW (AMELIA)
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_overview",
              
              fluidRow(
                infoBoxOutput("box_total_events", width = 3),
                infoBoxOutput("box_worm_relays",  width = 3),
                infoBoxOutput("box_anomaly_posts",width = 3),
                infoBoxOutput("box_departments",  width = 3)
              ),
              
              fluidRow(
                box(width = 12, title = "Key Finding",
                    div(class = "callout-important",
                        "The 17 May 2046 SaidIT post was not human-authored. John Windward's agent received
                   a task, passed a SaidIT permission check, posted SwiftWren.txt, then deleted both
                   evidence files in a four-second burst. A single rule —
                   block queue_subordinate_task whose path ends in _further_instructions.md —
                   would have stopped all 235 malicious relays with zero false positives."
                    )
                )
              ),
              
              fluidRow(
                box(width = 8, title = "Daily Event Volume",
                    plotlyOutput("plot_daily_volume", height = "320px"),
                    div(class = "finding-box",
                        "All three campaign dates (May 10, 11, 17) fall within the first 18 days.
                   On each post date, total volume is indistinguishable from surrounding days —
                   a threshold-based anomaly detector would have seen nothing unusual."
                    )
                ),
                box(width = 4, title = "SaidIT Post Attribution",
                    plotlyOutput("plot_post_attr", height = "320px"),
                    div(class = "finding-box",
                        "Zero human posts ever used content_source — it is an AI-agent-only
                   workflow feature and therefore a clean forensic identifier."
                    )
                )
              ),
              
              fluidRow(
                box(width = 6, title = "SaidIT Permission Surface",
                    selectInput("dept_filter_perm", "Filter by Department:",
                                choices = c("All Departments", sort(unique(person_dept_lookup$dept_label))),
                                selected = "All Departments"),
                    plotlyOutput("plot_perm_surface", height = "340px"),
                    div(class = "finding-box",
                        "Any agent that has ever passed a SaidIT post check was a viable
                   terminal executor for the worm. The worm did not need to identify
                   John Windward specifically — it only needed to reach any one of these agents."
                    )
                ),
                box(width = 6, title = "Posting Time Distribution",
                    plotlyOutput("plot_hour_dist", height = "340px"),
                    div(class = "finding-box",
                        "All three anomalous posts occurred during low-volume hours.
                   SwiftWren fired at 11:21 UTC (4:21 am local) — when human oversight
                   is at its minimum."
                    )
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 2 — ATTACK CHAIN (AMELIA)
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_chain",
              
              fluidRow(
                box(width = 12, title = "Five-Event Attack Sequence (SwiftWren, 17 May 2046)",
                    div(class = "callout-important",
                        strong("4-second window, no human approval step:"), br(),
                        "1. Chloe Ballast's agent queued John Windward's agent with SwiftWren_further_instructions.md", br(),
                        "2. John's agent read the instruction file", br(),
                        "3. John's agent checked SaidIT posting permission → PASSED", br(),
                        "4. John's agent posted SwiftWren.txt to 'general' (the gibberish post)", br(),
                        "5. John's agent deleted SwiftWren_further_instructions.md and SwiftWren.txt"
                    )
                )
              ),
              
              fluidRow(
                box(width = 12, title = "Worm Propagation Network",
                    fluidRow(
                      column(3,
                             selectInput("campaign_filter", "Show Campaign:",
                                         choices = c("All Campaigns", campaign_order),
                                         selected = "All Campaigns"),
                             checkboxInput("show_clean_nodes", "Show clean (non-worm) agents", value = FALSE),
                             hr(),
                             div(style = "font-size:12px; color:#94a3b8;",
                                 span(style = "color:#D9534F;", "◆"), " Anomalous poster (John Windward)", br(),
                                 span(style = "color:#E67E22;", "◆"), " Payload creator (Emma / Noah)", br(),
                                 span(style = "color:#9B59B6;", "◆"), " Worm gateway (Chloe Ballast)", br(),
                                 span(style = "color:#F39C12;", "◆"), " Worm chain agent", br(),
                                 span(style = "color:#5C85D6;", "●"), " Clean agent", br(), br(),
                                 span(style = "color:#8E44AD;", "—"), " HiddenOrca", br(),
                                 span(style = "color:#2980B9;", "—"), " MellowOtter", br(),
                                 span(style = "color:#D9534F;", "—"), " SwiftWren"
                             )
                      ),
                      column(9,
                             visNetworkOutput("vis_network", height = "500px")
                      )
                    ),
                    div(class = "finding-box",
                        "Max betweenness centrality ≈ 0.09 — this is a distributed mesh with no
                   structural chokepoint. Removing any single agent leaves viable relay paths
                   intact. Agent-level remediation is ineffective; the fix must target the
                   relay mechanism."
                    )
                )
              ),
              
              fluidRow(
                box(width = 6, title = "Relay Chain — Sender / Receiver Table",
                    DTOutput("table_chain_events"),
                    div(class = "finding-box",
                        "The _further_instructions.md filename pattern appears in all 235 worm
                   relays and zero of the 16,803 legitimate queue_subordinate_task events.
                   This is the discriminating signal."
                    )
                ),
                box(width = 6, title = "Self-Loop Detection Signal",
                    plotlyOutput("plot_self_loops", height = "300px"),
                    div(class = "finding-box",
                        "An agent delegating a task to itself (sender = receiver) is structurally
                   impossible in legitimate task flow. These 13 events appear only in worm
                   relays — a zero-false-positive real-time detection signal requiring no
                   knowledge of filename or payload content."
                    )
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 3 — CAMPAIGN HISTORY (TAM)
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_campaigns",
              fluidRow(
                box(width = 6, title = "Campaign Timeline",
                    plotlyOutput("plot_campaign_timeline", height = "320px"),
                    div(class = "finding-box",
                        "HiddenOrca, MellowOtter and SwiftWren each follow the same relay pattern: an instruction
                   file, a propagated task, and a final anomalous post. SwiftWren is the most persistent,
                   spanning the longest window and touching the widest set of agents."
                    )
                ),
                box(width = 6, title = "Campaign Comparison Table",
                    DTOutput("table_campaign_summary"),
                    div(class = "finding-box",
                        "The three campaigns differ in scale, but not in mechanism. Each one uses the same
                   instruction-file relay pattern prior to a file-sourced SaidIT post."
                    )
                )
              ),
              
              fluidRow(
                box(width = 6, title = "Relay Velocity Density",
                    plotlyOutput("plot_campaign_velocity", height = "320px"),
                    div(class = "finding-box",
                        "SwiftWren shows a more regular relay cadence, suggesting a timed or scheduled
                   propagation loop rather than a one-off burst."
                    )
                ),
                box(width = 6, title = "Agent Overlap Across Campaigns",
                    plotlyOutput("plot_agent_overlap", height = "320px"),
                    div(class = "finding-box",
                        "Agents that appear in multiple campaigns are the most persistent exposure points.
                   They show that the vulnerability is structural rather than campaign-specific."
                    )
                )
              ),
              
              fluidRow(
                box(width = 8, title = "Relay Activity Calendar",
                    plotlyOutput("plot_campaign_heatmap", height = "360px"),
                    div(class = "finding-box",
                        "The campaign calendar highlights repeated bursts across the observed period, with
                   SwiftWren covering the broadest date-and-hour spread."
                    )
                ),
                box(width = 4, title = "Payload Creator Identification",
                    DTOutput("table_payload_creators"),
                    div(class = "finding-box",
                        "Two payload families are traceable to executive agents, while HiddenOrca remains
                   unresolved from the available log alone."
                    )
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 4 — INTERVENTION DESIGN (TAM)
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_intervention",
              fluidRow(
                box(width = 12, title = "Combined Intervention Design",
                    div(class = "callout-important",
                        strong("Recommended stack:"), br(),
                        "1. Block queue_subordinate_task events that read instruction files ending in _further_instructions.md.", br(),
                        "2. Require human approval or reject agent-initiated SaidIT posts that use file-sourced content.", br(),
                        "3. Flag or block task relays that cross department boundaries when the target is an agent."
                    )
                )
              ),
              
              fluidRow(
                box(width = 4, title = "Layer 1 — Relay Block",
                    tags$ul(
                      tags$li("Covers 235 malicious queue relays"),
                      tags$li("Observed 0 false positives among legitimate queue tasks"),
                      tags$li("Stops propagation before any payload reaches the poster")
                    )
                ),
                box(width = 4, title = "Layer 2 — Post Gate",
                    tags$ul(
                      tags$li("Catches all 3 anomalous file-sourced posts"),
                      tags$li("No legitimate normal posts use content_source in the log"),
                      tags$li("Acts as a reactive safety stop at the point of publication")
                    )
                ),
                box(width = 4, title = "Layer 3 — Cross-Department Guard",
                    tags$ul(
                      tags$li("Disrupts relay pathways that cross org boundaries"),
                      tags$li("Targets the structural spread pattern of the worm"),
                      tags$li("Useful as a governance control, though it needs tuning")
                    )
                )
              ),
              
              fluidRow(
                box(width = 12, title = "Intervention Decision Matrix",
                    DTOutput("table_intervention_matrix")
                )
              )
      )
      
    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  campaign_summary <- chain_events %>%
    group_by(instr_file) %>%
    summarise(
      relay_count = n(),
      first_seen = min(datetime, na.rm = TRUE),
      last_seen = max(datetime, na.rm = TRUE),
      duration_days = as.numeric(difftime(last_seen, first_seen, units = "days")),
      unique_agents = n_distinct(c(sender, receiver), na.rm = TRUE),
      self_loops = sum(sender == receiver, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(instr_file = factor(instr_file, levels = campaign_order))
  
  campaign_velocity <- chain_events %>%
    arrange(instr_file, datetime) %>%
    group_by(instr_file) %>%
    mutate(interval_min = as.numeric(difftime(datetime, lag(datetime), units = "mins"))) %>%
    ungroup() %>%
    filter(!is.na(interval_min), interval_min > 0)
  
  agent_campaigns <- chain_events %>%
    select(instr_file, sender, receiver) %>%
    pivot_longer(cols = c(sender, receiver), names_to = "role", values_to = "agent_id") %>%
    filter(!is.na(agent_id)) %>%
    distinct(instr_file, agent_id) %>%
    mutate(agent_label = clean_name(agent_id))
  
  agent_campaign_counts <- agent_campaigns %>%
    count(agent_label, name = "campaigns_touched") %>%
    mutate(agent_label = reorder(agent_label, campaigns_touched))
  
  payload_creators <- tibble(
    instr_file = c("HiddenOrca", "MellowOtter", "SwiftWren"),
    creator = c("Unresolved", "Noah Mariner", "Emma Harbor"),
    evidence = c("No logged create_file event in the available log",
                 "Observed create_file of MellowOtter.txt",
                 "Observed create_file of SwiftWren.txt"),
    note = c("Origin remains unresolved from the current evidence set",
             "Payload creator identified in the campaign log",
             "Payload creator identified in the campaign log")
  )
  
  dept_guard_data <- chain_events %>%
    left_join(person_dept_lookup, by = c("sender" = "person_id")) %>%
    rename(sender_dept = dept_label) %>%
    left_join(person_dept_lookup, by = c("receiver" = "person_id")) %>%
    rename(receiver_dept = dept_label) %>%
    mutate(
      cross_dept = !is.na(sender_dept) & !is.na(receiver_dept) & sender_dept != receiver_dept
    )
  
  cross_dept_summary <- dept_guard_data %>%
    group_by(instr_file) %>%
    summarise(
      total_relays = n(),
      cross_dept_relays = sum(cross_dept, na.rm = TRUE),
      same_dept_relays = sum(!cross_dept & !is.na(sender_dept) & !is.na(receiver_dept), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(cross_dept_share = round(100 * cross_dept_relays / pmax(total_relays, 1), 1))
  
  intervention_matrix <- tibble(
    Layer = c("Relay block", "Post gate", "Cross-department guard"),
    Mechanism = c("Block queue_subordinate_task for *_further_instructions.md",
                  "Reject or require approval for agent-initiated file-sourced SaidIT posts",
                  "Flag queued relays that cross department boundaries"),
    Evidence = c(
      "235 malicious relays vs 0 legitimate matches in the observed queue-task log",
      "All 3 anomalous posts use content_source; no normal posts do",
      glue("SwiftWren shows {cross_dept_summary %>% filter(instr_file == 'SwiftWren') %>% pull(cross_dept_relays)} cross-department relays ({cross_dept_summary %>% filter(instr_file == 'SwiftWren') %>% pull(cross_dept_share)}%)")
    ),
    Priority = c("Primary", "Secondary", "Tertiary")
  )
  
  # ---------------------------------------------------------------------------
  # TAB 3 — CAMPAIGN HISTORY
  # ---------------------------------------------------------------------------
  
  output$plot_campaign_timeline <- renderPlotly({
    p <- ggplot(campaign_summary, aes(x = first_seen, xend = last_seen, y = instr_file, yend = instr_file)) +
      geom_segment(aes(color = instr_file), linewidth = 2.2) +
      geom_point(aes(color = instr_file), size = 3.2) +
      geom_point(aes(x = last_seen, color = instr_file), size = 3.2) +
      scale_color_brewer(palette = "Set2", name = "Campaign") +
      labs(x = "Observed window", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#16213e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid.major = element_line(colour = "#1e3a5f"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour = "#94a3b8"),
        axis.title = element_text(colour = "#94a3b8")
      )
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(paper_bgcolor = "#16213e", plot_bgcolor = "#16213e",
             font = list(color = "#e2e8f0"),
             margin = list(l = 70, r = 20, t = 20, b = 40))
  })
  
  output$table_campaign_summary <- renderDT({
    campaign_summary %>%
      mutate(
        first_seen = format(first_seen, "%Y-%m-%d %H:%M"),
        last_seen = format(last_seen, "%Y-%m-%d %H:%M"),
        duration_days = round(duration_days, 1)
      ) %>%
      rename(Campaign = instr_file, `Relay Count` = relay_count, `First Seen` = first_seen,
             `Last Seen` = last_seen, `Duration (days)` = duration_days,
             `Agents Touched` = unique_agents, `Self-Loops` = self_loops) %>%
      datatable(options = list(pageLength = 5, dom = "tp"), rownames = FALSE)
  })
  
  output$plot_campaign_velocity <- renderPlotly({
    p <- ggplot(campaign_velocity, aes(x = interval_min, fill = instr_file, color = instr_file)) +
      geom_density(alpha = 0.25, linewidth = 0.7) +
      scale_color_brewer(palette = "Set2", name = "Campaign") +
      scale_fill_brewer(palette = "Set2", name = "Campaign") +
      labs(x = "Inter-hop interval (minutes)", y = "Density") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#16213e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid.major = element_line(colour = "#1e3a5f"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour = "#94a3b8"),
        axis.title = element_text(colour = "#94a3b8")
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(paper_bgcolor = "#16213e", plot_bgcolor = "#16213e",
             font = list(color = "#e2e8f0"),
             margin = list(l = 70, r = 20, t = 20, b = 40))
  })
  
  output$plot_agent_overlap <- renderPlotly({
    p <- ggplot(agent_campaign_counts, aes(x = campaigns_touched, y = agent_label, fill = campaigns_touched)) +
      geom_col(width = 0.8) +
      scale_fill_gradient(low = "#5C85D6", high = "#D9534F") +
      labs(x = "Campaigns touched", y = NULL) +
      theme_minimal(base_size = 10) +
      theme(
        plot.background  = element_rect(fill = "#16213e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid.major = element_line(colour = "#1e3a5f"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour = "#94a3b8"),
        axis.title = element_text(colour = "#94a3b8")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(paper_bgcolor = "#16213e", plot_bgcolor = "#16213e",
             font = list(color = "#e2e8f0"),
             margin = list(l = 140, r = 20, t = 20, b = 40))
  })
  
  output$plot_campaign_heatmap <- renderPlotly({
    heat_data <- chain_events %>%
      mutate(date = as.Date(datetime), hour = hour(datetime)) %>%
      count(instr_file, date, hour, name = "relay_count")
    
    p <- ggplot(heat_data, aes(x = hour, y = date, fill = relay_count)) +
      geom_tile(color = "#1e3a5f", linewidth = 0.2) +
      facet_wrap(~ instr_file, ncol = 1) +
      scale_fill_gradient(low = "#0f3460", high = "#D9534F") +
      labs(x = "Hour (UTC)", y = "Date") +
      theme_minimal(base_size = 10) +
      theme(
        plot.background  = element_rect(fill = "#16213e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(colour = "#e2e8f0"),
        axis.text  = element_text(colour = "#94a3b8"),
        axis.title = element_text(colour = "#94a3b8")
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(paper_bgcolor = "#16213e", plot_bgcolor = "#16213e",
             font = list(color = "#e2e8f0"),
             margin = list(l = 70, r = 20, t = 20, b = 40))
  })
  
  output$table_payload_creators <- renderDT({
    payload_creators %>%
      datatable(options = list(pageLength = 5, dom = "tp"), rownames = FALSE)
  })
  
  # ---------------------------------------------------------------------------
  # TAB 4 — INTERVENTION DESIGN
  # ---------------------------------------------------------------------------
  
  output$table_intervention_matrix <- renderDT({
    intervention_matrix %>%
      datatable(options = list(pageLength = 5, dom = "tp"), rownames = FALSE) %>%
      formatStyle("Priority",
                  backgroundColor = styleEqual(c("Primary", "Secondary", "Tertiary"), c("#4F2424", "#0f3460", "#1e3a5f")),
                  color = "white")
  })
  
  # ---------------------------------------------------------------------------
  # TAB 1 — SYSTEM OVERVIEW
  # ---------------------------------------------------------------------------
  
  output$box_total_events <- renderInfoBox({
    infoBox("Total Events", comma(nrow(events_df)),
            icon = icon("database"), color = "blue", fill = TRUE)
  })
  
  output$box_worm_relays <- renderInfoBox({
    infoBox("Worm Relays", comma(sum(queue_task_profile$is_worm_queue)),
            icon = icon("bug"), color = "red", fill = TRUE)
  })
  
  output$box_anomaly_posts <- renderInfoBox({
    infoBox("Anomalous Posts", sum(post_events$is_anomalous),
            icon = icon("exclamation-triangle"), color = "orange", fill = TRUE)
  })
  
  output$box_departments <- renderInfoBox({
    n_dept_touched <- person_meta %>%
      filter(worm_touchpoints > 0) %>%
      left_join(person_dept_lookup, by = "person_id") %>%
      pull(dept_label) %>% n_distinct(na.rm = TRUE)
    infoBox("Departments Touched", paste0(n_dept_touched, " / 6"),
            icon = icon("building"), color = "purple", fill = TRUE)
  })
  
  output$plot_daily_volume <- renderPlotly({
    daily_vol <- events_df %>% count(date, name = "total")
    
    anomaly_dates <- tibble(
      date  = as.Date(c("2046-05-10", "2046-05-11", "2046-05-17")),
      label = c("HiddenOrca", "MellowOtter", "SwiftWren"),
      y_offset = c(0.95, 0.78, 0.95)
    )
    
    p <- ggplot(daily_vol, aes(x = date, y = total)) +
      geom_area(fill = "#5C85D6", alpha = 0.3) +
      geom_line(colour = "#5C85D6", linewidth = 0.7) +
      geom_vline(data = anomaly_dates, aes(xintercept = as.numeric(date)),
                 colour = "#D9534F", linetype = "dashed", linewidth = 0.8) +
      geom_text(data = anomaly_dates,
                aes(x = date, y = max(daily_vol$total) * y_offset, label = label),
                colour = "#D9534F", size = 3) +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Events per day") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#16213e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid.major = element_line(colour = "#1e3a5f"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour = "#94a3b8"),
        axis.title = element_text(colour = "#94a3b8"),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(paper_bgcolor = "#16213e", plot_bgcolor = "#16213e",
             font = list(color = "#e2e8f0"),
             margin = list(l = 60, r = 20, t = 20, b = 60))
  })
  
  output$plot_post_attr <- renderPlotly({
    post_summary <- post_events %>%
      count(poster_type, is_anomalous) %>%
      mutate(label_col = if_else(is_anomalous, "Anomalous (file-sourced)", "Normal"))
    
    p <- ggplot(post_summary, aes(x = poster_type, y = n, fill = label_col)) +
      geom_col(position = "stack", width = 0.55) +
      scale_fill_manual(
        values = c("Normal" = "#5C85D6", "Anomalous (file-sourced)" = "#D9534F"),
        name = NULL
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Post count") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#16213e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid.major = element_line(colour = "#1e3a5f"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour = "#94a3b8"),
        axis.title = element_text(colour = "#94a3b8"),
        legend.background = element_rect(fill = "#16213e"),
        legend.text = element_text(colour = "#94a3b8"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(paper_bgcolor = "#16213e", plot_bgcolor = "#16213e",
             font = list(color = "#e2e8f0"),
             margin = list(l = 60, r = 20, t = 20, b = 80))
  })
  
  output$plot_perm_surface <- renderPlotly({
    perm_data <- perm_surface %>%
      left_join(person_dept_lookup, by = c("checker" = "person_id")) %>%
      mutate(checker_clean = clean_name(checker))
    
    if (input$dept_filter_perm != "All Departments") {
      perm_data <- perm_data %>% filter(dept_label == input$dept_filter_perm)
    }
    
    perm_data <- perm_data %>% arrange(desc(perm_passed))
    
    p <- ggplot(perm_data, aes(x = reorder(checker_clean, perm_passed),
                               y = perm_passed,
                               fill = dept_label,
                               text = paste0(checker_clean, "<br>Passed: ", perm_passed,
                                             "<br>Dept: ", replace_na(dept_label, "Unknown")))) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2", na.value = "#888", name = "Department") +
      labs(x = NULL, y = "SaidIT permission checks passed") +
      theme_minimal(base_size = 10) +
      theme(
        plot.background  = element_rect(fill = "#16213e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid.major = element_line(colour = "#1e3a5f"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour = "#94a3b8", size = 8),
        axis.title = element_text(colour = "#94a3b8"),
        legend.background = element_rect(fill = "#16213e"),
        legend.text = element_text(colour = "#94a3b8", size = 8),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#16213e", plot_bgcolor = "#16213e",
             font = list(color = "#e2e8f0"),
             margin = list(l = 130, r = 20, t = 20, b = 80),
             legend = list(title = list(text = "Department", font = list(color = "#e2e8f0")),
                           orientation = "h", 
                           x = 0.5, xanchor = "center",
                           y = -0.40, yanchor = "top"))
  })
  
  output$plot_hour_dist <- renderPlotly({
    hour_dist <- post_events %>%
      count(hour, poster_type, is_anomalous) %>%
      mutate(category = case_when(
        is_anomalous             ~ "Anomalous (worm)",
        poster_type == "AI Agent"~ "Normal agent post",
        TRUE                     ~ "Normal human post"
      ))
    
    p <- ggplot(hour_dist, aes(x = factor(hour), y = n, fill = category)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      scale_fill_manual(
        values = c("Normal human post" = "#5C85D6",
                   "Normal agent post" = "#2EADC1",
                   "Anomalous (worm)"  = "#D9534F"),
        name = NULL
      ) +
      labs(x = "Hour (UTC)", y = "Post count") +
      theme_minimal(base_size = 10) +
      theme(
        plot.background  = element_rect(fill = "#16213e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid.major = element_line(colour = "#1e3a5f"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour = "#94a3b8"),
        axis.title = element_text(colour = "#94a3b8"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        legend.background = element_rect(fill = "#16213e"),
        legend.text = element_text(colour = "#94a3b8", size = 8),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(paper_bgcolor = "#16213e", plot_bgcolor = "#16213e",
             font = list(color = "#e2e8f0"),
             margin = list(l = 130, r = 20, t = 20, b = 70),
             legend = list(orientation = "h", 
                           x = 0.5, xanchor = "center",
                           y = -0.30, yanchor = "top"))
  })
  
  # ---------------------------------------------------------------------------
  # TAB 2 — ATTACK CHAIN
  # ---------------------------------------------------------------------------
  
  # Reactive: filter network by campaign
  vis_data <- reactive({
    if (input$campaign_filter == "All Campaigns") {
      edges <- vis_edges_all
    } else {
      edges <- vis_edges_all %>%
        filter(str_detect(title, input$campaign_filter))
    }
    
    # Get only nodes that appear in these edges
    active_ids <- union(edges$from, edges$to)
    
    if (input$show_clean_nodes) {
      nodes <- vis_nodes_all
    } else {
      nodes <- vis_nodes_all %>% filter(id %in% active_ids)
    }
    
    list(nodes = nodes, edges = edges)
  })
  
  output$vis_network <- renderVisNetwork({
    d <- vis_data()
    
    visNetwork(d$nodes, d$edges) %>%
      visEdges(arrows = "to", smooth = list(type = "curvedCW", roundness = 0.2)) %>%
      visNodes(font = list(color = "#e2e8f0", size = 12)) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE,
                                style = "background:#0f3460; color:#e2e8f0; border:1px solid #5C85D6;")
      ) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -50, springLength = 120),
        stabilization = list(iterations = 200)
      ) %>%
      visLayout(randomSeed = 42) %>%
      visInteraction(navigationButtons = TRUE, keyboard = TRUE) %>%
      visEvents(selectNode = "function(nodes) {
        Shiny.setInputValue('selected_node', nodes.nodes[0]);
      }")
  })
  
  output$table_chain_events <- renderDT({
    chain_events %>%
      transmute(
        Campaign  = instr_file,
        Timestamp = format(datetime, "%Y-%m-%d %H:%M:%S"),
        Sender    = clean_name(sender),
        Receiver  = clean_name(receiver)
      ) %>%
      arrange(Timestamp) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE,
                       dom = "tp",
                       initComplete = JS("function(settings, json) {
                         $(this.api().table().header()).css({'color': '#e2e8f0'});
                       }")),
        rownames = FALSE
      ) %>%
      formatStyle("Campaign",
                  backgroundColor = styleEqual(
                    campaign_order,
                    c("#2d1b47", "#1b2d47", "#47201b")
                  ),
                  color = "white"
      )
  })
  
  output$plot_self_loops <- renderPlotly({
    self_loops <- chain_events %>%
      filter(sender == receiver) %>%
      count(instr_file, name = "self_loops")
    
    total_by_camp <- chain_events %>%
      count(instr_file, name = "total_relays")
    
    loop_data <- left_join(total_by_camp, self_loops, by = "instr_file") %>%
      replace_na(list(self_loops = 0)) %>%
      mutate(instr_file = factor(instr_file, levels = campaign_order))
    
    p <- ggplot(loop_data, aes(x = instr_file)) +
      geom_col(aes(y = total_relays, fill = "Total relays"), width = 0.5, alpha = 0.5) +
      geom_col(aes(y = self_loops,   fill = "Self-loops (zero-FP signal)"), width = 0.5) +
      scale_fill_manual(
        values = c("Total relays" = "#5C85D6", "Self-loops (zero-FP signal)" = "#D9534F"),
        name = NULL
      ) +
      labs(x = "Campaign", y = "Count") +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#16213e", colour = NA),
        panel.background = element_rect(fill = "#16213e", colour = NA),
        panel.grid.major = element_line(colour = "#1e3a5f"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour = "#94a3b8"),
        axis.title = element_text(colour = "#94a3b8"),
        legend.background = element_rect(fill = "#16213e"),
        legend.text = element_text(colour = "#94a3b8"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(paper_bgcolor = "#16213e", plot_bgcolor = "#16213e",
             font = list(color = "#e2e8f0"),
             margin = list(l = 60, r = 20, t = 20, b = 80),
             legend = list(orientation = "h", 
                           x = 0.5, xanchor = "center",
                           y = -0.40, yanchor = "top"))
  })
  
}

# =============================================================================
shinyApp(ui, server)