# libraries
library(shiny)
library(bslib)

# ------------------------------------------------------------------
# Data
# ------------------------------------------------------------------
# Everything the app shows lives in data/. Edit build_repositories_csv.py and
# re-run it rather than editing the CSV by hand (text fields contain commas).
data_dir <- if (dir.exists("data")) "data" else file.path("..", "data")
if (requireNamespace("here", quietly = TRUE) && dir.exists(here::here("data"))) {
  data_dir <- here::here("data")
}

read_data <- function(file) {
  read.csv(file.path(data_dir, file), stringsAsFactors = FALSE,
           na.strings = c("NA", ""), check.names = FALSE, encoding = "UTF-8")
}

repos    <- read_data("repositories.csv")
taxonomy <- read_data("gemet_hierarchical_listing_3_2026.csv")
# community_databases.csv (repositories 25-30) is kept as a separate file and is
# deliberately not loaded here.

# Alphabetical order. The confidence column is kept for maintainers only.
repos <- repos[order(tolower(repos$name)), , drop = FALSE]
rownames(repos) <- NULL

# ------------------------------------------------------------------
# Helpers for multi-valued and numeric fields
# ------------------------------------------------------------------
split_multi <- function(x) {
  if (length(x) == 0 || is.na(x)) return(character(0))
  vals <- trimws(strsplit(as.character(x), ";", fixed = TRUE)[[1]])
  vals[nzchar(vals)]
}

unique_values <- function(col) sort(unique(unlist(lapply(col, split_multi))))

# "Inf" -> Inf, a number -> that number, NA -> NA
as_capacity <- function(x) {
  x <- as.character(x)
  out <- suppressWarnings(as.numeric(x))
  out[!is.na(x) & x == "Inf"] <- Inf
  out
}

# An undocumented (empty) value never excludes a repository: we would be hiding
# it because of a gap in our own data rather than a fact about the repository.
matches_any <- function(cell, wanted) {
  have <- split_multi(cell)
  length(have) == 0 || any(have %in% wanted)
}

make_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

# ------------------------------------------------------------------
# Filter vocabularies
# ------------------------------------------------------------------
ACCESS_CHOICES <- intersect(c("Open", "Restricted", "Closed"), unique_values(repos$access))

# Licence filter: grouped by what the licence lets others do with the data, so
# it works for any licence (CC, UK OGL, public domain, software licences, ...).
# The actual licence names (column `license`) are shown on the cards.
LICENSE_CHOICES <- c(
  "No restrictions"     = "No restrictions",
  "Credit required"     = "Credit required",
  "Share alike"         = "Share alike",
  "Non-commercial only" = "Non-commercial only",
  "Custom"              = "Custom"
)

# Size: the value is the capacity (GB) a repository needs to hold data of that
# size, so the comparison is ">= what I need".
SIZE_BANDS <- c(
  "Up to 50 GB"    = 50,
  "50 GB - 300 GB" = 300,
  "300 GB - 1 TB"  = 1000,
  "More than 1 TB" = 1e6
)

# Embargo: a repository allowing 24 months satisfies someone who needs 12.
EMBARGO_BANDS <- c(
  "6 months"  = 6,
  "12 months" = 12,
  "24 months" = 24,
  "36 months" = 36,
  "No limit"  = Inf
)

YES_NO <- c("Yes", "No")

url_license_chooser <- "https://ufal.github.io/public-license-selector/"

# ------------------------------------------------------------------
# Domain tree (GEMET)
# ------------------------------------------------------------------
taxonomy_nested <- local({
  out <- list()
  for (category in unique(taxonomy$Category)) {
    rows <- taxonomy[taxonomy$Category == category, , drop = FALSE]
    subs <- list()
    for (subcategory in unique(rows$Subcategory)) {
      subs[[subcategory]] <- unique(as.character(rows$Subitem[rows$Subcategory == subcategory]))
    }
    out[[category]] <- subs
  }
  out
})

build_maps <- function(tree) {
  leaf_map <- list(); leaves_under_sub <- list(); leaves_under_cat <- list()
  for (category in names(tree)) {
    cid <- make_id(category)
    cat_leaves <- character(0)
    for (subcategory in names(tree[[category]])) {
      sub_core  <- paste0(cid, "_", make_id(subcategory))
      sub_input <- paste0("sub_", sub_core)
      leaves    <- tree[[category]][[subcategory]]
      leaves_under_sub[[sub_input]] <- leaves
      cat_leaves <- c(cat_leaves, leaves)
      for (leaf in leaves) {
        leaf_map[[paste0("leaf_", sub_core, "_", make_id(leaf))]] <- leaf
      }
    }
    leaves_under_cat[[paste0("cat_", cid)]] <- unique(cat_leaves)
  }
  list(leaf_map = leaf_map, leaves_under_sub = leaves_under_sub,
       leaves_under_cat = leaves_under_cat)
}

maps             <- build_maps(taxonomy_nested)
leaf_map         <- maps$leaf_map
leaves_under_sub <- maps$leaves_under_sub
leaves_under_cat <- maps$leaves_under_cat
TREE_INPUTS      <- c(names(leaves_under_cat), names(leaves_under_sub), names(leaf_map))

# Where a repository's domain sits in the tree, for the card subtitle.
domain_path <- function(leaf) {
  hit <- taxonomy[tolower(taxonomy$Subitem) == tolower(leaf), , drop = FALSE]
  if (nrow(hit) == 0) return(leaf)
  sub <- hit$Subcategory[1]
  # Subcategories like "BIOSPHERE (organisms, ecosystems)" already say it all.
  if (grepl(leaf, sub, ignore.case = TRUE)) sub else paste(sub, "→", leaf)
}

# ------------------------------------------------------------------
# Display helpers. Every undocumented value is shown as UNKNOWN, so the
# wording is identical whichever field is missing.
# ------------------------------------------------------------------
UNKNOWN <- "unknown"

format_list <- function(x) {
  vals <- split_multi(x)
  if (length(vals) == 0) UNKNOWN else paste(vals, collapse = ", ")
}

format_capacity <- function(x) {
  cap <- as_capacity(x)
  if (is.na(cap))       return(UNKNOWN)
  if (is.infinite(cap)) return("no stated limit")
  if (cap >= 1000)      return(paste0(round(cap / 1000, 1), " TB per deposit"))
  paste0(cap, " GB per deposit")
}

format_embargo <- function(supported, months) {
  if (is.na(supported))           return(UNKNOWN)
  if (identical(supported, "No")) return("not offered")
  m <- as_capacity(months)
  if (is.na(m))       return("yes, case by case")
  if (is.infinite(m)) return("yes, no limit")
  paste0("yes, up to ", m, " months")
}

format_yes_no <- function(x) if (is.na(x)) UNKNOWN else tolower(x)

format_eligibility <- function(eligibility, institutions) {
  if (is.na(eligibility)) return(UNKNOWN)
  inst <- split_multi(institutions)
  inst <- if (length(inst)) paste(inst, collapse = ", ") else NULL
  switch(eligibility,
    "Open"           = "any researcher, free",
    "Open with fees" = if (is.null(inst)) "any researcher; fees unless your institution is a member"
                       else paste0("any researcher; free for members (", inst, ")"),
    "Members only"   = if (is.null(inst)) "members only" else paste0("members only (", inst, ")"),
    eligibility)
}

# One line on a card or in a modal. An unknown value is styled differently so
# it does not read as a verified fact at a glance.
fact <- function(label, value) {
  if (is.null(value)) return(NULL)
  tags$li(
    span(class = "k", paste0(label, ": ")),
    if (identical(value, UNKNOWN)) span(class = "v-unknown", value) else value
  )
}

repo_facts <- function(repo) {
  list(
    fact("Access",          format_list(repo$access)),
    fact("Licence",         format_list(repo$license)),
    fact("Size",            format_capacity(repo$max_size_gb)),
    fact("Embargo",         format_embargo(repo$embargo_supported, repo$embargo_max_months)),
    fact("Version control", format_yes_no(repo$version_control)),
    fact("API access",      format_yes_no(repo$api_access)),
    fact("DOI service",     format_yes_no(repo$doi_service)),
    fact("Affiliation required", format_yes_no(repo$affiliation_required))
  )
}

# ------------------------------------------------------------------
# Theme + responsive CSS
# ------------------------------------------------------------------
theme <- bs_theme(
  version = 4,
  bg = "#002fa7",
  fg = "#FFFFFF",
  primary = "#FF6347",
  base_font = font_google("Roboto Mono")
)

custom_css <- "
:root {
  --info-btn-size: 30px;
  --info-nudge-vertical: 0px;
}
body { -webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; }

.hero { padding: 40px 0 10px 0; margin-bottom: 10px; }
.hero h1 { font-weight: 600; line-height: 1.05; letter-spacing: -0.6px; margin-bottom: 8px; font-size: 3rem; color: #FFFFFF; text-align: left !important; }
.title-highlight { color: #FF6347; }
.subtitle { color: #B0C4FF; font-style: italic; margin-bottom: 22px; text-align: left; }

.controls-panel { background: transparent; border: 1px solid #FF6347; border-radius: 12px; padding: 18px; position: relative; }
.controls-panel h4 { font-weight: 600; margin-bottom: 14px; color: #FFFFFF; }
.controls-panel h5 { font-size: 1rem; margin: 0; }
.repo-card { background: transparent; border: 1px solid #FF6347; border-radius: 12px; padding: 18px; margin-bottom: 30px; height: 100%; display: flex; flex-direction: column; }
.repo-card h4 { font-weight: 600; font-size: 1.15rem; margin-bottom: 6px; color: #FFFFFF; }
.repo-card .domain-line { color: #B0C4FF; font-size: 0.8rem; margin-bottom: 10px; }
.repo-card .card-actions { margin-top: auto; padding-top: 10px; display: flex; gap: 8px; flex-wrap: wrap; }
.repo-facts { list-style: none; padding-left: 0; margin-bottom: 0; font-size: 0.85rem; }
.repo-facts li { margin-bottom: 3px; }
.repo-facts .k { color: #B0C4FF; }
.v-unknown { color: #8FA4E8; font-style: italic; }
.result-count { color: #B0C4FF; margin-bottom: 16px; }
.btn-small { padding: 6px 12px; font-size: 0.85rem; border-radius: 8px; }
.btn-selected { background-color: #FF6347 !important; color: #FFFFFF !important; border: 1px solid #FF6347 !important; }

.btn-info-circle {
  background-color: transparent !important;
  color: #FFFFFF !important;
  border: 1px solid rgba(255,255,255,0.85);
  width: var(--info-btn-size);
  height: var(--info-btn-size);
  padding: 0 !important;
  font-size: 1rem;
  font-weight: 700;
  border-radius: 50% !important;
  line-height: calc(var(--info-btn-size) - 2px);
  transition: all 0.15s ease;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  transform: translateY(var(--info-nudge-vertical));
  flex: 0 0 auto;
}
.btn-info-circle:hover, .btn-info-circle:focus {
  background-color: #FF6347 !important;
  border-color: #FF6347 !important;
  transform: translateY(calc(var(--info-nudge-vertical) - 1px));
  box-shadow: 0 6px 18px rgba(0,0,0,0.12);
}
.btn-info-circle:focus-visible { outline: 3px solid rgba(255,99,71,0.22); outline-offset: 2px; }

.btn-outline-danger { color: #FFFFFF; border-color: #FF6347; }
.btn-outline-danger:hover, .btn-outline-danger:focus { color: #FFFFFF; background-color: #FF6347; }

.modal-content { border-radius: 12px; background-color: #4169E1; color: #FFFFFF; }
.modal-content a { color: #FFFFFF; text-decoration: underline; }
.modal-content h5 { font-size: 1rem; font-weight: 600; margin-top: 14px; }
.modal-content .table { color: #FFFFFF; }
.modal-footer .btn { background-color: transparent; color: #FFFFFF; border: 1px solid #FF6347; border-radius: 8px; padding: 6px 12px; font-size: 0.85rem; transition: all 0.2s; }
.modal-footer .btn:hover, .modal-footer .btn:focus { background-color: #FF6347; color: #FFFFFF; border-color: #FF6347; }

.hero .container-fluid { text-align: left; padding-left: 20px; padding-right: 20px; }
.hero-logo { display: flex; justify-content: flex-end; align-items: flex-start; }
.hero-logo img { height: 110px; width: auto; }
@media (max-width: 767px) { .hero-logo { justify-content: flex-start; margin-bottom: 12px; } .hero-logo img { height: 80px; } }
@media (max-width: 767px) { .hero h1 { font-size: 2.2rem; } }

.tree-checkbox { margin-bottom: 6px; font-size: 0.95rem; color: #FFFFFF; }
.tree-checkbox .nested { margin-left: 18px; }
.tree-scroll { max-height: 300px; min-height: 160px; overflow-y: auto; padding: 8px 12px; border-radius: 8px; background: transparent; }
.tree-checkbox label { color: #FFFFFF; }

.controls-panel .form-check-label,
.controls-panel .form-check,
.controls-panel .radio,
.controls-panel .checkbox,
.controls-panel .shiny-input-container label,
.controls-panel .control-label { font-size: 0.95rem; color: #FFFFFF; }

.controls-panel .row-with-icon { display: flex; align-items: flex-start; justify-content: space-between; gap: 12px; margin-bottom: 12px; }
.controls-panel .row-with-icon .input-area { flex: 1 1 auto; min-width: 0; }
.controls-panel .row-with-icon .icon-area  { flex: 0 0 auto; display: flex; align-items: flex-start; justify-content: center; }
.controls-panel .domain-header { display: flex; align-items: flex-start; justify-content: space-between; gap: 12px; margin-bottom: 8px; }
.controls-panel .domain-tree { width: 100%; }
.controls-panel .reset-row { margin-top: 8px; }

@media (max-width: 576px) {
  .controls-panel { padding: 12px; }
  .repo-card { padding: 12px; }
  .controls-panel .row-with-icon { flex-direction: column; align-items: stretch; }
  .controls-panel .row-with-icon .icon-area { justify-content: flex-start; margin-top: 6px; }
}

.tree-scroll::-webkit-scrollbar { width: 8px; }
.tree-scroll::-webkit-scrollbar-track { background: transparent; border-radius: 8px; }
.tree-scroll::-webkit-scrollbar-thumb { background: rgba(255,255,255,0.12); border-radius: 8px; border: 2px solid transparent; background-clip: padding-box; }
.tree-scroll::-webkit-scrollbar-thumb:hover { background: rgba(255,255,255,0.20); }
.tree-scroll { scrollbar-width: thin; scrollbar-color: rgba(255,255,255,0.12) transparent; }
"

# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------
filter_row <- function(input_tag, info_id) {
  div(class = "row-with-icon",
      div(class = "input-area", input_tag),
      div(class = "icon-area", actionButton(info_id, "i", class = "btn-info-circle")))
}

ui <- fluidPage(
  theme = theme,
  tags$head(tags$style(HTML(custom_css))),

  div(class = "container-fluid hero",
      fluidRow(
        column(8,
               h1(HTML('Repository <span class="title-highlight">Finder</span>')),
               p(class = "subtitle",
                 "Find a repository for your ecological and environmental research data.")
        ),
        column(4, class = "hero-logo",
               tags$img(src = "nature_data_cube_logo.png", alt = "LTER-LIFE Nature Data Cube"))
      )
  ),

  fluidRow(
    column(width = 3,
           div(class = "controls-panel",
               h4("Selection criteria"),

               # 1. Domain
               div(class = "domain-header",
                   h5("Domain"),
                   actionButton("info_domain", "i", class = "btn-info-circle")),
               div(class = "domain-tree", div(class = "tree-scroll", uiOutput("category_ui"))),

               tags$hr(style = "border-color: rgba(255,99,71,0.15);"),

               # 2. Access
               filter_row(checkboxGroupInput("access", "Access", choices = ACCESS_CHOICES), "info_access"),

               # 3. Licence
               filter_row(checkboxGroupInput("license", "Licence", choices = LICENSE_CHOICES), "info_license"),

               # 4. Size
               filter_row(radioButtons("size", "Size of your data",
                                       choices = c("Any size" = "any", setNames(names(SIZE_BANDS), names(SIZE_BANDS))),
                                       selected = "any"),
                          "info_size"),

               tags$hr(style = "border-color: rgba(255,99,71,0.10);"),

               # 5 + 6. Embargo
               filter_row(checkboxInput("need_embargo", "I need an embargo", value = FALSE), "info_embargo"),
               conditionalPanel(
                 condition = "input.need_embargo == true",
                 style = "margin-left: 18px;",
                 radioButtons("embargo_length", "For at least",
                              choices = c("Any length" = "any", setNames(names(EMBARGO_BANDS), names(EMBARGO_BANDS))),
                              selected = "any")
               ),

               tags$hr(style = "border-color: rgba(255,99,71,0.10);"),

               # 7-9. Version control, API, DOI
               filter_row(checkboxGroupInput("version_control", "Version control", choices = YES_NO, inline = TRUE), "info_version"),
               filter_row(checkboxGroupInput("api_access", "API access", choices = YES_NO, inline = TRUE), "info_api"),
               filter_row(checkboxGroupInput("doi_service", "DOI service", choices = YES_NO, inline = TRUE), "info_doi"),
               filter_row(checkboxGroupInput("affiliation_required", "Affiliation required", choices = YES_NO, inline = TRUE), "info_affiliation"),

               div(class = "reset-row",
                   actionButton("reset", "Clear filters", class = "btn-outline-danger btn-small mt-2"))
           )
    ),
    column(width = 9,
           uiOutput("compare_ui"),
           uiOutput("result_count"),
           uiOutput("repo_grid"))
  )
)

# ------------------------------------------------------------------
# Server
# ------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Domain tree --------------------------------------------------
  output$category_ui <- renderUI({
    lapply(names(taxonomy_nested), function(category) {
      cid <- make_id(category)
      div(class = "tree-checkbox",
          checkboxInput(paste0("cat_", cid), label = category, value = FALSE),
          conditionalPanel(
            condition = paste0("input.cat_", cid, " == true"),
            class = "nested",
            lapply(names(taxonomy_nested[[category]]), function(subcategory) {
              sub_core <- paste0(cid, "_", make_id(subcategory))
              tagList(
                checkboxInput(paste0("sub_", sub_core), label = subcategory, value = FALSE),
                conditionalPanel(
                  condition = paste0("input.sub_", sub_core, " == true"),
                  class = "nested",
                  lapply(taxonomy_nested[[category]][[subcategory]], function(leaf) {
                    checkboxInput(paste0("leaf_", sub_core, "_", make_id(leaf)), label = leaf, value = FALSE)
                  })
                )
              )
            })
          )
      )
    })
  })

  # The most specific ticked level wins: leaves, then subcategories, then categories.
  allowed_domains <- reactive({
    ticked <- function(ids) ids[vapply(ids, function(id) isTRUE(input[[id]]), logical(1))]
    leaves <- ticked(names(leaf_map))
    if (length(leaves)) return(tolower(unlist(leaf_map[leaves])))
    subs <- ticked(names(leaves_under_sub))
    if (length(subs)) return(tolower(unlist(leaves_under_sub[subs])))
    cats <- ticked(names(leaves_under_cat))
    if (length(cats)) return(tolower(unlist(leaves_under_cat[cats])))
    character(0)
  })

  # ---- Filtering ----------------------------------------------------
  filtered_data <- reactive({
    data <- repos
    keep <- function(rows) data[rows, , drop = FALSE]

    # 1. Domain - generalist repositories are never excluded
    domains <- allowed_domains()
    if (length(domains)) {
      data <- keep(vapply(seq_len(nrow(data)), function(i) {
        if (identical(data$domain_scope[i], "any")) return(TRUE)
        have <- tolower(split_multi(data$domain[i]))
        length(have) == 0 || any(have %in% domains)
      }, logical(1)))
    }

    # 2-3. Access and licence - repositories offering any ticked value
    if (length(input$access))
      data <- keep(vapply(data$access, matches_any, logical(1), wanted = input$access))
    if (length(input$license))
      data <- keep(vapply(data$license_terms, matches_any, logical(1), wanted = input$license))

    # 4. Size - per-deposit capacity of at least the chosen band
    if (!is.null(input$size) && input$size != "any") {
      cap <- as_capacity(data$max_size_gb)
      data <- keep(is.na(cap) | cap >= SIZE_BANDS[[input$size]])
    }

    # 5-6. Embargo - supported, and for at least the chosen length
    if (isTRUE(input$need_embargo)) {
      data <- keep(is.na(data$embargo_supported) | data$embargo_supported != "No")
      if (!is.null(input$embargo_length) && input$embargo_length != "any") {
        months <- as_capacity(data$embargo_max_months)
        data <- keep(is.na(months) | months >= EMBARGO_BANDS[[input$embargo_length]])
      }
    }

    # 7-10. Yes / No criteria
    for (col in c("version_control", "api_access", "doi_service", "affiliation_required")) {
      wanted <- input[[col]]
      if (length(wanted)) data <- keep(is.na(data[[col]]) | data[[col]] %in% wanted)
    }

    data
  })

  output$result_count <- renderUI({
    n <- nrow(filtered_data())
    if (n == 0) return(NULL)
    p(class = "result-count", sprintf("%d %s", n, if (n == 1) "repository" else "repositories"))
  })

  # ---- Reset --------------------------------------------------------
  observeEvent(input$reset, {
    for (id in TREE_INPUTS) updateCheckboxInput(session, id, value = FALSE)
    for (id in c("access", "license", "version_control", "api_access", "doi_service", "affiliation_required"))
      updateCheckboxGroupInput(session, id, selected = character(0))
    updateRadioButtons(session, "size", selected = "any")
    updateCheckboxInput(session, "need_embargo", value = FALSE)
    updateRadioButtons(session, "embargo_length", selected = "any")
  })

  # ---- Compare selection --------------------------------------------
  selections <- reactiveValues(selected = character(0))

  lapply(repos$name, function(repo) {
    observeEvent(input[[paste0("sel_", make_id(repo))]], {
      if (repo %in% selections$selected) {
        selections$selected <- setdiff(selections$selected, repo)
      } else {
        selections$selected <- union(selections$selected, repo)
      }
    }, ignoreInit = TRUE)
  })

  output$compare_ui <- renderUI({
    n <- length(selections$selected)
    if (n >= 2) {
      div(style = "display:flex; align-items:center; gap:8px; margin-bottom: 12px;",
          actionButton("compare_btn", "Compare repositories", class = "btn-small btn-primary"),
          span(paste0(n, " selected")))
    } else if (n == 1) {
      div(style = "margin-bottom: 12px; color: #B0C4FF;", "Select 1 more repository to enable comparison.")
    }
  })

  # ---- Cards --------------------------------------------------------
  output$repo_grid <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(h4("No repositories match your selection", style = "color: #B0C4FF;"))
    selected <- selections$selected

    cards <- lapply(seq_len(nrow(data)), function(i) {
      repo <- data[i, , drop = FALSE]
      rid  <- make_id(repo$name)
      is_selected <- repo$name %in% selected
      domains <- split_multi(repo$domain)

      column(width = 6, class = "mb-4",
        div(class = "repo-card",
            h4(repo$name),
            if (identical(repo$domain_scope, "any")) div(class = "domain-line", "Generalist - any domain")
            else if (length(domains)) div(class = "domain-line", paste(vapply(domains, domain_path, character(1)), collapse = " · ")),
            tags$ul(class = "repo-facts", repo_facts(repo)),
            div(class = "card-actions",
                actionButton(paste0("sel_", rid), if (is_selected) "Selected" else "Compare",
                             class = if (is_selected) "btn-small btn-selected" else "btn-outline-danger btn-small"),
                actionButton(paste0("req_", rid), "Requirements", class = "btn-outline-danger btn-small"))
        )
      )
    })
    rows <- split(cards, ceiling(seq_along(cards) / 2))
    tagList(lapply(rows, function(r) do.call(fluidRow, unname(r))))
  })

  # ---- Requirements modal --------------------------------------------
  lapply(seq_len(nrow(repos)), function(i) {
    repo <- repos[i, , drop = FALSE]
    observeEvent(input[[paste0("req_", make_id(repo$name))]], {
      text_or_unknown <- function(x) if (is.na(x)) UNKNOWN else x
      asks <- list(
        fact("Metadata",      text_or_unknown(repo$metadata_req)),
        fact("Data standard", text_or_unknown(repo$data_standard_req)),
        fact("Licence",       text_or_unknown(repo$license_req))
      )
      offers <- c(
        repo_facts(repo),
        list(fact("Who can deposit", format_eligibility(repo$eligibility, repo$institutions)))
      )
      showModal(modalDialog(
        title = repo$name,
        h5("What this repository asks of you"),
        tags$ul(class = "repo-facts", asks),
        h5("What it offers you"),
        tags$ul(class = "repo-facts", offers),
        if (!is.na(repo$notes)) tagList(h5("Good to know"), p(style = "font-size: 0.85rem;", repo$notes)),
        if (!is.na(repo$url)) p(tags$a(href = repo$url, target = "_blank", "Repository policy page")),
        easyClose = TRUE, size = "l", footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)
  })

  # ---- Compare modal --------------------------------------------------
  observeEvent(input$compare_btn, {
    sels <- repos[repos$name %in% selections$selected, , drop = FALSE]
    if (nrow(sels) < 2) return()

    or_dash <- function(x) if (is.null(x)) "–" else x
    rows <- list(
      "Access"          = function(r) format_list(r$access),
      "Licence"         = function(r) format_list(r$license),
      "Size"            = function(r) format_capacity(r$max_size_gb),
      "Embargo"         = function(r) format_embargo(r$embargo_supported, r$embargo_max_months),
      "Version control" = function(r) format_yes_no(r$version_control),
      "API access"      = function(r) format_yes_no(r$api_access),
      "DOI service"     = function(r) format_yes_no(r$doi_service),
      "Affiliation required" = function(r) format_yes_no(r$affiliation_required),
      "Who can deposit" = function(r) format_eligibility(r$eligibility, r$institutions)
    )
    tbl <- tags$table(
      class = "table table-striped table-sm",
      tags$thead(tags$tr(tags$th(""), lapply(sels$name, function(n) tags$th(n)))),
      tags$tbody(lapply(names(rows), function(label) {
        tags$tr(tags$th(style = "white-space: nowrap;", label),
                lapply(seq_len(nrow(sels)), function(i) {
                  tags$td(style = "max-width: 220px; word-break: break-word;",
                          or_dash(rows[[label]](sels[i, , drop = FALSE])))
                }))
      }))
    )
    showModal(modalDialog(
      title = "Repository comparison",
      div(style = "max-width:100%; overflow-x:auto;", tbl),
      easyClose = TRUE, size = "l", footer = modalButton("Close")
    ))
  })

  # ---- Info modals ----------------------------------------------------
  info <- function(id, title, body) {
    observeEvent(input[[id]], {
      showModal(modalDialog(title = title, HTML(body), easyClose = TRUE, footer = modalButton("Close")))
    })
  }
  info("info_domain", "Domain",
       "Tick a category, subcategory or term. The most specific level you tick is used.")
  info("info_access", "Access",
       "As open as possible, as restricted as necessary.")
  info("info_license", "Licence",
       paste0("Licences are grouped by what they let others do with your data:<ul>",
              "<li><b>No restrictions</b>: anyone may reuse it for anything (public domain, CC0).</li>",
              "<li><b>Credit required</b>: reuse is free as long as you are credited (CC BY, UK Open Government Licence, MIT).</li>",
              "<li><b>Share alike</b>: derived work must be shared under the same terms (CC BY-SA, GPL).</li>",
              "<li><b>Non-commercial only</b>: no commercial reuse (CC BY-NC).</li>",
              "<li><b>Custom</b>: terms are agreed with the repository per deposit.</li></ul>",
              "Need help choosing a licence? <br><a href=\"",
              url_license_chooser, "\" target=\"_blank\">Open the licence chooser</a>"))
  info("info_size", "Size of your data",
       "Shows repositories that can hold at least this much data in one deposit.")
  info("info_embargo", "Embargo",
       "An embargo keeps your files closed until a set date while the dataset description is already public. Choose a length to see repositories that allow an embargo of at least that long.")
  info("info_version", "Version control",
       "Whether you can publish a new version of a dataset while earlier versions stay available.")
  info("info_api", "API access",
       "Whether the repository offers programmatic access to its data or metadata (for example a REST API or OAI-PMH).")
  info("info_affiliation", "Affiliation required",
       "Whether you must belong to a specific university, institute or network to deposit data here (for example Yoda and Git@WUR are for WUR staff only). Repositories that anyone can use, even if they charge a fee to non-members, count as no. The Requirements pop-up shows who can deposit.")
  info("info_doi", "DOI service",
       "Whether the repository issues a DOI (a persistent, citable identifier) for your deposit.")
}

# ------------------------------------------------------------------
shinyApp(ui, server)
