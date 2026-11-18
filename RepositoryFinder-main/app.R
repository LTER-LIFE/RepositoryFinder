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

# Alphabetical order. 
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

# Size: the user types how much data they have; a repository matches when its
# per-deposit capacity is at least that amount. Fixed bands were dropped because
# any band either hides repositories that fit the low end of it or shows ones
# too small for the high end.
SIZE_UNITS <- c("GB" = 1, "TB" = 1000)

# Embargo: a repository allowing 24 months satisfies someone who needs 12.
EMBARGO_BANDS <- c(
  "6 months"  = 6,
  "12 months" = 12,
  "24 months" = 24,
  "36 months" = 36,
  "No limit"  = Inf
)

YES_NO <- c("Yes", "No")
FREE_CHOICES <- c("Yes", "No", "Depends on institute")
REGION_CHOICES <- c("Europe", "United States")

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
  # Input ids nested below each checkbox, so unticking a parent can untick them
  children <- list()
  for (category in names(tree)) {
    cid <- make_id(category)
    cat_input <- paste0("cat_", cid)
    cat_leaves <- character(0)
    for (subcategory in names(tree[[category]])) {
      sub_core  <- paste0(cid, "_", make_id(subcategory))
      sub_input <- paste0("sub_", sub_core)
      leaves    <- tree[[category]][[subcategory]]
      leaves_under_sub[[sub_input]] <- leaves
      cat_leaves <- c(cat_leaves, leaves)
      leaf_inputs <- paste0("leaf_", sub_core, "_", make_id(leaves))
      for (j in seq_along(leaves)) leaf_map[[leaf_inputs[j]]] <- leaves[j]
      children[[sub_input]] <- leaf_inputs
      children[[cat_input]] <- c(children[[cat_input]], sub_input, leaf_inputs)
    }
    leaves_under_cat[[cat_input]] <- unique(cat_leaves)
  }
  list(leaf_map = leaf_map, leaves_under_sub = leaves_under_sub,
       leaves_under_cat = leaves_under_cat, children = children)
}

maps             <- build_maps(taxonomy_nested)
leaf_map         <- maps$leaf_map
leaves_under_sub <- maps$leaves_under_sub
leaves_under_cat <- maps$leaves_under_cat
tree_children    <- maps$children
TREE_INPUTS      <- c(names(leaves_under_cat), names(leaves_under_sub), names(leaf_map))

# ------------------------------------------------------------------
# Guided questions ("Help me choose")
# ------------------------------------------------------------------
# One question per filter. Each choice is c(label, short explanation); the
# explanation may be "". The answers are turned into ticked filters by
# apply_wizard_answers() in the server, so a question here only needs a
# matching rule there.
domain_hint <- function(category) {
  key <- toupper(substr(category, 1, 5))
  switch(key,
         "NATUR" = "For example species, habitats, soil, water or climate.",
         "HUMAN" = "For example agriculture, land use, pollution or fisheries.",
         "SOCIA" = "For example policy, management or socio-economic data.",
         "")
}

WIZARD_STEPS <- list(
  list(id = "domain", type = "multi",
       title = "What is your data about?",
       help  = "Pick the topics that fit your dataset. You can refine them afterwards in the domain list.",
       choices = setNames(lapply(names(taxonomy_nested), function(cat)
                            c(sub("^(.)", "\\U\\1", tolower(cat), perl = TRUE), domain_hint(cat))),
                          names(taxonomy_nested))),
  list(id = "size", type = "size",
       title = "How much data do you want to deposit at once?",
       help  = "An estimate is fine. Leave it empty if you do not know yet."),
  list(id = "access", type = "multi",
       title = "Who should be able to get your files?",
       help  = "Pick every option that would work for you.",
       choices = list(
         Open       = c("Open", "Anyone can download the files."),
         Restricted = c("Restricted", "People have to request access or log in."),
         Closed     = c("Closed", "Only the description is public; the files stay private.")
       )[ACCESS_CHOICES]),
  list(id = "embargo", type = "single",
       title = "Do your files need to stay closed for a while after publishing?",
       help  = "This is called an embargo, for example until your paper is out. The description of the dataset is public meanwhile.",
       choices = list(
         no          = c("No", ""),
         any         = c("Yes, but I don't know for how long yet", ""),
         `6 months`  = c("Yes, at least 6 months", ""),
         `12 months` = c("Yes, at least 12 months", ""),
         `24 months` = c("Yes, at least 24 months", ""),
         `36 months` = c("Yes, at least 36 months", ""),
         `No limit`  = c("Yes, with no time limit", "")
       )),
  list(id = "license", type = "multi",
       title = "Under which terms may others reuse your data?",
       help  = "Pick every option you are comfortable with.",
       choices = list(
         `No restrictions`     = c("No restrictions", "Anyone may reuse it for anything (for example CC0)."),
         `Credit required`     = c("Credit required", "Reuse is fine if they cite you (for example CC BY)."),
         `Share alike`         = c("Share alike", "Derived work must be shared under the same terms (for example CC BY-SA)."),
         `Non-commercial only` = c("Non-commercial only", "No commercial reuse (for example CC BY-NC)."),
         Custom                = c("Custom", "Terms are agreed with the repository per deposit.")
       )),
  list(id = "free", type = "single",
       title = "Does storing your data need to be free?",
       help  = "Some repositories are only free if your university or institute is a member.",
       choices = list(
         must     = c("Yes, it must be free", ""),
         institute = c("Free through my institute is also fine", ""),
         paying   = c("Paying is fine", "")
       )),
  list(id = "region", type = "single",
       title = "Where may your data be stored?",
       help  = "Your project, funder or institute may require storage in Europe.",
       choices = list(
         europe   = c("Only in Europe", ""),
         eu_us    = c("In Europe or the United States", ""),
         anywhere = c("Anywhere", "")
       )),
  list(id = "extras", type = "multi",
       title = "Anything else you need?",
       help  = "Pick what you need, or skip this question if none of these matter.",
       choices = list(
         doi_service     = c("A DOI", "A persistent identifier so others can cite your dataset."),
         version_control = c("Version control", "Publish new versions while older ones stay available."),
         api_access      = c("API access", "Get to the data from scripts or other software.")
       ))
)

# Choice labels with a short grey explanation underneath
wizard_choice_names <- function(choices) {
  unname(lapply(choices, function(ch) {
    if (nzchar(ch[2])) tags$span(tags$span(class = "wiz-choice-label", ch[1]),
                                 tags$span(class = "wiz-choice-help", ch[2]))
    else tags$span(class = "wiz-choice-label", ch[1])
  }))
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
    fact("Free to store data", format_yes_no(repo$free_to_store))
  )
}

# ------------------------------------------------------------------
# Theme + responsive CSS
# ------------------------------------------------------------------
# Colours, fonts and sizes follow the NIOO-KNAW website (https://nioo.knaw.nl).
theme <- bs_theme(
  version = 4,
  bg = "#EFF3F4",
  fg = "#082235",
  primary = "#004b85",
  base_font = font_google("Open Sans", wght = c(400, 600, 700)),
  heading_font = font_google("EB Garamond", wght = 600)
)

custom_css <- "
:root {
  --nioo-text: #082235;
  --nioo-title: #004b85;
  --nioo-link: #004b85;
  --nioo-button: #004b85;
  --nioo-accent: #FDD447;
  --nioo-border: #E2dfdc;
  --nioo-page: #EFF3F4;
  --info-btn-size: 22px;
  --info-nudge-vertical: 0px;
}
body { background-color: var(--nioo-page); color: var(--nioo-text); font-size: 16px; line-height: 1.8; -webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; }
h1, h2, h3, h4, h5 { font-family: 'EB Garamond', serif; font-weight: 600; color: var(--nioo-title); line-height: 1.25; margin: 0 0 .35em 0; }
h4 { font-size: 20px; }
h5 { font-size: 18px; }
::selection { background: var(--nioo-accent); }
a { color: var(--nioo-link); text-underline-offset: 40%; }
a:hover, a:focus { color: var(--nioo-title); }
input[type=checkbox], input[type=radio] { accent-color: var(--nioo-link); }

.hero { background: linear-gradient(47deg, rgba(0,75,133,.9) 60%, rgba(8,121,192,.7) 100%); border-radius: 12px; padding: 40px 28px 26px; margin: 20px 0 24px; }
.hero h1 { font-family: 'Open Sans', sans-serif; font-weight: 600; line-height: 1.05; margin-bottom: 8px; font-size: 3rem; color: #FFFFFF; text-align: left !important; }
.subtitle { color: rgba(255,255,255,.9); font-size: 18px; margin-bottom: 14px; text-align: left; }

.controls-panel { background: #FFFFFF; border: 1px solid var(--nioo-border); border-radius: 12px; padding: 18px; position: relative; }
.controls-panel h4 { font-weight: 600; font-size: 24px; margin-bottom: 14px; }
.controls-panel h5 { font-size: 18px; margin: 0; }
.repo-card { background: #FFFFFF; border: 1px solid var(--nioo-border); border-radius: 12px; padding: 18px; margin-bottom: 30px; height: 100%; display: flex; flex-direction: column; }
.repo-card h4 { font-weight: 600; font-size: 22px; margin-bottom: 6px; }

/* Floating compare bar */
.results-col { padding-bottom: 110px; }
.cmp-bar { position: fixed; left: 50%; bottom: 20px; transform: translateX(-50%); z-index: 1030; width: max-content; max-width: calc(100vw - 32px); box-sizing: border-box; display: flex; align-items: center; gap: 16px; flex-wrap: wrap; justify-content: center; background: #FFFFFF; border: 1px solid var(--nioo-border); border-radius: 16px; padding: 12px 16px; box-shadow: 0 8px 28px rgba(8, 34, 53, 0.18); animation: cmp-in .2s ease-out; }
@keyframes cmp-in { from { opacity: 0; transform: translate(-50%, 12px); } to { opacity: 1; transform: translate(-50%, 0); } }
.cmp-chips { display: flex; gap: 8px; flex-wrap: wrap; justify-content: center; }
.cmp-chip { display: inline-flex; align-items: center; gap: 2px; padding: 3px 4px 3px 12px; border-radius: 999px; background: #E6EFF6; color: var(--nioo-text); font-size: 0.88rem; line-height: 1.4; }
.cmp-chip-x { width: 28px; height: 28px; border: 0; background: transparent; border-radius: 50%; color: rgba(8, 34, 53, 0.65); font-size: 18px; line-height: 1; padding: 0; cursor: pointer; }
.cmp-chip-x:hover, .cmp-chip-x:focus { background: rgba(0, 75, 133, 0.15); color: var(--nioo-text); }
.cmp-actions { display: flex; align-items: center; gap: 14px; }
.cmp-hint { font-size: 0.88rem; color: rgba(8, 34, 53, 0.7); }
.cmp-clear { background: none; border: 0; padding: 6px 4px; font-size: 0.88rem; color: rgba(8, 34, 53, 0.7); text-decoration: underline; text-underline-offset: 3px; cursor: pointer; }
.cmp-clear:hover, .cmp-clear:focus { color: var(--nioo-link); }

/* Results split into recommended by LTER-LIFE and other repositories */
.suit-box { border-radius: 14px; padding: 16px 16px 2px; margin-bottom: 24px; }
.suit-rec { background: rgba(0, 75, 133, 0.07); border: 1px solid rgba(0, 75, 133, 0.18); }
.suit-other { background: rgba(8, 34, 53, 0.04); border: 1px solid rgba(8, 34, 53, 0.10); }
.suit-header { display: flex; justify-content: space-between; align-items: center; gap: 10px; margin-bottom: 14px; }
.suit-header h4 { margin: 0; font-size: 22px; }
.suit-other .suit-header h4 { color: rgba(8, 34, 53, 0.75); }
.suit-count { font-family: 'Open Sans', sans-serif; font-size: 0.9rem; font-weight: 400; color: rgba(8, 34, 53, 0.6); margin-left: 6px; }
.suit-cards { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px; margin-bottom: 14px; }
@media (max-width: 991px) { .suit-cards { grid-template-columns: minmax(0, 1fr); } }
.suit-box .repo-card { height: 100%; margin-bottom: 0; }
.suit-box .suit-extra { display: none; }
.suit-box.expanded .suit-extra { display: block; }
.suit-toggle-row { display: flex; justify-content: center; margin: -4px 0 12px; }
.suit-toggle { background: none; border: 0; padding: 6px 8px; font-size: 0.85rem; color: rgba(8, 34, 53, 0.65); text-decoration: underline; text-underline-offset: 3px; cursor: pointer; }
.suit-toggle::after { content: ''; display: inline-block; width: 6px; height: 6px; margin-left: 8px; border-right: 1.5px solid currentColor; border-bottom: 1.5px solid currentColor; transform: translateY(-3px) rotate(45deg); }
.suit-box.expanded .suit-toggle::after { transform: translateY(1px) rotate(-135deg); }
.suit-toggle:hover, .suit-toggle:focus { color: var(--nioo-link); }
.suit-empty { margin: 4px 0 16px; }
.repo-card .repo-desc { font-size: 0.95rem; line-height: 1.6; color: rgba(8,34,53,.75); margin-bottom: 8px; }
.repo-card .card-actions { margin-top: auto; padding-top: 10px; display: grid; grid-template-columns: auto auto; justify-content: start; gap: 8px; }
.repo-facts { list-style: none; padding-left: 0; margin-bottom: 0; font-size: 0.9rem; }
.repo-facts li { margin-bottom: 3px; }
.repo-facts .k { color: var(--nioo-title); font-weight: 600; }
.v-unknown { color: rgba(8,34,53,.55); font-style: italic; }
.muted { color: rgba(8,34,53,.6); }
.result-count { color: rgba(8,34,53,.6); margin-bottom: 16px; }

/* Buttons: NIOO small buttons - dark blue outline, uppercase, pill; hover fills them */
.btn-small { padding: 9px 15px 8px; font-size: 12px; line-height: 1.5; font-weight: 600; text-transform: uppercase; letter-spacing: .6px; border-radius: 1.8rem; border-width: 2px; transition: all .2s ease; }
.btn-store { grid-column: 1 / -1; }
.btn-outline-danger, .btn-primary { color: var(--nioo-button) !important; background-color: transparent !important; border: 2px solid var(--nioo-button) !important; }
.btn-outline-danger:hover, .btn-outline-danger:focus, .btn-primary:hover, .btn-primary:focus { color: #FFFFFF !important; background-color: var(--nioo-button) !important; border-color: var(--nioo-button) !important; box-shadow: none !important; }
.btn-selected { color: #FFFFFF !important; background-color: var(--nioo-button) !important; border: 2px solid var(--nioo-button) !important; }
.btn-selected:hover, .btn-selected:focus { color: var(--nioo-button) !important; background-color: transparent !important; box-shadow: none !important; }

.btn-info-circle {
  background-color: transparent !important;
  color: var(--nioo-link) !important;
  border: 1.5px solid var(--nioo-link);
  width: var(--info-btn-size);
  height: var(--info-btn-size);
  padding: 0 !important;
  font-family: 'Open Sans', sans-serif;
  font-size: 12px;
  font-weight: 600;
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
  background-color: var(--nioo-link) !important;
  color: #FFFFFF !important;
  border-color: var(--nioo-link) !important;
}
.btn-info-circle:focus-visible { outline: 3px solid rgba(0,75,133,0.25); outline-offset: 2px; }

.modal-content { border-radius: 12px; background-color: #FFFFFF; color: var(--nioo-text); border: none; }
.modal-title { font-family: 'EB Garamond', serif; font-weight: 600; color: var(--nioo-title); font-size: 28px; }
.modal-content a { color: var(--nioo-text); text-decoration: underline; }
.modal-content h5 { font-size: 1.15rem; font-weight: 600; margin-top: 14px; }
.modal-content .table { color: var(--nioo-text); }
.modal-content .table th { color: var(--nioo-title); }
.modal-footer .btn { background-color: transparent; color: var(--nioo-button); border: 2px solid var(--nioo-button); border-radius: 1.8rem; padding: 9px 15px 8px; font-size: 12px; font-weight: 600; text-transform: uppercase; letter-spacing: .6px; transition: all .2s ease; }
.modal-footer .btn:hover, .modal-footer .btn:focus { background-color: var(--nioo-button); color: #FFFFFF; border-color: var(--nioo-button); }

.hero .container-fluid { text-align: left; padding-left: 20px; padding-right: 20px; }
.hero-logo { display: flex; justify-content: flex-end; align-items: flex-start; }
.hero-logo img { height: 110px; width: auto; }
@media (max-width: 767px) { .hero-logo { justify-content: flex-start; margin-bottom: 12px; } .hero-logo img { height: 80px; } }
@media (max-width: 767px) { .hero h1 { font-size: 2.2rem; } }

.size-input { display: flex; gap: 8px; align-items: flex-start; }
.size-input .form-group { margin-bottom: 4px; }
.size-input input[type=number] { -moz-appearance: textfield; appearance: textfield; }
.size-input input[type=number]::-webkit-inner-spin-button,
.size-input input[type=number]::-webkit-outer-spin-button { -webkit-appearance: none; margin: 0; }
/* Guided questions */
.wiz-callout { background: #FFF6D6; border: 1px solid #F2D675; border-radius: 10px; padding: 14px 16px; margin-bottom: 18px; font-size: 0.95rem; line-height: 1.5; }
.wiz-callout.done { background: #E6EFF6; border-color: #C9DBEA; }
.wiz-callout strong { display: block; margin-bottom: 4px; }
.wiz-callout .btn-small { margin-top: 10px; }
.wiz-callout .wiz-links { display: flex; gap: 16px; flex-wrap: wrap; margin-top: 6px; }
.wiz-progress { height: 6px; background: var(--nioo-border); border-radius: 3px; overflow: hidden; margin: 6px 0 18px; }
.wiz-progress > div { height: 6px; background: var(--nioo-button); border-radius: 3px; transition: width .2s ease; }
.modal-body:has(.wiz-close) { padding: 32px 40px 16px; }
.wiz-close { position: absolute; top: 10px; right: 12px; width: 44px; height: 44px; padding: 0; border: 0; background: transparent; color: var(--nioo-text); font-size: 30px; line-height: 1; border-radius: 50%; }
.wiz-close:hover, .wiz-close:focus { background: var(--nioo-page); }
.wiz-footer .btn:disabled { opacity: .35; cursor: not-allowed; }
.wiz-step-count { font-size: 0.85rem; font-weight: 600; letter-spacing: .4px; color: rgba(8,34,53,.7); }
.wiz-title { font-size: 28px; margin-bottom: 4px; }
.wiz-help { color: rgba(8,34,53,.7); margin-bottom: 14px; }
.wiz-body .shiny-input-container { width: 100%; }
.wiz-body .checkbox, .wiz-body .radio { border: 1px solid var(--nioo-border); border-radius: 10px; padding: 10px 14px; margin: 0 0 8px; }
.wiz-body .checkbox label, .wiz-body .radio label { display: flex; gap: 12px; align-items: flex-start; width: 100%; margin: 0; padding: 0; cursor: pointer; }
.wiz-body .checkbox input, .wiz-body .radio input { position: static; margin: 5px 0 0; flex: none; }
.wiz-body .checkbox:has(input:checked), .wiz-body .radio:has(input:checked), .wiz-body .form-check:has(input:checked) { border-color: var(--nioo-button); background: #E6EFF6; }
.wiz-choice-label { display: block; font-weight: 600; }
.wiz-choice-help { display: block; font-size: 0.88rem; color: rgba(8,34,53,.7); }
.wiz-footer { display: flex; justify-content: space-between; align-items: center; gap: 12px; flex-wrap: wrap; border-top: 1px solid var(--nioo-border); padding-top: 16px; margin-top: 8px; }
.wiz-footer .right { display: flex; gap: 10px; align-items: center; flex-wrap: wrap; }
.wiz-skip { background: none; border: 0; color: var(--nioo-link); text-decoration: underline; font-size: 0.9rem; padding: 8px 6px; }
.btn-filled { color: #FFFFFF !important; background-color: var(--nioo-button) !important; border: 2px solid var(--nioo-button) !important; }
.btn-filled:hover, .btn-filled:focus { color: var(--nioo-button) !important; background-color: transparent !important; box-shadow: none !important; }
.tree-checkbox { margin-bottom: 6px; font-size: 0.95rem; color: var(--nioo-text); }
.tree-checkbox .nested { margin-left: 18px; }
.tree-scroll { max-height: 300px; min-height: 160px; overflow-y: auto; padding: 8px 12px; border-radius: 8px; border: 1px solid var(--nioo-border); background: transparent; }
.tree-checkbox label { color: var(--nioo-text); }

.controls-panel .form-check-label,
.controls-panel .form-check,
.controls-panel .radio,
.controls-panel .checkbox,
.controls-panel .shiny-input-container label,
.controls-panel .control-label { font-size: 0.95rem; color: var(--nioo-text); }
.controls-panel .control-label { font-weight: 600; }

.controls-panel .row-with-icon { display: flex; align-items: flex-start; justify-content: space-between; gap: 12px; margin-bottom: 12px; }
.controls-panel .row-with-icon .input-area { flex: 1 1 auto; min-width: 0; }
.controls-panel .row-with-icon .icon-area  { flex: 0 0 auto; display: flex; align-items: flex-start; justify-content: center; }
.controls-panel .domain-header { display: flex; align-items: flex-start; justify-content: space-between; gap: 12px; margin-bottom: 8px; }
.controls-panel .domain-tree { width: 100%; }
.controls-panel .reset-row { margin-top: 8px; }
.controls-panel hr { border-color: var(--nioo-border) !important; }

@media (max-width: 576px) {
  .controls-panel { padding: 12px; }
  .repo-card { padding: 12px; }
  .controls-panel .row-with-icon { flex-direction: column; align-items: stretch; }
  .controls-panel .row-with-icon .icon-area { justify-content: flex-start; margin-top: 6px; }
}

.tree-scroll::-webkit-scrollbar { width: 8px; }
.tree-scroll::-webkit-scrollbar-track { background: transparent; border-radius: 8px; }
.tree-scroll::-webkit-scrollbar-thumb { background: rgba(8,34,53,0.15); border-radius: 8px; border: 2px solid transparent; background-clip: padding-box; }
.tree-scroll::-webkit-scrollbar-thumb:hover { background: rgba(8,34,53,0.25); }
.tree-scroll { scrollbar-width: thin; scrollbar-color: rgba(8,34,53,0.15) transparent; }
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
               h1("Repository Finder"),
               p(class = "subtitle",
                 "Find a repository for your ecological and environmental research data.")
        ),
        column(4, class = "hero-logo",
               tags$img(src = "lter_life_logo.svg", alt = "LTER-LIFE"))
      )
  ),

  fluidRow(
    column(width = 3,
           div(class = "controls-panel",
               h4("Selection criteria"),

               # Guided route: answer a few questions and the filters get ticked
               uiOutput("wizard_callout"),

               # Your data: what it is and how big
               # Domain
               div(class = "domain-header",
                   h5("Domain"),
                   actionButton("info_domain", "i", class = "btn-info-circle")),
               div(class = "domain-tree", div(class = "tree-scroll", uiOutput("category_ui"))),

               tags$hr(style = "border-color: rgba(255,99,71,0.15);"),

               # Size
               filter_row(div(class = "form-group shiny-input-container",
                              tags$label(class = "control-label", `for` = "size_amount", "Size of your data"),
                              div(class = "size-input",
                                  numericInput("size_amount", NULL, value = NA, min = 0, step = 1, width = "120px"),
                                  selectInput("size_unit", NULL, choices = names(SIZE_UNITS), selected = "GB", width = "80px")),
                              div(class = "muted", style = "font-size: 0.85rem;", "Leave empty for any size.")),
                          "info_size"),

               tags$hr(style = "border-color: rgba(255,99,71,0.10);"),

               # Dealbreakers: can you use it, and may the data live there
               # Cost
               filter_row(checkboxGroupInput("free_to_store", "Free to store data", choices = FREE_CHOICES, inline = TRUE), "info_free"),
               # Storage location
               filter_row(checkboxGroupInput("storage_region", "Data stored in", choices = REGION_CHOICES, inline = TRUE), "info_region"),

               tags$hr(style = "border-color: rgba(255,99,71,0.10);"),

               # Who may see and reuse the data
               # Access
               filter_row(checkboxGroupInput("access", "Access", choices = ACCESS_CHOICES), "info_access"),

               # Embargo
               filter_row(tagList(tags$label(class = "control-label", "Embargo"),
                                  checkboxInput("need_embargo", "I need an embargo", value = FALSE)), "info_embargo"),
               conditionalPanel(
                 condition = "input.need_embargo == true",
                 style = "margin-left: 18px;",
                 radioButtons("embargo_length", "For at least",
                              choices = c("Any length" = "any", setNames(names(EMBARGO_BANDS), names(EMBARGO_BANDS))),
                              selected = "any")
               ),

               # Licence
               filter_row(checkboxGroupInput("license", "Licence", choices = LICENSE_CHOICES), "info_license"),

               tags$hr(style = "border-color: rgba(255,99,71,0.10);"),

               # Extras
               filter_row(checkboxGroupInput("doi_service", "DOI service", choices = YES_NO, inline = TRUE), "info_doi"),
               filter_row(checkboxGroupInput("version_control", "Version control", choices = YES_NO, inline = TRUE), "info_version"),
               filter_row(checkboxGroupInput("api_access", "API access", choices = YES_NO, inline = TRUE), "info_api"),

               div(class = "reset-row",
                   actionButton("reset", "Clear filters", class = "btn-outline-danger btn-small mt-2"))
           )
    ),
    column(width = 9, class = "results-col",
           uiOutput("result_count"),
           uiOutput("repo_grid"))
  ),

  # Floating compare bar (bottom of the screen, only while repositories are selected)
  uiOutput("compare_bar")
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

  # Unticking a category or subcategory unticks everything below it. Otherwise
  # the hidden children stay ticked and keep filtering.
  lapply(names(tree_children), function(id) {
    observeEvent(input[[id]], {
      if (!isTRUE(input[[id]]))
        for (child in tree_children[[id]]) updateCheckboxInput(session, child, value = FALSE)
    }, ignoreInit = TRUE)
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

    # Domain - generalist repositories are never excluded
    domains <- allowed_domains()
    if (length(domains)) {
      data <- keep(vapply(seq_len(nrow(data)), function(i) {
        if (identical(data$domain_scope[i], "any")) return(TRUE)
        have <- tolower(split_multi(data$domain[i]))
        length(have) == 0 || any(have %in% domains)
      }, logical(1)))
    }

    # Access and licence - repositories offering any ticked value
    if (length(input$access))
      data <- keep(vapply(data$access, matches_any, logical(1), wanted = input$access))
    if (length(input$license))
      data <- keep(vapply(data$license_terms, matches_any, logical(1), wanted = input$license))

    # Size - per-deposit capacity of at least the amount entered
    needed <- suppressWarnings(as.numeric(input$size_amount))
    if (length(needed) == 1 && !is.na(needed) && needed > 0) {
      needed_gb <- needed * SIZE_UNITS[[input$size_unit]]
      cap <- as_capacity(data$max_size_gb)
      data <- keep(is.na(cap) | cap >= needed_gb)
    }

    # Embargo - supported, and for at least the chosen length
    if (isTRUE(input$need_embargo)) {
      data <- keep(is.na(data$embargo_supported) | data$embargo_supported != "No")
      if (!is.null(input$embargo_length) && input$embargo_length != "any") {
        months <- as_capacity(data$embargo_max_months)
        data <- keep(is.na(months) | months >= EMBARGO_BANDS[[input$embargo_length]])
      }
    }

    # Yes / No criteria (and cost)
    for (col in c("version_control", "api_access", "doi_service", "free_to_store")) {
      wanted <- input[[col]]
      if (length(wanted)) data <- keep(is.na(data[[col]]) | data[[col]] %in% wanted)
    }

    # Storage location - repositories storing in any ticked region
    if (length(input$storage_region))
      data <- keep(vapply(data$storage_region, matches_any, logical(1), wanted = input$storage_region))

    data
  })

  output$result_count <- renderUI({
    n <- nrow(filtered_data())
    if (n == 0) return(NULL)
    p(class = "result-count", sprintf("%d %s", n, if (n == 1) "repository" else "repositories"))
  })

  # Unticking "I need an embargo" also resets the hidden length choice
  observeEvent(input$need_embargo, {
    if (!isTRUE(input$need_embargo)) updateRadioButtons(session, "embargo_length", selected = "any")
  }, ignoreInit = TRUE)

  # ---- Guided questions ("Help me choose") ---------------------------
  n_steps <- length(WIZARD_STEPS)
  wiz <- reactiveValues(step = 1, answers = list(), applied = FALSE,
                        expected = NULL, synced = FALSE)

  output$wizard_callout <- renderUI({
    if (isTRUE(wiz$applied)) {
      div(class = "wiz-callout done",
          "The filters below were ticked from your answers. You can still change them.",
          div(class = "wiz-links",
              actionLink("wiz_edit", "Change my answers"),
              actionLink("wiz_restart", "Start over")))
    } else {
      div(class = "wiz-callout",
          tags$strong("Not sure what to pick?"),
          div(actionButton("wiz_open", "Help me choose", class = "btn-small btn-filled")))
    }
  })

  # The input for one question, pre-filled with an earlier answer (going Back)
  wizard_input <- function(step, saved) {
    id <- paste0("wiz_", step$id)
    switch(step$type,
      multi  = checkboxGroupInput(id, NULL, choiceNames = wizard_choice_names(step$choices),
                                  choiceValues = names(step$choices),
                                  selected = if (length(saved)) saved else character(0)),
      single = radioButtons(id, NULL, choiceNames = wizard_choice_names(step$choices),
                            choiceValues = names(step$choices),
                            selected = if (length(saved)) saved else character(0)),
      size   = div(class = "size-input",
                   numericInput("wiz_size_amount", NULL, value = if (length(saved)) saved$amount else NA,
                                min = 0, step = 1, width = "160px"),
                   selectInput("wiz_size_unit", NULL, choices = names(SIZE_UNITS),
                               selected = if (length(saved)) saved$unit else "GB", width = "90px"))
    )
  }

  next_label <- function(i) if (i == n_steps) "Show repositories" else "Next"

  # Answers as they were when the pop-up opened; closing with the x restores
  # them, so nothing from an unfinished round is kept.
  answers_before <- list()

  output$wizard_body <- renderUI({
    i <- wiz$step
    step <- WIZARD_STEPS[[i]]
    tagList(
      div(class = "wiz-step-count", sprintf("Question %d of %d", i, n_steps)),
      div(class = "wiz-progress", div(style = sprintf("width: %d%%;", round(100 * i / n_steps)))),
      h3(class = "wiz-title", step$title),
      p(class = "wiz-help", step$help),
      div(class = "wiz-body", wizard_input(step, isolate(wiz$answers[[step$id]])))
    )
  })

  open_wizard <- function() {
    answers_before <<- isolate(wiz$answers)
    wiz$step <- 1
    showModal(modalDialog(
      actionButton("wiz_close", HTML("&times;"), class = "wiz-close",
                   `aria-label` = "Close without saving", title = "Close without saving"),
      uiOutput("wizard_body"),
      footer = div(class = "wiz-footer", style = "width: 100%;",
                   actionButton("wiz_back", "Previous question", class = "btn-outline-danger btn-small",
                                disabled = TRUE),
                   div(class = "right",
                       actionButton("wiz_skip", "I don't know / skip", class = "wiz-skip"),
                       actionButton("wiz_next", next_label(1), class = "btn-small btn-filled",
                                    disabled = TRUE))),
      size = "l", easyClose = FALSE
    ))
  }

  observeEvent(wiz$step, {
    updateActionButton(session, "wiz_back", disabled = wiz$step == 1)
    updateActionButton(session, "wiz_next", label = next_label(wiz$step))
  })

  # Has the open question been answered? Next only works when it has;
  # "I don't know / skip" is always available.
  current_answered <- reactive({
    step <- WIZARD_STEPS[[wiz$step]]
    if (step$type == "size") {
      amount <- suppressWarnings(as.numeric(input$wiz_size_amount))
      length(amount) == 1 && !is.na(amount) && amount > 0
    } else {
      length(input[[paste0("wiz_", step$id)]]) > 0
    }
  })

  observe({
    updateActionButton(session, "wiz_next", disabled = !current_answered())
  })

  set_answer <- function(id, value) {
    a <- wiz$answers
    a[[id]] <- value
    wiz$answers <- a
  }

  # Store what is currently filled in for the open question
  save_current <- function() {
    step <- WIZARD_STEPS[[wiz$step]]
    value <- if (step$type == "size") {
      amount <- suppressWarnings(as.numeric(input$wiz_size_amount))
      if (length(amount) == 1 && !is.na(amount) && amount > 0) list(amount = amount, unit = input$wiz_size_unit)
    } else {
      v <- input[[paste0("wiz_", step$id)]]
      if (length(v)) v
    }
    set_answer(step$id, value)
  }

  # Turn the answers into ticked filters. A skipped question leaves its filter
  # empty, so an empty answer list clears every filter.
  GROUP_FILTERS <- c("access", "license", "free_to_store", "storage_region",
                     "doi_service", "version_control", "api_access")

  # The filter values that a set of answers stands for
  wizard_target <- function(a) {
    embargo <- if (length(a$embargo)) a$embargo else "no"
    free    <- if (length(a$free)) a$free else "none"
    region  <- if (length(a$region)) a$region else "none"
    t <- list(
      tree           = paste0("cat_", make_id(a$domain)),
      size_amount    = if (length(a$size)) a$size$amount else NA,
      size_unit      = if (length(a$size)) a$size$unit else "GB",
      need_embargo   = embargo != "no",
      embargo_length = if (embargo == "no") "any" else embargo,
      access         = a$access,
      license        = a$license,
      free_to_store  = switch(free, must = "Yes", institute = c("Yes", "Depends on institute"), character(0)),
      storage_region = switch(region, europe = "Europe", eu_us = c("Europe", "United States"), character(0))
    )
    for (col in c("doi_service", "version_control", "api_access"))
      t[[col]] <- if (col %in% a$extras) "Yes" else character(0)
    t
  }

  # One comparable shape for both the target and the live filter values
  normalise_filters <- function(f) {
    chr <- function(x) if (length(x)) sort(as.character(x)) else character(0)
    amount <- suppressWarnings(as.numeric(f$size_amount))
    out <- list(
      tree         = chr(f$tree),
      size_amount  = if (length(amount) == 1 && !is.na(amount) && amount > 0) amount else NA_real_,
      size_unit    = if (length(f$size_unit)) as.character(f$size_unit) else "GB",
      need_embargo = isTRUE(f$need_embargo),
      embargo_length = if (isTRUE(f$need_embargo) && length(f$embargo_length)) f$embargo_length else "any"
    )
    for (col in GROUP_FILTERS) out[[col]] <- chr(f[[col]])
    out
  }

  current_filters <- reactive({
    f <- lapply(c("size_amount", "size_unit", "need_embargo", "embargo_length", GROUP_FILTERS),
                function(id) input[[id]])
    names(f) <- c("size_amount", "size_unit", "need_embargo", "embargo_length", GROUP_FILTERS)
    f$tree <- TREE_INPUTS[vapply(TREE_INPUTS, function(id) isTRUE(input[[id]]), logical(1))]
    normalise_filters(f)
  })

  # The "ticked from your answers" box stays only while the filters still match
  # the answers. The browser needs a moment to tick them, so first wait until
  # they match once; after that, any difference means the user changed a filter.
  observe({
    now <- current_filters()
    expected <- wiz$expected   # also re-check when new answers are applied
    isolate({
      if (!isTRUE(wiz$applied) || is.null(expected)) return()
      if (!isTRUE(wiz$synced)) {
        if (identical(now, expected)) wiz$synced <- TRUE
      } else if (!identical(now, expected)) {
        wiz$applied <- FALSE
      }
    })
  })

  apply_wizard_answers <- function(a) {
    t <- wizard_target(a)
    for (id in TREE_INPUTS) updateCheckboxInput(session, id, value = id %in% t$tree)
    updateNumericInput(session, "size_amount", value = t$size_amount)
    updateSelectInput(session, "size_unit", selected = t$size_unit)
    updateCheckboxInput(session, "need_embargo", value = t$need_embargo)
    updateRadioButtons(session, "embargo_length", selected = t$embargo_length)
    for (col in GROUP_FILTERS)
      updateCheckboxGroupInput(session, col, selected = if (length(t[[col]])) t[[col]] else character(0))
    wiz$expected <- normalise_filters(t)
    wiz$synced <- FALSE
  }

  go_forward <- function() {
    if (wiz$step < n_steps) {
      wiz$step <- wiz$step + 1
    } else {
      apply_wizard_answers(wiz$answers)
      wiz$applied <- TRUE
      removeModal()
    }
  }

  observeEvent(input$wiz_open, open_wizard())
  observeEvent(input$wiz_edit, open_wizard())
  observeEvent(input$wiz_restart, {
    wiz$answers <- list()
    wiz$applied <- FALSE
    apply_wizard_answers(list())
    open_wizard()
  })
  observeEvent(input$wiz_next, {
    if (!current_answered()) return()   # safety net; the button is greyed out then
    save_current()
    go_forward()
  })
  observeEvent(input$wiz_skip, { set_answer(WIZARD_STEPS[[wiz$step]]$id, NULL); go_forward() })
  observeEvent(input$wiz_back, {
    if (wiz$step > 1) {
      save_current()
      wiz$step <- wiz$step - 1
    }
  })
  observeEvent(input$wiz_close, {
    wiz$answers <- answers_before
    removeModal()
  })

  # ---- Reset --------------------------------------------------------
  observeEvent(input$reset, {
    wiz$answers <- list()
    wiz$applied <- FALSE
    for (id in TREE_INPUTS) updateCheckboxInput(session, id, value = FALSE)
    for (id in c("access", "license", "version_control", "api_access", "doi_service", "free_to_store", "storage_region"))
      updateCheckboxGroupInput(session, id, selected = character(0))
    updateNumericInput(session, "size_amount", value = NA)
    updateSelectInput(session, "size_unit", selected = "GB")
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

  # Floating bar at the bottom of the screen while repositories are selected.
  # Its buttons send one-off events from the browser (instead of actionButtons),
  # because the bar is redrawn on every change and a redrawn actionButton can
  # miss a click.
  send_event <- function(id, value = "1")
    sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'});", id, value)

  output$compare_bar <- renderUI({
    sel <- selections$selected
    n <- length(sel)
    if (n == 0) return(NULL)
    chips <- lapply(sel, function(name) {
      tags$span(class = "cmp-chip", name,
                tags$button(type = "button", class = "cmp-chip-x", HTML("&times;"),
                            `aria-label` = paste("Remove", name, "from comparison"),
                            onclick = send_event("cmp_remove", jsonlite::toJSON(name, auto_unbox = TRUE))))
    })
    div(class = "cmp-bar", role = "region", `aria-label` = "Repositories selected for comparison",
        div(class = "cmp-chips", chips),
        div(class = "cmp-actions",
            if (n == 1) tags$span(class = "cmp-hint", "Select one more to compare")
            else tags$button(type = "button", class = "btn btn-small btn-filled",
                             onclick = send_event("cmp_open"), sprintf("Compare (%d)", n)),
            tags$button(type = "button", class = "cmp-clear", onclick = send_event("cmp_clear"), "Clear all")))
  })

  observeEvent(input$cmp_remove, {
    selections$selected <- setdiff(selections$selected, input$cmp_remove)
  })
  observeEvent(input$cmp_clear, {
    selections$selected <- character(0)
  })

  # ---- Cards --------------------------------------------------------
  output$repo_grid <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(h4("No repositories match your selection", class = "muted"))
    selected <- selections$selected

    make_card <- function(i) {
      repo <- data[i, , drop = FALSE]
      rid  <- make_id(repo$name)
      is_selected <- repo$name %in% selected

      div(class = "repo-card",
          h4(repo$name),
          if (!is.null(repo$description) && !is.na(repo$description))
            p(class = "repo-desc", repo$description),
          div(class = "card-actions",
              if (!is.na(repo$website)) tags$a(href = repo$website, target = "_blank", rel = "noopener",
                                           class = "btn btn-outline-danger btn-small btn-store", "Visit the repository's website"),
              actionButton(paste0("sel_", rid), if (is_selected) "Selected" else "Compare",
                           class = if (is_selected) "btn-small btn-selected" else "btn-outline-danger btn-small"),
              actionButton(paste0("req_", rid), "Information", class = "btn-outline-danger btn-small"))
      )
    }

    # Two columns: recommended by LTER-LIFE, and the rest. Anything not marked
    # "Recommended" in lter_life_suitability (including empty) counts as other.
    suit <- if ("lter_life_suitability" %in% names(data)) data$lter_life_suitability else rep(NA, nrow(data))
    is_rec <- !is.na(suit) & tolower(trimws(suit)) == "recommended"

    # Each box shows its first row of cards; a button shows the rest. The toggle
    # runs in the browser and tells the server, so a box that was opened stays
    # open when the results are redrawn (after Compare or a filter change).
    SHOW_FIRST <- 2
    toggle_js <- function(key) paste0(
      "var b = this.closest('.suit-box'); var open = b.classList.toggle('expanded');",
      "this.textContent = open ? this.dataset.less : this.dataset.more;",
      "this.setAttribute('aria-expanded', open);",
      "Shiny.setInputValue('suit_open_", key, "', open);")

    suit_box <- function(rows, title, info_id, box_class, empty_text, more_text) {
      n <- length(rows)
      key <- sub("suit-", "", box_class)
      is_open <- isTRUE(isolate(input[[paste0("suit_open_", key)]]))
      cards <- lapply(seq_along(rows), function(k) {
        card <- make_card(rows[k])
        if (k > SHOW_FIRST) div(class = "suit-extra", card) else card
      })
      div(class = paste("suit-box", box_class, if (is_open) "expanded"),
          div(class = "suit-header",
              h4(title, tags$span(class = "suit-count", sprintf("(%d)", n))),
              actionButton(info_id, "i", class = "btn-info-circle")),
          if (n) div(class = "suit-cards", cards)
          else p(class = "muted suit-empty", empty_text),
          if (n > SHOW_FIRST)
            div(class = "suit-toggle-row",
                tags$button(type = "button", class = "suit-toggle",
                            `aria-expanded` = if (is_open) "true" else "false", onclick = toggle_js(key),
                            `data-more` = sprintf(more_text, n), `data-less` = "Show fewer",
                            if (is_open) "Show fewer" else sprintf(more_text, n))))
    }

    # Recommended first, the other repositories underneath
    tagList(
      suit_box(which(is_rec), "Recommended by LTER-LIFE", "info_recommended", "suit-rec",
               "No recommended repositories match your selection.",
               "Show all %d recommended repositories"),
      suit_box(which(!is_rec), "Other repositories", "info_other", "suit-other",
               "No other repositories match your selection.",
               "Show all %d other repositories")
    )
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
        list(fact("Who can deposit", format_eligibility(repo$eligibility, repo$institutions)),
             fact("Data stored in", if (is.na(repo$storage_location)) UNKNOWN else repo$storage_location))
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
  observeEvent(input$cmp_open, {
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
      "Free to store data" = function(r) format_yes_no(r$free_to_store),
      "Who can deposit" = function(r) format_eligibility(r$eligibility, r$institutions),
      "Data stored in"  = function(r) if (is.na(r$storage_location)) UNKNOWN else r$storage_location
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
  info("info_recommended", "Recommended by LTER-LIFE",
       paste0("These repositories meet all LTER-LIFE criteria:<ul>",
              "<li><b>DOI</b>: every published dataset gets a DOI, so it can be cited.</li>",
              "<li><b>Open access</b>: data can be published openly.</li>",
              "<li><b>Stored in Europe</b>: the data is kept on servers in Europe.</li>",
              "<li><b>Open to deposit</b>: researchers from LTER-LIFE institutes can deposit data themselves; the repository is not for members only.</li>",
              "<li><b>A real archive</b>: the repository keeps your files long term. Portals that collect data from elsewhere (such as GBIF or OBIS) and networks with their own database do not count.</li></ul>",
              "Recommended does not mean it fits every dataset: check the information on each card."))
  info("info_other", "Other repositories",
       paste0("These repositories do not meet all LTER-LIFE criteria (see the i next to Recommended by LTER-LIFE), ",
              "or we do not have enough information about them yet.<br><br>",
              "They can still be the right place for your data. For example ENA is the standard archive for DNA sequences, ",
              "and GBIF makes species records findable worldwide next to an archived copy elsewhere."))
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
       "Enter how much data you want to deposit at once. Only repositories that can hold at least that much in one deposit are shown. Repositories without a known size limit are always shown; check with them directly.")
  info("info_embargo", "Embargo",
       "An embargo keeps your files closed until a set date while the dataset description is already public. Choose a length to see repositories that allow an embargo of at least that long.")
  info("info_version", "Version control",
       "Whether you can publish a new version of a dataset while earlier versions stay available.")
  info("info_api", "API access",
       "Whether the repository offers programmatic access to its data or metadata (for example a REST API or OAI-PMH).")
  info("info_free", "Free to store data",
       "Whether you can deposit data without paying. Depends on institute means it is free only if your university or institute is a member or partner (for example 4TU.ResearchData is free for WUR staff, and Yoda and Git@WUR are for WUR staff only). The Requirements pop-up shows who can deposit.")
  info("info_region", "Data stored in",
       "Where the repository physically keeps your files. Useful when your funder, institution or privacy rules (GDPR) require data to stay in Europe. The Requirements pop-up gives the country.")
  info("info_doi", "DOI service",
       "Whether the repository issues a DOI (a persistent, citable identifier) for your deposit.")
}

# ------------------------------------------------------------------
shinyApp(ui, server)
