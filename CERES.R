# CERES v3.13.21 - Cost Estimating Relationship Evaluation Suite (web build).
# Public copy, comments removed.

CERES_T0 <- Sys.time()
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(readxl)
library(DT)
library(plotly)
library(shinycssloaders)
library(thematic)
library(leaps)
library(withr)

CERES_IN_BROWSER <- grepl("emscripten", R.version$platform, fixed = TRUE)

CERES_LAZY <- new.env(parent = emptyenv())
ceres_ns_full <- function(pkg) {
  if (!CERES_IN_BROWSER || isNamespaceLoaded(pkg)) return(asNamespace(pkg))
  t0 <- Sys.time()
  if (!nzchar(system.file(package = pkg))) ceres_mount_lazy(pkg)
  t1 <- Sys.time()
  ns <- asNamespace(pkg)
  message(sprintf("CERES package: %-14s files %5.2f s, load %5.2f s", pkg,
                  as.numeric(difftime(t1, t0, units = "secs")), as.numeric(difftime(Sys.time(), t1, units = "secs"))))
  ns
}
CERES_LEAN <- list(
  caret      = list(pkg = "caretlite", fns = c("nearZeroVar", "findCorrelation", "createDataPartition", "createFolds",
                                               "createMultiFolds", "RMSE", "dummyVars")),
  naniar     = list(pkg = "naniarlite"),
  DALEX      = list(pkg = "DALEXlite"),
  iBreakDown = list(pkg = "iBreakDownlite"),
  quantreg   = list(pkg = "quantreglite")
)
ceres_ns <- function(pkg) {
  lean <- if (CERES_IN_BROWSER) CERES_LEAN[[pkg]] else NULL
  if (is.null(lean)) return(ceres_ns_full(pkg))
  if (is.null(lean$fns)) return(ceres_ns_full(lean$pkg))
  structure(list(pkg = pkg, lean = lean), class = "ceres_lean_ns")
}
`$.ceres_lean_ns` <- function(x, name) {
  lean <- .subset2(x, "lean")
  if (name %in% lean$fns) getExportedValue(lean$pkg, name)
  else get(name, envir = ceres_ns_full(.subset2(x, "pkg")))
}
if (CERES_IN_BROWSER) suppressPackageStartupMessages(library(caretlite))
ceres_mount_lazy <- function(pkg) {
  base <- get0(".base_url", envir = globalenv(), ifnotfound = "")
  if (is.null(CERES_LAZY$meta)) {
    f <- tempfile(fileext = ".rds")
    utils::download.file(paste0(base, "packages/metadata_lazy.rds"), f, quiet = TRUE)
    CERES_LAZY$meta <- readRDS(f)
  }
  entry <- CERES_LAZY$meta[[pkg]]
  if (is.null(entry)) stop("CERES web build: package '", pkg, "' is not in this build")
  lib <- "/shinylive/webr/packages"
  if (!lib %in% .libPaths()) .libPaths(c(lib, .libPaths()))
  for (nm in entry$mount) {
    mp <- file.path(lib, nm)
    if (file.exists(mp) || nzchar(system.file(package = nm))) next
    path <- CERES_LAZY$meta[[nm]]$path %||% entry$paths[[nm]]
    getExportedValue("webr", "mount")(mp, paste0(base, path))
  }
  if ("ggplot2" %in% entry$mount && isNamespaceLoaded("thematic"))
    try(get("ggplot_build_set", envir = asNamespace("thematic"))(), silent = TRUE)
  invisible(TRUE)
}
if (CERES_IN_BROWSER) compiler::enableJIT(0)
ceres_mark <- function(what) if (CERES_IN_BROWSER) message(sprintf("CERES timing: %-28s %6.2f s (epoch %.0f)", what,
  as.numeric(difftime(Sys.time(), CERES_T0, units = "secs")), as.numeric(Sys.time()) * 1000))
ceres_mark("libraries attached")

if (CERES_IN_BROWSER) options(shiny.useragg = FALSE)
if (!CERES_IN_BROWSER && requireNamespace("ragg", quietly = TRUE)) {
  ragg_ok <- tryCatch({
    f <- tempfile(fileext = ".png")
    ragg::agg_png(f, width = 10, height = 10)
    graphics::plot.new()
    grDevices::dev.off()
    unlink(f)
    TRUE
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    FALSE
  })
  if (!ragg_ok) options(shiny.useragg = FALSE)
  rm(ragg_ok)
}

ceres_cache_dir <- function(what) {
  ok <- function(d) isTRUE(tryCatch({
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    f <- tempfile(tmpdir = d); writeLines("", f); unlink(f); TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE))
  d <- tryCatch(tools::R_user_dir(what, "cache"), error = function(e) "")
  if (nzchar(d) && ok(d)) return(d)
  d <- file.path(tempdir(), paste0(what, "-cache"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}
if (requireNamespace("sass", quietly = TRUE) && is.null(getOption("sass.cache")))
  options(sass.cache = tryCatch(sass::sass_file_cache(ceres_cache_dir("sass")), error = function(e) NULL))

options(shiny.maxRequestSize = 50 * 1024^2)

primary_color    = "#2F6FE4"
secondary_color  = paste0(primary_color, "50")
tertiary_color   = paste0(primary_color, "20")   

primary_color_passthrough = primary_color
secondary_color_passthrough  = secondary_color
tertiary_color_passthrough  = tertiary_color 
loading_color = primary_color

dark_primary_color    = "#5B8CFF"
dark_secondary_color  = paste0(dark_primary_color, "50")
dark_tertiary_color   = paste0(dark_primary_color, "20")   

dark_primary_color_passthrough = dark_primary_color
dark_secondary_color_passthrough  = dark_secondary_color
dark_tertiary_color_passthrough  = dark_tertiary_color 
dark_loading_color = dark_primary_color

hex_to_rgb <- function(hex) {
  hex <- gsub("#", "", hex)
  r <- strtoi(substr(hex, 1, 2), base=16)
  g <- strtoi(substr(hex, 3, 4), base=16)
  b <- strtoi(substr(hex, 5, 6), base=16)
  return(c(r, g, b))
}

rgb_distance <- function(rgb1, rgb2) {
  sqrt(sum((rgb1 - rgb2)^2))
}

default_colorway <- c(
  "#E0457B",
  "#F08A24",
  "#2F6FE4",
  "#16A34A",
  "#A3C93A",
  "#0EA5E9",
  "#7C5CFF",
  "#F5B700",
  "#DC2626"
)

primary_hex <- primary_color
primary_rgb <- hex_to_rgb(primary_hex)

COLOR_DISTANCE_THRESHOLD <- 105

selected_colors <- list(primary_hex)

for (color_hex in default_colorway) {
  candidate_rgb <- hex_to_rgb(color_hex)
  
  distances <- sapply(selected_colors, function(existing_hex) {
    rgb_distance(candidate_rgb, hex_to_rgb(existing_hex))
  })
  
  if (all(distances >= COLOR_DISTANCE_THRESHOLD)) {
    selected_colors[[length(selected_colors) + 1]] <- color_hex
  }
  
   if (length(selected_colors) >= 3) break
}

selected_palette <- unlist(selected_colors)

graph_color_1 = "#E0457B"
graph_color_2 = "#7C5CFF"
graph_fill_color_1 = paste0(graph_color_1, "38")
graph_fill_color_2 = paste0(graph_color_2, "38")

thematic_shiny()

plotlyOutput <- function(outputId, ..., reportTheme = FALSE)
  plotly::plotlyOutput(outputId, ..., reportTheme = reportTheme)

ceres_ui_version <- function() {
  www <- if (exists("expected_www")) expected_www else "www"
  f <- file.path(www, c("ceres-ui.js", "ceres-ui.css"))
  f <- f[file.exists(f)]
  if (!length(f)) return("0")
  format(as.numeric(max(file.mtime(f))), scientific = FALSE)
}

ceres_app_version <- function(candidates = c("CERES.R", "app.R", "project-r-src/CERES/R/CERES.R")) {
  for (f in candidates) {
    v <- tryCatch(readLines(f, n = 1L, warn = FALSE), error = function(e) character(0))
    if (length(v) == 1L && grepl("^#\\s*CERES\\s+v[0-9]", v)) return(regmatches(v, regexpr("v[0-9]+(\\.[0-9]+)*", v)))
  }
  ""
}

CERES_HEALTH_STYLE <- "bar"

ceres_cmp_snapshot <- function(model, mode, dep_var, eq_parts, n_obs, r2, adj_r2, sigma, fstat, coef_df) {
  terms_lab <- tryCatch(unbt(attr(stats::terms(model), "term.labels")), error = function(e) character(0))
  b  <- stats::coef(model); b[is.na(b)] <- 0
  X  <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
  y  <- tryCatch(stats::model.response(stats::model.frame(model)), error = function(e) NULL)
  yh <- if (!is.null(X) && ncol(X) == length(b)) drop(X %*% b) else NULL
  unit <- function(v) if (is.null(v)) NULL else if (identical(mode, "loglin")) exp(v) else v
  err <- attr(model, "error_type") %||% attr(model, "ceres_error") %||% NA_character_
  list(
    key     = paste(mode, dep_var, paste(terms_lab, collapse = " + "), err, eq_parts$text, sep = "|"),
    mode    = mode,
    error   = err,
    dep_var = dep_var,
    terms   = terms_lab,
    eq_text = eq_parts$text,
    eq_latex = eq_parts$latex,
    n = n_obs, r2 = unname(r2), adj_r2 = unname(adj_r2), sigma = unname(sigma),
    f = if (!is.null(fstat)) unname(fstat[1]) else NA_real_,
    coefs = coef_df,
    actual = unit(y), fitted = unit(yh),
    rows = rownames(model$model)
  )
}

c_section <- function(title, icon_name = NULL) {
  tags$div(class = "c-sec", if (!is.null(icon_name)) icon(icon_name), tags$span(title))
}

bn_direct_links <- function(edges, focus, cut = 0) {
  if (is.null(edges) || !nrow(edges)) return(data.frame(var = character(0), strength = numeric(0)))
  e <- edges[edges$strength >= cut & (edges$from == focus | edges$to == focus), , drop = FALSE]
  if (!nrow(e)) return(data.frame(var = character(0), strength = numeric(0)))
  other <- ifelse(e$from == focus, as.character(e$to), as.character(e$from))
  out <- stats::aggregate(list(strength = e$strength), list(var = other), max)
  out[order(-out$strength, out$var), , drop = FALSE]
}

bn_groups <- function(edges, vars, cut = 0.8) {
  vars <- unique(as.character(vars))
  parent <- setNames(vars, vars)
  find <- function(v) { while (parent[[v]] != v) v <- parent[[v]]; v }
  if (!is.null(edges) && nrow(edges)) {
    e <- edges[edges$strength >= cut, , drop = FALSE]
    for (i in seq_len(nrow(e))) {
      a <- as.character(e$from[i]); b <- as.character(e$to[i])
      if (!(a %in% vars) || !(b %in% vars)) next
      ra <- find(a); rb <- find(b)
      if (ra != rb) parent[[rb]] <- ra
    }
  }
  roots <- vapply(vars, find, character(1))
  groups <- unname(split(vars, roots))
  groups <- groups[lengths(groups) >= 2]
  groups[order(-lengths(groups), vapply(groups, `[`, character(1), 1))]
}

bn_link_matrix <- function(edges, vars) {
  vars <- unique(as.character(vars))
  m <- matrix(0, length(vars), length(vars), dimnames = list(vars, vars))
  if (!is.null(edges) && nrow(edges)) for (i in seq_len(nrow(edges))) {
    a <- as.character(edges$from[i]); b <- as.character(edges$to[i])
    if (a %in% vars && b %in% vars) {
      m[a, b] <- max(m[a, b], edges$strength[i]); m[b, a] <- m[a, b]
    }
  }
  m
}

light_theme <- bs_theme(
  version = 5,
  bootswatch = "bootstrap",
  bg = "#FFFFFF",
  fg = "#000000",
  primary = primary_color,
  secondary = primary_color,
  base_font = font_collection("SF Pro Text", "Segoe UI Variable Text", "Segoe UI", "system-ui", "sans-serif"),
  heading_font = font_collection("SF Pro Display", "SF Pro Text", "Segoe UI Variable Display", "Segoe UI", "system-ui", "sans-serif"),
  "font-size-base" = "0.9rem"
)|>
  bs_add_rules(
    "
    /* Custom h4 styling integrated into the theme */
    .h4, h4, .h5, h5 {
      font-size: 1.10rem !important;
      color: var(--bs-primary) !important;  }
      
      /* --- MOON KILL THE BLUE GLOW --- */
    .bootstrap-switch.bootstrap-switch-focused,
    .bootstrap-switch:focus,
    .bootstrap-switch-wrapper:focus-within {
      border-color: #D3D3D3 !important;
      /*  outline: 0 !important; */
        box-shadow: none !important;
        -webkit-box-shadow: none !important;
      }
      /* Fix toggle container offset — aligns moon icon correctly */
    .bootstrap-switch-container {
      margin-left: -40px !important;
    }
      
          /* Custom h4 styling integrated into the theme */
    .control-label {
      font-size: 0.95rem !important;
      color: var(--bs-primary) !important;
      font-weight: bold !important; }
      
          /* Style for the tab bar background in light mode */
    .nav-tabs {
      background-color: #e9ecef;
      border-bottom: 1px solid #dee2e6;
    }

    /* Style for the active tab to make it look connected */
    .nav-tabs .nav-link.active, .nav-tabs .nav-item.show .nav-link {
      background-color: var(--bs-body-bg); /* Uses the main page background color */
      border-bottom-color: var(--bs-body-bg);
    }
    
    /* Set text color for ALL nav links */
    .page-navbar .nav-link, 
    .nav-tabs-custom .nav-tabs .nav-link {
      color: var(--bs-primary) !important;
    }

    /* 2Set hover background for ALL nav links */
    .nav-tabs .nav-link:hover,
    .page-navbar .nav-link:hover, 
    .nav-tabs-custom .nav-tabs .nav-link:hover {
      background-color: var(--bs-primary) !important;
      color: var(--bs-body-bg) !important;
    }

    /* Style for the active tab */
    .page-navbar .nav-link.active, 
    .nav-tabs-custom .nav-tabs .nav-link.active {
      background-color: var(--bs-body-bg) !important;
      border-bottom-color: var(--bs-body-bg) !important;
      color: var(--bs-primary) !important;
    }
    
    /* Plotly brush shadow */
    .highlight-shadow {
      stroke-width: 0 !important;
    }

     /* Set boxplot fill and stroke color for light theme */
    .trace.boxes .point {
      fill: rgba(var(--bs-primary-rgb), 1.00) !important;
      stroke: var(--bs-primary) !important;
    }
    
    
        
    /* Active page number on data tab. */
    .pagination .active .page-link {
      color: var(--bs-body-bg) !important;
    }
    

      "
    )
    
    

ceres_theme_precompiled <- function(theme_obj, dir = "ceres-theme") {
  css <- file.path(dir, "bootstrap.min.css"); vers <- file.path(dir, "VERSIONS")
  if (!CERES_IN_BROWSER || !file.exists(css) || !file.exists(vers)) return(invisible(FALSE))
  v <- strsplit(readLines(vers, warn = FALSE), " ")
  v <- setNames(vapply(v, `[`, "", 2), vapply(v, `[`, "", 1))
  if (!identical(as.character(packageVersion("bslib")), unname(v[["bslib"]]))) {
    message("CERES: precompiled theme is for bslib ", v[["bslib"]], "; compiling instead")
    return(invisible(FALSE))
  }
  ns   <- asNamespace("bslib")
  orig <- get("precompiled_css_path", envir = ns)
  want <- rlang::hash(theme_obj)
  css  <- normalizePath(css)
  hook <- function(theme = bslib::bs_theme()) {
    if (identical(rlang::hash(theme), want)) css else orig(theme)
  }
  unlockBinding("precompiled_css_path", ns)
  assign("precompiled_css_path", hook, envir = ns)
  lockBinding("precompiled_css_path", ns)
  invisible(TRUE)
}
ceres_theme_precompiled(light_theme)

current_dir <- getwd()

if (grepl("webappruns", current_dir) || grepl("web_apps", current_dir)) {
  expected_www <- "project-r-src/CERES/R/www"
} else {
  expected_www <- "www"
}

if (dir.exists(expected_www)) {
  addResourcePath("static", expected_www)
}

nl_registry <- list(
  "Power Curve" = list(
    type = "multivariate",
    formula_base = "a * {terms}", 
    params_base = c("a"),
    param_prefix = "b_"           
  ),
  "Exponential" = list(
    type = "multivariate",
    formula_base = "a * exp({terms})", 
    params_base = c("a"),
    param_prefix = "b_"
  ),
  "Logistic (S-Curve)" = list(
    type = "multivariate",
    formula_base = "a / (1 + exp(-(c + {terms})))", 
    params_base = c("a", "c"),
    param_prefix = "b_"
  ),
  "Gompertz" = list(
    type = "univariate",
    formula_base = "a * exp(-b * exp(-c * {X1}))",
    params_base = c("a", "b", "c")
  ),
  "Michaelis-Menten" = list(
    type = "univariate",
    formula_base = "(a * {X1}) / (b + {X1})",
    params_base = c("a", "b")
  ),
  "Weibull (Type 1)" = list(
    type = "univariate",
    formula_base = "a - b * exp(-c * ({X1}^d))",
    params_base = c("a", "b", "c", "d")
  ),
  "Weibull (Type 2)" = list(
    type = "univariate",
    formula_base = "a * (1 - exp(-b * ({X1}^c)))",
    params_base = c("a", "b", "c")
  )
)
  

css <- HTML("
html, body {
  font-family: 'SF Pro Text', 'Segoe UI Variable Text', 'Segoe UI', system-ui, sans-serif !important;
}

.nav:not(.nav-hidden):not(.nav-stacked):not(.flex-column)>.form-inline {
    margin-top: 5px !important;
    margin-bottom: auto;
}
    
/* 2. Force the Navbar to 100% */
/* Removed the '>' to make it a descendant selector (safer) */
.navbar .container, 
.navbar .container-fluid, 
.navbar [class*='container'] {
    max-width: 100% !important;
    width: 100% !important;
}

.bootstrap-switch {
    /* border: 0px !important; */
    box-shadow: none !important; /* Removes any 3D depth effect */
}

.h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 {
 font-family: var(--bs-font-sans-serif);
}

.skin-blue body {
  font-family: var(--bs-font-sans-serif);
}

/* Removes padding from the CERES navbar. Default is too much and looks ugly. */
.navbar-static-top {
  padding-bottom: 0 !important;
  margin-bottom: 0 !important;
}

/* This is the correct place for the padding rule */
#data_tabs + .tab-content > .tab-pane {
  padding-top: 10px;
}

/* Moving the 'Click on a row to see model statistics and residual plots.' text */
#linear_reg_box + .tab-content > .tab-pane {
  padding-top: 10px;
}

/* Moving the 'Click on a row to see model statistics and residual plots.' text */
#nlls + .tab-content > .tab-pane {
  padding-top: 10px;
}



/* Prevent conditional panel flash on page load.
   All conditional panels start hidden; Shiny JS evaluates conditions
   and reveals the correct panels in the same render cycle. */
.shiny-panel-conditional {
  display: none;
}

div:has(> #save_transform_data) {
  margin-bottom: 10px;
}

.skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper,
.content-wrapper, .right-side,
.skin-blue .main-header .navbar,
.skin-blue .main-header .logo:hover,
.skin-blue .main-header .logo,
.navbar-default, .navbar-default:hover {
  background-color: var(--bs-body-bg) !important;
}

/* Trying to move CERES text down a bit. */
.navbar-brand {
  padding-top: 15px !important; /* Default is often 15px. Adjust as needed. */
}

/* Target the link inside the brand element */
.navbar-brand a {
/*padding-top: 3px !important; */
padding-bottom: 4.5px !important;
padding-right: 15px !important;
  display: flex;
  align-items: center;
  height: 100%;
  text-decoration: none !important;
}

/* Plotly brush shadow */
.highlight-shadow {
  stroke-width: 0 !important;
}

/* Target the link inside the brand element on hover */
.navbar-brand a:hover {
  text-decoration: underline !important;
}

.sidebar {
  color: var(--bs-primary);
}

.skin-blue .main-header .navbar .sidebar-toggle {
  color: var(--bs-primary);
}

.skin-blue .main-header .navbar .sidebar-toggle:hover {
  background-color: var(--bs-secondary);
}

.skin-blue .main-header .logo {
  color: var(--bs-primary);
  border-bottom: 0 solid transparent;
}

.box {
  position: relative;
  border-radius: 3px;
    /* Use primary color with low alpha for background */
  background: rgba(var(--bs-primary-rgb), 0.05);
  border-top: 0px solid var(--bs-border-color);
  margin-bottom: 20px;
  width: 100%;
  box-shadow: 0 1px 1px var(--bs-border-color);
}

body {
 color: var(--bs-body-color);
}

.nav-tabs-custom>.nav-tabs>li.active{border-top-color:#661E2B !important;}

.box.box-primary {
 border-top-color: var(--bs-primary);
}

.progress-bar {
 background-color: var(--bs-primary);
}

.alert-info, .bg-aqua, .callout.callout-info, .label-info, .modal-info .modal-body {
 background-color: var(--bs-primary) !important;
 color: var(--bs-light) !important;
}

.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
 background-color: var(--bs-primary);
 color: var(--bs-light);
 border-left-color: var(--bs-secondary);
}

.skin-blue .main-sidebar .sidebar .sidebar-menu a{
 background-color: var(--bs-tertiary-bg);
 color: var(--bs-body-color);
}

.skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
 background-color: var(--bs-secondary);
 color: var(--bs-body-color);
 border-left-color: var(--bs-primary);
}
               
.skin-blue .main-header .navbar .sidebar-toggle:hover{
 background-color: var(--bs-secondary);
}

.nav-tabs {
 border-bottom: 1px solid var(--bs-border-color);
 background-color: var(--bs-tertiary-bg);
}

.nav-tabs-custom .nav-tabs li.active {
 border-top-color: var(--bs-primary);
}

.nav-tabs-custom .nav-tabs li.active a{
 background-color: var(--bs-tertiary-bg);
}

.nav-tabs-custom .tab-content {
 background: var(--bs-tertiary-bg);
 padding: 10px;
 border-bottom-right-radius: 1px;
 border-bottom-left-radius: 1px;
}

.nav-tabs-custom .nav-tabs li.active:hover a:hover,
.nav-tabs-custom .nav-tabs li a:hover {
 background-color: var(--bs-secondary);
 color: var(--bs-body-color);
}

.dropdown-menu .active a,
.dropdown-menu .active a:focus,
.dropdown-menu .active a:hover,
.dropdown-menu .active a:active:focus {
 background-color: var(--bs-primary);
 outline: none !important;
}

.btn-default {
 background-color: var(--bs-primary);
 color: var(--bs-light);
}

.btn-default.focus, .btn-default:focus, .btn-default.hover, .btn-default:hover{
 color: var(--bs-light);
 background-color: var(--bs-secondary);
 border-color: var(--bs-body-color);
}

.skin-blue .sidebar a {
 color: var(--bs-body-color);
}

.bttn-fill.bttn-success {
 background: var(--bs-primary);
 color: #fff;
}

.bttn-fill.bttn-success:focus, .bttn-fill.bttn-success:hover{
 color: var(--bs-primary);
}

.bootstrap-select>.dropdown-toggle.bs-placeholder, .bootstrap-select>.dropdown-toggle.bs-placeholder:active, .bootstrap-select>.dropdown-toggle.bs-placeholder:focus, .bootstrap-select>.dropdown-toggle.bs-placeholder:hover {
 color: #fff!important;
}

.text-muted {
 color: var(--bs-secondary-color);
}

.irs-bar-edge {
 border: 1px solid var(--bs-primary);
 background: var(--bs-primary);
}

.irs--shiny .irs-bar {
 border-top: 1px solid var(--bs-primary);
 border-bottom: 1px solid var(--bs-primary);
 background: var(--bs-primary);
}

.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
 color: var(--bs-light);
 background-color: var(--bs-primary);
}

.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-primary, .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-primary {
 color: #fff;
 background: var(--bs-primary);
}

.nav-tabs-custom .tab-content,
.modal-body {
 background: var(--bs-body-bg);
}

.selectize-control.multi .selectize-input > div {
 background: var(--bs-primary);
 color: var(--bs-light);
 border: 0 solid rgba(0, 0, 0, 0);
}
.selectize-control.multi .selectize-input > div.active {
 background: #8d256f;
 color: #ffffff;
 border: 0 solid rgba(0, 0, 0, 0);
}

.modal-body {
 position: relative;
 padding-top: 0px;
 padding-right: 10px;
 padding-bottom: 0px;
 padding-left: 10px;
}

.nav-tabs-custom .nav-tabs li a,
.nav-tabs-custom .nav-tabs li a:focus,
.nav-tabs-custom .nav-tabs li a:hover {
  color: var(--bs-body-color) !important;
}

#data_upload_progress {
  margin-top: 5px;
}

/* Animated file upload progress bar */
.shiny-file-input-progress {
  display: block !important;
  height: 16px;
  background: transparent;
  border-radius: 6px;
  overflow: hidden;
}

.shiny-file-input-progress .progress-bar {
  background-color: #519162 !important;
  transition: width 0.3s ease;
  height: 100%;
}

.shiny-file-input-progress .progress-text {
  font-size: 0.75rem;
  margin-top: 4px;
  text-align: center;
}

/* Changes the progress bar to a pretty green */
.progress-bar {
  background-color: #519162 !important;
}

/* Widen progress notification box and keep it on screen */
.shiny-notification {
  width: 330px !important;
  min-width: 330px !important;
}

#shiny-notification-panel {
  right: 45px !important;
}


a, a:visited, a:hover, a:focus {
  color: var(--bs-primary) !important;
  text-decoration: none;
}

/*# #ols_cv_results {*/
/*#   height: auto !important;*/
/*# }*/

#ols_cv_results.html-fill-item {
  flex: 0 1 auto !important;
}

/* DARK MODE ONLY */
/* DARK MODE ONLY */
/* DARK MODE ONLY */
/* DARK MODE ONLY */
/* DARK MODE ONLY */
")

  ceres_build_ui <- function() page_navbar(
  theme = light_theme, 
  
  title = tags$a("CERES", href = "#", onclick = "Shiny.setInputValue('navTo', 'ExploreData', {priority: 'event'})"),
  id = "tabs",
  
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", href = "static/katex.min.css")),
    tags$head(tags$script(src = "static/katex.min.js")),
    tags$head(lapply(c("Text-Regular", "Text-Semibold", "Text-Bold", "Display-Bold"),
                     function(w) tags$link(rel = "preload", as = "font", type = "font/woff2", crossorigin = "anonymous",
                                           href = paste0("static/fonts/SF-Pro-", w, ".woff2")))),
    tags$head(htmltools::tagFunction(function()
      tags$link(id = "ceres-ui-css", rel = "stylesheet", href = paste0("static/ceres-ui.css?v=", ceres_ui_version())))),
    tags$head(htmltools::tagFunction(function()
      tags$script(src = paste0("static/ceres-ui.js?v=", ceres_ui_version())))),
    tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "static/ceres-logo-light.svg")),
    uiOutput("cmp_tray"),
    tags$head(tags$script(HTML(r"(
      var _cerClipIcon = '<svg xmlns="http://www.w3.org/2000/svg" width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2"/><rect x="8" y="2" width="8" height="4" rx="1" ry="1"/></svg>';
      var _cerCheckIcon = '<svg xmlns="http://www.w3.org/2000/svg" width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="#2d8a4e" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="20 6 9 17 4 12"/></svg>';

      function cerCopyEquation(btn) {
        var text = btn.getAttribute('data-eq');
        text = text.replace(/\u00d7/g, '*').replace(/\u2212/g, '-');
        function done() {
          btn.innerHTML = _cerCheckIcon;
          setTimeout(function() { btn.innerHTML = _cerClipIcon; }, 1500);
        }
        function fallback() {
          var ta = document.createElement('textarea');
          ta.value = text;
          ta.style.position = 'absolute';
          ta.style.top = '0';
          ta.style.left = '-9999px';
          ta.style.opacity = '0';
          ta.style.pointerEvents = 'none';
          // Append inside the modal so focus-trap doesn't block select()
          var container = btn.closest('.modal-body') || btn.closest('.modal-content') || document.body;
          container.appendChild(ta);
          ta.focus();
          ta.select();
          try { document.execCommand('copy'); done(); }
          catch(e) { btn.textContent = 'Copy'; }
          container.removeChild(ta);
        }
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(text).then(done).catch(fallback);
        } else {
          fallback();
        }
      }

      function renderKatexElements() {
        var els = document.querySelectorAll('.katex-eq');
        els.forEach(function(el) {
          if (el.dataset.katexRendered) return;
          var latex = el.dataset.latex;
          if (!latex || typeof katex === 'undefined') return;
          try {
            katex.render(latex, el, { throwOnError: false, displayMode: true });
            el.dataset.katexRendered = 'true';
          } catch(e) {}
        });
      }
      $(document).ready(function() {
        renderKatexElements();
        // Equations render the moment they arrive (the observer runs before
        // the browser paints), so the plain-text form never shows first. The
        // observer stays on but only looks inside nodes that were just added.
        if (typeof MutationObserver !== 'undefined') {
          new MutationObserver(function(muts) {
            for (var i = 0; i < muts.length; i++) {
              var added = muts[i].addedNodes;
              for (var j = 0; j < added.length; j++) {
                var n = added[j];
                if (n.nodeType === 1 && (n.classList.contains('katex-eq') || n.querySelector('.katex-eq'))) {
                  renderKatexElements(); return;
                }
              }
            }
          }).observe(document.body, { childList: true, subtree: true });
        }
        // Warm KaTeX up once (code path and the fonts an equation uses), off
        // screen, so the first real equation draws in its final form at once.
        if (typeof katex !== 'undefined') {
          var warm = document.createElement('div');
          warm.setAttribute('aria-hidden', 'true');
          warm.style.cssText = 'position:absolute;left:-9999px;top:0;visibility:hidden';
          document.body.appendChild(warm);
          try { katex.render('\\widehat{Cost} = 1.2\\left(\\frac{x_1}{\\mathbf{y}}\\right)^{0.5} \\times e^{2}', warm, { throwOnError: false, displayMode: true }); } catch (e) {}
          setTimeout(function () { warm.remove(); }, 10000);
        }
      });
    )"))),
    tags$head(tags$script(HTML(r"(
      (function(){
        // Timers/bars are keyed by message id so multiple producers can drive their own
        // bars concurrently — a single shared timer/regex made one producer freeze or
        // snap another's bar. Currently only 'screen' (Optimizing) is used.
        var words  = { screen: 'Optimizing' };
        var timers = {};
        function optBar(word){
          var found = null;
          document.querySelectorAll('.shiny-progress-notification').forEach(function(p){
            var msg = p.querySelector('.progress-message');
            var bar = p.querySelector('.progress-bar');
            if (bar && msg && msg.textContent.indexOf(word) !== -1) found = bar;
          });
          return found;
        }
        function setOpt(word, frac){
          var b = optBar(word);
          if (b) b.style.setProperty('width', (Math.max(0, Math.min(1, frac))*100).toFixed(1) + '%', 'important');
        }
        function stopTimer(id){
          if (timers[id]){ clearInterval(timers[id]); timers[id] = null; }
        }
        if (window.Shiny){
          Shiny.addCustomMessageHandler('cer_screen_start', function(m){
            var id = m.id || 'screen', word = words[id] || 'Optimizing';
            stopTimer(id);
            var t0 = performance.now(), tau = m.tau_ms || 1000, tgt = m.target || 0.95;
            setOpt(word, 0);
            timers[id] = setInterval(function(){
              var t = performance.now() - t0;
              setOpt(word, tgt * (1 - Math.exp(-t / tau)));
            }, 70);
          });
          Shiny.addCustomMessageHandler('cer_screen_done', function(m){
            var id = m.id || 'screen', word = words[id] || 'Optimizing';
            stopTimer(id);
            setOpt(word, 1);
          });
        }
      })();
    )"))),
    tags$head(HTML("<title>CERES</title>")),
    tags$head(tags$style(css)),
    tags$head(tags$style(HTML(
      '
        /* KaTeX equation colour — dark text on light gradient banner */
        .katex-eq .katex, .katex-eq .katex-html,
        .katex-eq .katex * { color: #222 !important; }
        .katex-eq .katex-display { margin: 0; }

        /* Copy icon button */
        .cer-copy-btn {
          background:#fff; border:1px solid #dee2e6; color:#555;
          border-radius:6px; padding:4px 7px; cursor:pointer;
          display:inline-flex; align-items:center; justify-content:center;
          transition: transform 0.08s ease, box-shadow 0.08s ease, background 0.08s ease;
          box-shadow: 0 1px 2px rgba(0,0,0,0.08);
          position:relative; z-index:9999; pointer-events:auto;
          font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;
        }
        .cer-copy-btn:hover { background:#f0f4ff; border-color:#b0bec5; }
        .cer-copy-btn:active { transform:scale(0.88); box-shadow:none; background:#e8edf8; }

        /* Custom selected row styling */
        table.dataTable tbody tr.selected td,
        table.dataTable tbody tr.selected {
          box-shadow: inset 0 0 0 9999px var(--bs-primary) !important;
          background-color: var(--bs-primary) !important;
          color: var(--bs-light) !important;
          border-top: 1px solid transparent !important;
          border-bottom: 1px solid transparent !important;
        }
  
        /* the pink stroke on highlighted lines */
        .js-plotly-plot .highlight {
          stroke: rgba(var(--bs-primary-rgb), 0.33) !important;
        }
        /* the highlighted (on-brush) lines */
        .js-plotly-plot .highlight {
          stroke-width: 24px !important;
        }
        
        .axis-extent-top, .axis-extent-bottom { display: none !important; }
        .js-plotly-plot .axis-title { font-weight: bold !important; }
        
        table.dataTable tbody tr.selected:hover td,
        table.dataTable tbody tr.selected:hover,
        table.dataTable.hover tbody tr.selected:hover td,
        table.dataTable.display tbody tr.selected:hover td,
        .dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody>table.dataTable tbody tr.selected:hover td {
          box-shadow: inset 0 0 0 9999px var(--bs-primary) !important;
          background-color: var(--bs-primary) !important;
          color: var(--bs-light) !important;
          border-top: 1px solid transparent !important;
          border-bottom: 1px solid transparent !important;
        }
        .modal-dialog{width: auto !important; max-width: 1000px}
        .red{background-color:#FFB8C3;}.blue{background-color:#6ECFEA;}
      '
    )))
  ),
  
  tabPanel("Explore Data",
                           sidebarLayout(
                             sidebarPanel(width = 2,
                                          tagAppendAttributes(class = "c-upload-panel", conditionalPanel(
                                            condition = "input.data_tabs == 'Data' || input.data_select === null || input.data_select === ''",
                                            fileInput("data_upload", "Upload Data (Excel or CSV)",
                                                      multiple = FALSE,
                                                      accept = c(".csv", ".xls", ".xlsx", ".xlsm")),
                                            uiOutput("data_upload_sheet_ui")
                                          )),
                                          conditionalPanel(
                                            condition = "input.data_select !== null && input.data_select !== ''",
                                            selectInput(inputId = "data_select", label = "Select Data to Analyze", choices = NULL)
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Data' && input.data_select !== null && input.data_select !== '' && output.has_multiple_datasets",
                                            div(align = "center",
                                                actionButton("remove_dataset_btn", "Remove Selected Dataset",
                                                             class = "btn-sm", icon = icon("trash")))
                                          ),
                                          conditionalPanel(
                                            condition = "input.missing_box == 'Impute Missing Data' & input.data_tabs == 'Missing Data'",
                                            tags$div(id = "imputation_vars_ui"),
                                            selectInput(inputId = "common_impute", "Imputation Method", choices = c("None", "Listwise Deletion", "Mean (Num) / Mode (Cat)", "Median (Num) / Mode (Cat)", "Bayesian Network"), selected = "None"),
                                            textInput(inputId = "impute_data_name", label = "Name Dataset with Imputed Values:", value = "Imputed Data"),
                                            div(align = "center", actionButton("save_imputed_data", "Save Imputed Data"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.missing_box == 'Mechanisms of Missingness' & input.data_tabs == 'Missing Data'",
                                            tags$div(id = "missing_patterns_ui",
                                              selectInput("selected_missing", "Variables with Missing Data:", choices = character(0))
                                            ),
                                            h5("Apply 10-Fold CV to fit and prune a decision tree to classify missing values. A single node suggests no discernible patterns in missing data.")
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Transforms' & input.transform_box == 'Transform'",
                                            tags$div(id = "transform_vars_ui",
                                              selectInput("log_transform_vars_selected",     "Natural Log (ln):",    choices = character(0), multiple = TRUE),
                                              selectInput("sqrt_transform_vars_selected",    "Square Root (sqrt):",  choices = character(0), multiple = TRUE),
                                              selectInput("inverse_transform_vars_selected", "Inverse (1/x):",       choices = character(0), multiple = TRUE)
                                            ),
                                            textInput(inputId = "transform_data_name",
                                                      label = "Name Dataset with Transformations:", 
                                                      value = "Log Transformed Data"),
                                            checkboxInput(inputId = "replace_transform", label = "Replace Original Data with Transformed Data?", value = FALSE),
                                            div(align = "center", actionButton("save_transform_data", "Save Data Transformations")),
                                            div(align = "center",actionButton("log_all_button",   "Convert Data to Log Space")),
                                            
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Transforms' & input.transform_box == 'Subset'",
                                            tags$div(id = "subset_vars_ui",
                                              selectInput("subset_var_selected", "Categorical Variable:", choices = character(0))
                                            ),
                                            textInput(inputId = "subset_data_name", label = "Name Dataset with Subset:", value = "Subset Data"),
                                            div(align = "center", actionButton("save_subset_data", "Save Data Subsets"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Transforms' & input.transform_box == 'Interact'",
                                            tags$div(id = "interact_vars_ui",
                                              selectInput("interact1_var_selected", "Variable A:", choices = character(0)),
                                              selectInput("interact2_var_selected", "Variable B:", choices = character(0)),
                                              checkboxGroupInput("interact_term_types", "Terms to Generate:",
                                                choices  = c("Products (A \u00d7 B)" = "product",
                                                             "Ratios (A \u00f7 B)"   = "ratio",
                                                             "Squared (A\u00b2, B\u00b2)" = "square"),
                                                selected = "product"),
                                              selectInput("interact_response_var", "Correlate with Response (optional):",
                                                choices = c("(none)"), selected = "(none)")
                                            ),
                                            textInput(inputId = "interact_data_name", label = "Name Dataset with Interactions:", value = "Interaction Data"),
                                            div(align = "center", actionButton("save_interact_data", "Save Data Interactions"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Transforms' & input.transform_box == 'Expand'",
                                            tags$div(id = "expand_vars_ui",
                                              selectInput("expand_cat_var_selected", "Categorical Variable(s) to Expand:", choices = character(0), multiple = TRUE)
                                            ),
                                            textInput(inputId = "expand_data_name", label = "Name Dataset with Expanded Columns:", value = "Expanded Data"),
                                            checkboxInput(inputId = "remove_original_expand", label = "Remove Original Categorical Column?", value = FALSE),
                                            div(align = "center", actionButton("save_expand_data", "Expand to Boolean Columns"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Collinearity'",
                                            tags$div(id = "vif_vars_ui",
                                                     h4("Collinearity settings"),
                                                     numericInput("vif_threshold", "Flag at R\u00b2 of:", value = 0.93, min = 0, max = 1, step = 0.01),
                                                     uiOutput("vif_thr_equiv"),
                                                     checkboxInput("vif_multivariate", "Also check each column against all the others (VIF)", value = FALSE),
                                                     div(align = "center", actionButton("test_vif", "Check for collinearity")),
                                                     hr(),
                                                     textInput("vif_new_name", "Save Reduced Collinearity Dataset", value = "Reduced Collinearity Data"),
                                                     div(align = "center", actionButton("save_reduced_data", "Save Reduced Collinearity Data"))
                                            )
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Network'",
                                            h4("Network Settings"),
                                            tags$div(
                                              style = "display: flex; justify-content: space-between; align-items: center;",
                                              tags$label("Variables", `for` = "bn_included_vars", class = "control-label"),
                                              actionLink("bn_select_all_vars", "(Select All)",
                                                         style = "font-size: 12px; font-weight: bold;")
                                            ),
                                            selectInput("bn_included_vars", label = NULL,
                                                        choices = character(0), multiple = TRUE),
                                            hr(),
                                            sliderInput("bn_bootstrap", "Bootstrap Samples",
                                                        min = 50, max = 500, step = 50, value = 100),
                                            sliderInput("bn_threshold", "Confidence Cutoff",
                                                        min = 0, max = 100, step = 1, value = 40,
                                                        post = "%"),
                                            div(align = "center",
                                                actionButton("bn_generate",
                                                             "Generate Network",
                                                             class = "btn-primary"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Boxplots'",
                                            tags$div(id = "boxplot_vars_ui",
                                              selectInput("selected_box",    "Variables:",          choices = character(0), multiple = TRUE),
                                              selectInput("cat_selected_box", "Group Boxplots by:", choices = character(0)),
                                              checkboxInput("boxplot_log_space", "Apply Log-Transform", value = FALSE)
                                            )
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Scatterplots'",
                                            tags$div(id = "viz_vars_ui",
                                              selectInput("x_selected", "X Axis", choices = character(0)),
                                              checkboxInput("scatter_x_log_space", "Apply Log-Transform", value = FALSE),
                                              selectInput("y_selected", "Y Axis", choices = character(0)),
                                              checkboxInput("scatter_y_log_space", "Apply Log-Transform", value = FALSE),
                                              selectInput("scatter_color_var", "Color by:", choices = character(0))
                                            ),
                                            selectInput(inputId = "trendline", label = "Trend Line Type", choices = c("linear", "polynomial", "exponential", "logarithmic", "power"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Correlation Matrix'",
                                            selectInput(inputId = "cor_method", "Correlation Method:", choices = c("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"), selected = "pearson"),
                                            checkboxInput("cor_include_cat", "Include Categorical Variables", value = FALSE),
                                            tags$small(class = "help-block", "Note: variables with no variation are dropped.")
                                          ),
                                          conditionalPanel(
                                            condition = "input.data_tabs == 'Outliers'",
                                            h4("Influence Settings"),
                                            selectInput("explore_infl_response", "Response Variable", choices = character(0)),
                                            tags$div(
                                              style = "display: flex; justify-content: space-between; align-items: center;",
                                              tags$label("Predictor Variables", `for` = "explore_infl_predictors", class = "control-label"),
                                              actionLink("explore_infl_select_all", "(Select All)",
                                                         style = "font-size: 12px; font-weight: bold;")
                                            ),
                                            selectInput("explore_infl_predictors", label = NULL,
                                                        choices = character(0), multiple = TRUE),
                                            tags$br(),
                                            div(align = "center", actionButton("explore_infl_compute", HTML("\u00a0Compute Influence"),
                                                                               icon = icon("calculator"), class = "btn-primary")),
                                            hr(),
                                            textInput("explore_infl_save_name", "Filtered Dataset Name", value = ""),
                                            div(align = "center", actionButton("explore_loglin_save_exclusions", HTML("\u00a0Save Filtered Dataset"),
                                                                               icon = icon("floppy-disk"), class = "btn-success"))
                                          )
                             ),
                             mainPanel(width = 10, style = "padding-right: 1.25%;",
                               tabsetPanel(id = "data_tabs",
                                           tabPanel("Data",
                                                    conditionalPanel(
                                                      condition = "input.data_select === null || input.data_select === ''",
                                                      tags$div(class = "c-welcome",
                                                        tags$span(class = "c-welcome-mark c-live-mark", `data-mode` = "always",
                                                                  tags$img(src = "static/ceres-logo-light.svg", alt = "")),
                                                        tags$h2("Start with a dataset"),
                                                        tags$p(class = "c-welcome-sub",
                                                               "Load an Excel workbook or a CSV file from the panel on the left. If a workbook has several sheets, you can choose which one to use."),
                                                        tags$div(class = "c-steps",
                                                          tags$div(class = "c-step", tags$span(class = "c-step-n", "1"),
                                                                   tags$div(tags$b("Load"), tags$span("Upload an Excel workbook or CSV of programs and costs."))),
                                                          tags$div(class = "c-step", tags$span(class = "c-step-n", "2"),
                                                                   tags$div(tags$b("Explore"), tags$span("Check summaries, missing data, potential outliers and correlations."))),
                                                          tags$div(class = "c-step", tags$span(class = "c-step-n", "3"),
                                                                   tags$div(tags$b("Generate"), tags$span("Fit, compare and tune CERs, then export the workbook.")))
                                                        )
                                                      )
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.data_select !== null && input.data_select !== ''",
                                                      uiOutput("repair_log_ui"),
                                                      uiOutput("data_health_ui"),
                                                      if (!identical(CERES_HEALTH_STYLE, "pill")) h4("Raw Data Table"),
                                                      withSpinner(DT::dataTableOutput('data_table'),
                                                                  color = getOption("spinner.color", default = loading_color),
                                                                  type = 1)
                                                    )
                                           ),
                                           tabPanel("Summary Stats",
                                                    h4("Summary Statistics for Numeric Data"),
                                                    DT::dataTableOutput('summary_stats_numeric', height = 'auto', fill = FALSE),
                                                    br(),
                                                    h4("Summary Statistics for Categorical Data"),
                                                    DT::dataTableOutput('summary_stats_cat', height = 'auto', fill = FALSE)
                                           ),
                                           tabPanel("Missing Data",
                                                    h4("Missing Data"),
                                                    tabBox(width = 12, id = "missing_box",
                                                           tabPanel("Impute Missing Data",
                                                                    tags$p(class = "c-hint", "Click on a row to see how imputation would change the distribution of the variable."),
                                                                    div(id = "input_placeholder_impute"),
                                                                    DT::dataTableOutput('impute_table', width = NULL)
                                                           ),
                                                           tabPanel("Mechanisms of Missingness",
                                                                    withSpinner(uiOutput("missing_tree_view"),
                                                                                color = getOption("spinner.color", default = loading_color),
                                                                                type = 1),
                                                                    tags$details(class = "c-mt-diagram",
                                                                      tags$summary(tags$span("\u25b6"), "Classic tree diagram"),
                                                                      plotOutput("missing_tree", height = 560)
                                                                    )
                                                           ),
                                                           tabPanel("Missingness Patterns", 
                                                                    tags$p(class = "c-hint", "Each column of dots is a set of variables missing together; the bar above it counts the rows with exactly that set. Bars on the left count each variable's gaps."),
                                                                    uiOutput("upset_ui")
                                                           )
                                                    )
                                           ),
                                           tabPanel("Transforms",
                                                    h4("Transform Data"),
                                                    tabBox(width = 12, id = "transform_box",
                                                           tabPanel("Transform",
                                                                    withSpinner(uiOutput("transforms_box"),
                                                                                color = getOption("spinner.color", default = loading_color),
                                                                                type = 1) 
                                                           ),
                                                           tabPanel("Subset",
                                                                    withSpinner(plotlyOutput("subsets", height = 650),
                                                                                color = getOption("spinner.color", default = loading_color),
                                                                                type = 1)
                                                           ),
                                                           tabPanel("Interact",
                                                                    uiOutput("interact_corr_plot_ui"),
                                                                    DT::dataTableOutput('interacts')
                                                           ),
                                                           tabPanel("Expand",
                                                                    DT::dataTableOutput('expand_preview_table')
                                                           )
                                                    )
                                           ),
                                           tabPanel("Collinearity",
                                                    h4("Collinearity check"),
                                                    tags$p(class = "c-hint", "Click Check for collinearity in the sidebar to check every numeric variable."),
                                                    uiOutput("vif_summary_header"),
                                                    uiOutput("vif_warnings"),
                                                    hr(),
                                                    h4("Final Candidate Set"),
                                                    uiOutput("vif_output"),
                                                    hr(),
                                                    h4("VIF for a set of predictors"),
                                                    tags$p(class = "c-hint",
                                                      "Pick the predictors you are considering for a CER. Each one's VIF is measured against all the others you picked, the same way the model tables' Max VIF is."),
                                                    tags$div(class = "c-map-bar",
                                                      selectInput("vifset_vars", NULL, choices = character(0), multiple = TRUE, width = "520px"),
                                                      checkboxInput("vifset_log", "Use logs (as Log Linear)", value = FALSE)),
                                                    uiOutput("vifset_table")
                                           ),
                                           tabPanel("Network",
                                                    h4("Network"),
                                                    tabsetPanel(id = "bn_subtabs",
                                                      tabPanel("Cost drivers",
                                                        tags$br(),
                                                        uiOutput("bn_focus_body")
                                                      ),
                                                      tabPanel("Groups",
                                                        tags$br(),
                                                        uiOutput("bn_groups_ui")
                                                      ),
                                                      tabPanel("Link matrix",
                                                        tags$br(),
                                                        uiOutput("bn_matrix_note"),
                                                        uiOutput("bn_matrix_box")
                                                      )
                                                    )
                                           ),
                                           tabPanel("Outliers",
                                                    h4("Outliers"),
                                                    tabsetPanel(id = "influence_subtabs",
                                                      tabPanel("Log-Linear",
                                                               tags$br(),
                                                               tags$p(class = "c-hint c-hint-pre", "Select a response variable and predictors in the sidebar, then click Compute Influence."),
                                                               tags$script(HTML("
                                                                 $(document).off('click.explore_loglin_excl').on('click.explore_loglin_excl', '.explore-loglin-excl-check', function() {
                                                                   var label = $(this).data('label');
                                                                   var checked = $(this).is(':checked');
                                                                   Shiny.setInputValue('explore_loglin_influence_checkbox',
                                                                     {label: label, checked: checked},
                                                                     {priority: 'event'});
                                                                 });
                                                               ")),
                                                               uiOutput("explore_loglin_influence_ui")
                                                      ),
                                                      tabPanel("Linear",
                                                               tags$br(),
                                                               tags$p(class = "c-hint c-hint-pre", "Select a response variable and predictors in the sidebar, then click Compute Influence."),
                                                               tags$script(HTML("
                                                                 $(document).off('click.explore_linear_excl').on('click.explore_linear_excl', '.explore-linear-excl-check', function() {
                                                                   var label = $(this).data('label');
                                                                   var checked = $(this).is(':checked');
                                                                   Shiny.setInputValue('explore_linear_influence_checkbox',
                                                                     {label: label, checked: checked},
                                                                     {priority: 'event'});
                                                                 });
                                                               ")),
                                                               uiOutput("explore_linear_influence_ui")
                                                      )
                                                    )
                                           ),
                                           tabPanel("Boxplots",
                                                    h4("Boxplots for Numeric Data"),
                                                    withSpinner(plotlyOutput("boxplots", height = 650), 
                                                                color = getOption("spinner.color", default = loading_color),
                                                                type = 1)  
                                           ),
                                           tabPanel("Scatterplots",
                                                    h4("Scatterplots"),
                                                    withSpinner(plotlyOutput("scatterplots", height = 550), 
                                                                color = getOption("spinner.color", default = loading_color),
                                                                type = 1),
                                                    DT::dataTableOutput('scatter_regression')
                                                    
                                           ),
                                           tabPanel("Correlation Matrix",
                                                    h4("Correlation Heatmap"),
                                                    withSpinner(plotlyOutput("heatmap", height = 650), 
                                                                color = getOption("spinner.color", default = loading_color),
                                                                type = 1)  
                                           ),
                               )
                             )
                           )
                  ),
                  
                  tabPanel("CER Generation",
                           sidebarLayout(
                             sidebarPanel(width = 2, class = "c-cer-sidebar",
                                          c_section("Data", "database"),
                                          selectInput(inputId = "data_select_cer_generation", label = "Dataset", choices = NULL),
                                          conditionalPanel(
                                            condition = "input.tabs_cer_generation != 'Non-Linear Models'",
                                            tags$div(id = "ols_predictor_ui",
                                              selectInput("ols_response", "Response Variable", choices = character(0)),
                                              tags$div(
                                                style = "display: flex; justify-content: space-between; align-items: center;",
                                                tags$label("Explanatory Variables", `for` = "ols_predictors", class = "control-label"),
                                                actionLink("ols_select_all", "(Select All)", style = "font-size: 12px; font-weight: bold;")
                                              ),
                                              selectInput("ols_predictors", label = NULL, choices = character(0), multiple = TRUE)
                                            ),
                                          ),
                                          c_section("Validation", "vial-circle-check"),
                                          selectInput("ols_holdout", "Test / Validation Method", choices = c("Train on all data" = "No (Train on all data)", "Holdout 20% for Testing" = "Yes (Holdout 20% for Testing)", "Holdout 50% for Testing" = "Yes (Holdout 50% for Testing)", "Manually Select Test Set" = "Yes (Manually Select Test Set)", "5-Fold Cross-Validation" = "Yes (5-Fold Cross-Validation)", "10-Fold Cross-Validation" = "Yes (10-Fold Cross-Validation)", "Leave-One-Out CV" = "Yes (Leave-One-Out CV)"), selected = "No (Train on all data)"),
                                          conditionalPanel(
                                            condition = "input.tabs_cer_generation == 'Linear Models' & input.linear_reg_box == 'Ordinary Least Squares'",
                                            c_section("Model", "sliders"),
                                            checkboxInput("ols_interact", "Include Interaction Effects", value = FALSE, width = NULL),
                                            selectInput("ols_penalty", "Regularization (Penalty)", choices = c("None", "LASSO", "Ridge", "Elastic Net"), selected = "None"),
                                            tags$div(id = "ols_train_bttn"),
                                            
                                            tags$br(),
                                            c_section("Simplify", "wand-magic-sparkles"),
                                            numericInput("simplify_n_vars", "Top N Unique Variables:", value = 10, min = 1),
                                            div(align = "center", actionButton("simplify_bttn", "Simplify Model"))
                                          ),
                                          conditionalPanel(
                                            condition = "input.tabs_cer_generation == 'Linear Models' & input.linear_reg_box == 'Ordinary Least Squares' & input.ord_ls == 'Predictions' & input.ols_penalty != 'None' & input.ols_holdout == 'Yes (Holdout 50% for Testing)'",
                                            div(align = "center", h5("Prediction Intervals are based on split conformal inference of test set residuals."))
                                          ),
                                          conditionalPanel(
                                            condition = "input.tabs_cer_generation == 'Linear Models' & input.linear_reg_box == 'Log Linear'",
                                            c_section("Model", "sliders"),
                                            checkboxInput("loglin_interact", "Include Interaction Effects", value = FALSE, width = NULL),
                                            selectInput("log_linear_error", "Error Term", choices = c("OLS", "MUPE", "ZMPE"), selected = "OLS"),
                                            checkboxInput("loglin_use_leaps", "Optimize Search", value = TRUE, width = NULL),
                                            tags$div(id = "best_subsets_train_bttn"),
                                          ),
                                          conditionalPanel(
                                            condition = "input.tabs_cer_generation == 'Linear Models' & input.linear_reg_box == 'Linear'",
                                            c_section("Model", "sliders"),
                                            checkboxInput("linear_simple_interact", "Include Interaction Effects", value = FALSE, width = NULL),
                                            selectInput("linear_error", "Error Term", choices = c("OLS", "MUPE", "ZMPE"), selected = "OLS"),
                                            checkboxInput("linear_use_leaps", "Optimize Search", value = TRUE, width = NULL),
                                            tags$div(id = "best_subsets_train_second_bttn"),
                                          ),
                                          conditionalPanel(
                                            condition = "input.tabs_cer_generation == 'Non-Linear Models'",
                                            c_section("Model", "sliders"),
                                            uiOutput("nl_sidebar_ui"),
                                            selectInput(inputId = "nl_error_term", label = "Error Term", choices = c("Least Squares", "MUPE")),
                                            tags$div(id = "nl_train_bttn"),
                                            tags$br()
                                          ),
                             ),
                             mainPanel(width = 10, style = "padding-right: 1.25%;",
                               tabsetPanel(id = "tabs_cer_generation",
                                           tabPanel("Linear Models",
                                                    tags$br(),
                                                    tabsetPanel(id = "linear_reg_box",
                                                                tabPanel("Ordinary Least Squares",
                                                                         tags$br(), 
                                                                         
                                                                         tabsetPanel(id = "ord_ls",
                                                                                     tabPanel("Coefficients",
                                                                                              DT::dataTableOutput("ols_cv_results"),
                                                                                              uiOutput("ols_coeffs_ui")
                                                                                     ),
                                                                                     tabPanel("Actuals vs Predicted",
                                                                                              tags$br(),
                                                                                              withSpinner(plotlyOutput("ols_actuals_vs_predicted", height = 450),
                                                                                                          color = getOption("spinner.color", default = loading_color),
                                                                                                          type = 1)   
                                                                                     ),
                                                                                     tabPanel("Residuals vs Predicted",
                                                                                              tags$br(),
                                                                                              withSpinner(plotlyOutput("ols_residuals_vs_predicted", height = 450),
                                                                                                          color = getOption("spinner.color", default = loading_color),
                                                                                                          type = 1)   
                                                                                     ),
                                                                                     tabPanel("Residuals vs Predictors",
                                                                                              tags$br(),
                                                                                              withSpinner(plotlyOutput("ols_residuals_vs_predictors", height = 560),
                                                                                                          color = getOption("spinner.color", default = loading_color),
                                                                                                          type = 1)   
                                                                                     ),
                                                                                     tabPanel("Residual Normality",
                                                                                              tags$br(),
                                                                                              withSpinner(plotlyOutput("ols_residual_normality", height = 450),
                                                                                                          color = getOption("spinner.color", default = loading_color),
                                                                                                          type = 1) 
                                                                                     ),
                                                                                     tabPanel("Predictions",
                                                                                              DT::dataTableOutput("ols_predict_table", fill = FALSE),
                                                                                              tags$div(style = "margin-top: 10px;"),
                                                                                              fluidRow(
                                                                                                column(width = 4,
                                                                                                       uiOutput("ols_predictions")
                                                                                                ),
                                                                                                column(width = 8,
                                                                                                       tabsetPanel(
                                                                                                         tabPanel("Prediction Distribution",
                                                                                                                  tags$br(),
                                                                                                                  withSpinner(
                                                                                                                    plotlyOutput("ols_dist_plot", height = 350),
                                                                                                                    color = getOption("spinner.color", default = loading_color),
                                                                                                                    type = 1
                                                                                                                  ),
                                                                                                                  tags$div(
                                                                                                                    style = "margin-top:12px; padding:0 4px;",
                                                                                                                    sliderInput(
                                                                                                                      "ols_pi_level",
                                                                                                                      label = NULL,
                                                                                                                      min   = 50, max = 99, value = 95, step = 1,
                                                                                                                      ticks = FALSE, post = "%",
                                                                                                                      width = "100%"
                                                                                                                    ),
                                                                                                                    tags$div(
                                                                                                                      style = "position:relative; margin-top:2px; height:16px; font-size:10px; color:#aaa; user-select:none;",
                                                                                                                      tags$span("50%", style = "position:absolute; left:0%;"),
                                                                                                                      tags$span("68%", style = "position:absolute; left:36.7%; transform:translateX(-50%);"),
                                                                                                                      tags$span("95%", style = "position:absolute; left:91.8%; transform:translateX(-50%);"),
                                                                                                                      tags$span("99%", style = "position:absolute; right:0%;")
                                                                                                                    )
                                                                                                                  ),
                                                                                                                  uiOutput("ols_dist_stats")
                                                                                                         ),
                                                                                                         tabPanel("Local Attributions",
                                                                                                                  tags$br(),
                                                                                                                  withSpinner(plotlyOutput("ols_breakdown", height = 400),
                                                                                                                              color = getOption("spinner.color", default = loading_color),
                                                                                                                              type = 1)
                                                                                                                  
                                                                                                                  
                                                                                                         )
                                                                                                       )
                                                                                                       
                                                                                                       
                                                                                                )
                                                                                              )
                                                                                     )
                                                                         )
                                                                ),
                                                                tabPanel("Log Linear",
                                                                         column(width = 12,
                                                                                tabsetPanel(id = "loglin_search_view",
                                                                                  tabPanel("All measures",
                                                                                    tags$div(id = "loglin_measures", class = "c-measures")),
                                                                                  tabPanel("Model map",
                                                                                    tags$div(class = "c-map-bar",
                                                                                      tags$span(class = "c-map-label", "Measure"),
                                                                                      selectInput("loglin_map_metric", NULL, choices = c("Adj. R-Squared", "R-Squared", "Weighted IC (%)", "Standard Error", "Coeff Significance (%)", "Geometric CV", "Multiplicative Error", "Max VIF"),
                                                                                                  selected = "Adj. R-Squared", width = "240px"),
                                                                                      uiOutput("loglin_map_note", inline = TRUE)),
                                                                                    withSpinner(plotlyOutput("loglin_model_map", height = 420), color = getOption("spinner.color", default = loading_color), type = 1))
                                                                                )
                                                                         ),
                                                                         column(width = 12,
                                                                                DT::dataTableOutput("selected_loglin_models")
                                                                         )
                                                                ),
                                                                tabPanel("Linear",
                                                                         column(width = 12,
                                                                                tabsetPanel(id = "linear_search_view",
                                                                                  tabPanel("All measures",
                                                                                    tags$div(id = "linear_measures", class = "c-measures")),
                                                                                  tabPanel("Model map",
                                                                                    tags$div(class = "c-map-bar",
                                                                                      tags$span(class = "c-map-label", "Measure"),
                                                                                      selectInput("linear_map_metric", NULL, choices = c("Adj. R-Squared", "R-Squared", "Weighted IC (%)", "Standard Error", "Coeff Significance (%)", "CV (%)", "MAPE (%)", "Max VIF"),
                                                                                                  selected = "Adj. R-Squared", width = "240px"),
                                                                                      uiOutput("linear_map_note", inline = TRUE)),
                                                                                    withSpinner(plotlyOutput("linear_model_map", height = 420), color = getOption("spinner.color", default = loading_color), type = 1))
                                                                                )
                                                                         ),
                                                                         column(width = 12,
                                                                                DT::dataTableOutput("selected_linear_simple_models")
                                                                         )
                                                                ),
                                                    )
                                           ),
                                           
                                           tabPanel("Non-Linear Models",
                                                    tags$br(),
                                                    uiOutput("nl_main_panel_ui")
                                           ),
                                           
                               )
                             )
                           )
                  ),
  
           nav_spacer(),
           nav_spacer(),
            nav_item(
            tags$div(class = "c-session",
              tags$button(type = "button", class = "c-session-btn", id = "c_session_btn",
                          title = "Save or open a session", `aria-haspopup` = "menu",
                          `aria-expanded` = "false", `aria-controls` = "c_session_menu",
                          icon("folder-open"), tags$span("Session"),
                          tags$span(class = "c-session-caret", icon("chevron-down"))),
              tags$div(class = "c-session-menu", id = "c_session_menu", role = "menu",
                tags$button(type = "button", class = "c-session-item", role = "menuitem",
                            `data-c-session` = "save",
                            tags$span(class = "c-si-icon", icon("download")),
                            tags$span(class = "c-si-text", tags$b("Save session"),
                                      tags$small("Data, selections and exclusions"))),
                tags$button(type = "button", class = "c-session-item", role = "menuitem",
                            `data-c-session` = "open",
                            tags$span(class = "c-si-icon", icon("folder-open")),
                            tags$span(class = "c-si-text", tags$b("Open session"),
                                      tags$small("Replaces what is loaded now"))),
                tags$button(type = "button", class = "c-session-item", role = "menuitem",
                            `data-c-session` = "new",
                            tags$span(class = "c-si-icon", icon("rotate-left")),
                            tags$span(class = "c-si-text", tags$b("Start fresh"),
                                      tags$small("Clears every dataset and result"))),
                tags$div(class = "c-session-foot", "Saved as .ceres.json - share it, or pick up where you left off.",
                         local({ v <- ceres_app_version(); if (nzchar(v)) tags$span(class = "c-session-ver", v) }))
              )
            )
            ),
            nav_item(
            tags$label(class = "c-theme-toggle", title = "Dark mode",
              tags$input(id = "dark_mode", type = "checkbox", `aria-label` = "Dark mode", autocomplete = "off"),
              tags$span(class = "c-tt-track",
                tags$span(class = "c-tt-thumb",
                  tags$span(class = "c-tt-sun", icon("sun")),
                  tags$span(class = "c-tt-moon", icon("moon"))))
            )
  ),
  footer = tags$div(class = "c-session-io", `aria-hidden` = "true",
    downloadButton("session_save", "Save session"),
    fileInput("session_open", NULL, accept = c(".json", "application/json")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('ceres_session_theme', function(m) {
        var el = document.getElementById('dark_mode');
        if (!el || el.checked === !!m.dark) return;
        el.checked = !!m.dark;
        el.dispatchEvent(new Event('change', { bubbles: true }));
      });"))
  )

              )

ceres_session_format_name    <- "ceres-session"
ceres_session_format_version <- 1L

ceres_session_num_json <- function(x, integer = FALSE) {
  if (!length(x)) return(structure("[]", class = "json"))
  s  <- rep("null", length(x))
  ok <- is.finite(x)
  if (any(ok)) {
    v <- x[ok]
    if (integer) {
      t <- sprintf("%d", as.integer(v))
    } else {
      t <- sprintf("%.15g", v)
      for (d in 16:17) {
        bad <- suppressWarnings(as.numeric(t)) != v
        if (!any(bad)) break
        t[bad] <- sprintf(paste0("%.", d, "g"), v[bad])
      }
      back <- jsonlite::parse_json(paste0("[", paste(t, collapse = ","), "]"), simplifyVector = TRUE)
      bad  <- as.numeric(back) != v
      if (any(bad)) t[bad] <- sprintf("%.17g", v[bad])
    }
    s[ok] <- t
  }
  structure(paste0("[", paste(s, collapse = ", "), "]"), class = "json")
}

ceres_session_encode_column <- function(name, x) {
  col <- list(name = name)
  if (is.factor(x)) {
    col$type   <- "factor"
    col$levels <- I(levels(x))
    if (is.ordered(x)) col$ordered <- TRUE
    col$values <- I(as.character(x))
  } else if (inherits(x, "POSIXct")) {
    col$type <- "POSIXct"
    if (!is.null(attr(x, "tzone"))) col$tz <- attr(x, "tzone")
    col$values <- ceres_session_num_json(unclass(x))
  } else if (inherits(x, "Date")) {
    col$type   <- "Date"
    col$values <- ceres_session_num_json(unclass(x))
  } else if (is.logical(x)) {
    col$type   <- "logical"
    col$values <- I(as.vector(x))
  } else if (is.integer(x)) {
    col$type   <- "integer"
    col$values <- ceres_session_num_json(x, integer = TRUE)
  } else if (is.double(x)) {
    col$type   <- "double"
    col$values <- ceres_session_num_json(as.vector(x))
    nf <- list(`NaN` = which(is.nan(x)), `Inf` = which(x == Inf), `-Inf` = which(x == -Inf))
    nf <- nf[lengths(nf) > 0L]
    if (length(nf)) col$nonfinite <- lapply(nf, I)
  } else if (is.character(x)) {
    col$type   <- "character"
    col$values <- I(as.vector(x))
  } else {
    col$type   <- "character"
    col$note   <- paste0("saved as text (was ", class(x)[1L], ")")
    col$values <- I(as.character(x))
  }
  col
}

ceres_session_encode_df <- function(df, name, provenance = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = TRUE)
  ri <- .row_names_info(df, 0L)
  rn <- if (is.integer(ri) && length(ri) == 2L && is.na(ri[1L]))
    list(type = "automatic", compact = ri[2L])
  else if (is.integer(ri))
    list(type = "integer", values = ceres_session_num_json(ri, integer = TRUE))
  else
    list(type = "character", values = I(as.character(ri)))
  out <- list(name = name, nrow = nrow(df), ncol = ncol(df), row_names = rn,
              columns = unname(Map(ceres_session_encode_column, names(df), df)))
  if (!is.null(provenance)) out$provenance <- provenance
  out
}

ceres_session_serialize <- function(datasets, active = list(), state = list(),
                                    ceres_version = NA_character_, saved_at = Sys.time(),
                                    provenance = NULL) {
  if (is.null(datasets)) datasets <- list()
  if (!is.list(datasets) || is.data.frame(datasets)) stop("datasets must be a named list of data frames.", call. = FALSE)
  nms <- names(datasets)
  if (length(datasets) && (is.null(nms) || any(is.na(nms) | !nzchar(nms)) || anyDuplicated(nms)))
    stop("Every dataset needs a unique, non-empty name.", call. = FALSE)
  ds <- lapply(seq_along(datasets), function(i)
    ceres_session_encode_df(datasets[[i]], nms[i],
                            if (!is.null(provenance)) provenance[[nms[i]]] else NULL))
  doc <- list(
    format        = ceres_session_format_name,
    version       = ceres_session_format_version,
    ceres_version = as.character(ceres_version),
    saved_at      = format(as.POSIXct(saved_at), "%Y-%m-%dT%H:%M:%S%z"),
    active        = active,
    datasets      = ds,
    state         = state
  )
  txt <- jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null",
                          json_verbatim = TRUE, digits = NA)
  enc2utf8(as.character(txt))
}

ceres_session_parse <- function(txt, max_version = ceres_session_format_version) {
  if (is.raw(txt)) {
    if (length(txt) >= 3L && identical(txt[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) txt <- txt[-(1:3)]
    txt <- rawToChar(txt)
  }
  txt <- paste(txt, collapse = "\n")
  Encoding(txt) <- "UTF-8"
  if (!nzchar(trimws(txt))) stop("The file is empty.", call. = FALSE)
  doc <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                                     simplifyMatrix = FALSE),
                  error = function(e) NULL)
  if (is.null(doc) || !is.list(doc) || is.null(names(doc)))
    stop("This is not a CERES session file (it is not readable JSON, or it is damaged).", call. = FALSE)
  if (!identical(doc$format, ceres_session_format_name))
    stop("This is not a CERES session file (it has no \"format\": \"ceres-session\").", call. = FALSE)
  v <- suppressWarnings(as.integer(doc$version))
  if (length(v) != 1L || is.na(v) || v < 1L)
    stop("This session file has no valid version number.", call. = FALSE)
  if (v > max_version)
    stop(sprintf(paste0("This session was saved by a newer CERES (session format %d; this CERES ",
                        "opens format %d and older). Update CERES to open it."), v, max_version),
         call. = FALSE)
  if (!is.null(doc$datasets) && !is.list(doc$datasets))
    stop("This session file is damaged (its dataset list is not readable).", call. = FALSE)
  doc
}

ceres_session_decode_column <- function(col, n) {
  nm   <- as.character(col$name)
  vals <- col$values
  if (is.list(vals)) vals <- unlist(vals)
  x <- switch(as.character(col$type),
    double = {
      x <- as.numeric(vals)
      nf <- col$nonfinite
      if (!is.null(nf)) {
        if (!is.null(nf[["NaN"]]))  x[as.integer(nf[["NaN"]])]  <- NaN
        if (!is.null(nf[["Inf"]]))  x[as.integer(nf[["Inf"]])]  <- Inf
        if (!is.null(nf[["-Inf"]])) x[as.integer(nf[["-Inf"]])] <- -Inf
      }
      x
    },
    integer   = as.integer(vals),
    logical   = as.logical(vals),
    character = as.character(vals),
    factor    = {
      lv <- col$levels
      if (is.list(lv)) lv <- unlist(lv)
      factor(as.character(vals), levels = as.character(lv), ordered = isTRUE(col$ordered))
    },
    Date      = structure(as.numeric(vals), class = "Date"),
    POSIXct   = {
      x <- structure(as.numeric(vals), class = c("POSIXct", "POSIXt"))
      if (!is.null(col$tz)) attr(x, "tzone") <- col$tz
      x
    },
    stop(sprintf("Column '%s' has an unknown type '%s'.", nm, as.character(col$type)), call. = FALSE)
  )
  if (length(x) != n)
    stop(sprintf("Column '%s' has %d values; the dataset has %d rows.", nm, length(x), n), call. = FALSE)
  x
}

ceres_session_decode_df <- function(d) {
  nm <- as.character(d$name)
  n  <- suppressWarnings(as.integer(d$nrow))
  if (length(n) != 1L || is.na(n) || n < 0L)
    stop(sprintf("Dataset '%s' has no valid row count.", nm), call. = FALSE)
  cols <- d$columns
  if (is.null(cols)) cols <- list()
  vals <- lapply(cols, ceres_session_decode_column, n = n)
  col_names <- vapply(cols, function(cl) as.character(cl$name), character(1L))
  rn <- d$row_names
  rn_vals <- rn$values
  if (is.list(rn_vals)) rn_vals <- unlist(rn_vals)
  rn_attr <- switch(as.character(rn$type),
    automatic = c(NA_integer_, if (is.null(rn$compact)) -n else as.integer(rn$compact)),
    integer   = as.integer(rn_vals),
    character = as.character(rn_vals),
    stop(sprintf("Dataset '%s' has unreadable row names.", nm), call. = FALSE))
  if (!identical(rn$type, "automatic") && length(rn_attr) != n)
    stop(sprintf("Dataset '%s' has %d row names for %d rows.", nm, length(rn_attr), n), call. = FALSE)
  df <- vals
  names(df) <- col_names
  attr(df, "row.names") <- rn_attr
  class(df) <- "data.frame"
  df
}

ceres_session_restore_data <- function(doc) {
  out <- list()
  for (d in doc$datasets) {
    nm <- d$name
    if (!is.character(nm) || length(nm) != 1L || is.na(nm) || !nzchar(nm))
      stop("A dataset in this session has no name.", call. = FALSE)
    if (!is.null(out[[nm]])) stop(sprintf("Dataset '%s' appears twice in this session.", nm), call. = FALSE)
    out[[nm]] <- ceres_session_decode_df(d)
  }
  out
}

ceres_session_restore_provenance <- function(doc) {
  out <- list()
  for (d in doc$datasets) if (!is.null(d$provenance)) out[[d$name]] <- d$provenance
  out
}

ceres_session_skip_ids <- c("navTo", "dark_mode", "data_upload", "data_upload_sheet",
                            "session_open", "session_refit_now", "impute_choice")
ceres_session_keep_input <- function(id, value) {
  if (id %in% ceres_session_skip_ids) return(FALSE)
  if (grepl(paste0("^\\.|^plotly_|^ceres_hidden|-",
                   "|_rows_(selected|current|all)$|_cells?_(selected|clicked|info|edit)$",
                   "|_columns_selected$|_search(_columns)?$|_state$|_cell_clicked$|_row_last_clicked$"), id))
    return(FALSE)
  if (is.null(value)) return(TRUE)
  if (inherits(value, "shinyActionButtonValue")) return(FALSE)
  is.atomic(value) && is.null(oldClass(value)) && (is.character(value) || is.numeric(value) || is.logical(value))
}

ceres_session_input_equal <- function(cur, want) {
  if (is.list(want)) want <- unlist(want)
  if (is.list(cur))  cur  <- unlist(cur)
  if (!length(cur) && !length(want)) return(TRUE)
  if (length(cur) != length(want)) return(FALSE)
  if (is.numeric(cur) && is.numeric(want))
    return(isTRUE(all.equal(as.numeric(cur), as.numeric(want), tolerance = 1e-9, check.attributes = FALSE)))
  if (is.logical(cur) || is.logical(want)) return(identical(as.logical(cur), as.logical(want)))
  identical(as.character(cur), as.character(want))
}

ceres_session_app_version <- function(paths) {
  for (p in paths) {
    if (!file.exists(p)) next
    l1 <- tryCatch(readLines(p, n = 1L, warn = FALSE), error = function(e) "")
    m  <- regmatches(l1, regexpr("v[0-9][0-9A-Za-z.\\-]*", l1))
    if (length(m)) return(sub("^v", "", m))
  }
  NA_character_
}

ceres_session_version <- ceres_session_app_version(
  unique(c("CERES.R", file.path(dirname(get0("expected_www", ifnotfound = "www")), "CERES.R"))))

sanitize_nl_colname <- function(name) {
  out <- gsub("[^a-zA-Z0-9_]", "_", name)
  if (!identical(make.names(out), out)) out <- paste0("v_", out)
  out
}

nl_display_names <- function(eq, cols) {
  if (!is.character(eq) || length(eq) != 1L || is.na(eq)) return(eq)
  cols <- unique(as.character(cols))
  safe <- vapply(cols, function(n) tryCatch(sanitize_nl_colname(n), error = function(e) n), character(1))
  for (i in order(nchar(safe), decreasing = TRUE)) {
    if (identical(safe[[i]], cols[[i]])) next
    eq <- gsub(paste0("(?<![A-Za-z0-9_.])", gsub("([^A-Za-z0-9_])", "\\\\\\1", safe[[i]]), "(?![A-Za-z0-9_.])"),
               gsub("\\", "\\\\", cols[[i]], fixed = TRUE), eq, perl = TRUE)
  }
  eq
}

nl_name_collisions <- function(cols) {
  san <- vapply(cols, sanitize_nl_colname, character(1))
  dup <- unique(san[duplicated(san)])
  if (length(dup) == 0L) return(NULL)
  groups <- vapply(dup, function(s) paste0("'", cols[san == s], "'", collapse = " and "), character(1))
  paste0("Column names ", paste(groups, collapse = "; "),
         " become the same Non-Linear identifier. Rename one of them first.")
}

bt <- function(x) {
  vapply(as.character(x), function(n) deparse(as.name(n), backtick = TRUE),
         character(1), USE.NAMES = FALSE)
}
unbt <- function(x) {
  if (length(x) == 0L) return(x)
  m <- gregexpr("`((?:[^`\\\\]|\\\\.)*)`", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(s)
    vapply(s, function(q) as.character(str2lang(q)), character(1), USE.NAMES = FALSE))
  x
}
CERES_MS_RULES <- list(
  r2  = list(col = "Adj. R-Squared",         op = ">=", pct = TRUE,  v = 70,  on = FALSE),
  sig = list(col = "Coeff Significance (%)", op = ">=", pct = TRUE,  v = 75,  on = FALSE),
  vif = list(col = "Max VIF",                op = "<",  pct = FALSE, v = 5,   on = FALSE),
  nv  = list(col = "# Variables",            op = "<=", pct = FALSE, v = 3,   on = FALSE),
  se  = list(col = "Standard Error",         op = "<=", pct = FALSE, v = 0.5, on = FALSE))
CERES_MS_LOWER_IS_BETTER <- c("# Variables", "Standard Error", "Geometric CV", "Multiplicative Error",
                              "CV (%)", "MAPE (%)", "Max VIF")
ceres_measures_ui <- function(ui = NULL) {
  n <- 50; basis <- "wic"
  rules <- lapply(CERES_MS_RULES, function(r) list(on = r$on, v = r$v))
  if (is.list(ui)) {
    if (!is.null(ui$n)) n <- ui$n
    if (identical(ui$basis, "all")) basis <- "all"
    for (k in names(ui$rules)) if (!is.null(rules[[k]]) && is.list(ui$rules[[k]])) {
      u <- ui$rules[[k]]
      rules[[k]] <- list(on = isTRUE(as.logical(u$on)), v = suppressWarnings(as.numeric(u$v))[1])
    }
  }
  if (!identical(n, "all")) {
    n <- suppressWarnings(as.numeric(n))[1]
    if (is.na(n) || n == 0) n <- 50
    n <- as.integer(max(1, floor(n + 0.5)))
  }
  on <- Filter(function(r) isTRUE(r$on), rules)
  list(n = n, basis = basis, rules = lapply(on, function(r) r$v))
}
ceres_measures_pick <- function(tbl, ui = ceres_measures_ui()) {
  N <- nrow(tbl)
  if (!N) return(integer(0))
  num <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
  X <- lapply(num, function(cn) { x <- as.numeric(tbl[[cn]]); x[!is.finite(x)] <- NA; x })
  names(X) <- num
  X <- X[vapply(X, function(x) any(!is.na(x)), logical(1))]
  all <- identical(ui$n, "all") || ui$n >= N
  n <- if (all) N else ui$n
  pass <- rep(TRUE, N)
  for (id in names(ui$rules)) {
    r <- CERES_MS_RULES[[id]]; x <- X[[r$col]]
    if (is.null(x)) next
    t <- if (r$pct) ui$rules[[id]] / 100 else ui$rules[[id]]
    ok <- switch(r$op, ">=" = x >= t - 1e-9, "<" = x < t, "<=" = x <= t + 1e-9)
    pass <- pass & !is.na(ok) & ok
  }
  cand <- which(pass)
  if (identical(ui$basis, "all") && !all) {
    for (cn in names(X)) {
      x <- X[[cn]]
      o <- order(x, decreasing = !(cn %in% CERES_MS_LOWER_IS_BETTER), na.last = NA, method = "radix")
      k <- seq_along(o)
      rank <- rep(length(o) + 1, N)
      rank[o] <- cummax(ifelse(c(TRUE, diff(x[o]) != 0), k, 0L))
      cand <- cand[rank[cand] <= n]
    }
  }
  if (length(cand) > 1 && length(X)) {
    wn <- if ("Weighted IC (%)" %in% names(X)) "Weighted IC (%)" else
          if ("Adj. R-Squared" %in% names(X)) "Adj. R-Squared" else names(X)[length(X)]
    cand <- cand[order(X[[wn]][cand], decreasing = !(wn %in% CERES_MS_LOWER_IS_BETTER),
                       na.last = TRUE, method = "radix")]
  }
  if (identical(ui$basis, "wic") && !all) cand <- utils::head(cand, n)
  sort(cand)
}
CERES_MS_CAP <- 500L
CERES_MS_CONTEXT <- 1000L
ceres_ms_cap <- function() {
  v <- suppressWarnings(as.integer(getOption("ceres.measures_cap", CERES_MS_CAP)))
  if (length(v) != 1L || is.na(v) || v < 1L) CERES_MS_CAP else v
}
ceres_measures_ui_capped <- function(ui, N, cap = ceres_ms_cap()) {
  if (N <= cap) return(ui)
  ui$n <- if (identical(ui$n, "all")) cap else min(as.integer(ui$n), cap)
  ui
}
CERES_MS_BAND_P <- c(0.05, 0.25, 0.5, 0.75, 0.95)
ceres_measures_static <- function(tbl) {
  N <- nrow(tbl)
  num <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
  out <- list(N = N, cols = list())
  for (cn in num) {
    x <- as.numeric(tbl[[cn]]); x[!is.finite(x)] <- NA
    if (!any(!is.na(x))) next
    hi <- !(cn %in% CERES_MS_LOWER_IS_BETTER)
    o <- order(x, decreasing = hi, na.last = NA, method = "radix")
    k <- length(o)
    rank <- rep(k + 1L, N)
    first <- c(TRUE, diff(x[o]) != 0)
    rank[o] <- cummax(ifelse(first, seq_len(k), 0L))
    lo <- min(x, na.rm = TRUE); hv <- max(x, na.rm = TRUE)
    y_rank <- if (N > 1) 1 - (rank - 1) / (N - 1) else rep(0.5, N)
    y_raw  <- if (hv > lo) (x - lo) / (hv - lo) else rep(0.5, N)
    out$cols[[cn]] <- list(rank = rank, range = c(lo, hv), best = x[o[1]], worst = x[o[k]], worst_rank = rank[o[k]],
                           band_rank = unname(stats::quantile(y_rank, CERES_MS_BAND_P, type = 7, names = FALSE)),
                           band_raw  = unname(stats::quantile(y_raw, CERES_MS_BAND_P, type = 7, na.rm = TRUE, names = FALSE)))
  }
  out
}
ceres_measures_message <- function(tbl, ui = ceres_measures_ui(), st = NULL, cap = ceres_ms_cap(), context = CERES_MS_CONTEXT) {
  N <- nrow(tbl)
  num <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
  vals <- function(rows) lapply(num, function(cn) { x <- as.numeric(tbl[[cn]][rows]); x[!is.finite(x)] <- NA; I(x) })
  if (N <= cap)
    return(list(rows = seq_len(N), msg = list(cols = I(num), names = I(rownames(tbl)), values = vals(seq_len(N)))))
  if (is.null(st)) st <- ceres_measures_static(tbl)
  ue <- ceres_measures_ui_capped(ui, N, cap)
  keep <- ceres_measures_pick(tbl, ue)
  pass <- rep(TRUE, N)
  for (id in names(ue$rules)) {
    r <- CERES_MS_RULES[[id]]; x <- suppressWarnings(as.numeric(tbl[[r$col]]))
    if (is.null(tbl[[r$col]]) || !any(is.finite(x))) next
    x[!is.finite(x)] <- NA
    t <- if (r$pct) ue$rules[[id]] / 100 else ue$rules[[id]]
    ok <- switch(r$op, ">=" = x >= t - 1e-9, "<" = x < t, "<=" = x <= t + 1e-9)
    pass <- pass & !is.na(ok) & ok
  }
  cand <- which(pass)
  wn <- if ("Weighted IC (%)" %in% names(st$cols)) "Weighted IC (%)" else
        if ("Adj. R-Squared" %in% names(st$cols)) "Adj. R-Squared" else utils::tail(names(st$cols), 1)
  if (length(wn) && length(cand) > 1) {
    w <- as.numeric(tbl[[wn]][cand]); w[!is.finite(w)] <- NA
    cand <- cand[order(w, decreasing = !(wn %in% CERES_MS_LOWER_IS_BETTER), na.last = TRUE, method = "radix")]
  }
  elig <- sort(union(keep, utils::head(cand, cap)))
  rest <- setdiff(seq_len(N), elig)
  ctx <- if (length(rest) && context > 0)
    rest[unique(round(seq(1, length(rest), length.out = min(context, length(rest)))))] else integer(0)
  rows <- sort(c(elig, ctx))
  sc <- st$cols[intersect(num, names(st$cols))]
  list(rows = rows, msg = list(
    cols = I(num), names = I(rownames(tbl)[rows]), values = vals(rows),
    total = N, cap = cap, ctx = I(rows %in% ctx),
    ranks = lapply(num, function(cn) if (is.null(sc[[cn]])) NULL else I(sc[[cn]]$rank[rows])),
    range = lapply(num, function(cn) if (is.null(sc[[cn]])) NULL else I(sc[[cn]]$range)),
    best = lapply(num, function(cn) if (is.null(sc[[cn]])) NULL else sc[[cn]]$best),
    worst = lapply(num, function(cn) if (is.null(sc[[cn]])) NULL else sc[[cn]]$worst),
    worst_rank = lapply(num, function(cn) if (is.null(sc[[cn]])) NULL else sc[[cn]]$worst_rank),
    band_rank = lapply(num, function(cn) if (is.null(sc[[cn]])) NULL else I(sc[[cn]]$band_rank)),
    band_raw = lapply(num, function(cn) if (is.null(sc[[cn]])) NULL else I(sc[[cn]]$band_raw))))
}
ceres_number_formats <- function(df, cols) {
  cols <- cols[cols %in% colnames(df)]
  cols <- cols[vapply(cols, function(cl) is.numeric(df[[cl]]), logical(1L))]
  out  <- list(decimal = character(0), whole = character(0), year = character(0))
  for (cl in cols) {
    x <- df[[cl]]; x <- x[is.finite(x)]
    whole <- length(x) > 0L && all(x == round(x))
    year_name  <- grepl("(^|[^a-z])(year|fy|yr)([^a-z]|$)|year", cl, ignore.case = TRUE)
    year_range <- whole && all(x >= 1800 & x <= 2200)
    grp <- if (whole && (year_name || year_range)) "year" else if (whole) "whole" else "decimal"
    out[[grp]] <- c(out[[grp]], cl)
  }
  out
}
CERES_BOX_ALL_POINTS_MAX <- 1000L
ceres_box_points <- function(n) if (n > CERES_BOX_ALL_POINTS_MAX) "suspectedoutliers" else "all"

ceres_boxplots_fig <- function(df, vars, pc, log_space = FALSE) {
  pal <- c("rgba(31,119,180,1)", "rgba(255,127,14,1)", "rgba(44,160,44,1)", "rgba(214,39,40,1)",
           "rgba(148,103,189,1)", "rgba(140,86,75,1)", "rgba(227,119,194,1)", "rgba(127,127,127,1)",
           "rgba(188,189,34,1)", "rgba(23,190,207,1)")
  n <- length(vars)
  ax <- function(kind, i) if (i == 1L) kind else paste0(kind, i)
  axl <- function(kind, i) if (i == 1L) paste0(kind, "axis") else paste0(kind, "axis", i)
  traces <- list(); lay <- list(); notes <- list()
  for (k in seq_len(n)) {
    v <- vars[k]; data <- df[, v]; name <- v
    if (isTRUE(log_space)) {
      if (any(data <= 0, na.rm = TRUE)) data[data <= 0] <- NA
      data <- log(data); name <- paste0("log(", v, ")")
    }
    keep <- !is.na(data)
    labels <- rownames(df)[keep]; data <- data[keep]
    if (n == 1L) { xi <- 1L; yb <- 1L; yh <- 2L } else { xi <- 2L * k - 1L; yh <- 2L * k - 1L; yb <- 2L * k }
    traces[[length(traces) + 1L]] <- list(
      fillcolor = paste0(pc, "2E"), x = data, boxpoints = ceres_box_points(length(data)), jitter = 0.35, pointpos = 0,
      line = list(color = pc, width = 1.5),
      marker = list(color = pc, size = 5, opacity = 0.55, line = list(color = pal[(2L * k - 2L) %% 10L + 1L])),
      text = labels, name = "​", type = "box", orientation = "h",
      xaxis = ax("x", xi), yaxis = ax("y", yb), frame = NULL)
    traces[[length(traces) + 1L]] <- list(
      x = data, marker = list(color = paste0(pc, "8C"), line = list(color = "rgba(255,255,255,0.9)", width = 1)),
      name = name, type = "histogram",
      error_y = list(color = pal[(2L * k - 1L) %% 10L + 1L]), error_x = list(color = pal[(2L * k - 1L) %% 10L + 1L]),
      xaxis = ax("x", xi), yaxis = ax("y", yh), frame = NULL)
    if (n == 1L) {
      lay$xaxis  <- list(domain = c(0, 1), automargin = TRUE, title = "", anchor = "y2", zeroline = FALSE)
      lay$xaxis2 <- list(title = "", zeroline = FALSE)
      lay$yaxis  <- list(domain = c(0.7, 1), automargin = TRUE, anchor = "x", title = "", zeroline = FALSE, showticklabels = FALSE)
      lay$yaxis2 <- list(domain = c(0, 0.66), automargin = TRUE, anchor = "x", title = "", zeroline = FALSE)
      x0 <- 0; ylo <- 0; h <- 1
    } else {
      nc <- ceiling(n / 2); r <- (k - 1L) %/% nc + 1L; cc <- (k - 1L) %% nc + 1L
      xd <- c((cc - 1) / nc + if (cc > 1) 0.03 else 0, cc / nc - if (cc < nc) 0.03 else 0)
      yd <- if (r == 1L) c(0.5 + 0.06, 1) else c(0, 0.5 - 0.06)
      x0 <- xd[1]; ylo <- yd[1]; h <- yd[2] - yd[1]
      lay[[axl("x", 2L * k - 1L)]] <- list(domain = xd, automargin = TRUE, title = "", anchor = ax("y", 2L * k - 1L), zeroline = FALSE)
      lay[[axl("x", 2L * k)]]      <- list(title = "", zeroline = FALSE, anchor = ax("y", 2L * k), domain = xd)
      lay[[axl("y", 2L * k - 1L)]] <- list(domain = c(ylo, ylo + 0.66 * h), automargin = TRUE, anchor = ax("x", 2L * k - 1L), zeroline = FALSE)
      lay[[axl("y", 2L * k)]]      <- list(domain = c(ylo + 0.7 * h, ylo + h), automargin = TRUE, anchor = ax("x", 2L * k - 1L),
                                           zeroline = FALSE, showticklabels = FALSE)
    }
    notes[[k]] <- list(text = paste0("<b>", name, "</b>"), x = x0, y = ylo + 1.02 * h, xref = "paper", yref = "paper",
                       xanchor = "left", yanchor = "bottom", showarrow = FALSE, font = list(size = 12))
  }
  lay <- c(lay, list(annotations = notes, shapes = list(), images = list(),
                     margin = list(b = 48, l = 48, t = 36, r = 20), hovermode = "closest", showlegend = FALSE,
                     bargap = 0.04, hoverlabel = list(namelength = 0)))
  p <- plotly::as_widget(list(data = traces, layout = lay,
                              config = list(modeBarButtonsToAdd = c("hoverclosest", "hovercompare"),
                                            showSendToCloud = FALSE, displayModeBar = FALSE)))
  p$x$source <- "A"
  p$x$base_url <- tryCatch(plotly:::get_domain(), error = function(e) "https://plot.ly")
  p$x$subplot <- TRUE
  p
}
ceres_upset_data <- function(df) {
  miss <- is.na(df)
  sz <- colSums(miss)
  sz <- sz[sz > 0]
  if (!length(sz)) return(NULL)
  ord  <- order(sz)
  sets <- rev(names(sz)[ord])
  m <- miss[, sets, drop = FALSE] * 1L
  m <- m[rowSums(m) > 0, , drop = FALSE]
  key <- apply(m, 1, paste, collapse = "")
  combos <- sort(unique(key))
  freq <- as.integer(table(factor(key, levels = combos)))
  o <- order(freq, decreasing = TRUE, method = "radix")
  combos <- combos[o]; freq <- freq[o]
  inter <- t(vapply(strsplit(combos, ""), function(b) b == "1", logical(length(sets))))
  if (length(sets) == 1L) inter <- t(inter)
  colnames(inter) <- sets
  list(sets = sets, set_size = unname(sz[sets]), inter = inter, freq = freq)
}
ceres_upset_draw <- function(ud, bar_col = "#2F6FE4", dot_col = "#334155", empty_col = "#E3E3E3",
                             shade_col = "#F0F3F7", axis_col = "#555555", text_col = "#111111", dark = FALSE) {
  if (isTRUE(dark)) { dot_col <- "#CBD5E1"; empty_col <- "#3A4352"; shade_col <- "#1A2230"
                      axis_col <- "#9AA4B2"; text_col <- "#E2E8F0"; bar_col <- "#5B8DEF" }
  op <- par(mar = c(0, 0, 0, 0), xpd = NA, family = "", col = text_col, fg = text_col)
  on.exit(par(op))
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  ns <- length(ud$sets); ni <- length(ud$freq)
  labels <- paste0(rev(ud$sets), "_NA")
  pad   <- strwidth("M", cex = 0.95)
  lab_w <- max(strwidth(labels, cex = 0.95)) + pad
  sx0 <- max(0.040, strwidth("000", cex = 0.85) + 0.01)
  sx1 <- max(sx0 + 0.10, 0.214 - max(0, lab_w - 0.069))
  lab_x <- sx1 + pad * 0.6 + lab_w / 2
  mx0 <- sx1 + pad * 0.6 + lab_w + pad * 0.4; mx1 <- 0.991
  my0 <- 0.082; my1 <- 0.654
  by1 <- 0.985
  rowh <- (my1 - my0) / ns
  yc <- my1 - rowh * (seq_len(ns) - 0.5)
  sets_top <- rev(ud$sets)
  xc <- mx0 + (mx1 - mx0) * seq_len(ni) / (ni + 1)
  bw <- (mx1 - mx0) / (ni + 1) * 0.6
  for (r in seq_len(ns)) if ((ns - r) %% 2 == 0)
    rect(mx0, yc[r] - rowh / 2, mx1, yc[r] + rowh / 2, col = shade_col, border = NA)
  ymax <- max(ud$freq); tk <- pretty(c(0, ymax), n = 4); tk <- tk[tk <= ymax & tk == round(tk)]
  ytop <- ymax * 1.16
  yb <- function(v) my1 + (by1 - my1) * v / ytop
  rect(xc - bw / 2, my1, xc + bw / 2, yb(ud$freq), col = bar_col, border = NA)
  text(xc, yb(ud$freq), ud$freq, pos = 3, offset = 0.45, col = bar_col, cex = 0.95)
  segments(mx0, my1, mx0, by1, col = axis_col)
  tk <- tk[tk > 0]
  segments(mx0 - 0.005, yb(tk), mx0, yb(tk), col = axis_col)
  text(mx0 - 0.008, yb(tk), tk, adj = c(1, 0.5), col = axis_col, cex = 0.95)
  text(mx0 - strwidth(max(tk), cex = 0.95) - pad * 1.6, (my1 + by1) / 2, "Intersection Size", srt = 90, cex = 1.15)
  smax <- max(ud$set_size); st <- pretty(c(0, smax), n = 4); st <- st[st <= smax & st == round(st)]
  sxw <- function(v) sx1 - (sx1 - sx0) * v / smax
  sh <- rowh * 0.4
  rect(sxw(rev(ud$set_size)), yc - sh / 2, sx1, yc + sh / 2, col = bar_col, border = NA)
  text(sxw(rev(ud$set_size)), yc, rev(ud$set_size), pos = 2, offset = 0.35, col = bar_col, cex = 0.85)
  segments(sx0 - 0.005, my0, sx1 + 0.007, my0, col = axis_col)
  segments(sxw(st), my0, sxw(st), my0 - 0.008, col = axis_col)
  text(sxw(st), my0 - 0.012, st, adj = c(0.5, 1), col = axis_col, cex = 0.95)
  text((sx0 + sx1) / 2, my0 - 0.055, "Set Size", cex = 1.15)
  text(lab_x, yc, labels, adj = c(0.5, 0.5), cex = 0.95)
  for (k in seq_len(ni)) {
    on <- ud$inter[k, sets_top]
    points(rep(xc[k], ns), yc, pch = 16, cex = 1.45, col = ifelse(on, dot_col, empty_col))
    if (sum(on) > 1) segments(xc[k], min(yc[on]), xc[k], max(yc[on]), col = dot_col, lwd = 2.2)
  }
  invisible(NULL)
}
ceres_empty_state <- function(text)
  tags$div(class = "c-empty-card", tags$div(class = "c-empty-text", text))
ceres_format_numbers <- function(dt, df, cols) {
  f <- ceres_number_formats(df, cols)
  if (length(f$decimal)) dt <- DT::formatCurrency(dt, f$decimal, currency = "", interval = 3, mark = ",")
  if (length(f$whole))   dt <- DT::formatCurrency(dt, f$whole, currency = "", interval = 3, mark = ",", digits = 0)
  if (length(f$year))    dt <- DT::formatRound(dt, f$year, digits = 0, mark = "")
  dt
}
ceres_cv_design <- function(formula, data) {
  mf    <- stats::model.frame(formula, data, na.action = stats::na.fail)
  terms <- attr(mf, "terms")
  x     <- stats::model.matrix(terms, mf)
  cons  <- attr(x, "contrasts")
  list(mf = mf, terms = terms, cons = cons, y = stats::model.response(mf),
       x = x[, colnames(x) != "(Intercept)", drop = FALSE])
}
ceres_cv_index <- function(y, method, number, index, seed) {
  if (!is.null(index)) return(index)
  withr::with_seed(seed, {
    rs_seed <- sample.int(.Machine$integer.max, 1L)
    withr::with_seed(rs_seed, ceres_ns("caret")$createFolds(y, if (method == "LOOCV") length(y) else number, returnTrain = TRUE))
  })
}
ceres_cv_metrics <- function(pred, obs) {
  ok <- !is.na(pred); pred <- pred[ok]; obs <- obs[ok]
  r <- if (length(unique(pred)) < 2 || length(unique(obs)) < 2) NA else
    tryCatch(stats::cor(pred, obs, use = "pairwise.complete.obs"), error = function(e) NA)
  out <- c(RMSE = sqrt(mean((pred - obs)^2)), Rsquared = r^2, MAE = mean(abs(pred - obs)))
  out[is.nan(out)] <- NA
  out
}
ceres_cv_summary <- function(per) {
  data.frame(RMSE = mean(per[, "RMSE"], na.rm = TRUE), Rsquared = mean(per[, "Rsquared"], na.rm = TRUE),
             MAE = mean(per[, "MAE"], na.rm = TRUE), RMSESD = stats::sd(per[, "RMSE"], na.rm = TRUE),
             RsquaredSD = stats::sd(per[, "Rsquared"], na.rm = TRUE), MAESD = stats::sd(per[, "MAE"], na.rm = TRUE))
}
ceres_cv_new_x <- function(object, newdata) {
  if (is.null(newdata)) newdata <- object$trainingData[, names(object$trainingData) != ".outcome", drop = FALSE]
  tt <- stats::delete.response(object$terms)
  mf <- stats::model.frame(tt, newdata, na.action = stats::na.omit, xlev = object$xlevels)
  mm <- stats::model.matrix(tt, mf, contrasts.arg = object$contrasts)
  mm[, colnames(mm) != "(Intercept)", drop = FALSE]
}
ceres_lm_cv <- function(formula, data, method = c("repeatedcv", "cv", "LOOCV"), number = 10L, index = NULL, seed = 23L) {
  method <- match.arg(method)
  ds <- ceres_cv_design(formula, data); y <- ds$y; n <- length(y)
  index <- ceres_cv_index(y, method, number, index, seed)
  dat <- as.data.frame(ds$x, stringsAsFactors = TRUE)
  dat$.outcome <- y
  fit <- function(rows) stats::lm(.outcome ~ ., data = dat[rows, , drop = FALSE])
  folds <- lapply(seq_along(index), function(i) {
    out_rows <- setdiff(seq_len(n), index[[i]])
    p <- suppressWarnings(stats::predict(fit(index[[i]]), dat[out_rows, , drop = FALSE]))
    data.frame(intercept = TRUE, pred = unname(p), obs = y[out_rows], rowIndex = out_rows,
               Resample = names(index)[i], stringsAsFactors = FALSE)
  })
  pred <- do.call(rbind, folds)
  if (method == "LOOCV") {
    pred$Resample <- NULL
    pred <- pred[order(pred$rowIndex), , drop = FALSE]
    m <- ceres_cv_metrics(pred$pred, pred$obs)
    results  <- data.frame(intercept = TRUE, RMSE = m[["RMSE"]], Rsquared = m[["Rsquared"]], MAE = m[["MAE"]])
    resample <- NULL
  } else {
    per <- t(vapply(folds, function(f) ceres_cv_metrics(f$pred, f$obs), numeric(3L)))
    resample <- data.frame(per, Resample = names(index), stringsAsFactors = FALSE)
    results  <- cbind(data.frame(intercept = TRUE), ceres_cv_summary(per))
  }
  row.names(pred) <- NULL
  training <- data.frame(.outcome = y, ds$mf[, -1L, drop = FALSE], check.names = FALSE)
  structure(list(method = "lm", modelType = "Regression", metric = "RMSE", results = results, pred = pred,
                 resample = resample, finalModel = fit(seq_len(n)), trainingData = training, terms = ds$terms,
                 coefnames = colnames(ds$x), contrasts = ds$cons, xlevels = stats::.getXlevels(ds$terms, ds$mf), index = index),
            class = "ceres_lm_cv")
}
predict.ceres_lm_cv <- function(object, newdata = NULL, ...)
  stats::predict(object$finalModel, as.data.frame(ceres_cv_new_x(object, newdata), stringsAsFactors = TRUE))
residuals.ceres_lm_cv <- function(object, ...) stats::residuals(object$finalModel, ...)
ceres_glmnet_predict <- function(x, y, alpha, newx, s) {
  ns <- ceres_ns("glmnet")
  slow <- function() unname(stats::predict(ns$glmnet(x, y, alpha = alpha, family = "gaussian"), newx, s = s))
  nobs <- nrow(x); nvars <- ncol(x)
  if (nvars < 2L || anyNA(x) || anyNA(y)) return(slow())
  y <- as.double(y); storage.mode(x) <- "double"
  if (sum((y - mean(y))^2) == 0) return(slow())
  big <- ns$glmnet.control()$big
  cl  <- rbind(rep(-big, nvars), rep(big, nvars)); storage.mode(cl) <- "double"
  ne  <- as.integer(nvars + 1L); nx <- as.integer(min(ne * 2L + 20L, nvars)); nlam <- 100L
  fit <- tryCatch(ns$elnet_exp(if (nvars < 500L) 1L else 2L, parm = as.double(alpha), x, y, rep(1, nobs), 0L,
                               rep(1, nvars), cl, ne, nx, nlam, if (nobs < nvars) 0.01 else 1e-4, double(1),
                               1e-7, 1L, 1L, 100000L, NULL, lmu = integer(1), a0 = double(nlam),
                               ca = matrix(0, nrow = nx, ncol = nlam), ia = integer(nx), nin = integer(nlam),
                               rsq = double(nlam), alm = double(nlam), nlp = integer(1), jerr = integer(1)),
                  error = function(e) NULL)
  if (is.null(fit) || fit$jerr != 0L || fit$lmu < 1L) return(slow())
  lmu <- fit$lmu
  beta <- matrix(0, nvars, lmu)
  ninmax <- max(fit$nin[seq_len(lmu)])
  if (ninmax > 0L) beta[fit$ia[seq_len(ninmax)], ] <- fit$ca[seq_len(ninmax), seq_len(lmu), drop = FALSE]
  lam <- fit$alm[seq_len(lmu)]
  if (length(lam) > 2L) { ll <- log(lam); lam[1] <- exp(2 * ll[2] - ll[3]) }
  nbeta <- rbind(fit$a0[seq_len(lmu)], beta)
  li <- ns$lambda.interp(lam, s)
  nb <- sweep(nbeta[, li$left, drop = FALSE], 2, li$frac, `*`) + sweep(nbeta[, li$right, drop = FALSE], 2, 1 - li$frac, `*`)
  unname(cbind(1, newx) %*% nb)
}
ceres_glmnet_cv <- function(formula, data, alpha, lambda, method = c("repeatedcv", "cv"), number = 10L, index = NULL, seed = 23L) {
  method <- match.arg(method)
  ds <- ceres_cv_design(formula, data); y <- ds$y; n <- length(y); x <- ds$x
  index  <- ceres_cv_index(y, method, number, index, seed)
  lambda <- unique(lambda)
  lambda <- c(max(lambda), lambda[-which.max(lambda)])
  fit <- function(rows) ceres_ns("glmnet")$glmnet(x[rows, , drop = FALSE], y[rows], alpha = alpha, family = "gaussian")
  folds <- lapply(seq_along(index), function(i) {
    out_rows <- setdiff(seq_len(n), index[[i]])
    p <- ceres_glmnet_predict(x[index[[i]], , drop = FALSE], y[index[[i]]], alpha,
                              x[out_rows, , drop = FALSE], lambda)
    list(out = out_rows, p = matrix(p, nrow = length(out_rows)))
  })
  per_lambda <- lapply(seq_along(lambda), function(j)
    t(vapply(folds, function(f) ceres_cv_metrics(f$p[, j], y[f$out]), numeric(3L))))
  results <- do.call(rbind, lapply(seq_along(lambda), function(j)
    cbind(data.frame(alpha = alpha, lambda = lambda[j]), ceres_cv_summary(per_lambda[[j]]))))
  ord <- order(results$lambda)
  results <- results[ord, , drop = FALSE]; per_lambda <- per_lambda[ord]
  row.names(results) <- NULL
  best <- which.min(results$RMSE)
  bestTune <- results[best, c("alpha", "lambda")]
  nl <- length(lambda); nf <- length(folds)
  fold_lam <- lapply(seq_len(nf), function(i) vapply(seq_len(nl), function(j) ceres_cv_metrics(folds[[i]]$p[, j], y[folds[[i]]$out]), numeric(3L)))
  all_res <- data.frame(alpha = alpha, lambda = rep(lambda, nf),
                        RMSE = unlist(lapply(fold_lam, function(m) m["RMSE", ])),
                        Rsquared = unlist(lapply(fold_lam, function(m) m["Rsquared", ])),
                        MAE = unlist(lapply(fold_lam, function(m) m["MAE", ])),
                        Resample = rep(names(index), each = nl), stringsAsFactors = FALSE)
  resample <- merge(all_res, bestTune)
  resample <- resample[, c("RMSE", "Rsquared", "MAE", "Resample"), drop = FALSE]
  n_out <- vapply(folds, function(f) length(f$out), integer(1L))
  all_pred <- data.frame(alpha = alpha,
                         lambda = unlist(lapply(seq_len(nf), function(i) rep(lambda, each = n_out[i]))),
                         pred = unlist(lapply(folds, function(f) as.vector(f$p))),
                         obs = unlist(lapply(folds, function(f) rep(y[f$out], nl))),
                         rowIndex = unlist(lapply(folds, function(f) rep(f$out, nl))),
                         Resample = rep(names(index), n_out * nl), stringsAsFactors = FALSE)
  pred <- merge(all_pred, bestTune)
  row.names(pred) <- NULL; row.names(resample) <- NULL
  final <- fit(seq_len(n))
  final$lambdaOpt <- bestTune$lambda
  training <- data.frame(.outcome = y, ds$mf[, -1L, drop = FALSE], check.names = FALSE)
  structure(list(method = "glmnet", modelType = "Regression", metric = "RMSE", results = results, bestTune = bestTune,
                 pred = pred, resample = resample, finalModel = final, trainingData = training, terms = ds$terms,
                 coefnames = colnames(x), contrasts = ds$cons, xlevels = stats::.getXlevels(ds$terms, ds$mf), index = index),
            class = "ceres_glmnet_cv")
}
predict.ceres_glmnet_cv <- function(object, newdata = NULL, ...)
  stats::predict(object$finalModel, ceres_cv_new_x(object, newdata), s = object$finalModel$lambdaOpt, type = "response")[, 1]
residuals.ceres_glmnet_cv <- function(object, ...)
  object$trainingData$.outcome - predict.ceres_glmnet_cv(object)
registerS3method("predict", "ceres_lm_cv", predict.ceres_lm_cv)
registerS3method("residuals", "ceres_lm_cv", residuals.ceres_lm_cv)
registerS3method("predict", "ceres_glmnet_cv", predict.ceres_glmnet_cv)
registerS3method("residuals", "ceres_glmnet_cv", residuals.ceres_glmnet_cv)
tex_text <- function(s) {
  s <- gsub("\\", "\001", s, fixed = TRUE)
  s <- gsub("([{}$%#&_])", "\\\\\\1", s)
  s <- gsub("~", "\\textasciitilde{}", s, fixed = TRUE)
  s <- gsub("^", "\\textasciicircum{}", s, fixed = TRUE)
  s <- gsub("`", "\\char\"60{}", s, fixed = TRUE)
  gsub("\001", "\\textbackslash{}", s, fixed = TRUE)
}
is_interaction_col <- function(vars, pool = vars, source = character(0)) {
  vapply(vars, function(v) {
    p <- strsplit(v, ":", fixed = TRUE)[[1]]
    length(p) > 1L && all(p %in% pool) && !(v %in% source)
  }, logical(1), USE.NAMES = FALSE)
}
is_dummy01 <- function(col) is.numeric(col) && length(col) > 0L && all(col %in% c(0, 1))
tex_num <- function(s) {
  m <- regmatches(s, regexec("^(-?[0-9.]+)e([+-])([0-9]+)$", s))[[1]]
  if (length(m) == 0L) return(s)
  paste0(m[2], " \\times 10^{", if (m[3] == "-") "-" else "", as.integer(m[4]), "}")
}
tex_base <- function(s) {
  s <- tex_num(s)
  if (grepl("times", s, fixed = TRUE)) paste0("\\left(", s, "\\right)") else s
}

loglin_design_term <- function(v, b, mf, log1p_cols = character(0), fmt,
                               times = " \u00d7 ", source = character(0)) {
  vars <- setdiff(names(mf)[-1], "(weights)")
  if (!(v %in% vars) || !is.numeric(mf[[v]])) return(NULL)
  tx <- function(z) paste0("\\text{", tex_text(z), "}")
  if (is_interaction_col(v, vars, source)) {
    p <- strsplit(v, ":", fixed = TRUE)[[1]]
    if (length(p) != 2L || !all(vapply(mf[p], is.numeric, logical(1L)))) return(NULL)
    dum    <- vapply(mf[p], is_dummy01, logical(1L))
    l1p    <- p %in% log1p_cols
    ln_t   <- function(i) if (l1p[i]) paste0("ln(", p[i], " + 1)") else paste0("ln(", p[i], ")")
    ln_x   <- function(i) if (l1p[i]) paste0("\\ln(", tx(p[i]), " + 1)") else paste0("\\ln(", tx(p[i]), ")")
    if (!any(dum)) {
      return(list(text  = paste0("exp(", fmt(b), times, ln_t(1), times, ln_t(2), ")"),
                  latex = paste0("e^{", tex_num(fmt(b)), " \\cdot ", ln_x(1), " \\cdot ", ln_x(2), "}"),
                  value = b, exp_form = FALSE))
    }
    if (all(dum)) {
      return(list(text  = paste0(fmt(exp(b)), " ^ (", p[1], times, p[2], ")"),
                  latex = paste0(tex_base(fmt(exp(b))), "^{", tx(p[1]), " \\cdot ", tx(p[2]), "}"),
                  value = exp(b), exp_form = TRUE))
    }
    a <- which(!dum); d <- which(dum)
    a_t <- if (l1p[a]) paste0("(", p[a], " + 1)") else p[a]
    a_x <- if (l1p[a]) paste0("(", tx(p[a]), " + 1)") else tx(p[a])
    return(list(text  = paste0(a_t, " ^ (", fmt(b), times, p[d], ")"),
                latex = paste0(a_x, "^{", tex_num(fmt(b)), " \\cdot ", tx(p[d]), "}"),
                value = b, exp_form = FALSE))
  }
  if (is_dummy01(mf[[v]])) {
    return(list(text  = paste0(fmt(exp(b)), " ^ ", v),
                latex = paste0(tex_base(fmt(exp(b))), "^{", tx(v), "}"),
                value = exp(b), exp_form = TRUE))
  }
  NULL
}

cer_equation <- function(coefs, mf, xlevels, dep_var, mode,
                         log1p_cols = character(0), fmt, source = character(0)) {
  names(coefs) <- unbt(names(coefs))
  coefs   <- coefs[!is.na(coefs)]
  icpt    <- if ("(Intercept)" %in% names(coefs)) coefs[["(Intercept)"]] else NA_real_
  term_cn <- setdiff(names(coefs), "(Intercept)")
  tx  <- function(v) paste0("\\text{", tex_text(v), "}")
  lhs_txt <- paste0(dep_var, " = ")
  lhs_tex <- paste0(tx(dep_var), " = ")
  vals <- numeric(0); expf <- logical(0)
  rec  <- function(v, val, e) { vals[[v]] <<- val; expf[[v]] <<- e }
  if (mode == "loglin") {
    vars     <- setdiff(names(mf)[-1], "(weights)")
    num_vars <- vars[vapply(mf[vars], is.numeric, logical(1L))]
    fac_vars <- setdiff(vars, num_vars)
    txt <- character(0); tex <- character(0); used <- character(0)
    add_pow <- function(v, b) {
      rec(v, b, FALSE)
      if (v %in% log1p_cols) {
        txt <<- c(txt, paste0("(", v, " + 1) ^ ", fmt(b)))
        tex <<- c(tex, paste0("(", tx(v), " + 1)^{", tex_num(fmt(b)), "}"))
      } else {
        txt <<- c(txt, paste0(v, " ^ ", fmt(b)))
        tex <<- c(tex, paste0(tx(v), "^{", tex_num(fmt(b)), "}"))
      }
    }
    add_dummy <- function(d, b) {
      rec(d, exp(b), TRUE)
      txt <<- c(txt, paste0(fmt(exp(b)), " ^ ", d))
      tex <<- c(tex, paste0(tex_base(fmt(exp(b))), "^{", tx(d), "}"))
    }
    for (v in intersect(num_vars, term_cn)) {
      sp <- loglin_design_term(v, coefs[[v]], mf, log1p_cols, fmt, " \u00d7 ", source)
      if (is.null(sp)) add_pow(v, coefs[[v]])
      else { txt <- c(txt, sp$text); tex <- c(tex, sp$latex); rec(v, sp$value, sp$exp_form) }
      used <- c(used, v)
    }
    for (cat in fac_vars) {
      lvls <- xlevels[[cat]]
      if (is.null(lvls)) next
      for (d in intersect(paste0(cat, lvls), term_cn)) { add_dummy(d, coefs[[d]]); used <- c(used, d) }
    }
    for (v in setdiff(term_cn, used)) {
      p <- strsplit(v, ":", fixed = TRUE)[[1]]
      if (length(p) > 1L && all(p %in% num_vars)) add_pow(v, coefs[[v]]) else add_dummy(v, coefs[[v]])
    }
    b0 <- fmt(exp(if (is.na(icpt)) 0 else icpt))
    if (!is.na(icpt)) rec("(Intercept)", exp(icpt), TRUE)
    list(text  = paste(c(paste0(lhs_txt, b0), txt), collapse = " \u00d7 "),
         latex = paste(c(paste0(lhs_tex, tex_num(b0)), tex), collapse = " \\times "),
         values = vals, exp_form = expf)
  } else {
    b0  <- if (!is.na(icpt)) fmt(icpt) else "0"
    if (!is.na(icpt)) rec("(Intercept)", icpt, FALSE)
    txt <- paste0(lhs_txt, b0)
    tex <- paste0(lhs_tex, tex_num(b0))
    for (v in term_cn) {
      b <- coefs[[v]]
      rec(v, b, FALSE)
      txt <- paste0(txt, if (b >= 0) " + " else " \u2212 ", fmt(abs(b)), " \u00d7 ", v)
      tex <- paste0(tex, if (b >= 0) " + " else " - ", tex_num(fmt(abs(b))), " \\times ", tx(v))
    }
    list(text = txt, latex = tex, values = vals, exp_form = expf)
  }
}

cer_fmt_eq <- function(x, digits = 4) {
  if (is.na(x)) return("0")
  ax <- abs(x)
  if (ax == 0) return("0")
  if (ax < 1e-3 || ax >= 1e6) formatC(x, format = "e", digits = digits)
  else                        formatC(x, format = "f", digits = digits)
}
cer_fmt_num   <- function(x, digits = 5) if (is.na(x)) "—" else formatC(x, digits = digits, format = "g")
cer_fmt_p     <- function(p) if (is.na(p)) "—" else if (p < 0.001) "< 0.001" else formatC(p, digits = 3, format = "f")
cer_sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else ""
}

cer_lm_diagnostics <- function(model) {
  resids        <- residuals(model); fitted_vals <- fitted(model)
  n_r           <- length(resids);   n_coef_np   <- sum(names(coef(model)) != "(Intercept)")
  norm_result   <- tryCatch(if (n_r <= 5000) shapiro.test(resids) else ceres_ns("nortest")$ad.test(resids), error = function(e) NULL)
  hetero_result <- tryCatch(cor.test(abs(resids), fitted_vals, method = "spearman", exact = FALSE), error = function(e) NULL)
  cook_d        <- tryCatch(cooks.distance(model), error = function(e) NULL)
  vif_max       <- if (n_coef_np > 1) tryCatch({
    max(vif_unweighted(model)$graded, na.rm = TRUE)
  }, error = function(e) NA_real_) else NA_real_
  fmt_p <- cer_fmt_p
  row <- function(label, value_str, pass, note)
    data.frame(Test = label, Result = value_str, pass = pass, Note = note, stringsAsFactors = FALSE)

  norm_row <- if (!is.null(norm_result)) {
    p <- norm_result$p.value; m <- if (n_r <= 5000) "Shapiro-Wilk" else "Anderson-Darling"
    row("Normality", paste0(m, "  p = ", fmt_p(p)), p > 0.05,
        if (p > 0.05) "Residuals consistent with normality"
        else "Non-normal residuals — consider transformations or robust methods")
  } else row("Normality", "—", NA, "Could not compute")

  hetero_row <- if (!is.null(hetero_result)) {
    p <- hetero_result$p.value
    row("Homoscedasticity", paste0("Spearman  p = ", fmt_p(p)), p > 0.05,
        if (p > 0.05) "No significant heteroscedasticity detected"
        else "Heteroscedasticity detected — consider WLS or robust SE")
  } else row("Homoscedasticity", "—", NA, "Could not compute")

  infl_row <- if (!is.null(cook_d)) {
    n_infl <- sum(cook_d > 4 / n_r, na.rm = TRUE)
    row("Influential Points", paste0(n_infl, " obs (Cook’s D > 4/n)"), n_infl == 0,
        if (n_infl == 0) "No highly influential observations"
        else paste0(n_infl, " obs may strongly influence the fit — review in Outliers tab"))
  } else row("Influential Points", "—", NA, "Could not compute")

  vif_row <- if (is.finite(vif_max))
    row("Multicollinearity", paste0("Max VIF = ", round(vif_max, 2)), vif_max < 10,
        if (vif_max < 10) "No severe collinearity detected"
        else "High collinearity (VIF > 10) — consider removing correlated predictors")
  else if (n_coef_np <= 1)
    row("Multicollinearity", "Single predictor", NA, "VIF not applicable")
  else
    row("Multicollinearity", "—", NA, "Could not compute")

  rbind(norm_row, hetero_row, infl_row, vif_row)
}

ceres_html_esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

ceres_report_fmt_value <- function(x) {
  if (is.factor(x) || is.character(x) || is.logical(x)) return(as.character(x))
  vapply(x, function(v) {
    if (is.na(v)) return("—")
    if (is.finite(v) && abs(v) < 1e15 && v == round(v)) return(formatC(v, format = "d", big.mark = ","))
    trimws(formatC(signif(v, 6), format = "fg", digits = 6, big.mark = ","))
  }, character(1), USE.NAMES = FALSE)
}

ceres_report_unit <- function(name) {
  m <- regmatches(name, regexec("\\(([^()]+)\\)\\s*$", name))[[1]]
  if (length(m) == 2L) trimws(m[2]) else NA_character_
}

ceres_report_table <- function(df, class = "tbl") {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0L) return("")
  cn    <- names(df)
  align <- ifelse(seq_along(cn) == 1L, "l", ifelse(cn %in% c("Sig.", "Status"), "c",
                  ifelse(cn %in% c("Note", "Reason", "Result", "Unit"), "l", "r")))
  th <- paste0("<th class=\"", align, "\">", ceres_html_esc(cn), "</th>", collapse = "")
  body <- if (nrow(df) == 0L) "" else paste0(vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(seq_along(cn), function(j) {
      paste0("<td class=\"", align[j], "\">", ceres_html_esc(df[[j]][i]), "</td>")
    }, character(1))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }, character(1)), collapse = "\n")
  paste0("<table class=\"", class, "\"><thead><tr>", th, "</tr></thead><tbody>\n",
         body, "\n</tbody></table>")
}

CERES_REPORT_CSS <- "
:root { --ink:#0f172a; --ink2:#334155; --muted:#64748b; --faint:#94a3b8;
  --line:#e2e8f0; --line2:#cbd5e1; --accent:#2f6fe4; --accent-ink:#1d4fb8;
  --accent-soft:#eef3fd; --good:#16a34a; --warn:#b45309; --bad:#dc2626; }
* { box-sizing:border-box; }
html, body { background:#ffffff; color:var(--ink); margin:0; }
body { font-family:'SF Pro Text',-apple-system,BlinkMacSystemFont,'Segoe UI Variable Text','Segoe UI',Roboto,Helvetica,Arial,sans-serif;
  font-size:13.5px; line-height:1.5; -webkit-font-smoothing:antialiased; }
.page { max-width:900px; margin:0 auto; padding:40px 40px 56px; }
header.top { border-bottom:2px solid var(--accent); padding-bottom:16px; margin-bottom:22px; }
.kicker { font-size:11px; font-weight:700; letter-spacing:.12em; text-transform:uppercase; color:var(--accent); margin:0 0 6px; }
h1 { font-size:24px; line-height:1.25; font-weight:700; letter-spacing:-.01em; margin:0 0 4px; }
.subtitle { color:var(--ink2); margin:0 0 12px; font-size:14px; }
dl.meta { display:flex; flex-wrap:wrap; gap:6px 26px; margin:0; font-size:12.5px; }
dl.meta div { display:flex; gap:6px; }
dl.meta dt { color:var(--muted); font-weight:500; margin:0; }
dl.meta dd { margin:0; font-weight:600; color:var(--ink); }
section { margin:0 0 26px; }
h2 { font-size:11.5px; font-weight:700; letter-spacing:.1em; text-transform:uppercase; color:var(--muted);
  margin:0 0 10px; padding-bottom:6px; border-bottom:1px solid var(--line); }
h3 { font-size:13px; font-weight:650; margin:16px 0 8px; color:var(--ink2); }
.eq { background:var(--accent-soft); border-left:4px solid var(--accent); border-radius:10px; padding:14px 18px; }
.eq .lbl { font-size:10.5px; font-weight:700; letter-spacing:.1em; text-transform:uppercase; color:var(--accent-ink); margin-bottom:6px; }
.eq code { display:block; font-family:'JetBrains Mono','SF Mono',Consolas,'Liberation Mono',Menlo,monospace; font-size:14px;
  color:var(--ink); white-space:pre-wrap; word-break:break-word; background:none; }
.warn { margin:10px 0 0; padding:8px 12px; border:1px solid var(--bad); color:var(--bad); border-radius:8px; font-weight:600; }
.tiles { display:flex; flex-wrap:wrap; gap:10px; }
.tile { flex:1 1 110px; border:1px solid var(--line); border-radius:10px; padding:10px 14px; text-align:center; }
.tile .l { font-size:10.5px; color:var(--muted); text-transform:uppercase; letter-spacing:.05em; }
.tile .v { font-size:18px; font-weight:700; font-variant-numeric:tabular-nums; margin-top:2px; }
.tile .s { font-size:10.5px; color:var(--muted); margin-top:1px; }
table { width:100%; border-collapse:collapse; font-size:12.5px; font-variant-numeric:tabular-nums; }
table.compact { font-size:11px; }
thead th { text-align:left; font-weight:650; color:var(--ink2); background:#f8fafc; border-bottom:2px solid var(--accent); padding:7px 10px; }
tbody td { padding:6px 10px; border-bottom:1px solid var(--line); vertical-align:top; }
table.compact thead th, table.compact tbody td { padding:4px 8px; }
tbody tr:nth-child(even) td { background:#fbfcfe; }
.l { text-align:left; } .r { text-align:right; } .c { text-align:center; }
table.even th, table.even td { text-align:center; width:20%; }
td.r { font-family:'JetBrains Mono','SF Mono',Consolas,'Liberation Mono',Menlo,monospace; font-size:.95em; }
.note { color:var(--muted); font-size:11.5px; margin:8px 0 0; }
.grid2 { display:grid; grid-template-columns:1fr 1fr; gap:16px; }
.kv { display:grid; grid-template-columns:repeat(auto-fill, minmax(190px, 1fr)); gap:0 18px; font-size:12.5px; }
.kv div { display:flex; justify-content:space-between; gap:10px; padding:5px 0; border-bottom:1px solid var(--line); }
.kv div.wide { grid-column:1 / -1; }
.kv span { color:var(--muted); } .kv b { font-weight:600; font-variant-numeric:tabular-nums; text-align:right; word-break:break-word; }
figure { margin:0; border:1px solid var(--line); border-radius:10px; padding:10px 10px 8px; }
figure img { display:block; width:100%; height:auto; }
figcaption { font-size:11.5px; color:var(--muted); margin-top:6px; }
figcaption b { color:var(--ink); font-weight:650; }
ul.use { margin:0; padding-left:20px; } ul.use li { margin:0 0 6px; }
.notes { font-size:11px; color:var(--muted); border-top:1px solid var(--line); padding-top:10px; margin-top:28px; }
.notes p { margin:0 0 4px; }
@media (max-width:700px) { .page { padding:24px 16px 40px; } .grid2 { grid-template-columns:1fr; } }
@page { size:auto; margin:14mm 12mm; }
@media print {
  html, body { background:#ffffff !important; }
  * { -webkit-print-color-adjust:exact; print-color-adjust:exact; box-shadow:none !important; text-shadow:none !important; }
  body { font-size:11.5px; }
  .page { max-width:none; padding:0; }
  section, figure, .eq, .tiles, tr { break-inside:avoid; page-break-inside:avoid; }
  section.flow { break-inside:auto; page-break-inside:auto; }
  h2, h3 { break-after:avoid; page-break-after:avoid; }
  thead { display:table-header-group; }
  .pb { break-before:page; page-break-before:always; }
  .grid2 { grid-template-columns:1fr 1fr; }
}
"

ceres_cer_report_html <- function(parts) {
  stopifnot(is.list(parts), length(parts$title) == 1L, length(parts$equation) == 1L)
  e   <- ceres_html_esc
  has <- function(x) !is.null(x) && length(x) > 0L && !(is.data.frame(x) && nrow(x) == 0L) &&
    !(is.character(x) && length(x) == 1L && (is.na(x) || !nzchar(x)))
  note <- function(x) if (has(x)) paste0("<p class=\"note\">", e(x), "</p>") else ""
  sec  <- function(id, title, ..., cls = NULL)
    paste0("<section id=\"", id, "\"", if (!is.null(cls)) paste0(" class=\"", cls, "\""), ">",
           "<h2>", e(title), "</h2>", ..., "</section>\n")

  meta_html <- if (has(parts$meta)) paste0("<dl class=\"meta\">",
    paste0("<div><dt>", e(names(parts$meta)), "</dt><dd>", e(parts$meta), "</dd></div>", collapse = ""),
    "</dl>") else ""
  head_html <- paste0(
    "<header class=\"top\"><p class=\"kicker\">CER report</p>",
    "<h1>", e(parts$title), "</h1>",
    if (has(parts$subtitle)) paste0("<p class=\"subtitle\">", e(parts$subtitle), "</p>") else "",
    meta_html, "</header>\n")

  eq_html <- sec("sec-equation", "Equation",
    "<div class=\"eq\"><div class=\"lbl\">", e(parts$equation_label %||% "CER equation"), "</div>",
    "<code>", e(parts$equation), "</code></div>",
    if (has(parts$warning)) paste0("<p class=\"warn\">", e(parts$warning), "</p>") else "")

  fit_html <- if (has(parts$stats) || has(parts$metrics)) {
    tiles <- if (has(parts$stats)) {
      st <- parts$stats
      sub <- if (!is.null(st$sub)) st$sub else rep(NA_character_, nrow(st))
      paste0("<div class=\"tiles\">", paste0(vapply(seq_len(nrow(st)), function(i)
        paste0("<div class=\"tile\"><div class=\"l\">", e(st$label[i]), "</div><div class=\"v\">",
               e(st$value[i]), "</div>",
               if (!is.na(sub[i]) && nzchar(sub[i])) paste0("<div class=\"s\">", e(sub[i]), "</div>") else "",
               "</div>"), character(1)), collapse = ""), "</div>")
    } else ""
    met <- if (has(parts$metrics)) {
      m <- parts$metrics
      paste0("<h3>Model search measures</h3><div class=\"kv\">",
             paste0("<div", ifelse(nchar(m[[2]]) > 20, " class=\"wide\"", ""), "><span>", e(m[[1]]),
                    "</span><b>", e(m[[2]]), "</b></div>", collapse = ""),
             "</div>", note(parts$metrics_note))
    } else ""
    sec("sec-fit", "Fit statistics", tiles, note(parts$stats_note), met)
  } else ""

  coef_html <- if (has(parts$coefficients)) sec("sec-coef", "Coefficients",
    ceres_report_table(parts$coefficients), note(parts$coef_note),
    if (has(parts$residuals)) paste0("<h3>Residuals</h3>",
                                     ceres_report_table(parts$residuals, class = "tbl even")) else "",
    if (has(parts$diagnostics)) paste0("<h3>Regression diagnostics</h3>",
                                       ceres_report_table(parts$diagnostics)) else "") else ""

  plots_html <- if (has(parts$plots)) sec("sec-plots", "Plots",
    "<div class=\"grid2\">", paste0(vapply(parts$plots, function(p)
      paste0("<figure><img alt=\"", e(p$title), "\" src=\"", p$src, "\">",
             "<figcaption><b>", e(p$title), "</b>",
             if (has(p$caption)) paste0(" — ", e(p$caption)) else "", "</figcaption></figure>"),
      character(1)), collapse = ""), "</div>") else ""

  ranges_html <- if (has(parts$ranges)) sec("sec-ranges", "Validity range",
    ceres_report_table(parts$ranges), note(parts$ranges_note)) else ""

  usage_html <- if (has(parts$usage)) sec("sec-usage", "How to use this CER",
    "<ul class=\"use\">", paste0("<li>", e(parts$usage), "</li>", collapse = ""), "</ul>") else ""

  data_html <- if (has(parts$programs) || has(parts$excluded)) sec("sec-data", "Data", cls = "flow pb",
    if (has(parts$programs)) paste0("<h3>Programs used (", nrow(parts$programs), ")</h3>",
                                    ceres_report_table(parts$programs, class = "tbl compact"),
                                    note(parts$programs_note)) else "",
    if (has(parts$excluded)) paste0("<h3>Programs not used (", nrow(parts$excluded), ")</h3>",
                                    ceres_report_table(parts$excluded, class = "tbl compact")) else "",
    note(parts$excluded_note)) else ""

  notes_html <- if (has(parts$notes)) paste0("<div class=\"notes\">",
    paste0("<p>", e(parts$notes), "</p>", collapse = ""), "</div>") else ""

  paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", e(paste("CER report -", parts$title)), "</title>\n",
    "<style>", CERES_REPORT_CSS, "</style>\n</head>\n<body>\n<main class=\"page\">\n",
    head_html, eq_html, fit_html, coef_html, plots_html, ranges_html, usage_html,
    data_html, notes_html,
    "</main>\n</body>\n</html>\n")
}

ceres_report_scatter_png <- function(x, y, xlab, ylab, ref = c("identity", "zero"),
                                     x_test = NULL, y_test = NULL,
                                     width = 900, height = 680) {
  ref <- match.arg(ref)
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  opened <- tryCatch({ grDevices::png(f, width = width, height = height, res = 150); TRUE },
                     error = function(e) FALSE)
  if (!opened) {
    opened <- tryCatch({ grDevices::png(f, width = width, height = height, res = 150, type = "cairo"); TRUE },
                       error = function(e) FALSE)
  }
  if (!opened) return(NA_character_)
  ok <- tryCatch({
    xs <- c(x, x_test); ys <- c(y, y_test)
    xs <- xs[is.finite(xs)]; ys <- ys[is.finite(ys)]
    xl <- range(xs); yl <- range(ys)
    if (ref == "identity") xl <- yl <- range(c(xs, ys))
    if (ref == "zero") yl <- range(c(yl, 0))
    tick_lab <- function(v) formatC(v, format = "fg", digits = 6, big.mark = ",")
    xt <- pretty(xl); yt <- pretty(yl)
    ylabs <- trimws(tick_lab(yt))
    left  <- 2.2 + 0.62 * max(nchar(ylabs))
    graphics::par(mar = c(4.2, left, 1.2, 2.8), mgp = c(2.6, 0.7, 0), las = 1,
                  family = "sans", col.axis = "#475569", col.lab = "#334155",
                  fg = "#94a3b8", cex.axis = 0.95, cex.lab = 1.05, bg = "white")
    graphics::plot.new()
    graphics::plot.window(xlim = xl, ylim = yl)
    graphics::abline(v = xt, h = yt, col = "#e2e8f0", lwd = 0.8)
    graphics::axis(1, at = xt, labels = trimws(tick_lab(xt)), col = NA, col.ticks = "#cbd5e1")
    graphics::axis(2, at = yt, labels = ylabs, col = NA, col.ticks = "#cbd5e1")
    graphics::title(xlab = xlab)
    graphics::title(ylab = ylab, line = left - 1.4)
    if (ref == "identity") graphics::abline(0, 1, col = "#334155", lty = 2, lwd = 1.4)
    else                   graphics::abline(h = 0, col = "#334155", lty = 2, lwd = 1.4)
    graphics::points(x, y, pch = 21, bg = grDevices::adjustcolor("#2F6FE4", 0.75),
                     col = "#1d4fb8", cex = 1.25, lwd = 0.8)
    has_test <- length(x_test) > 0L && length(x_test) == length(y_test)
    if (has_test)
      graphics::points(x_test, y_test, pch = 21, bg = grDevices::adjustcolor("#E0457B", 0.8),
                       col = "#a3264f", cex = 1.25, lwd = 0.8)
    graphics::legend("topleft", bty = "n", cex = 0.9, pt.cex = 1.1,
                     legend = c(if (has_test) c("Training", "Test") else "Programs",
                                if (ref == "identity") "Actual = Predicted" else "Zero"),
                     pch = c(21, if (has_test) 21, NA), lty = c(NA, if (has_test) NA, 2),
                     pt.bg = c(grDevices::adjustcolor("#2F6FE4", 0.75),
                               if (has_test) grDevices::adjustcolor("#E0457B", 0.8), NA),
                     col = c("#1d4fb8", if (has_test) "#a3264f", "#334155"),
                     text.col = "#334155")
    TRUE
  }, error = function(e) FALSE, finally = grDevices::dev.off())
  if (!isTRUE(ok) || !file.exists(f)) return(NA_character_)
  paste0("data:image/png;base64,",
         gsub("[\r\n]", "", jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))))
}

excel_sheet_is_table <- function(path, sheet) {
  d <- tryCatch(suppressMessages(readxl::read_excel(path, sheet = sheet, n_max = 50,
                                                    na = c("", "NA", "N/A", "n/a", "na", "#N/A", "-"))),
                error = function(e) NULL)
  if (is.null(d) || ncol(d) < 2L || nrow(d) < 1L) return(FALSE)
  named <- nzchar(names(d)) & !grepl("^\\.\\.\\.[0-9]+$", names(d))
  if (sum(named) < 2L || sum(named) < ncol(d) / 2) return(FALSE)
  any(rowSums(!is.na(d)) >= 2L)
}
excel_default_sheet <- function(path, sheets = readxl::excel_sheets(path)) {
  for (s in sheets) if (excel_sheet_is_table(path, s)) return(s)
  sheets[1L]
}

CERES_HEALTH_CHECKS <- c(missing = "Missing values", constant = "Columns that vary",
                         text_num = "Numbers stored as text", duplicates = "Duplicate elements",
                         skew = "Skewed columns", typos = "Spelling of categories",
                         rare = "Small categories", rows = "Number of elements")

ceres_health_is_plain_num <- function(x)
  grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", x)

ceres_health_parse_num <- function(x) {
  y <- gsub("[[:space:] $,%£€]", "", x)
  y <- sub("^\\((.*)\\)$", "-\\1", y)
  out <- suppressWarnings(as.numeric(y))
  out[!is.finite(out)] <- NA_real_
  out
}

ceres_health_fmt_pct <- function(p) {
  if (is.na(p)) return("")
  if (p > 0 && p < 0.01) return("<1%")
  if (p < 1 && p > 0.99) return(">99%")
  paste0(round(100 * p), "%")
}

ceres_health_quote <- function(x, max = 4L) {
  x <- unique(x)
  more <- length(x) - max
  q <- paste0("“", utils::head(x, max), "”")
  paste0(paste(q, collapse = ", "), if (more > 0L) paste0(" and ", more, " more") else "")
}

ceres_data_health <- function(df, skip_skew = character(0)) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  n  <- nrow(df); p <- ncol(df); cols <- names(df)
  is_num  <- vapply(df, is.numeric, logical(1L))
  is_flag <- is_num & vapply(df, function(x) all(x %in% c(0, 1, NA)), logical(1L))
  is_txt  <- vapply(df, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1L))
  na_n    <- vapply(df, function(x) sum(is.na(x)), numeric(1L))
  summary <- list(rows = n, cols = p,
                  numeric = sum(is_num & !is_flag), categorical = p - sum(is_num & !is_flag),
                  completeness = if (n * p > 0) 1 - sum(na_n) / (n * p) else NA_real_)
  findings <- list()
  add <- function(f) findings[[length(findings) + 1L]] <<- f
  chip <- function(label, note = NULL) list(label = label, note = note)
  txt_vals <- lapply(df, function(x) if (is.factor(x) || is.character(x)) as.character(x[!is.na(x)]) else NULL)
  if (n == 0L || p == 0L)
    return(list(summary = summary, findings = findings, passed = character(0)))

  top_share <- vapply(df, function(x) {
    v <- x[!is.na(x)]
    if (!length(v)) return(NA_real_)
    max(tabulate(match(v, unique(v)))) / length(v)
  }, numeric(1L))
  n_ok   <- n - na_n
  empty  <- cols[n_ok == 0]
  const  <- cols[n_ok >= 2 & !is.na(top_share) & top_share == 1]
  near   <- cols[n_ok >= 10 & !is.na(top_share) & top_share >= 0.95 & top_share < 1]
  flat   <- c(empty, const, near)
  if (length(flat)) {
    top_val <- function(cn) { v <- df[[cn]][!is.na(df[[cn]])]; u <- unique(v)
                              as.character(u[which.max(tabulate(match(v, u)))]) }
    add(list(id = "constant", status = if (length(empty) || length(const)) "warn" else "info",
      title = if (length(flat) == 1L) "1 column barely varies" else paste(length(flat), "columns barely vary"),
      detail = "A column with the same value in (nearly) every element can't explain differences in cost, so it can't help a CER.",
      chips = c(lapply(empty, function(cn) chip(cn, "empty")),
                lapply(const, function(cn) chip(cn, paste0("always ", ceres_health_quote(top_val(cn), 1L)))),
                lapply(near,  function(cn) chip(cn, paste0(ceres_health_fmt_pct(top_share[[cn]]), " ",
                                                             ceres_health_quote(top_val(cn), 1L))))),
      action = list(text = "Leave them out of your predictor lists.")))
  }

  miss <- cols[na_n > 0 & n_ok > 0]
  if (length(miss)) {
    miss <- miss[order(-na_n[miss])]
    share <- na_n[miss] / n
    add(list(id = "missing", status = if (any(share >= 0.25)) "warn" else "info",
      title = if (length(miss) == 1L) "1 column has missing values" else paste(length(miss), "columns have missing values"),
      detail = "A CER fit drops every element that is missing a value in any column it uses. Fill the gaps or see which elements they affect.",
      chips = lapply(miss, function(cn) chip(cn, paste0(na_n[[cn]], " · ", ceres_health_fmt_pct(na_n[[cn]] / n)))),
      action = list(goto = "missing", label = "Open Missing Data")))
  }

  typo_levels <- list()
  typo_chips  <- list()
  for (cn in cols[is_txt]) {
    v <- txt_vals[[cn]]
    if (length(v) < 2L) next
    u <- unique(v)
    key <- tolower(gsub("[[:space:] ]+", " ", trimws(u)))
    grp <- split(u, key)
    grp <- grp[lengths(grp) > 1L]
    notes <- vapply(grp, function(g) paste(paste0("“", g, "”"), collapse = " / "), character(1L))
    lv <- unlist(grp, use.names = FALSE)
    single <- u[nchar(trimws(u)) == 1L & !grepl("^[0-9]$", trimws(u))]
    if (length(single)) {
      plain_share <- mean(ceres_health_is_plain_num(trimws(v)))
      others <- setdiff(u, single)
      cnt <- table(v)[single]
      looks <- c(O = "0", o = "0", I = "1", l = "1", i = "1")
      for (s in single) {
        s0 <- trimws(s)
        if (plain_share >= 0.6) {
          notes <- c(notes, paste0("“", s, "” among numbers",
                                   if (!is.na(looks[s0])) paste0(" (the digit ", looks[[s0]], "?)") else ""))
          lv <- c(lv, s)
        } else if (length(others) && all(nchar(trimws(others)) >= 3L) && cnt[[s]] <= 2L) {
          notes <- c(notes, paste0("lone code “", s, "” (", cnt[[s]], if (cnt[[s]] == 1) " row)" else " rows)"))
          lv <- c(lv, s)
        }
      }
    }
    if (length(notes)) {
      typo_levels[[cn]] <- lv
      typo_chips[[length(typo_chips) + 1L]] <- chip(cn, paste(notes, collapse = "; "))
    }
  }
  if (length(typo_chips))
    add(list(id = "typos", status = "warn",
      title = if (length(typo_chips) == 1L) "1 column has values that look like typos" else paste(length(typo_chips), "columns have values that look like typos"),
      detail = "These look like one value typed two ways (or a letter typed for a digit), so the app counts them as separate categories.",
      chips = typo_chips,
      action = list(text = "Correct them in the source file and reload, so each value is spelled one way.")))

  tn_chips <- list()
  for (cn in setdiff(cols[is_txt], names(typo_levels))) {
    v <- trimws(txt_vals[[cn]])
    if (length(v) < 3L) next
    num <- ceres_health_parse_num(v)
    ok  <- !is.na(num)
    if (mean(ok) < 0.8) next
    if (all(num[ok] %in% c(0, 1))) next
    blockers <- txt_vals[[cn]][!ceres_health_is_plain_num(txt_vals[[cn]])]
    tn_chips[[length(tn_chips) + 1L]] <- chip(cn,
      if (length(blockers)) paste0("held back by ", ceres_health_quote(blockers))
      else "every value is a number, but the column is stored as text")
  }
  if (length(tn_chips))
    add(list(id = "text_num", status = "warn",
      title = if (length(tn_chips) == 1L) "1 column holds numbers stored as text" else paste(length(tn_chips), "columns hold numbers stored as text"),
      detail = "Almost every value is a number, but a few cells (commas, currency signs, spaces or words) make the app treat the column as categories.",
      chips = tn_chips,
      action = list(text = "Fix those cells in the source file so the column is all numbers, then reload.")))

  dup_chips <- list()
  rn <- row.names(df)
  if (n >= 2L) {
    d1 <- duplicated(df)
    if (any(d1)) {
      key <- do.call(paste, c(lapply(df, function(x) as.character(x)), sep = "\x1f"))
      grp <- split(rn, key)
      grp <- grp[lengths(grp) > 1L]
      for (g in utils::head(grp, 6L))
        dup_chips[[length(dup_chips) + 1L]] <- chip(paste(g, collapse = " = "), "identical rows")
    }
    default_rn <- identical(rn, as.character(seq_len(n)))
    if (!default_rn) {
      k <- tolower(gsub("[[:space:] ]+", " ", trimws(rn)))
      grp <- split(rn, k); grp <- grp[lengths(grp) > 1L]
      for (g in utils::head(grp, 6L))
        dup_chips[[length(dup_chips) + 1L]] <- chip(paste(paste0("“", g, "”"), collapse = " / "), "same name")
    } else if (is_txt[[1L]]) {
      v <- txt_vals[[1L]]
      if (length(v) >= 3L && length(unique(v)) >= 0.8 * length(v) && anyDuplicated(v)) {
        reps <- unique(v[duplicated(v)])
        dup_chips[[length(dup_chips) + 1L]] <- chip(cols[1L],
          paste0("repeats ", ceres_health_quote(reps, 3L), ", so it could not be used as element names"))
      }
    }
  }
  if (length(dup_chips))
    add(list(id = "duplicates", status = "warn",
      title = "Some elements may be listed twice",
      detail = "The same element entered twice counts twice in a CER fit.",
      chips = dup_chips,
      action = list(text = "Check these in the source file and keep one row per element.")))

  skew_rule <- paste("Flagged when every value is above zero, the largest is at least 10 times the smallest,",
                     "and the median sits low: the gap from the median up to the third quartile is at least",
                     "1.5 times the gap down to the first quartile. Quartiles ignore single extreme points,",
                     "so one extreme value alone does not trigger it.")
  sk_chips <- list(); sk_cols <- character(0)
  for (cn in cols[is_num & !is_flag]) {
    if (cn %in% skip_skew || grepl("_ln$", cn)) next
    x <- df[[cn]][!is.na(df[[cn]])]
    if (length(x) < 8L || any(x <= 0)) next
    rng <- max(x) / min(x)
    if (rng < 10) next
    q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE, type = 7)
    up <- q[3] - q[2]; lo <- q[2] - q[1]
    if (up > 0 && up >= 1.5 * lo) {
      sk_cols <- c(sk_cols, cn)
      sk_chips[[length(sk_chips) + 1L]] <- chip(cn, paste0("spans ", format(signif(rng, 2), big.mark = ",", scientific = FALSE), "×"))
    }
  }
  if (length(sk_chips))
    add(list(id = "skew", status = "info",
      title = if (length(sk_chips) == 1L) "1 column is strongly skewed" else paste(length(sk_chips), "columns are strongly skewed"),
      detail = "Most values are small with a long tail of large ones, common for cost and size. Log space often suits them: a Log Linear CER, or ln in Transforms.",
      tip = skew_rule, chips = sk_chips, columns = sk_cols,
      action = list(goto = "transforms", label = "Log them in Transforms")))

  rare_chips <- list()
  if (n >= 10L) for (cn in setdiff(cols[is_txt | is_flag], flat)) {
    v <- df[[cn]][!is.na(df[[cn]])]
    v <- as.character(v)
    tab <- table(v)
    if (length(tab) < 2L || length(tab) > max(10L, length(v) / 3)) next
    small <- tab[tab <= 2L & !(names(tab) %in% typo_levels[[cn]])]
    if (!length(small)) next
    rare_chips[[length(rare_chips) + 1L]] <- chip(cn, paste(
      paste0("“", names(small), "” ", as.integer(small), ifelse(small == 1L, " row", " rows")), collapse = ", "))
  }
  if (length(rare_chips))
    add(list(id = "rare", status = "info",
      title = if (length(rare_chips) == 1L) "1 column has a category with very few elements" else paste(length(rare_chips), "columns have categories with very few elements"),
      detail = "A category held by only one or two elements gets its own coefficient from almost no data, which can make a CER unstable.",
      chips = rare_chips,
      action = list(text = "Consider merging it with a similar category, or leave the column out.")))

  if (n < 30L) {
    k <- max(1L, floor(n / 10))
    add(list(id = "rows", status = if (n < 10L) "warn" else "info",
      title = paste0("Only ", n, if (n == 1L) " element" else " elements"),
      detail = paste0("A common rule of thumb is about 10 elements per predictor, which suggests ",
                      if (n < 10L) "a single predictor at most" else paste0("up to ", k, if (k == 1L) " predictor" else " predictors"),
                      " here. It is guidance, not a hard limit."),
      chips = list(),
      action = list(text = "Keep CERs small and check that each coefficient makes sense.")))
  }

  ord <- order(match(vapply(findings, `[[`, "", "status"), c("warn", "info")))
  findings <- findings[ord]
  hit <- vapply(findings, `[[`, "", "id")
  list(summary = summary, findings = findings,
       passed = unname(CERES_HEALTH_CHECKS[setdiff(names(CERES_HEALTH_CHECKS), hit)]))
}

ceres_repaired_name <- function(name) {
  ext  <- tools::file_ext(name)
  stem <- if (nzchar(ext)) substr(name, 1L, nchar(name) - nchar(ext) - 1L) else name
  paste0(sub("_+$", "", stem), "_repaired", if (nzchar(ext)) paste0(".", ext) else "")
}

ceres_auto_repair <- function(df, response) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!(response %in% names(df))) stop("'", response, "' is not a column of this dataset.")
  was_factor <- names(df)[vapply(df, is.factor, TRUE)]
  for (cn in was_factor) df[[cn]] <- as.character(df[[cn]])
  log <- list(); changed <- FALSE
  say <- function(step, text, change = TRUE) { log[[length(log) + 1L]] <<- list(step = step, text = text); if (change) changed <<- TRUE }
  imputed <- list()
  some <- function(x, k = 4L) {
    x <- as.character(x)
    paste0(paste(utils::head(x, k), collapse = ", "), if (length(x) > k) paste0(" and ", length(x) - k, " more") else "")
  }
  pct <- function(p) paste0(round(100 * p), "%")
  is_txt  <- function(x) is.character(x)
  is_flag <- function(x) is.numeric(x) && all(x %in% c(0, 1, NA))

  for (cn in names(df)[vapply(df, is_txt, TRUE)]) {
    v <- df[[cn]]; has <- !is.na(v)
    if (sum(has) < 3L) next
    num <- ceres_health_parse_num(trimws(v[has]))
    ok <- !is.na(num)
    if (mean(ok) < 0.8 || all(num[ok] %in% c(0, 1))) next
    fixed <- sum(!ceres_health_is_plain_num(trimws(v[has])) & ok)
    lost  <- sum(!ok)
    out <- rep(NA_real_, length(v)); out[has] <- num
    df[[cn]] <- out
    plain <- sub("_+$", "", cn); renamed <- ""
    if (plain != cn && nzchar(plain) && !(plain %in% names(df))) {
      names(df)[names(df) == cn] <- plain; was_factor <- setdiff(was_factor, cn)
      if (identical(response, cn)) response <- plain
      renamed <- paste0(" Renamed ", cn, " to ", plain, " (a number column now).")
    }
    say("Numbers stored as text", paste0(cn, ": numbers stored as text read as numbers", if (fixed) paste0(" (", fixed, if (fixed == 1) " cell" else " cells", " cleaned, e.g. ",
        some(paste0("“", v[has][!ceres_health_is_plain_num(trimws(v[has])) & ok], "”"), 2L), ")"),
        if (lost) paste0("; ", lost, if (lost == 1) " cell was not a number and is now missing" else " cells were not numbers and are now missing"), ".", renamed))
  }

  for (cn in names(df)[vapply(df, is_txt, TRUE)]) {
    v <- df[[cn]]; u <- unique(v[!is.na(v)])
    if (length(u) < 2L) next
    key <- tolower(gsub("[[:space:] ]+", " ", trimws(u)))
    for (g in split(u, key)) {
      if (length(g) < 2L) next
      cnt <- vapply(g, function(s) sum(v == s, na.rm = TRUE), 0)
      keep <- g[which.max(cnt)]
      hit <- !is.na(v) & v %in% setdiff(g, keep)
      v[hit] <- keep
      say("Category spellings", paste0(cn, ": ", some(paste0("“", setdiff(g, keep), "”")), " → “", keep, "” (",
                                       sum(hit), if (sum(hit) == 1) " cell)." else " cells)."))
    }
    df[[cn]] <- v
  }

  dup <- duplicated(df)
  if (any(dup)) {
    say("Duplicate elements", paste0("Dropped ", sum(dup), if (sum(dup) == 1) " exact copy: " else " exact copies: ", some(row.names(df)[dup]), "."))
    df <- df[!dup, , drop = FALSE]
  }
  rn <- row.names(df)
  if (!identical(rn, as.character(seq_len(nrow(df))))) {
    key <- tolower(gsub("[[:space:] ]+", " ", trimws(rn)))
    na_row <- rowSums(is.na(df))
    drop <- character(0)
    for (g in split(seq_along(rn), key)) {
      if (length(g) < 2L) next
      keep <- g[order(na_row[g], g)][1L]
      drop <- c(drop, rn[setdiff(g, keep)])
      say("Duplicate elements", paste0("Kept “", rn[keep], "” (most complete) and dropped ", some(paste0("“", rn[setdiff(g, keep)], "”")), "."))
    }
    if (length(drop)) df <- df[!(row.names(df) %in% drop), , drop = FALSE]
  }

  for (cn in setdiff(names(df), response)) {
    v <- df[[cn]][!is.na(df[[cn]])]
    if (!length(v)) { df[[cn]] <- NULL; say("Columns that do not vary", paste0("Dropped ", cn, ": empty.")); next }
    if (length(v) >= 2L && length(unique(v)) == 1L) {
      df[[cn]] <- NULL
      say("Columns that do not vary", paste0("Dropped ", cn, ": always “", v[1], "”, so it cannot explain cost."))
    }
  }

  miss_y <- is.na(df[[response]])
  if (any(miss_y)) {
    say("Missing values", paste0("Dropped ", sum(miss_y), if (sum(miss_y) == 1) " element" else " elements", " missing ", response,
                                 " (a response is never filled in): ", some(row.names(df)[miss_y]), "."))
    df <- df[!miss_y, , drop = FALSE]
  }
  if (ncol(df) > 1L) {
    half <- rowMeans(is.na(df)) > 0.5
    if (any(half)) {
      say("Missing values", paste0("Dropped ", sum(half), if (sum(half) == 1) " element" else " elements",
                                   " missing more than half their values: ", some(row.names(df)[half]), "."))
      df <- df[!half, , drop = FALSE]
    }
  }
  y <- df[[response]]
  link <- function(x) {
    if (!is.numeric(x) || !is.numeric(y)) return(NA_real_)
    ok <- !is.na(x) & !is.na(y)
    if (sum(ok) < 4L || stats::sd(x[ok]) == 0) return(NA_real_)
    abs(suppressWarnings(stats::cor(x[ok], y[ok], method = "spearman")))
  }
  fill <- vapply(df, function(x) mean(!is.na(x)), 0)
  cols <- setdiff(names(df)[fill < 1], response)
  regress <- character(0)
  for (cn in cols) {
    f <- fill[[cn]]
    if (f < 0.5) {
      df[[cn]] <- NULL; say("Missing values", paste0("Dropped ", cn, ": only ", pct(f), " filled (below 50%)."))
    } else if (f < 0.7) {
      lk <- link(df[[cn]])
      if (!is.na(lk) && lk >= 0.5) {
        regress <- c(regress, cn)
        say("Missing values", paste0("Kept ", cn, " although only ", pct(f), " filled: it is strongly linked to ", response,
                                     " (Spearman ", formatC(lk, format = "f", digits = 2), "). Its gaps are filled below; check them."))
      } else {
        df[[cn]] <- NULL; say("Missing values", paste0("Dropped ", cn, ": ", pct(f), " filled (50–70%) and not strongly linked to ", response, "."))
      }
    } else if (f < 0.9) regress <- c(regress, cn)
  }
  fill_simple <- function(cn, why) {
    x <- df[[cn]]; miss <- is.na(x)
    val <- if (is.numeric(x) && !is_flag(x)) stats::median(x, na.rm = TRUE) else {
      tb <- table(x); names(tb)[which.max(tb)] }
    if (is.numeric(x)) val <- as.numeric(val)
    x[miss] <- val; df[[cn]] <<- x
    imputed[[cn]] <<- row.names(df)[miss]
    say("Missing values", paste0(cn, ": ", sum(miss), if (sum(miss) == 1) " gap" else " gaps", " filled with the ",
                                 if (is.numeric(x) && !is_flag(x)) paste0("median (", format(signif(val, 4)), ")") else paste0("most common value (“", val, "”)"),
                                 why, ": ", some(row.names(df)[miss]), "."))
  }
  for (cn in setdiff(names(df)[vapply(df, anyNA, TRUE)], c(response, regress))) fill_simple(cn, "")
  for (cn in regress) {
    x <- df[[cn]]
    if (!is.numeric(x) || is_flag(x)) { fill_simple(cn, " (a category: regression does not apply)"); next }
    pred <- setdiff(names(df)[vapply(df, function(z) is.numeric(z) && !anyNA(z) && !is_flag(z), TRUE)], c(response, cn, regress))
    if (!length(pred)) { fill_simple(cn, " (no complete numeric column to predict it from)"); next }
    miss <- is.na(x)
    X <- cbind(1, as.matrix(df[, pred, drop = FALSE]))
    b <- stats::lm.fit(X[!miss, , drop = FALSE], x[!miss])$coefficients
    b[is.na(b)] <- 0
    x[miss] <- as.vector(X[miss, , drop = FALSE] %*% b)
    df[[cn]] <- x
    imputed[[cn]] <- row.names(df)[miss]
    say("Missing values", paste0(cn, ": ", sum(miss), if (sum(miss) == 1) " gap" else " gaps", " filled by regression on ",
                                 some(pred), ": ", some(row.names(df)[miss]), "."))
  }

  num <- setdiff(names(df)[vapply(df, function(z) is.numeric(z) && !is_flag(z) && !anyNA(z) && stats::sd(z) > 0, TRUE)], response)
  for (cn in num) {
    if (!(cn %in% names(df))) next
    others <- setdiff(intersect(num, names(df)), cn)
    if (length(others) < 2L) next
    pr <- utils::combn(others, 2L, simplify = FALSE)
    for (p2 in pr) {
      prod <- df[[p2[1]]] * df[[p2[2]]]
      if (isTRUE(all.equal(df[[cn]], prod, tolerance = 1e-6, check.attributes = FALSE))) {
        df[[cn]] <- NULL
        say("Near-copies", paste0("Dropped ", cn, ": it is ", p2[1], " × ", p2[2], ", so it repeats information already there."))
        break
      }
    }
  }
  repeat {
    num <- intersect(num, names(df))
    if (length(num) < 2L) break
    r <- suppressWarnings(abs(stats::cor(df[, num, drop = FALSE])))
    diag(r) <- 0; r[is.na(r)] <- 0
    if (max(r) < 0.95) break
    ij <- which(r == max(r), arr.ind = TRUE)[1, ]
    a <- num[ij[1]]; b <- num[ij[2]]
    la <- link(df[[a]]); lb <- link(df[[b]]); la[is.na(la)] <- 0; lb[is.na(lb)] <- 0
    drop <- if (lb > la) a else b; keep <- setdiff(c(a, b), drop)
    df[[drop]] <- NULL
    say("Near-copies", paste0("Dropped ", drop, ": a near-copy of ", keep, " (r = ", formatC(max(r), format = "f", digits = 2),
                              "); kept ", keep, ", more closely linked to ", response, "."))
  }

  if (!changed) say("Result", "Nothing needed repair: the dataset is unchanged.", change = FALSE)
  h <- tryCatch(ceres_data_health(df), error = function(e) NULL)
  if (!is.null(h)) for (f in h$findings) if (f$id %in% c("skew", "rare", "rows")) {
    what <- if (length(f$chips)) paste0(": ", some(vapply(f$chips, function(ch) paste0(ch$label, if (!is.null(ch$note)) paste0(" (", ch$note, ")")), ""), 3L)) else ""
    say("Left as they are", paste0(f$title, what, "."), change = FALSE)
  }

  for (cn in intersect(was_factor, names(df))) if (is.character(df[[cn]])) df[[cn]] <- factor(df[[cn]])
  list(data = df, log = log, imputed = imputed, response = response)
}

validate_nl_data <- function(data, x_vars, y_var, eq_name, n_params = 1) {
  n <- nrow(data)

  if (n <= n_params) {
    return(paste0("Not enough data rows to fit '", eq_name, "': need more than ",
                  n_params, " observations, but only ", n, " are available after removing missing values."))
  }

  needs_positive_x <- c("Power Curve", "Michaelis-Menten",
                         "Weibull (Type 1)", "Weibull (Type 2)")
  if (eq_name %in% needs_positive_x) {
    for (xv in x_vars) {
      if (any(data[[xv]] <= 0, na.rm = TRUE)) {
        return(paste0("'", eq_name, "' requires all values of '", xv,
                      "' to be strictly positive (> 0). ",
                      sum(data[[xv]] <= 0, na.rm = TRUE),
                      " non-positive value(s) found. Consider filtering or transforming the data."))
      }
    }
  }

  if (eq_name == "Power Curve" && any(data[[y_var]] <= 0, na.rm = TRUE)) {
    return(paste0("'Power Curve' requires all response values ('", y_var,
                  "') to be strictly positive (> 0)."))
  }

  if (eq_name == "Logistic (S-Curve)" && stats::var(data[[y_var]], na.rm = TRUE) < 1e-10) {
    return(paste0("Response variable '", y_var,
                  "' has near-zero variance. The Logistic S-Curve cannot be identified."))
  }

  NULL
}

fit_nlls_multistart <- function(formula, data, start_list, fit_fn = fit_nlls) {
  resp_name <- trimws(strsplit(formula, "~")[[1]][1])
  errors    <- character(length(start_list))
  best_fit  <- NULL; best_sse  <- Inf
  best_fb   <- NULL; best_fb_sse <- Inf

  for (i in seq_along(start_list)) {
    hit_maxiter <- FALSE
    result <- tryCatch(
      withCallingHandlers(
        fit_fn(formula, data, start_list[[i]]),
        warning = function(w) {
          if (grepl("maxiter|Number of iterations", conditionMessage(w), ignore.case = TRUE)) {
            hit_maxiter <<- TRUE
            invokeRestart("muffleWarning")
          }
        }
      ),
      error = function(e) list(.__err__ = e$message)
    )
    if (is.list(result) && !is.null(result$.__err__)) {
      errors[i] <- result$.__err__
      fit <- NULL
    } else {
      fit <- result
    }
    if (!is.null(fit)) {
      yhat <- tryCatch(predict(fit, data), error = function(e) NULL)
      if (!is.null(yhat) && !any(is.na(yhat)) && !any(is.infinite(yhat))) {
        sse <- sum((data[[resp_name]] - yhat)^2, na.rm = TRUE)
        if (is.finite(sse)) {
          if (hit_maxiter) {
            if (sse < best_fb_sse) { best_fb_sse <- sse; best_fb <- fit }
          } else if (sse < best_sse) {
            best_sse <- sse; best_fit <- fit
          }
        }
      }
    }
  }

  if (!is.null(best_fit)) return(best_fit)
  if (!is.null(best_fb))  return(best_fb)
  stop(paste0("All ", length(start_list), " starting value sets failed to converge. ",
              "Last error: ", tail(errors, 1)))
}

nl_start_guesses <- function(df, eq_name, x_vars, y_var) {
  reg <- nl_registry[[eq_name]]
  if(nrow(df) == 0) return(list())
  
  y <- df[[y_var]]
  x_val <- if(length(x_vars) == 1) df[[x_vars[1]]] else NULL
  
  safe_max_x <- if(!is.null(x_val)) max(x_val, na.rm = TRUE) else 1.0
  if (safe_max_x == 0) safe_max_x <- 1.0
  safe_max_y <- max(y, na.rm = TRUE)
  safe_min_y <- min(y, na.rm = TRUE)
  
  san_x <- vapply(x_vars, sanitize_nl_colname, character(1))
  san_y <- sanitize_nl_colname(y_var)
  if (anyDuplicated(c(san_y, san_x))) return(list())

  guesses <- list()

  if (eq_name == "Power Curve") {
    df_log <- df
    df_log[[y_var]] <- suppressWarnings(log(y + 1e-6))
    for (x in x_vars) df_log[[x]] <- suppressWarnings(log(df[[x]] + 1e-6))
    df_log_cols <- names(df_log)
    names(df_log)[df_log_cols %in% c(y_var, x_vars)] <- vapply(
      df_log_cols[df_log_cols %in% c(y_var, x_vars)], sanitize_nl_colname, character(1))
    formula_str <- paste0(san_y, " ~ ", paste0(san_x, collapse = " + "))
    fit <- try(lm(as.formula(formula_str), data = df_log), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      guesses[["a"]] <- exp(coef(fit)[1])
      for (i in seq_along(x_vars)) guesses[[paste0(reg$param_prefix, san_x[i])]] <- coef(fit)[i + 1]
    }
  } else if (eq_name == "Exponential") {
    df_log <- df
    df_log[[y_var]] <- suppressWarnings(log(y + 1e-6))
    df_log_cols <- names(df_log)
    names(df_log)[df_log_cols %in% c(y_var, x_vars)] <- vapply(
      df_log_cols[df_log_cols %in% c(y_var, x_vars)], sanitize_nl_colname, character(1))
    formula_str <- paste0(san_y, " ~ ", paste0(san_x, collapse = " + "))
    fit <- try(lm(as.formula(formula_str), data = df_log), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      guesses[["a"]] <- exp(coef(fit)[1])
      for (i in seq_along(x_vars)) guesses[[paste0(reg$param_prefix, san_x[i])]] <- coef(fit)[i + 1]
    }
  } else if (eq_name == "Logistic (S-Curve)") {
    a_guess <- safe_max_y * 1.05
    guesses[["a"]] <- a_guess
    transformed_y <- log(a_guess / (y + 1e-6) - 1)
    df_trans <- df
    df_trans[[y_var]] <- transformed_y
    df_trans <- df_trans[is.finite(df_trans[[y_var]]), , drop = FALSE]
    if (nrow(df_trans) >= 2) {
      df_trans_cols <- names(df_trans)
      names(df_trans)[df_trans_cols %in% c(y_var, x_vars)] <- vapply(
        df_trans_cols[df_trans_cols %in% c(y_var, x_vars)], sanitize_nl_colname, character(1))
      formula_str <- paste0(san_y, " ~ ", paste0(san_x, collapse = " + "))
      fit <- try(lm(as.formula(formula_str), data = df_trans), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        guesses[["c"]] <- -coef(fit)[1]
        for (i in seq_along(x_vars)) guesses[[paste0(reg$param_prefix, san_x[i])]] <- -coef(fit)[i + 1]
      }
    }
  } else if (eq_name == "Gompertz") {
    fit <- try(nls(y ~ SSgompertz(x_val, Asym, b2, b3)), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      guesses[["a"]] <- coef(fit)["Asym"]
      guesses[["b"]] <- coef(fit)["b2"]
      
      b3_val <- coef(fit)["b3"]
      guesses[["c"]] <- if (b3_val > 0) -log(b3_val) else 1 / safe_max_x
    } else {
      guesses[["a"]] <- safe_max_y; guesses[["b"]] <- 1.0; guesses[["c"]] <- 1 / safe_max_x
    }
  } else if (eq_name == "Michaelis-Menten") {
    fit <- try(nls(y ~ SSmicmen(x_val, Vm, K)), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      guesses[["a"]] <- coef(fit)["Vm"]; guesses[["b"]] <- coef(fit)["K"]
    } else {
      guesses[["a"]] <- safe_max_y; guesses[["b"]] <- median(x_val, na.rm = TRUE)
    }
  } else if (eq_name == "Weibull (Type 1)") {
    fit <- try(nls(y ~ SSweibull(x_val, Asym, Drop, lrc, pwr)), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      guesses[["a"]] <- coef(fit)["Asym"]; guesses[["b"]] <- coef(fit)["Drop"] 
      guesses[["c"]] <- exp(coef(fit)["lrc"]); guesses[["d"]] <- coef(fit)["pwr"]
    } else {
      guesses[["a"]] <- safe_max_y; guesses[["b"]] <- safe_max_y - safe_min_y
      guesses[["c"]] <- 1 / safe_max_x; guesses[["d"]] <- 1.0
    }
  } else if (eq_name == "Weibull (Type 2)") {
    fit <- try(nls(y ~ SSweibull(x_val, Asym, Drop, lrc, pwr)), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      guesses[["a"]] <- coef(fit)["Asym"] 
      guesses[["b"]] <- exp(coef(fit)["lrc"]); guesses[["c"]] <- coef(fit)["pwr"]
    } else {
      guesses[["a"]] <- safe_max_y; guesses[["b"]] <- 1 / safe_max_x; guesses[["c"]] <- 1.0
    }
  }
  
  expected_params <- reg$params_base
  if (reg$type == "multivariate") expected_params <- c(expected_params, paste0(reg$param_prefix, san_x))
  for (p in expected_params) {
    if (is.null(guesses[[p]]) || is.na(guesses[[p]]) || is.infinite(guesses[[p]])) guesses[[p]] <- 1.0
  }
  return(guesses)
}

make_nl_start_list <- function(param_names, seed_vals = NULL) {
  scales <- c(0.01, 0.1, 1, 10, 100, 1000, -1)
  n      <- length(param_names)
  if (n <= 3) {
    grid <- do.call(expand.grid, replicate(n, scales, simplify = FALSE))
    colnames(grid) <- param_names
    start_list <- lapply(seq_len(nrow(grid)),
                         function(i) as.list(setNames(as.numeric(grid[i, ]), param_names)))
  } else {
    start_list <- lapply(scales, function(s) setNames(as.list(rep(s, n)), param_names))
  }
  if (!is.null(seed_vals) && length(seed_vals) > 0) {
    start_list <- c(list(as.list(seed_vals)), start_list)
  }
  start_list
}

get_derivative <- function(formula, data, coeff_value){
  rhs_formula <- trimws(strsplit(formula, "[~]")[[1]][2])
  if(is.na(rhs_formula)){
    rhs_formula <- formula
  }
  rhs_expr <- parse(text = rhs_formula)[[1]]
  rhs_vars <- all.vars(rhs_expr)
  data_vars <- rhs_vars[rhs_vars %in% colnames(data)]
  coeff_vars <- setdiff(rhs_vars, data_vars)
  n_coeff <- length(coeff_vars)

  if(n_coeff == 0){
    n_row <- nrow(data)
    return(matrix(numeric(0), nrow = n_row, ncol = 0))
  }

  formals_list <- setNames(lapply(coeff_vars, function(v) quote(expr=)), coeff_vars)
  full_f <- as.function(c(formals_list, list(rhs_expr)))
  deriv_f <- ceres_ns("Deriv")$Deriv(full_f, combine = "cbind")

  for (v in data_vars) {
    assign(v, data[[v]], envir = environment(deriv_f))
  }

  cv_list <- as.list(coeff_value)
  if (!is.null(names(coeff_value)) && all(coeff_vars %in% names(coeff_value))) {
    args <- cv_list[coeff_vars]
  } else {
    args <- setNames(cv_list, coeff_vars)
  }
  jcob <- as.matrix(do.call(deriv_f, args))
  colnames(jcob) <- coeff_vars
  if (!is.null(names(coeff_value)) && all(coeff_vars %in% names(coeff_value))) {
    ord <- intersect(names(coeff_value), coeff_vars)
    jcob <- jcob[, ord, drop = FALSE]
  }
  return(jcob)
}

fit_nlls <- function(formula, data, initial_coeff) {
  f <- as.formula(formula)
  s <- as.list(initial_coeff)

  fit <- tryCatch(
    ceres_ns("minpack.lm")$nlsLM(f, data = data, start = s,
                      control = ceres_ns("minpack.lm")$nls.lm.control(maxiter = 500)),
    error = function(e) NULL
  )

  if (!is.null(fit)) return(fit)

  fit <- nls(f, data = data, start = s,
             control = nls.control(maxiter = 500, warnOnly = FALSE))
  return(fit)
}

fit_mupe <- function(formula, data, initial_coeff) {
  fit <- fit_nlls(formula, data, initial_coeff)
  cntr <- 0
  yhat_delta <- 100
  while (yhat_delta > 1e-12 & cntr < 100) {
    yhat_prior <- predict(fit, data)
    w <- 1 / pmax(abs(yhat_prior), 1e-10)^2
    refit_w <- tryCatch(
      ceres_ns("minpack.lm")$nlsLM(as.formula(formula), data = data, start = as.list(coef(fit)),
                        weights = w,
                        control = ceres_ns("minpack.lm")$nls.lm.control(maxiter = 500)),
      error = function(e) NULL
    )
    if (!is.null(refit_w)) fit <- refit_w else break
    yhat_new   <- predict(fit, data)
    yhat_delta <- sum((yhat_new - yhat_prior)^2) / max(sum(yhat_prior^2), 1e-10)
    cntr <- cntr + 1
  }
  return(fit)
}

nl_compile <- function(formula, data) {
  parts  <- strsplit(formula, "~", fixed = TRUE)[[1]]
  rhs    <- str2lang(trimws(paste(parts[-1], collapse = "~")))
  params <- setdiff(all.vars(rhs), names(data))
  params <- params[!vapply(params, exists, TRUE, envir = baseenv(), mode = "numeric", inherits = FALSE)]
  dex    <- tryCatch(deriv(rhs, params), error = function(e) NULL)
  env    <- list2env(as.list(data), parent = baseenv())
  y      <- data[[trimws(parts[1])]]
  n      <- length(y)
  value  <- function(p) {
    for (k in params) assign(k, p[[k]], envir = env)
    v <- as.numeric(eval(rhs, env))
    if (length(v) == 1L) v <- rep(v, n)
    v
  }
  model  <- function(p) {
    if (is.null(dex)) {
      v <- value(p)
      J <- vapply(params, function(k) {
        h <- 1e-6 * max(abs(p[[k]]), 1e-3); q1 <- q2 <- p; q1[[k]] <- p[[k]] + h; q2[[k]] <- p[[k]] - h
        (value(q1) - value(q2)) / (2 * h)
      }, numeric(n))
      if (is.null(dim(J))) J <- matrix(J, n)
      for (k in params) assign(k, p[[k]], envir = env)
      return(list(f = v, J = J))
    }
    for (k in params) assign(k, p[[k]], envir = env)
    v <- eval(dex, env)
    J <- attr(v, "gradient")
    v <- as.numeric(v)
    if (length(v) == 1L) v <- rep(v, n)
    if (is.null(dim(J)) || nrow(J) != n) J <- matrix(rep(as.numeric(J), each = n), n)
    list(f = v, J = J)
  }
  list(y = y, n = n, params = params, model = model, rhs = rhs, exact = !is.null(dex))
}
nl_program <- function(rhs, params, vars) {
  ops <- integer(0); args <- numeric(0)
  emit <- function(o, a = 0) { ops <<- c(ops, as.integer(o)); args <<- c(args, as.numeric(a)) }
  bin <- c("+" = 10L, "-" = 11L, "*" = 12L, "/" = 13L, "^" = 14L, "pmin" = 30L, "pmax" = 31L)
  un  <- c("exp" = 20L, "log" = 21L, "log10" = 22L, "sqrt" = 23L, "abs" = 24L, "sin" = 25L,
           "cos" = 26L, "tan" = 27L, "log2" = 28L)
  ok <- TRUE
  walk <- function(e) {
    if (!ok) return()
    if (is.numeric(e) && length(e) == 1L) return(emit(3L, e))
    if (is.name(e)) {
      nm <- as.character(e)
      if (nm %in% vars)   return(emit(1L, match(nm, vars) - 1L))
      if (nm %in% params) return(emit(2L, match(nm, params) - 1L))
      if (nm == "pi")     return(emit(3L, pi))
      ok <<- FALSE; return()
    }
    if (!is.call(e)) { ok <<- FALSE; return() }
    fn <- as.character(e[[1]]); a <- as.list(e)[-1]
    if (fn == "(" && length(a) == 1L) return(walk(a[[1]]))
    if (fn == "-" && length(a) == 1L) { walk(a[[1]]); return(emit(15L)) }
    if (fn == "+" && length(a) == 1L) return(walk(a[[1]]))
    if (fn %in% names(bin) && length(a) == 2L) { walk(a[[1]]); walk(a[[2]]); return(emit(bin[[fn]])) }
    if (fn %in% names(un) && length(a) == 1L) { walk(a[[1]]); return(emit(un[[fn]])) }
    if (fn == "log" && length(a) == 2L) {
      walk(a[[1]]); emit(21L); walk(a[[2]]); emit(21L); return(emit(13L)) }
    ok <<- FALSE
  }
  walk(rhs)
  if (!ok || !length(ops)) return(NULL)
  list(ops = ops, args = args)
}
nl_generic_starts <- function(m, n_draw = 400L, keep = 40L) {
  P <- length(m$params); if (!P) return(list())
  base <- setNames(rep(1, P), m$params)
  pts <- withr::with_seed(11, lapply(1:4, function(i)
    setNames(sample(c(-1, 1), P, TRUE) * 10^stats::runif(P, -4, 0), m$params)))
  lin <- vapply(seq_len(P), function(k) {
    seen <- 0L
    for (b in pts) {
      e1 <- tryCatch(m$model(as.list(b)), error = function(e) NULL)
      if (is.null(e1) || !all(is.finite(e1$J[, k]))) next
      q <- b; q[k] <- b[k] * 1.7 + 0.3
      e2 <- tryCatch(m$model(as.list(q)), error = function(e) NULL)
      if (is.null(e2) || !all(is.finite(e2$J[, k]))) next
      if (!isTRUE(all.equal(e1$J[, k], e2$J[, k], tolerance = 1e-7))) return(FALSE)
      if (any(abs(e1$J[, k]) > 0)) seen <- seen + 1L
    }
    seen >= 2L
  }, TRUE)
  nonlin <- which(!lin)
  draws <- withr::with_seed(7, lapply(seq_len(n_draw), function(i) {
    p <- base
    p[nonlin] <- sample(c(-1, 1, 1), length(nonlin), TRUE) * 10^stats::runif(length(nonlin), -4, 3)
    p
  }))
  if (!any(lin)) {
    sc <- vapply(draws, function(p) { f <- tryCatch(m$model(as.list(p))$f, error = function(e) NA); s <- sum((m$y - f)^2); if (is.finite(s)) s else Inf }, 0)
    return(draws[order(sc)][seq_len(min(keep, sum(is.finite(sc))))])
  }
  cand <- lapply(draws, function(p) {
    p[lin] <- 0
    e <- tryCatch(m$model(as.list(p)), error = function(e) NULL)
    if (is.null(e) || !all(is.finite(e$f)) || !all(is.finite(e$J[, lin]))) return(NULL)
    B <- e$J[, lin, drop = FALSE]
    q <- qr(B); cf <- qr.coef(q, m$y - e$f)
    if (anyNA(cf)) return(NULL)
    p[lin] <- cf
    f <- e$f + B %*% cf
    list(p = p, s = sum((m$y - f)^2))
  })
  cand <- Filter(function(c) !is.null(c) && is.finite(c$s), cand)
  if (!length(cand)) return(list())
  s <- vapply(cand, `[[`, 0, "s")
  lapply(cand[order(s)][seq_len(min(keep, length(cand)))], `[[`, "p")
}

nl_lm <- function(m, start, w = NULL, maxiter = 500) {
  sw  <- if (is.null(w)) rep(1, m$n) else sqrt(w)
  fn  <- function(p) { r <- sw * (m$y - m$model(p)$f); r[!is.finite(r)] <- 1e100; r }
  jac <- function(p) { J <- -sw * m$model(p)$J; J[!is.finite(J)] <- 0; J }
  o <- tryCatch(suppressWarnings(minpack.lm::nls.lm(par = start, fn = fn, jac = jac,
         control = minpack.lm::nls.lm.control(maxiter = maxiter))), error = function(e) NULL)
  if (is.null(o)) return(NULL)
  p <- unlist(o$par)[m$params]
  f <- m$model(as.list(p))$f
  if (!all(is.finite(f)) || !all(is.finite(p))) return(NULL)
  list(par = p, f = f, conv = o$info %in% 1:4)
}
nl_mupe_lm <- function(m, fit) {
  for (k in seq_len(100)) {
    w  <- 1 / pmax(abs(fit$f), 1e-10)^2
    nf <- nl_lm(m, fit$par, w)
    if (is.null(nf)) break
    d  <- sum((nf$f - fit$f)^2) / max(sum(fit$f^2), 1e-10)
    fit <- nf
    if (!is.finite(d) || d <= 1e-12) break
  }
  fit
}
nl_obj <- function(y, f, mupe) if (mupe) sum(((y - f) / pmax(abs(f), 1e-10))^2) else sum((y - f)^2)

nl_shape_starts <- function(eq, data, x_vars, y, keep = 12) {
  if (!length(x_vars) || !(eq %in% NL_EQ_CODES)) return(list())
  X   <- lapply(x_vars, function(v) as.numeric(data[[v]])); names(X) <- x_vars
  sdx <- vapply(X, function(x) { s <- stats::sd(x); if (is.finite(s) && s > 0) s else max(abs(x), 1) }, 0)
  x1  <- X[[1]]; s1 <- sdx[[1]]
  mx  <- stats::median(abs(x1)); if (!is.finite(mx) || mx == 0) mx <- 1
  bn  <- paste0("b_", x_vars)
  kk  <- c(-8, -4, -2, -1, -0.5, -0.25, -0.1, 0.1, 0.25, 0.5, 1, 2, 4, 8)
  dd  <- c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4)
  rr  <- c(0.01, 0.05, 0.1, 0.25, 0.5, 1, 2, 4, 8)
  grid_of <- function(vals, k) {
    g <- as.matrix(do.call(expand.grid, rep(list(vals), k)))
    if (nrow(g) > 600) g <- g[unique(round(seq(1, nrow(g), length.out = 600))), , drop = FALSE]
    g
  }
  cand <- list()
  add <- function(theta, B, lin) {
    B <- as.matrix(B)
    if (!all(is.finite(B))) return()
    cand[[length(cand) + 1L]] <<- list(theta = theta, B = B, lin = lin)
  }
  if (eq == "Power Curve") {
    if (any(vapply(X, function(x) any(x <= 0), TRUE))) return(list())
    g <- grid_of(seq(-1, 2.5, by = 0.25), length(X))
    for (i in seq_len(nrow(g))) add(setNames(g[i, ], bn), Reduce(`*`, Map(function(x, b) x^b, X, g[i, ])), "a")
  } else if (eq == "Exponential") {
    g <- grid_of(c(-3, -2, -1, -0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5, 1, 2, 3), length(X))
    for (i in seq_len(nrow(g))) { b <- g[i, ] / sdx
      add(setNames(b, bn), exp(Reduce(`+`, Map(`*`, X, b))), "a") }
  } else if (eq == "Logistic (S-Curve)") {
    g <- grid_of(c(-16, -8, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 8, 16), length(X))
    if (nrow(g) > 120) g <- g[unique(round(seq(1, nrow(g), length.out = 120))), , drop = FALSE]
    for (i in seq_len(nrow(g))) { b <- g[i, ] / sdx / length(X)
      eta <- Reduce(`+`, Map(`*`, X, b)); se <- stats::sd(eta); if (!is.finite(se) || se == 0) se <- 1
      for (mid in c(stats::quantile(eta, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), names = FALSE),
                    min(eta) - 2 * se, max(eta) + 2 * se))
        add(c(c = -mid, setNames(b, bn)), 1 / (1 + exp(-(eta - mid))), "a") }
  } else if (eq == "Gompertz") {
    for (k in kk) { cc <- k / s1
      for (r in c(0.01, 0.1, 0.5, 1, 2, 5, 20, 100)) { b <- r * exp(cc * stats::median(x1))
        add(c(b = b, c = cc), exp(-b * exp(-cc * x1)), "a") } }
  } else if (eq == "Michaelis-Menten") {
    for (e in seq(-2, 2, by = 0.25)) { b <- mx * 10^e; add(c(b = b), x1 / (b + x1), "a") }
  } else if (eq == "Weibull (Type 1)") {
    if (any(x1 < 0)) return(list())
    for (d in dd) for (r in rr) { cc <- r / mx^d
      add(c(c = cc, d = d), cbind(1, -exp(-cc * x1^d)), c("a", "b")) }
  } else if (eq == "Weibull (Type 2)") {
    if (any(x1 < 0)) return(list())
    for (d in dd) for (r in rr) { b <- r / mx^d
      add(c(b = b, c = d), 1 - exp(-b * x1^d), "a") }
  }
  if (!length(cand)) return(list())
  score <- vapply(cand, function(cd) {
    q <- qr(cd$B); lin <- qr.coef(q, y)
    if (anyNA(lin)) return(Inf)
    sum((y - cd$B %*% lin)^2)
  }, 0)
  ord <- order(score)[seq_len(min(keep, sum(is.finite(score))))]
  lapply(cand[ord], function(cd) c(setNames(qr.coef(qr(cd$B), y), cd$lin), cd$theta))
}

NL_EQ_CODES <- c("Power Curve", "Exponential", "Logistic (S-Curve)", "Gompertz",
                 "Michaelis-Menten", "Weibull (Type 1)", "Weibull (Type 2)")
nl_kernel_params <- function(eq, x_vars) {
  bn <- paste0("b_", x_vars)
  switch(eq, "Power Curve" = , "Exponential" = c("a", bn), "Logistic (S-Curve)" = c("a", "c", bn),
         "Gompertz" = c("a", "b", "c"), "Michaelis-Menten" = c("a", "b"),
         "Weibull (Type 1)" = c("a", "b", "c", "d"), "Weibull (Type 2)" = c("a", "b", "c"), NULL)
}

nl_kernel_two_pass <- function(call, S, mupe) {
  r <- call(S, 60L)
  ok <- which(r$ok == 1L)
  if (!length(ok)) return(r)
  ob <- if (mupe) r$spe else r$sse
  top <- ok[order(ob[ok])][seq_len(min(20L, length(ok)))]
  top <- top[r$conv[top] == 0L]
  if (length(top)) {
    r2 <- call(r$par[, top, drop = FALSE], 500L)
    for (nm in c("sse", "spe", "conv", "ok")) r[[nm]][top] <- r2[[nm]]
    r$par[, top] <- r2$par
  }
  r
}
nl_fit_builtin <- function(formula, data, eq, x_vars, term = "Least Squares",
                           seed = NULL, extra_starts = NULL, grid = TRUE, kernel = TRUE) {
  t_start <- proc.time()[[3]]
  jit <- compiler::enableJIT(0); on.exit(compiler::enableJIT(jit), add = TRUE)
  m    <- nl_compile(formula, data)
  mupe <- identical(term, "MUPE")
  fast <- get0("CERES_FAST", inherits = TRUE)
  canon <- nl_kernel_params(eq, x_vars)
  use_cpp <- isTRUE(kernel) && is.function(fast$nl_fit) && !is.null(canon) && setequal(canon, m$params)
  vars <- intersect(all.vars(m$rhs), names(data))
  prog <- if (is.null(canon) && isTRUE(kernel) && is.function(fast$nl_fit) &&
              all(vapply(data[vars], is.numeric, TRUE))) nl_program(m$rhs, m$params, vars)
  use_expr <- !is.null(prog)
  as_start <- function(s) {
    s <- unlist(s)
    v <- setNames(rep(1, length(m$params)), m$params)
    k <- intersect(names(s), m$params); v[k] <- as.numeric(s[k])
    v[!is.finite(v)] <- 1
    v
  }
  starts <- c(if (length(seed)) list(seed), nl_shape_starts(eq, data, x_vars, m$y, keep = if (use_cpp) 2000L else 12L),
              if (is.null(canon)) nl_generic_starts(m, keep = if (use_expr) 200L else 30L),
              extra_starts, if (grid) make_nl_start_list(m$params))
  starts <- lapply(starts, as_start)
  starts <- starts[!duplicated(lapply(starts, signif, 10))]
  fits <- list()
  if (use_cpp) {
    S <- vapply(starts, function(s) s[canon], numeric(length(canon)))
    if (is.null(dim(S))) S <- matrix(S, nrow = 1)
    r <- nl_kernel_two_pass(function(S, it) fast$nl_fit(match(eq, NL_EQ_CODES) - 1L, as.matrix(data[, x_vars, drop = FALSE]), m$y, S,
                                                       mupe, it, as.integer(fast$threads %||% 1L)), S, mupe)
    for (i in which(r$ok == 1L))
      fits[[length(fits) + 1L]] <- list(par = setNames(r$par[, i], canon)[m$params], conv = r$conv[i] == 1L,
                                        sse = r$sse[i], spe = r$spe[i])
  } else if (use_expr) {
    S <- vapply(starts, function(s) s[m$params], numeric(length(m$params)))
    if (is.null(dim(S))) S <- matrix(S, nrow = 1)
    r <- nl_kernel_two_pass(function(S, it) fast$nl_fit(-1L, as.matrix(data[, vars, drop = FALSE]), m$y, S, mupe, it,
                                                       as.integer(fast$threads %||% 1L), prog$ops, prog$args), S, mupe)
    for (i in which(r$ok == 1L))
      fits[[length(fits) + 1L]] <- list(par = setNames(r$par[, i], m$params), conv = r$conv[i] == 1L,
                                        sse = r$sse[i], spe = r$spe[i])
  } else {
    for (s in starts) { ft <- nl_lm(m, s, maxiter = 60); if (!is.null(ft)) fits[[length(fits) + 1L]] <- ft }
    if (length(fits)) {
      key  <- vapply(fits, function(ft) paste(signif(ft$par, 6), collapse = "|"), "")
      fits <- fits[!duplicated(key)]
      sse  <- vapply(fits, function(ft) nl_obj(m$y, ft$f, FALSE), 0)
      pick <- order(sse)[seq_len(min(8, length(fits)))]
      if (mupe) {
        spe  <- vapply(fits, function(ft) nl_obj(m$y, ft$f, TRUE), 0)
        pick <- unique(c(pick, order(spe)[seq_len(min(8, length(fits)))]))
      }
      fits <- lapply(fits[pick], function(ft) {
        full <- if (isTRUE(ft$conv)) ft else (nl_lm(m, ft$par) %||% ft)
        if (mupe) nl_mupe_lm(m, full) else full
      })
    }
  }
  if (!length(fits))
    stop(sprintf("None of the %d starting points gave a finite %s curve for this data.", length(starts), eq))
  obj  <- vapply(fits, function(ft) if (!is.null(ft$f)) nl_obj(m$y, ft$f, mupe) else if (mupe) ft$spe else ft$sse, 0)
  conv <- vapply(fits, function(ft) isTRUE(ft$conv), TRUE)
  ord  <- order(signif(obj / min(obj[is.finite(obj)]), 6), !conv)
  ord  <- ord[is.finite(obj[ord])]
  ord  <- ord[!duplicated(vapply(fits[ord], function(ft) paste(signif(ft$par, 6), collapse = "|"), ""))]
  last <- NULL
  for (i in utils::head(ord, 10)) {
    p   <- as.list(fits[[i]]$par)
    fi  <- fits[[i]]$f %||% tryCatch(m$model(p)$f, error = function(e) NULL)
    if (is.null(fi) || !all(is.finite(fi))) next
    out <- tryCatch(withCallingHandlers({
      if (!mupe) fit_nlls(formula, data, p)
      else minpack.lm::nlsLM(stats::as.formula(formula), data = data, start = p,
                             weights = 1 / pmax(abs(fi), 1e-10)^2,
                             control = minpack.lm::nls.lm.control(maxiter = 500))
    }, warning = function(w) if (grepl("maxiter|Number of iterations", conditionMessage(w), ignore.case = TRUE))
                               invokeRestart("muffleWarning")),
      error = function(e) { last <<- conditionMessage(e); NULL })
    if (!is.null(out)) {
      attr(out, "nl_info") <- list(starts = length(starts), converged = isTRUE(fits[[i]]$conv),
                                     secs = proc.time()[[3]] - t_start, kernel = use_cpp || use_expr)
      return(out)
    }
  }
  stop(paste0("The ", eq, " curve could not be fitted to this data", if (!is.null(last)) paste0(": ", last) else "."))
}

NL_CB_FUNS <- c("exp", "log", "log10", "log2", "sqrt", "abs", "sin", "cos", "tan", "pmin", "pmax")

nl_cb_check <- function(rhs, cols, cats = character(0), n = Inf) {
  out <- function(state, message = "", params = character(0), drivers = character(0), expr = NULL)
    list(state = state, message = message, params = params, drivers = drivers, expr = expr)
  rhs <- trimws(rhs %||% "")
  if (!nzchar(rhs)) return(out("empty", "Type an equation, or start from a shape above."))
  ex <- tryCatch(str2lang(rhs), error = function(e) e)
  if (inherits(ex, "error")) {
    if (grepl("unexpected end of input|INCOMPLETE", conditionMessage(ex)) || grepl("[-+*/^(,]\\s*$", rhs))
      return(out("incomplete", "Keep going: the equation isn't finished."))
    return(out("error", "Can't read this equation. Check the brackets and operators."))
  }
  near <- function(x, pool) {
    if (!length(pool)) return(NA_character_)
    d <- utils::adist(tolower(x), tolower(pool))[1, ]
    pre <- startsWith(tolower(pool), tolower(x))
    best <- if (any(pre)) which(pre)[1] else which.min(d)
    if (any(pre) || d[best] <= max(1, floor(nchar(x) / 3))) pool[best] else NA_character_
  }
  fns <- character(0)
  walk <- function(e) if (is.call(e)) { fns <<- c(fns, as.character(e[[1]])); lapply(as.list(e)[-1], walk) }
  walk(ex)
  bad_fn <- setdiff(unique(fns), c("+", "-", "*", "/", "^", "(", NL_CB_FUNS))
  if (length(bad_fn)) {
    s <- near(bad_fn[1], NL_CB_FUNS)
    return(out("error", paste0(bad_fn[1], "() isn't a function CERES knows.",
                               if (!is.na(s)) paste0(" Did you mean ", s, "()?") else
                                 paste0(" Use one of ", paste(NL_CB_FUNS, collapse = ", "), "."))))
  }
  syms <- setdiff(all.vars(ex), "pi")
  cat_used <- intersect(syms, cats)
  if (length(cat_used))
    return(out("error", paste0(cat_used[1], " is a category column. Non-linear equations need numeric drivers.")))
  drivers <- intersect(syms, cols)
  rest <- setdiff(syms, cols)
  for (s in rest) if (nchar(s) > 2 || grepl("^[A-Z]", s)) {
    m <- near(s, c(cols, cats))
    if (!is.na(m)) return(out("error", paste0(s, " isn't a column. Did you mean ", m, "?")))
  }
  params <- rest
  if (!length(drivers)) return(out("error", "Add a driver: one of the numeric columns.", params, drivers, ex))
  if (!length(params)) return(out("error", "Add a parameter to fit, such as a, b or c.", params, drivers, ex))
  if (is.finite(n) && n <= length(params))
    return(out("error", sprintf("%d parameters need more than %d rows of data.", length(params), n), params, drivers, ex))
  out("ready", sprintf("Ready · %d parameter%s · %d driver%s", length(params), if (length(params) == 1) "" else "s",
                       length(drivers), if (length(drivers) == 1) "" else "s"), params, drivers, ex)
}

nl_cb_latex <- function(ex, params, labels = character(0)) {
  txt <- function(s) gsub("([_#%&$])", "\\\\\\1", s)
  nm <- function(s) {
    if (s %in% params) {
      if (grepl("_", s)) { p <- strsplit(s, "_", fixed = TRUE)[[1]]
        return(paste0(p[1], "_{\\text{", txt(paste(p[-1], collapse = "_")), "}}")) }
      return(s)
    }
    if (s == "pi") return("\\pi")
    paste0("\\text{", txt(if (!is.na(labels[s])) labels[[s]] else s), "}")
  }
  prec <- function(e) {
    if (!is.call(e)) return(9)
    f <- as.character(e[[1]])
    if (f %in% c("+", "-") && length(e) == 3) 1 else if (f %in% c("*", "/")) 2 else if (f == "-") 3 else if (f == "^") 4 else 9
  }
  go <- function(e, need = 0) {
    s <- if (is.numeric(e)) format(e, digits = 6) else if (is.name(e)) nm(as.character(e)) else {
      f <- as.character(e[[1]]); a <- as.list(e)[-1]
      if (f == "(") go(a[[1]], need)
      else if (f %in% c("+", "-") && length(a) == 2) paste0(go(a[[1]], 1), " ", f, " ", go(a[[2]], 2))
      else if (f == "-" && length(a) == 1) paste0("-", go(a[[1]], 3))
      else if (f == "*") paste0(go(a[[1]], 2), " \\cdot ", go(a[[2]], 3))
      else if (f == "/") paste0("\\frac{", go(a[[1]]), "}{", go(a[[2]]), "}")
      else if (f == "^") paste0("{", go(a[[1]], 5), "}^{", go(a[[2]]), "}")
      else if (f == "exp") paste0("e^{", go(a[[1]]), "}")
      else if (f == "sqrt") paste0("\\sqrt{", go(a[[1]]), "}")
      else if (f == "abs") paste0("\\left|", go(a[[1]]), "\\right|")
      else if (f == "log") paste0("\\ln\\left(", go(a[[1]]), "\\right)")
      else if (f %in% c("log10", "log2")) paste0("\\log_{", sub("log", "", f), "}\\left(", go(a[[1]]), "\\right)")
      else if (f %in% c("pmax", "pmin")) paste0("\\", sub("p", "", f), "\\left(", paste(vapply(a, go, ""), collapse = ", "), "\\right)")
      else paste0("\\operatorname{", f, "}\\left(", paste(vapply(a, go, ""), collapse = ", "), "\\right)")
    }
    if (prec(e) < need && !(is.call(e) && as.character(e[[1]]) %in% c("/", "("))) paste0("\\left(", s, "\\right)") else s
  }
  go(ex)
}

nl_cb_meanings <- function(ex, params, drivers, response = "", labels = character(0)) {
  lab <- function(v) if (!is.na(labels[v])) labels[[v]] else v
  out <- setNames(rep(NA_character_, length(params)), params)
  set <- function(p, m) if (p %in% params && is.na(out[[p]])) out[[p]] <<- m
  is_p <- function(e) is.name(e) && as.character(e) %in% params
  is_v <- function(e) is.name(e) && as.character(e) %in% drivers
  strip <- function(e) { while (is.call(e) && identical(as.character(e[[1]]), "(")) e <- e[[2]]; e }
  terms <- function(e) { e <- strip(e)
    if (is.call(e) && as.character(e[[1]]) %in% c("+", "-") && length(e) == 3) c(terms(e[[2]]), terms(e[[3]])) else list(e) }
  factors <- function(e) { e <- strip(e)
    if (is.call(e) && as.character(e[[1]]) == "*") c(factors(e[[2]]), factors(e[[3]])) else list(e) }
  fixed <- if (grepl("cost|price|\\$", response, ignore.case = TRUE)) "fixed cost" else "constant"
  rate_inside <- function(e, what) {
    for (t in terms(e)) { f <- factors(t)
      ps <- Filter(is_p, f); vs <- Filter(is_v, f)
      if (length(ps) == 1 && length(vs) == 1 && length(f) == 2) set(as.character(ps[[1]]), paste(what, lab(as.character(vs[[1]]))))
      else if (length(f) == 1 && is_p(f[[1]])) set(as.character(f[[1]]), "shift") }
  }
  visit <- function(e) {
    e <- strip(e)
    if (!is.call(e)) return(invisible())
    f <- as.character(e[[1]])
    if (f == "^" && is_v(strip(e[[2]])) && is_p(strip(e[[3]]))) set(as.character(strip(e[[3]])), paste("exponent on", lab(as.character(strip(e[[2]])))))
    if (f == "exp") rate_inside(e[[2]], "growth rate on")
    if (f == "/") {
      num <- strip(e[[2]]); den <- strip(e[[3]])
      dt <- terms(den)
      if (length(dt) == 2 && identical(strip(dt[[1]]), 1) && is.call(strip(dt[[2]])) && as.character(strip(dt[[2]])[[1]]) == "exp") {
        for (q in Filter(is_p, factors(num))) set(as.character(q), "ceiling")
        inner <- strip(strip(dt[[2]])[[2]]); if (is.call(inner) && as.character(inner[[1]]) == "-" && length(inner) == 2) inner <- inner[[2]]
        rate_inside(inner, "steepness on")
      }
      if (length(dt) == 2 && sum(vapply(dt, function(t) is_p(strip(t)), TRUE)) == 1 && sum(vapply(dt, function(t) is_v(strip(t)), TRUE)) == 1) {
        q <- Filter(function(t) is_p(strip(t)), dt)[[1]]
        set(as.character(strip(q)), "half-way point")
        for (r in Filter(is_p, factors(num))) set(as.character(r), "ceiling")
      }
    }
    for (a in as.list(e)[-1]) visit(a)
  }
  for (t in terms(ex)) {
    t <- strip(t)
    if (is_p(t)) { set(as.character(t), fixed); next }
    f <- factors(t)
    if (length(f) > 1) for (q in Filter(is_p, f)) set(as.character(q), if (length(f) == 2 && any(vapply(f, is_v, TRUE))) paste("per unit of", lab(as.character(Filter(is_v, f)[[1]]))) else "scale")
    visit(t)
  }
  out
}

nl_cb_shapes <- function(drivers) {
  x1 <- drivers[1] %||% "X"; x2 <- if (length(drivers) > 1) drivers[2] else x1
  list(
    list(name = "Power",         f = "a·X^b",            eq = sprintf("a * %s^b", x1),                    d = "M8 43 Q 50 36 112 8"),
    list(name = "Power + fixed", f = "a + b·X^c",        eq = sprintf("a + b * %s^c", x1),                d = "M8 32 Q 55 27 112 6"),
    list(name = "Two drivers",   f = "a·X1^b·X2^c",      eq = sprintf("a * %s^b * %s^c", x1, x2),         d = "M8 42 Q 40 27 70 23 T 112 6"),
    list(name = "Exponential",   f = "a·e^(bX)",         eq = sprintf("a * exp(b * %s)", x1),             d = "M8 43 Q 85 41 112 6"),
    list(name = "S-curve",       f = "a/(1+e^-(c+bX))",  eq = sprintf("a / (1 + exp(-(c + b * %s)))", x1), d = "M8 43 C 55 43, 62 8, 112 8"),
    list(name = "Saturating",    f = "aX/(b+X)",         eq = sprintf("a * %s / (b + %s)", x1, x1),       d = "M8 43 Q 24 10 112 8"),
    list(name = "Blank",         f = "start empty",      eq = "",                                         d = "M60 15 L60 37 M49 26 L71 26"))
}

nl_cb_fail_message <- function(msg, eq = "Custom") {
  what <- if (identical(eq, "Custom")) "This equation" else paste("The", eq, "curve")
  if (grepl("singular|step factor|converge", msg, ignore.case = TRUE))
    return(paste0(what, " has no single best fit for this data: some of its parameters can trade off ",
                  "against each other, because the data doesn't show the shape it needs. Try a simpler shape or another driver."))
  if (grepl("finite", msg, ignore.case = TRUE))
    return(paste0(what, " gives impossible values (infinite or missing) for this data, for example a log or power of a value at or below zero."))
  sub("^The Custom curve could not be fitted to this data: ?", "", msg)
}

nl_cb_svg <- function(x, y, curve_x = NULL, curve_y = NULL, xlab = "", ylab = "") {
  W <- 560; H <- 250; L <- 52; R <- 12; T <- 12; B <- 34
  fin <- is.finite(x) & is.finite(y); x <- x[fin]; y <- y[fin]
  if (!length(x)) return(htmltools::HTML(""))
  xr <- range(c(x, curve_x), finite = TRUE); yr <- range(c(y, curve_y), finite = TRUE)
  if (diff(xr) == 0) xr <- xr + c(-1, 1); if (diff(yr) == 0) yr <- yr + c(-1, 1)
  yr <- yr + c(-1, 1) * diff(yr) * 0.04
  sx <- function(v) L + (v - xr[1]) / diff(xr) * (W - L - R)
  sy <- function(v) H - B - (v - yr[1]) / diff(yr) * (H - B - T)
  ticks <- pretty(yr, 4); ticks <- ticks[ticks >= yr[1] & ticks <= yr[2]]
  fmt <- function(v) vapply(v, function(u) {
    a <- abs(u)
    if (a >= 1e9) paste0(signif(u / 1e9, 3), "B") else if (a >= 1e6) paste0(signif(u / 1e6, 3), "M")
    else if (a >= 1e4) paste0(signif(u / 1e3, 3), "k") else format(signif(u, 3), big.mark = ",")
  }, "")
  grid <- paste0(sprintf('<line class="c-cb-grid" x1="%d" x2="%d" y1="%.1f" y2="%.1f"></line><text class="c-cb-tick" x="%d" y="%.1f" text-anchor="end">%s</text>',
                         L, W - R, sy(ticks), sy(ticks), L - 6, sy(ticks) + 4, fmt(ticks)), collapse = "")
  pts <- paste0(sprintf('<circle class="c-cb-pt" cx="%.1f" cy="%.1f" r="4"></circle>', sx(x), sy(y)), collapse = "")
  line <- if (length(curve_x)) {
    ok <- is.finite(curve_x) & is.finite(curve_y)
    sprintf('<path class="c-cb-curve" d="M%s"></path>', paste(sprintf("%.1f %.1f", sx(curve_x[ok]), sy(curve_y[ok])), collapse = " L"))
  } else ""
  htmltools::HTML(sprintf(
    '<svg viewBox="0 0 %d %d" width="100%%" role="img" aria-label="%s against %s">%s<line class="c-cb-axis" x1="%d" x2="%d" y1="%d" y2="%d"></line>%s%s<text class="c-cb-tick" x="%d" y="%d" text-anchor="middle">%s</text></svg>',
    W, H, htmltools::htmlEscape(ylab), htmltools::htmlEscape(xlab), grid, L, W - R, H - B, H - B, line, pts,
    (L + W - R) %/% 2, H - 8, htmltools::htmlEscape(xlab)))
}

for (.f in c("nl_compile", "nl_lm", "nl_mupe_lm", "nl_obj", "nl_shape_starts", "nl_generic_starts",
             "nl_program", "nl_fit_builtin", "nl_cb_check", "nl_cb_latex", "nl_cb_meanings", "nl_cb_svg"))
  if (exists(.f, inherits = FALSE)) assign(.f, tryCatch(compiler::cmpfun(get(.f)), error = function(e) get(.f)))
rm(.f)

scaled_ginv <- function(M) {
  d <- sqrt(pmax(diag(M), 0)); d[!is.finite(d) | d == 0] <- 1
  Dinv <- diag(1 / d, nrow = length(d))
  Dinv %*% ceres_ns("MASS")$ginv(Dinv %*% M %*% Dinv) %*% Dinv
}

irls_step_small <- function(new, old) {
  d <- abs(new - old) / pmax(abs(old), 1)
  isTRUE(max(d, na.rm = TRUE) < 1e-10)
}

irls_continue <- function(spec, X, y, maxit = 1000L) {
  b <- spec$coefficients; keep <- !is.na(b); Xk <- X[, keep, drop = FALSE]; bk <- b[keep]
  conv <- FALSE
  for (k in seq_len(maxit)) {
    yh <- drop(Xk %*% bk); yh[abs(yh) < 1e-6] <- 1e-6
    sw <- 1 / abs(yh)
    bn <- qr.coef(qr(Xk * sw), y * sw); bn[!is.finite(bn)] <- 0
    done <- irls_step_small(bn, bk); bk <- bn
    if (done) { conv <- TRUE; break }
  }
  yh <- drop(Xk %*% bk); yh[abs(yh) < 1e-6] <- 1e-6
  b[keep] <- bk
  spec$coefficients <- b; spec$weights <- 1 / yh^2
  spec$converged <- conv; spec$iterations <- (spec$iterations %||% 0L) + k
  spec
}

ceres_error_term <- function(x) if (identical(x, "WLR")) "MUPE" else x
WLR_REMOVED_NOTICE <- "The WLR error term has been removed from CERES; this model runs with MUPE instead."

fullrank_cols <- function(X, cols = seq_len(ncol(X))) {
  q <- base::qr(X[, cols, drop = FALSE], tol = 1e-7)
  if (q$rank == length(cols)) cols else cols[sort(q$pivot[seq_len(q$rank)])]
}
SCREEN_EXACT_MAX_LMSUBSETS <- 50L
SCREEN_EXACT_MAX_LEAPS     <- 44L
screen_lmsubsets_ok <- function() requireNamespace("lmSubsets", quietly = TRUE)
screen_exact_limit  <- function() if (screen_lmsubsets_ok()) SCREEN_EXACT_MAX_LMSUBSETS else SCREEN_EXACT_MAX_LEAPS
screen_best_subsets <- function(x, y, nbest) {
  p <- ncol(x); y <- as.numeric(y)
  if (p <= SCREEN_EXACT_MAX_LMSUBSETS && screen_lmsubsets_ok()) {
    W <- tryCatch({
      df <- data.frame(y, x, check.names = FALSE)
      vn <- paste0("V", seq_len(p))
      names(df) <- c("y", vn)
      ms <- lmSubsets::lmSubsets(y ~ ., data = df, nbest = nbest, include = "(Intercept)")
      sm <- ms$submodel
      ok <- which(!is.na(sm$RSS))
      ok <- ok[order(sm$SIZE[ok], sm$BEST[ok])]
      S  <- as.matrix(ms$subset)[ok, vn, drop = FALSE]
      storage.mode(S) <- "logical"
      dimnames(S) <- list(NULL, colnames(x))
      S
    }, error = function(e) NULL)
    if (!is.null(W) && nrow(W) > 0L && !anyNA(W)) return(structure(W, engine = "lmSubsets"))
  }
  method <- if (p > SCREEN_EXACT_MAX_LEAPS) "seqrep" else "exhaustive"
  W <- summary(leaps::regsubsets(x, y, nvmax = p, nbest = nbest, method = method,
                                 really.big = TRUE))$which[, -1, drop = FALSE]
  structure(W, engine = paste0("leaps-", method))
}

search_gram <- function(X_tr, y_tr) {
  Xn <- X_tr[, -1L, drop = FALSE]
  xb <- colMeans(Xn)
  Xc <- Xn - rep(xb, each = nrow(Xn))
  s  <- sqrt(colSums(Xc^2))
  ok <- is.finite(s) & s > 0
  s1 <- ifelse(ok, s, 1)
  yb <- mean(y_tr); yc <- y_tr - yb
  list(xb = xb, s = s1, ok = ok, C = crossprod(Xc) / tcrossprod(s1),
       thr = pmax(100 * 1e-7 * sqrt(colSums(Xn^2)) / s1, 3e-3),
       yb = yb, syy = sum(yc^2), sxy = drop(crossprod(Xc, yc)) / s1)
}
gram_chol <- function(g, j) {
  if (!length(j) || !all(g$ok[j])) return(NULL)
  U <- tryCatch(chol(g$C[j, j, drop = FALSE]), error = function(e) NULL)
  if (is.null(U) || any(!(diag(U) > g$thr[j]))) return(NULL)
  U
}
gram_ols <- function(g, j, U) {
  bs  <- backsolve(U, forwardsolve(U, g$sxy[j], upper.tri = TRUE, transpose = TRUE))
  ssr <- g$syy - sum(bs * g$sxy[j])
  if (!is.finite(ssr) || ssr <= 1e-9 * g$syy) return(NULL)
  b   <- bs / g$s[j]
  Ri  <- chol2inv(U)
  ci  <- diag(Ri)
  list(b = c(g$yb - sum(g$xb[j] * b), b), ssr = ssr, inv = ci / g$s[j]^2, cinv = ci, Rinv = Ri)
}
gram_vif_graded <- function(g, j, Ri, a) {
  ci <- diag(Ri)
  if (!anyDuplicated(a)) return(ci)
  vapply(unique(a), function(k) {
    i <- which(a == k)
    if (length(i) == 1L) return(ci[i])
    (det(g$C[j[i], j[i], drop = FALSE]) * det(Ri[i, i, drop = FALSE]))^(1 / length(i))
  }, numeric(1))
}

CERES_FAST_VERSION <- 5L
ceres_fast_load <- function() {
  if (isFALSE(getOption("ceres.fast", TRUE))) return(NULL)
  wrap <- function(get, how) {
    mx <- tryCatch(as.integer(get("ceresfast_max_threads")()), error = function(e) 1L)
    want <- getOption("ceres.threads")
    want <- if (is.numeric(want) && length(want) == 1L && is.finite(want)) as.integer(want)
            else tryCatch(as.integer(parallel::detectCores()), error = function(e) 1L)
    th <- max(1L, min(if (is.na(want)) 1L else want, if (is.na(mx)) 1L else mx))
    message("CERES: search kernels use ", th, " thread", if (th > 1L) "s" else "", " (", mx, " available to this build)")
    list(how = how, threads = th, ols_subsets = get("ols_subsets_cpp"),
         mupe_subsets = get("mupe_subsets_cpp"), zmpe_subsets = get("zmpe_subsets_cpp"),
         subset_cols = get("subset_cols_cpp"), assemble = get("assemble_specs_cpp"),
         nl_fit = tryCatch(get("nl_fit_cpp"), error = function(e) NULL))
  }
  if (requireNamespace("ceresfast", quietly = TRUE)) {
    v <- tryCatch(getExportedValue("ceresfast", "ceresfast_version")(), error = function(e) NA)
    if (identical(as.integer(v), CERES_FAST_VERSION)) {
      message("CERES: compiled search kernels from the installed ceresfast package")
      return(wrap(function(nm) getExportedValue("ceresfast", nm), "package"))
    }
    message("CERES: installed ceresfast is version ", v, ", expected ", CERES_FAST_VERSION, " - compiling the bundled source instead")
  }
  src <- c(getOption("ceres.fast_src"), file.path(getwd(), "ceresfast", "src", "ceres_fast.cpp"),
           file.path(dirname(get0("expected_www", ifnotfound = "www")), "ceresfast", "src", "ceres_fast.cpp"))
  src <- src[file.exists(src)][1]
  if (is.na(src) || !requireNamespace("Rcpp", quietly = TRUE) || !requireNamespace("RcppArmadillo", quietly = TRUE)) {
    message("CERES: compiled search kernels unavailable (", if (is.na(src)) "source not found" else "Rcpp / RcppArmadillo missing", ") - using R")
    return(NULL)
  }
  cache <- ceres_cache_dir("CERES")
  env <- new.env(parent = globalenv())
  t0 <- proc.time()[[3]]
  ok <- tryCatch({ Rcpp::sourceCpp(src, env = env, cacheDir = cache, showOutput = FALSE, verbose = FALSE); TRUE },
                 error = function(e) { message("CERES: compiling the search kernels failed (", conditionMessage(e), ") - using R"); FALSE })
  if (!ok || !identical(as.integer(env$ceresfast_version()), CERES_FAST_VERSION)) return(NULL)
  message(sprintf("CERES: compiled search kernels ready (%.1f s)", proc.time()[[3]] - t0))
  wrap(function(nm) get(nm, envir = env), "compiled")
}
CERES_FAST <- ceres_fast_load()

CERES_POOL_MAX <- 6L
ceres_eval_js <- function(code) getExportedValue("webr", "eval_js")(code)
CERES_APPLE <- CERES_IN_BROWSER && isTRUE(tryCatch({
  ua <- as.character(ceres_eval_js("String(self.navigator && self.navigator.userAgent || '')"))
  grepl("AppleWebKit", ua, fixed = TRUE) && !grepl("Chrome/|Chromium/|Edg/", ua)
}, error = function(e) FALSE))
if (CERES_APPLE) `%>%` <- magrittr::`%!>%`
if (CERES_APPLE) {
  modalDialog <- function(...) {
    cl <- sys.call()
    cl[[1]] <- quote(shiny::modalDialog)
    structure(list(call = cl, env = parent.frame()), class = "ceres_deferred_modal")
  }
  showModal <- function(ui, session = getDefaultReactiveDomain()) {
    force(ui); force(session)
    later::later(function() {
      tryCatch(shiny::withReactiveDomain(session, isolate({
        if (inherits(ui, "ceres_deferred_modal")) ui <- eval(ui$call, ui$env)
        shiny::showModal(ui, session = session)
      })), error = function(e) message("CERES: could not open the window: ", conditionMessage(e)))
    })
    invisible()
  }
}
ceres_pool_start <- function() {
  if (!CERES_IN_BROWSER || is.null(CERES_FAST)) return(FALSE)
  ok <- tryCatch({
    if (!isTRUE(ceres_eval_js("self.crossOriginIsolated === true && typeof SharedArrayBuffer !== 'undefined'"))) return(FALSE)
    base  <- get0(".base_url", envir = globalenv(), ifnotfound = "")
    pbase <- sub("shinylive/webr/?$", "ceres-pool/", base)
    n <- as.integer(min(CERES_POOL_MAX, max(1, ceres_eval_js("(navigator.hardwareConcurrency || 2)") - 1)))
    ceres_eval_js(sprintf("importScripts('%sceres-pool-sync.js'); CeresSyncPool.init('%s', %d)", pbase, pbase, n)) > 0
  }, error = function(e) FALSE)
  isTRUE(ok)
}
ceres_pool_run <- function(kind, a) {
  if (ceres_eval_js("CeresSyncPool.ready()") < 1) return(NULL)
  d <- tempfile("ceres_pool_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f64 <- function(x, nm) writeBin(as.double(x), file.path(d, nm), size = 8, endian = "little")
  i32 <- function(x, nm) writeBin(as.integer(x), file.path(d, nm), size = 4, endian = "little")
  if (kind == 0L) {
    C <- a[[1]]; s <- a[[2]]; xb <- a[[3]]; sxy <- a[[4]]; syy <- a[[5]]; yb <- a[[6]]; thr <- a[[7]]; okc <- a[[8]]
    sub <- a[[9]]; asg <- a[[10]]; Xp <- a[[11]]; ntr <- a[[12]]; X <- 0; y <- 0; ll <- 0L; cap <- 0L; n <- 1L
  } else {
    X <- a[[1]]; y <- a[[2]]; ll <- as.integer(isTRUE(a[[3]])); C <- a[[4]]; thr <- a[[5]]; okc <- a[[6]]
    sub <- a[[7]]; asg <- a[[8]]; Xp <- a[[9]]; cap <- as.integer(a[[10]]); n <- nrow(X)
    s <- xb <- sxy <- rep(0, ncol(C)); syy <- yb <- 0; ntr <- n
  }
  P <- ncol(C); NP <- nrow(Xp); M <- length(sub); P1 <- P + 1L
  f64(X, "X.f64"); f64(y, "y.f64"); f64(C, "C.f64"); f64(thr, "thr.f64"); f64(Xp, "Xp.f64")
  f64(s, "s.f64"); f64(xb, "xb.f64"); f64(sxy, "sxy.f64"); i32(okc, "okc.i32"); i32(asg, "asg.i32")
  i32(c(0L, cumsum(lengths(sub))), "off.i32"); i32(unlist(sub, use.names = FALSE) - 1L, "idx.i32")
  spec <- sprintf('{"kind":%d,"loglin":%d,"cap":%d,"n":%d,"P":%d,"NP":%d,"n_tr":%d,"syy":%.17g,"yb":%.17g,"dir":"%s","batch":%d,"timeout_s":120}',
                  kind, ll, cap, n, P, NP, as.integer(ntr), syy, yb, d, if (kind == 0L) 2048L else 256L)
  t_in <- Sys.time()
  st <- ceres_eval_js(sprintf("CeresSyncPool.run('%s')", spec))
  t_pool <- Sys.time()
  if (!startsWith(st, "ok")) return(NULL)
  on.exit(message(sprintf("CERES pool: %.2f s on the workers (incl. copies), %.2f s back into R",
                          as.numeric(difftime(t_pool, t_in, units = "secs")),
                          as.numeric(difftime(Sys.time(), t_pool, units = "secs")))), add = TRUE)
  rd <- function(nm, what, len) readBin(file.path(d, paste0("out_", nm, ".bin")), what, n = len, size = if (what == "integer") 4L else 8L, endian = "little")
  if (kind == 0L)
    return(list(ok = rd("ok", "integer", M), k = rd("k", "integer", M), b = matrix(rd("b", "double", M * P1), M, P1),
                ssr = rd("ssr", "double", M), nsig = rd("nsig", "integer", M), vif = rd("vif", "double", M),
                yhat = matrix(rd("yhat", "double", NP * M), NP, M)))
  cov_wanted <- kind == 2L || ll == 1L
  covs <- vector("list", M)
  if (cov_wanted) {
    hc <- rd("have_cov", "integer", M); kk <- lengths(sub) + 1L
    flat <- rd("cov", "double", sum(kk^2)); at <- c(0L, cumsum(kk^2))
    for (m in which(hc == 1L)) covs[[m]] <- matrix(flat[(at[m] + 1L):at[m + 1L]], kk[m], kk[m])
  }
  list(ok = rd("ok", "integer", M), conv = rd("conv", "integer", M), iter = rd("iter", "integer", M),
       b = matrix(rd("b", "double", M * P1), M, P1), sigma = rd("sigma", "double", M), r2 = rd("r2", "double", M),
       adj_r2 = rd("adj_r2", "double", M), ll = rd("ll", "double", M), nsig = rd("nsig", "integer", M),
       sig_ok = rd("sig_ok", "integer", M), vif = rd("vif", "double", M), yhat = matrix(rd("yhat", "double", NP * M), NP, M),
       w = matrix(rd("w", "double", n * M), n, M), cov = covs)
}
ceres_pool_wrap <- function(kind, fallback) {
  force(kind); force(fallback)
  function(...) {
    t0 <- Sys.time()
    r <- if (isFALSE(getOption("ceres.pool"))) NULL else tryCatch(ceres_pool_run(kind, list(...)), error = function(e) NULL)
    how <- "worker pool"
    if (is.null(r)) { r <- fallback(...); how <- "one thread (fallback)" }
    message(sprintf("CERES kernel %s: %d models on the %s in %.2f s", c("OLS", "MUPE", "ZMPE")[kind + 1L],
                    length(r$ok), how, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    r
  }
}
if (CERES_IN_BROWSER && ceres_pool_start()) {
  CERES_FAST$ols_subsets  <- ceres_pool_wrap(0L, CERES_FAST$ols_subsets)
  CERES_FAST$mupe_subsets <- ceres_pool_wrap(1L, CERES_FAST$mupe_subsets)
  CERES_FAST$zmpe_subsets <- ceres_pool_wrap(2L, CERES_FAST$zmpe_subsets)
  CERES_FAST$how <- "browser worker pool"
  message("CERES: search kernels on the browser worker pool")
}

ceres_precompile <- function(fn) {
  done <- tryCatch(identical(typeof(.Internal(bodyCode(fn))), "bytecode"), error = function(e) FALSE)
  if (done) return(fn)
  tryCatch(compiler::cmpfun(fn), error = function(e) fn)
}

expand_coefs <- function(b, nms) {
  out <- setNames(rep(NA_real_, length(nms)), nms)
  out[names(b)] <- b
  out
}

term_vif_graded <- function(R, a) {
  dR <- det(R)
  if (!is.finite(dR) || dR <= 0) return(rep(NA_real_, length(unique(a))))
  vapply(unique(a), function(k) {
    i <- which(a == k)
    (det(R[i, i, drop = FALSE]) * det(R[-i, -i, drop = FALSE]) / dR)^(1 / length(i))
  }, numeric(1))
}

lm_pred_interval <- function(model, newdata, level, mode) {
  err <- attr(model, "error_type") %||% "OLS"
  if (identical(mode, "linear") && err %in% c("MUPE", "ZMPE", "WLR") && !is.null(model$weights)) {
    yh <- as.numeric(predict(model, newdata = newdata))
    return(predict(model, newdata = newdata, interval = "predict", level = level,
                   weights = 1 / pmax(abs(yh), 1e-10)^2))
  }
  predict(model, newdata = newdata, interval = "predict", level = level)
}

vif_unweighted <- function(model) {
  X <- model.matrix(model); asg <- attr(X, "assign")
  cf   <- stats::coef(model)
  sds  <- apply(X, 2, stats::sd)
  keep <- asg != 0L & !is.na(cf[colnames(X)]) & is.finite(sds) & sds > 0
  if (sum(keep) < 2L) return(NULL)
  R  <- stats::cor(X[, keep, drop = FALSE]); a <- asg[keep]
  dR <- det(R); ks <- unique(a)
  if (!is.finite(dR) || dR <= 0) dR <- NA_real_
  labs <- attr(stats::terms(model), "term.labels")
  g <- vapply(ks, function(k) { i <- which(a == k)
    det(R[i, i, drop = FALSE]) * det(R[-i, -i, drop = FALSE]) / dR }, numeric(1))
  df_k <- vapply(ks, function(k) sum(a == k), numeric(1))
  data.frame(term = labs[ks], GVIF = g, Df = df_k, graded = g^(1 / df_k),
             stringsAsFactors = FALSE)
}

vif_for_set <- function(df, log = FALSE) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = TRUE)
  for (j in seq_along(df)) if (is.character(df[[j]])) df[[j]] <- factor(df[[j]])
  logged <- character(0)
  if (isTRUE(log)) for (j in names(df)) {
    x <- df[[j]]
    if (is.numeric(x) && any(!is.na(x)) && all(x[!is.na(x)] > 0)) { df[[j]] <- base::log(x); logged <- c(logged, j) }
  }
  df <- df[stats::complete.cases(df), , drop = FALSE]
  for (j in seq_along(df)) if (is.factor(df[[j]])) df[[j]] <- droplevels(df[[j]])
  if (ncol(df) < 2L || nrow(df) <= ncol(df) + 1L) return(NULL)
  orig <- names(df); safe <- paste0("v", seq_along(df)); names(df) <- safe
  df$.y <- seq_len(nrow(df))
  fit <- tryCatch(stats::lm(stats::reformulate(safe, ".y"), data = df), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  v <- vif_unweighted(fit)
  if (is.null(v)) return(NULL)
  out <- data.frame(variable = orig[match(v$term, safe)], VIF = v$graded, Df = v$Df, stringsAsFactors = FALSE)
  out$R2 <- ifelse(out$Df == 1, 1 - 1 / out$VIF, NA_real_)
  out <- out[order(-out$VIF, out$variable), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "n") <- nrow(df); attr(out, "logged") <- logged
  out
}

zmpe_mupe_start <- function(X, y, mode) {
  is_ll <- identical(mode, "loglin")
  b <- tryCatch(.lm.fit(X, if (is_ll) log(y) else y)$coefficients, error = function(e) NULL)
  if (is.null(b)) return(NULL)
  b[!is.finite(b)] <- 0
  XtX_U <- if (is_ll) tryCatch(chol(crossprod(X)), error = function(e) NULL) else NULL
  for (k in 1:50) {
    if (is_ll) {
      s <- y / exp(drop(X %*% b)) - 1
      g <- crossprod(X, s)
      d <- if (!is.null(XtX_U)) drop(backsolve(XtX_U, forwardsolve(XtX_U, g, upper.tri = TRUE, transpose = TRUE)))
           else tryCatch(drop(solve(crossprod(X), g)), error = function(e) NULL)
      if (is.null(d) || any(!is.finite(d))) break
      bn <- b + d
    } else {
      yh <- drop(X %*% b); yh[abs(yh) < 1e-6] <- 1e-6
      sw <- 1 / abs(yh)
      bn <- tryCatch(.lm.fit(X * sw, y * sw)$coefficients, error = function(e) NULL)
      if (is.null(bn) || any(!is.finite(bn))) break
    }
    done <- max(abs(bn - b) / pmax(abs(b), 1)) < 1e-6; b <- bn
    if (done) break
  }
  b
}
zmpe_solve <- function(X, y, mode, start = NULL, constrained = TRUE, maxit = 1000L, full_rank = FALSE) {
  kc <- if (full_rank) seq_len(ncol(X)) else fullrank_cols(X)
  if (length(kc) < ncol(X)) {
    st <- if (!is.null(start)) { s0 <- start[kc]; if (any(!is.finite(s0))) NULL else s0 } else NULL
    b  <- zmpe_solve(X[, kc, drop = FALSE], y, mode, st, constrained, maxit)
    if (is.null(b)) return(NULL)
    out <- rep(NA_real_, ncol(X)); out[kc] <- b
    out <- setNames(out, colnames(X))
    attr(out, "irls_converged") <- attr(b, "irls_converged"); attr(out, "irls_iterations") <- attr(b, "irls_iterations")
    return(out)
  }
  is_ll <- identical(mode, "loglin")
  pred  <- function(b) if (is_ll) exp(drop(X %*% b)) else drop(X %*% b)
  sspe  <- function(yh) sum((y / yh - 1)^2)
  proj  <- function(b) {
    yh0 <- pred(b); cc <- mean(y / yh0)
    if (!is.finite(cc) || cc <= 0) return(NULL)
    list(b = if (is_ll) { b[1] <- b[1] + log(cc); b } else b * cc, yh = yh0 * cc)
  }
  b <- start %||% zmpe_mupe_start(X, y, mode)
  if (is.null(b) || any(!is.finite(b))) b <- tryCatch(.lm.fit(X, if (is_ll) log(y) else y)$coefficients, error = function(e) NULL)
  if (is.null(b)) return(NULL)
  b[!is.finite(b)] <- 0
  if (constrained && is_ll && !identical(colnames(X)[1], "(Intercept)")) constrained <- FALSE
  yh <- NULL
  if (constrained) { bp <- proj(b); if (is.null(bp)) constrained <- FALSE else { b <- bp$b; yh <- bp$yh } }
  if (is.null(yh)) yh <- pred(b)
  pos <- all(is.finite(yh) & yh > 0)
  ok  <- function(yh) all(is.finite(yh)) && all(abs(yh) > 1e-12) && (!pos || all(yh > 0))
  if (!ok(yh)) return(b)
  f <- sspe(yh)
  it <- 0L; hit_cap <- TRUE
  for (it in seq_len(maxit)) {
    u <- y / yh; r <- u - 1
    if (!constrained) {
      J <- if (is_ll) -u * X else -(u / yh) * X
    } else if (is_ll) {
      if (ncol(X) < 2L) break
      Xs <- X[, -1L, drop = FALSE]
      J  <- -u * (Xs - rep(colSums(u * Xs) / sum(u), each = nrow(Xs)))
    } else {
      Z <- X / yh
      J <- -u * (Z - rep(colSums(u * Z) / sum(u), each = nrow(Z)))
    }
    d <- if (constrained && !is_ll) tryCatch({
      cb <- sqrt(sum(J^2) / ncol(J)) / max(sqrt(sum(b^2)), 1e-300)
      base::qr.coef(base::qr(rbind(J, cb * b)), c(-r, 0))
    }, error = function(e) NULL) else tryCatch(base::qr.coef(base::qr(J), -r), error = function(e) NULL)
    if (is.null(d)) break
    d[!is.finite(d)] <- 0
    bd   <- if (constrained && is_ll) b[-1L] else b
    tiny <- max(abs(d) / pmax(abs(bd), 1)) < 1e-10
    step <- 1; moved <- FALSE
    for (h in 1:40) {
      if (!constrained) { bn <- b + step * d; yn <- pred(bn) } else {
        pj <- if (is_ll) proj(c(b[1], b[-1] + step * d)) else proj(b + step * d)
        if (is.null(pj)) { step <- step / 2; next }
        bn <- pj$b; yn <- pj$yh
      }
      if (ok(yn)) { fn <- sspe(yn)
        if (is.finite(fn) && (fn < f || (h == 1L && fn <= f * (1 + 1e-12)))) { moved <- TRUE; break } }
      step <- step / 2
    }
    if (!moved) { hit_cap <- FALSE; break }
    b <- bn; yh <- yn; f <- fn
    if (tiny || max(abs(step * d)) < 1e-13) { hit_cap <- FALSE; break }
  }
  if (it < maxit) hit_cap <- FALSE
  attr(b, "irls_converged") <- !hit_cap; attr(b, "irls_iterations") <- it
  b
}

nlls_pi <- function(formula, fit, newdata, alpha = 0.05) {
  yhat <- predict(fit, newdata)
  z0   <- matrix(
    get_derivative(formula, newdata, coef(fit)),
    nrow = nrow(newdata), ncol = length(coef(fit))
  )

  varcov <- tryCatch(
    vcov(fit),
    error = function(e) {
      sigma(fit)^2 * scaled_ginv(t(z0) %*% z0)
    }
  )

  DoF          <- length(residuals(fit)) - length(coef(fit))
  t_multiplier <- qt(1 - alpha / 2, max(DoF, 1))

  result <- matrix(NA_real_, nrow = nrow(newdata), ncol = 3,
                   dimnames = list(NULL, c("fit", "lwr", "upr")))
  for (i in seq_len(nrow(result))) {
    adj_se      <- sqrt(sigma(fit)^2 + as.numeric(z0[i, ] %*% varcov %*% z0[i, ]))
    moe         <- t_multiplier * adj_se
    result[i, ] <- c(yhat[i], yhat[i] - moe, yhat[i] + moe)
  }
  result
}

mupe_pi <- function(formula, fit, newdata, alpha = 0.05) {
  yhat <- predict(fit, newdata)
  z0   <- matrix(
    get_derivative(formula, newdata, coef(fit)),
    nrow = nrow(newdata), ncol = length(coef(fit))
  )

  f_hat   <- as.numeric(fitted(fit))
  pct_res <- as.numeric(residuals(fit)) / f_hat
  DoF     <- length(pct_res) - length(coef(fit))
  sig_pct <- sqrt(sum(pct_res^2, na.rm = TRUE) / max(DoF, 1))

  varcov <- tryCatch(
    vcov(fit),
    error = function(e) {
      sig_pct^2 * scaled_ginv(t(z0) %*% z0)
    }
  )

  t_multiplier <- qt(1 - alpha / 2, max(DoF, 1))

  result <- matrix(NA_real_, nrow = nrow(newdata), ncol = 3,
                   dimnames = list(NULL, c("fit", "lwr", "upr")))
  for (i in seq_len(nrow(result))) {
    adj_se      <- sqrt(sig_pct^2 * yhat[i]^2 + as.numeric(z0[i, ] %*% varcov %*% z0[i, ]))
    moe         <- t_multiplier * adj_se
    result[i, ] <- c(yhat[i], yhat[i] - moe, yhat[i] + moe)
  }
  result
}

N_CORES <- min(4L, max(1L, parallel::detectCores(logical = FALSE) - 1L))

validate_nl_custom_eq <- function(expr_text) {
  parsed <- tryCatch(parse(text = expr_text), error = function(e) NULL)
  if (is.null(parsed)) return("Invalid equation syntax.")

  blocked <- c(
    "system", "system2", "shell", "shell.exec",
    "eval", "evalq", "eval.parent",
    ".Call", ".External", ".Internal", ".Primitive",
    "source", "parse",
    "file", "connection", "readLines", "writeLines", "readRDS", "saveRDS",
    "download.file", "url", "pipe", "socketConnection",
    "library", "require", "requireNamespace", "install.packages",
    "Sys.setenv", "Sys.getenv", "Sys.setwd", "setwd",
    "assign", "rm", "remove", "unlink", "file.remove",
    "do.call", "match.fun"
  )

  find_blocked <- function(x) {
    if (is.call(x)) {
      fn <- tryCatch(as.character(x[[1]]), error = function(e) "")
      if (any(fn %in% blocked)) return(fn[fn %in% blocked][1])
      for (i in seq_along(x)) {
        hit <- find_blocked(x[[i]])
        if (!is.null(hit)) return(hit)
      }
    } else if (is.recursive(x)) {
      for (i in seq_along(x)) {
        hit <- find_blocked(x[[i]])
        if (!is.null(hit)) return(hit)
      }
    }
    NULL
  }

  for (expr in parsed) {
    hit <- find_blocked(expr)
    if (!is.null(hit))
      return(paste0("Function '", hit, "' is not permitted in custom equations."))
  }
  NULL
}

ceres_mark("app top level done")
server <- function(input, output, session) {
  ceres_mark("session start")
  if (CERES_IN_BROWSER) observeEvent(input$ceres_pool_off, options(ceres.pool = !isTRUE(input$ceres_pool_off)), ignoreInit = TRUE)
  if (CERES_IN_BROWSER) observeEvent(input$ceres_prerender_capture, {
    session$sendCustomMessage("ceres_prerender_capture",
                              list(app_md5 = unname(tools::md5sum(file.path(CERES_APP_DIR, "app.R"))),
                                   paths = ceres_resource_map()))
  }, ignoreInit = TRUE)

  cer_source_cols <- function() {
    tryCatch(names(isolate(rv$dataset_library[[isolate(input$data_select_cer_generation) %||% ""]])),
             error = function(e) character(0))
  }

  session$userData$plotlyShinyEventIDs <- unique(c(session$userData$plotlyShinyEventIDs,
    paste0("plotly_click-", c("loglin_map_src", "linear_map_src", "loglin_tuning_src",
                              "loglin_exclusions_src", "linear_tuning_src", "linear_exclusions_src"))))

  DISTRIBUTION_TRUNCATION_LOW  <- 0.001
  DISTRIBUTION_TRUNCATION_HIGH <- 0.999
  OLS_HOLDOUT_CHOICES <- c(
    "No (Train on all data)",
    "Yes (Holdout 20% for Testing)",
    "Yes (Holdout 50% for Testing)",
    "Yes (Manually Select Test Set)",
    "Yes (5-Fold Cross-Validation)",
    "Yes (10-Fold Cross-Validation)",
    "Yes (Leave-One-Out CV)"
  )

  expand_categorical_to_boolean <- function(data, col, remove_original = FALSE) {
    stopifnot(
      is.data.frame(data),
      is.character(col), length(col) == 1,
      col %in% names(data),
      is.factor(data[[col]])
    )
    
    parent_base   <- sub("_$", "", col)
    data[[col]]   <- droplevels(data[[col]])
    lvls          <- levels(data[[col]])
    
    taken <- character(0)
    for (lev in lvls) {
      safe_lev        <- gsub("[^A-Za-z0-9]", "_", lev)
      new_col         <- paste0(parent_base, "_", safe_lev, "_")
      k <- 1L
      while (new_col %in% taken) {
        k <- k + 1L
        new_col <- paste0(parent_base, "_", safe_lev, "_", k, "_")
      }
      taken <- c(taken, new_col)
      raw             <- ifelse(is.na(data[[col]]), NA_integer_, as.integer(data[[col]] == lev))
      data[[new_col]] <- as.factor(raw)
    }
    
    if (remove_original) data[[col]] <- NULL
    data
  }
  
  
  
  primary_color <- reactive({
    if (isTRUE(input$dark_mode)) {
      dark_primary_color_passthrough 
    } else {
      primary_color_passthrough
    }
  })
  
  secondary_color  <- reactive({
    if (isTRUE(input$dark_mode)) {
      dark_secondary_color_passthrough
    } else {
      secondary_color_passthrough 
    }
  }) 
  
  
  tertiary_color   <- reactive({
    if (isTRUE(input$dark_mode)) {
      dark_tertiary_color_passthrough
    } else {
      tertiary_color_passthrough
    }
  })

  observe({
    color <- primary_color()
    shinyjs::runjs(sprintf(
      "var el = document.getElementById('ceres-spinner-css');
       if (!el) { el = document.createElement('style'); el.id = 'ceres-spinner-css'; document.head.appendChild(el); }
       el.textContent = '.load1 .loader, .load1 .loader:before, .load1 .loader:after { background: %s !important; color: %s !important; }';",
      color, color
    ))
  })

  session$onFlushed(function() {
    shinyjs::runjs("
      (function(){
        function colorFittingBar(){
          document.querySelectorAll('.shiny-progress-notification').forEach(function(p){
            var msg = p.querySelector('.progress-message');
            var bar = p.querySelector('.progress-bar');
            if(bar && msg && /(Optimizing|Fitting)/.test(msg.textContent)){
              bar.classList.add('progress-bar-striped','progress-bar-animated');
            } else if(bar){
              bar.classList.remove('progress-bar-striped','progress-bar-animated');
            }
          });
        }
        var mo = new MutationObserver(colorFittingBar);
        mo.observe(document.body, {childList:true, subtree:true, characterData:true});
      })();
    ")
  }, once = TRUE)

  rv <- reactiveValues(dataset_library = list(), active_dataset_key = NULL, categorical_data = NULL, numeric_data = NULL, row_names = NULL, events = NULL, evidence = NULL,
                       loglin_influence_data  = NULL,
                       loglin_influence_n_par = NULL,
                       loglin_ui_exclusions   = character(0),
                       loglin_error_type      = NULL,
                       linear_error_type      = NULL,
                       linear_influence_data  = NULL,
                       linear_influence_n_par = NULL,
                       linear_ui_exclusions   = character(0),
                       explore_loglin_influence_data  = NULL,
                       explore_loglin_influence_n_par = NULL,
                       explore_loglin_ui_exclusions   = character(0),
                       explore_linear_influence_data  = NULL,
                       explore_linear_influence_n_par = NULL,
                       explore_linear_ui_exclusions   = character(0),
                       nl_custom_fitted           = FALSE,
                       nl_custom_show_startvals   = FALSE,
                       nl_custom_eq_text          = "",
                       nl_custom_fitted_eq        = "",
                       nl_builder_render_token    = 0L,
                       linear_simple_running      = FALSE,
                       loglin_running             = FALSE,
                       bn_result               = NULL)
  
  observeEvent(input$navTo, {
    if (input$navTo == "ExploreData") {
      updateNavbarPage(session, "tabs", selected = "Explore Data")
      updateTabsetPanel(session, "data_tabs", selected = "Data")
    }
  })

  clear_dataset_key_state <- function(key) {
    if (is.null(key) || !nzchar(key)) return(invisible(NULL))
    rv$log_transformed_by_key[[key]] <- NULL
    if (!is.null(rv$repair_logs)) rv$repair_logs[[key]] <- NULL
    memo_keys <- ls(log_notice_memo)
    rm(list = memo_keys[startsWith(memo_keys, paste0(key, "\x1f"))],
       envir = log_notice_memo)
    invisible(NULL)
  }

  observeEvent(input$data_upload, {

    inFile <- input$data_upload
    if (is.null(inFile)){
      return(NULL)
    }

    MAX_FILE_SIZE <- 50 * 1024 * 1024
    if (inFile$size > MAX_FILE_SIZE) {
      showNotification("File too large (max 50 MB). Please upload a smaller file.", type = "error")
      return(NULL)
    }

    sheet <- NULL
    rv$upload_xl <- NULL
    if (tools::file_ext(inFile$name) %in% c("xls", "xlsx", "xlsm")) {
      sheets <- tryCatch(readxl::excel_sheets(inFile$datapath), error = function(e) character(0))
      if (length(sheets) > 1L) {
        sheet <- excel_default_sheet(inFile$datapath, sheets)
        rv$upload_xl <- list(file = inFile, sheets = sheets, loaded = sheet)
      }
    }
    ok <- ingest_upload(inFile, sheet)
    if (!is.null(rv$upload_xl)) {
      if (!isTRUE(ok)) {
        rv$upload_xl$loaded <- NA_character_
        showNotification(paste0("No sheet of ", inFile$name, " was loaded - none reads as a data table ",
                                "(a header row and data rows). Pick a sheet under Excel Sheet."),
                         type = "warning", duration = 10)
      } else if (!identical(sheet, rv$upload_xl$sheets[1L])) {
        showNotification(paste0("Loaded sheet '", sheet, "' - the first sheet ('", rv$upload_xl$sheets[1L],
                                "') is not a data table. Choose another sheet under Excel Sheet."),
                         type = "message", duration = 8)
      }
    }
  })

  output$data_upload_sheet_ui <- renderUI({
    x <- rv$upload_xl
    if (is.null(x)) return(NULL)
    if (is.na(x$loaded))
      selectInput("data_upload_sheet", "Excel Sheet", choices = c("(no sheet loaded)" = "", x$sheets), selected = "")
    else selectInput("data_upload_sheet", "Excel Sheet", choices = x$sheets, selected = x$loaded)
  })

  observeEvent(input$data_upload_sheet, {
    x  <- rv$upload_xl
    sh <- input$data_upload_sheet
    req(x, sh %in% x$sheets)
    if (identical(sh, x$loaded)) return(invisible(NULL))
    if (isTRUE(ingest_upload(x$file, sh))) {
      rv$upload_xl$loaded <- sh
      showNotification(paste0("Loaded sheet '", sh, "' from ", x$file$name, "."), type = "message", duration = 4)
    } else {
      updateSelectInput(session, "data_upload_sheet", selected = if (is.na(x$loaded)) "" else x$loaded)
    }
  }, ignoreInit = TRUE)

  upload_stage <- function(pct, text = NULL, reset = FALSE)
    session$sendCustomMessage("ceres_upload_stage", list(pct = pct, text = text, reset = reset))

  ingest_upload <- function(inFile, sheet = NULL) {

    upload_stage(45, "Reading file...")

    file_ext <- tools::file_ext(inFile$name)

    upload_stage(55, "Loading file...")
    if (file_ext == "csv") {
      raw_data <- tryCatch(
        read.csv(inFile$datapath, check.names = FALSE, na.strings = c("", "NA", "N/A", "n/a", "na", "#N/A")),
        error = function(e) NULL)
    } else if (file_ext %in% c("xls", "xlsx", "xlsm")) {
      raw_data <- tryCatch(
        suppressMessages(
          read_excel(inFile$datapath, sheet = sheet,
                     na = c("", "NA", "N/A", "n/a", "na", "#N/A", "-"))),
        error = function(e) NULL)
    } else {
      upload_stage(0, reset = TRUE)
      showNotification("Unsupported file type. Please upload a CSV or Excel file.", type = "error")
      return(FALSE)
    }

    upload_stage(65, "Processing data...")
    if (!is.null(sheet) && !is.null(raw_data) && !excel_sheet_is_table(inFile$datapath, sheet)) raw_data <- NULL
    if (is.null(raw_data) || ncol(as.data.frame(raw_data)) == 0 || nrow(as.data.frame(raw_data)) == 0) {
      upload_stage(0, reset = TRUE)
      showNotification(if (is.null(sheet))
                         "The uploaded file is empty or could not be read (no data rows/columns found)."
                       else paste0("Sheet '", sheet, "' of ", inFile$name,
                                   " could not be read as a data table (no header row with data rows found)."),
                       type = "error", duration = 8)
      return(FALSE)
    }
    data_raw <- data.frame(raw_data, check.names = FALSE)

    upload_stage(80, "Detecting row names...")
    rn_ok <- !is.numeric(data_raw[,1]) && !anyNA(data_raw[,1]) &&
      length(unique(data_raw[,1])) == nrow(data_raw) && ncol(data_raw) > 1L
    if (rn_ok) {
      rn_data <- tryCatch({
        d <- data_raw[, -1, drop = FALSE]
        row.names(d) <- data_raw[, 1]
        d
      }, error = function(e) NULL)
      rn_ok <- !is.null(rn_data)
    }
    if (rn_ok) {
      rv$row_names <- data_raw[,1]
      data_raw <- rn_data
    } else {
      rv$row_names <- seq_len(nrow(data_raw))
      row.names(data_raw) <- rv$row_names
    }

    file_name          <- inFile$name
    clear_dataset_key_state(file_name)
    is_num_raw         <- vapply(data_raw, is.numeric, logical(1L))
    rv$dataset_library[[file_name]] <- data_raw
    rv$active_dataset               <- data_raw
    rv$active_dataset_key           <- file_name
    rv$numeric_data                 <- colnames(data_raw)[is_num_raw]
    rv$categorical_data             <- colnames(data_raw)[!is_num_raw]
    rv$vars_with_missing_data       <- colnames(data_raw)[colSums(is.na(data_raw)) > 0L]
    update_all_dataset_selectors(file_name)

    upload_stage(100, "Upload complete")

    later::later(function() {
      shiny::withReactiveDomain(session, isolate({
        data       <- data_raw
        is_num     <- vapply(data, is.numeric, logical(1L))
        is_bin_num <- is_num & vapply(data, function(x) all(x %in% c(0, 1, NA)), logical(1L))
        to_factor  <- which(is_bin_num | !is_num)
        for (i in to_factor) data[[i]] <- as.factor(data[[i]])
        cat_idx   <- which(vapply(data, is.factor, logical(1L)))
        cat_names <- names(data)[cat_idx]
        needs_sfx <- !endsWith(cat_names, "_")
        names(data)[cat_idx[needs_sfx]] <- paste0(cat_names[needs_sfx], "_")
        if (anyDuplicated(names(data))) names(data) <- make.unique(names(data), sep = "_")

        rv$dataset_library[[file_name]] <- data
        rv$active_dataset               <- data
        rv$active_dataset_key           <- file_name
        rv$numeric_data                 <- colnames(data)[is_num & !is_bin_num]
        rv$categorical_data             <- colnames(data)[is_bin_num | !is_num]
        rv$vars_with_missing_data       <- colnames(data)[colSums(is.na(data)) > 0L]
        rv$impute                       <- rep("None", length(rv$vars_with_missing_data))

        update_missing_selections()
        update_transform_selections()
        update_boxplot_selections()
        update_scatterplot_selections()
        update_cer_generation_selections(key = file_name)
        num_cols <- rv$numeric_data
        all_cols <- colnames(data)
        updateSelectInput(session, "explore_infl_response",
                          choices = num_cols,
                          selected = if (length(num_cols) > 0L) num_cols[1L] else NULL)
        updateSelectInput(session, "explore_infl_predictors",
                          choices = setdiff(all_cols, if (length(num_cols) > 0L) num_cols[1L] else character(0)),
                          selected = NULL)
        updateTextInput(session, "explore_infl_save_name",
                        value = paste0(file_name, "_excl"))
      }))
    })
    TRUE
  }

  output$has_multiple_datasets <- reactive({ length(rv$dataset_library) > 1L })
  outputOptions(output, "has_multiple_datasets", suspendWhenHidden = FALSE)

  modal_cache <- new.env()
  result_window_opt_ph <- '<option value="__CERES_RP__" selected>__CERES_RP__</option>'
  result_window_nb_ph  <- 'data-from="47"'
  ceres_result_window_cached <- function(kind, build) {
    key <- paste(kind, isolate(primary_color()))
    hit <- get0(key, envir = modal_cache, inherits = FALSE)
    if (!is.null(hit)) return(hit)
    res <- shiny:::processDeps(build("__CERES_RP__", 47L), session)
    n_count <- function(p) lengths(regmatches(res$html, gregexpr(p, res$html, fixed = TRUE)))
    if (n_count(result_window_opt_ph) != 1L || n_count(result_window_nb_ph) != 1L) return(NULL)
    assign(key, res, envir = modal_cache)
    res
  }
  ceres_show_result_window <- function(kind, build, model_vars, nbins) {
    if (isTRUE(get0("CERES_APPLE", inherits = TRUE))) return(showModal(build(model_vars, nbins)))
    hit <- ceres_result_window_cached(kind, build)
    if (is.null(hit)) return(showModal(build(model_vars, nbins)))
    mv <- as.character(model_vars)
    opts <- if (length(mv)) paste0('<option value="', htmltools::htmlEscape(mv, TRUE), '"', ifelse(seq_along(mv) == 1L, " selected", ""),
                                   '>', htmltools::htmlEscape(mv), '</option>', collapse = "\n") else ""
    html <- sub(result_window_opt_ph, opts, hit$html, fixed = TRUE)
    html <- sub(result_window_nb_ph, paste0('data-from="', as.integer(nbins), '"'), html, fixed = TRUE)
    session$sendModal("show", list(html = html, deps = hit$deps))
  }
  ceres_warm_result_window <- function(kind, build) {
    if (isTRUE(get0("CERES_APPLE", inherits = TRUE))) return(invisible())
    later::later(function() try(shiny::withReactiveDomain(session, isolate(ceres_result_window_cached(kind, build))), silent = TRUE), 0.3)
  }

  prefetch_on <- new.env()
  observeEvent(input$ceres_prefetch, {
    m <- input$ceres_prefetch
    if (identical(m$op, "one")) {
      id <- as.character(m$id)[1]
      if (is.na(id) || exists(id, envir = prefetch_on, inherits = FALSE)) return()
      cur <- tryCatch(outputOptions(output, id), error = function(e) NULL)
      if (is.null(cur) || isFALSE(cur$suspendWhenHidden)) return()
      outputOptions(output, id, suspendWhenHidden = FALSE)
      assign(id, TRUE, envir = prefetch_on)
    } else if (identical(m$op, "once")) {
      id <- as.character(m$id)[1]
      if (is.na(id) || exists(id, envir = prefetch_on, inherits = FALSE)) return()
      cur <- tryCatch(outputOptions(output, id), error = function(e) NULL)
      if (is.null(cur) || isFALSE(cur$suspendWhenHidden)) return()
      outputOptions(output, id, suspendWhenHidden = FALSE)
      session$onFlushed(function() {
        if (!exists(id, envir = prefetch_on, inherits = FALSE))
          try(outputOptions(output, id, suspendWhenHidden = TRUE), silent = TRUE)
      }, once = TRUE)
    } else if (identical(m$op, "stale")) {
      ids  <- as.character(unlist(m$ids))
      outs <- tryCatch(session$.__enclos_env__$private$.outputs, error = function(e) NULL)
      stale <- if (is.null(outs) || !length(ids)) ids else ids[vapply(ids, function(id) {
        o <- outs[[id]]
        if (is.null(o)) return(FALSE)
        r <- tryCatch(isTRUE(o$.suspended) && !identical(body(o$.onResume), quote(NULL)), error = function(e) TRUE)
        isTRUE(r)
      }, logical(1))]
      session$sendCustomMessage("ceres_prefetch_stale", list(ids = I(stale), n = m$n))
    } else if (identical(m$op, "close")) {
      for (id in ls(prefetch_on)) try(outputOptions(output, id, suspendWhenHidden = TRUE), silent = TRUE)
      rm(list = ls(prefetch_on), envir = prefetch_on)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$remove_dataset_btn, {
    nm <- input$data_select
    req(nm, nzchar(nm), nm %in% names(rv$dataset_library))
    if (length(rv$dataset_library) <= 1L) {
      showNotification("Cannot remove the only dataset in the session.", type = "warning")
      return(NULL)
    }
    showModal(modalDialog(
      title = "Remove Dataset",
      paste0("Remove '", nm, "' from this session? This only frees in-app memory — the ",
             "original file is untouched. Models already fit from it keep ",
             "their results."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("remove_dataset_confirm", "Remove", class = "btn-danger")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$remove_dataset_confirm, {
    removeModal()
    nm <- input$data_select
    req(nm, nzchar(nm), nm %in% names(rv$dataset_library))
    if (length(rv$dataset_library) <= 1L) return(NULL)
    rv$dataset_library[[nm]] <- NULL
    clear_dataset_key_state(nm)
    target <- names(rv$dataset_library)[1L]
    rv$active_dataset_key <- NULL
    sync_active_dataset(target)
    choices <- names(rv$dataset_library)
    for (sel_id in c("data_select", "data_select_cer_generation")) {
      cur <- input[[sel_id]]
      updateSelectInput(session, sel_id, choices = choices,
                        selected = if (!is.null(cur) && cur %in% choices) cur else target)
    }
    showNotification(paste0("Removed '", nm, "' from the session."), type = "message", duration = 4)
  })
  
  sync_active_dataset <- function(key) {
    if (is.null(key) || !nzchar(key)) return(invisible(NULL))
    if (identical(key, isolate(rv$active_dataset_key))) return(invisible(NULL))
    new_data <- isolate(rv$dataset_library[[key]])
    if (is.null(new_data)) return(invisible(NULL))
    rv$active_dataset     <- new_data
    rv$active_dataset_key <- key
    is_num_sync           <- vapply(new_data, is.numeric, logical(1L))
    rv$numeric_data       <- colnames(new_data)[is_num_sync]
    rv$categorical_data   <- colnames(new_data)[!is_num_sync]
    rv$vars_with_missing_data <- colnames(new_data)[colSums(is.na(new_data)) > 0L]
    rv$row_names <- row.names(new_data)
    later::later(function() {
      shiny::withReactiveDomain(session, isolate({
        update_cer_generation_selections()
      }))
    })
  }

  refresh_explore_selections <- function() {
    later::later(function() {
      shiny::withReactiveDomain(session, isolate({
        key <- input$data_select
        if (is.null(key) || is.null(rv$dataset_library[[key]])) return(invisible(NULL))
        sync_active_dataset(key)
        rv$impute <- rep("None", length(rv$vars_with_missing_data))
        update_missing_selections();  update_transform_selections()
        update_boxplot_selections();  update_scatterplot_selections()
      }))
    })
  }
  observeEvent(input$data_select, refresh_explore_selections())

  store_dataset <- function(name, data) {
    clear_dataset_key_state(name)
    rv$dataset_library[[name]] <- data
    if (identical(name, rv$active_dataset_key) || identical(name, input$data_select)) {
      rv$active_dataset_key <- NULL
      sync_active_dataset(name)
    }
    if (identical(name, input$data_select)) refresh_explore_selections()
    if (identical(name, input$data_select_cer_generation)) update_cer_generation_selections(name)
    invisible(NULL)
  }

  get_data                 <- reactive({ sync_active_dataset(input$data_select); rv$active_dataset_key; req(rv$active_dataset) })
  sync_cer_generation_data <- reactive({ sync_active_dataset(input$data_select_cer_generation); rv$active_dataset_key })

  log_notice_memo <- new.env(parent = emptyenv())
  notify_log_once <- function(var, family, msg) {
    key <- paste(rv$active_dataset_key %||% "", var, family, sep = "\x1f")
    if (isTRUE(log_notice_memo[[key]])) return(invisible(NULL))
    log_notice_memo[[key]] <- TRUE
    showNotification(msg, type = "warning", duration = 6)
  }

  update_all_dataset_selectors <- function(selected) {
    choices <- names(rv$dataset_library)
    for (sel_id in c("data_select", "data_select_cer_generation"))
      updateSelectInput(session, sel_id, choices = choices, selected = selected)
  }

  observeEvent(input$data_select, {
    data <- rv$dataset_library[[input$data_select]]
    req(!is.null(data))
    is_num   <- vapply(data, is.numeric, logical(1L))
    num_cols <- colnames(data)[is_num]
    all_cols <- colnames(data)
    updateSelectInput(session, "explore_infl_response",   choices = num_cols, selected = if (length(num_cols) > 0L) num_cols[1L] else NULL)
    updateSelectInput(session, "explore_infl_predictors",
                      choices = setdiff(all_cols, if (length(num_cols) > 0L) num_cols[1L] else character(0)),
                      selected = NULL)
    updateTextInput(session,   "explore_infl_save_name",  value   = paste0(input$data_select, "_excl"))
    updateSelectInput(session, "vifset_vars",
                      choices = all_cols[vapply(data, function(x) is.numeric(x) || is.factor(x) || is.character(x), logical(1))],
                      selected = character(0))
    rv$explore_loglin_influence_data  <- NULL
    rv$explore_loglin_influence_n_par <- NULL
    rv$explore_loglin_ui_exclusions   <- character(0)
    rv$explore_linear_influence_data  <- NULL
    rv$explore_linear_influence_n_par <- NULL
    rv$explore_linear_ui_exclusions   <- character(0)
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  session_state <- new.env(parent = emptyenv())
  session_state$pending   <- list()
  session_state$models    <- list()
  session_state$rows      <- list()
  session_state$explore   <- NULL
  session_state$restoring <- FALSE
  session_state$settled   <- TRUE
  session_state$last_run  <- NULL
  session_kick <- reactiveVal(0L)

  session_run_buttons <- c(loglin_train = "Log Linear", linear_simple_train = "Linear",
                           ols_train = "OLS", nl_train = "Non-Linear")
  for (b in names(session_run_buttons)) local({
    btn <- b
    observeEvent(input[[btn]], { session_state$last_run <- btn }, ignoreInit = TRUE)
  })

  session_column_selectors <- c("ols_response", "ols_predictors", "explore_infl_response",
    "explore_infl_predictors", "selected_missing", "log_transform_vars_selected",
    "sqrt_transform_vars_selected", "inverse_transform_vars_selected", "subset_var_selected",
    "interact1_var_selected", "interact2_var_selected", "interact_response_var",
    "expand_cat_var_selected", "selected_box", "cat_selected_box", "x_selected", "y_selected",
    "scatter_color_var", "bn_included_vars")

  session_input_tier <- function(id) {
    if (id %in% c("data_select", "data_select_cer_generation")) return(1L)
    if (id %in% c("ols_response", "nl_equation", "nl_response", "subset_var_selected",
                  "explore_infl_response", "common_impute", "tabs")) return(2L)
    if (id %in% c("ols_penalty", "subset_var_selected_level", "nl_predictors", "nl_custom_eq")) return(4L)
    3L
  }

  session_fit_spec <- function(kind) {
    lin <- identical(kind, "linear")
    list(dataset    = as.character(input$data_select_cer_generation %||% ""),
         response   = as.character(input$ols_response %||% ""),
         predictors = as.character(input$ols_predictors %||% character(0)),
         holdout    = as.character(input$ols_holdout %||% ""),
         error      = as.character((if (lin) input$linear_error else input$log_linear_error) %||% ""),
         interact   = isTRUE(if (lin) input$linear_simple_interact else input$loglin_interact),
         leaps      = isTRUE(if (lin) input$linear_use_leaps else input$loglin_use_leaps))
  }
  session_same_spec <- function(a, b) {
    norm <- function(s) lapply(s[c("dataset", "response", "predictors", "holdout", "error", "interact", "leaps")],
                               function(v) as.character(unlist(v)))
    identical(norm(a), norm(b))
  }

  session_collect_state <- function() isolate({
    vals <- reactiveValuesToList(input)
    keep <- vapply(names(vals), function(id) ceres_session_keep_input(id, vals[[id]]), logical(1L))
    inputs <- lapply(vals[keep], function(v) I(if (is.null(v)) character(0) else v))

    model_bundle <- function(kind) {
      prefix  <- kind
      tbl     <- if (kind == "loglin") "selected_loglin_models" else "selected_linear_simple_models"
      list_k  <- if (kind == "loglin") "model_list_loglin" else "model_list_linear_simple"
      filt    <- if (kind == "loglin") filtered_log_linear_models_data else filtered_linear_models_data_second
      by      <- rv[[paste0(prefix, "_ui_exclusions_by_model")]] %||% list()
      cur_mod <- rv[[paste0(prefix, "_ui_exclusions_last_model")]]
      if (!is.null(cur_mod) && !is.na(cur_mod) && nzchar(cur_mod))
        by[[cur_mod]] <- rv[[paste0(prefix, "_ui_exclusions")]] %||% character(0)
      by <- by[lengths(by) > 0L]
      rs <- input[[paste0(tbl, "_rows_selected")]]
      selected <- NULL
      if (length(rs)) {
        fdf <- tryCatch(filt(), error = function(e) NULL)
        if (!is.null(fdf) && nrow(fdf) >= rs[1L]) selected <- rownames(fdf)[rs[1L]]
      }
      if (is.null(selected) && !is.null(rv[[list_k]]) && !is.null(cur_mod) && !is.na(cur_mod) && nzchar(cur_mod))
        selected <- cur_mod
      list(has_results    = !is.null(rv[[list_k]]),
           spec           = lapply(session_fit_spec(kind), I),
           selected_model = selected,
           pushed         = !is.null(rv[[paste0(prefix, "_tuning_pushed_model")]]),
           exclusions     = lapply(by, I))
    }

    list(
      theme    = if (isTRUE(input$dark_mode)) "dark" else "light",
      last_run = session_state$last_run,
      inputs   = inputs,
      models   = list(loglin = model_bundle("loglin"), linear = model_bundle("linear")),
      measures = list(loglin = input$loglin_measures_state$ui, linear = input$linear_measures_state$ui),
      explore_outliers = list(
        dataset = input$data_select,
        loglin  = I(rv$explore_loglin_ui_exclusions %||% character(0)),
        linear  = I(rv$explore_linear_ui_exclusions %||% character(0)),
        save_name = input$explore_infl_save_name),
      values = list(
        manual_test_set        = I(as.integer(rv$manual_test_set %||% integer(0))),
        log_transformed_by_key = lapply(rv$log_transformed_by_key %||% list(), I),
        repair_logs            = rv$repair_logs %||% list(),
        impute                 = list(dataset = rv$active_dataset_key,
                                      vars    = I(rv$vars_with_missing_data %||% character(0)),
                                      methods = I(as.character(rv$impute %||% character(0)))),
        nl_custom_eq_text      = rv$nl_custom_eq_text %||% "")
    )
  })

  output$session_save <- downloadHandler(
    filename = function() {
      base <- isolate(input$data_select)
      base <- if (length(base) == 1L && nzchar(base) && base %in% names(isolate(rv$dataset_library)))
        tools::file_path_sans_ext(base) else "CERES"
      base <- trimws(gsub("[\\\\/:*?\"<>|]+", "_", base))
      paste0(base, " session ", format(Sys.time(), "%Y-%m-%d %H%M"), ".ceres.json")
    },
    content = function(file) {
      lib <- isolate(rv$dataset_library) %||% list()
      txt <- ceres_session_serialize(
        lib,
        active = isolate(list(explore = input$data_select, cer_generation = input$data_select_cer_generation)),
        state = session_collect_state(),
        ceres_version = ceres_session_version,
        provenance = isolate(rv$dataset_origin))
      bytes <- charToRaw(enc2utf8(txt))
      con <- file(file, open = "wb")
      on.exit(close(con), add = TRUE)
      writeBin(bytes, con)
      mb    <- length(bytes) / 1024^2
      limit <- getOption("shiny.maxRequestSize", 5 * 1024^2) / 1024^2
      if (mb > limit) {
        showNotification(sprintf(paste0("This session file is %.1f MB. This CERES opens session files up to ",
                                        "%.0f MB, so it may not open here as it is."), mb, limit),
                         type = "warning", duration = 12)
      } else if (mb > 2) {
        showNotification(sprintf("Large session file (%.1f MB). It may take a few seconds to open.", mb),
                         type = "warning", duration = 8)
      }
    },
    contentType = "application/json"
  )
  outputOptions(output, "session_save", suspendWhenHidden = FALSE)

  autosave_last <- reactiveVal("")
  autosave_noted <- FALSE
  autosave_signal <- debounce(reactive({
    rv$dataset_library; rv$repair_logs; rv$log_transformed_by_key; rv$manual_test_set
    rv$model_list_loglin; rv$model_list_linear_simple; rv$ols_model; rv$nl_model
    inp <- reactiveValuesToList(input)
    inp[!grepl("^\\.clientdata|_rows_current$|_rows_all$|_state$|_cell_clicked$|^plotly_|_search$", names(inp))]
    Sys.time()
  }), 3000)
  observe({
    autosave_signal()
    if (!isTRUE(input$autosave_ready)) return()
    lib <- isolate(rv$dataset_library) %||% list()
    if (!length(lib)) return()
    txt <- tryCatch(ceres_session_serialize(
      lib,
      active = isolate(list(explore = input$data_select, cer_generation = input$data_select_cer_generation)),
      state = session_collect_state(),
      ceres_version = ceres_session_version,
      provenance = isolate(rv$dataset_origin)), error = function(e) NULL)
    if (is.null(txt)) return()
    if (nchar(txt, type = "bytes") > 25 * 1024^2) {
      if (!autosave_noted) {
        autosave_noted <<- TRUE
        showNotification("This session is too large to keep for a reload (over 25 MB). Use Session › Save to keep it.",
                         type = "message", duration = 10)
      }
      return()
    }
    if (identical(txt, isolate(autosave_last()))) return()
    autosave_last(txt)
    session$sendCustomMessage("ceres-autosave", list(text = txt, datasets = I(names(lib)),
                                                     at = as.numeric(Sys.time()) * 1000))
  })
  autosave_clear <- function() {
    autosave_last("")
    session$sendCustomMessage("ceres-autosave", list(clear = TRUE))
  }
  observeEvent(input$session_autorestore, {
    txt <- input$session_autorestore
    req(is.character(txt), nzchar(txt))
    doc <- tryCatch(ceres_session_parse(charToRaw(enc2utf8(txt))), error = function(e) {
      showNotification(paste("Could not reopen the last session.", conditionMessage(e)), type = "error", duration = 12)
      NULL
    })
    if (is.null(doc)) return()
    lib <- tryCatch(ceres_session_restore_data(doc), error = function(e) {
      showNotification(paste("Could not reopen the last session:", conditionMessage(e)), type = "error", duration = 12)
      NULL
    })
    if (is.null(lib)) return()
    session_restore(doc, lib)
    autosave_last(txt)
  })

  session_reset_analysis <- function() {
    for (s in c("ols_model", "ols_results", "ols_coeffs", "ols_table", "ols_table_second", "ols_sets",
                "ols_train_data", "ols_train_index", "ols_train_y", "ols_yhat", "ols_data_rownames",
                "lasso_test_residuals", "ridge_test_residuals", "elastic_test_residuals",
                "model_list_loglin", "predictors_loglin", "yhat_test_loglin", "y_test",
                "model_list_linear_simple", "predictors_linear_simple", "yhat_test_linear_simple", "y_test_second",
                "loglin_y_train", "loglin_x_train", "linear_y_train", "linear_x_train", "loglin_log1p_cols",
                "loglin_error_type", "linear_error_type", "train_index",
                "loglin_selected_model", "linear_selected_model",
                "loglin_influence_data", "loglin_influence_n_par", "linear_influence_data", "linear_influence_n_par",
                "loglin_ui_exclusions_by_model", "loglin_ui_exclusions_last_model", "loglin_tuning_pushed_model",
                "linear_ui_exclusions_by_model", "linear_ui_exclusions_last_model", "linear_tuning_pushed_model",
                "explore_loglin_influence_data", "explore_loglin_influence_n_par",
                "explore_linear_influence_data", "explore_linear_influence_n_par",
                "nl_model", "nl_coeffs", "nl_results", "nl_sets", "nl_cv_results", "nl_cv_details", "nl_cv_indices",
                "nl_fit_response", "nl_fit_eq_name", "nl_fit_formula", "nl_fit_log_y", "nl_fit_log_x_vars",
                "nl_fit_predictors", "nl_fit_error_term", "nl_custom_fitted_formula", "nl_custom_x_vars",
                "bn_result", "scatter_fit", "upload_xl", "impute", "manual_test_set",
                "cer_gen_last_dataset"))
      rv[[s]] <- NULL
    rv$loglin_ui_exclusions         <- character(0)
    rv$linear_ui_exclusions         <- character(0)
    rv$explore_loglin_ui_exclusions <- character(0)
    rv$explore_linear_ui_exclusions <- character(0)
    rv$nl_custom_fitted         <- FALSE
    rv$nl_custom_show_startvals <- FALSE
    rv$nl_custom_fitted_eq      <- ""
    rv$nl_custom_eq_text        <- ""
    rv$nl_builder_render_token  <- (rv$nl_builder_render_token %||% 0L) + 1L
    rv_vif$removed <- character(0)
    DT::selectRows(DT::dataTableProxy("selected_loglin_models"), NULL)
    DT::selectRows(DT::dataTableProxy("selected_linear_simple_models"), NULL)
  }

  session_start_fresh <- function() {
    autosave_clear()
    for (k in names(rv$dataset_library)) clear_dataset_key_state(k)
    session_reset_analysis()
    rv$dataset_library <- list()
    rv$dataset_origin  <- NULL
    rv$log_transformed_by_key <- NULL
    rv$repair_logs <- NULL
    rv$active_dataset_key <- NULL
    rv$active_dataset <- NULL; rv$numeric_data <- NULL; rv$categorical_data <- NULL
    rv$row_names <- NULL; rv$vars_with_missing_data <- NULL
    session_state$pending <- list(); session_state$models <- list(); session_state$rows <- list()
    session_state$explore <- NULL; session_state$restoring <- FALSE; session_state$settled <- TRUE
    session_state$last_run <- NULL
    for (id in session_column_selectors) updateSelectInput(session, id, choices = character(0))
    updateSelectInput(session, "data_select", choices = character(0), selected = character(0))
    updateSelectInput(session, "data_select_cer_generation", choices = character(0), selected = character(0))
    session$sendCustomMessage("ceres_start_fresh", list())
  }
  observeEvent(input$session_new, session_start_fresh(), ignoreInit = TRUE)

  session_restore <- function(doc, lib) {
    st   <- doc$state %||% list()
    vals <- st$values %||% list()
    keys <- names(lib) %||% character(0)

    for (k in union(names(rv$dataset_library), keys)) clear_dataset_key_state(k)
    session_reset_analysis()
    rv$dataset_library <- lib
    rv$dataset_origin  <- ceres_session_restore_provenance(doc)
    ltk <- vals$log_transformed_by_key %||% list()
    rv$log_transformed_by_key <- if (length(ltk))
      lapply(ltk[intersect(names(ltk), keys)], function(v) as.character(unlist(v))) else NULL
    rl <- vals$repair_logs %||% list()
    rv$repair_logs <- if (length(rl)) rl[intersect(names(rl), keys)] else NULL
    mts <- as.integer(unlist(vals$manual_test_set))
    rv$manual_test_set   <- if (length(mts)) mts else NULL
    rv$nl_custom_eq_text <- as.character(vals$nl_custom_eq_text %||% "")

    pick <- function(k) if (!is.null(k) && length(k) == 1L && k %in% keys) k else if (length(keys)) keys[1L] else NULL
    explore_key <- pick(doc$active$explore)
    cer_key     <- pick(doc$active$cer_generation)

    rv$active_dataset_key <- NULL
    if (length(keys)) {
      sync_active_dataset(explore_key)
    } else {
      rv$active_dataset <- NULL; rv$numeric_data <- NULL; rv$categorical_data <- NULL
      rv$row_names <- NULL; rv$vars_with_missing_data <- NULL
      for (id in session_column_selectors) updateSelectInput(session, id, choices = character(0))
    }
    updateSelectInput(session, "data_select", choices = keys, selected = explore_key %||% character(0))
    updateSelectInput(session, "data_select_cer_generation", choices = keys, selected = cer_key %||% character(0))
    if (!is.null(cer_key)) update_cer_generation_selections(key = cer_key)
    if (!is.null(explore_key)) {
      d <- lib[[explore_key]]
      num_cols <- names(d)[vapply(d, is.numeric, logical(1L))]
      updateSelectInput(session, "explore_infl_response", choices = num_cols,
                        selected = if (length(num_cols)) num_cols[1L] else character(0))
      updateSelectInput(session, "explore_infl_predictors",
                        choices = setdiff(names(d), num_cols[1L]), selected = character(0))
    }

    session$sendCustomMessage("ceres_session_theme", list(dark = identical(st$theme, "dark")))
    for (kind in c("loglin", "linear")) {
      ui <- st$measures[[kind]]
      if (is.list(ui) && length(ui)) {
        session$sendCustomMessage("ceres-measures-ui", list(id = paste0(kind, "_measures"), ui = ui))
        if (exists(kind, envir = measures_hdr, inherits = FALSE)) get(kind, envir = measures_hdr)(ceres_measures_ui(ui))
      }
    }

    inputs <- st$inputs %||% list()
    rv$bn_restore <- list(focus = as.character(unlist(inputs$bn_focus)))
    for (et in c("log_linear_error", "linear_error"))
      if (!is.null(inputs[[et]])) inputs[[et]] <- ceres_error_term(as.character(unlist(inputs[[et]]))[1])
    inputs[["data_select"]] <- explore_key
    inputs[["data_select_cer_generation"]] <- cer_key
    pend <- list()
    for (id in names(inputs)) {
      v <- inputs[[id]]
      if (is.list(v)) v <- unlist(v)
      if (is.null(v)) v <- character(0)
      if (!ceres_session_keep_input(id, v)) next
      pend[[id]] <- list(value = v, tier = session_input_tier(id), sends = 0L, ok = 0L,
                         parked = FALSE, seen = NULL)
    }
    session_state$pending  <- pend
    session_state$rows     <- list()
    session_state$last_run <- if (length(st$last_run) == 1L && st$last_run %in% names(session_run_buttons)) st$last_run else NULL

    mods <- list()
    for (kind in c("loglin", "linear")) {
      m <- st$models[[kind]]
      if (is.null(m)) next
      by <- m$exclusions %||% list()
      by <- lapply(by, function(v) as.character(unlist(v)))
      by <- by[lengths(by) > 0L]
      sel <- m$selected_model
      if (!length(by) && is.null(sel)) next
      mods[[kind]] <- list(spec = m$spec, by_model = by, selected = sel, pushed = isTRUE(m$pushed))
    }
    session_state$models <- mods
    eo <- st$explore_outliers
    session_state$explore <- if (!is.null(eo) && (length(unlist(eo$loglin)) || length(unlist(eo$linear))))
      list(dataset = eo$dataset, loglin = as.character(unlist(eo$loglin)), linear = as.character(unlist(eo$linear)),
           save_name = eo$save_name) else NULL

    session_state$doc       <- doc
    session_state$started   <- Sys.time()
    session_state$restoring <- TRUE
    session_state$settled   <- FALSE
    session_kick(session_kick() + 1L)
  }

  session_poll_step <- function() {
    p <- session_state$pending
    now <- Sys.time()
    if (length(p)) {
      live    <- names(input)
      ids     <- names(p)
      present <- ids %in% live
      cur     <- lapply(ids, function(id) if (id %in% live) input[[id]] else NULL)
      matched <- vapply(seq_along(ids), function(i)
        present[i] && ceres_session_input_equal(cur[[i]], p[[i]]$value), logical(1L))
      for (i in seq_along(ids)) if (isTRUE(p[[i]]$parked) && present[i] &&
                                    !identical(cur[[i]], p[[i]]$seen)) {
        p[[i]]$parked <- FALSE; p[[i]]$sends <- 0L
        p[[i]]$first_sent <- NULL; p[[i]]$last_sent <- NULL
      }
      parked   <- vapply(p, function(e) isTRUE(e$parked), logical(1L))
      tiers    <- vapply(p, function(e) e$tier, integer(1L))
      blocking <- present & !matched & !parked
      gate     <- if (any(blocking)) min(tiers[blocking]) else Inf
      keep <- rep(TRUE, length(ids))
      for (i in seq_along(ids)) {
        e <- p[[i]]
        if (!present[i] || e$parked) next
        if (matched[i]) {
          e$ok <- e$ok + 1L
          if (e$ok >= 3L) keep[i] <- FALSE
        } else if (e$tier <= gate) {
          e$ok <- 0L
          since_first <- if (is.null(e$first_sent)) 0 else as.numeric(difftime(now, e$first_sent, units = "secs"))
          since_last  <- if (is.null(e$last_sent)) Inf else as.numeric(difftime(now, e$last_sent, units = "secs"))
          if (e$sends >= 4L && since_first > 10) {
            e$parked <- TRUE; e$seen <- cur[[i]]
          } else if (since_last >= 1) {
            v <- e$value
            session$sendInputMessage(ids[i], list(value = if (length(v) == 1L) v else as.list(v),
                                                  selected = if (length(v) == 1L) v else as.list(v)))
            e$sends <- e$sends + 1L
            e$last_sent <- now
            if (is.null(e$first_sent)) e$first_sent <- now
          }
        }
        p[[i]] <- e
      }
      session_state$pending <- p[keep]
    }

    for (kind in names(session_state$rows)) {
      r   <- session_state$rows[[kind]]
      tbl <- if (kind == "loglin") "selected_loglin_models" else "selected_linear_simple_models"
      fdf <- tryCatch(if (kind == "loglin") filtered_log_linear_models_data() else filtered_linear_models_data_second(),
                      error = function(e) NULL)
      idx <- if (!is.null(fdf)) match(r$name, rownames(fdf)) else NA_integer_
      sel <- input[[paste0(tbl, "_rows_selected")]]
      if (is.na(idx) || r$tries >= 8L || identical(as.integer(sel), as.integer(idx))) {
        session_state$rows[[kind]] <- NULL
        next
      }
      if (as.numeric(difftime(Sys.time(), r$at, units = "secs")) > 0.8) {
        DT::selectRows(DT::dataTableProxy(tbl), idx)
        r$tries <- r$tries + 1L
        r$at    <- Sys.time()
        session_state$rows[[kind]] <- r
      }
    }

    if (!isTRUE(session_state$settled)) {
      p <- session_state$pending
      live <- names(input)
      busy <- vapply(names(p), function(id) id %in% live && !isTRUE(p[[id]]$parked), logical(1L))
      waited <- as.numeric(difftime(Sys.time(), session_state$started, units = "secs"))
      if ((!any(busy) && waited > 1.5) || waited > 30) session_settle()
    }
  }

  session_settle <- function() {
    session_state$settled   <- TRUE
    session_state$restoring <- FALSE
    doc  <- session_state$doc
    st   <- doc$state %||% list()
    imp  <- st$values$impute
    if (!is.null(imp) && identical(as.character(unlist(imp$vars)), as.character(rv$vars_with_missing_data)) &&
        length(unlist(imp$methods)))
      rv$impute <- as.character(unlist(imp$methods))

    n_ds <- length(doc$datasets)
    parts <- if (n_ds == 0L) "no datasets" else sprintf("%d dataset%s", n_ds, if (n_ds == 1L) "" else "s")
    inp <- st$inputs %||% list()
    first <- function(v) { v <- unlist(v); if (length(v)) as.character(v[1L]) else "" }
    lr <- session_state$last_run
    sel_lbl <- if (identical(lr, "loglin_train")) paste("Log Linear", first(inp$log_linear_error))
      else if (identical(lr, "linear_simple_train")) paste("Linear", first(inp$linear_error))
      else if (identical(lr, "ols_train")) "OLS"
      else if (identical(lr, "nl_train")) paste("Non-Linear", first(inp$nl_equation))
      else if (nzchar(first(inp$ols_response))) "CER Generation"
      else ""
    if (nzchar(trimws(sel_lbl))) parts <- c(parts, paste(trimws(sel_lbl), "selections"))
    n_excl <- sum(vapply(session_state$models, function(m) length(unique(unlist(m$by_model))), integer(1L))) +
      length(session_state$explore$loglin) + length(session_state$explore$linear)
    if (n_excl > 0L) parts <- c(parts, sprintf("%d exclusion%s", n_excl, if (n_excl == 1L) "" else "s"))
    msg <- paste0("Session restored: ", paste(parts, collapse = ", "), ".")
    if (!is.null(lr) || length(session_state$models)) msg <- paste(msg, "Run the model to see results.")
    saved_v <- doc$ceres_version
    if (length(saved_v) == 1L && !is.na(saved_v) && !is.na(ceres_session_version) &&
        isTRUE(tryCatch(package_version(saved_v) > package_version(ceres_session_version), error = function(e) FALSE)))
      msg <- paste0(msg, " (Saved by CERES v", saved_v, ", newer than this one.)")
    pushed <- vapply(session_state$models, function(m) isTRUE(m$pushed), logical(1L))
    if (any(pushed)) msg <- paste(msg, "Push the CER Tuning refit again after the run.")
    showNotification(
      msg, id = "ceres_session_restored", type = "message", duration = 15,
      action = if (!is.null(lr)) actionLink("session_refit_now",
                                            paste("Refit", session_run_buttons[[lr]], "now")))
  }

  observe({
    session_kick()
    if (!length(session_state$pending) && !length(session_state$rows) && isTRUE(session_state$settled)) return()
    live <- isolate(names(input))
    p <- session_state$pending
    busy <- !isTRUE(session_state$settled) || length(session_state$rows) ||
      any(vapply(names(p), function(id) id %in% live && !isTRUE(p[[id]]$parked), logical(1L)))
    invalidateLater(if (busy) 300 else 1000)
    isolate(session_poll_step())
  })

  observeEvent(input$session_open, {
    f <- input$session_open
    req(f)
    doc <- tryCatch({
      if (!grepl("\\.json$", f$name, ignore.case = TRUE))
        stop("Choose a CERES session file (.ceres.json).", call. = FALSE)
      ceres_session_parse(readBin(f$datapath, "raw", n = file.info(f$datapath)$size))
    }, error = function(e) {
      showNotification(paste("Could not open this session.", conditionMessage(e)), type = "error", duration = 12)
      NULL
    })
    if (is.null(doc)) return()
    lib <- tryCatch(ceres_session_restore_data(doc), error = function(e) {
      showNotification(paste("Could not open this session. The file is damaged:", conditionMessage(e)),
                       type = "error", duration = 12)
      NULL
    })
    if (is.null(lib)) return()
    session_restore(doc, lib)
  })

  observeEvent(input$session_refit_now, {
    btn <- session_state$last_run
    req(btn)
    removeNotification("ceres_session_restored")
    updateNavbarPage(session, "tabs", selected = "CER Generation")
    shinyjs::click(btn)
  })

  observeEvent(input$data_upload, {
    session_state$pending <- list(); session_state$models <- list()
    session_state$rows <- list(); session_state$explore <- NULL
  }, priority = 10)

  session_apply_models <- function(kind) {
    m <- session_state$models[[kind]]
    if (is.null(m)) return(invisible(NULL))
    if (is.null(rv[[if (kind == "loglin") "model_list_loglin" else "model_list_linear_simple"]])) return(invisible(NULL))
    if (!session_same_spec(isolate(session_fit_spec(kind)), m$spec)) return(invisible(NULL))
    session_state$models[[kind]] <- NULL
    rv[[paste0(kind, "_ui_exclusions_by_model")]]   <- m$by_model
    rv[[paste0(kind, "_ui_exclusions_last_model")]] <- NULL
    if (!is.null(m$selected)) {
      session_state$rows[[kind]] <- list(name = m$selected, tries = 0L, at = Sys.time())
      session_kick(session_kick() + 1L)
    }
  }
  observeEvent(rv$model_list_loglin,        session_apply_models("loglin"))
  observeEvent(rv$model_list_linear_simple, session_apply_models("linear"))

  session_apply_explore <- function(which) {
    e <- session_state$explore
    if (is.null(e) || !identical(input$data_select, e$dataset)) return(invisible(NULL))
    inf <- rv[[paste0("explore_", which, "_influence_data")]]
    if (is.null(inf) || !length(e[[which]])) return(invisible(NULL))
    rv[[paste0("explore_", which, "_ui_exclusions")]] <- intersect(e[[which]], inf$row_label)
    if (!is.null(e$save_name) && nzchar(e$save_name))
      updateTextInput(session, "explore_infl_save_name", value = e$save_name)
    e[[which]] <- character(0)
    session_state$explore <- if (length(e$loglin) || length(e$linear)) e else NULL
  }
  observeEvent(rv$explore_loglin_influence_data, session_apply_explore("loglin"), priority = 10)
  observeEvent(rv$explore_linear_influence_data, session_apply_explore("linear"), priority = 10)

  summary.mupe_loglin <- function(object, correlation = FALSE, ...) {
    s <- attr(object, "mupe_summary")
    if (is.null(s)) return(NextMethod())
    s$call      <- object$call
    s$terms     <- object$terms
    s$residuals <- object$residuals
    cu <- s$cov.unscaled
    if (isTRUE(correlation) && is.matrix(cu) && all(is.finite(cu)) && all(diag(cu) > 0)) {
      s$correlation <- cov2cor(cu)
      dimnames(s$correlation) <- list(rownames(s$coefficients), rownames(s$coefficients))
    }
    class(s)    <- "summary.lm"
    s
  }

  fit_irls <- function(formula, data, error_type, subset_index = NULL,
                       mode = "linear") {

    if (identical(error_type, "WLR")) {
      warning(WLR_REMOVED_NOTICE)
      error_type <- ceres_error_term(error_type)
    }

    if (!is.null(subset_index)) {
      data_fit <- data[subset_index, , drop = FALSE]
    } else {
      data_fit <- data
    }
    data_fit <- droplevels(data_fit)

    fit <- lm(formula, data = data_fit)

    if (error_type == "OLS") {
      return(fit)
    }

    X <- model.matrix(formula, data_fit)
    Y <- model.response(model.frame(fit))
    keep_cols    <- !is.na(coef(fit))
    all_coef_nms <- names(coef(fit))
    X <- X[, keep_cols, drop = FALSE]
    start_params <- coef(fit)[keep_cols]

    if (error_type == "MUPE") {
      if (mode == "loglin") {
        Y_unit <- exp(Y)
        beta   <- start_params
        beta[!is.finite(beta)] <- 0
        conv <- FALSE
        for (k in 1:1000) {
          yhat <- exp(as.vector(X %*% beta))
          s    <- Y_unit / yhat - 1
          delta <- tryCatch(
            as.vector(solve(crossprod(X), crossprod(X, s))),
            error = function(e) NULL
          )
          if (is.null(delta) || any(!is.finite(delta))) {
            warning("MUPE GN solve failed or returned non-finite delta; returning last beta.")
            break
          }
          beta <- beta + delta
          delta_norm <- sqrt(sum(delta^2))
          if (!is.finite(delta_norm)) break
          if (delta_norm < 1e-12) { conv <- TRUE; break }
        }
        final_coefs   <- beta
        fitted_unit   <- exp(as.vector(X %*% final_coefs))
        mupe_fit               <- fit
        mupe_fit$coefficients  <- expand_coefs(final_coefs, all_coef_nms)
        mupe_fit$fitted.values <- log(fitted_unit)
        mupe_fit$residuals     <- Y - mupe_fit$fitted.values
        attr(mupe_fit, "mupe_unit_yhat") <- fitted_unit
        attr(mupe_fit, "irls_converged") <- conv; attr(mupe_fit, "irls_iterations") <- k

        n_obs_fit <- length(Y)
        p_eff_fit <- max(0L, ncol(X) - 1L)
        df_r_fit  <- max(1L, n_obs_fit - p_eff_fit - 1L)
        r_log     <- mupe_fit$residuals
        sigma_log <- sqrt(sum(r_log^2) / df_r_fit)
        w_fit     <- 1 / fitted_unit^2
        rss_w     <- sum(w_fit * (Y_unit - fitted_unit)^2)
        y_bar_w   <- sum(w_fit * Y_unit) / sum(w_fit)
        tss_w     <- sum(w_fit * (Y_unit - y_bar_w)^2)
        r2_fit    <- if (tss_w > 0) 1 - rss_w / tss_w else NA_real_
        adj_r2_fit <- if (!is.na(r2_fit))
          1 - (1 - r2_fit) * (n_obs_fit - 1L) / df_r_fit else NA_real_

        cov_unsc <- tryCatch(chol2inv(chol(crossprod(X))), error = function(e) NULL)
        coef_se  <- if (!is.null(cov_unsc))
          sigma_log * sqrt(pmax(diag(cov_unsc), 0)) else rep(NA_real_, length(final_coefs))

        est_disp <- final_coefs
        se_disp  <- coef_se
        t_vals_fit <- est_disp / se_disp
        p_vals_fit <- 2 * pt(-abs(t_vals_fit), df = df_r_fit)
        coef_tbl_fit <- cbind(Estimate     = est_disp,
                              "Std. Error" = se_disp,
                              "t value"    = t_vals_fit,
                              "Pr(>|t|)"   = p_vals_fit)
        colnames(coef_tbl_fit) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
        rownames(coef_tbl_fit) <- names(final_coefs)

        f_val_fit <- if (p_eff_fit > 0L && df_r_fit > 0L && !is.na(r2_fit) && r2_fit < 1)
          (r2_fit / p_eff_fit) / ((1 - r2_fit) / df_r_fit) else NA_real_
        fstat_fit <- if (is.finite(f_val_fit))
          c(value = f_val_fit, numdf = p_eff_fit, dendf = df_r_fit) else NULL

        attr(mupe_fit, "mupe_summary") <- list(
          coefficients  = coef_tbl_fit,
          r.squared     = r2_fit,
          adj.r.squared = adj_r2_fit,
          sigma         = sigma_log,
          df            = c(p_eff_fit + 1L, df_r_fit, p_eff_fit + 1L),
          fstatistic    = fstat_fit,
          cov.unscaled  = if (!is.null(cov_unsc)) cov_unsc else
                          matrix(NA_real_, length(final_coefs), length(final_coefs)),
          aliased       = setNames(!keep_cols, all_coef_nms)
        )
        class(mupe_fit) <- c("mupe_loglin", class(mupe_fit))
        return(mupe_fit)
      } else {
        formula_w <- formula
        environment(formula_w) <- environment()
        prev_coefs <- coef(fit)
        conv <- FALSE
        for (k in 1:1000) {
          yhat <- fit$fitted.values
          yhat[abs(yhat) < 1e-6] <- 1e-6
          w <- 1 / yhat^2
          fit <- lm(formula_w, data = data_fit, weights = w)
          if (irls_step_small(coef(fit), prev_coefs)) { conv <- TRUE; break }
          prev_coefs <- coef(fit)
        }
        attr(fit, "irls_converged") <- conv; attr(fit, "irls_iterations") <- k
        return(fit)
      }
    }

    if (error_type == "ZMPE") {
      is_ll  <- identical(mode, "loglin")
      Y_unit <- if (is_ll) exp(Y) else Y
      final_coefs <- tryCatch(zmpe_solve(X, Y_unit, if (is_ll) "loglin" else "linear"),
                              error = function(e) NULL)
      z_conv <- attr(final_coefs, "irls_converged") %||% FALSE; z_it <- attr(final_coefs, "irls_iterations") %||% NA_integer_
      if (is.null(final_coefs) || any(!is.finite(final_coefs))) {
        warning("ZMPE failed; returning OLS.")
        final_coefs <- start_params
      }
      final_coefs <- setNames(final_coefs, names(start_params))
      zmpe_fit <- fit
      zmpe_fit$coefficients  <- expand_coefs(final_coefs, all_coef_nms)
      attr(zmpe_fit, "irls_converged") <- z_conv; attr(zmpe_fit, "irls_iterations") <- z_it
      if (is_ll) {
        fitted_unit <- exp(as.vector(X %*% final_coefs))
        zmpe_fit$fitted.values <- log(fitted_unit)
        zmpe_fit$residuals     <- Y - zmpe_fit$fitted.values
      } else {
        fitted_unit <- as.vector(X %*% final_coefs)
        yh_lin <- fitted_unit; yh_lin[abs(yh_lin) < 1e-6] <- 1e-6
        w_lin  <- 1 / yh_lin^2
        formula_w <- formula
        environment(formula_w) <- environment()
        shell_w <- tryCatch(lm(formula_w, data = data_fit, weights = w_lin), error = function(e) NULL)
        if (!is.null(shell_w)) {
          attr(shell_w, "irls_converged") <- z_conv; attr(shell_w, "irls_iterations") <- z_it
          zmpe_fit <- shell_w
          zmpe_fit$coefficients <- expand_coefs(final_coefs, all_coef_nms)
        }
        zmpe_fit$fitted.values <- fitted_unit
        zmpe_fit$residuals     <- Y - fitted_unit
      }

      n_obs_fit <- length(Y)
      p_eff_fit <- max(0L, ncol(X) - 1L)
      df_r_fit  <- max(1L, n_obs_fit - p_eff_fit - 1L)
      sigma_fit <- sqrt(sum(zmpe_fit$residuals^2) / df_r_fit)
      w_fit   <- 1 / pmax(fitted_unit^2, 1e-12)
      rss_w   <- sum(w_fit * (Y_unit - fitted_unit)^2)
      y_bar_w <- sum(w_fit * Y_unit) / sum(w_fit)
      tss_w   <- sum(w_fit * (Y_unit - y_bar_w)^2)
      if (!is_ll) {
        sigma_fit <- sqrt(sum(((Y_unit - yh_lin) / yh_lin)^2) / df_r_fit)
        rss_w     <- sum(((Y_unit - yh_lin) / yh_lin)^2)
        y_bar_w   <- sum(w_lin * Y_unit) / sum(w_lin)
        tss_w     <- sum(w_lin * (Y_unit - y_bar_w)^2)
      }
      r2_fit  <- if (tss_w > 0) 1 - rss_w / tss_w else NA_real_
      adj_r2_fit <- if (!is.na(r2_fit))
        1 - (1 - r2_fit) * (n_obs_fit - 1L) / df_r_fit else NA_real_

      cov_unsc <- tryCatch(if (is_ll) chol2inv(chol(crossprod(X)))
                           else chol2inv(chol(crossprod(X * sqrt(w_lin)))), error = function(e) NULL)
      coef_se  <- if (!is.null(cov_unsc))
        sigma_fit * sqrt(pmax(diag(cov_unsc), 0)) else rep(NA_real_, length(final_coefs))

      est_disp <- final_coefs
      se_disp  <- coef_se
      t_vals_fit <- est_disp / se_disp
      p_vals_fit <- 2 * pt(-abs(t_vals_fit), df = df_r_fit)
      coef_tbl_fit <- cbind(Estimate     = est_disp,
                            "Std. Error" = se_disp,
                            "t value"    = t_vals_fit,
                            "Pr(>|t|)"   = p_vals_fit)
      colnames(coef_tbl_fit) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      rownames(coef_tbl_fit) <- names(final_coefs)

      f_val_fit <- if (p_eff_fit > 0L && df_r_fit > 0L && !is.na(r2_fit) && r2_fit < 1)
        (r2_fit / p_eff_fit) / ((1 - r2_fit) / df_r_fit) else NA_real_
      fstat_fit <- if (is.finite(f_val_fit))
        c(value = f_val_fit, numdf = p_eff_fit, dendf = df_r_fit) else NULL

      attr(zmpe_fit, "mupe_summary") <- list(
        coefficients  = coef_tbl_fit,
        r.squared     = r2_fit,
        adj.r.squared = adj_r2_fit,
        sigma         = sigma_fit,
        df            = c(p_eff_fit + 1L, df_r_fit, p_eff_fit + 1L),
        fstatistic    = fstat_fit,
        cov.unscaled  = if (!is.null(cov_unsc)) cov_unsc else
                        matrix(NA_real_, length(final_coefs), length(final_coefs)),
        aliased       = setNames(!keep_cols, all_coef_nms)
      )
      class(zmpe_fit) <- c("mupe_loglin", class(zmpe_fit))
      return(zmpe_fit)
    }

    warning(sprintf("fit_irls: unknown error_type '%s'; returning OLS.", error_type))
    return(fit)
  }
  
  output$nl_sidebar_ui <- renderUI({
    req(rv$numeric_data)
    
    default_y <- if (!is.null(input$ols_response)) input$ols_response else rv$numeric_data[1]
    
    tagList(
      selectInput("nl_response", "Response Variable (Y)", choices = rv$numeric_data, selected = default_y),
      checkboxInput("nl_log_y", "Apply Log-Transform", value = FALSE),
      selectInput("nl_equation", "Non-linear Equation", choices = c(names(nl_registry), "Custom")),
      uiOutput("nl_predictors_ui"),
      checkboxInput("nl_log_x", "Apply Log-Transform", value = FALSE)
    )
  })

  nl_panel_mode <- reactiveVal(NULL)
  observe({
    req(input$nl_equation)
    nl_panel_mode(if (input$nl_equation != "Custom") "builtin"
                  else if (!isTRUE(rv$nl_custom_fitted)) "builder" else "custom")
  })
  nl_results_tabset <- function() {
    tabsetPanel(id = "nlls",
      tabPanel("Model Specification",
        tags$script(HTML("
           $(document).ready(function() {
             setTimeout(function() {
               $('div[data-value=\"Model Specification\"]').removeClass('html-fill-item html-fill-container bslib-gap-spacing');
             }, 100);
           });
         ")),
        h4("Model formula", style = "margin-bottom: 2px;"),
        tags$p("Built from the equation and the variables you choose in the sidebar.",
        style = "font-size: 0.9em; margin-top: 0px; margin-bottom: 2px;"),
        column(width = 12,
          verbatimTextOutput("nlls_formula_display")
        ),
        column(width = 12,
          DT::dataTableOutput("nl_results"),
          tags$br()
        ),
        column(width = 12,
          uiOutput("nl_coeffs_heading", inline = TRUE),
          DT::dataTableOutput("nl_coeffs")
        ),
        uiOutput("nl_export_ui")
      ),
      tabPanel("Actuals vs Predicted",
        tags$br(),
        withSpinner(plotlyOutput("nl_actuals_vs_predicted", height = 450),
                    color = getOption("spinner.color", default = loading_color),
                    type = 1)
      ),
      tabPanel("Residuals vs Predicted",
        tags$br(),
        withSpinner(plotlyOutput("nl_residuals_vs_predicted", height = 450),
                    color = getOption("spinner.color", default = loading_color),
                    type = 1)
      ),
      tabPanel("Residual Normality",
        tags$br(),
        withSpinner(plotlyOutput("nl_residual_normality", height = 450),
                    color = getOption("spinner.color", default = loading_color),
                    type = 1)
      ),
      tabPanel("Predictions",
        tags$br(),
        DT::dataTableOutput("nl_predict_table", fill = FALSE),
        tags$div(style = "margin-top: 10px;"),
        fluidRow(
          column(width = 4, uiOutput("nl_predictions")),
          column(width = 8,
            withSpinner(plotlyOutput("nl_pred_chart", height = 400),
                        color = getOption("spinner.color", default = loading_color),
                        type = 1)
          )
        )
      )
    )
  }
  output$nl_main_panel_ui <- renderUI({
    mode <- nl_panel_mode()
    req(mode)
    if (mode == "builtin") {
      nl_results_tabset()
    } else if (mode == "builder") {
      uiOutput("nl_custom_builder_ui")
    } else {
      tagList(uiOutput("nl_custom_strip"), nl_results_tabset())
    }
  })
  output$nl_coeffs_heading <- renderUI({
    if (!is.null(rv$nl_coeffs)) h5("Final Estimated Parameters")
  })
  output$nl_export_ui <- renderUI({
    req(!is.null(rv$nl_model), !is.null(rv$nl_coeffs))
    column(width = 12,
      tags$br(),
      downloadButton("export_nl_excel", "Export Statistics", icon = icon("file-excel"),
                     class = "btn-sm",
                     style = paste0("background-color: ", primary_color(), " !important; ",
                                    "color: ", if (isTRUE(input$dark_mode)) "#000000" else "#FFFFFF", " !important; ",
                                    "border: none;")),
      downloadButton("report_nl", "CER report", icon = icon("file-lines"),
                     class = "btn-sm c-report-btn")
    )
  })
  outputOptions(output, "nl_export_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "nl_coeffs_heading", suspendWhenHidden = FALSE)
  output$nl_custom_strip <- renderUI({
    req(rv$nl_model)
    div(class = "c-nl-strip",
      div(class = "c-nl-strip-row",
        tags$span(class = "c-nl-strip-label", "Equation"),
        tags$code(class = "c-nl-strip-eq", paste0(input$nl_response, " ~ ", input$nl_custom_eq)),
        actionButton("nl_custom_edit", "Edit Equation", icon = icon("pencil"),
                     class = "btn-sm btn-outline-primary c-nl-strip-edit")
      ),
      tags$div(class = "c-nl-strip-coefs",
        paste(names(coef(rv$nl_model)), round(coef(rv$nl_model), 3), sep = " = ", collapse = ",   "))
    )
  })

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  nl_cb_cols <- reactive({
    df <- rv$dataset_library[[input$data_select_cer_generation]]
    req(df)
    san <- vapply(names(df), sanitize_nl_colname, "")
    num <- vapply(df, is.numeric, TRUE)
    list(df = df, num = unname(san[num]), cat = unname(san[!num]),
         labels = setNames(names(df), san), resp_choices = names(df)[num])
  })
  nl_cb_state <- reactive({
    req(input$nl_equation == "Custom", input$nl_response)
    cc <- nl_cb_cols()
    rs <- sanitize_nl_colname(input$nl_response)
    n  <- sum(is.finite(cc$df[[input$nl_response]]))
    st <- nl_cb_check(nl_custom_eq_debounced(), setdiff(cc$num, rs), cc$cat, n)
    st$response <- rs
    st
  })
  nl_cb_parset <- reactiveVal(character(0))
  observe({
    p <- tryCatch(nl_cb_state()$params, error = function(e) character(0))
    if (!identical(p, isolate(nl_cb_parset()))) nl_cb_parset(p)
  })

  output$nl_custom_builder_ui <- renderUI({
    req(input$nl_equation == "Custom", input$nl_response)
    rv$nl_builder_render_token
    cc <- nl_cb_cols()
    drivers <- setdiff(cc$num, sanitize_nl_colname(input$nl_response))
    eq0 <- isolate(if (nzchar(trimws(rv$nl_custom_fitted_eq %||% ""))) rv$nl_custom_fitted_eq else rv$nl_custom_eq_text %||% "")
    tags$div(class = "c-cb",
      tags$div(class = "c-cb-shapes", lapply(nl_cb_shapes(drivers), function(s)
        tags$button(type = "button", class = "c-cb-shape", `data-eq` = s$eq, onclick = "ceresCbTemplate(this)",
          HTML(sprintf('<svg viewBox="0 0 120 50" width="100%%" height="44" aria-hidden="true"><line class="c-cb-axis" x1="6" y1="45" x2="114" y2="45"></line><path class="%s" d="%s"></path></svg>',
                       if (nzchar(s$eq)) "c-cb-shape-curve" else "c-cb-shape-plus", s$d)),
          tags$span(class = "c-cb-shape-name", s$name), tags$span(class = "c-cb-shape-f", s$f)))),
      tags$div(class = "c-cb-card",
        tags$div(class = "c-cb-eqrow",
          tags$label(`for` = "nl_cb_response", class = "c-sr-only", "Response"),
          tags$select(id = "nl_cb_response", class = "c-cb-resp",
                      lapply(cc$resp_choices, function(v) tags$option(value = v, selected = if (identical(v, input$nl_response)) NA, v))),
          tags$span(class = "c-cb-eqsign", "="),
          tags$div(id = "nl_cb_editor", class = "c-cb-input c-cb-editor", contenteditable = "true", role = "textbox",
                   `aria-label` = "Equation", spellcheck = "false", autocapitalize = "off", autocorrect = "off",
                   `data-placeholder` = "a + b * Weight^c"),
          tags$textarea(id = "nl_custom_eq", class = "c-cb-hidden", `aria-hidden` = "true", tabindex = "-1", eq0)),
        uiOutput("nl_cb_head"),
        tags$div(class = "c-cb-palette",
          tags$div(class = "c-cb-group", tags$span(class = "c-cb-label", "Drivers"), uiOutput("nl_cb_drivers", class = "c-cb-chips")),
          tags$div(class = "c-cb-group", tags$span(class = "c-cb-label", "Parameters"), uiOutput("nl_cb_paramchips", class = "c-cb-chips")),
          tags$div(class = "c-cb-group c-cb-group-wide", tags$span(class = "c-cb-label", "Insert"),
            tags$div(class = "c-cb-chips", lapply(list(c("+", " + "), c("−", " - "), c("×", " * "), c("÷", " / "), c("xʸ", "^"),
                                                     c("( )", "()"), c("exp", "exp()"), c("log", "log()"), c("log10", "log10()"),
                                                     c("√", "sqrt()"), c("|x|", "abs()"), c("max", "pmax(, )")), function(o)
              tags$button(type = "button", class = "c-cb-op", onclick = sprintf("nlInsertAtCursor('%s')", o[2]), o[1])))))),
      tags$div(class = "c-cb-bottom",
        tags$div(class = "c-cb-card",
          tags$div(class = "c-cb-cardhead", tags$span(class = "c-cb-title", "Live fit"), uiOutput("nl_cb_fitstats", inline = TRUE)),
          uiOutput("nl_cb_chart")),
        tags$div(class = "c-cb-card",
          tags$div(class = "c-cb-cardhead", tags$span(class = "c-cb-title", "Parameters")),
          uiOutput("nl_cb_params"))))
  })
  outputOptions(output, "nl_custom_builder_ui", suspendWhenHidden = FALSE)

  observeEvent(input$nl_cb_response, {
    if (!identical(input$nl_cb_response, input$nl_response))
      updateSelectInput(session, "nl_response", selected = input$nl_cb_response)
  }, ignoreInit = TRUE)

  output$nl_cb_head <- renderUI({
    st <- nl_cb_state(); cc <- nl_cb_cols()
    tex <- if (!is.null(st$expr) && st$state == "ready")
      paste0("\\text{", gsub("([_#%&$])", "\\\\\\1", input$nl_response), "} = ", nl_cb_latex(st$expr, st$params, cc$labels))
    cls <- switch(st$state, ready = "c-cb-status is-ready", error = "c-cb-status is-error", "c-cb-status")
    tags$div(class = "c-cb-head",
      if (!is.null(tex)) tags$div(class = "katex-eq c-cb-typeset", `data-latex` = tex) else tags$div(class = "c-cb-typeset is-empty"),
      tags$span(class = cls, role = "status", st$message))
  })
  output$nl_cb_drivers <- renderUI({
    st <- nl_cb_state(); cc <- nl_cb_cols()
    lapply(setdiff(cc$num, st$response), function(v)
      tags$button(type = "button", class = paste("c-cb-driver", if (v %in% st$drivers) "is-used"), `data-name` = v,
                  onclick = sprintf("nlInsertAtCursor('%s')", gsub("'", "\\\\'", v)), cc$labels[[v]] %||% v))
  })
  output$nl_cb_paramchips <- renderUI({
    st <- nl_cb_state(); cc <- nl_cb_cols()
    mm <- if (length(st$params) && !is.null(st$expr)) nl_cb_meanings(st$expr, st$params, st$drivers, input$nl_response, cc$labels) else character(0)
    eq <- nl_custom_eq_debounced() %||% ""
    typed <- regmatches(eq, gregexpr("[A-Za-z_.][A-Za-z0-9_.]*", eq))[[1]]
    nxt <- setdiff(letters, c(st$params, typed))[1]
    tagList(
      lapply(st$params, function(p) tags$button(type = "button", class = "c-cb-param", onclick = sprintf("nlInsertAtCursor('%s')", p),
        tags$b(p), if (!is.na(mm[p])) tags$span(mm[[p]]))),
      tags$button(type = "button", class = "c-cb-param is-new", onclick = "nlInsertNewParam()", paste("+", nxt)))
  })

  nl_cb_fit <- reactive({
    st <- nl_cb_state()
    req(st$state == "ready")
    cc <- nl_cb_cols()
    d <- cc$df; names(d) <- vapply(names(d), sanitize_nl_colname, "")
    d <- d[, c(st$response, st$drivers), drop = FALSE]
    d <- d[stats::complete.cases(d), , drop = FALSE]
    seed <- unlist(lapply(st$params, function(p) { v <- input[[paste0("nl_custom_start_", p)]]
      if (is.numeric(v) && length(v) == 1 && is.finite(v)) setNames(v, p) }))
    fit <- tryCatch(nl_fit_builtin(paste(st$response, "~", nl_custom_eq_debounced()), d, "Custom", character(0),
                                   input$nl_error_term %||% "Least Squares", seed = seed),
                    error = function(e) e)
    list(fit = fit, d = d, st = st)
  })
  output$nl_cb_fitstats <- renderUI({
    r <- tryCatch(nl_cb_fit(), error = function(e) NULL)
    if (is.null(r)) return(tags$span(class = "c-cb-stats", "Waiting for a complete equation"))
    if (inherits(r$fit, "error"))
      return(tags$span(class = "c-cb-stats is-error", nl_cb_fail_message(conditionMessage(r$fit), input$nl_equation)))
    info <- attr(r$fit, "nl_info") %||% list()
    y <- r$d[[r$st$response]]
    r2 <- 1 - sum(stats::residuals(r$fit)^2) / sum((y - mean(y))^2)
    tags$span(class = "c-cb-stats",
      tags$span(HTML(sprintf("R² <b>%.2f</b>", r2))),
      tags$span(class = if (isTRUE(info$converged)) "is-good" else "is-warn", if (isTRUE(info$converged)) "Converged" else "Stopped at the iteration limit"),
      if (!is.null(info$starts)) tags$span(sprintf("%s starting points tried", format(info$starts, big.mark = ","))),
      if (!is.null(info$secs)) tags$span(sprintf("fitted in %.2f s", info$secs)))
  })
  output$nl_cb_chart <- renderUI({
    r <- tryCatch(nl_cb_fit(), error = function(e) NULL)
    if (is.null(r) || inherits(r$fit, "error")) return(tags$div(class = "c-cb-chart is-empty", "The fitted curve appears here as you type."))
    cc <- nl_cb_cols(); st <- r$st; y <- r$d[[st$response]]
    lab <- function(v) cc$labels[[v]] %||% v
    svg <- if (length(st$drivers) == 1) {
      x <- r$d[[st$drivers]]
      gx <- seq(min(x), max(x), length.out = 120)
      gy <- tryCatch(as.numeric(stats::predict(r$fit, stats::setNames(data.frame(gx), st$drivers))), error = function(e) NULL)
      nl_cb_svg(x, y, gx, gy, lab(st$drivers), input$nl_response)
    } else {
      f <- as.numeric(stats::fitted(r$fit))
      nl_cb_svg(f, y, range(c(f, y)), range(c(f, y)), paste("Fitted", input$nl_response), paste("Actual", input$nl_response))
    }
    tags$div(class = "c-cb-chart", svg)
  })
  nl_cb_last <- reactiveVal(list())
  output$nl_cb_params <- renderUI({
    ps <- nl_cb_parset()
    if (!length(ps)) return(tags$div(class = "c-cb-chart is-empty", "Parameters appear here."))
    last <- isolate(nl_cb_last())
    lapply(ps, function(p) {
      r <- last[[p]] %||% list(value = "—", range = "95% range —", meaning = "", left = 0, width = 0, shaky = FALSE, note = "")
      tags$div(class = paste("c-cb-prow", if (isTRUE(r$shaky)) "is-shaky"), id = paste0("nl_cb_row_", p),
        tags$div(class = "c-cb-ptop", tags$span(tags$b(p), tags$span(class = "c-cb-pmean", id = paste0("nl_cb_m_", p), r$meaning)),
                 tags$span(class = "c-cb-pval", id = paste0("nl_cb_v_", p), r$value)),
        tags$div(class = "c-cb-bar", tags$div(class = "c-cb-barfill", id = paste0("nl_cb_b_", p),
                                              style = sprintf("left:%s%%;width:%s%%", r$left, r$width))),
        tags$div(class = "c-cb-pfoot", tags$span(id = paste0("nl_cb_r_", p), paste0(r$range, if (nzchar(r$note %||% "")) paste0(" · ", r$note))),
          tags$span(class = "c-cb-start", tags$label(`for` = paste0("nl_custom_start_", p), "Start"),
            tags$input(type = "number", id = paste0("nl_custom_start_", p), class = "c-cb-startin", placeholder = "auto", step = "any",
                       value = { v <- isolate(input[[paste0("nl_custom_start_", p)]]); if (is.numeric(v) && length(v) == 1 && is.finite(v)) v }))))
    })
  })
  observe({
    if (!identical(input$nl_equation, "Custom")) return()
    r <- tryCatch(nl_cb_fit(), error = function(e) NULL)
    ps <- nl_cb_parset()
    st <- tryCatch(nl_cb_state(), error = function(e) NULL)
    cc <- nl_cb_cols()
    mm <- if (!is.null(st) && !is.null(st$expr) && length(ps)) nl_cb_meanings(st$expr, ps, st$drivers, input$nl_response, cc$labels) else NULL
    ok <- !is.null(r) && !inherits(r$fit, "error")
    ci <- if (ok) tryCatch({ s <- summary(r$fit)$coefficients; q <- stats::qt(0.975, max(1, stats::df.residual(r$fit)))
      cbind(est = s[, 1], lo = s[, 1] - q * s[, 2], hi = s[, 1] + q * s[, 2]) }, error = function(e) NULL)
    fmt <- function(v) if (is.finite(v)) formatC(v, format = "fg", digits = 4, big.mark = ",") else "—"
    rows <- lapply(ps, function(p) {
      m <- if (!is.null(mm) && !is.na(mm[p])) mm[[p]] else ""
      if (is.null(ci) || !(p %in% rownames(ci)))
        return(list(p = p, meaning = m, value = "—", range = "95% range —", left = 0, width = 0, shaky = FALSE, note = ""))
      e <- ci[p, "est"]; lo <- ci[p, "lo"]; hi <- ci[p, "hi"]
      hw <- (hi - lo) / 2
      shaky <- !is.finite(hw) || (lo < 0 && hi > 0 && e != 0) || hw > abs(e)
      span <- max(2 * hw, abs(e) * 0.25, 1e-12, na.rm = TRUE)
      left <- if (is.finite(lo)) max(0, min(100, (lo - (e - span)) / (2 * span) * 100)) else 0
      width <- if (is.finite(hw)) max(2, min(100 - left, (hi - lo) / (2 * span) * 100)) else 100
      list(p = p, meaning = m, value = fmt(e), range = paste0("95% range ", fmt(lo), " – ", fmt(hi)),
           left = round(left, 1), width = round(width, 1), shaky = shaky,
           note = if (shaky) "not well determined" else "")
    })
    nl_cb_last(setNames(rows, vapply(rows, `[[`, "", "p")))
    session$sendCustomMessage("ceres-cb-fit", list(rows = rows))
  })

  output$nl_predictors_ui <- renderUI({
    req(input$nl_equation, input$nl_response)
    if (input$nl_equation == "Custom") return(NULL)
    reg <- nl_registry[[input$nl_equation]]
    is_multi <- (reg$type == "multivariate")

    cur_data <- rv$dataset_library[[input$data_select_cer_generation]]
    req(!is.null(cur_data))
    cer_numeric <- names(cur_data)[vapply(cur_data, is.numeric, logical(1L))]
    available_x <- setdiff(cer_numeric, input$nl_response)

    cur_sel <- intersect(isolate(input$nl_predictors), available_x)
    selectInput("nl_predictors", "Explanatory Variables (X)",
                choices  = available_x,
                selected = cur_sel,
                multiple = is_multi)
  })

  observe({
    req(input$nl_equation, input$nl_predictors, input$nl_response)
    if (input$nl_equation == "Custom") return()
    nl_val_key <- input$data_select_cer_generation
    nl_val_df  <- if (!is.null(nl_val_key) && nzchar(nl_val_key))
      rv$dataset_library[[nl_val_key]] else NULL
    req(!is.null(nl_val_df))
    x_vars <- input$nl_predictors
    y_var  <- input$nl_response
    eq_name <- input$nl_equation
    reg     <- nl_registry[[eq_name]]
    if (length(x_vars) == 0) return()

    cols_needed <- c(y_var, x_vars)
    cols_present <- intersect(cols_needed, names(nl_val_df))
    if (length(cols_present) < length(cols_needed)) return()

    df <- na.omit(nl_val_df[, cols_needed, drop = FALSE])
    n_params <- length(reg$params_base) + if (reg$type == "multivariate") length(x_vars) else 0L
    msg <- validate_nl_data(df, x_vars = x_vars, y_var = y_var,
                            eq_name = eq_name, n_params = n_params)
    if (!is.null(msg)) {
      showNotification(msg, type = "warning", duration = 10)
    }
  })

  nlls_formula_string <- reactive({
    req(input$nl_equation, input$nl_response)

    if (input$nl_equation == "Custom") {
      req(input$nl_custom_eq, nchar(trimws(input$nl_custom_eq)) > 0)
      san_y <- sanitize_nl_colname(input$nl_response)
      return(paste0(san_y, " ~ ", input$nl_custom_eq))
    }

    req(input$nl_predictors)
    eq_name <- input$nl_equation
    reg <- nl_registry[[eq_name]]
    x_vars <- input$nl_predictors
    y_var <- input$nl_response
    
    if (length(x_vars) == 0) return("Awaiting variables...")
    if (reg$type == "univariate" && length(x_vars) > 1) {
      x_vars <- x_vars[1]
    }
    
    san_x <- vapply(x_vars, sanitize_nl_colname, character(1))
    san_y <- sanitize_nl_colname(y_var)

    if (reg$type == "multivariate") {
      if (eq_name == "Power Curve") {
        terms_str <- paste0("(", san_x, "^", reg$param_prefix, san_x, ")", collapse = " * ")
      } else if (eq_name == "Exponential" || eq_name == "Logistic (S-Curve)") {
        terms_str <- paste0("(", reg$param_prefix, san_x, " * ", san_x, ")", collapse = " + ")
      }
    } else {
      terms_str <- ""
    }

    rhs_str <- reg$formula_base
    if (reg$type == "multivariate") {
      rhs_str <- gsub("\\{terms\\}", terms_str, rhs_str)
    } else {
      rhs_str <- gsub("\\{X1\\}", san_x[1], rhs_str)
    }

    return(paste0(san_y, " ~ ", rhs_str))
  })
  
  output$nlls_formula_display <- renderText({
    f <- tryCatch(nlls_formula_string(), shiny.silent.error = function(e) NULL)
    if (is.null(f))
      return(if (identical(input$nl_equation, "Custom")) "Type the right-hand side of your equation in the sidebar."
             else "Choose explanatory variables in the sidebar to build the formula.")
    nl_display_names(f,
                     names(rv$dataset_library[[input$data_select_cer_generation]] %||% list()))
  })

  nl_active_predictors <- reactive({
    if (isTRUE(rv$nl_custom_fitted) && length(rv$nl_custom_x_vars) > 0) {
      rv$nl_custom_x_vars
    } else {
      input$nl_predictors
    }
  })
  
  nl_guesses <- reactive({
    req(input$nl_equation, input$nl_response, input$data_select_cer_generation)
    nl_data <- rv$dataset_library[[input$data_select_cer_generation]]
    req(nl_data)
    if (input$nl_equation == "Custom") return(list())
    req(input$nl_predictors)
    eq_name <- input$nl_equation
    reg <- nl_registry[[eq_name]]
    x_vars <- input$nl_predictors
    y_var <- input$nl_response

    if (length(x_vars) == 0) return(list())
    if (reg$type == "univariate" && length(x_vars) > 1) x_vars <- x_vars[1]

    if (!all(c(y_var, x_vars) %in% names(nl_data))) return(list())
    df <- na.omit(nl_data[, c(y_var, x_vars), drop = FALSE])
    nl_start_guesses(df, eq_name, x_vars, y_var)
  })

  nl_custom_eq_debounced <- debounce(reactive(input$nl_custom_eq), 300)

  nl_custom_params <- reactive({
    req(input$nl_equation == "Custom")
    tryCatch(nl_cb_state()$params, error = function(e) character(0))
  })

  nl_custom_valid <- reactive(isTRUE(tryCatch(nl_cb_state()$state == "ready", error = function(e) FALSE)))

  observe({
    input$nl_equation
    rv$nl_custom_fitted         <- FALSE
    rv$nl_custom_show_startvals <- FALSE
    rv$nl_custom_x_vars         <- NULL
  })

  observeEvent(input$nl_custom_edit, {
    rv$nl_custom_fitted         <- FALSE
    rv$nl_builder_render_token  <- rv$nl_builder_render_token + 1L
  })

  observe({
    eq <- input$nl_custom_eq
    if (!is.null(eq) && nchar(trimws(eq)) > 0) rv$nl_custom_eq_text <- eq
  })

  observe({
    req(input$nl_equation == "Custom")
    if (isTRUE(nl_custom_valid())) {
      shinyjs::enable("nl_train")
    } else {
      shinyjs::disable("nl_train")
    }
  })

  observe({
    req(input$nl_equation != "Custom")
    shinyjs::enable("nl_train")
  })

  
  selected_log_linear_model <- reactive({
    req(input$selected_loglin_models_rows_selected)

    filtered_df <- filtered_log_linear_models_data()
    req(nrow(filtered_df) >= input$selected_loglin_models_rows_selected)

    selected_model_name <- rownames(filtered_df)[input$selected_loglin_models_rows_selected]

    pushed <- rv$loglin_tuning_pushed_model
    if (!is.null(pushed) && identical(attr(pushed, "pushed_for"), selected_model_name)) return(pushed)

    model_index <- match(selected_model_name, names(rv$model_list_loglin))
    req(!is.na(model_index))

    spec <- rv$model_list_loglin[[model_index]]
    log1p_cols <- rv$loglin_log1p_cols %||% character(0)
    if (inherits(spec, "lm")) {
      attr(spec, "log1p_cols") <- log1p_cols
      attr(spec, "error_type") <- rv$loglin_error_type %||% "OLS"
      return(spec)
    }
    pred_names <- intersect(colnames(rv$loglin_x_train), spec$pred_names)
    req(length(pred_names) == length(spec$pred_names))
    form       <- as.formula(paste("y ~", paste(bt(pred_names), collapse = " + ")))
    df_fit     <- cbind(y = rv$loglin_y_train, rv$loglin_x_train[, pred_names, drop = FALSE])
    fit_lm     <- lm(form, data = df_fit)
    if (!is.null(spec$mupe_summary)) {
      X_mat <- model.matrix(fit_lm)
      fit_lm$coefficients  <- spec$coefficients
      fit_lm$fitted.values <- as.vector(X_mat %*% ifelse(is.na(spec$coefficients), 0, spec$coefficients))
      fit_lm$residuals     <- df_fit$y - fit_lm$fitted.values
      attr(fit_lm, "mupe_summary") <- spec$mupe_summary
      attr(fit_lm, "mupe_weights") <- spec$weights
      class(fit_lm) <- c("mupe_loglin", class(fit_lm))
    }
    attr(fit_lm, "log1p_cols") <- log1p_cols
    attr(fit_lm, "irls_converged") <- spec$converged; attr(fit_lm, "irls_iterations") <- spec$iterations
    attr(fit_lm, "error_type") <- rv$loglin_error_type %||% "OLS"
    fit_lm
  })
  
  selected_linear_model <- reactive({
    req(input$selected_linear_simple_models_rows_selected)

    filtered_df_second <- filtered_linear_models_data_second()
    req(nrow(filtered_df_second) >= input$selected_linear_simple_models_rows_selected)

    selected_model_name_second <- rownames(filtered_df_second)[input$selected_linear_simple_models_rows_selected]

    pushed <- rv$linear_tuning_pushed_model
    if (!is.null(pushed) && identical(attr(pushed, "pushed_for"), selected_model_name_second)) return(pushed)

    model_index_second <- match(selected_model_name_second, names(rv$model_list_linear_simple))
    req(!is.na(model_index_second))

    spec <- rv$model_list_linear_simple[[model_index_second]]
    if (inherits(spec, "lm")) {
      attr(spec, "error_type") <- rv$linear_error_type %||% "OLS"
      return(spec)
    }
    pred_names <- intersect(colnames(rv$linear_x_train), spec$pred_names)
    req(length(pred_names) == length(spec$pred_names))
    form       <- as.formula(paste("y ~", paste(bt(pred_names), collapse = " + ")))
    df_fit     <- cbind(y = rv$linear_y_train, rv$linear_x_train[, pred_names, drop = FALSE])
    if (is.null(spec$mupe_summary) && !is.null(spec$weights) && isFALSE(spec$converged))
      spec <- irls_continue(spec, model.matrix(form, df_fit), df_fit$y)
    fit_lm     <- lm(form, data = df_fit, weights = spec$weights)
    if (!is.null(spec$mupe_summary)) {
      X_mat <- model.matrix(fit_lm)
      fit_lm$coefficients  <- spec$coefficients
      fit_lm$fitted.values <- as.vector(X_mat %*% ifelse(is.na(spec$coefficients), 0, spec$coefficients))
      fit_lm$residuals     <- df_fit$y - fit_lm$fitted.values
      attr(fit_lm, "mupe_summary") <- spec$mupe_summary
      class(fit_lm) <- c("mupe_loglin", class(fit_lm))
    } else if (!is.null(spec$weights)) {
      X_mat <- model.matrix(fit_lm)
      fit_lm$coefficients  <- setNames(spec$coefficients, names(coef(fit_lm)))
      fit_lm$fitted.values <- setNames(as.vector(X_mat %*% ifelse(is.na(spec$coefficients), 0, spec$coefficients)), rownames(X_mat))
      fit_lm$residuals     <- setNames(df_fit$y - fit_lm$fitted.values, rownames(X_mat))
      sm  <- summary.lm(fit_lm)
      w_s <- spec$weights; r_s <- fit_lm$residuals
      ybw <- sum(w_s * df_fit$y) / sum(w_s)
      r2s <- 1 - sum(w_s * r_s^2) / sum(w_s * (df_fit$y - ybw)^2)
      p_s <- sm$df[1] - 1L; d_s <- sm$df[2]
      f_s <- if (p_s > 0 && d_s > 0 && is.finite(r2s) && r2s < 1) (r2s / p_s) / ((1 - r2s) / d_s) else NA_real_
      attr(fit_lm, "mupe_summary") <- list(
        coefficients = sm$coefficients, r.squared = r2s,
        adj.r.squared = 1 - (1 - r2s) * (length(r_s) - 1L) / d_s,
        sigma = sm$sigma, df = sm$df,
        fstatistic = if (is.finite(f_s)) c(value = f_s, numdf = p_s, dendf = d_s) else NULL,
        cov.unscaled = sm$cov.unscaled, aliased = sm$aliased)
      class(fit_lm) <- c("mupe_loglin", class(fit_lm))
    }
    attr(fit_lm, "irls_converged") <- spec$converged; attr(fit_lm, "irls_iterations") <- spec$iterations
    attr(fit_lm, "error_type") <- rv$linear_error_type %||% "OLS"
    fit_lm
  })

  observeEvent(input$simplify_bttn, {
    shinyjs::disable("simplify_bttn")
    shinyjs::html("simplify_bttn", "Running...")
    on.exit({ shinyjs::enable("simplify_bttn"); shinyjs::html("simplify_bttn", "Simplify Model") })
    req(rv$active_dataset, input$ols_response, input$ols_predictors)
    n_keep <- input$simplify_n_vars
    if (!isTRUE(is.numeric(n_keep) && length(n_keep) == 1L && is.finite(n_keep) && n_keep >= 1)) {
      showNotification("Enter a whole number of at least 1 for Top N Unique Variables.",
                       type = "warning", duration = 6)
      return(invisible(NULL))
    }
    n_keep <- floor(n_keep)

    y_name <- input$ols_response
    x_pool <- input$ols_predictors
    
    if(length(x_pool) < 2) {
      showNotification("Select at least 2 predictors to run simplification.", type = "warning")
      return()
    }
    if (choose(length(x_pool), 2L) > 800) {
      showNotification(
        paste0("Simplify would fit ", format(length(x_pool) + choose(length(x_pool), 2L), big.mark = ","),
               " models for ", length(x_pool), " predictors — capped at 800 pairwise ",
               "combinations (40 predictors). Reduce the predictor selection."),
        type = "error", duration = 12
      )
      return()
    }
    
    src_simplify <- rv$dataset_library[[input$data_select_cer_generation]] %||% rv$active_dataset
    req(all(c(y_name, x_pool) %in% colnames(src_simplify)))
    data_work <- na.omit(src_simplify[, c(y_name, x_pool)])
    results_list <- list()
    vars_list    <- list()
    
    withProgress(message = 'Simplifying Data...', value = 0, {
      
      for (var in x_pool) {
        form <- as.formula(paste0(bt(y_name), " ~ ", bt(var)))
        fit <- try(lm(form, data = data_work), silent = TRUE)
        
        if (!inherits(fit, "try-error")) {
          s <- summary(fit)
          vars_list[[length(vars_list) + 1]] <- var
          results_list[[length(results_list) + 1]] <- data.frame(
            Idx = length(vars_list),
            R2 = s$r.squared, 
            SE = s$sigma, 
            stringsAsFactors = FALSE
          )
        }
      }
      incProgress(0.3, detail = "Mono-variable tests complete")
      
      combos <- combn(x_pool, 2)
      total_combos <- ncol(combos)
      
      for (i in 1:total_combos) {
        v1 <- combos[1, i]
        v2 <- combos[2, i]
        
        form <- as.formula(paste0(bt(y_name), " ~ ", bt(v1), " + ", bt(v2)))
        fit <- try(lm(form, data = data_work), silent = TRUE)
        
        if (!inherits(fit, "try-error")) {
          s <- summary(fit)
          vars_list[[length(vars_list) + 1]] <- c(v1, v2)
          results_list[[length(results_list) + 1]] <- data.frame(
            Idx = length(vars_list),
            R2 = s$r.squared, 
            SE = s$sigma, 
            stringsAsFactors = FALSE
          )
        }
        
        if(i %% 20 == 0) incProgress(0.5 * (i / total_combos))
      }
    })
    
    all_metrics <- do.call(rbind, results_list)

    if (is.null(all_metrics) || nrow(all_metrics) == 0) {
      showNotification(
        "Simplify Model: no candidate model could be fit (too few complete rows across the selected variables).",
        type = "error", duration = 8
      )
      return()
    }

    all_metrics <- all_metrics[order(-all_metrics$R2, all_metrics$SE), ]
    
    useful_vars <- c()
    for(i in seq_len(nrow(all_metrics))) {
      vars_in_eqn <- vars_list[[all_metrics$Idx[i]]]
      useful_vars <- unique(c(useful_vars, vars_in_eqn))
      
      if(length(useful_vars) >= n_keep) break
    }
    
    final_survivors <- head(useful_vars, n_keep)
    
    simplified_name <- paste0(input$data_select_cer_generation, " (Simplified)")

    cer_src <- rv$dataset_library[[input$data_select_cer_generation]]
    req(cer_src)
    keep_cols <- intersect(c(y_name, final_survivors), names(cer_src))
    store_dataset(simplified_name, cer_src[, keep_cols, drop = FALSE])
    
    updateSelectInput(session, "data_select", 
                      choices = names(rv$dataset_library), 
                      selected = input$data_select)
    
    updateSelectInput(session, "data_select_cer_generation", 
                      choices = names(rv$dataset_library), 
                      selected = simplified_name)
    
    showNotification(paste("Success! Created dataset with top", length(final_survivors), "predictors."), type = "message")
  })
  
  
  
  
  rv_vif <- reactiveValues(removed = character(0))

  vif_btn_id <- function(var, kind, i, slot = "") {
    paste0("remove_", gsub("[^A-Za-z0-9_]", "_", var), "_", kind, "_", i,
           if (nzchar(slot)) paste0("_", slot) else "")
  }
  vif_remove_handles <- list()
  
  observeEvent(input$test_vif, {
    shinyjs::disable("test_vif")
    shinyjs::html("test_vif", "Running...")
  })
  observe({
    req(input$test_vif > 0)
    vif_conflicts()
    shinyjs::enable("test_vif")
    shinyjs::html("test_vif", "Check for collinearity")
  })

  vif_conflicts <- eventReactive(input$test_vif, {
    req(input$data_select, nzchar(input$data_select))
    data <- rv$dataset_library[[input$data_select]]
    req(!is.null(data))
    thr <- input$vif_threshold
    if (!isTRUE(is.numeric(thr) && length(thr) == 1L && is.finite(thr) && thr > 0 && thr <= 1)) {
      showNotification("Enter an R-squared threshold between 0 and 1 (for example 0.93).",
                       type = "warning", duration = 6)
      shinyjs::enable("test_vif")
      shinyjs::html("test_vif", "Check for collinearity")
      req(FALSE)
    }

    convert_to_numeric <- function(x) {
      if (is.factor(x)) {
        if(nlevels(droplevels(x)) == 2L) {
          return(as.numeric(x == levels(droplevels(x))[2]))
        } else {
          return(as.numeric(as.character(x)))
        }
      } else {
        return(x)
      }
    }

    rv_vif$removed <- character(0)
    
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    dummy_vars <- names(data)[sapply(data, function(x) is.factor(x) && nlevels(droplevels(x)) == 2L)]
    
    all_numeric <- union(numeric_vars, dummy_vars)
    
    conflicts <- list()
    n_vars <- length(all_numeric)

    withProgress(message = "Testing for multicollinearity...", value = 0, {

      incProgress(0.05, detail = "Preparing data...")
      vecs <- vapply(all_numeric, function(v) convert_to_numeric(data[[v]]), numeric(nrow(data)))
      if (!is.matrix(vecs)) vecs <- matrix(vecs, ncol = 1)
      colnames(vecs) <- all_numeric
      keep_cols   <- colMeans(!is.na(vecs)) >= 0.5
      vecs        <- vecs[, keep_cols, drop = FALSE]
      all_numeric <- all_numeric[keep_cols]
      n_vars      <- ncol(vecs)
      vecs <- vecs[complete.cases(vecs), , drop = FALSE]
      n_obs <- nrow(vecs)
      if (n_obs < 2 || n_vars < 1) return(conflicts)
      sd_vecs <- apply(vecs, 2, sd, na.rm = TRUE)

      incProgress(0.15, detail = "Pairwise correlation test...")
      if (n_vars > 1) {
        cor_mat <- cor(vecs, use = "complete.obs")
        for (i in 1:(n_vars - 1)) {
          for (j in (i + 1):n_vars) {
            r <- cor_mat[i, j]
            if (!is.na(r) && r^2 >= thr) {
              msg <- paste("Variables", all_numeric[i], "and", all_numeric[j],
                           "are highly collinear (pairwise R² =", round(r^2, 4), ").")
              conflicts[[length(conflicts) + 1]] <- list(
                type    = "pairwise",
                var1    = all_numeric[i],
                var2    = all_numeric[j],
                r2_pair = r^2,
                message = msg
              )
            }
          }
        }
      }

      run_product_scan <- n_vars >= 3
      if (run_product_scan) {
        n_pairs_est <- choose(n_vars, 2)
        if (as.double(n_obs) * n_pairs_est > 5e7) {
          run_product_scan <- FALSE
          showNotification(
            paste0("Product-collinearity scan skipped: ", n_vars, " variables × ",
                   format(n_obs, big.mark = ","), " rows would need ",
                   format(n_pairs_est, big.mark = ","),
                   " product columns. Pairwise and multivariate checks still ran."),
            type = "warning", duration = 10
          )
        }
      }
      if (run_product_scan) {
        incProgress(0.1, detail = "Building product vectors...")

        pair_idx <- combn(n_vars, 2)
        n_pairs  <- ncol(pair_idx)

        prod_mat <- vecs[, pair_idx[1, ]] * vecs[, pair_idx[2, ]]
        if (is.vector(prod_mat)) prod_mat <- matrix(prod_mat, ncol = 1)

        incProgress(0.1, detail = "Computing all product correlations...")

        ok_x    <- sd_vecs > 0
        sd_prod <- apply(prod_mat, 2, sd)
        ok_p    <- sd_prod > 0

        if (any(ok_x) && any(ok_p) && n_obs > 1) {
          vecs_s <- scale(vecs[, ok_x, drop = FALSE])
          prod_s <- scale(prod_mat[, ok_p, drop = FALSE])

          cor_all <- (t(vecs_s) %*% prod_s) / (n_obs - 1)

          x_map <- which(ok_x)
          p_map <- which(ok_p)

          incProgress(0.5, detail = "Scanning for product relationships...")

          for (ri in seq_len(nrow(cor_all))) {
            xi <- x_map[ri]
            for (ci in seq_len(ncol(cor_all))) {
              pi  <- p_map[ci]
              yj  <- pair_idx[1, pi]
              zk  <- pair_idx[2, pi]
              if (xi == yj || xi == zk) next
              r2 <- cor_all[ri, ci]^2
              if (!is.na(r2) && r2 >= thr) {
                msg <- paste("Detected that", all_numeric[xi],
                             "is nearly equal to a constant times",
                             all_numeric[yj], "*", all_numeric[zk],
                             "(R² =", round(r2, 4), ").")
                conflicts[[length(conflicts) + 1]] <- list(
                  type = "product",
                  X    = all_numeric[xi],
                  Y    = all_numeric[yj],
                  Z    = all_numeric[zk],
                  r2   = r2,
                  message = msg
                )
              }
            }
          }
        }
      } else {
        incProgress(0.75)
      }

      if (isTRUE(input$vif_multivariate) && n_vars >= 3 && n_obs > n_vars) {
        for (i in seq_len(n_vars)) {
          if (!is.finite(sd_vecs[i]) || sd_vecs[i] == 0) next
          yv  <- vecs[, i]
          Xo  <- cbind(1, vecs[, -i, drop = FALSE])
          f   <- tryCatch(.lm.fit(Xo, yv), error = function(e) NULL)
          if (is.null(f)) next
          ssr  <- sum(f$residuals^2)
          sst  <- sum((yv - mean(yv))^2)
          r2_i <- if (sst > 0) 1 - ssr / sst else NA_real_
          if (!is.na(r2_i) && r2_i >= thr) {
            vif_i <- if (r2_i < 1) 1 / (1 - r2_i) else Inf
            conflicts[[length(conflicts) + 1]] <- list(
              type = "multivariate",
              var  = all_numeric[i],
              r2   = r2_i,
              vif  = vif_i,
              message = paste0(all_numeric[i],
                " is a near-linear combination of the other predictors (R² = ",
                round(r2_i, 4), ", VIF = ",
                if (is.finite(vif_i)) round(vif_i, 1) else "∞", ").")
            )
          }
        }
      }

    })

    attr(conflicts, "dataset") <- input$data_select
    conflicts
  })

  vif_current_conflicts <- reactive({
    cf <- vif_conflicts()
    req(identical(attr(cf, "dataset"), input$data_select))
    cf
  })
  observeEvent(input$data_select, {
    if (length(rv_vif$removed)) rv_vif$removed <- character(0)
  }, ignoreInit = TRUE)

  vif_sorted_conflicts <- reactive({
    conflicts <- vif_current_conflicts()
    if (length(conflicts) == 0) return(list(sorted = list(), var_counts = integer(0)))

    var_list <- character(0)
    for (c in conflicts) {
      if (c$type == "pairwise") {
        var_list <- c(var_list, c$var1, c$var2)
      } else if (c$type == "product") {
        var_list <- c(var_list, c$X, c$Y, c$Z)
      } else if (c$type == "multivariate") {
        var_list <- c(var_list, c$var)
      }
    }
    counts <- setNames(as.integer(table(var_list)), names(table(var_list)))

    sorted_idx <- order(
      sapply(conflicts, function(c) {
        vars <- if (c$type == "pairwise") c(c$var1, c$var2)
                else if (c$type == "product") c(c$X, c$Y, c$Z)
                else c$var
        max(counts[vars])
      }),
      sapply(conflicts, function(c) if (c$type == "pairwise") c$r2_pair else c$r2),
      decreasing = TRUE
    )

    list(sorted = conflicts[sorted_idx], var_counts = counts)
  })

  vif_severity_summary <- reactive({
    conflicts <- vif_current_conflicts()
    if (length(conflicts) == 0) return(NULL)

    n_pairwise     <- sum(sapply(conflicts, function(x) x$type == "pairwise"))
    n_product      <- sum(sapply(conflicts, function(x) x$type == "product"))
    n_multivariate <- sum(sapply(conflicts, function(x) x$type == "multivariate"))
    max_r2 <- max(sapply(conflicts, function(x) if (x$type == "pairwise") x$r2_pair else x$r2))

    severity <- if (max_r2 >= 0.95) "HIGH" else if (max_r2 >= 0.90) "MODERATE" else "LOW"

    list(severity = severity, n_pairwise = n_pairwise, n_product = n_product,
         n_multivariate = n_multivariate, max_r2 = max_r2)
  })

  output$vif_thr_equiv <- renderUI({
    thr <- input$vif_threshold
    if (!isTRUE(is.numeric(thr) && is.finite(thr) && thr > 0 && thr < 1)) return(NULL)
    tags$p(class = "c-bn-note", style = "margin-top:-6px;",
           sprintf("R² %.2f is VIF %.1f (VIF = 1 / (1 − R²)).", thr, 1 / (1 - thr)))
  })
  output$vifset_table <- renderUI({
    vars <- input$vifset_vars
    if (length(vars) < 2L) return(tags$p(class = "c-bn-note", "Pick at least two predictors."))
    data <- rv$dataset_library[[input$data_select]]
    req(!is.null(data), all(vars %in% names(data)))
    v <- vif_for_set(data[, vars, drop = FALSE], log = isTRUE(input$vifset_log))
    if (is.null(v))
      return(tags$p(class = "c-hint", "Not enough complete rows to measure VIF for these predictors."))
    verdict <- function(x) if (!is.finite(x)) "dup" else if (x >= 10) "high" else if (x >= 5) "watch" else "ok"
    vtxt <- c(ok = "Fine", watch = "Watch (5–10)", high = "High (10+)", dup = "Duplicate")
    top <- v[1, ]
    msg <- if (!is.finite(top$VIF)) tagList(tags$b(top$variable), " is an exact combination of the others: drop it or one of the variables it duplicates.")
           else if (top$VIF >= 10) tagList(tags$b(top$variable), sprintf(" has VIF %.1f: the others explain %s of it, so its coefficient will be unstable. Drop it or a variable it overlaps with.",
                                                                          top$VIF, if (is.na(top$R2)) "most" else paste0(round(100 * top$R2), "%")))
           else if (top$VIF >= 5) tagList("The highest VIF is ", tags$b(sprintf("%.1f", top$VIF)), " (", top$variable, "): worth watching, usually acceptable in a CER.")
           else tagList("Every VIF is below 5: these predictors do not overlap enough to cause trouble together.")
    logged <- attr(v, "logged")
    tagList(
      tags$div(class = "c-takeaway", msg),
      tags$table(class = "c-vif-table",
        tags$thead(tags$tr(tags$th("Predictor"), tags$th("VIF"), tags$th("Explained by the others"), tags$th(""))),
        tags$tbody(lapply(seq_len(nrow(v)), function(i) {
          k <- verdict(v$VIF[i])
          tags$tr(tags$td(v$variable[i], if (v$Df[i] > 1) tags$small(paste0(" (", v$Df[i] + 1, " levels)"))),
                  tags$td(if (is.finite(v$VIF[i])) sprintf("%.2f", v$VIF[i]) else "∞"),
                  tags$td(if (is.na(v$R2[i])) "—" else paste0(round(100 * v$R2[i], 1), "%")),
                  tags$td(tags$span(class = paste0("c-vif-pill c-vif-", k), vtxt[[k]])))
        }))),
      tags$p(class = "c-bn-note",
        sprintf("%d complete rows. ", attr(v, "n")),
        if (isTRUE(input$vifset_log)) {
          if (length(logged)) paste0("Logged: ", paste(logged, collapse = ", "), ". ") else "No column could be logged (zeros or negatives). "
        },
        "VIF = 1 / (1 − R²), where R² comes from regressing that predictor on the others you picked; a factor is graded as one term (GVIF^(1/Df)). Rules of thumb: under 5 fine, 5–10 watch, 10+ high."))
  })

  output$vif_summary_header <- renderUI({
    ran <- tryCatch({ vif_current_conflicts(); TRUE }, shiny.silent.error = function(e) FALSE)
    if (!ran) return(NULL)
    severity <- vif_severity_summary()
    conflicts <- vif_current_conflicts()
    if (length(conflicts) == 0) return(NULL)

    is_dark <- isTRUE(input$dark_mode)

    sev_colors <- list(
      HIGH    = list(bg = "#f8d7da", border = "#f5c6cb", text = "#721c24"),
      MODERATE = list(bg = "#fff3cd", border = "#ffeeba", text = "#856404"),
      LOW     = list(bg = "#d4edda", border = "#c3e6cb", text = "#155724")
    )
    sev <- sev_colors[[severity$severity]]

    txt_col <- if (is_dark) "#F0F0F5" else "#1A1A2E"
    txt_muted <- if (is_dark) "#8B949E" else "#4B5563"

    tagList(
      div(style = sprintf(
        "display:flex;align-items:center;gap:12px;margin-bottom:14px;
         padding:8px 14px;background:%s;border:1px solid %s;
         border-radius:6px;color:%s;",
        sev$bg, sev$border, sev$text
      ),
        tags$span(sprintf("SEVERITY: %s", severity$severity),
          style = sprintf("font-weight:700;font-size:11px;text-transform:uppercase;
                          background:%s;border-radius:3px;padding:3px 8px;color:%s;", sev$border, sev$text)),
        tags$span(sprintf("%d pairwise  \u00b7  %d product%s",
          severity$n_pairwise, severity$n_product,
          if (isTRUE(input$vif_multivariate)) sprintf("  \u00b7  %d multivariate (VIF)", severity$n_multivariate) else ""),
          style = sprintf("font-size:12px;font-weight:500;color:%s;", txt_muted)),
        tags$span(sprintf("Max R\u00b2 = %.4f", severity$max_r2),
          style = sprintf("font-size:12px;opacity:0.75;margin-left:auto;color:%s;", txt_muted))
      )
    )
  })

  output$vif_warnings <- renderUI({
    all_conflicts  <- vif_current_conflicts()
    result         <- vif_sorted_conflicts()
    sorted_conflicts <- result$sorted
    removed_vars   <- rv_vif$removed

    if (length(all_conflicts) == 0) {
      return(h5("No multicollinearity issues detected among numeric variables based on the current threshold."))
    }

    is_dark <- isTRUE(input$dark_mode)

    card_bg    <- if (is_dark) "#161B22" else "#FFFFFF"
    card_border <- if (is_dark) "#30363D" else "#E5E7EB"
    card_shadow <- if (is_dark) "0 1px 3px rgba(0,0,0,0.3)" else "0 1px 3px rgba(0,0,0,0.06)"
    txt_primary <- if (is_dark) "#F0F0F5" else "#1A1A2E"
    txt_secondary <- if (is_dark) "#8B949E" else "#4B5563"
    txt_muted   <- if (is_dark) "#8B949E" else "#9CA3AF"
    stat_bg    <- if (is_dark) "#1c2128" else "#F8F9FC"
    chip_green_bg  <- if (is_dark) "#1A3A1A" else "#e8f4e8"
    chip_amber_bg  <- if (is_dark) "#3D2E00" else "#fff3cd"

    var_list <- character(0)
    for (c in all_conflicts) {
      if (c$type == "pairwise") {
        var_list <- c(var_list, c$var1, c$var2)
      } else if (c$type == "product") {
        var_list <- c(var_list, c$X, c$Y, c$Z)
      } else if (c$type == "multivariate") {
        var_list <- c(var_list, c$var)
      }
    }
    counts_table <- table(var_list)
    var_counts <- setNames(as.integer(counts_table), names(counts_table))

    pairwise_pos <- list()
    product_pos  <- list()
    multivariate_pos <- list()
    seq_pairwise <- 0L
    seq_product  <- 0L
    seq_multivariate <- 0L
    for (i in seq_along(all_conflicts)) {
      c <- all_conflicts[[i]]
      if (c$type == "pairwise") {
        seq_pairwise <- seq_pairwise + 1L
        pairwise_pos[[paste(c$var1, c$var2, sep = "\x1f")]] <- seq_pairwise
      } else if (c$type == "product") {
        seq_product <- seq_product + 1L
        product_pos[[paste(c$X, c$Y, c$Z, sep = "\x1f")]] <- seq_product
      } else if (c$type == "multivariate") {
        seq_multivariate <- seq_multivariate + 1L
        multivariate_pos[[c$var]] <- seq_multivariate
      }
    }

    chip <- function(name, count, bg_col, txt_col, badge_col) {
      tags$span(
        name,
        style = sprintf(
          "color:%s;font-weight:600;background:%s;border-radius:2px;
           padding:1px 5px;font-size:12px;display:inline-block;margin:0 2px;
           box-shadow:0 1px 2px rgba(0,0,0,0.08);",
          txt_col, bg_col
        ),
        tags$span(as.character(count),
          style = sprintf("background:%s;color:white;border-radius:10px;
                          padding:1px 5px;font-size:10px;font-weight:700;
                          margin-left:3px;vertical-align:middle;", badge_col))
      )
    }

    card_list <- lapply(seq_along(sorted_conflicts), function(idx) {
      c <- sorted_conflicts[[idx]]

      if (c$type == "pairwise") {
        if (c$var1 %in% removed_vars || c$var2 %in% removed_vars) return(NULL)
        orig_i    <- pairwise_pos[[paste(c$var1, c$var2, sep = "\x1f")]]
        r2_val    <- c$r2_pair
        badge_col <- "#d9534f"
        card_accent <- "#d9534f"
        type_lbl  <- "Pairwise Collinearity"
        count_a   <- var_counts[[c$var1]]
        count_b   <- var_counts[[c$var2]]
        detail_txt <- sprintf("These two variables move together (R\u00b2 = %.4f)", r2_val)
        btn_a_id  <- vif_btn_id(c$var1, "pair", orig_i, "a")
        btn_b_id  <- vif_btn_id(c$var2, "pair", orig_i, "b")

        chip_a <- chip(c$var1, count_a, chip_green_bg, "#2d6e2d", badge_col)
        chip_b <- chip(c$var2, count_b, chip_green_bg, "#2d6e2d", badge_col)

        tags$div(
          style = sprintf(
            "border:1px solid %s;border-radius:6px;padding:12px 14px;
             background:%s;margin-bottom:10px;border-left:5px solid %s;
             box-shadow:%s;",
            card_border, card_bg, card_accent, card_shadow
          ),
          div(style = "display:flex;align-items:baseline;gap:8px;margin-bottom:8px;",
            tags$span(type_lbl,
              style = sprintf("font-size:10px;font-weight:700;text-transform:uppercase;
                              letter-spacing:0.4px;color:%s;", txt_muted)),
            tags$span(sprintf("R\u00b2 = %.4f", r2_val),
              style = sprintf("margin-left:auto;font-size:12px;font-weight:600;color:%s;", txt_primary))
          ),
          div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:6px;margin-bottom:10px;",
            chip_a,
            tags$span("\u2192", style = sprintf("font-size:14px;color:%s;", txt_muted)),
            chip_b
          ),
          tags$p(detail_txt,
            style = sprintf("font-size:11px;color:%s;margin:0 0 10px 0;font-style:italic;", txt_secondary)),
          div(style = "display:flex;gap:8px;",
            actionButton(btn_a_id, label = paste("Remove", c$var1), class = "btn-sm", style = "font-size:11px;"),
            actionButton(btn_b_id, label = paste("Remove", c$var2), class = "btn-sm", style = "font-size:11px;")
          )
        )

      } else if (c$type == "product") {
        if (c$X %in% removed_vars || c$Y %in% removed_vars || c$Z %in% removed_vars) return(NULL)
        orig_i    <- product_pos[[paste(c$X, c$Y, c$Z, sep = "\x1f")]]
        r2_val    <- c$r2
        badge_col <- "#c17d2a"
        card_accent <- "#c17d2a"
        type_lbl  <- "Product Relationship"
        count_y   <- var_counts[[c$Y]]
        count_z   <- var_counts[[c$Z]]
        count_x   <- var_counts[[c$X]]
        detail_txt <- sprintf("%s \u2248 %s \u00d7 %s (R\u00b2 = %.4f)", c$X, c$Y, c$Z, r2_val)
        btn_a_id  <- vif_btn_id(c$Y, "prod", orig_i, "a")
        btn_b_id  <- vif_btn_id(c$Z, "prod", orig_i, "b")

        chip_y  <- chip(c$Y, count_y, chip_green_bg, "#2d6e2d", badge_col)
        chip_z  <- chip(c$Z, count_z, chip_green_bg, "#2d6e2d", badge_col)
        chip_x  <- chip(c$X, count_x, chip_amber_bg, "#c17d2a", badge_col)

        tags$div(
          style = sprintf(
            "border:1px solid %s;border-radius:6px;padding:12px 14px;
             background:%s;margin-bottom:10px;border-left:5px solid %s;
             box-shadow:%s;",
            card_border, card_bg, card_accent, card_shadow
          ),
          div(style = "display:flex;align-items:baseline;gap:8px;margin-bottom:8px;",
            tags$span(type_lbl,
              style = sprintf("font-size:10px;font-weight:700;text-transform:uppercase;
                              letter-spacing:0.4px;color:%s;", txt_muted)),
            tags$span(sprintf("R\u00b2 = %.4f", r2_val),
              style = sprintf("margin-left:auto;font-size:12px;font-weight:600;color:%s;", txt_primary))
          ),
          div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:6px;margin-bottom:10px;",
            chip_y,
            tags$span("\u00d7", style = sprintf("font-size:14px;color:%s;", txt_muted)),
            chip_z,
            tags$span("\u2248", style = sprintf("font-size:14px;color:%s;", txt_muted)),
            chip_x
          ),
          tags$p(detail_txt,
            style = sprintf("font-size:11px;color:%s;margin:0 0 10px 0;font-style:italic;", txt_secondary)),
          div(style = "display:flex;gap:8px;",
            actionButton(btn_a_id, label = paste("Remove", c$Y), class = "btn-sm", style = "font-size:11px;"),
            actionButton(btn_b_id, label = paste("Remove", c$Z), class = "btn-sm", style = "font-size:11px;")
          )
        )

      } else {
        if (c$var %in% removed_vars) return(NULL)
        orig_i     <- multivariate_pos[[c$var]]
        r2_val     <- c$r2
        vif_val    <- c$vif
        card_accent <- "#8e44ad"
        badge_col  <- "#8e44ad"
        type_lbl   <- "Multivariate VIF"
        count_x    <- var_counts[[c$var]]
        vif_txt    <- if (is.finite(vif_val)) sprintf("%.1f", vif_val) else "∞"
        detail_txt <- sprintf("%s is a near-linear combination of the other predictors (VIF = %s)", c$var, vif_txt)
        btn_id     <- vif_btn_id(c$var, "mvif", orig_i)
        chip_x     <- chip(c$var, count_x, chip_amber_bg, "#8e44ad", badge_col)

        tags$div(
          style = sprintf(
            "border:1px solid %s;border-radius:6px;padding:12px 14px;
             background:%s;margin-bottom:10px;border-left:5px solid %s;
             box-shadow:%s;",
            card_border, card_bg, card_accent, card_shadow
          ),
          div(style = "display:flex;align-items:baseline;gap:8px;margin-bottom:8px;",
            tags$span(type_lbl,
              style = sprintf("font-size:10px;font-weight:700;text-transform:uppercase;
                              letter-spacing:0.4px;color:%s;", txt_muted)),
            tags$span(sprintf("VIF = %s  (R² = %.4f)", vif_txt, r2_val),
              style = sprintf("margin-left:auto;font-size:12px;font-weight:600;color:%s;", txt_primary))
          ),
          div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:6px;margin-bottom:10px;",
            chip_x,
            tags$span("≈ f(all other predictors)", style = sprintf("font-size:12px;color:%s;", txt_muted))
          ),
          tags$p(detail_txt,
            style = sprintf("font-size:11px;color:%s;margin:0 0 10px 0;font-style:italic;", txt_secondary)),
          div(style = "display:flex;gap:8px;",
            actionButton(btn_id, label = paste("Remove", c$var), class = "btn-sm", style = "font-size:11px;")
          )
        )
      }
    })

    card_list <- Filter(Negate(is.null), card_list)

    if (length(card_list) == 0) {
      tagList(
        div(style = sprintf("text-align:center;padding:20px;color:%s;font-size:13px;", txt_muted),
          h5("\u2713 All detected conflicts have been resolved via removal.",
            style = if (is_dark) "color:#3FB950;" else "color:#2d6e2d;"),
          p("Proceed to save your reduced dataset below.")
        )
      )
    } else {
      tagList(
        tags$div(style = sprintf("font-size:9px;font-weight:700;text-transform:uppercase;letter-spacing:0.5px;color:%s;margin-bottom:8px;", txt_muted),
          "Conflicts (sorted by severity)"
        ),
        do.call(tagList, card_list)
      )
    }
  })
  
  observe({
    conflicts <- vif_conflicts()

    for (h in vif_remove_handles) try(h$destroy(), silent = TRUE)
    vif_remove_handles <<- list()

    if (length(conflicts) == 0) return()

    add_remove_observer <- function(btn_id, var) {
      vif_remove_handles[[length(vif_remove_handles) + 1L]] <<- observeEvent(input[[btn_id]], {
        rv_vif$removed <- unique(c(rv_vif$removed, var))
      }, ignoreInit = TRUE, once = TRUE)
    }

    pairwise_conflicts <- Filter(function(x) x$type == "pairwise", conflicts)
    for (i in seq_along(pairwise_conflicts)) {
      local({
        idx <- i
        var1 <- pairwise_conflicts[[idx]]$var1
        var2 <- pairwise_conflicts[[idx]]$var2
        add_remove_observer(vif_btn_id(var1, "pair", idx, "a"), var1)
        add_remove_observer(vif_btn_id(var2, "pair", idx, "b"), var2)
      })
    }

    product_conflicts <- Filter(function(x) x$type == "product", conflicts)
    for (i in seq_along(product_conflicts)) {
      local({
        idx <- i
        Y <- product_conflicts[[idx]]$Y
        Z <- product_conflicts[[idx]]$Z
        add_remove_observer(vif_btn_id(Y, "prod", idx, "a"), Y)
        add_remove_observer(vif_btn_id(Z, "prod", idx, "b"), Z)
      })
    }

    multivariate_conflicts <- Filter(function(x) x$type == "multivariate", conflicts)
    for (i in seq_along(multivariate_conflicts)) {
      local({
        idx <- i
        v <- multivariate_conflicts[[idx]]$var
        add_remove_observer(vif_btn_id(v, "mvif", idx), v)
      })
    }
  })
  
  final_candidates <- reactive({
    req(input$data_select, nzchar(input$data_select))
    df <- rv$dataset_library[[input$data_select]]
    req(!is.null(df))
    numeric_vars <- names(df)[vapply(df, is.numeric, logical(1L))]
    dummy_vars   <- names(df)[vapply(df, function(x) is.factor(x) && nlevels(droplevels(x)) == 2L, logical(1L))]
    all_vars <- union(numeric_vars, dummy_vars)

    setdiff(all_vars, rv_vif$removed)
  })
  
  output$vif_output <- renderUI({
    req(rv$active_dataset)

    is_dark <- isTRUE(input$dark_mode)

    card_bg     <- if (is_dark) "#161B22" else "#FFFFFF"
    stat_bg     <- if (is_dark) "#1c2128" else "#F8F9FC"
    stat_border <- if (is_dark) "#30363D" else "#e0e8f4"
    txt_primary <- if (is_dark) "#F0F0F5" else "#1A1A2E"
    txt_muted   <- if (is_dark) "#8B949E" else "#9CA3AF"
    removed_txt  <- if (is_dark) "#F85149" else "#a94442"
    removed_bg  <- if (is_dark) "#3D1A1A" else "#f8d7da"
    removed_border <- if (is_dark) "#5C2020" else "#f5c6cb"
    remaining_txt <- if (is_dark) "#3FB950" else "#2d6e2d"
    remaining_bg  <- if (is_dark) "#1A3A1A" else "#e8f4e8"
    remaining_border <- if (is_dark) "#1E3D1E" else "transparent"

    req(input$data_select, nzchar(input$data_select))
    df_vif <- rv$dataset_library[[input$data_select]]
    req(!is.null(df_vif))
    numeric_vars <- names(df_vif)[vapply(df_vif, is.numeric, logical(1L))]
    dummy_vars   <- names(df_vif)[vapply(df_vif, function(x) is.factor(x) && nlevels(droplevels(x)) == 2L, logical(1L))]
    all_numeric  <- union(numeric_vars, dummy_vars)

    removed   <- rv_vif$removed
    remaining <- final_candidates()

    removed_chips <- if (length(removed) > 0) {
      lapply(removed, function(v) {
        tags$span(v,
          style = sprintf(
            "color:%s;background:%s;border:1px solid %s;border-radius:3px;
             padding:3px 9px;font-size:12px;text-decoration:line-through;
             margin:2px;display:inline-block;",
            removed_txt, removed_bg, removed_border
          )
        )
      })
    } else {
      list(tags$span("None", style = sprintf("color:%s;font-style:italic;font-size:12px;", txt_muted)))
    }

    remaining_chips <- if (length(remaining) > 0) {
      lapply(remaining, function(v) {
        tags$span(v,
          style = sprintf(
            "color:%s;font-weight:600;background:%s;border-radius:3px;
             padding:3px 9px;font-size:12px;margin:2px;display:inline-block;",
            remaining_txt, remaining_bg
          )
        )
      })
    } else {
      list(tags$span("None remaining", style = sprintf("color:%s;font-style:italic;font-size:12px;", txt_muted)))
    }

    tagList(
      div(style = sprintf(
        "display:flex;gap:20px;margin-bottom:14px;font-size:12px;color:%s;
         padding:6px 12px;background:%s;border-radius:5px;border:1px solid %s;",
        txt_primary, stat_bg, stat_border
      ),
        tags$span(sprintf("Total checked: %d", length(all_numeric)),
          style = sprintf("font-weight:500;color:%s;", txt_primary)),
        tags$span(sprintf("Removed: %d", length(removed)),
          style = sprintf("font-weight:600;color:%s;",
            if (length(removed) > 0) removed_txt else txt_muted)),
        tags$span(sprintf("Final candidates: %d", length(remaining)),
          style = sprintf("font-weight:600;color:%s;",
            if (length(remaining) > 0) remaining_txt else txt_muted))
      ),

      div(style = "margin-bottom:14px;",
        tags$div(style = sprintf("font-size:9px;font-weight:700;text-transform:uppercase;
                         letter-spacing:0.5px;color:%s;margin-bottom:6px;", removed_txt),
          "Removed Variables"),
        div(style = "display:flex;flex-wrap:wrap;gap:5px;", removed_chips)
      ),

      div(
        tags$div(style = sprintf("font-size:9px;font-weight:700;text-transform:uppercase;
                         letter-spacing:0.5px;color:%s;margin-bottom:6px;", remaining_txt),
          "Final Candidate Set"),
        div(style = "display:flex;flex-wrap:wrap;gap:5px;", remaining_chips)
      ),

      tags$small("Note: Categorical variables are automatically preserved and not listed here.",
        style = sprintf("color:%s;font-size:10px;margin-top:10px;display:block;", txt_muted))
    )
  })
  
  observeEvent(input$save_reduced_data, {
    shinyjs::disable("save_reduced_data")
    shinyjs::html("save_reduced_data", "Saving...")
    on.exit({ shinyjs::enable("save_reduced_data"); shinyjs::html("save_reduced_data", "Save Reduced Collinearity Data") })
    req(input$data_select, nzchar(input$data_select))
    src_data <- rv$dataset_library[[input$data_select]]
    req(!is.null(src_data))
    req(input$vif_new_name)

    cols_to_keep <- setdiff(names(src_data), rv_vif$removed)

    new_data <- src_data[, cols_to_keep, drop = FALSE]
    
    store_dataset(input$vif_new_name, new_data)
    
    update_all_dataset_selectors(input$vif_new_name)
    
    showNotification(paste(input$vif_new_name, "saved successfully."), type = "message")
  })

  observeEvent(input$data_select, {
    df <- rv$dataset_library[[input$data_select]]
    if (!is.null(df)) {
      vars <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "bn_included_vars", choices = vars, selected = vars)
    }
    rv$bn_result        <- NULL
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  observeEvent(input$bn_select_all_vars, {
    req(input$data_select)
    df <- rv$dataset_library[[input$data_select]]
    if (is.null(df)) return()
    vars <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "bn_included_vars", selected = vars)
  })

  observeEvent(input$bn_generate, {
    req(input$data_select)
    included <- input$bn_included_vars
    if (is.null(included) || length(included) < 2) {
      showNotification("Select at least 2 variables to generate a network.", type = "warning")
      return()
    }
    df <- rv$dataset_library[[input$data_select]]
    df <- df[, included, drop = FALSE]
    df <- df[complete.cases(df), , drop = FALSE]
    df <- as.data.frame(lapply(df, function(x) as.numeric(x)))
    n_obs  <- nrow(df)
    n_boot <- input$bn_bootstrap
    if (n_obs < 5) {
      showNotification("Not enough complete cases to learn a network.", type = "error")
      return()
    }
    rv$bn_result        <- NULL
    withProgress(message = paste("Running", n_boot, "bootstrap samples..."), value = 0.1, {
      tryCatch({
        bootstrap <- withr::with_seed(42, ceres_ns("bnlearn")$boot.strength(
          df, R = n_boot, algorithm = "hc",
          algorithm.args = list(restart = 5, perturb = 5, max.iter = 500)
        ))
        setProgress(0.85, detail = "Averaging network structure...")
        edges_df <- as.data.frame(bootstrap)
        edges_df <- edges_df[edges_df$direction >= 0.5, ]
        lk <- setNames(included, names(df))
        edges_df$from <- unname(lk[as.character(edges_df$from)])
        edges_df$to   <- unname(lk[as.character(edges_df$to)])
        setProgress(1.0)
        rv$bn_result <- list(edges_df = edges_df, n_obs = n_obs, n_boot = n_boot, vars = included)
      }, error = function(e) {
        showNotification(paste("Network learning failed:", conditionMessage(e)), type = "error")
      })
    })
  })

  bn_hint <- function() ceres_empty_state("Pick variables in the sidebar, then click Generate Network.")
  bn_all_vars <- reactive({
    req(rv$bn_result)
    v <- rv$bn_result$vars %||% character(0)
    e <- rv$bn_result$edges_df
    unique(c(v, as.character(e$from), as.character(e$to)))
  })
  bn_cut <- reactive((input$bn_threshold %||% 40) / 100)
  bn_pct <- function(x) paste0(round(100 * x), "%")

  output$bn_focus_body <- renderUI({
    if (is.null(rv$bn_result)) return(bn_hint())
    tagList(uiOutput("bn_focus_pick_ui"),
            fluidRow(column(5, uiOutput("bn_focus_ui")), column(7, uiOutput("bn_focus_summary"))))
  })
  output$bn_focus_pick_ui <- renderUI({
    if (is.null(rv$bn_result)) return(NULL)
    vars <- bn_all_vars()
    resp <- isolate(input$ols_response)
    rs   <- isolate(rv$bn_restore)
    if (!is.null(rs)) isolate(rv$bn_restore <- NULL)
    dflt <- if (length(rs$focus) == 1 && rs$focus %in% vars) rs$focus else
            if (length(resp) == 1 && resp %in% vars) resp else
            if (any(grepl("$", vars, fixed = TRUE))) vars[grepl("$", vars, fixed = TRUE)][1] else vars[1]
    tags$div(class = "c-map-bar",
      tags$span(class = "c-map-label", "Cost to explain"),
      selectInput("bn_focus", NULL, choices = vars, selected = dflt, width = "260px"))
  })
  bn_focus_links <- reactive({
    req(rv$bn_result, input$bn_focus)
    bn_direct_links(rv$bn_result$edges_df, input$bn_focus, bn_cut())
  })
  output$bn_focus_ui <- renderUI({
    if (is.null(rv$bn_result)) return(bn_hint())
    req(input$bn_focus)
    d <- bn_focus_links(); f <- input$bn_focus
    n_other <- length(setdiff(bn_all_vars(), c(f, d$var)))
    if (!nrow(d))
      return(tags$div(class = "c-takeaway",
        tags$b(f), " has no direct link at the ", bn_pct(bn_cut()), " cutoff. ",
        "Lower the confidence cutoff in the sidebar to see weaker links."))
    tagList(
      tags$div(class = "c-takeaway",
        tags$b(f), " is directly linked to ", tags$b(paste(nrow(d), if (nrow(d) == 1) "variable" else "variables"), .noWS = "after"),
        paste0(". The other ", n_other, " relate to it only through other variables, so once these are in a CER they add little. ",
               "Untick any that are just another version of the same cost.")),
      checkboxGroupInput("bn_focus_pick", NULL,
        choiceNames = lapply(seq_len(nrow(d)), function(i) tags$span(class = "c-bn-pick",
          tags$span(class = "c-bn-pick-name", d$var[i]), tags$span(class = "c-bn-leader"),
          tags$span(class = "c-bn-bar", tags$span(style = paste0("width:", round(100 * d$strength[i]), "%"))),
          tags$span(class = "c-bn-pct", bn_pct(d$strength[i])))),
        choiceValues = d$var, selected = d$var),
      actionButton("bn_use_predictors", "Use ticked as predictors", icon = icon("arrow-right"), class = "btn-primary"),
      tags$p(class = "c-bn-note", "Bars: share of the ", rv$bn_result$n_boot,
             " resamples in which the two were linked. Links say two variables move together, not which causes which."))
  })
  output$bn_focus_summary <- renderUI({
    if (is.null(rv$bn_result)) return(NULL)
    req(input$bn_focus)
    d <- bn_focus_links(); f <- input$bn_focus; nb <- rv$bn_result$n_boot
    if (!nrow(d)) return(NULL)
    word <- function(x) if (x >= 0.8) "strong" else if (x >= 0.6) "moderate" else "weaker"
    times <- function(x) paste0(round(x * nb), " of ", nb, " resamples")
    costy <- grepl("$", d$var, fixed = TRUE)
    pred  <- d[!costy, , drop = FALSE]; oth <- d[costy, , drop = FALSE]
    indirect <- setdiff(bn_all_vars(), c(f, d$var))
    item <- function(head, body) tags$div(class = "c-bn-sum-item", tags$div(class = "c-bn-sum-head", head), tags$div(body))
    tags$div(class = "c-bn-summary",
      tags$div(class = "c-bn-sum-title", "What this means"),
      if (nrow(pred)) item("Strongest driver",
        tagList(tags$b(pred$var[1]), paste0(" is linked to ", f, " in ", times(pred$strength[1]),
                                            " — a ", word(pred$strength[1]), " link. Start a CER with it."))),
      if (nrow(pred) > 1) item("Also worth trying",
        tagList(lapply(seq_len(nrow(pred))[-1], function(i) tags$span(class = "c-bn-sum-line",
          tags$b(pred$var[i]), paste0(" — ", word(pred$strength[i]), ", ", times(pred$strength[i])))))),
      if (nrow(oth)) item("Other costs, not predictors",
        tagList(paste(oth$var, collapse = ", "),
                " move with it because they are costs too (they carry a $ unit). Leave them out of the predictors.")),
      if (length(indirect)) item("Linked only indirectly",
        paste0(length(indirect), if (length(indirect) == 1) " variable" else " variables",
               " (", paste(utils::head(indirect, 4), collapse = ", "), if (length(indirect) > 4) ", …", ")",
               " relate to ", f, " only through the ones above, so they add little once those are in the model.")),
      if (nrow(pred)) item("Suggested starting CER",
        tags$code(class = "c-bn-sum-eq", paste0(f, " ~ ", paste(pred$var, collapse = " + ")))))
  })

  observeEvent(input$bn_use_predictors, {
    f <- input$bn_focus; picks <- input$bn_focus_pick %||% character(0)
    if (!length(picks)) { showNotification("Tick at least one variable first.", type = "warning"); return() }
    if (!identical(input$data_select_cer_generation, input$data_select)) {
      showNotification(paste0("CER Generation is using a different dataset. Choose “", input$data_select,
                              "” there first."), type = "warning", duration = 8)
      return()
    }
    cols <- names(rv$dataset_library[[input$data_select]])
    updateSelectInput(session, "ols_response", selected = f)
    updateSelectInput(session, "ols_predictors", choices = setdiff(cols, f), selected = picks)
    updateNavbarPage(session, "tabs", selected = "CER Generation")
    showNotification(paste0("Response ", f, " with ", length(picks), " predictor(s) set in CER Generation."),
                     type = "message", duration = 5)
  })

  output$bn_groups_ui <- renderUI({
    if (is.null(rv$bn_result)) return(bn_hint())
    cut <- max(0.8, bn_cut())
    vars <- bn_all_vars(); e <- rv$bn_result$edges_df
    g <- bn_groups(e, vars, cut)
    alone <- setdiff(vars, unlist(g))
    m <- bn_link_matrix(e, vars)
    cards <- lapply(seq_along(g), function(i) {
      v <- g[[i]]
      sub <- m[v, v, drop = FALSE]; sub[sub < cut] <- NA
      best <- vapply(v, function(x) max(sub[x, ], na.rm = TRUE), numeric(1))
      tags$div(class = "c-bn-group",
        tags$div(class = "c-bn-group-head", paste("Group", i), tags$span(paste(length(v), "variables"))),
        tags$div(class = "c-bn-chips", lapply(v[order(-best)], function(x)
          tags$span(class = "c-bn-chip", x, tags$small(bn_pct(best[[x]]))))))
    })
    tagList(
      tags$div(class = "c-takeaway",
        if (length(g)) tagList(tags$b(length(g)), if (length(g) == 1) " group" else " groups",
                               " of variables travel together (linked in at least ", bn_pct(cut), " of resamples). ",
                               "Two predictors from the same group carry much the same information, so a CER rarely needs both; ",
                               "a cost that shares a group with predictors is a sign those predictors explain it.")
        else tagList("No variables are linked in at least ", bn_pct(cut), " of resamples, so none clearly duplicate each other.")),
      tags$div(class = "c-bn-groups", cards),
      if (length(alone)) tags$div(class = "c-bn-alone",
        tags$span(class = "c-map-label", "Stand alone"),
        tags$div(class = "c-bn-chips", lapply(alone, function(x) tags$span(class = "c-bn-chip c-bn-chip-quiet", x)))),
      tags$p(class = "c-bn-note", "The percentage on each name is its strongest link inside the group."))
  })

  bn_matrix_order <- reactive({
    vars <- bn_all_vars(); m <- bn_link_matrix(rv$bn_result$edges_df, vars)
    if (length(vars) < 3) return(list(m = m, vars = vars))
    hc <- stats::hclust(stats::as.dist(1 - m), method = "average")
    list(m = m[hc$order, hc$order, drop = FALSE], vars = vars[hc$order])
  })
  output$bn_matrix_note <- renderUI({
    if (is.null(rv$bn_result)) return(bn_hint())
    tags$div(class = "c-takeaway",
      "Each square is how often two variables were linked across the ", rv$bn_result$n_boot,
      " resamples (darker = more often); pairs below the ", bn_pct(bn_cut()), " cutoff are left blank. ",
      "Variables are ordered so that those linked to each other sit together: a dark block on the diagonal is a group, ",
      "and a row with almost no colour is a variable that stands on its own.")
  })
  output$bn_matrix_box <- renderUI({
    if (is.null(rv$bn_result)) return(NULL)
    n <- length(bn_all_vars())
    plotlyOutput("bn_link_matrix", height = paste0(max(420, 120 + 30 * n), "px"))
  })
  output$bn_link_matrix <- renderPlotly({
    req(rv$bn_result)
    o <- bn_matrix_order(); m <- o$m; v <- o$vars
    diag(m) <- NA
    m[!is.na(m) & m < bn_cut()] <- NA
    plot_ly(x = v, y = v, z = m, type = "heatmap", zmin = 0, zmax = 1,
            colorscale = list(c(0, "#F8FAFC"), c(0.4, "#BFD3FB"), c(1, "#2F6FE4")),
            xgap = 2, ygap = 2,
            text = outer(v, v, function(a, b) paste0(b, " ↔ ", a)),
            hovertemplate = "%{text}<br>linked in %{z:.0%} of resamples<extra></extra>",
            colorbar = list(title = list(text = "Linked", font = list(size = 11)), thickness = 10, len = 0.8,
                            outlinewidth = 0, tickformat = ".0%")) %>%
      layout(title = list(text = "How often each pair was linked"),
             xaxis = list(tickangle = -40, showgrid = FALSE, categoryorder = "array", categoryarray = v),
             yaxis = list(autorange = "reversed", showgrid = FALSE, categoryorder = "array", categoryarray = v),
             margin = list(t = 58, l = 130, r = 20, b = 120)) %>%
      config(displayModeBar = FALSE)
  })

  observeEvent(input$explore_infl_select_all, {
    req(rv$active_dataset, input$explore_infl_response)
    all_vars        <- colnames(rv$active_dataset)
    valid_predictors <- setdiff(all_vars, input$explore_infl_response)
    updateSelectInput(session, "explore_infl_predictors", selected = valid_predictors)
  })

  observeEvent(input$explore_infl_response, {
    req(input$data_select)
    data <- rv$dataset_library[[input$data_select]]
    req(!is.null(data))
    valid_choices <- setdiff(colnames(data), input$explore_infl_response)
    updateSelectInput(session, "explore_infl_predictors",
                      choices  = valid_choices,
                      selected = intersect(input$explore_infl_predictors, valid_choices))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$explore_infl_compute, {
    shinyjs::disable("explore_infl_compute")
    on.exit(shinyjs::enable("explore_infl_compute"))
    req(input$data_select, input$explore_infl_response, input$explore_infl_predictors)

    resp  <- input$explore_infl_response
    preds <- input$explore_infl_predictors
    dat   <- rv$dataset_library[[input$data_select]]
    req(dat)
    y_raw <- dat[[resp]]

    rv$explore_linear_influence_data  <- NULL
    rv$explore_linear_influence_n_par <- NULL
    rv$explore_linear_ui_exclusions   <- character(0)
    rv$explore_loglin_influence_data  <- NULL
    rv$explore_loglin_influence_n_par <- NULL
    rv$explore_loglin_ui_exclusions   <- character(0)

    fit_data_lin <- dat[, c(resp, preds), drop = FALSE]
    fit_data_lin <- fit_data_lin[complete.cases(fit_data_lin), , drop = FALSE]
    names(fit_data_lin)[1] <- "y"
    model_lin <- tryCatch(lm(y ~ ., data = fit_data_lin), error = function(e) NULL)
    if (!is.null(model_lin)) {
      row_lbl_lin <- rownames(fit_data_lin)
      if (is.null(row_lbl_lin)) row_lbl_lin <- as.character(seq_len(nrow(fit_data_lin)))
      rv$explore_linear_influence_data <- data.frame(
        row_label    = row_lbl_lin,
        cooks_d      = as.numeric(cooks.distance(model_lin)),
        leverage     = as.numeric(hatvalues(model_lin)),
        std_residual = as.numeric(rstandard(model_lin)),
        stringsAsFactors = FALSE
      )
      rv$explore_linear_influence_n_par <- length(coef(model_lin))
      rv$explore_linear_ui_exclusions   <- character(0)
    } else {
      showNotification("Could not fit linear model. Check variable selections.", type = "warning")
    }

    if (!is.numeric(y_raw) || any(y_raw <= 0, na.rm = TRUE)) {
      showNotification("Response variable must be > 0 for Log-Linear influence. Log-Linear tab skipped.", type = "warning")
    } else {
      fit_data_ll <- dat[, c(resp, preds), drop = FALSE]
      fit_data_ll <- fit_data_ll[complete.cases(fit_data_ll), , drop = FALSE]
      fit_data_ll[[resp]] <- log(fit_data_ll[[resp]])
      ll_ok <- TRUE
      for (p in preds) {
        if (is.numeric(fit_data_ll[[p]])) {
          if (any(fit_data_ll[[p]] <= 0, na.rm = TRUE)) {
            showNotification(paste0("Predictor '", p, "' must be > 0 for Log-Linear influence. Log-Linear tab skipped."), type = "warning")
            ll_ok <- FALSE
            break
          }
          fit_data_ll[[p]] <- log(fit_data_ll[[p]])
        }
      }
      if (ll_ok) {
        names(fit_data_ll)[1] <- "y"
        model_ll <- tryCatch(lm(y ~ ., data = fit_data_ll), error = function(e) NULL)
        if (!is.null(model_ll)) {
          row_lbl_ll <- rownames(fit_data_ll)
          if (is.null(row_lbl_ll)) row_lbl_ll <- as.character(seq_len(nrow(fit_data_ll)))
          rv$explore_loglin_influence_data <- data.frame(
            row_label    = row_lbl_ll,
            cooks_d      = as.numeric(cooks.distance(model_ll)),
            leverage     = as.numeric(hatvalues(model_ll)),
            std_residual = as.numeric(rstandard(model_ll)),
            stringsAsFactors = FALSE
          )
          rv$explore_loglin_influence_n_par <- length(coef(model_ll))
          rv$explore_loglin_ui_exclusions   <- character(0)
        } else {
          showNotification("Could not fit the log-linear model. Check variable selections. Log-Linear tab skipped.", type = "warning")
        }
      }
    }

    updateTextInput(session, "explore_infl_save_name",
                    value = paste0(input$data_select, "_excl_influence"))
  })

  output$explore_loglin_influence_ui <- renderUI({
    if (is.null(rv$explore_loglin_influence_data))
      return(ceres_empty_state("Pick predictor variables in the sidebar, then click Compute Influence."))
    pc <- primary_color()
    tagList(
      withSpinner(plotlyOutput("explore_loglin_influence_plot", width = "95%", height = 400),
                  color = getOption("spinner.color", default = pc),
                  type = 1),
      tags$br(),
      DT::dataTableOutput("explore_loglin_influence_table"),
      tags$br(),
      div(align = "right",
          actionButton("explore_loglin_reset_exclusions", "Reset All Exclusions", icon = icon("undo")))
    )
  })

  output$explore_loglin_influence_plot <- renderPlotly({
    req(rv$explore_loglin_influence_data)
    inf   <- rv$explore_loglin_influence_data
    n_obs <- nrow(inf)
    n_par <- if (!is.null(rv$explore_loglin_influence_n_par)) rv$explore_loglin_influence_n_par else 2L

    lev_cutoff <- 2 * n_par / n_obs
    std_cutoff <- 2

    serious       <- (abs(inf$std_residual) > 3) | (inf$cooks_d > 1) | (inf$leverage > 3 * n_par / n_obs)
    moderate      <- !serious & ((abs(inf$std_residual) > std_cutoff) | (inf$leverage > lev_cutoff) | (inf$cooks_d > 4 / n_obs))
    marker_colors <- ifelse(serious, default_colorway[1], ifelse(moderate, default_colorway[2], primary_color()))
    cd_rng        <- range(inf$cooks_d, na.rm = TRUE)
    px_sizes      <- if (!is.finite(diff(cd_rng)) || diff(cd_rng) <= 0) rep(12, nrow(inf)) else
                       8 + 32 * (inf$cooks_d - cd_rng[1]) / diff(cd_rng)

    sr_min <- if (is.finite(min(inf$std_residual, na.rm = TRUE))) min(inf$std_residual, na.rm = TRUE) - 0.5 else -3
    sr_max <- if (is.finite(max(inf$std_residual, na.rm = TRUE))) max(inf$std_residual, na.rm = TRUE) + 0.5 else  3
    lev_max <- if (is.finite(max(inf$leverage,     na.rm = TRUE))) max(inf$leverage,     na.rm = TRUE) + 0.01 else 1

    grp <- ifelse(serious, "Influential", ifelse(moderate, "Worth a look", "Typical"))
    p_inf <- plot_ly()
    for (g in c("Typical", "Worth a look", "Influential")) {
      w <- which(grp == g)
      if (!length(w)) next
      d <- inf[w, , drop = FALSE]
      p_inf <- p_inf %>% add_trace(
        x = d$leverage, y = d$std_residual, type = "scatter", mode = "markers",
        marker = list(color = marker_colors[w], opacity = 0.8, size = px_sizes[w],
                      line = list(color = "rgba(255,255,255,0.9)", width = 1)),
        text = paste0("Row: ", d$row_label,
                      "<br>Cook's D: ", round(d$cooks_d, 4),
                      "<br>Leverage: ", round(d$leverage, 4),
                      "<br>Std Resid: ", round(d$std_residual, 3)),
        hoverinfo = "text", name = g, inherit = FALSE)
    }
    p_inf %>%
      add_segments(
        x = lev_cutoff, xend = lev_cutoff,
        y = sr_min, yend = sr_max,
        line = list(color = "#888888", dash = "dash", width = 1),
        name = paste0("Leverage cutoff (", round(lev_cutoff, 3), ")"),
        inherit = FALSE
      ) %>%
      add_segments(
        x = 0, xend = lev_max,
        y = std_cutoff, yend = std_cutoff,
        line = list(color = "#888888", dash = "dash", width = 1),
        name = "Std. residual \u00b12", legendgroup = "sr2", inherit = FALSE
      ) %>%
      add_segments(
        x = 0, xend = lev_max,
        y = -std_cutoff, yend = -std_cutoff,
        line = list(color = "#888888", dash = "dash", width = 1),
        name = "Std. residual \u00b12", legendgroup = "sr2", showlegend = FALSE, inherit = FALSE
      ) %>%
      layout(
        title  = list(text = "Influence \u00b7 bubble size = Cook's D"),
        xaxis  = list(title = "Leverage (Hat Value)"),
        yaxis  = list(title = "Standardized Residual"),
        legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom"),
        margin = list(t = 84)
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$explore_loglin_influence_table <- DT::renderDataTable({
    req(rv$explore_loglin_influence_data)
    inf   <- rv$explore_loglin_influence_data
    n_obs <- nrow(inf)
    n_par <- if (!is.null(rv$explore_loglin_influence_n_par)) rv$explore_loglin_influence_n_par else 2L

    lev_cutoff      <- 2 * n_par / n_obs
    excluded_labels <- isolate(rv$explore_loglin_ui_exclusions)

    excl_html <- vapply(inf$row_label, function(lbl) {
      chk <- if (lbl %in% excluded_labels) "checked" else ""
      sprintf('<input type="checkbox" class="explore-loglin-excl-check" data-label="%s" %s>', htmltools::htmlEscape(lbl), chk)
    }, character(1))

    df_display <- data.frame(
      `Row`          = inf$row_label,
      `Cook's D`     = round(inf$cooks_d,      4),
      `Leverage`     = round(inf$leverage,     4),
      `Std Residual` = round(inf$std_residual, 3),
      `Exclude`      = excl_html,
      check.names    = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      df_display,
      rownames  = FALSE,
      selection = "none",
      escape    = FALSE,
      options   = list(
        pageLength = 15, scrollX = TRUE, dom = "ftp",
        columnDefs = list(list(className = "dt-center", targets = 4L))
      )
    ) %>%
      DT::formatStyle("Cook's D",
        backgroundColor = DT::styleInterval(4 / n_obs, c("transparent", if (isTRUE(input$dark_mode)) "#7B2030" else "#FFB8C3"))) %>%
      DT::formatStyle("Leverage",
        backgroundColor = DT::styleInterval(lev_cutoff, c("transparent", if (isTRUE(input$dark_mode)) "#7B2030" else "#FFB8C3"))) %>%
      DT::formatStyle("Std Residual",
        backgroundColor = DT::styleInterval(c(-2, 2), c(if (isTRUE(input$dark_mode)) "#1A5276" else "#6ECFEA", "transparent", if (isTRUE(input$dark_mode)) "#7B2030" else "#FFB8C3")))
  }, server = FALSE)

  observeEvent(input$explore_loglin_influence_checkbox, {
    info  <- input$explore_loglin_influence_checkbox
    lbl   <- info$label
    chked <- isTRUE(info$checked)
    current <- rv$explore_loglin_ui_exclusions
    rv$explore_loglin_ui_exclusions <- if (chked) union(current, lbl) else setdiff(current, lbl)
  })

  observeEvent(input$explore_loglin_reset_exclusions, {
    rv$explore_loglin_ui_exclusions <- character(0)
    showNotification("All exclusions cleared.", type = "message", duration = 2)
  })

  output$explore_linear_influence_ui <- renderUI({
    if (is.null(rv$explore_linear_influence_data))
      return(ceres_empty_state("Pick predictor variables in the sidebar, then click Compute Influence."))
    pc <- primary_color()
    tagList(
      withSpinner(plotlyOutput("explore_linear_influence_plot", width = "95%", height = 400),
                  color = getOption("spinner.color", default = pc),
                  type = 1),
      tags$br(),
      DT::dataTableOutput("explore_linear_influence_table"),
      tags$br(),
      div(align = "right",
          actionButton("explore_linear_reset_exclusions", "Reset All Exclusions", icon = icon("undo")))
    )
  })

  output$explore_linear_influence_plot <- renderPlotly({
    req(rv$explore_linear_influence_data)
    inf   <- rv$explore_linear_influence_data
    n_obs <- nrow(inf)
    n_par <- if (!is.null(rv$explore_linear_influence_n_par)) rv$explore_linear_influence_n_par else 2L

    lev_cutoff <- 2 * n_par / n_obs
    std_cutoff <- 2

    serious       <- (abs(inf$std_residual) > 3) | (inf$cooks_d > 1) | (inf$leverage > 3 * n_par / n_obs)
    moderate      <- !serious & ((abs(inf$std_residual) > std_cutoff) | (inf$leverage > lev_cutoff) | (inf$cooks_d > 4 / n_obs))
    marker_colors <- ifelse(serious, default_colorway[1], ifelse(moderate, default_colorway[2], primary_color()))
    cd_rng        <- range(inf$cooks_d, na.rm = TRUE)
    px_sizes      <- if (!is.finite(diff(cd_rng)) || diff(cd_rng) <= 0) rep(12, nrow(inf)) else
                       8 + 32 * (inf$cooks_d - cd_rng[1]) / diff(cd_rng)

    sr_min <- if (is.finite(min(inf$std_residual, na.rm = TRUE))) min(inf$std_residual, na.rm = TRUE) - 0.5 else -3
    sr_max <- if (is.finite(max(inf$std_residual, na.rm = TRUE))) max(inf$std_residual, na.rm = TRUE) + 0.5 else  3
    lev_max <- if (is.finite(max(inf$leverage,     na.rm = TRUE))) max(inf$leverage,     na.rm = TRUE) + 0.01 else 1

    grp <- ifelse(serious, "Influential", ifelse(moderate, "Worth a look", "Typical"))
    p_inf <- plot_ly()
    for (g in c("Typical", "Worth a look", "Influential")) {
      w <- which(grp == g)
      if (!length(w)) next
      d <- inf[w, , drop = FALSE]
      p_inf <- p_inf %>% add_trace(
        x = d$leverage, y = d$std_residual, type = "scatter", mode = "markers",
        marker = list(color = marker_colors[w], opacity = 0.8, size = px_sizes[w],
                      line = list(color = "rgba(255,255,255,0.9)", width = 1)),
        text = paste0("Row: ", d$row_label,
                      "<br>Cook's D: ", round(d$cooks_d, 4),
                      "<br>Leverage: ", round(d$leverage, 4),
                      "<br>Std Resid: ", round(d$std_residual, 3)),
        hoverinfo = "text", name = g, inherit = FALSE)
    }
    p_inf %>%
      add_segments(
        x = lev_cutoff, xend = lev_cutoff,
        y = sr_min, yend = sr_max,
        line = list(color = "#888888", dash = "dash", width = 1),
        name = paste0("Leverage cutoff (", round(lev_cutoff, 3), ")"),
        inherit = FALSE
      ) %>%
      add_segments(
        x = 0, xend = lev_max,
        y = std_cutoff, yend = std_cutoff,
        line = list(color = "#888888", dash = "dash", width = 1),
        name = "Std. residual \u00b12", legendgroup = "sr2", inherit = FALSE
      ) %>%
      add_segments(
        x = 0, xend = lev_max,
        y = -std_cutoff, yend = -std_cutoff,
        line = list(color = "#888888", dash = "dash", width = 1),
        name = "Std. residual \u00b12", legendgroup = "sr2", showlegend = FALSE, inherit = FALSE
      ) %>%
      layout(
        title  = list(text = "Influence \u00b7 bubble size = Cook's D"),
        xaxis  = list(title = "Leverage (Hat Value)"),
        yaxis  = list(title = "Standardized Residual"),
        legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom"),
        margin = list(t = 84)
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$explore_linear_influence_table <- DT::renderDataTable({
    req(rv$explore_linear_influence_data)
    inf   <- rv$explore_linear_influence_data
    n_obs <- nrow(inf)
    n_par <- if (!is.null(rv$explore_linear_influence_n_par)) rv$explore_linear_influence_n_par else 2L

    lev_cutoff      <- 2 * n_par / n_obs
    excluded_labels <- isolate(rv$explore_linear_ui_exclusions)

    excl_html <- vapply(inf$row_label, function(lbl) {
      chk <- if (lbl %in% excluded_labels) "checked" else ""
      sprintf('<input type="checkbox" class="explore-linear-excl-check" data-label="%s" %s>', htmltools::htmlEscape(lbl), chk)
    }, character(1))

    df_display <- data.frame(
      `Row`          = inf$row_label,
      `Cook's D`     = round(inf$cooks_d,      4),
      `Leverage`     = round(inf$leverage,     4),
      `Std Residual` = round(inf$std_residual, 3),
      `Exclude`      = excl_html,
      check.names    = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      df_display,
      rownames  = FALSE,
      selection = "none",
      escape    = FALSE,
      options   = list(
        pageLength = 15, scrollX = TRUE, dom = "ftp",
        columnDefs = list(list(className = "dt-center", targets = 4L))
      )
    ) %>%
      DT::formatStyle("Cook's D",
        backgroundColor = DT::styleInterval(4 / n_obs, c("transparent", if (isTRUE(input$dark_mode)) "#7B2030" else "#FFB8C3"))) %>%
      DT::formatStyle("Leverage",
        backgroundColor = DT::styleInterval(lev_cutoff, c("transparent", if (isTRUE(input$dark_mode)) "#7B2030" else "#FFB8C3"))) %>%
      DT::formatStyle("Std Residual",
        backgroundColor = DT::styleInterval(c(-2, 2), c(if (isTRUE(input$dark_mode)) "#1A5276" else "#6ECFEA", "transparent", if (isTRUE(input$dark_mode)) "#7B2030" else "#FFB8C3")))
  }, server = FALSE)

  observeEvent(input$explore_linear_influence_checkbox, {
    info  <- input$explore_linear_influence_checkbox
    lbl   <- info$label
    chked <- isTRUE(info$checked)
    current <- rv$explore_linear_ui_exclusions
    rv$explore_linear_ui_exclusions <- if (chked) union(current, lbl) else setdiff(current, lbl)
  })

  observeEvent(input$explore_loglin_save_exclusions, {
    shinyjs::disable("explore_loglin_save_exclusions")
    on.exit(shinyjs::enable("explore_loglin_save_exclusions"))
    req(rv$active_dataset)

    active_subtab   <- input$influence_subtabs
    use_linear      <- !is.null(active_subtab) && active_subtab == "Linear"
    excluded_labels <- if (use_linear) rv$explore_linear_ui_exclusions else rv$explore_loglin_ui_exclusions
    inf_data        <- if (use_linear) rv$explore_linear_influence_data else rv$explore_loglin_influence_data

    req(inf_data)

    if (length(excluded_labels) == 0) {
      showNotification("No rows marked for exclusion.", type = "message")
      return()
    }

    base_data <- rv$dataset_library[[input$data_select]]
    req(base_data)
    excl_indices <- which(rownames(base_data) %in% excluded_labels)
    if (length(excl_indices) == 0) {
      showNotification("Could not match excluded rows to dataset.", type = "warning")
      return()
    }

    new_name <- trimws(input$explore_infl_save_name)
    if (nchar(new_name) == 0) new_name <- paste0(input$data_select, "_excl_", length(excl_indices), "obs")

    store_dataset(new_name, base_data[-excl_indices, , drop = FALSE])
    update_all_dataset_selectors(new_name)

    showNotification(
      paste0("Dataset '", new_name, "' saved with ", length(excl_indices), " row(s) removed."),
      type = "message", duration = 6
    )
  })

  observeEvent(input$explore_linear_reset_exclusions, {
    rv$explore_linear_ui_exclusions <- character(0)
    showNotification("All exclusions cleared.", type = "message", duration = 2)
  })

  update_missing_selections <- function() {
    choices <- if (is.null(rv$vars_with_missing_data)) "No Missing Data" else rv$vars_with_missing_data
    updateSelectInput(session, "selected_missing", choices = choices)
  }

  update_transform_selections <- function() {
    all_cols <- colnames(rv$active_dataset)
    num_cols <- rv$numeric_data
    cat_cols <- rv$categorical_data

    updateSelectInput(session, "log_transform_vars_selected",     choices = num_cols)
    updateSelectInput(session, "sqrt_transform_vars_selected",    choices = num_cols)
    updateSelectInput(session, "inverse_transform_vars_selected", choices = num_cols)
    updateSelectInput(session, "subset_var_selected",             choices = cat_cols)
    updateSelectInput(session, "interact1_var_selected",          choices = all_cols)
    updateSelectInput(session, "interact2_var_selected",          choices = all_cols,
      selected = if (length(all_cols) >= 2) all_cols[2] else all_cols[1])
    updateSelectInput(session, "interact_response_var",
      choices  = c("(none)", all_cols),
      selected = "(none)")

    multi_level_cats <- cat_cols[vapply(rv$active_dataset[cat_cols],
                                        function(x) length(levels(x)) > 2L,
                                        logical(1L))]
    updateSelectInput(session, "expand_cat_var_selected", choices = multi_level_cats)
  }

  observeEvent(input$subset_var_selected, {
    removeUI(selector = "#subset_var_selected_level")
    df_sub <- if (!is.null(input$data_select) && nzchar(input$data_select))
      rv$dataset_library[[input$data_select]] else NULL
    lvls <- if (!is.null(df_sub) && input$subset_var_selected %in% colnames(df_sub))
      levels(df_sub[[input$subset_var_selected]]) else character(0)
    insertUI(selector = "#subset_vars_ui", ui = div(id = "subset_var_selected_level",
                                                 selectInput(inputId = "subset_var_selected_level", label = "Variable Level:", choices = lvls, multiple = TRUE)))
  })
  
  update_boxplot_selections <- function() {
    num_cols      <- rv$numeric_data
    selected_boxes <- if (length(num_cols) >= 4) num_cols[1:4] else num_cols
    updateSelectInput(session, "selected_box",    choices = num_cols, selected = selected_boxes)
    updateSelectInput(session, "cat_selected_box", choices = c("None", rv$categorical_data))
  }

  update_scatterplot_selections <- function() {
    all_cols <- colnames(rv$active_dataset)
    num_cols <- rv$numeric_data
    updateSelectInput(session, "x_selected",        choices = num_cols, selected = num_cols[1])
    updateSelectInput(session, "y_selected",        choices = num_cols, selected = num_cols[2])
    updateSelectInput(session, "scatter_color_var", choices = c("None", all_cols))
  }

  update_cer_generation_selections <- function(key = input$data_select_cer_generation) {
    cur_data <- rv$dataset_library[[key]]
    if (is.null(cur_data)) return(invisible(NULL))
    cer_numeric <- names(cur_data)[vapply(cur_data, is.numeric, logical(1L))]
    response_var <- if (length(cer_numeric) > 0L) cer_numeric[1L] else character(0)
    dataset_changed <- !identical(key, rv$cer_gen_last_dataset)
    rv$cer_gen_last_dataset <- key

    if (dataset_changed) {
      updateSelectInput(session, "ols_response",   choices = cer_numeric,
                        selected = response_var)
      updateSelectInput(session, "ols_predictors", choices = setdiff(names(cur_data), response_var),
                        selected = character(0))
    } else {
      keep_resp <- if (!is.null(input$ols_response) && input$ols_response %in% cer_numeric)
        input$ols_response else response_var
      pred_choices <- setdiff(names(cur_data), keep_resp)
      keep_preds   <- intersect(input$ols_predictors, pred_choices)
      updateSelectInput(session, "ols_response",   choices = cer_numeric,  selected = keep_resp)
      updateSelectInput(session, "ols_predictors", choices = pred_choices, selected = keep_preds)
    }
  }

  health_result <- debounce(reactive({
    key <- input$data_select
    req(key, nzchar(key))
    df <- rv$dataset_library[[key]]
    req(!is.null(df))
    skip <- if (is.null(rv$log_transformed_by_key)) character(0) else rv$log_transformed_by_key[[key]] %||% character(0)
    c(list(key = key), ceres_data_health(df, skip_skew = skip))
  }), 250)

  output$data_health_ui <- renderUI({
    h <- health_result()
    s <- h$summary
    fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
    st  <- vapply(h$findings, `[[`, "", "status")
    lvl <- if (any(st == "warn")) "warn" else if (length(st)) "info" else "ok"
    headline <- if (!length(st)) "Looks healthy"
                else if (length(st) == 1L) "1 thing to check" else paste(length(st), "things to check")
    pct <- if (is.na(s$completeness)) "–" else ceres_health_fmt_pct(s$completeness)
    if (identical(pct, "100%") && s$completeness < 1) pct <- ">99%"
    fig_line <- paste(c(paste(fmt_int(s$rows), if (s$rows == 1) "element" else "elements"),
                        paste(fmt_int(s$cols), if (s$cols == 1) "variable" else "variables"),
                        paste(pct, "complete")), collapse = " · ")
    go_js <- function(target) sprintf("Shiny.setInputValue('health_goto', '%s', {priority: 'event'})", target)
    toggle_js <- function(v) sprintf("Shiny.setInputValue('health_toggle', '%s', {priority: 'event'})", v)
    dot <- function(l) tags$span(class = paste0("c-hc-dot is-", l), `aria-hidden` = "true")
    lvl_word <- c(warn = "Warning", info = "Note", ok = "OK")
    repairable <- any(vapply(h$findings, function(f) f$id %in% c("missing", "constant", "text_num", "duplicates", "typos"), TRUE))
    repair_btn <- if (repairable) tags$button(type = "button", class = "btn btn-primary btn-sm c-hc-repair",
      onclick = "Shiny.setInputValue('health_repair', Date.now(), {priority: 'event'})",
      title = "Fix these into a new dataset (the original stays as it is)", icon("wand-magic-sparkles"), "Auto repair")
    open <- h$key %in% (rv$health_expanded %||% character(0))
    flip <- toggle_js(if (open) "collapse" else "expand")
    flip_title <- if (open) "Hide the data health check" else "Show the data health check"

    rows <- lapply(h$findings, function(f) {
      act <- f$action
      tags$div(class = paste0("c-hc-row is-", f$status),
        tags$span(class = paste0("c-hc-dot is-", f$status), role = "img", `aria-label` = lvl_word[[f$status]]),
        tags$div(class = "c-hc-body",
          tags$div(class = "c-hc-find", f$title,
            if (!is.null(f$tip)) tags$span(class = "c-hc-tip", tabindex = "0", `data-tip` = f$tip,
                                           `aria-label` = f$tip, "?")),
          tags$div(class = "c-hc-detail", f$detail),
          if (length(f$chips)) tags$div(class = "c-hc-chips",
            lapply(f$chips, function(ch) tags$span(class = "c-hc-chip",
              tags$b(ch$label),
              if (!is.null(ch$note) && nzchar(ch$note)) tags$span(class = "c-hc-note", ch$note))))),
        tags$div(class = "c-hc-act",
          if (!is.null(act$goto))
            tags$button(type = "button", class = "btn btn-default btn-sm c-hc-go", onclick = go_js(act$goto),
                        act$label, icon("arrow-right"))
          else tags$span(class = "c-hc-todo", act$text)))
    })
    findings <- tagList(
      if (length(rows)) tags$div(class = "c-hc-list", rows),
      tags$div(class = "c-hc-foot",
        if (length(h$passed)) tagList(dot("ok"),
          if (length(rows)) paste0("Passed: ", paste(h$passed, collapse = ", "), ".")
          else paste0("All ", length(h$passed), " checks passed: ", paste(h$passed, collapse = ", "), "."))))

    if (identical(CERES_HEALTH_STYLE, "pill")) {
      return(tags$div(class = "c-hp-row",
        tags$h4("Raw Data Table"),
        tags$div(class = "c-hp-anchor",
          tags$button(type = "button", class = paste0("c-hp-pill is-", lvl, if (open) " is-open"), onclick = flip,
                      title = flip_title, `aria-expanded` = tolower(open), dot(lvl), tags$span(headline), icon("chevron-down")),
          if (open) tags$section(class = paste0("c-hc c-hp-pop is-", lvl), `aria-label` = "Data health",
            tags$div(class = "c-hp-top",
              tags$div(class = "c-hp-info", tags$div(class = "c-hp-k", "Data health"), tags$div(class = "c-hp-f", fig_line)),
              repair_btn),
            tags$div(class = "c-hp-scroll", findings)))))
    }

    tags$section(class = paste0("c-hc c-hb is-", lvl, if (open) " is-open"), `aria-label` = "Data health",
      tags$div(class = "c-hb-head",
        tags$div(class = "c-hb-text", onclick = flip, title = flip_title,
          tags$div(class = "c-hc-kicker", "Data health"),
          tags$div(class = "c-hb-title", dot(lvl), headline)),
        tags$div(class = "c-hb-end", repair_btn,
          tags$button(type = "button", class = "c-hc-arrow", onclick = flip, title = flip_title,
                      `aria-label` = flip_title, `aria-expanded` = tolower(open), icon("chevron-down")))),
      if (open) tags$div(class = "c-hb-body", tags$div(class = "c-hb-figs", fig_line), findings))
  })

  observeEvent(input$health_toggle, {
    key <- input$data_select
    req(key, nzchar(key))
    cur <- rv$health_expanded %||% character(0)
    rv$health_expanded <- if (identical(input$health_toggle, "expand")) union(cur, key) else setdiff(cur, key)
  })

  observeEvent(input$health_goto, {
    if (identical(input$health_goto, "missing")) {
      updateTabsetPanel(session, "data_tabs", selected = "Missing Data")
      updateTabsetPanel(session, "missing_box", selected = "Impute Missing Data")
    } else if (identical(input$health_goto, "transforms")) {
      updateTabsetPanel(session, "data_tabs", selected = "Transforms")
      updateTabsetPanel(session, "transform_box", selected = "Transform")
      h  <- tryCatch(health_result(), error = function(e) NULL)
      df <- rv$dataset_library[[input$data_select]]
      sk <- Filter(function(f) identical(f$id, "skew"), h$findings)
      if (!is.null(df) && length(sk))
        updateSelectInput(session, "log_transform_vars_selected",
                          choices  = names(df)[vapply(df, is.numeric, logical(1L))],
                          selected = sk[[1L]]$columns)
    }
  })

  observeEvent(input$health_repair, {
    key <- input$data_select
    df  <- rv$dataset_library[[key]]
    req(df)
    num <- names(df)[vapply(df, is.numeric, TRUE)]
    if (!length(num)) {
      showNotification("Auto repair needs a numeric response column, and this dataset has none.", type = "warning")
      return()
    }
    guess <- num[grepl("cost|price|\\$|usd|dollar", num, ignore.case = TRUE)][1]
    if (is.na(guess)) guess <- num[1]
    showModal(modalDialog(
      title = "Auto repair",
      tags$p(sprintf("Fixes what Data Health found and saves the result as a new dataset, %s. “%s” itself stays as it is.",
                     ceres_repaired_name(key), key)),
      selectInput("repair_response", "Which column is the response (cost)?", choices = num, selected = guess, width = "100%"),
      tags$p(class = "text-muted", style = "font-size: 0.9em;",
             "Elements missing the response are dropped, never filled in. Everything Auto repair changes is listed afterwards."),
      footer = tagList(modalButton("Cancel"), actionButton("repair_go", "Repair", class = "btn-primary")),
      easyClose = TRUE))
  })
  observeEvent(input$repair_go, {
    key <- input$data_select; resp <- input$repair_response
    df  <- rv$dataset_library[[key]]
    req(df, resp)
    removeModal()
    res <- tryCatch(ceres_auto_repair(df, resp), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Auto repair stopped:", conditionMessage(res)), type = "error", duration = 10)
      return()
    }
    nm <- ceres_repaired_name(key); i <- 2L
    while (nm %in% names(rv$dataset_library)) { nm <- sub("(\\.[^.]*)?$", paste0(" ", i, "\\1"), ceres_repaired_name(key)); i <- i + 1L }
    clear_dataset_key_state(nm)
    rv$dataset_library[[nm]] <- res$data
    logs <- rv$repair_logs %||% list()
    logs[[nm]] <- list(from = key, response = resp, at = format(Sys.time(), "%Y-%m-%d %H:%M"),
                       log = res$log, imputed = res$imputed)
    rv$repair_logs <- logs
    update_all_dataset_selectors(nm)
    showNotification(sprintf("Repaired copy saved as %s; %s is unchanged.", nm, key), type = "message", duration = 6)
  })

  output$repair_log_ui <- renderUI({
    key <- input$data_select
    lg  <- (rv$repair_logs %||% list())[[key %||% ""]]
    if (is.null(lg)) return(NULL)
    steps <- unique(vapply(lg$log, `[[`, "", "step"))
    tags$section(class = "c-rl", `aria-label` = "Auto repair log",
      tags$div(class = "c-rl-head",
        tags$div(tags$div(class = "c-rl-kicker", "Auto repair"),
                 tags$div(class = "c-rl-title", sprintf("Repaired from %s · response %s · %s", lg$from, lg$response, lg$at))),
        downloadButton("repair_log_download", "Download log", class = "btn-sm c-rl-dl")),
      tags$div(class = "c-rl-list", lapply(steps, function(s)
        tags$div(class = "c-rl-step", tags$div(class = "c-rl-stepname", s),
          tags$ul(lapply(Filter(function(l) identical(l$step, s), lg$log), function(l) tags$li(l$text)))))),
      if (length(lg$imputed)) tags$div(class = "c-rl-foot", "Filled-in cells are shaded blue in the table below."))
  })
  output$repair_log_download <- downloadHandler(
    filename = function() paste0(tools::file_path_sans_ext(input$data_select %||% "dataset"), " - auto repair log.txt"),
    content = function(file) {
      lg <- (rv$repair_logs %||% list())[[input$data_select %||% ""]]
      req(lg)
      lines <- c(sprintf("CERES auto repair of %s (response: %s), %s", lg$from, lg$response, lg$at),
                 sprintf("Saved as: %s", input$data_select), "")
      for (s in unique(vapply(lg$log, `[[`, "", "step")))
        lines <- c(lines, s, paste0("  - ", vapply(Filter(function(l) identical(l$step, s), lg$log), `[[`, "", "text")), "")
      if (length(lg$imputed)) {
        lines <- c(lines, "Filled-in cells")
        for (cn in names(lg$imputed)) lines <- c(lines, sprintf("  %s: %s", cn, paste(lg$imputed[[cn]], collapse = ", ")))
      }
      writeLines(lines, file, useBytes = TRUE)
    })

  output$data_table <- DT::renderDataTable(server = FALSE, {

    get_data()
    
    dt <- datatable(rv$active_dataset, colnames = c("Row Name" = 1), selection = 'single', options = list(scrollX='400px', processing = FALSE))

    dt <- ceres_format_numbers(dt, rv$active_dataset, rv$numeric_data)

    if (ncol(rv$active_dataset) > 0)
      dt <- dt %>% formatStyle(colnames(rv$active_dataset), backgroundColor = styleEqual(NA, "rgba(224,69,123,0.18)"))
    imp <- (rv$repair_logs %||% list())[[rv$active_dataset_key %||% ""]]$imputed
    for (cn in intersect(names(imp), colnames(rv$active_dataset))) {
      rows <- which(row.names(rv$active_dataset) %in% unlist(imp[[cn]]))
      if (length(rows)) dt <- dt %>% formatStyle(cn, target = "cell", backgroundColor = styleRow(rows, "rgba(47,111,228,0.16)"))
    }

    dt
  })
  
  output$summary_stats_numeric <- DT::renderDataTable(server = FALSE, {
    req(rv$active_dataset)
    if (length(rv$numeric_data) == 0) {
      return(datatable(data.frame(Message = "No numeric variables in this dataset."),
                       options = list(dom = "t")))
    }

    num_data <- rv$active_dataset[, rv$numeric_data, drop = FALSE]

    numeric_summary <- round(data.frame(
      Min     = vapply(num_data, min,    numeric(1L), na.rm = TRUE),
      `25%`   = vapply(num_data, quantile, numeric(1L), probs = 0.25, na.rm = TRUE, type = 9),
      Mean    = vapply(num_data, mean,   numeric(1L), na.rm = TRUE),
      Median  = vapply(num_data, median, numeric(1L), na.rm = TRUE),
      `75%`   = vapply(num_data, quantile, numeric(1L), probs = 0.75, na.rm = TRUE, type = 9),
      Max     = vapply(num_data, max,    numeric(1L), na.rm = TRUE),
      St.Dev. = vapply(num_data, sd,     numeric(1L), na.rm = TRUE),
      `NA's`  = vapply(num_data, function(x) sum(is.na(x)), numeric(1L)),
      check.names = FALSE
    ), 3)

    datatable(numeric_summary, colnames = c("Numeric Variables" = 1),
              options = list(dom = "t", pageLength = nrow(numeric_summary), scrollX = '400px')) %>%
      formatCurrency(c("Min","25%","Mean","Median","75%","Max","St.Dev."),
                     currency = "", interval = 3, mark = ",")
  })
  
  output$summary_stats_cat <- DT::renderDataTable(server = FALSE, {
    req(rv$active_dataset)
    if (length(rv$categorical_data) == 0) {
      return(datatable(data.frame(Message = "No categorical variables in this dataset."),
                       options = list(dom = "t")))
    }

    cat_data <- rv$active_dataset[, rv$categorical_data, drop = FALSE]

    cat_summary <- as.data.frame(
      t(vapply(cat_data, function(x) {
        if (!is.factor(x)) x <- factor(x)
        s     <- summary(x, maxsum = 8)
        counts <- as.integer(s)
        value  <- paste0(names(s), ": ", counts, " (", round(counts / length(x), 4) * 100, "%)")
        length(value) <- 8L
        value
      }, character(8L))),
      stringsAsFactors = FALSE
    )
    colnames(cat_summary) <- paste0("Level ", 1:8)

    datatable(cat_summary, colnames = c("Categorical Variables" = 1),
              options = list(dom = "t", pageLength = nrow(cat_summary), scrollX = '400px'))
  })
  
  bn_impute_cache <- new.env(parent = emptyenv())
  bn_impute <- function(data) {
    key <- paste(rv$active_dataset_key %||% "", paste(rv$impute, collapse = "|"),
                 rlang::hash(data), sep = "::")
    if (identical(bn_impute_cache$key, key)) return(bn_impute_cache$imputed)
    bn_data <- data
    int_cols <- vapply(bn_data, is.integer, logical(1L))
    bn_data[int_cols] <- lapply(bn_data[int_cols], as.numeric)
    tmp <- tryCatch(
      withr::with_seed(1, ceres_ns("bnlearn")$structural.em(bn_data, return.all = TRUE, max.iter = 10)),
      error = function(e) {
        showNotification(
          paste0("Bayesian Network imputation failed: ", conditionMessage(e),
                 " — BN-imputed columns were left unchanged."),
          type = "error", duration = 10)
        NULL
      })
    if (is.null(tmp)) return(NULL)
    bn_impute_cache$key     <- key
    bn_impute_cache$imputed <- tmp[["imputed"]]
    bn_impute_cache$imputed
  }

  observeEvent(input$save_imputed_data, {
    shinyjs::disable("save_imputed_data")
    shinyjs::html("save_imputed_data", "Saving...")
    on.exit({ shinyjs::enable("save_imputed_data"); shinyjs::html("save_imputed_data", "Save Imputed Data") })
    req(length(rv$vars_with_missing_data)>0)
    req(nchar(trimws(input$impute_data_name %||% "")) > 0)
    get_data()
    data <- rv$active_dataset
    
    imp <- rv$impute
    if (length(imp) != length(rv$vars_with_missing_data))
      imp <- rep("None", length(rv$vars_with_missing_data))
    cntr <- 0
    for (i in rv$vars_with_missing_data){
      cntr <- cntr + 1
      if (imp[cntr] == "Mean Substitution" && is.numeric(data[,i])){
        data[is.na(data[,i]),i] <- mean(data[,i], na.rm = TRUE)
      } else if (imp[cntr] == "Median Substitution" && is.numeric(data[,i])) {
        data[is.na(data[,i]),i] <- median(data[,i], na.rm = TRUE)
      } else if (imp[cntr] == "Listwise Deletion") {
        data <- data[!is.na(data[,i]), ]
      } else if (imp[cntr] == "Mode Substitution" && !is.numeric(data[,i])){
        data[is.na(data[,i]),i] <- names(which.max(table(data[,i])))
      }
    }
    
    if ("Bayesian Network" %in% imp){
      bn_result <- bn_impute(data)
      if (!is.null(bn_result)) data <- bn_result
    }

    store_dataset(input$impute_data_name, data)
    updateSelectInput(session, inputId = "data_select", choices = names(rv$dataset_library), selected = input$impute_data_name)
    updateSelectInput(session, inputId = "data_select_cer_generation", choices = names(rv$dataset_library), selected = input$impute_data_name)
  })
  
  observeEvent(input$common_impute, {
    req(length(rv$vars_with_missing_data) > 0)
    
    cntr <- 0
    for (i in seq_along(rv$vars_with_missing_data)){
      cntr <- cntr + 1
      if(input$common_impute == "None"){
        rv$impute[i] <- "None"
      } else if (input$common_impute == "Listwise Deletion"){
        rv$impute[i] <- "Listwise Deletion"
      } else if (input$common_impute == "Mean (Num) / Mode (Cat)"){
        if (rv$vars_with_missing_data[i] %in% rv$numeric_data){
          rv$impute[i] <- "Mean Substitution"
        } else {
          rv$impute[i] <- "Mode Substitution"
        }
      } else if (input$common_impute == "Median (Num) / Mode (Cat)"){
        if (rv$vars_with_missing_data[i] %in% rv$numeric_data){
          rv$impute[i] <- "Median Substitution"
        } else {
          rv$impute[i] <- "Mode Substitution"
        }
      } else if (input$common_impute == "Bayesian Network") {
        rv$impute[i] <- "Bayesian Network"
      }
    }
  })
  
  output$impute_table <- DT::renderDataTable({
    get_data()
    req(rv$active_dataset)
    req(length(rv$vars_with_missing_data)>0)
    
    imp <- rv$impute
    if (length(imp) != length(rv$vars_with_missing_data))
      imp <- rep("None", length(rv$vars_with_missing_data))
    
    data <- rv$active_dataset
    
    results <- data.frame(
      `Number of Missing Values` = vapply(rv$vars_with_missing_data,
        function(v) sum(is.na(rv$active_dataset[[v]])), integer(1L)),
      `Impute With` = imp,
      check.names = FALSE
    )
    row.names(results) <- rv$vars_with_missing_data
    datatable(results, colnames = c('Variable(s) with Missing Data' = 1), selection = 'single', options = list(dom = 't', pageLength = length(rv$vars_with_missing_data)))
    
  })
  
  observeEvent(input$impute_table_rows_selected,{
    req(rv$active_dataset)
    get_data()
    req(length(rv$vars_with_missing_data)>0)
    
    if(rv$vars_with_missing_data[input$impute_table_rows_selected] %in% rv$numeric_data){
      update_dialog <- modalDialog(
        tags$div(class = "c-map-bar",
          tags$span(class = "c-map-label", "Method"),
          selectInput("impute_choice", NULL, choices = c("None", "Listwise Deletion", "Mean Substitution", "Median Substitution", "Bayesian Network"), selected = rv$impute[input$impute_table_rows_selected], width = "260px"),
          actionButton("save_impute", "Save choice", class = "btn-primary")),
        plotlyOutput("density_difference", height = 400),
        title = paste(rv$vars_with_missing_data[input$impute_table_rows_selected],"Imputation"), size = c("xl"), easyClose = TRUE, footer = "", fade = TRUE)
      showModal(update_dialog)
    } else {
      update_dialog <- modalDialog(
        tags$div(class = "c-map-bar",
          tags$span(class = "c-map-label", "Method"),
          selectInput("impute_choice", NULL, choices = c("None", "Listwise Deletion", "Mode Substitution", "Bayesian Network"), selected = rv$impute[input$impute_table_rows_selected], width = "260px"),
          actionButton("save_impute", "Save choice", class = "btn-primary")),
        plotlyOutput("density_difference", height = 400),
        title = paste(rv$vars_with_missing_data[input$impute_table_rows_selected],"Imputation"), size = c("xl"), easyClose = TRUE, footer = "", fade = TRUE)
      showModal(update_dialog)
    }
    
  })
  
  observeEvent(input$save_impute, {
    if (length(rv$impute) != length(rv$vars_with_missing_data))
      rv$impute <- rep("None", length(rv$vars_with_missing_data))
    rv$impute[input$impute_table_rows_selected] <- input$impute_choice
    removeModal()
    
  })
  
  output$density_difference <- renderPlotly({
    req(rv$active_dataset)
    req(length(rv$vars_with_missing_data)>0)
    req(input$impute_table_rows_selected)
    
    data <- rv$active_dataset
    
    x <- rv$active_dataset[,rv$vars_with_missing_data[input$impute_table_rows_selected]]
    x_impute <- x
    
    if (rv$vars_with_missing_data[input$impute_table_rows_selected] %in% rv$numeric_data){
      if (input$impute_choice == "Mean Substitution"){
        x_impute[is.na(x_impute)] <- mean(x, na.rm = TRUE)
      } else if (input$impute_choice == "Median Substitution") {
        x_impute[is.na(x_impute)] <- median(x, na.rm = TRUE)
      } else if (input$impute_choice == "Listwise Deletion") {
        x_impute <- x_impute[!is.na(x_impute)]
      } else if (input$impute_choice == "Bayesian Network"){
        cntr <- 0
        for (i in rv$vars_with_missing_data){
          cntr <- cntr + 1
          if (rv$impute[cntr] == "Mean Substitution"){
            data[is.na(data[,i]),i] <- mean(data[,i], na.rm = TRUE)
          } else if (rv$impute[cntr] == "Median Substitution") {
            data[is.na(data[,i]),i] <- median(data[,i], na.rm = TRUE)
          } else if (rv$impute[cntr] == "Listwise Deletion") {
            data <- data[!is.na(data[,i]), ]
          } else if (rv$impute[cntr] == "Mode Substitution"){
            data[is.na(data[,i]),i] <- names(which.max(table(data[,i])))
          }
        }
        
        data     <- bn_impute(data)
        req(!is.null(data))
        x_impute <- data[,rv$vars_with_missing_data[input$impute_table_rows_selected]]
      }

      x_d  <- na.omit(x); x_d  <- x_d[is.finite(x_d)]
      xi_d <- na.omit(x_impute); xi_d <- xi_d[is.finite(xi_d)]
      validate(need(length(x_d) >= 2 && length(xi_d) >= 2,
                    "Not enough non-missing values in this column to draw a density preview."))
      fit_x <- density(x_d)
      fit_x_impute <- density(xi_d)

      plot_ly() %>%
        add_trace(x = fit_x$x, y = fit_x$y, type = "scatter", mode = "lines", fill = "tozeroy", name = "As loaded", inherit = FALSE,
                  line = list(color = "rgba(100,116,139,0.9)", width = 2), fillcolor = "rgba(100,116,139,0.16)") %>%
        add_trace(x = fit_x_impute$x, y = fit_x_impute$y, type = "scatter", mode = "lines", fill = "tozeroy", name = "After imputation", inherit = FALSE,
                  line = list(color = primary_color(), width = 2.5), fillcolor = paste0(primary_color(), "26")) %>%
        layout(title = "Distribution before and after imputation",
               xaxis = list(title = rv$vars_with_missing_data[input$impute_table_rows_selected], zeroline = FALSE),
               yaxis = list(title = "Density", zeroline = FALSE),
               legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.04, yanchor = "bottom"),
               hovermode = "x unified") %>%
        config(displayModeBar = FALSE)
      
    } else {
      if (input$impute_choice == "Mode Substitution"){
        x_impute[is.na(x_impute)] <- names(which.max(table(x)))
      } else if (input$impute_choice == "Listwise Deletion") {
        x_impute <- x_impute[!is.na(x_impute)]
      } else if (input$impute_choice == "Bayesian Network"){
        cntr <- 0
        for (i in rv$vars_with_missing_data){
          cntr <- cntr + 1
          if (rv$impute[cntr] == "Mean Substitution"){
            data[is.na(data[,i]),i] <- mean(data[,i], na.rm = TRUE)
          } else if (rv$impute[cntr] == "Median Substitution") {
            data[is.na(data[,i]),i] <- median(data[,i], na.rm = TRUE)
          } else if (rv$impute[cntr] == "Listwise Deletion") {
            data <- data[!is.na(data[,i]), ]
          } else if (rv$impute[cntr] == "Mode Substitution"){
            data[is.na(data[,i]),i] <- names(which.max(table(data[,i])))
          }
        }
        
        data     <- bn_impute(data)
        req(!is.null(data))
        x_impute <- data[,rv$vars_with_missing_data[input$impute_table_rows_selected]]
      }

      plot_ly() %>%
        add_histogram(x = x, name = "As loaded", marker = list(color = "rgba(100,116,139,0.45)")) %>%
        add_histogram(x = x_impute,  name = "After imputation", marker = list(color = paste0(primary_color(), "8C"))) %>%
        layout(title = "Distribution before and after imputation", barmode = "group", bargap = 0.25,
               legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.04, yanchor = "bottom")) %>%
        config(displayModeBar = FALSE)
    }
    
  })
  
  output$upset <- renderPlot({
    get_data()
    req(rv$active_dataset)
    
    num_var_with_missing_values <- length(rv$vars_with_missing_data)
    req(num_var_with_missing_values > 1)
    ceres_upset_draw(ceres_upset_data(rv$active_dataset), dark = isTRUE(input$dark_mode))
  })
  output$upset_ui <- renderUI({
    get_data()
    req(rv$active_dataset)
    miss <- rv$vars_with_missing_data %||% character(0)
    if (length(miss) > 1L)
      return(withSpinner(plotOutput("upset", height = 650),
                         color = getOption("spinner.color", default = loading_color), type = 1))
    if (length(miss) == 1L)
      return(tags$div(class = "c-mt-card c-mt-none",
                      tags$div(class = "c-mt-verdict", "Only one column has gaps"),
                      tags$p(tags$b(miss), " is the only column with missing values. This chart shows which ",
                             "columns are missing together, so it needs gaps in at least two. See ",
                             tags$b("Mechanisms of Missingness"), " for what predicts the gaps in ", tags$b(miss), ".")))
    tags$div(class = "c-mt-card c-mt-none",
             tags$div(class = "c-mt-verdict", "No missing data"),
             tags$p("Every column in this dataset is complete."))
  })
  
  missing_tree_fit <- reactive({
    get_data()
    req(rv$active_dataset)
    num_var_with_missing_values <- length(rv$vars_with_missing_data)
    col_index <- which(colnames(rv$active_dataset) == input$selected_missing)
    if (is.null(input$selected_missing) || num_var_with_missing_values == 0 || length(col_index) == 0)
      return(NULL)
    y <- factor(is.na(rv$active_dataset[,col_index]), levels = c(FALSE, TRUE), labels = c("Not Missing", "Missing"))
    x <- rv$active_dataset[,-col_index]
    data <- data.frame(y, x, check.names = FALSE)
    tree <- withr::with_seed(23, ceres_ns("rpart")$rpart(y ~ ., data = data, control = ceres_ns("rpart")$rpart.control(minsplit = 5, xval = 10)))
    n_splits_full <- max(tree$cptable[, "nsplit"])
    cpt <- tree$cptable
    if (nrow(cpt) > 1 && all(is.finite(cpt[, "xerror"]))) {
      best_cp <- cpt[which.min(cpt[, "xerror"]), "CP"]
      tree <- ceres_ns("rpart")$prune(tree, cp = best_cp)
    }
    list(tree = tree, var = input$selected_missing, n = length(y), n_missing = sum(y == "Missing"),
         n_splits_full = n_splits_full, n_splits = sum(tree$frame$var != "<leaf>"))
  })

  output$missing_tree <- renderPlot({
    fit <- missing_tree_fit()
    if (is.null(fit)) {
      plot.new()
      text(0.5, 0.5, "No missing data", cex = 1.6, col = "#64748B")
      return(invisible())
    }
    wrap_split <- function(x, labs, digits, varlen, faclen)
      vapply(labs, function(l) paste(strwrap(l, width = 34), collapse = "\n"), character(1))
    ceres_ns("rpart.plot")$rpart.plot(fit$tree, extra = 1, tweak = 1.15,
               box.palette = c("#D3E1FC", "#F9CFDD"),
               border.col = "#94A3B8", shadow.col = 0,
               col = "#0F172A", split.col = "#334155", split.cex = 0.95,
               split.box.col = "white", split.border.col = 0,
               branch.col = "#94A3B8", branch.lwd = 1.4,
               split.fun = wrap_split)
  })

  output$missing_tree_view <- renderUI({
    fit <- missing_tree_fit()
    if (is.null(fit))
      return(tags$div(class = "c-mt-card c-mt-none",
                      tags$div(class = "c-mt-verdict", "No missing data"),
                      tags$p("Every column in this dataset is complete.")))
    tree  <- fit$tree
    fr    <- tree$frame
    pct   <- function(a, b) if (b > 0) round(100 * a / b) else 0
    fmt_n <- function(v) format(v, big.mark = ",", scientific = FALSE, trim = TRUE)

    nice_cond <- function(lbl) {
      lbl <- trimws(lbl)
      m <- regmatches(lbl, regexec("^(.+?)(>=|<=|<|>|=)(.+)$", lbl))[[1]]
      if (length(m) != 4) return(lbl)
      v <- unbt(m[2]); op <- m[3]; val <- m[4]
      if (op == "=") {
        lev <- strsplit(val, ",", fixed = TRUE)[[1]]
        return(paste0(v, " is ", paste(lev, collapse = ", ")))
      }
      num <- suppressWarnings(as.numeric(val))
      shown <- if (is.na(num)) val else fmt_n(signif(num, 4))
      op_txt <- c(">=" = "≥", "<=" = "≤", "<" = "<", ">" = ">")[[op]]
      paste(v, op_txt, shown)
    }

    leaves <- which(fr$var == "<leaf>")
    node_ids <- as.integer(rownames(fr))
    paths <- if (length(leaves) > 1)
      ceres_ns("rpart")$path.rpart(tree, nodes = node_ids[leaves], pretty = 0, print.it = FALSE)
    else list()

    rows <- lapply(seq_along(leaves), function(k) {
      i   <- leaves[k]
      cnt <- fr$yval2[i, 2:3]
      n_i <- sum(cnt); miss <- cnt[2]
      conds <- if (length(paths)) setdiff(paths[[k]], "root") else character(0)
      list(n = n_i, miss = miss, share = if (n_i > 0) miss / n_i else 0,
           conds = vapply(conds, nice_cond, character(1)))
    })
    rows <- rows[order(-vapply(rows, `[[`, numeric(1), "share"), -vapply(rows, `[[`, numeric(1), "n"))]

    overall <- pct(fit$n_missing, fit$n)
    patterned <- length(leaves) > 1
    hot <- sum(vapply(rows, function(r) r$share >= 0.5, logical(1)))

    head_card <- tags$div(class = "c-mt-card c-mt-head",
      tags$div(class = "c-mt-kicker", "Missing values in"),
      tags$div(class = "c-mt-var", fit$var),
      tags$div(class = "c-mt-stats",
        tags$span(class = "c-mt-big", paste0(fmt_n(fit$n_missing), " of ", fmt_n(fit$n))),
        tags$span(class = "c-mt-sub", paste0(overall, "% of rows are missing"))),
      tags$div(class = paste("c-mt-verdict", if (patterned) "is-pattern" else "is-random"),
        if (patterned) "Missing values follow a pattern"
        else "No pattern found"),
      tags$p(class = "c-mt-explain",
        if (patterned)
          paste0("A decision tree split the rows into ", length(rows), " groups using the other columns",
                 if (fit$n_splits < fit$n_splits_full)
                   paste0(" (pruned by 10-fold cross-validation from ", fit$n_splits_full, " splits to ", fit$n_splits, "). ")
                 else " (10-fold cross-validation kept every split). ",
                 if (hot > 0) paste0(hot, if (hot == 1) " group is" else " groups are",
                                     " mostly missing, and its conditions below show where the gaps are.")
                 else "No group is mostly missing; the gaps lean towards the groups at the top.")
        else
          paste0("After 10-fold cross-validation the tree keeps no split",
                 if (fit$n_splits_full > 0) paste0(" (the full tree had ", fit$n_splits_full, ", none of which held up)") else "",
                 ": no column reliably separates rows with a missing value from the rest, which is consistent with values missing at random."))
    )

    if (!patterned) return(tags$div(class = "c-mt", head_card))

    row_tags <- lapply(rows, function(r) {
      share_pct <- round(100 * r$share)
      tags$div(class = paste("c-mt-row", if (r$share >= 0.5) "is-hot" else ""),
        tags$div(class = "c-mt-conds",
          if (length(r$conds) == 0) tags$span(class = "c-mt-chip", "All rows")
          else lapply(seq_along(r$conds), function(j) tagList(
            if (j > 1) tags$span(class = "c-mt-and", "and"),
            tags$span(class = "c-mt-chip", r$conds[[j]])))),
        tags$div(class = "c-mt-meter",
          tags$div(class = "c-mt-bar", tags$span(style = paste0("width:", max(share_pct, 2), "%;"))),
          tags$div(class = "c-mt-count",
            tags$b(paste0(fmt_n(r$miss), " of ", fmt_n(r$n))), " missing",
            tags$span(class = "c-mt-pct", paste0(share_pct, "%"))))
      )
    })

    tags$div(class = "c-mt",
      head_card,
      tags$div(class = "c-mt-card c-mt-groups",
        tags$div(class = "c-mt-groups-title", "Groups, most missing first"),
        row_tags))
  })

  observeEvent(input$log_all_button, {
    req(input$data_select %in% names(rv$dataset_library))
    current_data_name <- input$data_select
    data_to_log <- rv$dataset_library[[current_data_name]]
    
    req(data_to_log)
    
    data_log <- data_to_log
    
    
    numeric_vars <- names(data_log)[sapply(data_log, is.numeric)]
    log_transform_vars <- numeric_vars[sapply(data_log[numeric_vars], function(x) all(x > 0, na.rm = TRUE))]
    
    for (var in log_transform_vars) {
      data_log[[var]] <- log(data_log[[var]])
    }

    skipped_vars <- setdiff(numeric_vars, log_transform_vars)
    if (length(skipped_vars) > 0) {
      showNotification(
        paste0("Log space: skipped ", length(skipped_vars),
               " column(s) with non-positive values (log undefined): ",
               paste(skipped_vars, collapse = ", ")),
        type = "warning", duration = 7
      )
    }

    if(input$replace_transform) {
      store_dataset(current_data_name, data_log)
      rv$log_transformed_by_key[[current_data_name]] <- log_transform_vars
      new_name <- current_data_name
      showNotification("Original dataset updated with log-transformed columns.", type = "message")

    } else {
      new_name <- input$transform_data_name
      if (new_name %in% names(rv$dataset_library)) {
        showNotification(paste("Dataset name", new_name, "already exists. Please choose a different name or replace the original."), type = "warning")
        return()
      }
      rv$dataset_library[[new_name]] <- data_log
      rv$log_transformed_by_key[[new_name]] <- log_transform_vars
      showNotification("Dataset converted to log space (for selected columns) and saved.", type = "message")
    }
    
    update_all_dataset_selectors(new_name)
    
    
  })
  
  
  observeEvent(input$save_transform_data, {
    req(length(input$log_transform_vars_selected)>0 | length(input$sqrt_transform_vars_selected)>0 | length(input$inverse_transform_vars_selected)>0)
    req(nchar(trimws(input$transform_data_name %||% "")) > 0)
    get_data()
    data <- rv$active_dataset
    src     <- data
    drop_after <- character(0)

    if (length(input$log_transform_vars_selected)>0){
      for (i in input$log_transform_vars_selected){
        data[,paste0(i,"_ln")] <- log(src[,i])
        if (input$replace_transform) drop_after <- c(drop_after, i)
      }
    }

    if (length(input$sqrt_transform_vars_selected)>0){
      for (i in input$sqrt_transform_vars_selected){
        data[,paste0(i,"_sqrt")] <- sqrt(src[,i])
        if (input$replace_transform) drop_after <- c(drop_after, i)
      }
    }

    if (length(input$inverse_transform_vars_selected)>0){
      for (i in input$inverse_transform_vars_selected){
        data[,paste0(i,"_inv")] <- 1/(src[,i])
        if (input$replace_transform) drop_after <- c(drop_after, i)
      }
    }

    for (i in unique(drop_after)) if (i %in% names(data)) data[[i]] <- NULL
    
    for (j in names(data)) if (is.numeric(data[[j]])) {
      bad <- !is.finite(data[[j]]); if (any(bad)) data[[j]][bad] <- NA
    }
    
    store_dataset(input$transform_data_name, data)
    updateSelectInput(session, inputId = "data_select", choices = names(rv$dataset_library), selected = input$transform_data_name)
    updateSelectInput(session, inputId = "data_select_cer_generation", choices = names(rv$dataset_library), selected = input$transform_data_name)
  })
  
  observeEvent(input$save_subset_data, {
    req(rv$active_dataset)
    req(rv$categorical_data)
    req(input$subset_var_selected, input$subset_var_selected %in% colnames(rv$active_dataset))
    req(nchar(trimws(input$subset_data_name %||% "")) > 0)
    req(length(input$subset_var_selected_level) > 0)
    sub_col <- rv$active_dataset[, input$subset_var_selected]
    n_na    <- sum(is.na(sub_col))
    data    <- rv$active_dataset[which(sub_col %in% input$subset_var_selected_level), ]
    if (n_na > 0L)
      showNotification(paste0(n_na, if (n_na == 1L) " program" else " programs", " with no ",
                              input$subset_var_selected, " recorded ",
                              if (n_na == 1L) "was" else "were", " left out of the subset."),
                       type = "message", duration = 6)
    
    store_dataset(input$subset_data_name, data)
    updateSelectInput(session, inputId = "data_select", choices = names(rv$dataset_library), selected = input$subset_data_name)
    updateSelectInput(session, inputId = "data_select_cer_generation", choices = names(rv$dataset_library), selected = input$subset_data_name)
  })
  
  observeEvent(input$save_interact_data, {
    req(rv$active_dataset)
    req(nchar(trimws(input$interact_data_name)) > 0)
    terms <- interact_terms_data()
    req(length(terms) > 0)
    data <- rv$active_dataset
    for (t in terms) data[[t$name]] <- t$values
    new_name <- trimws(input$interact_data_name)
    store_dataset(new_name, data)
    updateSelectInput(session, inputId = "data_select",                   choices = names(rv$dataset_library), selected = new_name)
    updateSelectInput(session, inputId = "data_select_cer_generation",    choices = names(rv$dataset_library), selected = new_name)
    showNotification(paste0(length(terms), " term(s) added to \u2018", new_name, "\u2019"), type = "message", duration = 3)
  })

  observeEvent(input$save_expand_data, {
    shinyjs::disable("save_expand_data")
    shinyjs::html("save_expand_data", "Expanding...")
    on.exit({ shinyjs::enable("save_expand_data"); shinyjs::html("save_expand_data", "Expand to Boolean Columns") })
    req(rv$active_dataset)
    req(length(input$expand_cat_var_selected) > 0)
    req(nchar(trimws(input$expand_data_name)) > 0)

    data <- rv$active_dataset
    for (col in input$expand_cat_var_selected) {
      if (!col %in% names(data)) next
      if (!is.factor(data[[col]])) next
      data <- expand_categorical_to_boolean(data, col, remove_original = input$remove_original_expand)
    }

    new_name <- trimws(input$expand_data_name)
    store_dataset(new_name, data)
    updateSelectInput(session, inputId = "data_select",                   choices = names(rv$dataset_library), selected = new_name)
    updateSelectInput(session, inputId = "data_select_cer_generation",    choices = names(rv$dataset_library), selected = new_name)
  })

  output$transforms_box <- renderUI({
    n <- length(input$log_transform_vars_selected) + length(input$sqrt_transform_vars_selected) +
         length(input$inverse_transform_vars_selected)
    if (n == 0)
      return(ceres_empty_state("Pick columns in the sidebar (natural log, square root or inverse) to see each one before and after."))
    plotlyOutput("transforms", height = paste0(max(380, 60 + 230 * n), "px"))
  })

  output$transforms <- renderPlotly({
    get_data()
    req(rv$active_dataset)
    req(length(input$log_transform_vars_selected) > 0 |
        length(input$sqrt_transform_vars_selected) > 0 |
        length(input$inverse_transform_vars_selected) > 0)

    if (length(rv$numeric_data) == 0) {
      plot_ly(x = 0.5, y = 0.5, type = 'scatter', mode = 'text',
              text = "No Numeric Data to Display", textposition = 'middle',
              textfont = list(color = '#000000', size = 16)) %>%
        layout(xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE,
                            showline = FALSE, showgrid = FALSE),
               yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE,
                            showline = FALSE, showgrid = FALSE)) %>%
        config(displayModeBar = FALSE)
    } else {
      make_annot <- function(label)
        list(text = paste0("<b>", label, "</b>"), font = list(size = 12), xref = "paper", yref = "paper",
             yanchor = "bottom", xanchor = "left", align = "left",
             x = 0, y = 1.01, showarrow = FALSE)

      build_pair <- function(var_name, xf_fn, xf_label) {
        raw <- rv$active_dataset[, var_name]
        subplot(
          plot_ly(x = raw, type = "histogram", name = var_name,
                  marker = list(color = "rgba(100,116,139,0.55)",
                                line = list(color = "rgba(255,255,255,0.9)", width = 1))) %>%
            layout(annotations = make_annot(var_name), bargap = 0.04),
          plot_ly(x = xf_fn(raw), type = "histogram", name = xf_label,
                  marker = list(color = paste0(primary_color(), "A6"),
                                line = list(color = "rgba(255,255,255,0.9)", width = 1))) %>%
            layout(annotations = make_annot(xf_label), bargap = 0.04),
          nrows = 1, titleX = TRUE, margin = c(0.04, 0.04, 0.02, 0.02)
        ) %>%
          layout(title = var_name,
                 xaxis = list(title = "", zeroline = FALSE),
                 yaxis = list(title = "", zeroline = FALSE),
                 showlegend = FALSE) %>%
          config(displayModeBar = FALSE)
      }

      transform_specs <- list(
        list(vars = input$log_transform_vars_selected,
             fn   = log,
             lab  = function(n) paste0("ln(", n, ")")),
        list(vars = input$sqrt_transform_vars_selected,
             fn   = sqrt,
             lab  = function(n) paste0("sqrt(", n, ")")),
        list(vars = input$inverse_transform_vars_selected,
             fn   = function(x) 1 / x,
             lab  = function(n) paste0("1/(", n, ")"))
      )

      graphs <- unlist(lapply(transform_specs, function(spec) {
        lapply(spec$vars, function(v) build_pair(v, spec$fn, spec$lab(v)))
      }), recursive = FALSE)

      if (length(graphs) > 1) {
        do.call(plotly::subplot, c(graphs, list(nrows = length(graphs), titleX = TRUE,
                                                 margin = c(0.04, 0.04, 28 / (60 + 230 * length(graphs)), 28 / (60 + 230 * length(graphs)))))) %>%
          layout(title = list(text = "Before and after"), margin = list(t = 64, l = 48, r = 20, b = 40))
      } else {
        graphs[[1]]
      }
    }
  })
  
  output$subsets <- renderPlotly({
    get_data()
    req(rv$active_dataset)
    req(input$subset_var_selected %in% colnames(rv$active_dataset))
    req(input$subset_var_selected_level %in% levels(rv$active_dataset[,input$subset_var_selected]))

    data <- rv$active_dataset
    data_subset <- data[which(data[,input$subset_var_selected] %in% input$subset_var_selected_level), ]

    n_full   <- nrow(data)
    n_subset <- nrow(data_subset)
    req(n_full > 0)

    rows  <- 10L
    cols  <- as.integer(ceiling(n_full / rows))
    idx   <- seq_len(rows * cols)
    xx    <- ((idx - 1L) %/% rows) + 1L
    yy    <- ((idx - 1L) %%  rows) + 1L
    status <- ifelse(idx > n_full, NA_character_,
                     ifelse(idx <= n_subset, "Subset", "Remove"))
    df <- data.frame(x = xx, y = yy, status = status, stringsAsFactors = FALSE)
    df <- df[!is.na(df$status), , drop = FALSE]
    df$status <- factor(df$status, levels = c("Subset", "Remove"))

    plot_ly(df, x = ~x, y = ~y, color = ~status,
            colors = c("Subset" = "#0087cd", "Remove" = graph_color_1),
            type = "scatter", mode = "markers",
            marker = list(symbol = "square", size = 16,
                          line = list(color = "white", width = 1)),
            hoverinfo = "text",
            text = ~paste0(status, " (", n_subset, " kept / ", n_full - n_subset, " removed)")) %>%
      layout(title = "1 Block = 1 Data Point",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "",
                          scaleanchor = "x", autorange = "reversed"),
             legend = list(title = list(text = "")),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
      config(displayModeBar = FALSE)
  })
  
  interact_terms_data <- reactive({
    req(rv$active_dataset)
    req(input$interact1_var_selected %in% colnames(rv$active_dataset))
    req(input$interact2_var_selected %in% colnames(rv$active_dataset))
    req(length(input$interact_term_types) >= 1)

    data  <- rv$active_dataset
    va    <- input$interact1_var_selected
    vb    <- input$interact2_var_selected
    types <- input$interact_term_types

    expand_col <- function(var) {
      if (var %in% rv$numeric_data) {
        m <- matrix(as.numeric(data[[var]]), ncol = 1); colnames(m) <- var; m
      } else {
        w <- data.frame(data[[var]]); colnames(w) <- var
        dum <- ceres_ns("caret")$dummyVars(" ~ .", data = w)
        as.matrix(data.frame(predict(dum, newdata = w)))
      }
    }

    terms <- list()

    if ("product" %in% types) {
      m1 <- expand_col(va); m2 <- expand_col(vb)
      for (i in seq_len(ncol(m1))) {
        for (j in seq_len(ncol(m2))) {
          nm <- paste0(colnames(m1)[i], "X", colnames(m2)[j])
          terms[[nm]] <- list(name = nm, values = m1[, i] * m2[, j])
        }
      }
    }

    if ("ratio" %in% types && va %in% rv$numeric_data && vb %in% rv$numeric_data) {
      nm <- paste0(va, "/", vb)
      ratio_vals <- as.numeric(data[[va]]) / as.numeric(data[[vb]])
      ratio_vals[!is.finite(ratio_vals)] <- NA
      terms[[nm]] <- list(name = nm, values = ratio_vals)
    }

    if ("square" %in% types) {
      for (v in c(va, vb)) {
        if (v %in% rv$numeric_data) {
          nm <- paste0(v, "^2")
          terms[[nm]] <- list(name = nm, values = as.numeric(data[[v]])^2)
        }
      }
    }

    terms
  })

  output$interact_corr_plot_ui <- renderUI({
    resp <- input$interact_response_var
    if (is.null(resp) || resp == "(none)") return(NULL)
    req(input$interact1_var_selected %in% colnames(rv$active_dataset))
    pc <- primary_color()
    div(style = "margin-bottom: 8px;",
      withSpinner(plotlyOutput("interact_corr_plot", height = "220px"), color = pc, type = 1)
    )
  })

  output$interact_corr_plot <- renderPlotly({
    terms <- interact_terms_data()
    req(length(terms) > 0)
    resp <- input$interact_response_var
    req(!is.null(resp) && resp != "(none)" && resp %in% colnames(rv$active_dataset))
    y <- rv$active_dataset[[resp]]
    req(is.numeric(y))

    corrs <- vapply(terms, function(t) {
      suppressWarnings(cor(t$values, y, use = "complete.obs"))
    }, numeric(1))
    corrs[is.na(corrs)] <- 0

    df_c <- data.frame(term = names(corrs), corr = corrs, stringsAsFactors = FALSE)
    df_c <- df_c[order(abs(df_c$corr), decreasing = TRUE), ]
    bar_colors <- ifelse(df_c$corr >= 0, primary_color(), default_colorway[1])

    plot_ly(df_c,
            x           = ~corr,
            y           = ~factor(term, levels = rev(term)),
            type        = "bar",
            orientation = "h",
            marker      = list(color = bar_colors),
            text        = ~paste0(round(corr, 3)),
            textposition = "outside",
            hoverinfo   = "text") %>%
      layout(
        title      = list(text = paste0("Correlation with \u2018", resp, "\u2019"), font = list(size = 13)),
        xaxis      = list(title = "Pearson r", range = c(-1.15, 1.15), zeroline = TRUE),
        yaxis      = list(title = "", tickfont = list(size = 10)),
        margin     = list(l = 180, r = 50, t = 40, b = 40),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$interacts <- renderDT({
    get_data()
    req(rv$active_dataset)

    terms <- interact_terms_data()
    if (length(terms) == 0) {
      return(DT::datatable(
        data.frame(Message = "Select \u22652 variables and at least one term type to preview."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }

    va <- input$interact1_var_selected; vb <- input$interact2_var_selected
    src_vars     <- unique(c(va, vb))
    valid_vars   <- src_vars[src_vars %in% colnames(rv$active_dataset)]
    data_preview <- rv$active_dataset[, valid_vars, drop = FALSE]
    for (t in terms) data_preview[[t$name]] <- t$values
    row.names(data_preview) <- rv$row_names

    num_cols <- names(data_preview)[vapply(data_preview, is.numeric, logical(1))]
    dt <- DT::datatable(data_preview, colnames = c("Row Name" = 1),
                        options = list(pageLength = 15, scrollX = TRUE, dom = "ftp"))
    if (length(num_cols)) dt <- DT::formatRound(dt, num_cols, digits = 4, mark = ",")
    dt
  })

  output$expand_preview_table <- DT::renderDataTable({
    req(rv$active_dataset)
    req(length(input$expand_cat_var_selected) > 0)

    selected_cols <- input$expand_cat_var_selected
    valid_cols    <- selected_cols[selected_cols %in% names(rv$active_dataset) &
                                   sapply(selected_cols, function(c) is.factor(rv$active_dataset[[c]]))]
    if (length(valid_cols) == 0) return(NULL)

    before_cols  <- names(rv$active_dataset)
    preview_data <- rv$active_dataset
    for (col in valid_cols) {
      preview_data <- expand_categorical_to_boolean(preview_data, col, remove_original = FALSE)
    }
    new_cols     <- setdiff(names(preview_data), before_cols)
    preview_cols <- c(valid_cols, new_cols)

    DT::datatable(
      head(preview_data[, preview_cols, drop = FALSE], 50),
      options = list(scrollX = TRUE, pageLength = 10, dom = 'tp'),
      caption = paste0("Preview: expanding ", length(valid_cols), " column(s) into ",
                       length(new_cols), " boolean column(s)")
    )
  })

  output$boxplots <- renderPlotly({
    get_data()
    req(rv$active_dataset, rv$numeric_data)
    req(input$selected_box, input$cat_selected_box)
    req(all(input$selected_box %in% colnames(rv$active_dataset)))
    
    if (length(rv$numeric_data) == 0 || length(input$selected_box) == 0){
      plot_ly(x = 0.5, y = 0.5, type = 'scatter',
              mode = 'text', text = "No Numeric Data to Display", textposition = 'middle',
              textfont = list(color = '#000000', size = 16)) %>%
        layout(xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE,showline = FALSE,showticklabels = FALSE,showgrid = FALSE), yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE,showline = FALSE,showticklabels = FALSE,showgrid = FALSE)) %>% 
        config(displayModeBar = FALSE)
    } else if (input$cat_selected_box == "None"){

      if (isTRUE(input$boxplot_log_space))
        for (i in input$selected_box)
          if (any(rv$active_dataset[, i] <= 0, na.rm = TRUE))
            notify_log_once(i, "box",
              paste0("Log space: '", i, "' has non-positive values \u2014 those points are omitted."))
      ceres_boxplots_fig(rv$active_dataset, input$selected_box, primary_color(),
                         log_space = isTRUE(input$boxplot_log_space))

    } else {
      p <- plot_ly(type = "box")
      
      for (i in input$selected_box){
        data <- rv$active_dataset[,i]
        name <- i

        if (isTRUE(input$boxplot_log_space)){
          if (any(data <= 0, na.rm = TRUE)) {
            notify_log_once(i, "box",
              paste0("Log space: '", i, "' has non-positive values — those points are omitted."))
            data[data <= 0] <- NA
          }
          data <- log(data)
          name <- paste0("log(",i,")")
        }
        p <- p %>% add_trace(x = rv$active_dataset[,input$cat_selected_box], y = data, name = name, type = "box", boxpoints = ceres_box_points(sum(!is.na(data))), jitter = 0.3, pointpos = 0,
                             text = rownames(rv$active_dataset))
      }

      p %>% layout(boxmode = "group", hoverlabel = list(namelength = 0),
                   colorway = c("#2F6FE4", "#E0457B", "#16A34A", "#F08A24", "#7C5CFF", "#0EA5E9", "#F5B700"),
                   title = list(text = paste0("<b>Distributions</b> by ", input$cat_selected_box),
                                x = 0, xanchor = "left", xref = "paper", y = 0.99, yanchor = "top", font = list(size = 15)),
                   legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.04, yanchor = "bottom"),
                   margin = list(t = 78, l = 60, r = 20, b = 56)) %>%
        config(displayModeBar = FALSE)
    }
  })

  
  output$scatterplots <- renderPlotly({
    get_data()
    req(rv$active_dataset, rv$numeric_data)
    req(input$x_selected, input$y_selected, input$scatter_color_var, input$trendline)
    req(input$x_selected %in% colnames(rv$active_dataset))
    req(input$y_selected %in% colnames(rv$active_dataset))
    
    plot_dat <- rv$active_dataset
    plot_dat <- plot_dat[order(plot_dat[,input$x_selected]),]
    
    x <- plot_dat[,input$x_selected]
    y <- plot_dat[,input$y_selected]
    name_x <- input$x_selected
    name_y <- input$y_selected
    
    if (isTRUE(input$scatter_x_log_space) && is.numeric(x)){
      if (any(x <= 0, na.rm = TRUE)) {
        notify_log_once(name_x, "scatter",
          paste0("Log space: '", name_x, "' has non-positive values — those points are omitted."))
        x[x <= 0] <- NA
      }
      x <- log(x)
      name_x <- paste0("log(",name_x,")")
    }

    if (isTRUE(input$scatter_y_log_space) && is.numeric(y)){
      if (any(y <= 0, na.rm = TRUE)) {
        notify_log_once(name_y, "scatter",
          paste0("Log space: '", name_y, "' has non-positive values — those points are omitted."))
        y[y <= 0] <- NA
      }
      y <- log(y)
      name_y <- paste0("log(",name_y,")")
    }
    
    fit_dat <- data.frame(x = x, y = y)
    need_pos_y <- input$trendline %in% c("exponential", "power")
    need_pos_x <- input$trendline %in% c("logarithmic", "power")
    bad_log <- (need_pos_y && any(y <= 0, na.rm = TRUE)) ||
               (need_pos_x && any(x <= 0, na.rm = TRUE))
    fit <- if (bad_log) NULL else tryCatch(
      switch(input$trendline,
             linear      = lm(y ~ x, data = fit_dat),
             polynomial  = lm(y ~ x + I(x^2), data = fit_dat),
             exponential = lm(log(y) ~ x, data = fit_dat),
             logarithmic = lm(y ~ log(x), data = fit_dat),
             power       = lm(log(y) ~ log(x), data = fit_dat),
             NULL),
      error = function(e) NULL)
    if (is.null(fit)) {
      if (bad_log)
        notify_log_once(paste(name_x, name_y), paste0("trend-", input$trendline),
                        paste0("Trendline '", input$trendline,
                               "' needs positive values; showing data without a trendline."))
      yhat <- rep(NA_real_, length(x))
    } else {
      yhat <- predict(fit, fit_dat)
      if (input$trendline %in% c("exponential", "power")) yhat <- exp(yhat)
    }

    rv$scatter_fit <- fit
    fit_r2 <- if (!is.null(fit)) round(summary(fit)$r.squared, 2) * 100 else NA

    sc_title  <- list(text = paste0("<b>", name_y, "</b> vs <b>", name_x, "</b>"),
                      x = 0, xanchor = "left", xref = "paper", y = 0.985, yanchor = "top",
                      font = list(size = 15))
    sc_layout <- function(p, legend = TRUE) {
      p %>% layout(title = sc_title,
                   margin = list(t = 78, l = 72, r = 28, b = 64),
                   xaxis = list(title = list(text = name_x, standoff = 12), zeroline = FALSE),
                   yaxis = list(title = list(text = name_y, standoff = 12), zeroline = FALSE),
                   showlegend = legend,
                   legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.06, yanchor = "bottom",
                                 font = list(size = 12)),
                   hovermode = "closest") %>%
        config(displayModeBar = FALSE)
    }
    trend_name <- if (is.na(fit_r2)) paste0(input$trendline, " trend")
                  else paste0(input$trendline, " trend \u00b7 R\u00b2 = ", fit_r2, "%")
    add_trend <- function(p) {
      add_trace(p, x = x, y = yhat, name = trend_name, type = "scatter", mode = "lines",
                line = list(color = "#7C5CFF", width = 2.5, shape = "spline"),
                hoverinfo = "skip", inherit = FALSE)
    }
    pt_marker <- list(size = 11, opacity = 0.82, line = list(color = "rgba(255,255,255,0.95)", width = 1.2))
    hover_txt <- paste0("<b>", rownames(plot_dat), "</b><br>", name_x, ": ", format(round(x, 3), big.mark = ","),
                        "<br>", name_y, ": ", format(round(y, 3), big.mark = ","))

    if (input$scatter_color_var == "None"){
      plot_ly(x = x, y = y, type = "scatter", mode = "markers", name = "Programs",
              marker = c(pt_marker, list(color = "#2F6FE4")),
              text = hover_txt, hoverinfo = "text") %>%
        add_trend() %>% sc_layout()
    } else {
      if (input$scatter_color_var %in% rv$numeric_data){
        pl_colorscale <- list(c(0, "#BFD3FB"), c(0.5, "#2F6FE4"), c(1, "#6D28D9"))
        plot_ly(x = x, y = y, type = "scatter", mode = "markers", name = "Programs",
                marker = c(pt_marker, list(color = plot_dat[,input$scatter_color_var], colorscale = pl_colorscale,
                                           colorbar = list(title = list(text = input$scatter_color_var, font = list(size = 11)),
                                                           thickness = 10, len = 0.75, outlinewidth = 0))),
                text = paste0(hover_txt, "<br>", input$scatter_color_var, ": ", round(plot_dat[,input$scatter_color_var], 2)),
                hoverinfo = "text", showlegend = FALSE) %>%
          add_trend() %>% sc_layout()
      } else {
        pl_colorscale <- c("#2F6FE4", "#E0457B", "#16A34A", "#F08A24", "#7C5CFF", "#0EA5E9", "#F5B700")
        cv  <- plot_dat[, input$scatter_color_var]
        lv  <- if (is.factor(cv)) levels(cv) else sort(unique(as.character(cv)))
        grp <- factor(paste0(input$scatter_color_var, " = ", as.character(cv)),
                      levels = paste0(input$scatter_color_var, " = ", lv))
        plot_ly(x = x, y = y, type = "scatter", mode = "markers",
                color = grp, colors = pl_colorscale,
                marker = pt_marker,
                text = paste0(hover_txt, "<br>", input$scatter_color_var, ": ", plot_dat[,input$scatter_color_var]),
                hoverinfo = "text") %>%
          add_trend() %>% sc_layout()
      }
    }
  })
  
  output$scatter_regression <- DT::renderDataTable({
    get_data()
    req(rv$active_dataset)
    req(rv$scatter_fit)
    
    fit <- rv$scatter_fit
    
    x_name <- input$x_selected
    if (isTRUE(input$scatter_x_log_space)){
      x_name <- paste0("ln(",input$x_selected,")")
    }
    y_name <- input$y_selected
    if (isTRUE(input$scatter_y_log_space)){
      y_name <- paste0("ln(",input$y_selected,")")
    }
    
    r2_str <- paste0("  |  R² = ", round(summary(fit)$r.squared, 4))
    if (input$trendline == "linear"){
      name <- paste0(y_name," = ",round(fit$coefficients[1],2)," + ",round(fit$coefficients[2],4),"*",x_name, r2_str)
    } else if (input$trendline == "polynomial"){
      name <- paste0(y_name, " = ", round(fit$coefficients[1], 2), " + ", round(fit$coefficients[2], 4), "*", x_name, " + ", round(fit$coefficients[3], 4), "*", x_name, "^2", r2_str)
    } else if (input$trendline == "exponential"){
      name <- paste0(y_name," = ",round(exp(fit$coefficients[1]),2)," * exp(",round(fit$coefficients[2],4),"*",x_name,")", r2_str)
    } else if (input$trendline == "logarithmic"){
      name <- paste0(y_name," = ",round(fit$coefficients[1],2)," + ",round(fit$coefficients[2],4),"*ln(",x_name,")", r2_str)
    } else if (input$trendline == "power"){
      name <- paste0(y_name," = ",round(exp(fit$coefficients[1]),2)," * ",x_name,"^",round(fit$coefficients[2],4), r2_str)
    }
    
    dat <- round(summary(rv$scatter_fit)$coefficients,6)
    
    if(input$x_selected %in% rv$categorical_data){
      name <- " "
    }
    
    dt <- datatable(dat, colnames = setNames(1L, name), selection = 'none', options = list(dom = 't'))
    
    dt %>%
      formatCurrency("Estimate",currency = "", interval = 3, mark = ",", digits = 4) %>%
      formatCurrency("Std. Error",currency = "", interval = 3, mark = ",", digits = 4) %>%
      formatCurrency("t value",currency = "", interval = 3, mark = ",", digits = 4) %>%
      formatPercentage('Pr(>|t|)', interval = 3, digits = 3, mark = ",")
  })
  
  
  ceres_corr_heatmap <- function(plot_dat) {
    plot_ly(x = colnames(plot_dat), y = rownames(plot_dat), z = abs(plot_dat), type = "heatmap",
            text = matrix(sprintf("%+.3f", plot_dat), nrow = nrow(plot_dat)),
            zmin = 0, zmax = 1, xgap = 2, ygap = 2,
            colorscale = list(c(0, "#E0457B"), c(0.5, "#F8FAFC"), c(1, "#2F6FE4")),
            colorbar = list(title = list(text = "|r|", font = list(size = 11)),
                            thickness = 10, len = 0.8, outlinewidth = 0,
                            tickvals = c(0, 0.25, 0.5, 0.75, 1)),
            hovertemplate = "%{y} \u00d7 %{x}<br>r = %{text}  (|r| = %{z:.3f})<extra></extra>") %>%
      layout(xaxis = list(showgrid = FALSE, tickangle = -40),
             yaxis = list(showgrid = FALSE, autorange = "reversed"),
             margin = list(t = 24, l = 110, r = 20, b = 110))
  }

  output$heatmap <- renderPlotly({
    get_data()
    req(rv$active_dataset)
    
    keep_cats <- rv$categorical_data[vapply(rv$active_dataset[rv$categorical_data],
                                             function(x) length(levels(x)) >= 2L, logical(1L))]
    dat <- rv$active_dataset[, c(rv$numeric_data, keep_cats), drop = FALSE]
    if (!isTRUE(input$cor_include_cat)) {
      dat <- dat[, colnames(dat) %in% rv$numeric_data, drop = FALSE]
    }
    dat  <- dat[, colSums(!is.na(dat)) > 0L, drop = FALSE]
    dat  <- na.omit(dat)
    validate(need(ncol(dat) >= 1 && nrow(dat) >= 2,
                  "Not enough complete rows/columns to compute a correlation matrix."))
    trsf <- as.data.frame(model.matrix(~ . - 1, data = dat), check.names = FALSE)

    no_variation <- function(data) {
      out <- lapply(data, function(x) length(unique(x)))
      want <- which(!out > 1)
      unlist(want)
    }

    n_const <- length(no_variation(trsf))
    validate(need(ncol(trsf) - n_const >= 2,
                  "Fewer than two varying columns remain — no correlations to show."))
    if(n_const > 0){
      plot_dat <- cor(trsf[,-no_variation(trsf)], method = input$cor_method)
      
      rownames(plot_dat) <- unbt(rownames(plot_dat))
      colnames(plot_dat) <- unbt(colnames(plot_dat))
      
      ceres_corr_heatmap(plot_dat) %>%
        config(displayModeBar = FALSE)
    } else {
      plot_dat <- cor(trsf, method = input$cor_method)
      
      rownames(plot_dat) <- unbt(rownames(plot_dat))
      colnames(plot_dat) <- unbt(colnames(plot_dat))
      
      ceres_corr_heatmap(plot_dat) %>%
        config(displayModeBar = FALSE)
    }
  })
  

  observeEvent(input$data_select_cer_generation, {
    req(rv$active_dataset)
    sync_cer_generation_data()
    update_cer_generation_selections()
  })
  
  get_requirement <- function(col_names){
    interaction_index <- which(grepl(":", col_names, fixed = TRUE))
    requirement <- matrix(NA, nrow = 2, ncol = length(interaction_index))

    for (i in seq_along(interaction_index)){
      parts <- strsplit(col_names[interaction_index[i]], ":")[[1]]
      requirement[1,i] <- which(col_names == parts[1])
      requirement[2,i] <- which(col_names == parts[2])
    }
    colnames(requirement) <- (length(col_names) - length(interaction_index) + 1):length(col_names)

    return(requirement)
  }

  expand_for_leaps <- function(x) {
    parts   <- list()
    col_map <- character(0)

    for (col_nm in colnames(x)) {
      col <- x[[col_nm]]

      if (is.numeric(col) || is.integer(col)) {
        mat <- matrix(as.numeric(col), ncol = 1, dimnames = list(NULL, col_nm))
        parts[[length(parts) + 1]] <- mat
        col_map[col_nm] <- col_nm

      } else if (is.logical(col)) {
        mat <- matrix(as.integer(col), ncol = 1, dimnames = list(NULL, col_nm))
        parts[[length(parts) + 1]] <- mat
        col_map[col_nm] <- col_nm

      } else {
        fac  <- as.factor(col)
        lvls <- levels(fac)
        if (length(lvls) <= 1) next

        if (length(lvls) == 2) {
          mat <- matrix(as.integer(fac) - 1L, ncol = 1, dimnames = list(NULL, col_nm))
          parts[[length(parts) + 1]] <- mat
          col_map[col_nm] <- col_nm

        } else {
          tmp     <- data.frame(fac = fac)
          dum_mat <- model.matrix(~ fac, data = tmp)[, -1, drop = FALSE]
          d_names <- paste0(col_nm, "__", lvls[-1])
          colnames(dum_mat) <- d_names
          parts[[length(parts) + 1]] <- dum_mat
          for (d in d_names) col_map[d] <- col_nm
        }
      }
    }

    list(numeric_mat = do.call(cbind, parts), col_map = col_map)
  }

  run_cer_fitting <- function(
    mode,
    y_raw,
    x_raw,
    predictors,
    response_name,
    holdout,
    manual_test_set,
    error_type,
    use_interact,
    use_leaps,
    session
  ) {

    if (identical(error_type, "WLR")) {
      showNotification(WLR_REMOVED_NOTICE, type = "warning", duration = 10)
      error_type <- ceres_error_term(error_type)
    }

    x <- x_raw
    colnames(x) <- predictors

    complete_rows <- complete.cases(cbind(y_raw, x))
    na_cols <- c(
      if (any(is.na(y_raw))) response_name else character(0),
      colnames(x)[vapply(x, anyNA, logical(1L))]
    )
    if (length(na_cols) > 0) {
      showNotification(
        paste0("Warning: The following selected columns contain blank/missing values: ",
               paste(na_cols, collapse = ", "),
               ". Rows with missing values will be excluded from model fitting."),
        type = "warning", duration = 10
      )
      y_raw <- y_raw[complete_rows]
      x     <- x[complete_rows, , drop = FALSE]
    }

    if (length(unique(y_raw[!is.na(y_raw)])) <= 1) {
      showNotification(
        paste0("Error: Response variable '", response_name,
               "' is constant (all values identical). A regression model needs a response that varies. ",
               "Pick a different column."),
        type = "error", duration = 12
      )
      return(NULL)
    }

    const_cols <- colnames(x)[vapply(x, function(c) length(unique(c[!is.na(c)])) <= 1, logical(1L))]
    if (length(const_cols) > 0) {
      showNotification(
        paste0("Warning: Constant-value column(s) removed (no variance): ",
               paste(const_cols, collapse = ", "), "."),
        type = "warning", duration = 10
      )
      x <- x[, !colnames(x) %in% const_cols, drop = FALSE]
      if (ncol(x) == 0) {
        showNotification("Error: All predictors are constant. Cannot fit a model.", type = "error")
        return(NULL)
      }
    }

    if (mode == "loglin") {
      numeric_x_cols <- vapply(x, is.numeric, logical(1L))
      num_names <- colnames(x)[numeric_x_cols]

      col_status <- lapply(x[, numeric_x_cols, drop = FALSE], function(c) {
        c2 <- c[!is.na(c)]
        c(has_neg = any(c2 < 0), has_zero = any(c2 == 0))
      })
      neg_cols   <- num_names[vapply(col_status, `[[`, logical(1), "has_neg")]
      log1p_cols <- num_names[vapply(col_status, `[[`, logical(1), "has_zero")]

      if (any(y_raw < 0, na.rm = TRUE)) {
        showNotification(
          paste0("Warning: Response variable '", response_name,
                 "' contains negative values. Converting to absolute values so the log-linear model can run."),
          type = "warning", duration = 10
        )
        y_raw <- abs(y_raw)
      }

      if (length(neg_cols) > 0) {
        showNotification(
          paste0("Warning: Negative values detected in numeric predictor(s): ",
                 paste(neg_cols, collapse = ", "),
                 ". Converted to absolute values for the log-linear run."),
          type = "warning", duration = 10
        )
        x[neg_cols] <- lapply(x[neg_cols], abs)
      }

      if (any(y_raw == 0, na.rm = TRUE)) {
        showNotification(
          paste0("Error: Response variable '", response_name,
                 "' contains zero values. log(0) is undefined. Drop affected rows ",
                 "or choose a Linear model instead."),
          type = "error"
        )
        return(NULL)
      }

      if (length(log1p_cols) > 0) {
        showNotification(
          paste0("Warning: Zero values detected in numeric predictor(s): ",
                 paste(log1p_cols, collapse = ", "),
                 ". Applied log(x + 1) shift so these predictors can participate ",
                 "in the log-linear fit. Equation form for these terms is (x + 1)^b."),
          type = "warning", duration = 12
        )
      }

      y <- log(y_raw)
      x[num_names] <- Map(
        function(col, nm) if (nm %in% log1p_cols) log1p(col) else log(col),
        x[num_names], num_names
      )
    } else {
      y <- y_raw
      log1p_cols <- character(0)
    }

    nzv_idx <- ceres_ns("caret")$nearZeroVar(x)
    if (length(nzv_idx) > 0) {
      nzv_names <- colnames(x)[nzv_idx]
      showNotification(
        paste0("Warning: Near-zero variance predictor(s) removed: ",
               paste(nzv_names, collapse = ", "), "."),
        type = "warning", duration = 10
      )
      x <- x[, -nzv_idx, drop = FALSE]
      if (ncol(x) == 0) {
        showNotification("Error: All predictors have zero variance. Aborting.", type = "error")
        return(NULL)
      }
    }

    numeric_x <- x[, vapply(x, is.numeric, logical(1L)), drop = FALSE]
    if (ncol(numeric_x) > 1) {
      cor_matrix <- suppressWarnings(cor(numeric_x))
      cor_matrix[is.na(cor_matrix)] <- 0
      high_cor <- ceres_ns("caret")$findCorrelation(cor_matrix, cutoff = 0.95)
      if (length(high_cor) > 0) {
        cols_to_remove <- colnames(numeric_x)[high_cor]
        showNotification(
          paste0("Warning: Redundant predictor(s) removed (correlation > 0.95 — these produce essentially identical fitted values): ",
                 paste(cols_to_remove, collapse = ", "), "."),
          type = "warning", duration = 10
        )
        x <- x[, !colnames(x) %in% cols_to_remove, drop = FALSE]
      }
    }

    if (holdout == OLS_HOLDOUT_CHOICES[1] || holdout %in% OLS_HOLDOUT_CHOICES[5:7]) {
      train_index <- seq_along(y)
      y_test <- y

    } else if (holdout == OLS_HOLDOUT_CHOICES[2]) {
      train_index <- withr::with_seed(123, ceres_ns("caret")$createDataPartition(y, p = 0.8)[[1]])
      y_test <- y[-train_index]

    } else if (holdout == OLS_HOLDOUT_CHOICES[3]) {
      train_index <- withr::with_seed(123, ceres_ns("caret")$createDataPartition(y, p = 0.5)[[1]])
      y_test <- y[-train_index]

    } else {
      test_index <- match(manual_test_set, which(complete_rows))
      test_index <- test_index[!is.na(test_index)]
      if (length(test_index) == 0) {
        train_index <- seq_along(y)
        y_test <- y
        holdout <- OLS_HOLDOUT_CHOICES[1]
        showNotification(
          "Manual test set is empty (none selected, or all selected rows had missing values) — training on all data.",
          type = "warning", duration = 8
        )
      } else if (length(test_index) >= length(y)) {
        train_index <- seq_along(y)
        y_test <- y
        holdout <- OLS_HOLDOUT_CHOICES[1]
        showNotification(
          "Manual test set contains every usable row — no rows left to train on. Training on all data instead.",
          type = "warning", duration = 8
        )
      } else {
        all_index <- seq_along(y)
        train_index <- all_index[-test_index]
        y_test <- y[-train_index]
      }
    }

    predict_all <- holdout %in% OLS_HOLDOUT_CHOICES[c(1L, 5:7)]

    interact_path <- use_interact && ncol(x) > 1L
    if (interact_path) {
      if (any(grepl(":", colnames(x), fixed = TRUE))) {
        showNotification(
          paste0("Interaction Effects cannot run with column names containing ':' (",
                 paste(grep(":", colnames(x), fixed = TRUE, value = TRUE), collapse = ", "),
                 ") — rename those columns first."),
          type = "error", duration = 12
        )
        return(NULL)
      }
      x_sub <- model.matrix(y ~ ., data = cbind(y, x))[, -1, drop = FALSE]
      x <- model.matrix(y ~ .^2, data = cbind(y, x))[, -1, drop = FALSE]
      colnames(x_sub) <- unbt(colnames(x_sub))
      colnames(x)     <- unbt(colnames(x))

      if (ncol(x) > 24L) {
        showNotification(
          paste0("Interaction Effects expanded the design to ", ncol(x), " columns — ",
                 "exhaustive interaction search is capped at 24 to avoid exhausting memory. ",
                 "Reduce the number of predictors (or run without Interaction Effects)."),
          type = "error", duration = 15
        )
        return(NULL)
      }

      n_main    <- ncol(x_sub)
      p_all     <- ncol(x)
      inter_idx <- if (p_all > n_main) (n_main + 1L):p_all else integer(0)
      req_mat   <- get_requirement(colnames(x))

      interact_screened <- isTRUE(use_leaps) && n_main > 5L && length(inter_idx) > 0L

      subset_specs <- NULL
      if (interact_screened) {
        x_screen <- x[train_index, , drop = FALSE]
        keep_idx <- seq_len(ncol(x_screen))
        qr_scr   <- qr(cbind(1, x_screen))
        qr_scr   <- list(rank = qr_scr$rank - 1L, pivot = qr_scr$pivot[-1L] - 1L)
        if (qr_scr$rank < ncol(x_screen)) {
          keep_idx <- sort(qr_scr$pivot[seq_len(qr_scr$rank)])
          x_screen <- x_screen[, keep_idx, drop = FALSE]
        }
        nbest_scr <- 10L
        scr_which <- tryCatch(screen_best_subsets(x_screen, y[train_index], nbest = nbest_scr),
                              error = function(e) NULL)
        if (!is.null(scr_which)) {
          seen      <- new.env(parent = emptyenv())
          specs_out <- vector("list", 0L)
          n_specs   <- 0L
          for (r in seq_len(nrow(scr_which))) {
            sel      <- keep_idx[which(scr_which[r, ])]
            sel_ints <- sel[sel %in% inter_idx]
            if (length(sel_ints)) {
              parents <- unique(unlist(lapply(as.character(sel_ints),
                                              function(ii) req_mat[, ii])))
              sel <- union(sel, parents)
            }
            sel <- sort(as.integer(sel))
            key <- paste(sel, collapse = ",")
            if (!isTRUE(seen[[key]])) {
              seen[[key]]  <- TRUE
              n_specs      <- n_specs + 1L
              specs_out[[n_specs]] <- sel
            }
          }
          subset_specs <- specs_out
          showNotification(
            paste0("Optimize Search: interaction space screened by branch-and-bound — ",
                   "evaluating ", n_specs, " hierarchy-completed candidates (top ",
                   nbest_scr, " per model size). Uncheck 'Optimize Search' to run ",
                   "the full exhaustive hierarchical sweep."),
            type = "message", duration = 10)
        }
      }

      if (is.null(subset_specs)) {
      subset_specs <- vector("list", 0L)
      n_specs      <- 0L
      main_bit     <- 0:(n_main - 1L)
      for (mbits in seq_len(2L^n_main - 1L)) {
        M <- (1L:n_main)[bitwAnd(mbits, bitwShiftL(1L, main_bit)) != 0L]
        allowed <- if (length(inter_idx)) {
          inter_idx[vapply(inter_idx,
                           function(ii) all(req_mat[, as.character(ii)] %in% M),
                           logical(1))]
        } else integer(0)
        n_alw   <- length(allowed)
        alw_bit <- if (n_alw > 0L) 0:(n_alw - 1L) else integer(0)
        for (ibits in 0:(2L^n_alw - 1L)) {
          I <- if (n_alw > 0L) allowed[bitwAnd(ibits, bitwShiftL(1L, alw_bit)) != 0L] else integer(0)
          n_specs <- n_specs + 1L
          subset_specs[[n_specs]] <- sort(c(M, I))
        }
      }
      if (n_specs > 2L^20L) {
        showNotification(
          paste0("Interaction Effects generated ", format(n_specs, big.mark = ","),
                 " hierarchical candidate models — capped at ",
                 format(2L^20L, big.mark = ","), " to avoid exhausting the ",
                 "container. Reduce the number of predictors."),
          type = "error", duration = 15
        )
        return(NULL)
      }
      }
      x_names      <- colnames(x)
      total_models <- n_specs
    }

    model_list    <- list()
    predictors_list <- list()
    yhat_test     <- list()
    p_eff_known   <- NULL

    num_predictors <- ncol(x)
    cntr <- 0

    w_sig <- 0.005
    w_k   <- 0.010
    w_vif <- 0.005
    gamma_ebic <- 1.1

    progress_label <- if (mode == "loglin") "Fitting Log Linear Permutations" else "Fitting Linear Permutations"
    ceres_mark("search: inputs prepared")
    PROG_MAX <- 1 / 0.6
    progress1 <- shiny::Progress$new(session, min = 0, max = PROG_MAX)
    progress1$set(message = progress_label, value = 0)
    prog_handed_on <- FALSE
    on.exit(if (!prog_handed_on) suppressWarnings(try(progress1$close(), silent = TRUE)), add = TRUE)

    {
      selected_error <- error_type
      irls_cap <- 1000L

      used_ols_fast_path <- FALSE

      if (!interact_path) {
        use_leaps_flag <- isTRUE(use_leaps) && (num_predictors >= 8)

        if (use_leaps_flag) {
          progress1$set(message = paste("Optimizing", if (mode == "loglin") "Log Linear" else "Linear", "Permutations"), value = 1)
          leaps_prep <- expand_for_leaps(x[train_index, , drop = FALSE])
          leaps_x    <- leaps_prep$numeric_mat
          col_map    <- leaps_prep$col_map
          leaps_y    <- as.numeric(y[train_index])
          qr_leaps <- qr(cbind(1, leaps_x))
          qr_leaps <- list(rank = qr_leaps$rank - 1L, pivot = qr_leaps$pivot[-1L] - 1L)
          if (qr_leaps$rank < ncol(leaps_x)) {
            dep_cols <- colnames(leaps_x)[qr_leaps$pivot[-seq_len(qr_leaps$rank)]]
            showNotification(
              paste0("Removed linearly dependent column(s) before screening: ",
                     paste(dep_cols, collapse = ", "),
                     ". (Exact linear combinations cannot be searched.)"),
              type = "warning", duration = 12
            )
            leaps_x <- leaps_x[, qr_leaps$pivot[seq_len(qr_leaps$rank)], drop = FALSE]
          }
          screen_p_cols <- ncol(leaps_x)
          screen_est_s  <- min(120, max(0.3, if (screen_lmsubsets_ok()) 0.34 * 1.4^(screen_p_cols - 44)
                                             else 1.5 * 1.6^(screen_p_cols - 32)))
          screen_limit <- screen_exact_limit()
          if (screen_p_cols > screen_limit) {
            showNotification(
              paste0("Screening ", screen_p_cols, " expanded columns — exhaustive search past ",
                     screen_limit, " columns would block the app for a very long time, so the ",
                     "optimizer switched to sequential-replacement screening. Reduce predictors ",
                     "(or collapse high-cardinality categoricals) for a fully exhaustive search."),
              type = "warning", duration = 15
            )
          }
          session$sendCustomMessage("cer_screen_start",
                                    list(id = "screen", tau_ms = screen_est_s * 500, target = 0.95))
          reg_which <- tryCatch(screen_best_subsets(leaps_x, leaps_y, nbest = 32L),
                                error = function(e) e)
          session$sendCustomMessage("cer_screen_done", list(id = "screen", done = TRUE))
          if (inherits(reg_which, "error")) {
            showNotification(
              paste0("Optimized screening failed (", conditionMessage(reg_which),
                     "). Try fewer predictors, or untick 'Optimize Search'."),
              type = "error", duration = 15
            )
            return(NULL)
          }

          candidate_preds <- unique(lapply(seq_len(nrow(reg_which)), function(i) {
            selected_dummies <- colnames(reg_which)[reg_which[i, ]]
            sort(unique(col_map[selected_dummies]))
          }))
          x_names      <- colnames(x)
          subset_specs <- lapply(candidate_preds, function(nm) sort(match(nm, x_names)))
          total_models <- length(subset_specs)
          progress1$set(message = paste("Fitting", if (mode == "loglin") "Log Linear" else "Linear",
                                        "Permutations"), value = 0)
        } else {
          if (num_predictors > 20L) {
            showNotification(
              paste0("Exhaustive search over ", num_predictors, " predictors would fit ",
                     format(2^num_predictors - 1, big.mark = ","), " models — capped at 20 ",
                     "predictors to avoid exhausting the container. Enable 'Optimize Search' ",
                     "(handles any count) or reduce predictors."),
              type = "error", duration = 15
            )
            return(NULL)
          }
          if (num_predictors > 12) {
            showNotification(
              paste0("Running all ", 2L^num_predictors - 1L, " model permutations (",
                     num_predictors, " predictors). This may take several minutes. ",
                     "Enable 'Optimize Search' to reduce computation time."),
              type = "warning", duration = 20
            )
          }
          total_models <- 2L^num_predictors - 1L
          x_names      <- colnames(x)
          subset_specs <- vector("list", total_models)
          idx <- 0L
          for (i in seq_len(num_predictors)) {
            cases <- combn(num_predictors, i)
            for (j in seq_len(ncol(cases))) {
              idx <- idx + 1L
              subset_specs[[idx]] <- as.integer(cases[, j])
            }
          }
        }
      }
          n_cores  <- N_CORES
          use_par  <- FALSE

          if (selected_error == "OLS") {
            used_ols_fast_path <- TRUE
            X_mm    <- model.matrix(~ ., data = as.data.frame(x))
            mm_asgn <- attr(X_mm, "assign")
            y_tr    <- y[train_index]
            n_tr    <- length(y_tr)
            ss_tot  <- sum((y_tr - mean(y_tr))^2)

            results     <- matrix(NA_real_, nrow = total_models, ncol = 8)
            model_names <- character(total_models)
            aic_vec  <- numeric(total_models)
            bic_vec  <- numeric(total_models)
            R_full <- cor(X_mm[train_index, -1L, drop = FALSE])
            X_trm  <- X_mm[train_index, , drop = FALSE]
            gram   <- search_gram(X_trm, y_tr)
            mm_nms <- colnames(X_mm)

            fast_ols <- NULL
            progress1$set(value = 0.03, detail = paste(format(total_models, big.mark = ","), "models"))
            if (!is.null(CERES_FAST)) {
              jcols    <- tryCatch(CERES_FAST$subset_cols(subset_specs, as.integer(mm_asgn[-1L])),
                                   error = function(e) lapply(subset_specs, function(sp) which(mm_asgn[-1L] %in% sp)))
              X_pred   <- if (predict_all) X_mm else X_mm[-train_index, , drop = FALSE]
              fast_ols <- tryCatch(CERES_FAST$ols_subsets(gram$C, gram$s, gram$xb, gram$sxy, gram$syy, gram$yb,
                                                          gram$thr, gram$ok, jcols, as.integer(mm_asgn[-1L]),
                                                          X_pred, as.integer(n_tr), CERES_FAST$threads),
                                   error = function(e) NULL)
            }
            todo <- seq_along(subset_specs)
            if (!is.null(fast_ols)) {
              idx  <- which(fast_ols$ok == 1L)
              todo <- which(fast_ols$ok != 1L)
              if (length(idx)) {
                k_f    <- fast_ols$k[idx]; ssr_f <- fast_ols$ssr[idx]
                df_f   <- n_tr - k_f - 1L
                r2_f   <- 1 - ssr_f / ss_tot
                adj_f  <- 1 - (1 - r2_f) * (n_tr - 1L) / pmax(df_f, 1L)
                sig_f  <- sqrt(ssr_f / pmax(df_f, 1L))
                ll_f   <- -n_tr / 2 * (log(2 * pi) + 1 - log(n_tr) + log(ssr_f))
                aic_vec[idx] <- -2 * ll_f + 2 * (k_f + 2L)
                bic_vec[idx] <- -2 * ll_f + log(n_tr) * (k_f + 2L)
                YH <- fast_ols$yhat[, idx, drop = FALSE]
                if (mode == "loglin") {
                  m6_f <- ifelse(sig_f > 2.1482832, 10, sqrt(exp(sig_f^2) - 1))
                  m7_f <- ifelse(sig_f > 2.1899295, 10, exp(sig_f^2 / 2) - 1)
                } else {
                  m6_f <- sqrt(colMeans((y_test - YH)^2, na.rm = TRUE)) / abs(mean(y_test))
                  m7_f <- colMeans(abs(y_test - YH) / pmax(abs(y_test), 1e-10), na.rm = TRUE)
                }
                results[idx, ] <- cbind(lengths(subset_specs[idx]), r2_f, adj_f, sig_f,
                                        fast_ols$nsig[idx] / k_f, m6_f, m7_f, fast_ols$vif[idx])
                p_eff_known <- rep(NA_real_, total_models)
                p_eff_known[idx] <- rowSums(!is.na(fast_ols$b[idx, -1L, drop = FALSE]))
                progress1$set(value = 0.5, detail = "assembling")
                asm <- tryCatch(CERES_FAST$assemble(fast_ols, idx, jcols, subset_specs, x_names, mm_nms,
                                                    0L, FALSE, as.integer(n_tr)), error = function(e) NULL)
                if (!is.null(asm)) {
                  predictors_list[idx] <- asm$pred; model_names[idx] <- asm$names
                  yhat_test[idx] <- asm$yhat;       model_list[idx]  <- asm$specs
                  rm(asm)
                } else {
                pn_f <- lapply(subset_specs[idx], function(sp) x_names[sp])
                predictors_list[idx] <- pn_f
                model_names[idx]     <- vapply(pn_f, paste, character(1), collapse = ", ")
                yhat_test[idx]       <- lapply(seq_along(idx), function(i) YH[, i])
                model_list[idx]      <- lapply(seq_along(idx), function(i) {
                  cols <- c(1L, jcols[[idx[i]]] + 1L)
                  list(pred_names = pn_f[[i]],
                       coefficients = setNames(fast_ols$b[idx[i], seq_along(cols)], mm_nms[cols]))
                })
                }
                rm(YH)
              }
              progress1$set(value = if (length(todo)) length(idx) / total_models else 1,
                            detail = paste0(round(length(idx) / total_models * 100), "%"))
            }

            for (cntr in todo) {
              spec       <- subset_specs[[cntr]]
              pred_names <- x_names[spec]
              xcols      <- c(1L, which(mm_asgn %in% spec))
              xcols_all  <- xcols
              g_U        <- gram_chol(gram, xcols[-1L] - 1L)
              g_fit      <- if (!is.null(g_U)) gram_ols(gram, xcols[-1L] - 1L, g_U) else NULL
              if (!is.null(g_fit)) {
                coefs    <- setNames(g_fit$b, mm_nms[xcols])
                ss_res   <- g_fit$ssr
              } else {
                xcols    <- fullrank_cols(X_trm, xcols)
                X_tr     <- X_trm[, xcols, drop = FALSE]
                fit      <- .lm.fit(X_tr, y_tr)
                coefs    <- setNames(fit$coefficients, colnames(X_tr))
                coefs[!is.finite(coefs)] <- 0
                ss_res   <- sum(fit$residuals^2)
              }
              p_eff      <- length(xcols) - 1L
              df_r       <- n_tr - p_eff - 1L

              r2     <- 1 - ss_res / ss_tot
              adj_r2 <- 1 - (1 - r2) * (n_tr - 1L) / max(df_r, 1L)
              sigma  <- sqrt(ss_res / max(df_r, 1L))
              ll_val <- -n_tr / 2 * (log(2 * pi) + 1 - log(n_tr) + log(ss_res))
              df_mod <- p_eff + 2L
              aic_vec[cntr] <- -2 * ll_val + 2 * df_mod
              bic_vec[cntr] <- -2 * ll_val + log(n_tr) * df_mod

              if (!is.null(g_fit)) {
                prop_sig <- { t_v <- g_fit$b[-1L] / (sigma * sqrt(pmax(g_fit$inv, 0))); t_v[!is.finite(t_v)] <- 0
                  n_np <- length(xcols_all) - 1L
                  if (n_np > 0L) sum(abs(t_v) >= 2) / n_np else NA_real_ }
              } else {
              XtXinv   <- tryCatch(chol2inv(chol(crossprod(X_tr))), error = function(e) NULL)
              prop_sig <- if (!is.null(XtXinv) && all(is.finite(diag(XtXinv)))) {
                se_v <- sigma * sqrt(pmax(diag(XtXinv), 0))
                { t_v <- (coefs / se_v)[-1L]; t_v[!is.finite(t_v)] <- 0
                  n_np <- length(xcols_all) - 1L
                  if (n_np > 0L) sum(abs(t_v) >= 2) / n_np else NA_real_ }
              } else NA_real_
              }

              max_vif <- if (p_eff <= 1L) 1.0 else
                if (!is.null(g_fit)) max(gram_vif_graded(gram, xcols[-1L] - 1L, g_fit$Rinv, mm_asgn[xcols[-1L]])) else
                tryCatch(max(term_vif_graded(R_full[xcols[-1L] - 1L, xcols[-1L] - 1L, drop = FALSE], mm_asgn[xcols[-1L]])),
                         error = function(e) NA_real_)

              if (predict_all) {
                yhat_test_vec <- as.numeric(X_mm[, xcols, drop = FALSE] %*% coefs)
              } else {
                yhat_test_vec <- as.numeric(X_mm[-train_index, xcols, drop = FALSE] %*% coefs)
              }

              if (mode == "loglin") {
                m6 <- if (sigma > 2.1482832) 10 else sqrt(exp(sigma^2) - 1)
                m7 <- if (sigma > 2.1899295) 10 else exp(sigma^2 / 2) - 1
              } else {
                m6 <- sqrt(mean((y_test - yhat_test_vec)^2, na.rm = TRUE)) / abs(mean(y_test))
                m7 <- mean(abs(y_test - yhat_test_vec) / pmax(abs(y_test), 1e-10), na.rm = TRUE)
              }
              results[cntr, ] <- c(length(spec), r2, adj_r2, sigma, prop_sig, m6, m7, max_vif)

              yhat_test[[cntr]]     <- yhat_test_vec
              predictors_list[[cntr]] <- pred_names
              model_names[cntr]    <- paste(pred_names, collapse = ", ")
              model_list[[cntr]]   <- list(
                pred_names    = pred_names,
                coefficients  = expand_coefs(coefs, colnames(X_mm)[xcols_all])
              )
              if (cntr %% max(1L, total_models %/% 100L) == 0L || cntr == total_models)
                progress1$set(value = cntr / total_models,
                              detail = paste0(round(cntr / total_models * 100), "%"))
            }
            cntr    <- total_models
            results <- as.data.frame(results)

          } else if (selected_error == "MUPE" && !use_par) {
            used_ols_fast_path <- TRUE
            X_mm    <- model.matrix(~ ., data = as.data.frame(x))
            mm_asgn <- attr(X_mm, "assign")
            y_tr    <- y[train_index]
            n_tr    <- length(y_tr)

            results     <- matrix(NA_real_, nrow = total_models, ncol = 8)
            model_names <- character(total_models)
            aic_vec  <- numeric(total_models)
            bic_vec  <- numeric(total_models)
            R_full <- cor(X_mm[train_index, -1L, drop = FALSE])
            X_trm  <- X_mm[train_index, , drop = FALSE]
            gram   <- search_gram(X_trm, y_tr)
            mm_nms <- colnames(X_mm)

            fast_m <- NULL
            progress1$set(value = 0.03, detail = paste(format(total_models, big.mark = ","), "models"))
            if (!is.null(CERES_FAST$mupe_subsets)) {
              jcols  <- tryCatch(CERES_FAST$subset_cols(subset_specs, as.integer(mm_asgn[-1L])),
                                 error = function(e) lapply(subset_specs, function(sp) which(mm_asgn[-1L] %in% sp)))
              X_pred <- if (predict_all) X_mm else X_mm[-train_index, , drop = FALSE]
              fast_m <- tryCatch(CERES_FAST$mupe_subsets(X_trm, y_tr, mode == "loglin", gram$C, gram$thr, gram$ok,
                                                         jcols, as.integer(mm_asgn[-1L]), X_pred, as.integer(irls_cap),
                                                         CERES_FAST$threads),
                                 error = function(e) NULL)
            }
            todo <- seq_along(subset_specs)
            if (!is.null(fast_m)) {
              idx  <- which(fast_m$ok == 1L)
              todo <- which(fast_m$ok != 1L)
              if (length(idx)) {
                k_f   <- lengths(jcols[idx])
                sig_f <- fast_m$sigma[idx]
                aic_vec[idx] <- -2 * fast_m$ll[idx] + 2 * (k_f + 2L)
                bic_vec[idx] <- -2 * fast_m$ll[idx] + log(n_tr) * (k_f + 2L)
                YH <- fast_m$yhat[, idx, drop = FALSE]
                if (mode == "loglin") {
                  m6_f <- ifelse(sig_f > 2.1482832, 10, sqrt(exp(sig_f^2) - 1))
                  m7_f <- ifelse(sig_f > 2.1899295, 10, exp(sig_f^2 / 2) - 1)
                } else {
                  m6_f <- sqrt(colMeans((y_test - YH)^2, na.rm = TRUE)) / abs(mean(y_test))
                  m7_f <- colMeans(abs(y_test - YH) / pmax(abs(y_test), 1e-10), na.rm = TRUE)
                }
                results[idx, ] <- cbind(lengths(subset_specs[idx]), fast_m$r2[idx], fast_m$adj_r2[idx], sig_f,
                                        ifelse(fast_m$sig_ok[idx] == 1L, fast_m$nsig[idx] / k_f, NA_real_),
                                        m6_f, m7_f, fast_m$vif[idx])
                p_eff_known <- rep(NA_real_, total_models)
                p_eff_known[idx] <- rowSums(!is.na(fast_m$b[idx, -1L, drop = FALSE]))
                progress1$set(value = 0.5, detail = "assembling")
                asm <- tryCatch(CERES_FAST$assemble(fast_m, idx, jcols, subset_specs, x_names, mm_nms,
                                                    1L, mode == "loglin", as.integer(n_tr)), error = function(e) NULL)
                if (!is.null(asm)) {
                  predictors_list[idx] <- asm$pred; model_names[idx] <- asm$names
                  yhat_test[idx] <- asm$yhat;       model_list[idx]  <- asm$specs
                  rm(asm)
                } else {
                pn_f <- lapply(subset_specs[idx], function(sp) x_names[sp])
                predictors_list[idx] <- pn_f
                model_names[idx]     <- vapply(pn_f, paste, character(1), collapse = ", ")
                yhat_test[idx]       <- lapply(seq_along(idx), function(i) YH[, i])
                model_list[idx]      <- lapply(seq_along(idx), function(i) {
                  m     <- idx[i]
                  cols  <- c(1L, jcols[[m]] + 1L)
                  kk    <- length(cols) - 1L
                  dfr   <- n_tr - kk - 1L
                  coefs <- setNames(fast_m$b[m, seq_along(cols)], mm_nms[cols])
                  ms    <- NULL
                  if (mode == "loglin") {
                    cu  <- fast_m$cov[[m]]
                    se  <- if (!is.null(cu)) fast_m$sigma[m] * sqrt(pmax(diag(cu), 0)) else rep(NA_real_, length(coefs))
                    tv  <- coefs / se
                    tbl <- cbind(Estimate = coefs, "Std. Error" = se, "t value" = tv,
                                 "Pr(>|t|)" = 2 * pt(-abs(tv), df = max(dfr, 1L)))
                    colnames(tbl) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
                    rownames(tbl) <- names(coefs)
                    r2m   <- fast_m$r2[m]
                    f_val <- if (kk > 0L && dfr > 0L && is.finite(r2m) && r2m < 1) (r2m / kk) / ((1 - r2m) / dfr) else NA_real_
                    ms <- list(coefficients  = tbl,
                               r.squared     = r2m,
                               adj.r.squared = fast_m$adj_r2[m],
                               sigma         = fast_m$sigma[m],
                               df            = c(kk + 1L, max(dfr, 0L), kk + 1L),
                               fstatistic    = if (is.finite(f_val)) c(value = f_val, numdf = kk, dendf = dfr) else NULL,
                               cov.unscaled  = if (!is.null(cu)) cu else matrix(NA_real_, length(coefs), length(coefs)),
                               aliased       = setNames(rep(FALSE, length(cols)), mm_nms[cols]))
                  }
                  list(pred_names = pn_f[[i]], coefficients = coefs, weights = fast_m$w[, m],
                       converged = fast_m$conv[m] == 1L, iterations = fast_m$iter[m], mupe_summary = ms)
                })
                }
                rm(YH)
              }
              progress1$set(value = if (length(todo)) length(idx) / total_models else 1,
                            detail = paste0(round(length(idx) / total_models * 100), "%"))
            }

            for (cntr in todo) {
              spec       <- subset_specs[[cntr]]
              pred_names <- x_names[spec]
              xcols      <- c(1L, which(mm_asgn %in% spec))
              xcols_all  <- xcols
              g_U        <- gram_chol(gram, xcols[-1L] - 1L)
              if (is.null(g_U)) xcols <- fullrank_cols(X_trm, xcols)
              X_tr       <- X_trm[, xcols, drop = FALSE]
              p_eff      <- length(xcols) - 1L
              df_r       <- n_tr - p_eff - 1L

              if (mode == "loglin") {
                Y_unit <- exp(y_tr)
                coefs  <- setNames(.lm.fit(X_tr, y_tr)$coefficients, colnames(X_tr))
                coefs[!is.finite(coefs)] <- 0
                conv <- FALSE
                XtX_U <- tryCatch(chol(crossprod(X_tr)), error = function(e) NULL)
                for (k in seq_len(irls_cap)) {
                  yh_unit <- exp(as.vector(X_tr %*% coefs))
                  s       <- Y_unit / yh_unit - 1
                  delta   <- if (!is.null(XtX_U))
                    backsolve(XtX_U, forwardsolve(XtX_U, drop(crossprod(X_tr, s)), upper.tri = TRUE, transpose = TRUE))
                  else tryCatch(
                    as.vector(solve(crossprod(X_tr), crossprod(X_tr, s))),
                    error = function(e) NULL
                  )
                  if (is.null(delta) || any(!is.finite(delta))) break
                  coefs <- setNames(coefs + delta, colnames(X_tr))
                  delta_norm <- sqrt(sum(delta^2))
                  if (!is.finite(delta_norm)) break
                  if (delta_norm < 1e-12) { conv <- TRUE; break }
                }
                yhat      <- as.vector(X_tr %*% coefs)
                yhat_unit <- exp(yhat)
                r         <- y_tr - yhat
                r_unit    <- Y_unit - yhat_unit
                w_cur     <- 1 / yhat_unit^2
                sigma     <- sqrt(sum(r^2) / max(df_r, 1L))
                rss_w     <- sum(w_cur * r_unit^2)
                y_bar_w   <- sum(w_cur * Y_unit) / sum(w_cur)
                tss_w     <- sum(w_cur * (Y_unit - y_bar_w)^2)
                r2        <- 1 - rss_w / tss_w
                adj_r2    <- 1 - (1 - r2) * (n_tr - 1L) / max(df_r, 1L)
                ll_val    <- 0.5 * (sum(log(w_cur)) - n_tr * (log(2 * pi) + 1 - log(n_tr) + log(rss_w)))
              } else {
                coefs <- setNames(.lm.fit(X_tr, y_tr)$coefficients, colnames(X_tr))
                coefs[!is.finite(coefs)] <- 0
                yhat  <- as.vector(X_tr %*% coefs)
                prev  <- coefs
                w_cur <- rep(1.0, n_tr)
                conv  <- FALSE
                for (k in seq_len(irls_cap)) {
                  yhat[abs(yhat) < 1e-6] <- 1e-6
                  w_cur <- 1 / yhat^2
                  sw    <- sqrt(w_cur)
                  coefs <- setNames(.lm.fit(X_tr * sw, y_tr * sw)$coefficients, colnames(X_tr))
                  yhat  <- as.vector(X_tr %*% coefs)
                  if (irls_step_small(coefs, prev)) { conv <- TRUE; break }
                  prev  <- coefs
                }
                yhat[abs(yhat) < 1e-6] <- 1e-6
                w_cur <- 1 / yhat^2
                r       <- y_tr - yhat
                rss_w   <- sum(w_cur * r^2)
                y_bar_w <- sum(w_cur * y_tr) / sum(w_cur)
                tss_w   <- sum(w_cur * (y_tr - y_bar_w)^2)
                r2      <- 1 - rss_w / tss_w
                adj_r2  <- 1 - (1 - r2) * (n_tr - 1L) / max(df_r, 1L)
                sigma   <- sqrt(rss_w / max(df_r, 1L))
                ll_val  <- 0.5 * (sum(log(w_cur)) - n_tr * (log(2 * pi) + 1 - log(n_tr) + log(rss_w)))
              }

              if (mode == "loglin") {
                cov_unscaled <- if (!is.null(XtX_U)) chol2inv(XtX_U) else
                  tryCatch(chol2inv(chol(crossprod(X_tr))), error = function(e) NULL)
                cov_sigma    <- sqrt(sum(r^2) / max(df_r, 1L))
              } else {
                cov_unscaled <- tryCatch(chol2inv(chol(crossprod(X_tr * sqrt(w_cur)))), error = function(e) NULL)
                cov_sigma    <- sigma
              }
              prop_sig <- if (!is.null(cov_unscaled) && all(is.finite(diag(cov_unscaled)))) {
                se_v <- cov_sigma * sqrt(pmax(diag(cov_unscaled), 0))
                { t_v <- (coefs / se_v)[-1L]; t_v[!is.finite(t_v)] <- 0
                  n_np <- length(xcols_all) - 1L
                  if (n_np > 0L) sum(abs(t_v) >= 2) / n_np else NA_real_ }
              } else NA_real_

              max_vif <- if (p_eff <= 1L) 1.0 else
                if (!is.null(g_U)) max(gram_vif_graded(gram, xcols[-1L] - 1L, chol2inv(g_U), mm_asgn[xcols[-1L]])) else
                tryCatch(max(term_vif_graded(R_full[xcols[-1L] - 1L, xcols[-1L] - 1L, drop = FALSE], mm_asgn[xcols[-1L]])),
                         error = function(e) NA_real_)

              if (predict_all) {
                yhat_test_vec <- as.numeric(X_mm[, xcols, drop = FALSE] %*% coefs)
              } else {
                yhat_test_vec <- as.numeric(X_mm[-train_index, xcols, drop = FALSE] %*% coefs)
              }

              df_mod <- p_eff + 2L
              aic_vec[cntr] <- -2 * ll_val + 2 * df_mod
              bic_vec[cntr] <- -2 * ll_val + log(n_tr) * df_mod

              results[cntr, 1:5] <- c(length(spec), r2, adj_r2, sigma, prop_sig)

              if (mode == "loglin") {
                geo_cv <- if (sigma > 2.1482832) 10 else sqrt(exp(sigma^2) - 1)
                mult_e <- if (sigma > 2.1899295) 10 else exp(sigma^2 / 2) - 1
                results[cntr, 6:7] <- c(geo_cv, mult_e)
              } else {
                rmse_val <- sqrt(mean((y_test - yhat_test_vec)^2, na.rm = TRUE))
                mape_val <- mean(abs(y_test - yhat_test_vec) / pmax(abs(y_test), 1e-10), na.rm = TRUE)
                results[cntr, 6:7] <- c(rmse_val / abs(mean(y_test)), mape_val)
              }

              results[cntr, 8] <- max_vif

              yhat_test[[cntr]]       <- yhat_test_vec
              predictors_list[[cntr]] <- pred_names
              model_names[cntr]       <- paste(pred_names, collapse = ", ")

              coef_se_raw <- if (!is.null(cov_unscaled)) cov_sigma * sqrt(pmax(diag(cov_unscaled), 0)) else rep(NA_real_, length(coefs))
              est_disp <- coefs
              se_disp  <- coef_se_raw
              t_vals   <- est_disp / se_disp
              p_vals   <- 2 * pt(-abs(t_vals), df = max(df_r, 1L))
              coef_tbl <- cbind(Estimate = est_disp,
                                "Std. Error" = se_disp,
                                "t value"  = t_vals,
                                "Pr(>|t|)" = p_vals)
              colnames(coef_tbl) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
              rownames(coef_tbl) <- names(coefs)
              f_val <- if (p_eff > 0L && df_r > 0L && is.finite(r2) && r2 < 1) (r2 / p_eff) / ((1 - r2) / df_r) else NA_real_
              fstat <- if (is.finite(f_val)) c(value = f_val, numdf = p_eff, dendf = df_r) else NULL

              mupe_summary_obj <- list(
                coefficients  = coef_tbl,
                r.squared     = r2,
                adj.r.squared = adj_r2,
                sigma         = sigma,
                df            = c(p_eff + 1L, max(df_r, 0L), p_eff + 1L),
                fstatistic    = fstat,
                cov.unscaled  = if (!is.null(cov_unscaled)) cov_unscaled else matrix(NA_real_, length(coefs), length(coefs)),
                aliased       = setNames(!(colnames(X_mm)[xcols_all] %in% colnames(X_tr)), colnames(X_mm)[xcols_all])
              )

              model_list[[cntr]]      <- list(
                pred_names    = pred_names,
                coefficients  = expand_coefs(setNames(coefs, colnames(X_tr)), colnames(X_mm)[xcols_all]),
                weights       = w_cur,
                converged     = conv, iterations = k,
                mupe_summary  = if (mode == "loglin") mupe_summary_obj else NULL
              )

              if (cntr %% max(1L, total_models %/% 100L) == 0L || cntr == total_models)
                progress1$set(value = cntr / total_models,
                              detail = paste0(round(cntr / total_models * 100), "%"))
            }
            cntr    <- total_models
            results <- as.data.frame(results)

          } else if (selected_error == "ZMPE" && !use_par) {
            used_ols_fast_path <- TRUE
            X_mm    <- model.matrix(~ ., data = as.data.frame(x))
            mm_asgn <- attr(X_mm, "assign")
            y_tr    <- y[train_index]
            n_tr    <- length(y_tr)

            results     <- matrix(NA_real_, nrow = total_models, ncol = 8)
            model_names <- character(total_models)
            aic_vec  <- numeric(total_models)
            bic_vec  <- numeric(total_models)
            R_full <- cor(X_mm[train_index, -1L, drop = FALSE])
            X_trm  <- X_mm[train_index, , drop = FALSE]
            gram   <- search_gram(X_trm, y_tr)
            mm_nms <- colnames(X_mm)

            fast_z <- NULL
            progress1$set(value = 0.03, detail = paste(format(total_models, big.mark = ","), "models"))
            if (!is.null(CERES_FAST$zmpe_subsets)) {
              jcols  <- tryCatch(CERES_FAST$subset_cols(subset_specs, as.integer(mm_asgn[-1L])),
                                 error = function(e) lapply(subset_specs, function(sp) which(mm_asgn[-1L] %in% sp)))
              X_pred <- if (predict_all) X_mm else X_mm[-train_index, , drop = FALSE]
              fast_z <- tryCatch(CERES_FAST$zmpe_subsets(X_trm, y_tr, mode == "loglin", gram$C, gram$thr, gram$ok,
                                                         jcols, as.integer(mm_asgn[-1L]), X_pred, 1000L,
                                                         CERES_FAST$threads),
                                 error = function(e) NULL)
            }
            todo <- seq_along(subset_specs)
            if (!is.null(fast_z)) {
              idx  <- which(fast_z$ok == 1L)
              todo <- which(fast_z$ok != 1L)
              if (length(idx)) {
                k_f   <- lengths(jcols[idx])
                sig_f <- fast_z$sigma[idx]
                aic_vec[idx] <- -2 * fast_z$ll[idx] + 2 * (k_f + 2L)
                bic_vec[idx] <- -2 * fast_z$ll[idx] + log(n_tr) * (k_f + 2L)
                YH <- fast_z$yhat[, idx, drop = FALSE]
                if (mode == "loglin") {
                  m6_f <- ifelse(sig_f > 2.1482832, 10, sqrt(exp(sig_f^2) - 1))
                  m7_f <- ifelse(sig_f > 2.1899295, 10, exp(sig_f^2 / 2) - 1)
                } else {
                  m6_f <- sqrt(colMeans((y_test - YH)^2, na.rm = TRUE)) / abs(mean(y_test))
                  m7_f <- colMeans(abs(y_test - YH) / pmax(abs(y_test), 1e-10), na.rm = TRUE)
                }
                results[idx, ] <- cbind(lengths(subset_specs[idx]), fast_z$r2[idx], fast_z$adj_r2[idx], sig_f,
                                        ifelse(fast_z$sig_ok[idx] == 1L, fast_z$nsig[idx] / k_f, NA_real_),
                                        m6_f, m7_f, fast_z$vif[idx])
                p_eff_known <- rep(NA_real_, total_models)
                p_eff_known[idx] <- rowSums(!is.na(fast_z$b[idx, -1L, drop = FALSE]))
                progress1$set(value = 0.5, detail = "assembling")
                asm <- tryCatch(CERES_FAST$assemble(fast_z, idx, jcols, subset_specs, x_names, mm_nms,
                                                    1L, TRUE, as.integer(n_tr)), error = function(e) NULL)
                if (!is.null(asm)) {
                  predictors_list[idx] <- asm$pred; model_names[idx] <- asm$names
                  yhat_test[idx] <- asm$yhat;       model_list[idx]  <- asm$specs
                  rm(asm)
                } else {
                pn_f <- lapply(subset_specs[idx], function(sp) x_names[sp])
                predictors_list[idx] <- pn_f
                model_names[idx]     <- vapply(pn_f, paste, character(1), collapse = ", ")
                yhat_test[idx]       <- lapply(seq_along(idx), function(i) YH[, i])
                model_list[idx]      <- lapply(seq_along(idx), function(i) {
                  m     <- idx[i]
                  cols  <- c(1L, jcols[[m]] + 1L)
                  kk    <- length(cols) - 1L
                  dfr   <- n_tr - kk - 1L
                  coefs <- setNames(fast_z$b[m, seq_along(cols)], mm_nms[cols])
                  cu    <- fast_z$cov[[m]]
                  se    <- if (!is.null(cu)) fast_z$sigma[m] * sqrt(pmax(diag(cu), 0)) else rep(NA_real_, length(coefs))
                  tv    <- coefs / se
                  tbl   <- cbind(Estimate = coefs, "Std. Error" = se, "t value" = tv,
                                 "Pr(>|t|)" = 2 * pt(-abs(tv), df = max(dfr, 1L)))
                  colnames(tbl) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
                  rownames(tbl) <- names(coefs)
                  r2m   <- fast_z$r2[m]
                  f_val <- if (kk > 0L && dfr > 0L && is.finite(r2m) && r2m < 1) (r2m / kk) / ((1 - r2m) / dfr) else NA_real_
                  list(pred_names = pn_f[[i]], coefficients = coefs, weights = fast_z$w[, m],
                       converged = fast_z$conv[m] == 1L, iterations = fast_z$iter[m],
                       mupe_summary = list(coefficients  = tbl,
                                           r.squared     = r2m,
                                           adj.r.squared = fast_z$adj_r2[m],
                                           sigma         = fast_z$sigma[m],
                                           df            = c(kk + 1L, max(dfr, 0L), kk + 1L),
                                           fstatistic    = if (is.finite(f_val)) c(value = f_val, numdf = kk, dendf = dfr) else NULL,
                                           cov.unscaled  = if (!is.null(cu)) cu else matrix(NA_real_, length(coefs), length(coefs)),
                                           aliased       = setNames(rep(FALSE, length(cols)), mm_nms[cols])))
                })
                }
                rm(YH)
              }
              progress1$set(value = if (length(todo)) length(idx) / total_models else 1,
                            detail = paste0(round(length(idx) / total_models * 100), "%"))
            }

            for (cntr in todo) {
              spec       <- subset_specs[[cntr]]
              pred_names <- x_names[spec]
              xcols      <- c(1L, which(mm_asgn %in% spec))
              xcols_all  <- xcols
              g_U        <- gram_chol(gram, xcols[-1L] - 1L)
              if (is.null(g_U)) xcols <- fullrank_cols(X_trm, xcols)
              X_tr       <- X_trm[, xcols, drop = FALSE]
              p_eff      <- length(xcols) - 1L
              df_r       <- n_tr - p_eff - 1L

              if (mode == "loglin") {
                Y_unit <- exp(y_tr)
                coefs  <- zmpe_solve(X_tr, Y_unit, "loglin", full_rank = TRUE)
                if (is.null(coefs)) coefs <- .lm.fit(X_tr, y_tr)$coefficients
                conv   <- attr(coefs, "irls_converged") %||% FALSE; k <- attr(coefs, "irls_iterations") %||% NA_integer_
                coefs  <- setNames(as.vector(coefs), colnames(X_tr))
                yhat       <- as.vector(X_tr %*% coefs)
                yhat_unit  <- exp(yhat)
                r          <- y_tr - yhat
                r_unit     <- Y_unit - yhat_unit
                w_cur      <- 1 / yhat_unit^2
                sigma      <- sqrt(sum(r^2) / max(df_r, 1L))
                rss_w      <- sum((r_unit / yhat_unit)^2)
                y_bar_w    <- sum(w_cur * Y_unit) / sum(w_cur)
                tss_w      <- sum(w_cur * (Y_unit - y_bar_w)^2)
                r2         <- 1 - rss_w / tss_w
                adj_r2     <- 1 - (1 - r2) * (n_tr - 1L) / max(df_r, 1L)
                ll_val     <- 0.5 * (sum(log(w_cur)) - n_tr * (log(2 * pi) + 1 - log(n_tr) + log(rss_w)))
                cov_unscaled <- tryCatch(chol2inv(chol(crossprod(X_tr))), error = function(e) NULL)
                cov_sigma    <- sigma
              } else {
                coefs <- zmpe_solve(X_tr, y_tr, "linear", full_rank = TRUE)
                if (is.null(coefs)) coefs <- .lm.fit(X_tr, y_tr)$coefficients
                conv  <- attr(coefs, "irls_converged") %||% FALSE; k <- attr(coefs, "irls_iterations") %||% NA_integer_
                coefs <- setNames(as.vector(coefs), colnames(X_tr))
                yhat       <- as.vector(X_tr %*% coefs)
                yhat[abs(yhat) < 1e-6] <- 1e-6
                r          <- y_tr - yhat
                w_cur      <- 1 / yhat^2
                rss_w      <- sum((r / yhat)^2)
                y_bar_w    <- sum(w_cur * y_tr) / sum(w_cur)
                tss_w      <- sum(w_cur * (y_tr - y_bar_w)^2)
                r2         <- 1 - rss_w / tss_w
                adj_r2     <- 1 - (1 - r2) * (n_tr - 1L) / max(df_r, 1L)
                sigma      <- sqrt(rss_w / max(df_r, 1L))
                ll_val     <- 0.5 * (sum(log(w_cur)) - n_tr * (log(2 * pi) + 1 - log(n_tr) + log(rss_w)))
                cov_unscaled <- tryCatch(chol2inv(chol(crossprod(X_tr * sqrt(w_cur)))), error = function(e) NULL)
                cov_sigma    <- sigma
              }

              prop_sig <- if (!is.null(cov_unscaled) && all(is.finite(diag(cov_unscaled)))) {
                se_v <- cov_sigma * sqrt(pmax(diag(cov_unscaled), 0))
                { t_v <- (coefs / se_v)[-1L]; t_v[!is.finite(t_v)] <- 0
                  n_np <- length(xcols_all) - 1L
                  if (n_np > 0L) sum(abs(t_v) >= 2) / n_np else NA_real_ }
              } else NA_real_

              max_vif <- if (p_eff <= 1L) 1.0 else
                if (!is.null(g_U)) max(gram_vif_graded(gram, xcols[-1L] - 1L, chol2inv(g_U), mm_asgn[xcols[-1L]])) else
                tryCatch(max(term_vif_graded(R_full[xcols[-1L] - 1L, xcols[-1L] - 1L, drop = FALSE], mm_asgn[xcols[-1L]])),
                         error = function(e) NA_real_)

              if (predict_all) {
                yhat_test_vec <- as.numeric(X_mm[, xcols, drop = FALSE] %*% coefs)
              } else {
                yhat_test_vec <- as.numeric(X_mm[-train_index, xcols, drop = FALSE] %*% coefs)
              }

              df_mod <- p_eff + 2L
              aic_vec[cntr] <- -2 * ll_val + 2 * df_mod
              bic_vec[cntr] <- -2 * ll_val + log(n_tr) * df_mod

              results[cntr, 1:5] <- c(length(spec), r2, adj_r2, sigma, prop_sig)

              if (mode == "loglin") {
                geo_cv <- if (sigma > 2.1482832) 10 else sqrt(exp(sigma^2) - 1)
                mult_e <- if (sigma > 2.1899295) 10 else exp(sigma^2 / 2) - 1
                results[cntr, 6:7] <- c(geo_cv, mult_e)
              } else {
                rmse_val <- sqrt(mean((y_test - yhat_test_vec)^2, na.rm = TRUE))
                mape_val <- mean(abs(y_test - yhat_test_vec) / pmax(abs(y_test), 1e-10), na.rm = TRUE)
                results[cntr, 6:7] <- c(rmse_val / abs(mean(y_test)), mape_val)
              }

              results[cntr, 8] <- max_vif

              yhat_test[[cntr]]       <- yhat_test_vec
              predictors_list[[cntr]] <- pred_names
              model_names[cntr]       <- paste(pred_names, collapse = ", ")

              coef_se_raw <- if (!is.null(cov_unscaled)) cov_sigma * sqrt(pmax(diag(cov_unscaled), 0)) else rep(NA_real_, length(coefs))
              est_disp <- coefs
              se_disp  <- coef_se_raw
              t_vals   <- est_disp / se_disp
              p_vals   <- 2 * pt(-abs(t_vals), df = max(df_r, 1L))
              coef_tbl <- cbind(Estimate = est_disp, "Std. Error" = se_disp,
                                "t value" = t_vals, "Pr(>|t|)" = p_vals)
              colnames(coef_tbl) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
              rownames(coef_tbl) <- names(coefs)
              f_val <- if (p_eff > 0L && df_r > 0L && is.finite(r2) && r2 < 1) (r2 / p_eff) / ((1 - r2) / df_r) else NA_real_
              fstat <- if (is.finite(f_val)) c(value = f_val, numdf = p_eff, dendf = df_r) else NULL
              zmpe_summary_obj <- list(
                coefficients  = coef_tbl,
                r.squared     = r2,
                adj.r.squared = adj_r2,
                sigma         = sigma,
                df            = c(p_eff + 1L, max(df_r, 0L), p_eff + 1L),
                fstatistic    = fstat,
                cov.unscaled  = if (!is.null(cov_unscaled)) cov_unscaled else matrix(NA_real_, length(coefs), length(coefs)),
                aliased       = setNames(!(colnames(X_mm)[xcols_all] %in% colnames(X_tr)), colnames(X_mm)[xcols_all])
              )

              model_list[[cntr]]      <- list(
                pred_names    = pred_names,
                coefficients  = expand_coefs(setNames(coefs, colnames(X_tr)), colnames(X_mm)[xcols_all]),
                weights       = w_cur,
                converged     = conv, iterations = k,
                mupe_summary  = zmpe_summary_obj
              )

              if (cntr %% max(1L, total_models %/% 100L) == 0L || cntr == total_models)
                progress1$set(value = cntr / total_models,
                              detail = paste0(round(cntr / total_models * 100), "%"))
            }
            cntr    <- total_models
            results <- as.data.frame(results)

          } else {
              form   <- as.formula("y ~ .")
              for (spec in subset_specs) {
                cntr       <- cntr + 1L
                pred_names <- x_names[spec]
                x_part     <- x[, spec, drop = FALSE]
                fit_data   <- as.data.frame(cbind(y, x_part))
                predictors_list[[cntr]] <- pred_names
                m <- fit_irls(form, data = fit_data,
                              error_type = selected_error,
                              subset_index = train_index,
                              mode = mode)
                if (predict_all) {
                  yhat_test[[cntr]] <- predict(m, fit_data)
                } else {
                  yhat_test[[cntr]] <- predict(m, fit_data[-train_index, , drop = FALSE])
                }
                try({
                  environment(m$terms) <- baseenv()
                  if (!is.null(m$model)) environment(attr(m$model, "terms")) <- baseenv()
                }, silent = TRUE)
                model_list[[cntr]] <- m
                if (cntr %% max(1L, total_models %/% 100L) == 0L || cntr == total_models)
                  progress1$set(value = cntr / total_models,
                                detail = paste0(round(cntr / total_models * 100), "%"))
              }
          }
    }

    progress1$set(value = 1, message = "Scoring models", detail = "")
    ceres_mark("search: models fitted")

    {
    p <- ncol(x)
    p_eff_vec <- if (length(p_eff_known) == length(model_list)) p_eff_known else rep(NA_real_, length(model_list))
    p_miss    <- which(is.na(p_eff_vec))
    if (length(p_miss)) p_eff_vec[p_miss] <- vapply(model_list[p_miss], function(m) {
      cf <- if (inherits(m, "lm")) stats::coef(m) else m$coefficients
      sum(names(cf) != "(Intercept)" & !is.na(cf))
    }, numeric(1))

    if (!used_ols_fast_path) {
      X_mm_sc    <- model.matrix(~ ., data = as.data.frame(x))
      mm_asgn_sc <- attr(X_mm_sc, "assign")
      R_full_sc  <- cor(X_mm_sc[train_index, -1L, drop = FALSE])
      x_cols_sc  <- colnames(x)

      results     <- as.data.frame(matrix(NA_real_, nrow = cntr, ncol = 8))
      model_names <- character(cntr)

      aic_vec  <- numeric(cntr)
      bic_vec  <- numeric(cntr)
      ebic_vec <- numeric(cntr)

      for (i in seq_len(cntr)) {
        s <- summary(model_list[[i]])
        results[i, 1] <- length(predictors_list[[i]])
        results[i, 2] <- s$r.squared
        results[i, 3] <- s$adj.r.squared

        if (mode == "loglin") {
          se_model <- s$sigma
          results[i, 4] <- se_model
          { t_v <- s$coefficients[-1, 3]; t_v[!is.finite(t_v)] <- 0
            n_noni <- max(0L, length(coef(model_list[[i]])) - 1L)
            results[i, 5] <- if (n_noni > 0) sum(abs(t_v) >= 2) / n_noni else NA_real_ }
          if (se_model > 2.1482832) {
            results[i, 6] <- 10
          } else {
            results[i, 6] <- sqrt(exp(se_model^2) - 1)
          }
          if (se_model > 2.1899295) {
            results[i, 7] <- 10
          } else {
            results[i, 7] <- ((exp((se_model^2) / 2)) - 1)
          }
        } else {
          results[i, 4] <- s$sigma
          { t_v <- s$coefficients[-1, 3]; t_v[!is.finite(t_v)] <- 0
            n_noni <- max(0L, length(coef(model_list[[i]])) - 1L)
            results[i, 5] <- if (n_noni > 0) sum(abs(t_v) >= 2) / n_noni else NA_real_ }
          yhat_test_unscaled <- yhat_test[[i]]
          rmse_unit <- sqrt(mean((y_test - yhat_test_unscaled)^2))
          mape_unit <- mean(abs(y_test - yhat_test_unscaled) / pmax(abs(y_test), 1e-10))
          results[i, 6] <- rmse_unit / abs(mean(y_test))
          results[i, 7] <- mape_unit
        }

        num_vars_in_model <- results[i, 1]
        if (num_vars_in_model <= 1) {
          results[i, 8] <- 1.0
        } else {
          spec_i  <- match(predictors_list[[i]], x_cols_sc)
          xcols_i <- c(1L, which(mm_asgn_sc %in% spec_i))
          vc_i    <- xcols_i[-1L] - 1L
          vc_i    <- vc_i[is.finite(diag(R_full_sc)[vc_i])]
          max_vif <- tryCatch(
            max(term_vif_graded(R_full_sc[vc_i, vc_i, drop = FALSE], mm_asgn_sc[vc_i + 1L])),
            error = function(e) NA_real_
          )
          results[i, 8] <- if (is.nan(max_vif) || (is.infinite(max_vif) && max_vif < 0)) NA else max_vif
        }

        ll <- logLik(model_list[[i]])
        ll_val <- as.numeric(ll)
        df_val <- attr(ll, "df")
        n_val  <- attr(ll, "nobs")
        aic_vec[i] <- -2 * ll_val + 2 * df_val
        bic_vec[i] <- -2 * ll_val + log(n_val) * df_val
        k_val <- p_eff_vec[i]
        if (p > 1 && k_val > 0) {
          ebic_vec[i] <- bic_vec[i] + (2 * gamma_ebic * k_val * log(p))
        } else {
          ebic_vec[i] <- bic_vec[i]
        }

        model_names[i] <- paste(predictors_list[[i]], collapse = ", ")
        progress1$set(value = PROG_MAX * (0.6 + 0.1 * i / cntr), detail = paste(round(i / cntr * 100, 1), "%", sep = ""))
      }

    } else {
      k_val    <- p_eff_vec[seq_len(cntr)]
      ebic_vec <- bic_vec[seq_len(cntr)]
      ebic_on  <- !is.na(k_val) & p > 1 & k_val > 0
      ebic_vec[ebic_on] <- bic_vec[seq_len(cntr)][ebic_on] + 2 * gamma_ebic * k_val[ebic_on] * log(p)
    }

    if (cntr == 0) {
      return(NULL)
    }

    base_ic_scores <- ebic_vec
    ic_scale <- median(abs(base_ic_scores), na.rm = TRUE)

    n_obs <- length(train_index)
    hybrid_scaler <- (2 + log(n_obs)) / 2

    num_vars      <- p_eff_vec
    prop_sig      <- results[, 5]
    max_vif_clean <- results[, 8]
    max_vif_clean[is.na(max_vif_clean) | max_vif_clean <= 0] <- 1

    unit_penalty_sig <- num_vars * (1 - prop_sig) * hybrid_scaler
    unit_penalty_k   <- num_vars * hybrid_scaler
    unit_penalty_vif <- log(max_vif_clean) * hybrid_scaler

    scaler_k   <- ic_scale / (median(abs(unit_penalty_k), na.rm = TRUE) + 1e-9)
    scaler_sig <- scaler_k
    scaler_vif <- scaler_k

    penalty_sig <- w_sig * unit_penalty_sig * scaler_sig
    penalty_k   <- w_k   * unit_penalty_k   * scaler_k
    if (isTRUE(use_interact)) {
      penalty_vif <- rep(0, length(unit_penalty_vif))
    } else {
      penalty_vif <- w_vif * unit_penalty_vif * scaler_vif
    }

    wic_adjusted <- base_ic_scores + penalty_sig + penalty_k + penalty_vif

    min_wic <- min(wic_adjusted, na.rm = TRUE)
    max_wic <- max(wic_adjusted, na.rm = TRUE)

    scaled_scores <- if (min_wic == max_wic) {
      100
    } else {
      100 * (max_wic - wic_adjusted) / (max_wic - min_wic)
    }

    results[, 9] <- scaled_scores

    ceres_mark("search: models scored")
    cv_active <- holdout %in% OLS_HOLDOUT_CHOICES[5:7] && cntr > 0
    if (cv_active) {
      n_obs_cv <- length(y)
      K_cv <- switch(holdout,
        "Yes (5-Fold Cross-Validation)"  = min(5L,  n_obs_cv),
        "Yes (10-Fold Cross-Validation)" = min(10L, n_obs_cv),
        "Yes (Leave-One-Out CV)"         = n_obs_cv
      )
      if (K_cv >= n_obs_cv && selected_error %in% c("MUPE", "ZMPE") &&
          n_obs_cv * cntr > 50000) {
        K_cv <- min(10L, n_obs_cv)
        showNotification(
          paste0("Leave-One-Out CV with ", selected_error, " would require ~",
                 format(n_obs_cv * cntr, big.mark = ","),
                 " iterative refits — using 10-Fold CV instead (OLS supports true LOOCV via its closed form)."),
          type = "warning", duration = 10
        )
      }
      is_loo <- K_cv >= n_obs_cv
      fold_assignments <- if (is_loo) {
        lapply(seq_len(n_obs_cv), function(i) i)
      } else {
        withr::with_seed(123, ceres_ns("caret")$createFolds(y, k = K_cv, list = TRUE))
      }

      fold_of_row <- integer(n_obs_cv)
      for (k in seq_along(fold_assignments)) fold_of_row[fold_assignments[[k]]] <- k
      n_folds <- length(fold_assignments)

      cv_path <- if (selected_error %in% c("OLS", "MUPE", "ZMPE")) selected_error else "GENERIC"
      progress1$set(value = PROG_MAX * 0.75, message = "Cross-validating", detail = "")

      fit_mupe <- function(X_tr, y_tr, Y_unit_tr, init_coefs = NULL) {
        if (mode == "loglin") {
          coefs <- if (is.null(init_coefs)) {
            tryCatch(.lm.fit(X_tr, y_tr)$coefficients, error = function(e) NULL)
          } else init_coefs
          if (is.null(coefs)) return(NULL)
          coefs[!is.finite(coefs)] <- 0
          XtX_inv <- tryCatch(chol2inv(chol(crossprod(X_tr))), error = function(e) NULL)
          for (gn_iter in 1:1000) {
            yh_unit <- exp(as.vector(X_tr %*% coefs))
            s       <- Y_unit_tr / yh_unit - 1
            g       <- crossprod(X_tr, s)
            delta   <- if (!is.null(XtX_inv)) {
              as.vector(XtX_inv %*% g)
            } else {
              tryCatch(as.vector(solve(crossprod(X_tr), g)), error = function(e) NULL)
            }
            if (is.null(delta) || any(!is.finite(delta))) break
            coefs <- coefs + delta
            d_norm <- sqrt(sum(delta^2))
            if (!is.finite(d_norm) || d_norm < 1e-12) break
          }
        } else {
          coefs <- if (is.null(init_coefs)) {
            tryCatch(.lm.fit(X_tr, y_tr)$coefficients, error = function(e) NULL)
          } else init_coefs
          if (is.null(coefs)) return(NULL)
          coefs[!is.finite(coefs)] <- 0
          yhat_tr <- as.vector(X_tr %*% coefs)
          prev    <- coefs
          for (gn_iter in 1:1000) {
            yhat_tr[abs(yhat_tr) < 1e-6] <- 1e-6
            sw    <- 1 / abs(yhat_tr)
            coefs <- tryCatch(.lm.fit(X_tr * sw, y_tr * sw)$coefficients, error = function(e) NULL)
            if (is.null(coefs) || any(!is.finite(coefs))) return(NULL)
            yhat_tr <- as.vector(X_tr %*% coefs)
            if (irls_step_small(coefs, prev)) break
            prev <- coefs
          }
        }
        if (any(!is.finite(coefs))) NULL else coefs
      }

      fit_zmpe <- function(X_tr, y_tr, Y_unit_tr, init_coefs = NULL) {
        coefs <- tryCatch(zmpe_solve(X_tr, if (mode == "loglin") Y_unit_tr else y_tr, mode,
                                     start = init_coefs),
                          error = function(e) NULL)
        if (is.null(coefs) || any(!is.finite(coefs))) NULL else coefs
      }

      progress_cv <- shiny::Progress$new(session, min = 0, max = 1)
      progress_cv$set(message = paste0(K_cv, "-fold CV across models"), value = 0)
      on.exit(suppressWarnings(try(progress_cv$close(), silent = TRUE)), add = TRUE)

      x_df_cv  <- as.data.frame(x, check.names = FALSE)
      Y_unit_full <- if (cv_path %in% c("MUPE", "ZMPE") && mode == "loglin") exp(y) else NULL

      for (cv_i in seq_len(cntr)) {
        pred_names_i <- predictors_list[[cv_i]]
        if (length(pred_names_i) == 0L) next

        oof_preds <- rep(NA_real_, n_obs_cv)
        cv_model_ok <- TRUE

        if (cv_path != "GENERIC") {
          X_full <- NULL
          if (exists("X_mm", inherits = FALSE) && exists("mm_asgn", inherits = FALSE)) {
            term_idx <- match(pred_names_i, colnames(x))
            if (!anyNA(term_idx)) {
              X_full <- X_mm[, c(1L, which(mm_asgn %in% term_idx)), drop = FALSE]
            }
          }
          if (is.null(X_full)) {
            sub_df_cv <- x_df_cv[, pred_names_i, drop = FALSE]
            X_full <- tryCatch(model.matrix(~ ., data = sub_df_cv), error = function(e) NULL)
          }
          if (is.null(X_full) || !is.numeric(X_full)) {
            results[cv_i, c(2, 3, 4, 6, 7)] <- NA_real_
            next
          }
          X_full <- X_full[, fullrank_cols(X_full), drop = FALSE]
          p_cols <- ncol(X_full)

          if (cv_path == "OLS") {
            if (is_loo) {
              fit_full <- tryCatch(.lm.fit(X_full, y), error = function(e) NULL)
              if (!is.null(fit_full)) {
                QR <- tryCatch(qr(X_full), error = function(e) NULL)
                if (!is.null(QR) && QR$rank == p_cols) {
                  h <- pmin(rowSums(qr.Q(QR)^2), 1 - 1e-10)
                  r_full <- fit_full$residuals
                  oof_preds <- y - r_full / (1 - h)
                }
              }
            } else {
              for (k in seq_len(n_folds)) {
                test_idx <- fold_assignments[[k]]
                n_train  <- n_obs_cv - length(test_idx)
                if (n_train <= p_cols) { cv_model_ok <- FALSE; break }
                X_tr  <- X_full[-test_idx, , drop = FALSE]
                y_tr  <- y[-test_idx]
                if (tryCatch(qr(X_tr)$rank, error = function(e) 0L) < p_cols) { cv_model_ok <- FALSE; break }
                fit_k <- tryCatch(.lm.fit(X_tr, y_tr), error = function(e) NULL)
                if (is.null(fit_k) || any(!is.finite(fit_k$coefficients))) {
                  cv_model_ok <- FALSE; break
                }
                oof_preds[test_idx] <- as.numeric(X_full[test_idx, , drop = FALSE] %*% fit_k$coefficients)
              }
            }
          } else {
            fit_fn  <- if (cv_path == "MUPE") fit_mupe else fit_zmpe
            warm <- NULL
            m_i  <- model_list[[cv_i]]
            cand <- if (inherits(m_i, "lm")) {
              tryCatch(stats::coef(m_i), error = function(e) NULL)
            } else {
              m_i$coefficients
            }
            if (!is.null(cand) && !is.null(names(cand)) &&
                all(colnames(X_full) %in% names(cand)) && all(is.finite(cand[colnames(X_full)]))) {
              warm <- as.vector(cand[colnames(X_full)])
            }
            if (is.null(warm)) warm <- fit_fn(X_full, y, Y_unit_full, init_coefs = NULL)
            for (k in seq_len(n_folds)) {
              test_idx <- fold_assignments[[k]]
              n_train  <- n_obs_cv - length(test_idx)
              if (n_train <= p_cols) { cv_model_ok <- FALSE; break }
              X_tr      <- X_full[-test_idx, , drop = FALSE]
              y_tr      <- y[-test_idx]
              fold_rank <- tryCatch(qr(X_tr)$rank, error = function(e) NA_integer_)
              if (is.na(fold_rank) || fold_rank < p_cols) { cv_model_ok <- FALSE; break }
              Y_unit_tr <- if (!is.null(Y_unit_full)) Y_unit_full[-test_idx] else NULL
              coefs_k   <- fit_fn(X_tr, y_tr, Y_unit_tr, init_coefs = warm)
              if (is.null(coefs_k)) { cv_model_ok <- FALSE; break }
              oof_preds[test_idx] <- as.numeric(X_full[test_idx, , drop = FALSE] %*% coefs_k)
            }
          }
        } else {
          fit_data_i <- data.frame(y = y, x_df_cv[, pred_names_i, drop = FALSE], check.names = FALSE)
          form_i <- as.formula("y ~ .")
          p_cols <- tryCatch(ncol(model.matrix(form_i, data = fit_data_i)),
                             error = function(e) length(pred_names_i) + 1L)
          for (k in seq_len(n_folds)) {
            test_idx <- fold_assignments[[k]]
            if (length(test_idx) >= n_obs_cv) next
            n_train <- n_obs_cv - length(test_idx)
            if (n_train <= p_cols) { cv_model_ok <- FALSE; break }
            train_idx <- seq_len(n_obs_cv)[-test_idx]

            fold_model <- tryCatch(
              fit_irls(form_i, data = fit_data_i, error_type = selected_error,
                       subset_index = train_idx, mode = mode),
              error = function(e) NULL
            )
            if (is.null(fold_model) ||
                any(is.na(tryCatch(stats::coef(fold_model), error = function(e) NA)))) {
              cv_model_ok <- FALSE; break
            }

            oof_preds[test_idx] <- tryCatch(
              as.numeric(predict(fold_model, fit_data_i[test_idx, , drop = FALSE])),
              error = function(e) rep(NA_real_, length(test_idx))
            )
          }
        }

        valid <- is.finite(oof_preds)
        if (!cv_model_ok || sum(valid) < 3L) {
          results[cv_i, c(2, 3, 4, 6, 7)] <- NA_real_
          next
        }

        y_obs <- y[valid]
        y_hat <- oof_preds[valid]
        r_cv  <- y_obs - y_hat
        n_v   <- length(y_obs)
        p_eff <- p_cols - 1L
        df_r  <- max(n_v - p_eff - 1L, 1L)

        ss_res <- sum(r_cv^2)
        ss_tot <- sum((y_obs - mean(y_obs))^2)
        r2_cv  <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
        adj_r2_cv <- if (is.finite(r2_cv)) 1 - (1 - r2_cv) * (n_v - 1L) / df_r else NA_real_
        sigma_cv  <- sqrt(ss_res / df_r)

        if (mode == "loglin" && selected_error %in% c("MUPE", "ZMPE")) {
          yo_u <- exp(y_obs); yh_u <- exp(y_hat)
          w_cv <- 1 / pmax(yh_u^2, 1e-12)
          sw_cv  <- sum(w_cv)
          ybar_u <- sum(w_cv * yo_u) / sw_cv
          ssr_u  <- sum(w_cv * (yo_u - yh_u)^2)
          sst_u  <- sum(w_cv * (yo_u - ybar_u)^2)
          r2_cv     <- if (is.finite(sst_u) && sst_u > 0) 1 - ssr_u / sst_u else NA_real_
          adj_r2_cv <- if (is.finite(r2_cv)) 1 - (1 - r2_cv) * (n_v - 1L) / df_r else NA_real_
        }
        if (mode == "linear" && selected_error %in% c("MUPE", "ZMPE", "WLR")) {
          w_cv   <- 1 / pmax(y_hat^2, 1e-12)
          ybar_w <- sum(w_cv * y_obs) / sum(w_cv)
          ssr_w  <- sum(w_cv * r_cv^2)
          sst_w  <- sum(w_cv * (y_obs - ybar_w)^2)
          r2_cv     <- if (is.finite(sst_w) && sst_w > 0) 1 - ssr_w / sst_w else NA_real_
          adj_r2_cv <- if (is.finite(r2_cv)) 1 - (1 - r2_cv) * (n_v - 1L) / df_r else NA_real_
          sigma_cv  <- sqrt(ssr_w / df_r)
        }

        results[cv_i, 2] <- r2_cv
        results[cv_i, 3] <- adj_r2_cv
        results[cv_i, 4] <- sigma_cv

        if (mode == "loglin") {
          results[cv_i, 6] <- if (sigma_cv > 2.1482832) 10 else sqrt(exp(sigma_cv^2) - 1)
          results[cv_i, 7] <- if (sigma_cv > 2.1899295) 10 else exp(sigma_cv^2 / 2) - 1
        } else {
          rmse_cv <- sqrt(mean(r_cv^2))
          mape_cv <- mean(abs(r_cv) / pmax(abs(y_obs), 1e-10))
          y_bar   <- mean(y_obs)
          results[cv_i, 6] <- if (abs(y_bar) > 1e-12) rmse_cv / abs(y_bar) else NA_real_
          results[cv_i, 7] <- mape_cv
        }

        if (cv_i %% max(1L, cntr %/% 50L) == 0L || cv_i == cntr) {
          progress_cv$set(value = cv_i / cntr,
                          detail = paste0(round(cv_i / cntr * 100), "%"))
        }
      }
      suppressWarnings(try(progress_cv$close(), silent = TRUE))
    }

    if (mode == "loglin") {
      colnames(results) <- c("# Variables", "R-Squared", "Adj. R-Squared", "Standard Error",
                             "Coeff Significance (%)", "Geometric CV", "Multiplicative Error",
                             "Max VIF", "Weighted IC (%)")
    } else {
      colnames(results) <- c("# Variables", "R-Squared", "Adj. R-Squared", "Standard Error",
                             "Coeff Significance (%)", "CV (%)", "MAPE (%)",
                             "Max VIF", "Weighted IC (%)")
    }

    row.names(results) <- model_names
    names(model_list)  <- model_names

    progress1$set(value = PROG_MAX * 0.9, message = "Preparing results", detail = "")
    prog_handed_on <- isTRUE(tryCatch({
      session$onFlushed(function() suppressWarnings(try(progress1$close(), silent = TRUE)), once = TRUE); TRUE
    }, error = function(e) FALSE))

    ceres_mark("search: CV / tables done")
    list(
      ols_table      = results,
      model_list     = model_list,
      predictor_list = predictors_list,
      yhat_test      = yhat_test,
      y_test         = y_test,
      y_train        = if (used_ols_fast_path) y[train_index] else NULL,
      x_train        = if (used_ols_fast_path)
        as.data.frame(x[train_index, , drop = FALSE], optional = TRUE) else NULL,
      train_index    = train_index,
      log1p_cols     = if (mode == "loglin") log1p_cols else character(0)
    )
    }
  }

  observeEvent(input$linear_simple_train, {
    shinyjs::disable("linear_simple_train")
    shinyjs::html("linear_simple_train", "Running...")
    on.exit({ shinyjs::enable("linear_simple_train"); shinyjs::html("linear_simple_train", "Run Linear Model") })
    req(rv$active_dataset)
    sync_cer_generation_data()
    req(mean(input$ols_predictors %in% colnames(rv$active_dataset)) == 1)
    req(mean(input$ols_response   %in% colnames(rv$active_dataset)) == 1)

    rv$linear_simple_running <- TRUE
    on.exit(rv$linear_simple_running <- FALSE, add = TRUE)
    rv$ols_table_second <- NULL

    result <- run_cer_fitting(
      mode            = "linear",
      y_raw           = rv$active_dataset[, input$ols_response],
      x_raw           = as.data.frame(rv$active_dataset[, input$ols_predictors, drop = FALSE]),
      predictors      = input$ols_predictors,
      response_name   = input$ols_response,
      holdout         = input$ols_holdout,
      manual_test_set = rv$manual_test_set,
      error_type      = input$linear_error,
      use_interact    = input$linear_simple_interact,
      use_leaps       = input$linear_use_leaps,
      session         = session
    )
    if (is.null(result)) return()

    rv$ols_table_second         <- result$ols_table
    rv$linear_error_type        <- input$linear_error
    rv$model_list_linear_simple <- result$model_list
    rv$predictors_linear_simple <- result$predictor_list
    rv$yhat_test_linear_simple  <- result$yhat_test
    rv$y_test_second            <- result$y_test
    if (!is.null(result$y_train)) {
      rv$linear_y_train <- result$y_train
      rv$linear_x_train <- result$x_train
    }
  })
  
  measures_hdr <- new.env()
  ceres_install_measures <- function(prefix, table_fn, running_fn) {
    id  <- paste0(prefix, "_measures")
    ver <- reactiveVal(0L)
    hdr <- reactiveVal(ceres_measures_ui(NULL))
    assign(prefix, hdr, envir = measures_hdr)
    observeEvent(input[[paste0(id, "_state")]], {
      hdr(ceres_measures_ui(input[[paste0(id, "_state")]]$ui))
    }, ignoreInit = TRUE)
    st <- reactive({
      tbl <- table_fn()
      if (!is.data.frame(tbl) || nrow(tbl) <= ceres_ms_cap()) return(NULL)
      ceres_measures_static(tbl)
    })
    sel <- reactiveVal(NULL)
    observe({
      u <- hdr()
      k <- list(basis = u$basis, rules = u$rules, n = if (identical(u$basis, "all")) u$n else NULL)
      if (!identical(k, isolate(sel()))) sel(k)
    })
    observe({
      tbl <- table_fn()
      running <- isTRUE(running_fn())
      big <- is.data.frame(tbl) && nrow(tbl) > ceres_ms_cap()
      if (big) sel()
      v <- isolate(ver()) + 1L
      ver(v)
      if (running || !is.data.frame(tbl) || !nrow(tbl)) {
        session$sendCustomMessage("ceres-measures", list(id = id, v = v, clear = TRUE))
        return()
      }
      if (!isTRUE(session$userData$ceres_plotly_js)) {
        session$userData$ceres_plotly_js <- TRUE
        insertUI("head", "beforeEnd", immediate = TRUE,
                 ui = htmltools::attachDependencies(tags$span(), list(plotly:::plotlyMainBundle())))
      }
      m <- ceres_measures_message(tbl, isolate(hdr()), if (big) st() else NULL)$msg
      session$sendCustomMessage("ceres-measures", c(list(id = id, v = v), m))
    })
    reactive({
      tbl <- table_fn()
      req(tbl)
      tbl[ceres_measures_pick(tbl, ceres_measures_ui_capped(hdr(), nrow(tbl))), , drop = FALSE]
    })
  }

  filtered_linear_models_data_second <- ceres_install_measures(
    "linear", function() rv$ols_table_second, function() rv$linear_simple_running)

  filtered_model_list_second <- reactive({
    filtered_df_second <- filtered_linear_models_data_second()
    keep_names_second  <- rownames(filtered_df_second)
    
    all_names_second <- names(rv$model_list_linear_simple)
    
    keep_idxs_second <- match(keep_names_second, all_names_second)
    if (any(is.na(keep_idxs_second))) {
      missing_second <- keep_names_second[is.na(keep_idxs_second)]
      warning(">>> Could not match these model names back to rv$model_list_linear_simple: ", paste(missing_second, collapse = ", "))
    }
    keep_idxs_second <- keep_idxs_second[!is.na(keep_idxs_second)]
    
    rv$model_list_linear_simple[keep_idxs_second]
  })
  
    
  
  output$selected_linear_simple_models <- DT::renderDataTable({
    models_to_display_second <- filtered_linear_models_data_second()
    
    datatable(models_to_display_second, escape = FALSE, selection = 'single', 
              colnames = c("Variable Subsets" = 1), 
              options = list(dom = "tip", pageLength = 10, order = list(list(9, 'desc')))) %>%
      formatPercentage('R-Squared', interval = 3, digits = 3, mark = ",") %>%
      formatPercentage('Adj. R-Squared', interval = 3, digits = 3, mark = ",") %>%
      formatCurrency('Standard Error', currency = "", interval = 3, mark = ",", digits = 3) %>%
      formatPercentage('Coeff Significance (%)', interval = 3, digits = 3, mark = ",") %>%
      formatPercentage('CV (%)', interval = 3, digits = 3, mark = ",") %>% 
      formatPercentage('MAPE (%)', interval = 3, digits = 3, mark = ",") %>% 
      formatCurrency('Max VIF', currency = "", interval = 3, mark = ",", digits = 2) %>%
      formatCurrency('Weighted IC (%)', currency = "", interval = 3, mark = ",", digits = 3)
  })
  
  output$linear_simple_best_predictors <- renderPlotly({
    req(!rv$linear_simple_running)
    req(rv$predictors_linear_simple, rv$ols_table_second)
    
    models_selected_by_brush_second <- filtered_linear_models_data_second() 
    
    if (nrow(models_selected_by_brush_second) == 0) {
      return(
        plot_ly() %>% 
          layout(title = "Predictors in Subsets (No Models Selected)",
                 xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, mirror = TRUE),
                 yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, mirror = TRUE),
                 paper_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255),
                 plot_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255)) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    selected_model_names_second <- rownames(models_selected_by_brush_second)
    original_indices_second <- match(selected_model_names_second, rownames(rv$ols_table_second))
    original_indices_second <- original_indices_second[!is.na(original_indices_second)] 
    
    if (length(original_indices_second) == 0) {
      return(plot_ly() %>% layout(title = "Error: Could not map selected models."))
    }
    
    list_of_predictor_sets_for_selected_models_second <- rv$predictors_linear_simple[original_indices_second]
    all_predictors_in_selected_models_second <- unlist(list_of_predictor_sets_for_selected_models_second)
    
    if (is.null(all_predictors_in_selected_models_second) || length(all_predictors_in_selected_models_second) == 0) {
      return(
        plot_ly() %>% 
          layout(title = "No Predictors in Selected Models",
                 paper_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255),
                 plot_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255)) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    predictor_counts_second <- table(as.factor(all_predictors_in_selected_models_second))
    
    if (length(predictor_counts_second) == 0) {
      return(
        plot_ly() %>% 
          layout(title = "No Predictors to Display",
                 paper_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255),
                 plot_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255)) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    predictor_freq_df_second <- as.data.frame(predictor_counts_second, stringsAsFactors = FALSE)
    colnames(predictor_freq_df_second) <- c("Predictor", "Frequency")
    predictor_freq_df_second$Proportion <- predictor_freq_df_second$Frequency / nrow(models_selected_by_brush_second)
    predictor_freq_df_second <- predictor_freq_df_second[order(predictor_freq_df_second$Proportion, decreasing = TRUE), ]
    
    plot_ly(
      data = predictor_freq_df_second,
      x = ~Predictor, 
      y = ~Proportion,
      name = "Predictor Frequency (Linear)", 
      type = "bar", 
      marker = list(color = primary_color())
    ) %>%
      layout(
        title = "Proportion of Predictors", 
        yaxis = list(title = "", tickformat = ".0%"), 
        xaxis = list(title = "", 
                     categoryorder = "array",
                     categoryarray = ~Predictor), 
        paper_bgcolor = rgb(37,59,151,0.05, maxColorValue = 255),
        plot_bgcolor = rgb(37,59,151,0.05, maxColorValue = 255)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  observeEvent(input$loglin_train, {
    shinyjs::disable("loglin_train")
    shinyjs::html("loglin_train", "Running...")
    on.exit({ shinyjs::enable("loglin_train"); shinyjs::html("loglin_train", "Run Log-Linear Model") })
    req(rv$active_dataset)
    sync_cer_generation_data()
    req(mean(input$ols_predictors %in% colnames(rv$active_dataset)) == 1)
    req(mean(input$ols_response   %in% colnames(rv$active_dataset)) == 1)

    rv$loglin_running <- TRUE
    on.exit(rv$loglin_running <- FALSE, add = TRUE)
    rv$ols_table <- NULL
    ceres_mark("search: Run clicked (log-linear)")

    result <- run_cer_fitting(
      mode            = "loglin",
      y_raw           = rv$active_dataset[, input$ols_response],
      x_raw           = as.data.frame(rv$active_dataset[, input$ols_predictors, drop = FALSE]),
      predictors      = input$ols_predictors,
      response_name   = input$ols_response,
      holdout         = input$ols_holdout,
      manual_test_set = rv$manual_test_set,
      error_type      = input$log_linear_error,
      use_interact    = input$loglin_interact,
      use_leaps       = input$loglin_use_leaps,
      session         = session
    )
    if (is.null(result)) return()

    rv$ols_table          <- result$ols_table
    rv$loglin_error_type  <- input$log_linear_error
    rv$model_list_loglin  <- result$model_list
    rv$predictors_loglin  <- result$predictor_list
    rv$yhat_test_loglin   <- result$yhat_test
    rv$y_test             <- result$y_test
    rv$train_index        <- result$train_index
    rv$loglin_log1p_cols  <- result$log1p_cols %||% character(0)
    ceres_mark("search: results stored")
    if (!is.null(result$y_train)) {
      rv$loglin_y_train <- result$y_train
      rv$loglin_x_train <- result$x_train
    }
  })
  
  
  filtered_log_linear_models_data <- ceres_install_measures(
    "loglin", function() rv$ols_table, function() rv$loglin_running)

  filtered_model_list <- reactive({
    filtered_df <- filtered_log_linear_models_data()
    keep_names  <- rownames(filtered_df)
    
    all_names <- names(rv$model_list_loglin)
    keep_idxs <- match(keep_names, all_names)
    if (any(is.na(keep_idxs))) {
      missing <- keep_names[is.na(keep_idxs)]
      warning(">>> Could not match these model names back to rv$model_list_loglin: ", paste(missing, collapse = ", "))
    }
    keep_idxs <- keep_idxs[!is.na(keep_idxs)]
    
    rv$model_list_loglin[keep_idxs]
  })
  
  CERES_MAP_LOWER_BETTER <- c("Standard Error", "Geometric CV", "Multiplicative Error",
                              "Max VIF", "CV (%)", "MAPE (%)")
  CERES_MAP_PERCENT <- c("R-Squared", "Adj. R-Squared", "Coeff Significance (%)",
                         "Geometric CV", "Multiplicative Error", "CV (%)", "MAPE (%)")

  ceres_model_map <- function(df, metric, source) {
    req(is.data.frame(df), nrow(df) > 0, metric %in% names(df), "# Variables" %in% names(df))
    k <- df[["# Variables"]]
    y <- df[[metric]]
    ok <- is.finite(y) & is.finite(k)
    req(any(ok))
    lower <- metric %in% CERES_MAP_LOWER_BETTER
    pct   <- metric %in% CERES_MAP_PERCENT
    fmt   <- function(v) if (pct) sprintf("%.2f%%", 100 * v) else format(round(v, 3), big.mark = ",")
    pick  <- function(ix) ix[if (lower) which.min(y[ix]) else which.max(y[ix])]
    best_at <- unname(vapply(split(which(ok), k[ok]), pick, integer(1)))
    best_at <- best_at[order(k[best_at])]
    overall <- pick(best_at)
    others  <- setdiff(which(ok), best_at)
    jit     <- withr::with_seed(7, stats::runif(length(others), -0.14, 0.14))
    lab     <- function(ix) paste0("<b>", rownames(df)[ix], "</b><br>",
                                   k[ix], if (k[ix][1] == 1) " variable" else " variables",
                                   "<br>", metric, ": ", vapply(y[ix], fmt, character(1)))

    p <- plot_ly(source = source)
    if (length(others))
      p <- p %>% add_trace(x = k[others] + jit, y = y[others], type = "scatter", mode = "markers",
                           name = "Other models", customdata = others,
                           marker = list(color = "rgba(100,116,139,0.42)", size = 8,
                                         line = list(color = "rgba(255,255,255,0.9)", width = 1)),
                           text = lab(others), hoverinfo = "text")
    p %>%
      add_trace(x = k[best_at], y = y[best_at], type = "scatter", mode = "lines+markers",
                name = "Best at each size", customdata = best_at,
                line = list(color = "#2F6FE4", width = 2),
                marker = list(color = "#2F6FE4", size = 11, line = list(color = "#FFFFFF", width = 1.5)),
                text = lab(best_at), hoverinfo = "text") %>%
      add_trace(x = k[overall], y = y[overall], type = "scatter", mode = "markers",
                name = "Best overall", customdata = overall,
                marker = list(color = "rgba(0,0,0,0)", size = 24, line = list(color = "#7C5CFF", width = 2.5)),
                text = lab(overall), hoverinfo = "text") %>%
      layout(title = list(text = paste0("Model map · ", metric,
                                        if (lower) "  (lower is better)" else "  (higher is better)")),
             xaxis = list(title = "Number of variables", dtick = 1, zeroline = FALSE),
             yaxis = list(title = metric, zeroline = FALSE, tickformat = if (pct) ".0%" else NULL),
             legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom"),
             hovermode = "closest", margin = list(t = 84, l = 70, r = 24, b = 56)) %>%
      event_register("plotly_click") %>%
      config(displayModeBar = FALSE)
  }

  ceres_install_model_map <- function(prefix, table_id, filtered_fn, full_fn, running_fn) {
    src <- paste0(prefix, "_map_src")
    output[[paste0(prefix, "_model_map")]] <- renderPlotly({
      req(!isTRUE(running_fn()))
      ceres_model_map(filtered_fn(), input[[paste0(prefix, "_map_metric")]] %||% "Adj. R-Squared", src)
    })
    output[[paste0(prefix, "_map_note")]] <- renderUI({
      full <- full_fn(); shown <- filtered_fn()
      req(is.data.frame(full), is.data.frame(shown))
      if (nrow(shown) < nrow(full))
        tags$span(class = "c-map-note", paste0("Showing ", nrow(shown), " of ", nrow(full),
                                               " models (narrowed in All measures)"))
      else tags$span(class = "c-map-note", paste0(nrow(full), " models"))
    })
    observeEvent(event_data("plotly_click", source = src), {
      d <- event_data("plotly_click", source = src)
      idx <- suppressWarnings(as.integer(d$customdata[[1]]))
      req(length(idx) == 1, !is.na(idx))
      DT::selectRows(DT::dataTableProxy(table_id), idx)
    }, ignoreInit = TRUE)
  }

  CERES_CMP_MAX <- 4L
  CERES_CMP_COLS <- c("#2F6FE4", "#E0457B", "#16A34A", "#F08A24")

  cmp_label <- function(sn) paste0(sn$dep_var, " ~ ", if (length(sn$terms)) paste(sn$terms, collapse = " + ") else "1")
  cmp_kind  <- function(sn) paste0(if (identical(sn$mode, "loglin")) "Log-linear" else "Linear",
                                   if (!is.na(sn$error)) paste0(" · ", sn$error) else "")

  pin_model <- function(mode) {
    sn <- rv$cmp_current[[mode]]
    if (is.null(sn)) {
      showNotification("Open the model's Fit Statistics tab first, then pin it.", type = "warning")
      return(invisible())
    }
    pins <- rv$cmp_pins %||% list()
    if (any(vapply(pins, function(x) identical(x$key, sn$key), logical(1)))) {
      showNotification("This model is already pinned.", type = "message")
      return(invisible())
    }
    if (length(pins) >= CERES_CMP_MAX) {
      showNotification(paste0("You can compare up to ", CERES_CMP_MAX, " models. Unpin one first."), type = "warning")
      return(invisible())
    }
    rv$cmp_pins <- c(pins, list(sn))
    showNotification(paste0("Pinned for comparison (", length(rv$cmp_pins), " of ", CERES_CMP_MAX, ")."), type = "message")
  }
  observeEvent(input$loglin_pin_compare, pin_model("loglin"))
  observeEvent(input$linear_pin_compare, pin_model("linear"))

  output$cmp_tray <- renderUI({
    pins <- rv$cmp_pins %||% list()
    if (!length(pins)) return(NULL)
    tags$div(class = "c-cmp-tray",
      tags$span(class = "c-cmp-dots",
        lapply(seq_along(pins), function(i) tags$span(class = "c-cmp-dot", style = paste0("background:", CERES_CMP_COLS[i])))),
      actionButton("cmp_open", paste0("Compare ", length(pins), if (length(pins) == 1) " model" else " models"),
                   class = "btn-primary", icon = icon("table-columns")),
      actionLink("cmp_clear", "Clear", class = "c-cmp-clear"))
  })
  observeEvent(input$cmp_clear, { rv$cmp_pins <- list() })
  observeEvent(input$cmp_clear_all, { rv$cmp_pins <- list(); removeModal() })
  observeEvent(input$cmp_unpin, {
    i <- suppressWarnings(as.integer(input$cmp_unpin))
    pins <- rv$cmp_pins %||% list()
    req(!is.na(i), i >= 1, i <= length(pins))
    rv$cmp_pins <- pins[-i]
    if (!length(rv$cmp_pins)) removeModal()
  })

  output$cmp_view <- renderUI({
    pins <- rv$cmp_pins %||% list()
    req(length(pins) > 0)
    same_kind <- length(unique(vapply(pins, function(x) x$mode, character(1)))) == 1
    best_of <- function(vals, higher = TRUE) {
      ok <- is.finite(vals); if (!any(ok)) return(rep(FALSE, length(vals)))
      target <- if (higher) max(vals[ok]) else min(vals[ok])
      ok & abs(vals - target) < 1e-12
    }
    r2s  <- vapply(pins, function(x) x$r2 %||% NA_real_, numeric(1))
    ar2s <- vapply(pins, function(x) x$adj_r2 %||% NA_real_, numeric(1))
    ses  <- vapply(pins, function(x) x$sigma %||% NA_real_, numeric(1))
    b_r2 <- best_of(r2s); b_ar2 <- best_of(ar2s); b_se <- if (same_kind) best_of(ses, FALSE) else rep(FALSE, length(ses))
    f4 <- function(x, d = 4) if (is.null(x) || !is.finite(x)) "—" else formatC(x, digits = d, format = "f")
    fg <- function(x) if (is.null(x) || !is.finite(x)) "—" else formatC(x, digits = 4, format = "g")
    stat <- function(label, value, best) tags$div(class = paste("c-cmp-stat", if (isTRUE(best)) "is-best" else ""),
                                                 tags$span(label), tags$b(value))
    cols <- lapply(seq_along(pins), function(i) {
      sn <- pins[[i]]
      cf <- sn$coefs
      tags$div(class = "c-cmp-col", style = paste0("--c-cmp:", CERES_CMP_COLS[i], ";"),
        tags$div(class = "c-cmp-head",
          tags$span(class = "c-cmp-swatch"),
          tags$div(tags$div(class = "c-cmp-kind", cmp_kind(sn)),
                   tags$div(class = "c-cmp-name", cmp_label(sn))),
          tags$button(type = "button", class = "c-cmp-unpin", title = "Unpin",
                      onclick = sprintf("Shiny.setInputValue('cmp_unpin', %d, {priority: 'event'})", i),
                      HTML("&times;"))),
        tags$div(class = "c-cmp-eq",
          if (!is.null(sn$eq_latex) && nzchar(sn$eq_latex))
            tags$span(class = "katex-eq", `data-latex` = sn$eq_latex, tags$code(sn$eq_text))
          else tags$code(sn$eq_text)),
        tags$div(class = "c-cmp-stats",
          stat("Observations", sn$n, FALSE),
          stat("R²", f4(sn$r2), b_r2[i]),
          stat("Adj. R²", f4(sn$adj_r2), b_ar2[i]),
          stat(if (identical(sn$mode, "loglin")) "Residual SE (log)" else "Residual SE", fg(sn$sigma), b_se[i]),
          stat("F", fg(sn$f), FALSE)),
        tags$table(class = "c-cmp-coefs",
          tags$thead(tags$tr(tags$th("Term"), tags$th("Estimate"), tags$th("p"))),
          tags$tbody(lapply(seq_len(nrow(cf)), function(j) {
            p <- cf[j, 4]
            tags$tr(tags$td(rownames(cf)[j]), tags$td(fg(cf[j, 1])),
                    tags$td(class = if (is.finite(p) && p < 0.05) "is-sig" else "",
                            if (!is.finite(p)) "—" else if (p < 0.001) "< 0.001" else formatC(p, digits = 3, format = "f")))
          })))
      )
    })
    tagList(
      if (!same_kind) tags$p(class = "c-hint",
        "These models mix log-linear and linear fits: their R² and residual SE are measured on different scales, so compare them with care (no 'best' is marked for SE)."),
      tags$div(class = "c-cmp-grid", style = paste0("grid-template-columns: repeat(", length(pins), ", minmax(0, 1fr));"), cols),
      tags$div(class = "c-cmp-plotcard", plotlyOutput("cmp_plot", height = 420))
    )
  })

  output$cmp_plot <- renderPlotly({
    pins <- rv$cmp_pins %||% list()
    req(length(pins) > 0)
    p <- plot_ly()
    rng <- c()
    for (i in seq_along(pins)) {
      sn <- pins[[i]]
      if (is.null(sn$actual) || is.null(sn$fitted)) next
      rng <- range(c(rng, sn$actual, sn$fitted), na.rm = TRUE)
      p <- p %>% add_trace(x = sn$fitted, y = sn$actual, type = "scatter", mode = "markers",
                           name = paste0(i, ". ", cmp_label(sn)),
                           marker = list(color = CERES_CMP_COLS[i], size = 8, opacity = 0.75,
                                         line = list(color = "rgba(255,255,255,0.9)", width = 1)),
                           text = paste0("<b>", sn$rows, "</b><br>Predicted: ", format(round(sn$fitted), big.mark = ","),
                                         "<br>Actual: ", format(round(sn$actual), big.mark = ",")),
                           hoverinfo = "text")
    }
    req(length(rng) == 2, all(is.finite(rng)))
    p %>% add_trace(x = rng, y = rng, type = "scatter", mode = "lines", name = "y = x",
                    line = list(color = "#000000", dash = "dash", width = 1.5), hoverinfo = "skip") %>%
      layout(title = list(text = "Actuals vs predicted (unit space)"),
             xaxis = list(title = "Predicted", zeroline = FALSE),
             yaxis = list(title = "Actual", zeroline = FALSE),
             legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.2),
             margin = list(t = 58, l = 70, r = 20, b = 40)) %>%
      config(displayModeBar = FALSE)
  })

  observeEvent(input$cmp_open, {
    showModal(modalDialog(
      title = tags$div(class = "c-modal-head",
        tags$span(class = "c-modal-title-text", "Compare models"),
        tags$div(class = "c-modal-actions", actionLink("cmp_clear_all", "Clear all", class = "c-cmp-clear"))),
      uiOutput("cmp_view"),
      size = "xl", easyClose = TRUE, footer = NULL))
  })

  ceres_install_model_map("loglin", "selected_loglin_models",
                          function() filtered_log_linear_models_data(),
                          function() rv$ols_table,
                          function() rv$loglin_running)
  ceres_install_model_map("linear", "selected_linear_simple_models",
                          function() filtered_linear_models_data_second(),
                          function() rv$ols_table_second,
                          function() rv$linear_simple_running)

  output$selected_loglin_models <- DT::renderDataTable({
    models_to_display <- filtered_log_linear_models_data()
    
    datatable(models_to_display, escape = FALSE, selection = 'single', 
              colnames = c("Variable Subsets" = 1), 
              options = list(dom = "tip", pageLength = 10, order = list(list(9, 'desc')))) %>%
      formatPercentage('R-Squared', interval = 3, digits = 3, mark = ",") %>%
      formatPercentage('Adj. R-Squared', interval = 3, digits = 3, mark = ",") %>%
      formatCurrency('Standard Error', currency = "", interval = 3, mark = ",", digits = 3) %>%
      formatPercentage('Coeff Significance (%)', interval = 3, digits = 3, mark = ",") %>%
      formatPercentage('Geometric CV', interval = 3, digits = 3, mark = ",") %>%
      formatPercentage('Multiplicative Error', interval = 3, digits = 3, mark = ",") %>%
      formatCurrency('Max VIF',currency = "", interval = 3, mark = ",") %>%
      formatCurrency('Weighted IC (%)',currency = "", interval = 3, mark = ",", digits = 3)
  })
  
  output$loglin_best_predictors <- renderPlotly({
    req(!rv$loglin_running)
    req(rv$predictors_loglin, rv$ols_table)
    
    models_selected_by_brush <- filtered_log_linear_models_data() 
    
    if (nrow(models_selected_by_brush) == 0) {
      return(
        plot_ly() %>% 
          layout(title = "Predictors in Subsets (No Models Selected)",
                 xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, mirror = TRUE),
                 yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, mirror = TRUE),
                 paper_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255),
                 plot_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255)) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    selected_model_names <- rownames(models_selected_by_brush)
    
    original_indices <- match(selected_model_names, rownames(rv$ols_table))
    original_indices <- original_indices[!is.na(original_indices)]
    
    if (length(original_indices) == 0) {
      return(plot_ly() %>% layout(title = "Error: Could not map selected models to predictor lists."))
    }
    
    list_of_predictor_sets_for_selected_models <- rv$predictors_loglin[original_indices]
    
    all_predictors_in_selected_models <- unlist(list_of_predictor_sets_for_selected_models)
    
    if (is.null(all_predictors_in_selected_models) || length(all_predictors_in_selected_models) == 0) {
      return(
        plot_ly() %>% 
          layout(title = "No Predictors in Selected Models",
                 paper_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255),
                 plot_bgcolor=rgb(37,59,151,0.05,maxColorValue = 255)) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    predictor_counts <- table(as.factor(all_predictors_in_selected_models))
    
    if (length(predictor_counts) == 0) {
      return(
        plot_ly() %>% 
          layout(title = "No Predictors to Display",
                 paper_bgcolor=rgb(37,59,151,0.05, maxColorValue = 255),
                 plot_bgcolor=rgb(37,59,151,0.05,maxColorValue = 255)) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    predictor_freq_df <- as.data.frame(predictor_counts, stringsAsFactors = FALSE)
    colnames(predictor_freq_df) <- c("Predictor", "Frequency")
    
    predictor_freq_df$Proportion <- predictor_freq_df$Frequency / nrow(models_selected_by_brush)
    
    predictor_freq_df <- predictor_freq_df[order(predictor_freq_df$Proportion, decreasing = TRUE), ]
    
    plot_ly(
      data = predictor_freq_df,
      x = ~Predictor, 
      y = ~Proportion,
      name = "Predictor Frequency", 
      type = "bar", 
      marker = list(color = primary_color())
    ) %>%
      layout(
        title = "Proportion of Predictors", 
        yaxis = list(title = "", tickformat = ".0%"),
        xaxis = list(title = "", 
                     categoryorder = "array",
                     categoryarray = ~Predictor), 
        paper_bgcolor = rgb(37,59,151,0.05, maxColorValue = 255),
        plot_bgcolor = rgb(37,59,151,0.05, maxColorValue = 255)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  observeEvent(input$ols_holdout, {
    if (!identical(input$ols_holdout, "Yes (Manually Select Test Set)")) return()
    if (isTRUE(session_state$restoring) && length(rv$manual_test_set)) return()
    sync_cer_generation_data()
    key <- input$data_select_cer_generation
    has_data <- (!is.null(key) && nzchar(key) && !is.null(rv$dataset_library[[key]])) ||
      !is.null(rv$active_dataset)
    if (!isTRUE(has_data)) {
      showNotification(
        "Load a dataset and select it under \"Select Data to Analyze\" before choosing a manual test set.",
        type = "warning", duration = 8
      )
      return()
    }
    showModal(modalDialog(
      div(
        tags$br(),
        tags$p("Click rows to mark them as the holdout test set (multi-select). Then Save."),
        DT::DTOutput("select_test_set"),
        hr(),
        div(align = "center",
            actionButton("save_test_set", "Save Test Data"),
            actionButton("cancel_test_set", "Cancel", class = "btn-default",
                         style = "margin-left: 8px;"))
      ),
      title = "Select Test Set Rows",
      size = "l",
      easyClose = TRUE,
      footer = NULL,
      fade = TRUE
    ))
  })

  output$select_test_set <- DT::renderDataTable(server = FALSE, {
    key <- input$data_select_cer_generation
    df  <- if (!is.null(key) && nzchar(key)) rv$dataset_library[[key]] else NULL
    if (is.null(df)) df <- rv$active_dataset
    req(!is.null(df), nrow(df) > 0L, ncol(df) > 0L)

    num_cols <- colnames(df)[vapply(df, is.numeric, logical(1L))]
    dt <- DT::datatable(
      df,
      selection = "multiple",
      colnames   = c("Row Name" = 1),
      options    = list(pageLength = 10, scrollX = TRUE, processing = FALSE,
                        deferRender = FALSE)
    )
    dt <- ceres_format_numbers(dt, df, num_cols)
    dt
  })
  
  
  
  observeEvent(input$save_test_set, {
    sel <- input$select_test_set_rows_selected
    if (is.null(sel) || length(sel) == 0L) {
      showNotification("No rows selected — click one or more test rows, then Save.",
                       type = "warning", duration = 6)
      return()
    }
    rv$manual_test_set <- sel
    removeModal()
    showNotification(
      paste0(length(sel), " row(s) saved as the manual test set."),
      type = "message", duration = 5
    )
  })

  observeEvent(input$cancel_test_set, {
    removeModal()
  })
  
  loglin_result_window <- function(model_vars, nbins) modalDialog(
  
    title = tags$div(class = "c-modal-head",
      tags$span(class = "c-modal-title-text", "Model Summary (Log Linear)"),
      tags$div(class = "c-modal-actions",
        actionButton("loglin_pin_compare", "Pin to compare", icon = icon("thumbtack")),
        downloadButton("report_log_linear", "CER report", icon = icon("file-lines")),
        downloadButton("export_log_linear_excel", "Export Statistics", icon = icon("file-excel"), class = "btn-primary"))
    ),
    
    div(
      tabBox(width = 12,
             tabPanel("Fit Statistics",
                      tags$br(),
                      uiOutput("fit_summary")
             ),
             tabPanel("Actuals vs Predicted",
                      withSpinner(plotlyOutput("actuals_vs_predicted", width="95%", height = 450),
                                  color = getOption("spinner.color", default = primary_color()),
                                  type = 1)
             ),
             tabPanel("Residuals vs Predicted",
                      withSpinner(plotlyOutput("residual_vs_predicted", width="95%", height = 450),
                                  color = getOption("spinner.color", default = primary_color()),
                                  type = 1)
             ),
             tabPanel("Residuals vs Predictors",
                      withSpinner(plotlyOutput("residual_vs_predictors", width="95%", height = 450),
                                  color = getOption("spinner.color", default = primary_color()),
                                  type = 1),
                      selectInput("residual_predictor", "Select Predictor", choices = model_vars)
             ),
             tabPanel("Residual Normality",
                      withSpinner(plotlyOutput("residual_normality", width="95%", height = 450),
                                  color = getOption("spinner.color", default = primary_color()),
                                  type = 1),
                      sliderInput("residual_nbins", "Histogram Bins:", value = nbins, min = 5, max = 50)
             ),
             tabPanel("CER Tuning",
                      tags$script(HTML("
                        $(document).off('click.loglin_excl').on('click.loglin_excl', '.loglin-excl-check', function() {
                          var label = $(this).data('label');
                          var checked = $(this).is(':checked');
                          Shiny.setInputValue('loglin_influence_checkbox',
                            {label: label, checked: checked},
                            {priority: 'event'});
                        });
                        if (typeof Shiny !== 'undefined' && !Shiny._ceresDtExclHandlerRegistered) {
                          window._ceresExclState = window._ceresExclState || {};
                          window._ceresApplyExcl = function(cls) {
                            var set = window._ceresExclState[cls] || {};
                            document.querySelectorAll('.' + cls).forEach(function(cb) {
                              cb.checked = !!set[cb.getAttribute('data-label')];
                            });
                          };
                          Shiny.addCustomMessageHandler('ceres_dt_excl_update', function(msg) {
                            var labels = (msg && msg.labels) ? msg.labels : [];
                            var cls = (msg && msg.cls) ? msg.cls : '';
                            if (!cls) return;
                            var set = {};
                            for (var i = 0; i < labels.length; i++) set[String(labels[i])] = true;
                            window._ceresExclState[cls] = set;
                            window._ceresApplyExcl(cls);
                          });
                          // Re-apply on every DataTables redraw so rows that were on a
                          // different page get the correct checkbox state when paged in.
                          $(document).on('draw.dt', function() {
                            for (var c in window._ceresExclState) window._ceresApplyExcl(c);
                          });
                          Shiny._ceresDtExclHandlerRegistered = true;
                        }
                      ")),
                      tabsetPanel(
                        id = "loglin_outliers_view",
                        tabPanel("Exclusions",
                                 tags$br(),
                                 tags$div(style = "position:relative;",
                                          withSpinner(plotlyOutput("loglin_tuning_plot", width = "100%", height = 400),
                                                      color = getOption("spinner.color", default = primary_color()),
                                                      type = 1),
                                          tags$div(style = "position:absolute; top:120px; right:45px; z-index:10; max-width:130px;",
                                                   uiOutput("loglin_tuning_side_stats_tuning"))
                                 )
                        ),
                        tabPanel("Outliers",
                                 tags$br(),
                                 tags$div(style = "position:relative;",
                                          withSpinner(plotlyOutput("loglin_influence_plot", width = "100%", height = 400),
                                                      color = getOption("spinner.color", default = primary_color()),
                                                      type = 1),
                                          tags$div(style = "position:absolute; top:120px; right:45px; z-index:10; max-width:130px;",
                                                   uiOutput("loglin_tuning_side_stats_excl"))
                                 )
                        )
                      ),
                      fluidRow(
                        column(12,
                               tags$div(
                                 style = "font-size:0.7em; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#6c757d; margin-bottom:6px;",
                                 "Equation"
                               ),
                               uiOutput("loglin_tuning_equation")
                        )
                      ),
                      fluidRow(
                        column(12,
                               tags$details(
                                 style = "margin-top:12px; margin-bottom:4px;",
                                 tags$summary(
                                   style = "font-size:0.7em; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#6c757d; cursor:pointer; list-style:none; display:flex; align-items:center; gap:6px; margin-bottom:6px;",
                                   tags$span(style = "font-size:1.1em;", "▶"),
                                   "Live Prediction"
                                 ),
                                 tags$div(
                                   style = "border:1px solid #dee2e6; border-radius:6px; padding:12px 14px; background:#f8f9fa;",
                                   tags$div(
                                     style = "display:flex; align-items:baseline; gap:12px; flex-wrap:wrap; margin-bottom:10px;",
                                     tags$span(style = "font-size:0.78em; color:#6c757d;",
                                               "Updates live as points are excluded — no Refit needed."),
                                     actionLink("loglin_tuning_pred_sync", "copy from Predictions",
                                                style = "font-size:0.78em; font-weight:600;")
                                   ),
                                   uiOutput("loglin_tuning_pred_inputs"),
                                   uiOutput("loglin_tuning_prediction")
                                 )
                               )
                        )
                      ),
                      tags$br(),
                      fluidRow(
                        column(12, align = "left", class = "c-actions",
                               actionButton("loglin_tuning_push", "Refit",
                                            icon = icon("share-from-square"),
                                            class = "btn-primary",
                                            style = "display:block; width:95px; margin-bottom:6px;"),
                               actionButton("loglin_reset_exclusions", "Reset",
                                            icon = icon("undo"),
                                            style = "display:block; width:95px;")
                        )
                      ),
                      tags$br(),
                      tags$details(
                        style = "margin-bottom:16px;",
                        tags$summary(
                          style = "font-size:0.7em; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#6c757d; cursor:pointer; list-style:none; display:flex; align-items:center; gap:6px; margin-bottom:6px;",
                          tags$span(style = "font-size:1.1em;", "▶"),
                          "Data Table"
                        ),
                        DT::dataTableOutput("loglin_influence_table"),
                        tags$br(),
                        fluidRow(
                          column(12, align = "left",
                                 actionButton("loglin_save_exclusions", "Save Filtered Dataset",
                                              icon = icon("floppy-disk"))
                          )
                        )
                      )
             ),
             tabPanel("Predictions",
                      tags$br(),
                      DT::dataTableOutput("loglin_predict_table", fill = FALSE),
                      tags$div(style = "margin-top: 10px;"),
                      fluidRow(
                        column(width = 4,
                               uiOutput("loglin_predictions")
                        ),
                        column(width = 8,
                               tabsetPanel(
                                 tabPanel("Prediction Distribution",
                                          tags$br(),
                                          withSpinner(
                                            plotlyOutput("loglin_dist_plot", height = 350),
                                            color = getOption("spinner.color", default = primary_color()),
                                            type = 1
                                          ),
                                          tags$div(
                                            style = "margin-top:12px; padding:0 4px;",
                                            sliderInput(
                                              "loglin_pi_level",
                                              label = NULL,
                                              min   = 50, max = 99, value = 95, step = 1,
                                              ticks = FALSE, post = "%",
                                              width = "100%"
                                            ),
                                            tags$div(
                                              style = "position:relative; margin-top:2px; height:16px; font-size:10px; color:#aaa; user-select:none;",
                                              tags$span("50%", style = "position:absolute; left:0%;"),
                                              tags$span("68%", style = "position:absolute; left:36.7%; transform:translateX(-50%);"),
                                              tags$span("95%", style = "position:absolute; left:91.8%; transform:translateX(-50%);"),
                                              tags$span("99%", style = "position:absolute; right:0%;")
                                            )
                                          ),
                                          uiOutput("loglin_dist_stats")
                                 ),
                                 tabPanel("Local Attributions",
                                          tags$br(),
                                          withSpinner(plotlyOutput("loglin_breakdown", height = 400),
                                                      color = getOption("spinner.color", default = primary_color()),
                                                      type = 1)
                                 )
                               )
                        )
                      )
             )
      )
    ),
    size = c("xl"), easyClose = TRUE, footer = "Click Outside to Close.", fade = TRUE)

  observeEvent(rv$model_list_loglin, ceres_warm_result_window("loglin", loglin_result_window), ignoreInit = TRUE)

  observeEvent(input$selected_loglin_models_rows_selected, {
    model <- selected_log_linear_model()
    req(model)
    rv$loglin_selected_model <- model

    n_par <- length(coef(model))
    row_lbl <- rownames(model$model)
    if (is.null(row_lbl)) row_lbl <- as.character(seq_len(nrow(model$model)))
    row_lbl <- make.unique(row_lbl)
    rv$loglin_influence_data <- data.frame(
      row_label    = row_lbl,
      cooks_d      = as.numeric(cooks.distance(model)),
      leverage     = as.numeric(hatvalues(model)),
      std_residual = as.numeric(rstandard(model)),
      stringsAsFactors = FALSE
    )
    rv$loglin_influence_n_par <- n_par

    model_vars <- setdiff(names(model$model)[-1], "(weights)")

    p_col <- primary_color()
    
    txt_col <- "#FFFFFF !important"
    
    if (isTRUE(input$dark_mode)) {
      txt_col <- "#000000 !important"
    }
    
    ceres_show_result_window("loglin", loglin_result_window, model_vars, max(5L, min(50L, floor(sqrt(length(model$residuals))))))
  })

  register_cer_outputs <- function(
    mode,
    rv,
    input,
    output,
    session,
    selected_model_fn,
    filtered_models_fn
  ) {

    id_map <- if (mode == "loglin") list(
      fit_summary         = "fit_summary",
      predictions         = "loglin_predictions",
      predict_table       = "loglin_predict_table",
      dist_plot           = "loglin_dist_plot",
      dist_label          = "loglin_dist_label",
      dist_stats          = "loglin_dist_stats",
      breakdown           = "loglin_breakdown",
      actuals_predicted   = "actuals_vs_predicted",
      resid_predicted     = "residual_vs_predicted",
      resid_predictors    = "residual_vs_predictors",
      resid_normality     = "residual_normality",
      influence_plot      = "loglin_influence_plot",
      influence_table     = "loglin_influence_table",
      pred_prefix         = "loglin_pred_",
      pi_level            = "loglin_pi_level",
      resid_predictor     = "residual_predictor",
      resid_nbins         = "residual_nbins",
      influence_checkbox  = "loglin_influence_checkbox",
      save_exclusions     = "loglin_save_exclusions",
      reset_exclusions    = "loglin_reset_exclusions",
      row_selected        = "selected_loglin_models_rows_selected",
      excl_class          = "loglin-excl-check",
      selected_model_key  = "loglin_selected_model",
      influence_data_key  = "loglin_influence_data",
      influence_n_par_key = "loglin_influence_n_par",
      ui_exclusions_key   = "loglin_ui_exclusions",
      model_list_key      = "model_list_loglin",
      yhat_test_key       = "yhat_test_loglin",
      y_test_key          = "y_test",
      tuning_plot         = "loglin_tuning_plot",
      tuning_equation     = "loglin_tuning_equation",
      tuning_stats        = "loglin_tuning_stats",
      tuning_push         = "loglin_tuning_push",
      tuning_pushed_key   = "loglin_tuning_pushed_model",
      tuning_src             = "loglin_tuning_src",
      exclusions_src         = "loglin_exclusions_src",
      tuning_side_stats_a    = "loglin_tuning_side_stats_tuning",
      tuning_side_stats_b    = "loglin_tuning_side_stats_excl",
      tuning_prediction      = "loglin_tuning_prediction",
      tuning_pred_inputs     = "loglin_tuning_pred_inputs",
      tuning_pred_sync       = "loglin_tuning_pred_sync",
      tuning_pred_prefix     = "loglin_tpred_"
    ) else list(
      fit_summary         = "fit_summary_second",
      predictions         = "linear_predictions",
      predict_table       = "linear_predict_table",
      dist_plot           = "linear_dist_plot",
      dist_label          = "linear_dist_label",
      dist_stats          = "linear_dist_stats",
      breakdown           = "linear_breakdown",
      actuals_predicted   = "actuals_vs_predicted_second",
      resid_predicted     = "residual_vs_predicted_second",
      resid_predictors    = "residual_vs_predictors_second",
      resid_normality     = "residual_normality_second",
      influence_plot      = "linear_influence_plot",
      influence_table     = "linear_influence_table",
      pred_prefix         = "linear_pred_",
      pi_level            = "linear_pi_level",
      resid_predictor     = "residual_predictor_second",
      resid_nbins         = "residual_nbins_second",
      influence_checkbox  = "linear_influence_checkbox",
      save_exclusions     = "linear_save_exclusions",
      reset_exclusions    = "linear_reset_exclusions",
      row_selected        = "selected_linear_simple_models_rows_selected",
      excl_class          = "linear-excl-check",
      selected_model_key  = "linear_selected_model",
      influence_data_key  = "linear_influence_data",
      influence_n_par_key = "linear_influence_n_par",
      ui_exclusions_key   = "linear_ui_exclusions",
      model_list_key      = "model_list_linear_simple",
      yhat_test_key       = "yhat_test_linear_simple",
      y_test_key          = "y_test_second",
      tuning_plot         = "linear_tuning_plot",
      tuning_equation     = "linear_tuning_equation",
      tuning_stats        = "linear_tuning_stats",
      tuning_push         = "linear_tuning_push",
      tuning_pushed_key   = "linear_tuning_pushed_model",
      tuning_src             = "linear_tuning_src",
      exclusions_src         = "linear_exclusions_src",
      tuning_side_stats_a    = "linear_tuning_side_stats_tuning",
      tuning_side_stats_b    = "linear_tuning_side_stats_excl",
      tuning_prediction      = "linear_tuning_prediction",
      tuning_pred_inputs     = "linear_tuning_pred_inputs",
      tuning_pred_sync       = "linear_tuning_pred_sync",
      tuning_pred_prefix     = "linear_tpred_"
    )

    pred_input_ids <- function(vars, prefix = id_map$pred_prefix) {
      paste0(prefix,
             make.unique(gsub("[^A-Za-z0-9_.]", "_", vars), sep = "_"))
    }

    build_new_data <- function(model, pred_vars, prefix = id_map$pred_prefix) {
      cer_key    <- input$data_select_cer_generation
      pre_logged <- if (!is.null(cer_key) && nzchar(cer_key))
        rv$log_transformed_by_key[[cer_key]] %||% character(0) else character(0)
      log1p_cols <- attr(model, "log1p_cols") %||% character(0)
      main_vars  <- pred_vars[!is_interaction_col(pred_vars, source = cer_source_cols())]
      int_vars   <- setdiff(pred_vars, main_vars)
      ids        <- pred_input_ids(main_vars, prefix)
      input_vals <- setNames(
        lapply(ids, function(id) input[[id]]),
        main_vars
      )
      req(!any(vapply(input_vals, is.null, logical(1L))))
      nd <- data.frame(
        lapply(main_vars, function(var) {
          val <- input_vals[[var]]
          if (is.numeric(model$model[[var]])) {
            if (mode == "loglin" &&
                !all(model$model[[var]] %in% c(0, 1))) {
              v <- max(as.numeric(val), 1e-9)
              if (var %in% log1p_cols) log1p(v)
              else if (var %in% pre_logged) log(max(log(v), 1e-9))
              else log(v)
            } else {
              as.numeric(val)
            }
          } else {
            lvls <- model$xlevels[[var]]
            if (is.null(lvls)) lvls <- levels(model$model[[var]])
            if (is.null(lvls)) lvls <- sort(unique(as.character(model$model[[var]])))
            factor(val, levels = lvls)
          }
        }),
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )
      colnames(nd) <- main_vars
      for (iv in int_vars) {
        p <- strsplit(iv, ":", fixed = TRUE)[[1]]
        req(all(p %in% names(nd)))
        nd[[iv]] <- Reduce(`*`, nd[p])
      }
      nd <- nd[, pred_vars, drop = FALSE]
      list(nd = nd, input_vals = input_vals, pre_logged = pre_logged)
    }

    output[[id_map$fit_summary]] <- renderUI({
      sel <- input[[id_map$row_selected]]
      req(sel)
      model <- selected_model_fn()
      req(model)

      coefs    <- coef(model)
      names(coefs) <- unbt(names(coefs))
      intercept <- coefs["(Intercept)"]
      dep_var  <- input$ols_response
      mod_sum  <- summary(model)
      rownames(mod_sum$coefficients) <- unbt(rownames(mod_sum$coefficients))
      coef_df  <- as.data.frame(mod_sum$coefficients)
      colnames(coef_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      r2       <- mod_sum$r.squared
      adj_r2   <- mod_sum$adj.r.squared
      sigma    <- mod_sum$sigma
      df_res   <- mod_sum$df[2]
      n_obs    <- nrow(model$model)
      fstat    <- mod_sum$fstatistic
      f_p      <- if (!is.null(fstat)) pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      resid_q  <- quantile(residuals(model), probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

      fmt_eq <- cer_fmt_eq
      eq_parts <- cer_equation(coef(model), model$model, model$xlevels, dep_var %||% "y", mode,
                               attr(model, "log1p_cols") %||% character(0), fmt_eq,
                               cer_source_cols())
      eq_str   <- eq_parts$text
      eq_label <- if (mode == "loglin") "Log-Linear CER Equation" else "Linear CER Equation"
      cmp <- isolate(rv$cmp_current) %||% list()
      cmp[[mode]] <- ceres_cmp_snapshot(model, mode, dep_var %||% "y", eq_parts,
                                        n_obs, r2, adj_r2, sigma, fstat, coef_df)
      rv$cmp_current <- cmp
      conv_note <- if (isFALSE(attr(model, "irls_converged")))
        paste0("Did not converge (", attr(model, "irls_iterations"), " iterations) - the coefficients are the last iterate.")

      fmt_num <- cer_fmt_num
      fmt_p   <- cer_fmt_p
      is_dark    <- isTRUE(input$dark_mode)
      bg_card    <- if (is_dark) "#2e2e2e" else "#f8f9fa"
      bg_grad1   <- if (is_dark) "#272727" else "#eef2fa"
      bg_grad2   <- if (is_dark) "#2e2e2e" else "#f8f9fa"
      col_text   <- if (is_dark) "#f1f5f9" else "#212529"
      col_muted  <- if (is_dark) "#9ca3af" else "#6c757d"
      col_border <- if (is_dark) "#4a4a4a" else "#dee2e6"
      col_hdr    <- if (is_dark) "#e2e8f0" else "#333"
      bg_row_alt <- if (is_dark) "#3a3a3a" else "#f8f9fa"
      bg_row_std <- if (is_dark) "#2e2e2e" else "#ffffff"
      sig_stars <- cer_sig_stars
      sig_color <- function(p) {
        if (is.na(p)) return("#bbb")
        if (p < 0.001) "#1a7a3a" else if (p < 0.01) "#2a8a4e" else if (p < 0.05) "#b07000" else if (p < 0.1) "#888" else "#bbb"
      }

      show_unit <- identical(mode, "loglin")
      coef_rows <- lapply(seq_len(nrow(coef_df)), function(i) {
        vn   <- rownames(coef_df)[i]
        est  <- coef_df[i, "Estimate"];   se   <- coef_df[i, "Std. Error"]
        tval <- coef_df[i, "t value"];    pval <- coef_df[i, "Pr(>|t|)"]
        bg <- if (i %% 2 == 0) bg_row_alt else bg_row_std
        tags$tr(
          style = paste0("background:", bg, ";"),
          tags$td(style = paste0("padding:6px 10px; font-weight:600; color:", col_text, ";"),                             vn),
          tags$td(style = paste0("padding:6px 10px; text-align:right; font-family:monospace; color:", col_text, ";"),     fmt_num(est)),
          if (show_unit) tags$td(style = paste0("padding:6px 10px; text-align:right; font-family:monospace; color:", col_text, ";"),
                                 { u <- unname(eq_parts$values[vn]); if (is.na(u)) "—" else fmt_eq(u) }),
          tags$td(style = paste0("padding:6px 10px; text-align:right; font-family:monospace; color:", col_text, ";"),     fmt_num(se)),
          tags$td(style = paste0("padding:6px 10px; text-align:right; font-family:monospace; color:", col_text, ";"),     fmt_num(tval)),
          tags$td(style = paste0("padding:6px 10px; text-align:right; font-family:monospace; color:", col_text, ";"),     fmt_p(pval)),
          tags$td(style = paste0("padding:6px 10px; text-align:center; font-weight:bold; color:", sig_color(pval), ";"), sig_stars(pval))
        )
      })

      stat_card <- function(label, value, sub = NULL) {
        tags$div(class = "c-stat",
          style = paste0("background:", bg_card, "; border:1px solid ", col_border, "; border-radius:6px; padding:10px 16px; min-width:90px; text-align:center; flex:1;"),
          tags$div(class = "c-stat-l", style = paste0("font-size:0.72em; color:", col_muted, "; text-transform:uppercase; letter-spacing:0.05em; margin-bottom:2px;"), label),
          tags$div(class = "c-stat-v", style = paste0("font-size:1.2em; font-weight:700; font-family:monospace; color:", col_text, ";"), value),
          if (!is.null(sub)) tags$div(style = paste0("font-size:0.72em; color:", col_muted, "; margin-top:2px;"), sub)
        )
      }
      f_label <- if (!is.null(fstat)) paste0("F(", round(fstat[2], 0), ", ", round(fstat[3], 0), " dim.)") else "F-stat"
      f_val   <- if (!is.null(fstat)) formatC(fstat[1], digits = 3, format = "g") else "\u2014"

      diag_row <- function(label, value_str, pass, note) {
        bg     <- if (isTRUE(pass))  (if (is_dark) "#1a2e1e" else "#f0faf4") else
                  if (isFALSE(pass)) (if (is_dark) "#2e1a1a" else "#fdf0f0") else
                                     (if (is_dark) "#2e2e2e" else "#fafafa")
        ic_col <- if (isTRUE(pass)) "#2d8a4e" else if (isFALSE(pass)) "#c0392b" else (if (is_dark) "#666" else "#aaa")
        ic_txt <- if (isTRUE(pass)) "\u2714" else if (isFALSE(pass)) "\u2718" else "\u2014"
        note_col <- if (is_dark) "#9ca3af" else "#555"
        tags$tr(
          style = paste0("background:", bg, ";"),
          tags$td(style = paste0("padding:6px 10px; font-weight:600; color:", col_text, ";"),                     label),
          tags$td(style = paste0("padding:6px 10px; font-family:monospace; font-size:0.9em; color:", col_text, ";"), value_str),
          tags$td(style = paste0("padding:6px 10px; text-align:center; font-weight:bold; color:", ic_col, ";"),  ic_txt),
          tags$td(style = paste0("padding:6px 10px; color:", note_col, "; font-size:0.88em;"),                    note)
        )
      }
      diag_df   <- cer_lm_diagnostics(model)
      diag_rows <- lapply(seq_len(nrow(diag_df)), function(i)
        diag_row(diag_df$Test[i], diag_df$Result[i], diag_df$pass[i], diag_df$Note[i]))

      eq_latex <- eq_parts$latex

      p_col_val <- primary_color()
      hdr_style <- paste0(
        "background:linear-gradient(135deg,", bg_grad1, " 0%,", bg_grad2, " 100%);",
        "color:", col_hdr, "; border-bottom:2px solid ", p_col_val, ";"
      )
      sect_label <- function(txt) tags$div(class = "c-sect",
        style = paste0("font-size:0.7em; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:", col_muted, "; margin-bottom:6px;"),
        txt
      )

      tags$div(
        style = "padding:8px 2px;",

        tags$div(class = "c-eq-card",
          style = paste0(
            paste0("background:linear-gradient(135deg,", bg_grad1, " 0%,", bg_grad2, " 100%);"),
            "border-radius:10px; padding:14px 18px; margin-bottom:16px;",
            "border-left:4px solid ", p_col_val, "; position:relative;"
          ),
          tags$div(
            style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:6px;",
            tags$div(style = paste0("font-size:0.68em; font-weight:700; letter-spacing:0.1em; text-transform:uppercase; color:", col_muted, ";"), eq_label),
            tags$button(
              type = "button",
              class = "cer-copy-btn",
              title = "Copy equation as plain text",
              `data-eq` = eq_str,
              onclick = "cerCopyEquation(this);",
              HTML('<svg xmlns="http://www.w3.org/2000/svg" width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2"/><rect x="8" y="2" width="8" height="4" rx="1" ry="1"/></svg>')
            )
          ),
          if (!is.null(eq_latex) && nzchar(eq_latex)) {
            tags$span(
              class = "katex-eq",
              `data-latex` = eq_latex,
              style = paste0("font-size:1.05em; display:block; word-break:break-word; color:", col_text, ";"),
              tags$code(style = paste0("font-family:monospace; color:", col_text, "; background:transparent; font-size:1em;"), eq_str)
            )
          } else {
            tags$div(style = paste0("font-family:monospace; font-size:1.05em; color:", col_text, "; word-break:break-word;"), eq_str)
          }
        ),

        if (!is.null(conv_note))
          tags$div(style = "margin-bottom:12px; padding:8px 12px; border-radius:6px; border:1px solid #c0392b; color:#c0392b; font-weight:600;",
                   conv_note),

        tags$div(
          style = "margin-bottom:16px;",
          sect_label("Model Fit"),
          tags$div(class = "c-stat-row",
            style = "display:flex; gap:8px; flex-wrap:wrap;",
            stat_card("Observations", as.character(n_obs)),
            stat_card("R\u00b2",      formatC(r2,     digits = 4, format = "f")),
            stat_card("Adj. R\u00b2", formatC(adj_r2, digits = 4, format = "f")),
            stat_card("Residual SE",  formatC(sigma,  digits = 4, format = "g")),
            stat_card(f_label,        f_val)
          )
        ),

        tags$div(
          style = "margin-bottom:16px;",
          sect_label("Coefficients"),
          tags$div(class = "c-coef",
            style = paste0("border:1px solid ", col_border, "; border-radius:6px; overflow:hidden;"),
            tags$table(
              style = "width:100%; border-collapse:collapse; font-size:0.9em;",
              tags$thead(tags$tr(
                style = hdr_style,
                tags$th(style = "padding:8px 10px; text-align:left;",   "Variable"),
                tags$th(style = "padding:8px 10px; text-align:right;",  "Coefficient Estimate"),
                if (show_unit) tags$th(style = "padding:8px 10px; text-align:right;", "Unit-Space Value"),
                tags$th(style = "padding:8px 10px; text-align:right;",  "Std. Error"),
                tags$th(style = "padding:8px 10px; text-align:right;",  "T-value"),
                tags$th(style = "padding:8px 10px; text-align:right;",  "P-value"),
                tags$th(style = "padding:8px 10px; text-align:center;", "Sig.")
              )),
              tags$tbody(coef_rows)
            )
          ),
          tags$div(
            style = paste0("font-size:0.8em; font-family:monospace; color:", col_muted, "; margin-top:6px; letter-spacing:0.03em; line-height:1.6;"),
            "Signif. codes:\u00a0 0\u00a0\u2018***\u2019\u00a0 0.001\u00a0\u2018**\u2019\u00a0 0.01\u00a0\u2018*\u2019\u00a0 0.05\u00a0\u2018.\u2019\u00a0 0.1\u00a0\u2018\u00a0\u2019\u00a0 1"
          )
        ),

        tags$details(
          style = "margin-bottom:16px;",
          tags$summary(
            style = paste0("font-size:0.7em; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:", col_muted, "; cursor:pointer; list-style:none; display:flex; align-items:center; gap:6px; margin-bottom:6px;"),
            tags$span(style = "font-size:1.1em;", "\u25b6"),
            "Residuals"
          ),
          tags$div(
            style = paste0("border:1px solid ", col_border, "; border-radius:6px; overflow:hidden;"),
            tags$table(
              style = "width:100%; border-collapse:collapse; font-size:0.9em;",
              tags$thead(tags$tr(
                style = hdr_style,
                tags$th(style = "padding:8px 10px; text-align:center; width:20%;", "Min"),
                tags$th(style = "padding:8px 10px; text-align:center; width:20%;", "1Q"),
                tags$th(style = "padding:8px 10px; text-align:center; width:20%;", "Median"),
                tags$th(style = "padding:8px 10px; text-align:center; width:20%;", "3Q"),
                tags$th(style = "padding:8px 10px; text-align:center; width:20%;", "Max")
              )),
              tags$tbody(tags$tr(
                style = paste0("background:", bg_row_std, ";"),
                tags$td(style = paste0("padding:8px 10px; text-align:center; font-family:monospace; color:", col_text, ";"), fmt_num(resid_q[1])),
                tags$td(style = paste0("padding:8px 10px; text-align:center; font-family:monospace; color:", col_text, ";"), fmt_num(resid_q[2])),
                tags$td(style = paste0("padding:8px 10px; text-align:center; font-family:monospace; font-weight:600; color:", col_text, ";"), fmt_num(resid_q[3])),
                tags$td(style = paste0("padding:8px 10px; text-align:center; font-family:monospace; color:", col_text, ";"), fmt_num(resid_q[4])),
                tags$td(style = paste0("padding:8px 10px; text-align:center; font-family:monospace; color:", col_text, ";"), fmt_num(resid_q[5]))
              ))
            )
          )
        ),

        tags$details(
          tags$summary(
            style = paste0("font-size:0.7em; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:", col_muted, "; cursor:pointer; list-style:none; display:flex; align-items:center; gap:6px; margin-bottom:6px;"),
            tags$span(style = "font-size:1.1em;", "\u25b6"),
            "Regression Diagnostics"
          ),
          tags$div(
            style = paste0("border:1px solid ", col_border, "; border-radius:6px; overflow:hidden;"),
            tags$table(
              style = "width:100%; border-collapse:collapse; font-size:0.9em;",
              tags$thead(tags$tr(
                style = hdr_style,
                tags$th(style = "padding:8px 10px; text-align:left;",   "Test"),
                tags$th(style = "padding:8px 10px; text-align:left;",   "Result"),
                tags$th(style = "padding:8px 10px; text-align:center;", "Status"),
                tags$th(style = "padding:8px 10px; text-align:left;",   "Note")
              )),
              tags$tbody(diag_rows)
            )
          )
        )
      )
    })
    outputOptions(output, id_map$fit_summary, suspendWhenHidden = FALSE)

    output[[id_map$predictions]] <- renderUI({
      model     <- selected_model_fn()
      req(model)
      mm         <- model$model
      pred_vars  <- setdiff(names(mm)[-1], "(weights)")
      main_vars  <- pred_vars[!is_interaction_col(pred_vars, source = cer_source_cols())]
      ids        <- pred_input_ids(main_vars)
      cer_key    <- input$data_select_cer_generation
      pre_logged <- if (!is.null(cer_key) && nzchar(cer_key))
        rv$log_transformed_by_key[[cer_key]] %||% character(0) else character(0)
      log1p_cols <- attr(model, "log1p_cols") %||% character(0)
      out <- tagList()
      for (i in seq_along(main_vars)) {
        var <- main_vars[i]
        col <- mm[[var]]
        if (is.numeric(col)) {
          med_col <- median(col, na.rm = TRUE)
          default_val <- if (mode == "loglin" && !all(col %in% c(0, 1))) {
            if (var %in% log1p_cols)      expm1(med_col)
            else if (var %in% pre_logged) exp(exp(med_col))
            else                          exp(med_col)
          } else med_col
          if (is.finite(default_val) && default_val >= 1) default_val <- round(default_val, 0)
          out[[i]] <- numericInput(ids[i], var, value = default_val)
        } else {
          lvls <- model$xlevels[[var]]
          if (is.null(lvls)) lvls <- levels(col)
          if (is.null(lvls)) lvls <- sort(unique(as.character(col)))
          most_common <- names(sort(table(col), decreasing = TRUE))[1]
          out[[i]] <- selectInput(ids[i], var,
                                  choices = lvls, selected = most_common)
        }
      }
      out
    })

    output[[id_map$predict_table]] <- renderDT({
      model     <- selected_model_fn()
      req(model)
      pred_vars <- setdiff(names(model$model)[-1], "(weights)")
      parts     <- build_new_data(model, pred_vars)
      new_data  <- parts$nd

      p90 <- tryCatch(lm_pred_interval(model, new_data, 0.90, mode), error = function(e) NULL)
      p95 <- tryCatch(lm_pred_interval(model, new_data, 0.95, mode), error = function(e) NULL)
      req(!is.null(p90), !is.null(p95))

      if (mode == "loglin") {
        vals    <- c(exp(p90[1,"fit"]), exp(p90[1,"lwr"]), exp(p90[1,"upr"]),
                     exp(p95[1,"lwr"]), exp(p95[1,"upr"]))
        row_lbl <- "Log-Linear Prediction (median)"
      } else {
        vals    <- c(p90[1,"fit"], p90[1,"lwr"], p90[1,"upr"],
                     p95[1,"lwr"], p95[1,"upr"])
        row_lbl <- "Linear Prediction"
      }
      results <- matrix(vals, nrow = 1)
      colnames(results) <- c("Estimate", "90% Low PI", "90% High PI", "95% Low PI", "95% High PI")
      rownames(results) <- row_lbl
      datatable(results,
                colnames  = c("", "Estimate", "90% Low PI", "90% High PI", "95% Low PI", "95% High PI"),
                options   = list(dom = "t", ordering = FALSE),
                selection = "none") %>%
        formatCurrency(c("Estimate", "90% Low PI", "90% High PI", "95% Low PI", "95% High PI"),
                       currency = "", interval = 3, mark = ",", digits = 3)
    }, server = FALSE)

    output[[id_map$dist_plot]] <- renderPlotly({
      model     <- selected_model_fn()
      req(model)
      pred_vars <- setdiff(names(model$model)[-1], "(weights)")
      parts     <- build_new_data(model, pred_vars)
      new_data  <- parts$nd
      req(input[[id_map$pi_level]])

      pred_ref <- tryCatch(lm_pred_interval(model, new_data, 0.95, mode), error = function(e) NULL)
      req(!is.null(pred_ref))
      L        <- input[[id_map$pi_level]] / 100
      t_L      <- qt(1 - (1 - L) / 2, df.residual(model))

      if (mode == "loglin") {
        mu      <- pred_ref[1, "fit"]
        SE_pred <- (mu - pred_ref[1, "lwr"]) / qt(0.975, df.residual(model))
        estimate <- exp(mu)
        low      <- exp(mu - t_L * SE_pred)
        high     <- exp(mu + t_L * SE_pred)
        x_min    <- qlnorm(DISTRIBUTION_TRUNCATION_LOW,  meanlog = mu, sdlog = SE_pred)
        x_max    <- qlnorm(DISTRIBUTION_TRUNCATION_HIGH, meanlog = mu, sdlog = SE_pred)
        x_grid   <- seq(x_min, x_max, length.out = 400)
        y_grid   <- dlnorm(x_grid, meanlog = mu, sdlog = SE_pred)
        y_low    <- dlnorm(low,  meanlog = mu, sdlog = SE_pred) * 2.2
        y_high   <- dlnorm(high, meanlog = mu, sdlog = SE_pred) * 2.2
      } else {
        mu      <- pred_ref[1, "fit"]
        SE_pred <- (mu - pred_ref[1, "lwr"]) / qt(0.975, df.residual(model))
        estimate <- mu
        low      <- mu - t_L * SE_pred
        high     <- mu + t_L * SE_pred
        x_min    <- qnorm(DISTRIBUTION_TRUNCATION_LOW,  mean = mu, sd = SE_pred)
        x_max    <- qnorm(DISTRIBUTION_TRUNCATION_HIGH, mean = mu, sd = SE_pred)
        x_grid   <- seq(x_min, x_max, length.out = 400)
        y_grid   <- dnorm(x_grid, mean = mu, sd = SE_pred)
        y_low    <- dnorm(low,  mean = mu, sd = SE_pred) * 2
        y_high   <- dnorm(high, mean = mu, sd = SE_pred) * 2
      }

      rq_fit_cer <- tryCatch(
        ceres_ns("quantreg")$rq(formula(model), data = model$model, tau = 0.5),
        error = function(e) NULL
      )
      if (mode == "loglin") {
        median_raw_cer <- if (is.null(rq_fit_cer)) mu else {
          tryCatch(predict(rq_fit_cer, newdata = new_data)[[1]], error = function(e) mu)
        }
        median_est   <- exp(median_raw_cer)
        y_median_ann <- dlnorm(median_est, meanlog = mu, sdlog = SE_pred) * 2.2
      } else {
        median_est <- if (is.null(rq_fit_cer)) mu else {
          tryCatch(predict(rq_fit_cer, newdata = new_data)[[1]], error = function(e) mu)
        }
        y_median_ann <- dnorm(median_est, mean = mu, sd = SE_pred) * 2.2
      }

      in_pi   <- x_grid >= low & x_grid <= high
      x_shade <- c(low, x_grid[in_pi], high)
      y_shade <- c(0, y_grid[in_pi], 0)
      fmt      <- function(x) formatC(x, format = "f", digits = 3, big.mark = ",")
      pi_label <- paste0(round(L * 100), "%")
      y_peak   <- max(y_grid)

      pc_rgb       <- col2rgb(primary_color())[, 1]
      median_color <- paste0("rgba(", pc_rgb[1], ",", pc_rgb[2], ",", pc_rgb[3], ",0.65)")
      ann_low           <- list(
        x = low, y = y_low * 1.1,
        xref = "x", yref = "y",
        text = paste0("<b>", fmt(low), "</b><br><span style='font-size:10px;'>", pi_label, " Low</span>"),
        showarrow = FALSE, xanchor = "center", yanchor = "bottom",
        font = list(color = "#2F6FE4", size = 11)
      )
      ann_high          <- list(
        x = high, y = y_high * 1.1,
        xref = "x", yref = "y",
        text = paste0("<b>", fmt(high), "</b><br><span style='font-size:10px;'>", pi_label, " High</span>"),
        showarrow = FALSE, xanchor = "center", yanchor = "bottom",
        font = list(color = "#2F6FE4", size = 11)
      )
      gap_frac <- abs(median_est - estimate) / (x_max - x_min)
      mean_y   <- if (gap_frac < 0.08) y_peak * 1.24 else y_peak * 1.05
      ann_mean_center   <- list(
        x = estimate, y = mean_y,
        xref = "x", yref = "y",
        text = paste0("<b>", fmt(estimate), "</b><br><span style='font-size:10px;'>",
                      if (mode == "loglin") "Median, exp(Xb)" else "Mean", "</span>"),
        showarrow = FALSE, xanchor = "center", yanchor = "bottom",
        font = list(color = primary_color(), size = 11)
      )
      ann_median_center <- list(
        x = median_est, y = y_peak * 1.05,
        xref = "x", yref = "y",
        text = paste0("<b>", fmt(median_est), "</b><br><span style='font-size:10px;'>",
                      if (mode == "loglin") "Quantile-reg. median" else "Median", "</span>"),
        showarrow = FALSE, xanchor = "center", yanchor = "bottom",
        font = list(color = median_color, size = 11)
      )

      plot_ly() %>%
        add_trace(
          x = x_shade, y = y_shade,
          type = "scatter", mode = "lines",
          fill = "tozeroy",
          fillcolor = "rgba(47, 111, 228, 0.18)",
          line = list(color = "transparent"),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        add_trace(
          x = x_grid, y = y_grid,
          type = "scatter", mode = "lines",
          line = list(color = "#2F6FE4", width = 2.5),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        add_segments(
          x = median_est, xend = median_est,
          y = 0, yend = y_peak * 1.05,
          line = list(color = median_color, width = 2, dash = "dash"),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        add_segments(
          x = estimate, xend = estimate,
          y = 0, yend = y_peak * 1.05,
          line = list(color = primary_color(), width = 2, dash = "dash"),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        add_segments(
          x = low, xend = low,
          y = 0, yend = y_low,
          line = list(color = "#2F6FE4", width = 1.5, dash = "dot"),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        add_segments(
          x = high, xend = high,
          y = 0, yend = y_high,
          line = list(color = "#2F6FE4", width = 1.5, dash = "dot"),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        layout(
          xaxis = list(
            title = input$ols_response,
            zeroline = FALSE, showgrid = FALSE
          ),
          yaxis = list(
            title = "Density",
            zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE
          ),
          margin = list(t = 55, r = 20, b = 50, l = 40),
          annotations = list(ann_low, ann_mean_center, ann_median_center, ann_high),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    })

    output[[id_map$dist_stats]] <- renderUI({
      model     <- selected_model_fn()
      req(model)
      pred_vars <- setdiff(names(model$model)[-1], "(weights)")
      parts     <- build_new_data(model, pred_vars)
      new_data  <- parts$nd
      req(input[[id_map$pi_level]])

      pred_ref <- tryCatch(lm_pred_interval(model, new_data, 0.95, mode), error = function(e) NULL)
      req(!is.null(pred_ref))
      L        <- input[[id_map$pi_level]] / 100
      t_L      <- qt(1 - (1 - L) / 2, df.residual(model))

      if (mode == "loglin") {
        mu      <- pred_ref[1, "fit"]
        SE_pred <- (mu - pred_ref[1, "lwr"]) / qt(0.975, df.residual(model))
        estimate <- exp(mu)
        low      <- exp(mu - t_L * SE_pred)
        high     <- exp(mu + t_L * SE_pred)
      } else {
        mu      <- pred_ref[1, "fit"]
        SE_pred <- (mu - pred_ref[1, "lwr"]) / qt(0.975, df.residual(model))
        estimate <- mu
        low      <- mu - t_L * SE_pred
        high     <- mu + t_L * SE_pred
      }

      rq_fit_cer_s <- tryCatch(
        ceres_ns("quantreg")$rq(formula(model), data = model$model, tau = 0.5),
        error = function(e) NULL
      )
      if (mode == "loglin") {
        median_raw_cer_s <- if (is.null(rq_fit_cer_s)) mu else {
          tryCatch(predict(rq_fit_cer_s, newdata = new_data)[[1]], error = function(e) mu)
        }
        median_est <- exp(median_raw_cer_s)
      } else {
        median_est <- if (is.null(rq_fit_cer_s)) mu else {
          tryCatch(predict(rq_fit_cer_s, newdata = new_data)[[1]], error = function(e) mu)
        }
      }

      fmt      <- function(x) formatC(x, format = "f", digits = 3, big.mark = ",")
      pi_label <- paste0(round(L * 100), "%")

      tags$div(
        style = paste0(
          "display:flex;",
          "background:white; border:1px solid #dee2e6;",
          "border-radius:5px; padding:8px 0; margin-top:8px;",
          "box-shadow:0 1px 3px rgba(0,0,0,.05);"
        ),
        tags$div(
          style = "flex:1; text-align:center; padding:0 8px;",
          tags$div(style = "font-size:15px; font-weight:700; color:#2F6FE4;", fmt(low)),
          tags$div(style = "font-size:10px; color:#888; margin-top:2px;",
                   paste0(pi_label, " Low"))
        ),
        tags$div(
          style = "flex:1; text-align:center; border-left:1px solid #eee; padding:0 8px;",
          tags$div(style = paste0("font-size:16px; font-weight:700; color:", primary_color(), ";"), fmt(estimate)),
          tags$div(style = "font-size:10px; color:#888; margin-top:2px;",
                   if (mode == "loglin") "Median, exp(Xb)" else "Mean")
        ),
        tags$div(
          style = "flex:1; text-align:center; border-left:1px solid #eee; padding:0 8px;",
          tags$div(style = paste0("font-size:16px; font-weight:700; color:", paste0(primary_color(), "A6"), ";"), fmt(median_est)),
          tags$div(style = "font-size:10px; color:#888; margin-top:2px;",
                   if (mode == "loglin") "Quantile-reg. median" else "Median")
        ),
        tags$div(
          style = "flex:1; text-align:center; border-left:1px solid #eee; padding:0 8px;",
          tags$div(style = "font-size:15px; font-weight:700; color:#2F6FE4;", fmt(high)),
          tags$div(style = "font-size:10px; color:#888; margin-top:2px;",
                   paste0(pi_label, " High"))
        )
      )
    })

    output[[id_map$breakdown]] <- renderPlotly({
      model     <- selected_model_fn()
      req(model)
      pred_vars <- setdiff(names(model$model)[-1], "(weights)")
      req(length(pred_vars) > 1)
      parts      <- build_new_data(model, pred_vars)
      new_data   <- parts$nd
      input_vals <- parts$input_vals

      if (mode == "loglin") {
        explain_obj <- ceres_ns("DALEX")$explain(
          model,
          data    = model$model[, -1, drop = FALSE],
          y       = model$model[, 1],
          label   = "Log-Linear",
          verbose = FALSE
        )
        la         <- ceres_ns("iBreakDown")$local_attributions(explain_obj, new_data)
        cum_unit   <- exp(la$cumulative)
        unit_bars  <- diff(c(0, cum_unit))
        n          <- length(unit_bars)
        unit_bars[n] <- cum_unit[n]
        measure    <- c(rep("relative", n - 1), "total")
        main_lbl_vars <- pred_vars[!is_interaction_col(pred_vars, source = cer_source_cols())]
        sorted_vars <- main_lbl_vars[order(nchar(main_lbl_vars), decreasing = TRUE)]
        y_labels <- sapply(as.character(la$variable), function(lbl) {
          if (lbl == "intercept")  return(lbl)
          if (lbl == "prediction") return("Prediction")
          for (var in sorted_vars) {
            if (startsWith(lbl, paste0(var, " = ")) && is.numeric(model$model[[var]])) {
              orig_val <- as.numeric(input_vals[[var]])
              return(paste0(var, " = ", format(round(orig_val, 2), big.mark = ",", scientific = FALSE)))
            }
          }
          lbl
        })
        x_vals <- unit_bars
        y_vals <- factor(y_labels, levels = y_labels)
      } else {
        explain_obj <- ceres_ns("DALEX")$explain(
          model,
          data    = model$model[, -1, drop = FALSE],
          y       = model$model[, 1],
          label   = "Linear",
          verbose = FALSE
        )
        la      <- ceres_ns("iBreakDown")$local_attributions(explain_obj, new_data)
        x_vals  <- la$contribution
        y_raw   <- as.character(la$variable)
        y_raw[y_raw == "prediction"] <- "Prediction"
        y_vals  <- factor(y_raw, levels = y_raw)
        measure <- c(rep("relative", length(x_vals) - 1), "total")
      }

      plot_ly(data.frame(x = x_vals, y = y_vals, measure = measure),
              x = ~x, y = ~y, measure = ~measure,
              type = "waterfall", name = "New Prediction",
              orientation = "h",
              totals = list(marker = list(color = primary_color())),
              connector = list(mode = "between",
                               line = list(width = 4, color = "rgb(0, 0, 0)", dash = 0))) %>%
        layout(title      = "Local Attributions",
               xaxis      = list(title = "Marginal Contribution to Estimate", tickfont = "16", ticks = "outside"),
               yaxis      = list(title = "", type = "category", autorange = "reversed"),
               showlegend = FALSE) %>%
        config(displayModeBar = FALSE)
    })

    output[[id_map$actuals_predicted]] <- renderPlotly({
      sel <- input[[id_map$row_selected]]
      req(sel)
      model <- selected_model_fn()
      req(model)

      yhat     <- model$fitted.values
      y        <- model$model[[1]]
      row_lbls <- rownames(model$model)
      if (is.null(row_lbls)) row_lbls <- as.character(seq_along(y))

      filtered_df <- filtered_models_fn()
      keep_names  <- rownames(filtered_df)
      keep_idxs   <- match(keep_names, names(rv[[id_map$model_list_key]]))
      keep_idxs   <- keep_idxs[!is.na(keep_idxs)]
      req(length(keep_idxs) >= sel)
      current_idx <- keep_idxs[sel]
      yhat_test   <- rv[[id_map$yhat_test_key]][[current_idx]]
      y_test      <- rv[[id_map$y_test_key]]

      p <- plot_ly(
        x         = yhat,
        y         = y,
        type      = "scatter",
        mode      = "markers",
        marker    = list(color = primary_color(), size = 10),
        name      = "Training Data",
        text      = paste0("Row Name: ", row_lbls,
                           "<br>Actual: ",    round(y,    3),
                           "<br>Predicted: ", round(yhat, 3)),
        hoverinfo = "text"
      ) %>%
        add_trace(
          inherit   = FALSE,
          x         = sort(y),
          y         = sort(y),
          type      = "scatter",
          mode      = "lines",
          line      = list(color = "#000000", dash = "dash", width = 2),
          name      = "y = x",
          hoverinfo = "none"
        ) %>%
        layout(
          title = input$ols_response,
          xaxis = list(title = "Predicted Values"),
          yaxis = list(title = "Actual Values")
        ) %>%
        config(displayModeBar = FALSE)

      if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[2:4] &&
          !is.null(yhat_test) && !is.null(y_test) &&
          length(yhat_test) == length(y_test)) {
        test_names <- names(y_test)
        if (is.null(test_names) || length(test_names) != length(y_test))
          test_names <- paste("Test Obs", seq_along(yhat_test))
        p <- p %>% add_trace(
          inherit   = FALSE,
          x         = yhat_test,
          y         = y_test,
          type      = "scatter",
          mode      = "markers",
          marker    = list(color = graph_color_1, size = 10),
          name      = "Test Data",
          text      = paste0("Row Name: ",   test_names,
                             "<br>Actual: ",    round(y_test,    3),
                             "<br>Predicted: ", round(yhat_test, 3)),
          hoverinfo = "text"
        )
      }
      p
    })

    output[[id_map$resid_predicted]] <- renderPlotly({
      sel <- input[[id_map$row_selected]]
      req(sel)
      model <- selected_model_fn()
      req(model)

      pred_train <- model$fitted.values
      y_train    <- model$model[[1]]
      resids     <- y_train - pred_train
      row_lbls   <- rownames(model$model)
      if (is.null(row_lbls)) row_lbls <- as.character(seq_along(pred_train))

      filtered_df <- filtered_models_fn()
      keep_names  <- rownames(filtered_df)
      keep_idxs   <- match(keep_names, names(rv[[id_map$model_list_key]]))
      keep_idxs   <- keep_idxs[!is.na(keep_idxs)]
      req(length(keep_idxs) >= sel)
      current_idx <- keep_idxs[sel]

      p <- plot_ly(
        x         = pred_train,
        y         = resids,
        type      = "scatter",
        mode      = "markers",
        marker    = list(color = primary_color(), size = 10),
        name      = "Train Residuals",
        text      = paste0("Row Name: ",  row_lbls,
                           "<br>Predicted: ", round(pred_train, 3),
                           "<br>Residual: ",  round(resids,     3)),
        hoverinfo = "text"
      ) %>%
        layout(
          title = paste("Residuals vs Predicted:", input$ols_response),
          xaxis = list(title = "Predicted Values"),
          yaxis = list(title = "Residuals", zeroline = FALSE),
          shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                             line = list(color = "rgba(100,116,139,0.7)", width = 1.5, dash = "dash")))
        ) %>%
        config(displayModeBar = FALSE)

      if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[2:4]) {
        yhat_test   <- rv[[id_map$yhat_test_key]][[current_idx]]
        y_test_vec  <- rv[[id_map$y_test_key]]
        if (!is.null(yhat_test) && !is.null(y_test_vec) &&
            length(yhat_test) == length(y_test_vec)) {
          resids_test <- y_test_vec - yhat_test
          test_names  <- names(yhat_test)
          if (is.null(test_names)) test_names <- paste("Test Obs", seq_along(yhat_test))
          p <- p %>% add_trace(
            x         = yhat_test,
            y         = resids_test,
            type      = "scatter",
            mode      = "markers",
            marker    = list(color = graph_color_1, size = 10),
            name      = "Test Residuals",
            text      = paste0("Row Name: ",  test_names,
                               "<br>Predicted: ", round(yhat_test,   3),
                               "<br>Residual: ",  round(resids_test, 3)),
            hoverinfo = "text"
          )
        }
      }
      p
    })

    output[[id_map$resid_predictors]] <- renderPlotly({
      model <- selected_model_fn()
      req(model, model$model, model$residuals, input[[id_map$resid_predictor]])
      req(input[[id_map$resid_predictor]] %in% colnames(model$model))

      x         <- model$model[, input[[id_map$resid_predictor]]]
      epsilon   <- model$residuals
      x_display <- if (is.numeric(x)) round(x, 3) else as.character(x)
      row_names <- rownames(model$model)
      if (is.null(row_names)) row_names <- seq_along(epsilon)

      plot_ly(x = ~x, y = ~epsilon, type = "scatter", mode = "markers",
              marker    = list(color = primary_color(), size = 10),
              name      = "Fit Residuals",
              text      = ~paste0("Row Name: ", row_names,
                                  "<br>", input[[id_map$resid_predictor]], ": ", x_display,
                                  "<br>Residual: ", round(epsilon, 3)),
              hoverinfo = "text") %>%
        layout(title = paste("Residuals vs", input[[id_map$resid_predictor]]),
               xaxis = list(title = input[[id_map$resid_predictor]]),
               yaxis = list(title = "Residuals", zeroline = FALSE),
               shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0, line = list(color = "rgba(100,116,139,0.7)", width = 1.5, dash = "dash")))) %>%
        config(displayModeBar = FALSE)
    })

    output[[id_map$resid_normality]] <- renderPlotly({
      model <- selected_model_fn()
      req(model, model$residuals)
      epsilon <- model$residuals
      req(length(epsilon) > 5)

      num_bins  <- if (!is.null(input[[id_map$resid_nbins]])) input[[id_map$resid_nbins]] else floor(sqrt(length(epsilon)))
      fit_dens  <- density(epsilon, na.rm = TRUE)
      sigma_val <- summary(model)$sigma
      req(sigma_val, is.finite(sigma_val), sigma_val > 0)
      emp_normal <- dnorm(fit_dens$x, mean = 0, sd = sigma_val)

      p1 <- plot_ly(x = ~epsilon, type = "histogram", name = "Histogram",
                    nbinsx = num_bins,
                    marker = list(color = secondary_color(),
                                  line  = list(color = primary_color(), width = 2))) %>%
        add_trace(x = ~fit_dens$x, y = ~fit_dens$y, type = "scatter", mode = "lines",
                  fill = "tozeroy", yaxis = "y2", name = "Density", inherit = FALSE,
                  line = list(color = primary_color(), width = 2.5), fillcolor = paste0(primary_color(), "1A")) %>%
        add_trace(x = ~fit_dens$x, y = ~emp_normal, type = "scatter", mode = "lines",
                  fill = "tozeroy", yaxis = "y2", name = "Normal Density", inherit = FALSE,
                  line = list(color = graph_color_2, width = 2, dash = "dash"), fillcolor = "rgba(0,0,0,0)") %>%
        layout(title = "Histogram/Density Plot",
               xaxis  = list(title = "Residuals"),
               yaxis  = list(title = "Frequency"),
               yaxis2 = list(showticklabels = FALSE, showgrid = FALSE,
                              overlaying = "y", side = "right"))

      sorted_data         <- sort(epsilon)
      n                   <- length(sorted_data)
      percentiles         <- ppoints(n)
      theoretical_quantiles <- qnorm(percentiles)
      k   <- 0.895 / (sqrt(n) * (1 - 0.01/sqrt(n) + 0.85/n))
      LCL <- qnorm(pmax(percentiles - k, 1e-10))
      UCL <- qnorm(pmin(percentiles + k, 1 - 1e-10))
      q_sample  <- quantile(sorted_data, c(0.25, 0.75), na.rm = TRUE)
      q_theo    <- qnorm(c(0.25, 0.75))
      slope     <- (q_sample[2] - q_sample[1]) / (q_theo[2] - q_theo[1])
      intercept <- q_sample[1] - slope * q_theo[1]
      qq_line_y <- intercept + slope * theoretical_quantiles

      p2 <- plot_ly(x = ~theoretical_quantiles, y = ~sorted_data,
                    type = "scatter", mode = "markers",
                    marker = list(color = primary_color()), name = "Data") %>%
        add_trace(x = ~theoretical_quantiles, y = ~qq_line_y, type = "scatter", mode = "lines",
                  name = "Expected Quantiles", inherit = FALSE,
                  line = list(color = graph_color_2, width = 2, dash = "dash")) %>%
        add_trace(x = ~UCL, y = ~sorted_data, type = "scatter", mode = "lines",
                  name = "Upper 95% CI", inherit = FALSE,
                  line = list(color = "rgba(100,116,139,0.6)", width = 1.2, dash = "dot")) %>%
        add_trace(x = ~LCL, y = ~sorted_data, type = "scatter", mode = "lines",
                  name = "Lower 95% CI", inherit = FALSE,
                  line = list(color = "rgba(100,116,139,0.6)", width = 1.2, dash = "dot")) %>%
        layout(title = "Q-Q Plot",
               xaxis = list(title = "Theoretical Normal Quantiles",
                            range = range(theoretical_quantiles) + c(-0.35, 0.35)),
               yaxis = list(title = "Sample Quantiles"))

      subplot(p1, p2, nrows = 1, titleX = TRUE, titleY = TRUE) %>%
        layout(showlegend = FALSE, title = "Residual Normality") %>%
        config(displayModeBar = FALSE)
    })

    output[[id_map$influence_plot]] <- renderPlotly({
      inf   <- rv[[id_map$influence_data_key]]
      req(inf)
      n_obs <- nrow(inf)
      n_par <- if (!is.null(rv[[id_map$influence_n_par_key]])) rv[[id_map$influence_n_par_key]] else 2L

      lev_cutoff <- 2 * n_par / n_obs
      std_cutoff <- 2

      serious       <- (abs(inf$std_residual) > 3) | (inf$cooks_d > 1) | (inf$leverage > 3 * n_par / n_obs)
      moderate      <- !serious & ((abs(inf$std_residual) > std_cutoff) | (inf$leverage > lev_cutoff) | (inf$cooks_d > 4 / n_obs))
      marker_colors <- ifelse(serious, default_colorway[1], ifelse(moderate, default_colorway[2], primary_color()))
      cd_rng        <- range(inf$cooks_d, na.rm = TRUE)
      px_sizes      <- if (!is.finite(diff(cd_rng)) || diff(cd_rng) <= 0) rep(12, nrow(inf)) else
                         8 + 32 * (inf$cooks_d - cd_rng[1]) / diff(cd_rng)

      excluded_labels <- rv[[id_map$ui_exclusions_key]]
      if (is.null(excluded_labels)) excluded_labels <- character(0)
      excl_mask <- inf$row_label %in% excluded_labels
      marker_colors[excl_mask] <- "#A0A0A0"
      marker_opacity <- ifelse(excl_mask, 0.55, 0.85)

      sr_min  <- if (is.finite(min(inf$std_residual, na.rm = TRUE))) min(inf$std_residual, na.rm = TRUE) - 0.5 else -3
      sr_max  <- if (is.finite(max(inf$std_residual, na.rm = TRUE))) max(inf$std_residual, na.rm = TRUE) + 0.5 else  3
      lev_max <- if (is.finite(max(inf$leverage,     na.rm = TRUE))) max(inf$leverage,     na.rm = TRUE) + 0.01 else 1

      hover_txt <- paste0(ifelse(excl_mask, "(EXCLUDED) ", ""),
                          "Row: ",        inf$row_label,
                          "<br>Cook's D: ",  round(inf$cooks_d,      4),
                          "<br>Leverage: ",  round(inf$leverage,     4),
                          "<br>Std Resid: ", round(inf$std_residual, 3),
                          ifelse(excl_mask, "<br><i>(click to re-include)</i>",
                                            "<br><i>(click to exclude)</i>"))

      p_main <- plot_ly(
        data       = inf,
        x          = ~leverage,
        y          = ~std_residual,
        type       = "scatter",
        mode       = "markers",
        source     = id_map$exclusions_src,
        customdata = ~row_label,
        marker     = list(color = I(marker_colors), opacity = marker_opacity,
                          size = px_sizes,
                          line = list(color = '#333333', width = 1)),
        text       = hover_txt,
        hoverinfo  = "text",
        name       = "Observations"
      )

      p_main %>%
        event_register("plotly_click") %>%
        add_segments(
          x = lev_cutoff, xend = lev_cutoff,
          y = sr_min, yend = sr_max,
          line       = list(color = "#888888", dash = "dash", width = 1),
          name       = paste0("Leverage cutoff (", round(lev_cutoff, 3), ")"),
          inherit    = FALSE,
          showlegend = FALSE
        ) %>%
        add_segments(
          x = 0, xend = lev_max,
          y = std_cutoff, yend = std_cutoff,
          line       = list(color = "#888888", dash = "dash", width = 1),
          name       = "Std Resid = +2",
          inherit    = FALSE,
          showlegend = FALSE
        ) %>%
        add_segments(
          x = 0, xend = lev_max,
          y = -std_cutoff, yend = -std_cutoff,
          line       = list(color = "#888888", dash = "dash", width = 1),
          name       = "Std Resid = -2",
          inherit    = FALSE,
          showlegend = FALSE
        ) %>%
        layout(
          xaxis = list(title = "Leverage (Hat Value)"),
          yaxis = list(title = "Standardized Residual"),
          showlegend = TRUE,
          margin = list(l = 60, r = 150, t = 30, b = 60, pad = 0, autoexpand = FALSE),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    })

    output[[id_map$influence_table]] <- DT::renderDataTable({
      inf   <- rv[[id_map$influence_data_key]]
      req(inf)
      n_obs <- nrow(inf)
      n_par <- if (!is.null(rv[[id_map$influence_n_par_key]])) rv[[id_map$influence_n_par_key]] else 2L

      lev_cutoff      <- 2 * n_par / n_obs
      excluded_labels <- isolate(rv[[id_map$ui_exclusions_key]])
      if (is.null(excluded_labels)) excluded_labels <- character(0)
      excl_html <- vapply(inf$row_label, function(lbl) {
        chk <- if (lbl %in% excluded_labels) "checked" else ""
        sprintf('<input type="checkbox" class="%s" data-label="%s" %s>',
                id_map$excl_class, htmltools::htmlEscape(lbl), chk)
      }, character(1))

      df_display <- data.frame(
        `Row`          = inf$row_label,
        `Cook's D`     = round(inf$cooks_d,      4),
        `Leverage`     = round(inf$leverage,     4),
        `Std Residual` = round(inf$std_residual, 3),
        `Exclude`      = excl_html,
        check.names    = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df_display,
        rownames  = FALSE,
        selection = "none",
        escape    = FALSE,
        options   = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "tp",
          stateSave  = TRUE,
          columnDefs = list(list(className = "dt-center", targets = 4L))
        )
      ) %>%
        DT::formatStyle(
          "Cook's D",
          backgroundColor = DT::styleInterval(
            4 / n_obs,
            c("transparent", if (isTRUE(input$dark_mode)) "#7B2030" else "#FFB8C3")
          )
        ) %>%
        DT::formatStyle(
          "Leverage",
          backgroundColor = DT::styleInterval(
            lev_cutoff,
            c("transparent", if (isTRUE(input$dark_mode)) "#7B2030" else "#FFB8C3")
          )
        ) %>%
        DT::formatStyle(
          "Std Residual",
          backgroundColor = DT::styleInterval(
            c(-2, 2),
            c(if (isTRUE(input$dark_mode)) "#1A5276" else "#6ECFEA",
              "transparent",
              if (isTRUE(input$dark_mode)) "#7B2030" else "#FFB8C3")
          )
        )
    }, server = FALSE)

    observeEvent(input[[id_map$influence_checkbox]], {
      info    <- input[[id_map$influence_checkbox]]
      lbl     <- info$label
      chked   <- isTRUE(info$checked)
      current <- rv[[id_map$ui_exclusions_key]]
      rv[[id_map$ui_exclusions_key]] <- if (chked) union(current, lbl) else setdiff(current, lbl)
    })

    observeEvent(input[[id_map$save_exclusions]], {
      shinyjs::disable(id_map$save_exclusions)
      on.exit(shinyjs::enable(id_map$save_exclusions))
      req(rv[[id_map$influence_data_key]])

      excluded_labels <- rv[[id_map$ui_exclusions_key]]
      if (length(excluded_labels) == 0) {
        showNotification("No rows marked for exclusion.", type = "message")
        return()
      }

      base_key  <- input$data_select_cer_generation
      base_data <- rv$dataset_library[[base_key]]
      model     <- rv[[id_map$selected_model_key]]
      if (is.null(base_data) || is.null(model) || is.null(model$model)) {
        showNotification("Could not locate the model's source dataset. No change made.", type = "warning")
        return()
      }

      train_rn  <- rownames(model$model)
      canon_lbl <- make.unique(train_rn)
      excl_rn   <- train_rn[canon_lbl %in% excluded_labels]
      excl_indices <- which(rownames(base_data) %in% excl_rn)

      if (length(excl_indices) == 0) {
        showNotification("Could not match excluded rows to the source dataset. No change made.", type = "warning")
        return()
      }
      if (length(unique(excl_rn)) != length(excluded_labels)) {
        showNotification(
          paste0("Note: ", length(unique(excl_rn)), " of ", length(excluded_labels),
                 " excluded rows matched the source dataset."),
          type = "warning", duration = 6
        )
      }

      n_excl   <- length(excl_indices)
      new_data <- base_data[-excl_indices, , drop = FALSE]
      new_name <- paste0(base_key, "_excl_", n_excl, "obs")

      store_dataset(new_name, new_data)
      updateSelectInput(session, "data_select",
                        choices = names(rv$dataset_library), selected = new_name)
      updateSelectInput(session, "data_select_cer_generation",
                        choices = names(rv$dataset_library), selected = new_name)
      removeModal()
      showNotification(
        paste0("Dataset '", new_name, "' saved with ", n_excl,
               " observation(s) removed. Select it and re-run your models."),
        type = "message", duration = 6
      )
    })

    observeEvent(input[[id_map$reset_exclusions]], {
      rv[[id_map$ui_exclusions_key]] <- character(0)
      rv[[id_map$tuning_pushed_key]] <- NULL
      showNotification("Exclusions cleared and refit reverted.", type = "message", duration = 2)
    })

    by_model_key <- paste0(id_map$ui_exclusions_key, "_by_model")
    last_model_key <- paste0(id_map$ui_exclusions_key, "_last_model")

    current_tuning_model_name <- reactive({
      rs <- input[[id_map$row_selected]]
      if (is.null(rs) || length(rs) == 0L) return(NA_character_)
      fdf <- filtered_models_fn()
      if (is.null(fdf) || nrow(fdf) < rs) return(NA_character_)
      rownames(fdf)[rs]
    })

    observeEvent(current_tuning_model_name(), {
      rv[[id_map$tuning_pushed_key]] <- NULL

      new_name  <- current_tuning_model_name()
      prev_name <- isolate(rv[[last_model_key]])
      lst       <- isolate(rv[[by_model_key]])
      if (is.null(lst)) lst <- list()

      if (!is.null(prev_name) && !is.na(prev_name) && nzchar(prev_name)) {
        lst[[prev_name]] <- isolate(rv[[id_map$ui_exclusions_key]]) %||% character(0)
      }

      if (!is.na(new_name) && nzchar(new_name)) {
        new_excl <- lst[[new_name]]
        if (is.null(new_excl)) new_excl <- character(0)
        rv[[id_map$ui_exclusions_key]] <- new_excl
        rv[[last_model_key]] <- new_name
      }
      rv[[by_model_key]] <- lst
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    observe({
      cur <- rv[[id_map$ui_exclusions_key]]
      nm  <- isolate(current_tuning_model_name())
      if (is.null(nm) || is.na(nm) || !nzchar(nm)) return()
      lst <- isolate(rv[[by_model_key]])
      if (is.null(lst)) lst <- list()
      cur_vec <- if (is.null(cur)) character(0) else cur
      if (!identical(lst[[nm]], cur_vec)) {
        lst[[nm]] <- cur_vec
        rv[[by_model_key]] <- lst
      }
    })

    .excl_label_to_idx <- function(model_obj, labels) {
      if (is.null(labels) || length(labels) == 0L) return(integer(0))
      rn <- rownames(model_obj$model)
      if (is.null(rn)) return(integer(0))
      rn <- make.unique(rn)
      idx <- which(rn %in% labels)
      if (length(idx) == 0L) integer(0) else idx
    }

    live_tuning_fit <- reactive({
      model <- rv[[id_map$selected_model_key]]
      req(model)
      data_full <- model$model
      req(!is.null(data_full))
      excl_idx <- .excl_label_to_idx(model, rv[[id_map$ui_exclusions_key]])
      n <- nrow(data_full)
      keep_idx <- setdiff(seq_len(n), excl_idx)
      n_par <- length(stats::coef(model))
      if (length(keep_idx) < n_par + 1L) return(NULL)
      err <- ceres_error_term(attr(model, "error_type") %||% "OLS")
      fit <- tryCatch(
        if (err == "OLS") lm(formula(model), data = data_full[keep_idx, , drop = FALSE])
        else fit_irls(formula(model), data = data_full[keep_idx, , drop = FALSE],
                      error_type = err, mode = mode),
        error = function(e) NULL
      )
      if (is.null(fit)) return(NULL)
      attr(fit, "error_type") <- err
      yhat_unit <- attr(fit, "mupe_unit_yhat")
      if (!is.null(yhat_unit)) attr(fit, "mupe_weights") <- 1 / yhat_unit^2
      fit
    })

    output[[id_map$tuning_plot]] <- renderPlotly({
      model_orig <- rv[[id_map$selected_model_key]]
      req(model_orig)
      data_full <- model_orig$model
      req(!is.null(data_full))
      fit <- live_tuning_fit()
      y_full <- data_full[[1]]
      n <- nrow(data_full)
      row_lbls <- rownames(data_full)
      if (is.null(row_lbls)) row_lbls <- as.character(seq_len(n))
      row_lbls <- make.unique(row_lbls)
      excl <- .excl_label_to_idx(model_orig, rv[[id_map$ui_exclusions_key]])

      yhat_full <- if (!is.null(fit)) {
        tryCatch(
          as.numeric(suppressWarnings(predict(fit, newdata = data_full))),
          error = function(e) rep(NA_real_, n)
        )
      } else {
        as.numeric(model_orig$fitted.values)
      }
      inc_mask <- !(seq_len(n) %in% excl)

      pc       <- primary_color()
      pc_rgb   <- col2rgb(pc)[, 1]
      pc_band  <- paste0("rgba(", pc_rgb[1], ",", pc_rgb[2], ",", pc_rgb[3], ",0.18)")

      p <- plot_ly(source = id_map$tuning_src)

      pi_lvl <- 0.80
      if (!is.null(fit)) {
        s_fit  <- tryCatch(summary(fit), error = function(e) NULL)
        if (!is.null(s_fit) && !is.null(s_fit$sigma) && !is.null(s_fit$df)) {
          sigma_hat <- s_fit$sigma
          df_resid  <- s_fit$df[2]
          if (is.finite(sigma_hat) && is.finite(df_resid) && df_resid > 0) {
            t_crit  <- qt(1 - (1 - pi_lvl) / 2, df_resid)
            p_dim   <- length(stats::coef(fit))
            n_fit   <- length(stats::fitted(fit))
            h_bar   <- if (is.finite(n_fit) && n_fit > 0) p_dim / n_fit else 0
            pi_half <- t_crit * sigma_hat * sqrt(1 + h_bar)
            rng     <- range(yhat_full, na.rm = TRUE, finite = TRUE)
            if (all(is.finite(rng)) && rng[2] > rng[1]) {
              grid_x <- seq(rng[1], rng[2], length.out = 100)
              half_x <- if (identical(mode, "linear") && !is.null(fit$weights) &&
                            (attr(fit, "error_type") %||% "OLS") %in% c("MUPE", "ZMPE", "WLR"))
                pi_half * abs(grid_x) else pi_half
              p <- p %>% add_ribbons(
                x = grid_x, ymin = grid_x - half_x, ymax = grid_x + half_x,
                fillcolor = pc_band, line = list(color = "transparent"),
                name = paste0(round(pi_lvl * 100), "% PI"), hoverinfo = "skip"
              )
            }
          }
        }
      }

      axrng <- range(c(y_full, yhat_full), na.rm = TRUE, finite = TRUE)
      p <- p %>% add_trace(
        x = axrng, y = axrng,
        type = "scatter", mode = "lines",
        line = list(color = "rgba(100,116,139,0.85)", dash = "dash", width = 1.5),
        name = "y = x", hoverinfo = "skip"
      )

      inf_data <- rv[[id_map$influence_data_key]]
      n_par_inf <- if (!is.null(rv[[id_map$influence_n_par_key]])) {
        rv[[id_map$influence_n_par_key]]
      } else 2L

      color_for_idx <- function(idx) {
        if (is.null(inf_data) || nrow(inf_data) < n || length(idx) == 0) {
          return(rep(pc, length(idx)))
        }
        sub <- inf_data[idx, , drop = FALSE]
        n_inf <- nrow(inf_data)
        lev_cut <- 2 * n_par_inf / n_inf
        serious  <- (abs(sub$std_residual) > 3) |
                    (sub$cooks_d > 1) |
                    (sub$leverage > 3 * n_par_inf / n_inf)
        moderate <- !serious & ((abs(sub$std_residual) > 2) |
                                (sub$leverage > lev_cut) |
                                (sub$cooks_d > 4 / n_inf))
        ifelse(serious, default_colorway[1],
               ifelse(moderate, default_colorway[2], pc))
      }

      inc_idx    <- which(inc_mask)
      inc_colors <- color_for_idx(inc_idx)
      inc_hover  <- if (!is.null(inf_data) && nrow(inf_data) >= n) {
        sub <- inf_data[inc_idx, , drop = FALSE]
        paste0("Row: ", row_lbls[inc_mask],
               "<br>Actual: ",    round(y_full[inc_mask], 3),
               "<br>Predicted: ", round(yhat_full[inc_mask], 3),
               "<br>Cook's D: ",  round(sub$cooks_d, 4),
               "<br>Leverage: ",  round(sub$leverage, 4),
               "<br>Std Resid: ", round(sub$std_residual, 4),
               "<br><i>(click to exclude)</i>")
      } else {
        paste0("Row: ", row_lbls[inc_mask],
               "<br>Actual: ",    round(y_full[inc_mask], 3),
               "<br>Predicted: ", round(yhat_full[inc_mask], 3),
               "<br><i>(click to exclude)</i>")
      }

      inc_group <- ifelse(inc_colors == default_colorway[1], "Influential",
                          ifelse(inc_colors == default_colorway[2], "Worth a look", "Included"))
      for (grp in c("Included", "Worth a look", "Influential")) {
        w <- which(inc_group == grp)
        if (!length(w)) next
        p <- p %>% add_trace(
          x = yhat_full[inc_idx[w]], y = y_full[inc_idx[w]],
          type = "scatter", mode = "markers",
          marker = list(color = inc_colors[w], size = 11, opacity = 0.85,
                        line = list(color = "white", width = 1)),
          customdata = inc_idx[w],
          text = inc_hover[w],
          hoverinfo = "text", name = grp
        )
      }

      if (any(!inc_mask)) {
        p <- p %>% add_trace(
          x = yhat_full[!inc_mask], y = y_full[!inc_mask],
          type = "scatter", mode = "markers",
          marker = list(symbol = "x", size = 13, color = "#888888", opacity = 0.6,
                        line = list(color = "#444444", width = 1)),
          customdata = which(!inc_mask),
          text = paste0("(EXCLUDED) Row: ", row_lbls[!inc_mask],
                        "<br>Actual: ", round(y_full[!inc_mask], 3),
                        "<br>Predicted: ", round(yhat_full[!inc_mask], 3),
                        "<br><i>(click to re-include)</i>"),
          hoverinfo = "text", name = "Excluded"
        )
      }

      p %>%
        event_register("plotly_click") %>%
        layout(
          title = list(text = "Actual vs predicted · click a point to exclude it"),
          xaxis = list(title = "Predicted", zeroline = FALSE),
          yaxis = list(title = "Actual",    zeroline = FALSE),
          legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom"),
          margin = list(l = 60, r = 150, t = 84, b = 60, pad = 0, autoexpand = FALSE),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    })

    .toggle_exclusion_labels <- function(clicked_labels) {
      if (length(clicked_labels) == 0L) return(invisible())
      cur <- isolate(rv[[id_map$ui_exclusions_key]])
      if (is.null(cur)) cur <- character(0)
      to_add    <- setdiff(clicked_labels, cur)
      to_remove <- intersect(clicked_labels, cur)
      rv[[id_map$ui_exclusions_key]] <-
        sort(unique(c(setdiff(cur, to_remove), to_add)))
    }

    .quiet_event_data <- function(src) {
      withCallingHandlers(
        event_data("plotly_click", source = src),
        warning = function(w) {
          if (grepl("not registered", conditionMessage(w), fixed = TRUE)) {
            invokeRestart("muffleWarning")
          }
        }
      )
    }

    tuning_click_ed <- reactive({ .quiet_event_data(id_map$tuning_src) })
    exclusions_click_ed <- reactive({ .quiet_event_data(id_map$exclusions_src) })

    observeEvent(tuning_click_ed(), {
      ed <- tuning_click_ed()
      if (is.null(ed) || is.null(ed$customdata)) return()
      rid <- suppressWarnings(as.integer(ed$customdata))
      rid <- rid[!is.na(rid)]
      if (length(rid) == 0L) return()
      model <- isolate(rv[[id_map$selected_model_key]])
      if (is.null(model) || is.null(model$model)) return()
      rn <- rownames(model$model)
      if (is.null(rn) || length(rn) == 0L) return()
      rn <- make.unique(rn)
      rid <- rid[rid >= 1L & rid <= length(rn)]
      if (length(rid) == 0L) return()
      .toggle_exclusion_labels(rn[rid])
    })

    observeEvent(exclusions_click_ed(), {
      ed <- exclusions_click_ed()
      if (is.null(ed) || is.null(ed$customdata)) return()
      lbls <- as.character(ed$customdata)
      lbls <- lbls[nzchar(lbls)]
      if (length(lbls) == 0L) return()
      .toggle_exclusion_labels(lbls)
    })

    observe({
      excl <- rv[[id_map$ui_exclusions_key]]
      if (is.null(excl)) excl <- character(0)
      session$sendCustomMessage(
        "ceres_dt_excl_update",
        list(labels = as.list(excl), cls = id_map$excl_class)
      )
    })

    observeEvent(input[[id_map$tuning_push]], {
      fit <- live_tuning_fit()
      if (is.null(fit)) {
        showNotification("No live refit available to push (too few rows after exclusions).",
                         type = "warning", duration = 4)
        return()
      }
      attr(fit, "tuning_pushed") <- TRUE
      attr(fit, "pushed_for") <- current_tuning_model_name()
      excl_labels_snap <- rv[[id_map$ui_exclusions_key]] %||% character(0)
      attr(fit, "tuning_excluded_labels") <- excl_labels_snap
      orig_model <- rv[[id_map$selected_model_key]]
      if (!is.null(orig_model)) {
        attr(fit, "log1p_cols") <- attr(orig_model, "log1p_cols")
      }
      if (!is.null(orig_model) && !is.null(orig_model$model) &&
          length(excl_labels_snap) > 0L) {
        rn_orig <- rownames(orig_model$model)
        if (!is.null(rn_orig)) rn_orig <- make.unique(rn_orig)
        keep_mask <- !is.null(rn_orig) & (rn_orig %in% excl_labels_snap)
        if (any(keep_mask)) {
          attr(fit, "tuning_excluded_rows_df") <- orig_model$model[keep_mask, , drop = FALSE]
        }
      }
      rv[[id_map$tuning_pushed_key]] <- fit
      showNotification(
        "Refit pushed to modal. All tabs reflect the new fit. Pick a different model row in the parent table to revert.",
        type = "message", duration = 5
      )
    })

    .refit_block_reason <- function(model_orig, excl_labels) {
      if (is.null(model_orig) || is.null(model_orig$model)) return(NULL)
      data_full <- model_orig$model
      rn <- rownames(data_full)
      if (!is.null(rn)) rn <- make.unique(rn)
      excl_idx <- if (is.null(rn)) integer(0) else
                    which(rn %in% (excl_labels %||% character(0)))
      keep_idx <- setdiff(seq_len(nrow(data_full)), excl_idx)
      if (length(keep_idx) == 0L) return(NULL)
      pred_cols <- setdiff(names(data_full)[-1L], "(weights)")
      collapsed <- character(0)
      for (cn in pred_cols) {
        full_vals <- data_full[[cn]]
        if (!is.numeric(full_vals) && !is.factor(full_vals)) next
        u_full <- unique(full_vals)
        u_kept <- unique(full_vals[keep_idx])
        if (length(u_full) > 1L && length(u_kept) <= 1L) {
          removed <- setdiff(as.character(u_full), as.character(u_kept))
          removed_txt <- if (length(removed) > 3L)
            paste0(paste(removed[1:3], collapse = ", "), ", …")
          else paste(removed, collapse = ", ")
          collapsed <- c(collapsed, paste0(cn, " = ", removed_txt))
        }
      }
      if (length(collapsed) == 0L) NULL else collapsed
    }

    .tpred_default <- function(model, var, pre_logged, log1p_cols) {
      col <- model$model[[var]]
      med <- median(col, na.rm = TRUE)
      dv <- if (identical(mode, "loglin") && !all(col %in% c(0, 1))) {
        if (var %in% log1p_cols)      expm1(med)
        else if (var %in% pre_logged) exp(exp(med))
        else                          exp(med)
      } else med
      if (is.finite(dv) && dv >= 1) dv <- round(dv, 0)
      dv
    }

    output[[id_map$tuning_pred_inputs]] <- renderUI({
      model <- rv[[id_map$selected_model_key]]
      req(model)
      pred_vars <- setdiff(names(model$model)[-1], "(weights)")
      main_vars <- pred_vars[!is_interaction_col(pred_vars, source = cer_source_cols())]
      if (!length(main_vars)) return(NULL)
      ids <- pred_input_ids(main_vars, id_map$tuning_pred_prefix)
      cer_key    <- input$data_select_cer_generation
      pre_logged <- if (!is.null(cer_key) && nzchar(cer_key))
        rv$log_transformed_by_key[[cer_key]] %||% character(0) else character(0)
      log1p_cols <- attr(model, "log1p_cols") %||% character(0)

      flds <- lapply(seq_along(main_vars), function(i) {
        var <- main_vars[i]
        cur <- isolate(input[[ids[i]]])
        col <- model$model[[var]]
        tags$div(
          style = "flex:0 0 175px; max-width:175px; overflow-wrap:anywhere; word-break:break-word;",
          if (is.numeric(col)) {
            numericInput(ids[i], var,
                         value = if (!is.null(cur)) cur
                                 else .tpred_default(model, var, pre_logged, log1p_cols))
          } else {
            lvls <- model$xlevels[[var]] %||% levels(col) %||%
                    sort(unique(as.character(col)))
            selectInput(ids[i], var, choices = lvls,
                        selected = if (!is.null(cur) && cur %in% lvls) cur
                                   else names(sort(table(col), decreasing = TRUE))[1])
          }
        )
      })
      tags$div(class = "live-pred-inputs",
               style = "display:flex; flex-wrap:wrap; gap:2px 18px; align-items:flex-start;",
               flds)
    })

    observeEvent(input[[id_map$tuning_pred_sync]], {
      model <- rv[[id_map$selected_model_key]]
      req(model)
      pred_vars <- setdiff(names(model$model)[-1], "(weights)")
      main_vars <- pred_vars[!is_interaction_col(pred_vars, source = cer_source_cols())]
      src_ids <- pred_input_ids(main_vars, id_map$pred_prefix)
      dst_ids <- pred_input_ids(main_vars, id_map$tuning_pred_prefix)
      for (i in seq_along(main_vars)) {
        v <- input[[src_ids[i]]]
        if (is.null(v)) next
        if (is.numeric(model$model[[main_vars[i]]])) {
          updateNumericInput(session, dst_ids[i], value = v)
        } else {
          updateSelectInput(session, dst_ids[i], selected = v)
        }
      }
      showNotification("Copied input values from the Predictions tab.",
                       type = "message", duration = 3)
    }, ignoreInit = TRUE)

    output[[id_map$tuning_prediction]] <- renderUI({
      fit <- live_tuning_fit()
      if (is.null(fit)) {
        return(tags$div(style = "font-size:0.8em;color:#6c757d;",
                        "Live prediction unavailable — too few points remain after exclusions."))
      }
      pred_vars <- setdiff(names(fit$model)[-1], "(weights)")
      if (!length(pred_vars)) return(NULL)

      parts <- tryCatch(build_new_data(fit, pred_vars, id_map$tuning_pred_prefix),
                        error = function(e) NULL)
      if (is.null(parts)) {
        return(tags$div(style = "font-size:0.8em;color:#6c757d;",
                        "Enter values above to see a live estimate."))
      }
      nd <- parts$nd

      p90 <- tryCatch(lm_pred_interval(fit, nd, 0.90, mode),
                      error = function(e) NULL)
      if (is.null(p90)) {
        return(tags$div(style = "font-size:0.8em;color:#b02a37;",
                        "Prediction failed for the current inputs (often an unseen factor ",
                        "level once points are excluded)."))
      }

      is_log <- identical(mode, "loglin")
      est <- if (is_log) exp(p90[1, "fit"]) else p90[1, "fit"]
      lwr <- if (is_log) exp(p90[1, "lwr"]) else p90[1, "lwr"]
      upr <- if (is_log) exp(p90[1, "upr"]) else p90[1, "upr"]

      contrib <- tryCatch({
        tt <- stats::delete.response(stats::terms(fit))
        mf <- stats::model.frame(tt, nd, xlev = fit$xlevels)
        X  <- stats::model.matrix(tt, mf, contrasts.arg = fit$contrasts)
        cf <- stats::coef(fit)
        keep <- intersect(names(cf), colnames(X))
        stats::setNames(as.numeric(X[1, keep]) * as.numeric(cf[keep]), keep)
      }, error = function(e) NULL)

      fmt <- function(v) formatC(v, format = "f", big.mark = ",", digits = 2)
      n_used <- nrow(fit$model)
      n_all  <- nrow((rv[[id_map$selected_model_key]])$model)

      infl_tbl <- NULL
      if (!is.null(contrib) && length(contrib)) {
        nm      <- unbt(names(contrib))
        is_int  <- nm == "(Intercept)"
        non_int <- contrib[!is_int]
        denom   <- sum(abs(non_int))
        rows <- lapply(seq_along(contrib), function(i) {
          share <- if (is_int[i] || denom == 0) NA_real_ else abs(contrib[i]) / denom
          zebra <- if (i %% 2 == 0) "background:#eef1f4;" else ""
          tags$tr(
            tags$td(style = paste0("padding:3px 10px;", zebra), nm[i]),
            tags$td(style = paste0("padding:3px 10px;text-align:right;", zebra), fmt(contrib[i])),
            if (is_log) tags$td(style = paste0("padding:3px 10px;text-align:right;", zebra),
                                paste0("x", formatC(exp(contrib[i]), format = "f", digits = 3))),
            tags$td(style = paste0("padding:3px 10px;text-align:right;", zebra),
                    if (is.na(share)) "—" else paste0(formatC(100 * share, format = "f", digits = 1), "%"))
          )
        })
        infl_tbl <- tags$table(
          style = "font-size:0.78em;border-collapse:collapse;margin-top:8px;",
          tags$thead(tags$tr(
            tags$th(style = "padding:3px 10px;text-align:left;border-bottom:1px solid #dee2e6;",  "Term"),
            tags$th(style = "padding:3px 10px;text-align:right;border-bottom:1px solid #dee2e6;",
                    if (is_log) "b x  (log)" else "Contribution"),
            if (is_log) tags$th(style = "padding:3px 10px;text-align:right;border-bottom:1px solid #dee2e6;", "Factor"),
            tags$th(style = "padding:3px 10px;text-align:right;border-bottom:1px solid #dee2e6;", "Share")
          )),
          tags$tbody(rows)
        )
      }

      tags$div(
        tags$div(style = "margin-top:4px;",
                 tags$span(style = "font-size:0.78em;font-weight:700;letter-spacing:0.06em;text-transform:uppercase;color:#6c757d;margin-right:10px;",
                           if (is_log) "Estimate (median)" else "Estimate"),
                 tags$span(style = "font-size:1.25em;font-weight:700;", fmt(est))),
        tags$div(style = "font-size:0.78em;color:#6c757d;margin-top:2px;",
                 paste0("90% PI: ", fmt(lwr), "  to  ", fmt(upr),
                        "   |   fit on ", n_used, " of ", n_all, " points")),
        infl_tbl,
        if (is_log) tags$div(style = "font-size:0.7em;color:#6c757d;margin-top:6px;",
                             "Log-linear: terms are multiplicative. Factor = exp(b x); ",
                             "Share = fraction of the total non-intercept log contribution.")
      )
    })

    output[[id_map$tuning_equation]] <- renderUI({
      fit <- live_tuning_fit()
      if (is.null(fit)) {
        reason <- .refit_block_reason(isolate(rv[[id_map$selected_model_key]]),
                                      rv[[id_map$ui_exclusions_key]])
        msg <- if (!is.null(reason)) {
          paste0("Refit unavailable — every observation for ",
                 paste(reason, collapse = "; "),
                 " has been excluded, so that predictor no longer varies and ",
                 "its effect cannot be estimated. Re-include at least one of ",
                 "those points to refit.")
        } else {
          "Refit unavailable — too few observations remain after exclusions to fit the model."
        }
        return(tags$p(style = "color:#999; font-style:italic;", msg))
      }
      fmt_eq <- function(x, digits = 4) {
        if (is.na(x)) return("0")
        ax <- abs(x)
        if (ax == 0) return("0")
        if (ax < 1e-3 || ax >= 1e6) formatC(x, format = "e", digits = digits)
        else                        formatC(x, format = "f", digits = digits)
      }

      dep_var <- input$ols_response %||% all.vars(formula(fit))[1]
      log1p_cols <- character(0)
      orig_model <- isolate(rv[[id_map$selected_model_key]])
      if (!is.null(orig_model)) {
        attr_log1p <- attr(orig_model, "log1p_cols")
        if (!is.null(attr_log1p)) log1p_cols <- attr_log1p
      }

      eq_parts <- cer_equation(coef(fit), fit$model, fit$xlevels, dep_var, mode, log1p_cols, fmt_eq,
                               cer_source_cols())
      eq_str   <- eq_parts$text
      eq_latex <- eq_parts$latex

      pc_val <- primary_color()
      tags$div(class = "c-eq-card",
        style = paste0("background:", pc_val, "15; border-radius:8px; padding:14px 18px; ",
                       "border-left:4px solid ", pc_val, ";"),
        if (!is.null(eq_latex) && nzchar(eq_latex)) {
          tags$span(
            class = "katex-eq",
            `data-latex` = eq_latex,
            style = "font-size:1.05em; display:block; word-break:break-word; color:#222;",
            tags$code(style = "font-family:monospace; color:#222; background:transparent; font-size:1em;", eq_str)
          )
        } else {
          tags$div(style = "font-family:monospace; font-size:1.05em; color:#222; word-break:break-word;", eq_str)
        }
      )
    })

    .side_stats_ui <- function() {
      fit <- live_tuning_fit()
      model_for_count <- selected_model_fn()
      if (is.null(model_for_count) || is.null(model_for_count$model)) return(NULL)
      n_excl <- length(rv[[id_map$ui_exclusions_key]] %||% character(0))
      pc <- primary_color()
      side_pill <- function(label, value) {
        tags$div(class = "c-pill-stat", style = paste0("display:block; margin-bottom:6px; padding:5px 10px; ",
                                "background:", pc, "15; border-radius:6px; ",
                                "text-align:center;"),
                 tags$div(style = paste0("font-size:9px; color:", pc,
                                         "; font-weight:700; text-transform:uppercase; ",
                                         "letter-spacing:0.04em;"),
                          label),
                 tags$div(style = "font-size:13px; font-weight:700; margin-top:1px; line-height:1.1;",
                          as.character(value)))
      }
      if (is.null(fit)) return(NULL)
      s <- summary(fit)
      tagList(
        side_pill("R²",          round(s$r.squared, 4)),
        side_pill("Adj. R²",     round(s$adj.r.squared, 4)),
        side_pill("Std. Error",  round(s$sigma, 4)),
        side_pill("Included",    nrow(fit$model)),
        side_pill("Excluded",    n_excl)
      )
    }
    output[[id_map$tuning_side_stats_a]] <- renderUI({ .side_stats_ui() })
    output[[id_map$tuning_side_stats_b]] <- renderUI({ .side_stats_ui() })

  }

  register_cer_outputs(
    mode               = "loglin",
    rv                 = rv,
    input              = input,
    output             = output,
    session            = session,
    selected_model_fn  = selected_log_linear_model,
    filtered_models_fn = filtered_log_linear_models_data
  )

  linear_result_window <- function(model_vars_second, nbins) modalDialog(
  
    title = tags$div(class = "c-modal-head",
      tags$span(class = "c-modal-title-text", "Model Summary (Linear)"),
      tags$div(class = "c-modal-actions",
        actionButton("linear_pin_compare", "Pin to compare", icon = icon("thumbtack")),
        downloadButton("report_linear", "CER report", icon = icon("file-lines")),
        downloadButton("export_linear_excel", "Export Statistics", icon = icon("file-excel"), class = "btn-primary"))
    ),
    
    div(
      tabBox(width = 12,
             tabPanel("Fit Statistics",
                      tags$br(),
                      uiOutput("fit_summary_second")
             ),
             tabPanel("Actuals vs Predicted",
                      withSpinner(plotlyOutput("actuals_vs_predicted_second", width="95%", height = 450),
                                  color = getOption("spinner.color", default = primary_color()),
                                  type = 1)
             ),
             tabPanel("Residuals vs Predicted",
                      withSpinner(plotlyOutput("residual_vs_predicted_second", width="95%", height = 450),
                                  color = getOption("spinner.color", default = primary_color()),
                                  type = 1)
             ),
             tabPanel("Residuals vs Predictors",
                      withSpinner(plotlyOutput("residual_vs_predictors_second", width="95%", height = 450),
                                  color = getOption("spinner.color", default = primary_color()),
                                  type = 1),
                      selectInput("residual_predictor_second", "Select Predictor", choices = model_vars_second)
             ),
             tabPanel("Residual Normality",
                      withSpinner(plotlyOutput("residual_normality_second", width="95%", height = 450),
                                  color = getOption("spinner.color", default = primary_color()),
                                  type = 1),
                      sliderInput("residual_nbins_second", "Histogram Bins:", value = nbins, min = 5, max = 50)
             ),
             tabPanel("CER Tuning",
                      tags$script(HTML("
                        $(document).off('click.linear_excl').on('click.linear_excl', '.linear-excl-check', function() {
                          var label = $(this).data('label');
                          var checked = $(this).is(':checked');
                          Shiny.setInputValue('linear_influence_checkbox',
                            {label: label, checked: checked},
                            {priority: 'event'});
                        });
                        if (typeof Shiny !== 'undefined' && !Shiny._ceresDtExclHandlerRegistered) {
                          window._ceresExclState = window._ceresExclState || {};
                          window._ceresApplyExcl = function(cls) {
                            var set = window._ceresExclState[cls] || {};
                            document.querySelectorAll('.' + cls).forEach(function(cb) {
                              cb.checked = !!set[cb.getAttribute('data-label')];
                            });
                          };
                          Shiny.addCustomMessageHandler('ceres_dt_excl_update', function(msg) {
                            var labels = (msg && msg.labels) ? msg.labels : [];
                            var cls = (msg && msg.cls) ? msg.cls : '';
                            if (!cls) return;
                            var set = {};
                            for (var i = 0; i < labels.length; i++) set[String(labels[i])] = true;
                            window._ceresExclState[cls] = set;
                            window._ceresApplyExcl(cls);
                          });
                          // Re-apply on every DataTables redraw so rows that were on a
                          // different page get the correct checkbox state when paged in.
                          $(document).on('draw.dt', function() {
                            for (var c in window._ceresExclState) window._ceresApplyExcl(c);
                          });
                          Shiny._ceresDtExclHandlerRegistered = true;
                        }
                      ")),
                      tabsetPanel(
                        id = "linear_outliers_view",
                        tabPanel("Exclusions",
                                 tags$br(),
                                 tags$div(style = "position:relative;",
                                          withSpinner(plotlyOutput("linear_tuning_plot", width = "100%", height = 400),
                                                      color = getOption("spinner.color", default = primary_color()),
                                                      type = 1),
                                          tags$div(style = "position:absolute; top:120px; right:45px; z-index:10; max-width:130px;",
                                                   uiOutput("linear_tuning_side_stats_tuning"))
                                 )
                        ),
                        tabPanel("Outliers",
                                 tags$br(),
                                 tags$div(style = "position:relative;",
                                          withSpinner(plotlyOutput("linear_influence_plot", width = "100%", height = 400),
                                                      color = getOption("spinner.color", default = primary_color()),
                                                      type = 1),
                                          tags$div(style = "position:absolute; top:120px; right:45px; z-index:10; max-width:130px;",
                                                   uiOutput("linear_tuning_side_stats_excl"))
                                 )
                        )
                      ),
                      fluidRow(
                        column(12,
                               tags$div(
                                 style = "font-size:0.7em; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#6c757d; margin-bottom:6px;",
                                 "Equation"
                               ),
                               uiOutput("linear_tuning_equation")
                        )
                      ),
                      fluidRow(
                        column(12,
                               tags$details(
                                 style = "margin-top:12px; margin-bottom:4px;",
                                 tags$summary(
                                   style = "font-size:0.7em; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#6c757d; cursor:pointer; list-style:none; display:flex; align-items:center; gap:6px; margin-bottom:6px;",
                                   tags$span(style = "font-size:1.1em;", "▶"),
                                   "Live Prediction"
                                 ),
                                 tags$div(
                                   style = "border:1px solid #dee2e6; border-radius:6px; padding:12px 14px; background:#f8f9fa;",
                                   tags$div(
                                     style = "display:flex; align-items:baseline; gap:12px; flex-wrap:wrap; margin-bottom:10px;",
                                     tags$span(style = "font-size:0.78em; color:#6c757d;",
                                               "Updates live as points are excluded — no Refit needed."),
                                     actionLink("linear_tuning_pred_sync", "copy from Predictions",
                                                style = "font-size:0.78em; font-weight:600;")
                                   ),
                                   uiOutput("linear_tuning_pred_inputs"),
                                   uiOutput("linear_tuning_prediction")
                                 )
                               )
                        )
                      ),
                      tags$br(),
                      fluidRow(
                        column(12, align = "left", class = "c-actions",
                               actionButton("linear_tuning_push", "Refit",
                                            icon = icon("share-from-square"),
                                            class = "btn-primary",
                                            style = "display:block; width:95px; margin-bottom:6px;"),
                               actionButton("linear_reset_exclusions", "Reset",
                                            icon = icon("undo"),
                                            style = "display:block; width:95px;")
                        )
                      ),
                      tags$br(),
                      tags$details(
                        style = "margin-bottom:16px;",
                        tags$summary(
                          style = "font-size:0.7em; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#6c757d; cursor:pointer; list-style:none; display:flex; align-items:center; gap:6px; margin-bottom:6px;",
                          tags$span(style = "font-size:1.1em;", "\u25b6"),
                          "Data Table"
                        ),
                        DT::dataTableOutput("linear_influence_table"),
                        tags$br(),
                        fluidRow(
                          column(12, align = "left",
                                 actionButton("linear_save_exclusions", HTML("\u00a0Save Filtered Dataset"),
                                              icon = icon("floppy-disk"))
                          )
                        )
                      )
             ),
             tabPanel("Predictions",
                      tags$br(),
                      DT::dataTableOutput("linear_predict_table", fill = FALSE),
                      tags$div(style = "margin-top: 10px;"),
                      fluidRow(
                        column(width = 4,
                               uiOutput("linear_predictions")
                        ),
                        column(width = 8,
                               tabsetPanel(
                                 tabPanel("Prediction Distribution",
                                          tags$br(),
                                          withSpinner(
                                            plotlyOutput("linear_dist_plot", height = 350),
                                            color = getOption("spinner.color", default = primary_color()),
                                            type = 1
                                          ),
                                          tags$div(
                                            style = "margin-top:12px; padding:0 4px;",
                                            sliderInput(
                                              "linear_pi_level",
                                              label = NULL,
                                              min   = 50, max = 99, value = 95, step = 1,
                                              ticks = FALSE, post = "%",
                                              width = "100%"
                                            ),
                                            tags$div(
                                              style = "position:relative; margin-top:2px; height:16px; font-size:10px; color:#aaa; user-select:none;",
                                              tags$span("50%", style = "position:absolute; left:0%;"),
                                              tags$span("68%", style = "position:absolute; left:36.7%; transform:translateX(-50%);"),
                                              tags$span("95%", style = "position:absolute; left:91.8%; transform:translateX(-50%);"),
                                              tags$span("99%", style = "position:absolute; right:0%;")
                                            )
                                          ),
                                          uiOutput("linear_dist_stats")
                                 ),
                                 tabPanel("Local Attributions",
                                          tags$br(),
                                          withSpinner(plotlyOutput("linear_breakdown", height = 400),
                                                      color = getOption("spinner.color", default = primary_color()),
                                                      type = 1)
                                 )
                               )
                        )
                      )
             )
      )
    ),
    size = c("xl"), easyClose = TRUE, footer = "Click Outside to Close.", fade = TRUE)

  observeEvent(rv$model_list_linear_simple, ceres_warm_result_window("linear", linear_result_window), ignoreInit = TRUE)

  observeEvent(input$selected_linear_simple_models_rows_selected, {
    model_second <- selected_linear_model()
    req(model_second)
    rv$linear_selected_model <- model_second

    model_vars_second <- setdiff(names(model_second$model)[-1], "(weights)")

    n_par_lin   <- length(coef(model_second))
    row_lbl_lin <- rownames(model_second$model)
    if (is.null(row_lbl_lin)) row_lbl_lin <- as.character(seq_len(nrow(model_second$model)))
    row_lbl_lin <- make.unique(row_lbl_lin)
    rv$linear_influence_data <- data.frame(
      row_label    = row_lbl_lin,
      cooks_d      = as.numeric(cooks.distance(model_second)),
      leverage     = as.numeric(hatvalues(model_second)),
      std_residual = as.numeric(rstandard(model_second)),
      stringsAsFactors = FALSE
    )
    rv$linear_influence_n_par <- n_par_lin

    p_col <- primary_color()
    
    txt_col <- "#FFFFFF !important"
    
    if (isTRUE(input$dark_mode)) {
      txt_col <- "#000000 !important"
    }
    
    
    ceres_show_result_window("linear", linear_result_window, model_vars_second, max(5L, min(50L, floor(sqrt(length(model_second$residuals))))))
  })

  register_cer_outputs(
    mode               = "linear",
    rv                 = rv,
    input              = input,
    output             = output,
    session            = session,
    selected_model_fn  = selected_linear_model,
    filtered_models_fn = filtered_linear_models_data_second
  )

  observeEvent(input$ols_predictors,{
    
    if (length(input$ols_predictors) < 2){
      updateSelectInput(session, "ols_penalty", choices = "None", selected = "None")
    } else {
      updateSelectInput(session, "ols_penalty", choices = c("None", "LASSO", "Ridge", "Elastic Net"), selected = input$ols_penalty)
    }
    
  })
  
  observeEvent(input$ols_response, {
    req(input$ols_response)
    cur_data <- rv$dataset_library[[input$data_select_cer_generation]]
    req(!is.null(cur_data))

    all_vars <- colnames(cur_data)
    available_x <- setdiff(all_vars, input$ols_response)
    
    current_x <- input$ols_predictors
    
    new_selection <- setdiff(current_x, input$ols_response)
    
    updateSelectInput(session, "ols_predictors", 
                      choices = available_x, 
                      selected = new_selection)
  })
  
  observeEvent(input$ols_select_all, {
    req(input$ols_response)
    cur_data <- rv$dataset_library[[input$data_select_cer_generation]]
    req(!is.null(cur_data))

    valid_predictors <- setdiff(colnames(cur_data), input$ols_response)
    updateSelectInput(session, "ols_predictors",
                      selected = valid_predictors)
  })
  
  observeEvent(input$ols_train,{
    shinyjs::disable("ols_train")
    shinyjs::html("ols_train", "Running...")
    on.exit({ shinyjs::enable("ols_train"); shinyjs::html("ols_train", "Run OLS Model") })
    req(rv$active_dataset)
    sync_cer_generation_data()
    req(input$ols_predictors)
    
    rv$ols_results <- NULL

    if (!input$ols_response %in% colnames(rv$active_dataset)) {
      showNotification("Response variable not found in current dataset. Please re-select.", type = "error")
      return(NULL)
    }
    stale_preds <- setdiff(input$ols_predictors, colnames(rv$active_dataset))
    if (length(stale_preds) > 0) {
      showNotification(
        paste0("Skipping ", length(stale_preds), " predictor(s) not in current dataset: ",
               paste(stale_preds, collapse = ", ")),
        type = "warning"
      )
    }
    valid_preds <- intersect(input$ols_predictors, colnames(rv$active_dataset))
    if (length(valid_preds) == 0) {
      showNotification("No valid predictors in current dataset.", type = "error")
      return(NULL)
    }

    y <- rv$active_dataset[, input$ols_response]
    x <- rv$active_dataset[, valid_preds, drop = FALSE]
    
    nzv_idx <- ceres_ns("caret")$nearZeroVar(x)
    if (length(nzv_idx) > 0) {
      showNotification("Removing zero-variance predictors.", type = "warning")
      x <- x[, -nzv_idx, drop = FALSE]
      if (ncol(x) == 0) {
        showNotification("All predictors have zero variance. Aborting.", type = "error")
        return(NULL)
      }
    }
    
    numeric_x <- x[, vapply(x, is.numeric, logical(1L)), drop = FALSE]
    if (ncol(numeric_x) > 1) {
      cor_matrix <- cor(numeric_x)
      high_cor <- ceres_ns("caret")$findCorrelation(cor_matrix, cutoff = 0.95)
      if (length(high_cor) > 0) {
        showNotification("Removing highly correlated predictors.", type = "warning")
        cols_to_remove <- colnames(numeric_x)[high_cor]
        x <- x[, !colnames(x) %in% cols_to_remove, drop = FALSE]
      }
    }
    
    data <- data.frame(y, x, check.names = FALSE)
    complete_rows <- complete.cases(data)
    data <- data[complete_rows, , drop = FALSE]

    if (input$ols_holdout == OLS_HOLDOUT_CHOICES[1] || input$ols_holdout %in% OLS_HOLDOUT_CHOICES[5:7]){
      train_index <- seq_along(data$y)
      y_test <- data$y
      data_test <- data
      data_train <- data

    } else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[2]){
      train_index <- withr::with_seed(123, ceres_ns("caret")$createDataPartition(data$y, p = 0.8)[[1]])
      y_test <- data$y[-train_index]
      data_test <- data[-train_index,]
      data_train <- data[train_index,]

    } else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[3]){
      train_index <- withr::with_seed(123, ceres_ns("caret")$createDataPartition(data$y, p = 0.5)[[1]])
      y_test <- data$y[-train_index]
      data_test <- data[-train_index,]
      data_train <- data[train_index,]

    } else {
      test_index <- match(rv$manual_test_set, which(complete_rows))
      test_index <- test_index[!is.na(test_index)]

      if (length(test_index) == 0){
        train_index <- seq_along(data$y)
        y_test <- data$y
        data_test <- data
        data_train <- data
      } else if (length(test_index) >= nrow(data)) {
        train_index <- seq_along(data$y)
        y_test <- data$y
        data_test <- data
        data_train <- data
        showNotification(
          "Manual test set contains every usable row — no rows left to train on. Training on all data instead.",
          type = "warning", duration = 8
        )
      } else {
        all_index <- seq_along(data$y)
        train_index <- all_index[-test_index]
        y_test <- data$y[-train_index]
        data_test <- data[-train_index,]
        data_train <- data[train_index,]
      }
    }

    if (input$ols_interact){
      ols_formula <- as.formula("y ~ .^2")
    } else {
      ols_formula <- as.formula("y ~ .")
    }

    cv_indices <- withr::with_seed(123, ceres_ns("caret")$createMultiFolds(data_train$y, k = 10, times = 5))

    if (input$ols_penalty != "None") {
      n_design_cols <- tryCatch(
        ncol(model.matrix(ols_formula, data = data_train)[, -1, drop = FALSE]),
        error = function(e) 0L
      )
      if (is.null(n_design_cols) || n_design_cols < 2L) {
        showNotification(
          paste0("Regularization (", input$ols_penalty, ") needs at least 2 predictor ",
                 "columns; only ", n_design_cols, " remain after the collinearity check. ",
                 "Select more predictors, or use penalty = None for a single predictor."),
          type = "error", duration = 10
        )
        return()
      }
    }

    if (input$ols_penalty == "None"){
      cv_args <- if (input$ols_holdout == OLS_HOLDOUT_CHOICES[5]) {
        list(method = "cv", number = 5)
      } else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[6]) {
        list(method = "cv", number = 10)
      } else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[7]) {
        list(method = "LOOCV")
      } else {
        list(method = "repeatedcv", index = cv_indices)
      }
      withProgress(message = 'Performing Cross Validation', value = 1, {
        model <- do.call(ceres_lm_cv, c(list(formula = ols_formula, data = data_train, seed = 23L), cv_args))
      })
      
      yhat_test <- predict(model, data_test)
      test_error <- ceres_ns("caret")$RMSE(yhat_test, data_test$y)
      yhat_train <- predict(model, data_train)
      train_error <- ceres_ns("caret")$RMSE(yhat_train, data_train$y)
      
      results <- matrix(NA, nrow = 1, ncol = 3)
      results[1,1] <- if (!is.null(model$resample)) mean(model$resample$RMSE)
                      else min(model$results$RMSE)
      results[1,2] <- train_error
      results[1,3] <- if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[5:7]) NA else test_error
      colnames(results) <- c("CV RMSE", "Train RMSE", "Test RMSE")
      row.names(results) <- "OLS Regression"
      rv$ols_coeffs <- coef(model$finalModel)
      
    } else if (input$ols_penalty == "LASSO") {

      len <- 100
      init <- ceres_ns("glmnet")$glmnet(Matrix::as.matrix(model.matrix(ols_formula, data = data_train)[,-1]), as.matrix(data_train$y), family = "gaussian", nlambda = len+2, alpha = 1)
      lambda <- unique(init$lambda)
      lambda <- lambda[-c(1, length(lambda))]
      lambda <- lambda[1:min(length(lambda), len)]

      lasso_grid <- expand.grid(alpha = 1, lambda = lambda)

      withProgress(message = 'Performing Cross Validation', value = 1, {
        model <- ceres_glmnet_cv(ols_formula, data_train, alpha = 1, lambda = lasso_grid$lambda,
                                 method = "repeatedcv", index = cv_indices[1:25], seed = 23L)
      })
      
      yhat_test <- predict(model, data_test)
      test_error <- ceres_ns("caret")$RMSE(yhat_test, data_test$y)
      yhat_train <- predict(model, data_train)
      train_error <- ceres_ns("caret")$RMSE(yhat_train, data_train$y)
      
      if(input$ols_holdout == OLS_HOLDOUT_CHOICES[3]){
        rv$lasso_test_residuals <- abs(data_test$y - yhat_test)
      }
      
      results <- matrix(NA, nrow = 1, ncol = 5)
      results[1,1] <- as.numeric(model$bestTune$alpha)
      results[1,2] <- as.numeric(model$bestTune$lambda)
      results[1,3] <- mean(model$resample$RMSE)
      results[1,4] <- train_error
      results[1,5] <- if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[5:7]) NA else test_error
      colnames(results) <- c("alpha","lambda", "CV RMSE", "Train RMSE", "Test RMSE")
      row.names(results) <- "LASSO Regression"
      rv$ols_coeffs <- coef(model$finalModel, model$finalModel$lambdaOpt)
      
      
    } else if (input$ols_penalty == "Ridge") {
      
      len <- 100
      init <- ceres_ns("glmnet")$glmnet(Matrix::as.matrix(model.matrix(ols_formula, data = data_train)[,-1]), as.matrix(data_train$y), family = "gaussian", nlambda = len+2, alpha = 0)
      lambda <- unique(init$lambda)
      lambda <- lambda[-c(1, length(lambda))]
      lambda <- lambda[1:min(length(lambda), len)]
      
      ridge_grid <- expand.grid(alpha = 0, lambda = lambda)

      withProgress(message = 'Performing Cross Validation', value = 1, {
        model <- ceres_glmnet_cv(ols_formula, data_train, alpha = 0, lambda = ridge_grid$lambda,
                                 method = "cv", number = 5L, seed = 23L)
      })
      
      yhat_test <- predict(model, data_test)
      test_error <- ceres_ns("caret")$RMSE(yhat_test, data_test$y)
      yhat_train <- predict(model, data_train)
      train_error <- ceres_ns("caret")$RMSE(yhat_train, data_train$y)
      
      if(input$ols_holdout == OLS_HOLDOUT_CHOICES[3]){
        rv$ridge_test_residuals <- abs(data_test$y - yhat_test)
      }
      
      results <- matrix(NA, nrow = 1, ncol = 5)
      results[1,1] <- as.numeric(model$bestTune$alpha)
      results[1,2] <- as.numeric(model$bestTune$lambda)
      results[1,3] <- mean(model$resample$RMSE)
      results[1,4] <- train_error
      results[1,5] <- if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[5:7]) NA else test_error
      colnames(results) <- c("alpha","lambda", "CV RMSE", "Train RMSE", "Test RMSE")
      row.names(results) <- "Ridge Regression"
      rv$ols_coeffs <- coef(model$finalModel, model$finalModel$lambdaOpt)
      
    } else if (input$ols_penalty == "Elastic Net") {
      
      len <- 20
      init <- ceres_ns("glmnet")$glmnet(Matrix::as.matrix(model.matrix(ols_formula, data = data_train)[,-1]), as.matrix(data_train$y), family = "gaussian", nlambda = len+2, alpha = 0.5)
      lambda <- unique(init$lambda)
      lambda <- lambda[-c(1, length(lambda))]
      lambda <- lambda[1:min(length(lambda), len)]

      alpha_seq  <- seq(from = 0, to = 1, by = 0.1)
      n_alpha    <- length(alpha_seq)
      best_rmse  <- Inf
      best_model <- NULL

      withr::with_seed(23, withProgress(message = 'Performing Cross Validation', value = 0, {
        for (i in seq_along(alpha_seq)) {
          a        <- alpha_seq[i]
          grid_i   <- expand.grid(alpha = a, lambda = lambda)
          model_i  <- suppressWarnings(ceres_glmnet_cv(ols_formula, data_train, alpha = a, lambda = grid_i$lambda,
                                                       method = "repeatedcv", index = cv_indices[1:17]))
          rmse_i   <- min(model_i$results$RMSE)
          if (rmse_i < best_rmse) {
            best_rmse  <- rmse_i
            best_model <- model_i
          }
          incProgress(1 / n_alpha, detail = paste0(""))
        }
      }))

      model <- best_model
      
      yhat_test <- predict(model, data_test)
      test_error <- ceres_ns("caret")$RMSE(yhat_test, data_test$y)
      yhat_train <- predict(model, data_train)
      train_error <- ceres_ns("caret")$RMSE(yhat_train, data_train$y)
      
      if(input$ols_holdout == OLS_HOLDOUT_CHOICES[3]){
        rv$elastic_test_residuals <- abs(data_test$y - yhat_test)
      }
      
      results <- matrix(NA, nrow = 1, ncol = 5)
      results[1,1] <- as.numeric(model$bestTune$alpha)
      results[1,2] <- as.numeric(model$bestTune$lambda)
      results[1,3] <- mean(model$resample$RMSE)
      results[1,4] <- train_error
      results[1,5] <- if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[5:7]) NA else test_error
      colnames(results) <- c("alpha","lambda", "CV RMSE", "Train RMSE", "Test RMSE")
      row.names(results) <- "Elastic Net Regression"
      rv$ols_coeffs <- coef(model$finalModel, model$finalModel$lambdaOpt)
      
    }
    
    names(yhat_train) <- row.names(data_train)
    names(yhat_test) <- row.names(data_test)
    
    rv$train_index <- train_index
    rv$ols_train_index <- train_index
    rv$ols_train_y     <- data_train$y
    rv$ols_data_rownames <- rownames(data)
    rv$ols_model <- model
    rv$ols_results <- results
    rv$ols_sets <- list("yhat_train" = yhat_train, "yhat_test" = yhat_test, "y_train" = data_train$y, "y_test" = data_test$y)
    rv$ols_train_data <- data_train

    if (input$ols_penalty != "None" && input$ols_holdout != OLS_HOLDOUT_CHOICES[3]) {
      showNotification(
        "Prediction intervals are only available with 50% holdout (conformal inference). The Predictions tab will show an estimate only.",
        type = "warning", duration = 10
      )
    }
    
    
  })
  
  output$ols_actuals_vs_predicted <- renderPlotly({
    req(rv$ols_model)
    req(rv$ols_sets)
    
    yhat <- rv$ols_sets[["yhat_train"]]
    y <- rv$ols_sets[["y_train"]]
    yhat_test <- rv$ols_sets[["yhat_test"]]
    y_test <- rv$ols_sets[["y_test"]]
    
    p <- plot_ly(x = yhat, y = y, type = "scatter", mode = "markers", marker = list(color = primary_color(), size = 10), name = "Training Data",
                 text = paste0("Row Name: ", names(yhat), "<br>Actual: ", round(y, 3), "<br>Predicted: ", round(yhat, 3)),
                 hoverinfo = "text") %>%
      add_trace(inherit = FALSE, x = sort(y), y = sort(y), type = "scatter", mode = "lines", line = list(color = "#000000", dash = 'dash', width = 2), name = "y = x", hoverinfo = "none") %>%
      layout(title = input$ols_response, xaxis = list(title = "Predicted Values"), yaxis = list(title = "Actuals")) %>%
      config(displayModeBar = FALSE)

    if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[2:4]){
      p <- p %>% add_trace(inherit = FALSE, x = yhat_test, y = y_test, type = "scatter", mode = "markers", marker = list(color = graph_color_1, size = 10), name = "Test Data",
                           text = paste0("Row Name: ", names(yhat_test), "<br>Actual: ", round(y_test, 3), "<br>Predicted: ", round(yhat_test, 3)),
                           hoverinfo = "text")
    }

    p

  })
  
  output$ols_residuals_vs_predicted <- renderPlotly({
    req(rv$ols_model)
    req(rv$ols_sets)
    
    yhat <- rv$ols_sets[["yhat_train"]]
    y <- rv$ols_sets[["y_train"]]
    train_residuals <- y - yhat
    yhat_test <- rv$ols_sets[["yhat_test"]]
    y_test <- rv$ols_sets[["y_test"]]
    test_residuals <- y_test - yhat_test
    
    p <- plot_ly(x = yhat, y = train_residuals, type = "scatter", mode = "markers", marker = list(color = primary_color(), size = 10), name = "Training Data",
                 text = paste0("Row Name: ", names(yhat), "<br>Predicted: ", round(yhat, 3), "<br>Residual: ", round(train_residuals, 3)),
                 hoverinfo = "text") %>%
      layout(title = paste("Residuals vs Predicted:", input$ols_response), xaxis = list(title = "Predicted"), yaxis = list(title = "Residuals", zeroline = FALSE),
             shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                line = list(color = "rgba(100,116,139,0.7)", width = 1.5, dash = "dash")))) %>%
      config(displayModeBar = FALSE)

    if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[2:4]){
      p <- p %>% add_trace(inherit = FALSE, x = yhat_test, y = test_residuals, type = "scatter", mode = "markers", marker = list(color = graph_color_1, size = 10), name = "Test Data",
                           text = paste0("Row Name: ", names(yhat_test), "<br>Predicted: ", round(yhat_test, 3), "<br>Residual: ", round(test_residuals, 3)),
                           hoverinfo = "text")
    }

    p

  })
  
  output$ols_residuals_vs_predictors <- renderPlotly({
    req(rv$ols_model)
    req(rv$ols_sets)
    
    yhat <- rv$ols_sets[["yhat_train"]]
    y <- rv$ols_sets[["y_train"]]
    residuals <- y - yhat
    
    yhat_test <- rv$ols_sets[["yhat_test"]]
    y_test <- rv$ols_sets[["y_test"]]
    test_residuals <- y_test - yhat_test
    
    preds <- as.data.frame(rv$ols_model$trainingData[, -1, drop = FALSE])
    colnames(preds) <- unbt(unbt(colnames(preds)))
    row_lab <- if (!is.null(names(yhat))) names(yhat) else as.character(seq_along(residuals))
    pt_col  <- ifelse(residuals >= 0, "#2F6FE4", "#E0457B")

    panels <- lapply(colnames(preds), function(v) {
      raw <- preds[[v]]
      x <- suppressWarnings(as.numeric(raw))
      vals <- sort(unique(stats::na.omit(x)))
      is_bin <- length(vals) <= 2
      tick_txt <- if (is.factor(raw)) levels(raw)[vals] else format(vals)
      x_plot <- if (is_bin) x + withr::with_seed(1, stats::runif(length(x), -0.12, 0.12)) else x
      plot_ly(x = x_plot, y = residuals, type = "scatter", mode = "markers", showlegend = FALSE,
              marker = list(color = pt_col, size = 7, opacity = 0.78,
                            line = list(color = "rgba(255,255,255,0.9)", width = 1)),
              text = paste0("<b>", row_lab, "</b><br>", v, ": ", format(round(x, 4), big.mark = ","),
                            "<br>Residual: ", format(round(residuals, 3), big.mark = ",")),
              hoverinfo = "text") %>%
        layout(xaxis = c(list(title = list(text = v, font = list(size = 12)), zeroline = FALSE),
                         if (is_bin) list(tickvals = vals, ticktext = tick_txt, range = c(min(vals) - 0.6, max(vals) + 0.6)) else list()),
               yaxis = list(title = list(text = "Residual", font = list(size = 12)), zeroline = TRUE, zerolinewidth = 1.5))
    })

    n_col <- min(3L, length(panels))
    n_row <- ceiling(length(panels) / n_col)
    subplot(panels, nrows = n_row, shareY = TRUE, titleX = TRUE, titleY = TRUE,
            margin = c(0.035, 0.035, 0.09, 0.09)) %>%
      layout(title = list(text = "Training residuals by predictor"),
             hovermode = "closest",
             margin = list(t = 58, l = 64, r = 20, b = 56)) %>%
      config(displayModeBar = FALSE)
  })

  output$ols_residual_normality <- renderPlotly({
    req(rv$ols_model)
    model <- rv$ols_model
    
    epsilon <- residuals(model)

    fit <- density(epsilon)
    emp_normal <- dnorm(fit$x, mean = 0, sd = sd(epsilon))

    p1 <- plot_ly(x = epsilon, type = "histogram", name = "Resid. Histogram", nbinsx = max(5L, floor(sqrt(length(epsilon)))), marker = list(color = secondary_color(),line = list(color = primary_color(), width = 2))) %>%
      add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Resid. Density", inherit = FALSE, line = list(color = primary_color(), width = 2.5), fillcolor = paste0(primary_color(), "1A")) %>% 
      add_trace(x = fit$x, y = emp_normal, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Normal Density", inherit = FALSE, line = list(color = graph_color_2, width = 2, dash = "dash"), fillcolor = "rgba(0,0,0,0)") %>% 
      layout(title = "Histogram/Density Plot", xaxis = list(title = "Residuals"), yaxis = list(title = "Frequency"), yaxis2 = list(showticklabels = FALSE, showgrid = FALSE, overlaying = "y", side = "right")) %>%
      config(displayModeBar = FALSE)
    
    
    sorted_data <- sort(epsilon)
    n <- length(sorted_data)
    percentiles <- (seq_along(sorted_data) - 0.5)/length(sorted_data)
    theoretical_quantiles <- qnorm(percentiles)
    k <- 0.895/(sqrt(n) * (1 - 0.01/sqrt(n) + 0.85/n))
    LCL <- qnorm(pmax(percentiles - k, 1e-10))
    UCL <- qnorm(pmin(percentiles + k, 1 - 1e-10))
    
    equality_data <- data.frame(x = c(qnorm(0.25), qnorm(0.75)), y = as.numeric(quantile(epsilon,c(0.25, 0.75))))
    fit <- lm(y ~ x, data = equality_data)
    
    domain <- seq(from = min(theoretical_quantiles), to = max(theoretical_quantiles), length.out = 100)
    
    new_data <- data.frame(x = domain)
    yhat <- predict(fit, new_data)
    
    p2 <- plot_ly(x = theoretical_quantiles, y = sorted_data, type = "scatter", mode = "markers", marker = list(color = primary_color()), name = "Data") %>% 
      add_trace(x = domain, y = yhat, type = "scatter", mode = "lines", name = "Expected Quantiles", inherit = FALSE, line = list(color = graph_color_2, width = 2, dash = "dash")) %>%
      add_trace(x = UCL, y = sorted_data, type = "scatter", mode = "lines", name = "Upper 95% CI", inherit = FALSE, line = list(color = "rgba(100,116,139,0.6)", width = 1.2, dash = "dot")) %>%
      add_trace(x = LCL, y = sorted_data, type = "scatter", mode = "lines", name = "Lower 95% CI", inherit = FALSE, line = list(color = "rgba(100,116,139,0.6)", width = 1.2, dash = "dot")) %>%
      layout(title = "Q-Q Plot", xaxis = list(title = "Theoretical Normal Quantiles", range = range(theoretical_quantiles) + c(-0.35, 0.35)), yaxis = list(title = "Sample Quantiles")) %>%
      config(displayModeBar = FALSE)
    
    subplot(p1, p2, nrows = 1, titleX = TRUE, titleY = TRUE) %>% layout(showlegend = FALSE, title = "Residual Normality")
  })
  
  output$ols_cv_results <- renderDT({
    req(rv$ols_results)
    
    tmp <- rv$ols_results
    
    if(input$ols_holdout == "No (Train on all data)"){
      tmp[1,ncol(tmp)] <- NA
    }
    
    pen <- isolate(input$ols_penalty)
    cv_label <- if (pen == "LASSO") "Repeated 10-Fold CV (25 folds)"
                else if (pen == "Ridge") "5-Fold CV"
                else if (pen == "Elastic Net") "Repeated 10-Fold CV (17 folds)"
                else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[5]) "5-Fold CV"
                else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[6]) "10-Fold CV"
                else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[7]) "Leave-One-Out CV"
                else "5× Repeated 10-Fold CV"
    dt <- datatable(tmp, colnames = setNames(1, cv_label), options = list(dom = "t", ordering = FALSE)) %>%
      formatCurrency('CV RMSE',currency = "", interval = 3, mark = ",", digits = 3) %>%
      formatCurrency('Train RMSE',currency = "", interval = 3, mark = ",", digits = 3) %>%
      formatCurrency('Test RMSE',currency = "", interval = 3, mark = ",", digits = 3)
    
    if (isolate(input$ols_penalty) != "None"){
      dt <- dt %>% 
        formatCurrency('alpha',currency = "", interval = 3, mark = ",", digits = 5) %>%
        formatCurrency('lambda',currency = "", interval = 3, mark = ",", digits = 5)
    }
    
    dt
    
  })
  
  observeEvent(input$ols_cv_results_rows_selected,{
    
    update_dialog <- modalDialog(
      div(
        tabBox(width = 12,
               tabPanel("Cross-Validation Grid Search",
                        DTOutput("ols_cv_table")
               ),
               tabPanel("RMSE Distribution",
                        withSpinner(plotlyOutput("ols_cv_boxplot", height = 450),
                                    color = getOption("spinner.color", default = primary_color()),
                                    type = 1)
               ),
               tabPanel("RMSE by Fold",
                        fluidRow(
                          column(width = 6, 
                                 div(align = "center", h4("Cross-Validation by Fold")),
                                 DTOutput("ols_resample_table")  
                          ),
                          column(width = 6, 
                                 div(align = "center", h4("Held Out Data from Fold")),
                                 DTOutput("ols_single_fold_table") 
                          )
                        )
               )
        )
        
        
      ),
      title = "Cross Validation Results", size = c("xl"), easyClose = TRUE, footer = "", fade = TRUE)
    showModal(update_dialog)
    
  })
  
  output$ols_cv_table <- renderDT({
    req(rv$ols_model)
    
    dt <- datatable(rv$ols_model$results, rownames = FALSE, options = list(dom = "tip", scrollX = '400px')) %>%
      formatCurrency('RMSE',currency = "", interval = 3, mark = ",", digits = 5) %>%
      formatPercentage('Rsquared', interval = 3, digits = 3, mark = ",") %>%
      formatCurrency('MAE',currency = "", interval = 3, mark = ",", digits = 5) %>%
      formatCurrency('RMSESD',currency = "", interval = 3, mark = ",", digits = 3) %>%
      formatCurrency('RsquaredSD',currency = "", interval = 3, mark = ",", digits = 3) %>%
      formatCurrency('MAESD',currency = "", interval = 3, mark = ",", digits = 3)
    
    if(isolate(input$ols_penalty) != "None"){
      dt <- dt %>% 
        formatPercentage('alpha', interval = 3, digits = 3, mark = ",") %>%
        formatCurrency('lambda',currency = "", interval = 3, mark = ",", digits = 5)
    }
    
    dt
    
  }, server = FALSE)
  
  output$ols_cv_boxplot <- renderPlotly({
    req(rv$ols_model)
    validate(need(!is.null(rv$ols_model$resample),
                  "Per-fold RMSE is not available for Leave-One-Out CV."))

    data <- rv$ols_model$resample$RMSE
    
    boxplot <- plot_ly(x = data, name = "RMSE by Fold", type = "box", boxpoints = "all", jitter = 0.3, fillcolor = secondary_color(), pointpos = 0, boxmean = TRUE,
                       marker = list(color = secondary_color(), line = list(color = primary_color())),
                       line = list(color = primary_color()))
    
    histogram <- plot_ly(x = data, type = "histogram", name = "RMSE by Fold",
                         marker = list(color = secondary_color(), line = list(color = primary_color(), width = 2)))
    
    subplot(boxplot,histogram,nrows = 2, shareX = TRUE) %>%  layout(title = "\nFinal Model RMSE by Fold",
                                                                    xaxis = list(title = "", zeroline = FALSE),
                                                                    yaxis = list(title = "", zeroline = FALSE),
                                                                    showlegend = FALSE) %>% config(displayModeBar = FALSE)
  })
  
  output$ols_resample_table <- renderDT({
    req(rv$ols_model)
    validate(need(!is.null(rv$ols_model$resample),
                  "Per-fold results are not available for Leave-One-Out CV."))

    tmp <- rv$ols_model$resample
    row.names(tmp) <- rv$ols_model$resample$Resample
    tmp <- tmp[,-4]
    
    datatable(tmp, colnames = c("Fold and Repetition" = 1), selection = "single", options = list(dom = "tip")) %>%
      formatCurrency('RMSE',currency = "", interval = 3, mark = ",", digits = 5) %>%
      formatPercentage('Rsquared', interval = 3, digits = 3, mark = ",") %>%
      formatCurrency('MAE',currency = "", interval = 3, mark = ",", digits = 5)
    
  }, server = FALSE)
  
  
  output$ols_single_fold_table <- renderDT({
    req(rv$ols_model)
    req(!is.null(rv$ols_model$resample))
    req(input$ols_resample_table_rows_selected)

    tmp <- rv$ols_model$pred[which(rv$ols_model$pred$Resample == rv$ols_model$resample$Resample[input$ols_resample_table_rows_selected]),]
    tmp$rowIndex <- (rv$ols_data_rownames %||% rv$row_names)[rv$ols_train_index %||% rv$train_index][tmp$rowIndex]
    tmp <- tmp[,c("Resample","rowIndex", "pred", "obs")]
    colnames(tmp) <- c("Selected Row", "Name", "Predicted", "Actual")
    
    datatable(tmp, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = nrow(tmp))) %>%
      formatCurrency('Predicted',currency = "", interval = 3, mark = ",", digits = 5) %>%
      formatCurrency('Actual',currency = "", interval = 3, mark = ",", digits = 5)
    
    
  }, server = FALSE)
  
  
  output$ols_coeffs <- renderDT({
    req(rv$ols_model)
    req(rv$ols_coeffs)
    
    if(isolate(input$ols_penalty) == "None"){
      coeff_values <- summary(rv$ols_model$finalModel)$coefficients
      
      rownames(coeff_values) <- unbt(unbt(rownames(coeff_values)))
      coeff_values <- as.data.frame(coeff_values, check.names = FALSE)
      pv <- coeff_values[["Pr(>|t|)"]]
      coeff_values[["Pr(>|t|)"]] <- ifelse(is.na(pv), "—", ifelse(pv < 0.001, "< 0.001", sprintf("%.3f", pv)))

      dt <- datatable(coeff_values, colnames = c("Variables" = 1), selection = "none", options = list(dom = "t", pageLength = min(nrow(coeff_values), 25), ordering = FALSE,
                                                                                                         columnDefs = list(list(className = "dt-right", targets = 4)))) %>%
        formatCurrency('Estimate',currency = "", interval = 3, mark = ",", digits = 5) %>%
        formatCurrency('Std. Error',currency = "", interval = 3, mark = ",", digits = 5) %>%
        formatCurrency('t value',currency = "", interval = 3, mark = ",", digits = 5)
    } else {
      coeff_names <- names(rv$ols_coeffs)
      if(is.null(coeff_names)){
        coeff_names <- row.names(rv$ols_coeffs)
      }
      
      coeff_names <- unbt(unbt(coeff_names))
      
      coeff_values <- t(as.matrix(as.numeric(rv$ols_coeffs), nrow = 1, ncol = length(coeff_names)))
      coeff_values[which(coeff_values == 0)] <- NA
      colnames(coeff_values) <- coeff_names
      row.names(coeff_values) <- "Estimate"
      
      dt <- datatable(t(coeff_values), colnames = c("Variables" = 1), selection = "none", options = list(dom = "t", pageLength = ncol(coeff_values), ordering = FALSE)) %>%
        formatCurrency('Estimate',currency = "", interval = 3, mark = ",", digits = 5)
    }
    dt
  })
  
  output$ols_regularization <- renderPlotly({
    req(rv$ols_model)
    req(input$ols_penalty != "None")
    
    model <- rv$ols_model
    
    req(model$finalModel$lambda)
    
    lambda <- sort(unique(model$results$lambda), decreasing = TRUE)
    betas <- as.matrix(coef(model$finalModel, lambda))[-1, , drop = FALSE]
    coeffs <- coef(model$finalModel, model$finalModel$lambdaOpt)[-1,]
    lambda <- log(lambda)
    
    p <- plot_ly()
    
    for (i in seq_len(nrow(betas))){
      p <- p %>% add_trace(x = lambda, y = betas[i,], type = "scatter", mode = "lines", name = row.names(betas)[i])
    }
    
    p <- p %>% add_segments(x = log(model$finalModel$lambdaOpt), xend = log(model$finalModel$lambdaOpt), y = min(betas), yend = max(betas), name = "Optimal Lambda", line = list(color = "black", dash = "dash"))
    
    for (i in which(coeffs != 0)){
      p <- p %>% add_trace(x = log(model$finalModel$lambdaOpt), y = coeffs[i], type = "scatter", mode = "markers", showlegend = FALSE, name = names(coeffs)[i], text = paste0(names(coeffs)[i],": ",round(coeffs[i],5)), hoverinfo = 'text', marker = list(color = "black"))
    }
    
    p %>% layout(title = "Coefficient Regularization", xaxis = list(title = "log(lambda)"), yaxis = list(title = "Coefficients")) %>% config(displayModeBar = FALSE)
    
  })
  
  output$ols_coeffs_ui <- renderUI({
    if (identical(input$ols_penalty %||% "None", "None"))
      return(DT::dataTableOutput("ols_coeffs"))
    fluidRow(
      column(width = 5,
             DT::dataTableOutput("ols_coeffs")
      ),
      column(width = 7,  
             withSpinner(  
               plotlyOutput("ols_regularization", height = 450),  
               color = getOption("spinner.color", default = primary_color()),
               type = 1  
             )
      )
    )
  })
  
  
  ols_pred_ids <- function(vars) paste0("ols_pred_", make.unique(gsub("[^A-Za-z0-9_.]", "_", vars), sep = "_"))
  ols_new_data <- function() {
    vars <- input$ols_predictors
    ids  <- ols_pred_ids(vars)
    new_data <- data.frame()
    for (k in seq_along(vars)) {
      req(input[[ids[k]]])
      new_data[1, vars[k]] <- input[[ids[k]]]
    }
    new_data
  }
  ols_classical_pi <- function(model, new_data, level) {
    tt <- delete.response(model$terms)
    mf <- model.frame(tt, new_data, na.action = na.pass, xlev = model$xlevels)
    mm <- model.matrix(tt, mf, contrasts.arg = model$contrasts)
    mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
    predict(model$finalModel, as.data.frame(mm, check.names = FALSE),
            interval = "predict", level = level)
  }

  output$ols_predictions <- renderUI({
    req(input$ols_predictors)
    req(input$data_select_cer_generation, nzchar(input$data_select_cer_generation))
    df_ols <- rv$dataset_library[[input$data_select_cer_generation]]
    req(!is.null(df_ols), all(input$ols_predictors %in% colnames(df_ols)))

    output <- tagList()
    ids <- ols_pred_ids(input$ols_predictors)

    for (i in seq_along(input$ols_predictors)){
      output[[i]] <- tagList()
      col_i <- df_ols[[input$ols_predictors[i]]]
      if (is.numeric(col_i)){
        output[[i]][[1]] <- numericInput(ids[i], input$ols_predictors[i], { med <- median(col_i, na.rm = TRUE); if (med < 1) med else round(med, 0) })
      } else {
        output[[i]][[1]] <- selectInput(ids[i], input$ols_predictors[i], choices = levels(col_i))
      }

    }

    output
    
  })
  
  output$ols_predict_table <- renderDT({
    req(rv$ols_model)
    
    new_data <- ols_new_data()

    yhat <- predict(rv$ols_model, new_data)
    rv$ols_yhat <- yhat
    
    if (input$ols_penalty == "None") {
      classical <- tryCatch({
        cl <- ols_classical_pi(rv$ols_model, new_data, 0.9)
        c(cl[2], cl[3])
      }, error = function(e) c(NA_real_, NA_real_))
      
      results <- matrix(NA, nrow = 1, ncol = 3)
      results[1,1] <- yhat[1]
      results[1,2] <- classical[1]
      results[1,3] <- classical[2]
      
      colnames(results) <- c("Estimate", "90% Low PI", "90% High PI")
      row.names(results) <- "Ordinary LS Prediction"
      
      datatable(results, colnames = c("", "Estimate", "90% Low PI", "90% High PI"), options = list(dom = "t", ordering = FALSE), selection = "none") %>%
        formatCurrency('Estimate',currency = "", interval = 3, mark = ",", digits = 3) %>%
        formatCurrency('90% Low PI',currency = "", interval = 3, mark = ",", digits = 3) %>%
        formatCurrency('90% High PI',currency = "", interval = 3, mark = ",", digits = 3)
    } else if (input$ols_penalty == "LASSO") {
      
      if (input$ols_holdout == "Yes (Holdout 50% for Testing)"){
        lasso_test_residuals <- rv$lasso_test_residuals
        alpha <- 0.1
        req(length(lasso_test_residuals) > 0)
        quantile_index <- min(length(lasso_test_residuals), ceiling((1-alpha)*(length(lasso_test_residuals)+1)))
        margin_of_predictive_error <- sort(lasso_test_residuals)[quantile_index]
        lower_bound <- yhat - margin_of_predictive_error
        upper_bound <- yhat + margin_of_predictive_error
        
        results <- matrix(NA, nrow = 1, ncol = 3)
        results[1,1] <- yhat
        results[1,2] <- lower_bound
        results[1,3] <- upper_bound
        colnames(results) <- c("Estimate", "90% PI Low", "90% PI High")
        row.names(results) <- "Lasso Prediction"
        
        datatable(results, options = list(dom = "t", ordering = FALSE), selection = "none") %>%
          formatCurrency('Estimate',currency = "", interval = 3, mark = ",", digits = 3) %>%
          formatCurrency('90% PI Low',currency = "", interval = 3, mark = ",", digits = 3) %>%
          formatCurrency('90% PI High',currency = "", interval = 3, mark = ",", digits = 3)
        
      } else {
        results <- matrix(NA, nrow = 1, ncol = 1)
        results[1,1] <- yhat[1]
        colnames(results) <- c("Estimate")
        row.names(results) <- "Lasso Prediction (PI requires 50% holdout)"
        datatable(results, colnames = c("Estimate"), options = list(dom = "t", ordering = FALSE), selection = "none") %>%
          formatCurrency('Estimate',currency = "", interval = 3, mark = ",", digits = 3)
      }

    } else if (input$ols_penalty == "Ridge") {

      if (input$ols_holdout == "Yes (Holdout 50% for Testing)"){
        ridge_test_residuals <- rv$ridge_test_residuals
        alpha <- 0.1
        req(length(ridge_test_residuals) > 0)
        quantile_index <- min(length(ridge_test_residuals), ceiling((1-alpha)*(length(ridge_test_residuals)+1)))
        margin_of_predictive_error <- sort(ridge_test_residuals)[quantile_index]
        lower_bound <- yhat - margin_of_predictive_error
        upper_bound <- yhat + margin_of_predictive_error
        
        results <- matrix(NA, nrow = 1, ncol = 3)
        results[1,1] <- yhat
        results[1,2] <- lower_bound
        results[1,3] <- upper_bound
        colnames(results) <- c("Estimate", "90% PI Low", "90% PI High")
        row.names(results) <- "Ridge Prediction"
        
        datatable(results, options = list(dom = "t", ordering = FALSE), selection = "none") %>%
          formatCurrency('Estimate',currency = "", interval = 3, mark = ",", digits = 3) %>%
          formatCurrency('90% PI Low',currency = "", interval = 3, mark = ",", digits = 3) %>%
          formatCurrency('90% PI High',currency = "", interval = 3, mark = ",", digits = 3)
        
      } else {
        results <- matrix(NA, nrow = 1, ncol = 1)
        results[1,1] <- yhat[1]
        colnames(results) <- c("Estimate")
        row.names(results) <- "Ridge Prediction (PI requires 50% holdout)"
        datatable(results, colnames = c("Estimate"), options = list(dom = "t", ordering = FALSE), selection = "none") %>%
          formatCurrency('Estimate',currency = "", interval = 3, mark = ",", digits = 3)
      }

    } else if (input$ols_penalty == "Elastic Net") {
      
      if (input$ols_holdout == "Yes (Holdout 50% for Testing)"){
        elastic_test_residuals <- rv$elastic_test_residuals
        alpha <- 0.1
        req(length(elastic_test_residuals) > 0)
        quantile_index <- min(length(elastic_test_residuals), ceiling((1-alpha)*(length(elastic_test_residuals)+1)))
        margin_of_predictive_error <- sort(elastic_test_residuals)[quantile_index]
        lower_bound <- yhat - margin_of_predictive_error
        upper_bound <- yhat + margin_of_predictive_error
        
        results <- matrix(NA, nrow = 1, ncol = 3)
        results[1,1] <- yhat
        results[1,2] <- lower_bound
        results[1,3] <- upper_bound
        colnames(results) <- c("Estimate", "90% PI Low", "90% PI High")
        row.names(results) <- "Elastic Net Prediction"
        
        datatable(results, options = list(dom = "t", ordering = FALSE), selection = "none") %>%
          formatCurrency('Estimate',currency = "", interval = 3, mark = ",", digits = 3) %>%
          formatCurrency('90% PI Low',currency = "", interval = 3, mark = ",", digits = 3) %>%
          formatCurrency('90% PI High',currency = "", interval = 3, mark = ",", digits = 3)
        
      } else {
        results <- matrix(NA, nrow = 1, ncol = 1)
        results[1,1] <- yhat[1]
        colnames(results) <- c("Estimate")
        row.names(results) <- "Elastic Net Prediction (PI requires 50% holdout)"
        datatable(results, colnames = c("Estimate"), options = list(dom = "t", ordering = FALSE), selection = "none") %>%
          formatCurrency('Estimate',currency = "", interval = 3, mark = ",", digits = 3)
      }
    }
  })

  output$ols_dist_plot <- renderPlotly({
    req(rv$ols_model)
    validate(need(input$ols_penalty == "None",
                  "Prediction distribution chart is available for OLS with no penalty only."))
    req(input$ols_pi_level)

    new_data <- ols_new_data()

    estimate      <- tryCatch(predict(rv$ols_model, new_data)[1], error = function(e) NA_real_)
    req(is.finite(estimate))
    SE_pred <- tryCatch({
      pred_ref      <- ols_classical_pi(rv$ols_model, new_data, 0.95)
      (pred_ref[1, "fit"] - pred_ref[1, "lwr"]) / qt(0.975, df.residual(rv$ols_model$finalModel))
    }, error = function(e) NA_real_)
    req(is.finite(SE_pred))
    L             <- input$ols_pi_level / 100
    t_L           <- qt(1 - (1 - L) / 2, df.residual(rv$ols_model$finalModel))
    low           <- estimate - t_L * SE_pred
    high          <- estimate + t_L * SE_pred

    rq_fit_ols <- tryCatch(
      ceres_ns("quantreg")$rq(formula(rv$ols_model$finalModel),
                   data = rv$ols_model$finalModel$model,
                   tau  = 0.5),
      error = function(e) NULL
    )
    median_est <- if (is.null(rq_fit_ols)) estimate else {
      tryCatch(predict(rq_fit_ols, newdata = new_data)[[1]], error = function(e) estimate)
    }

    x_min  <- qnorm(DISTRIBUTION_TRUNCATION_LOW, mean = estimate, sd = SE_pred)
    x_max  <- qnorm(DISTRIBUTION_TRUNCATION_HIGH, mean = estimate, sd = SE_pred)
    x_grid <- seq(x_min, x_max, length.out = 400)
    y_grid <- dnorm(x_grid, mean = estimate, sd = SE_pred)

    in_pi   <- x_grid >= low & x_grid <= high
    x_shade <- c(low, x_grid[in_pi], high)
    y_shade <- c(0, y_grid[in_pi], 0)

    fmt      <- function(x) formatC(x, format = "f", digits = 3, big.mark = ",")
    pi_label <- paste0(round(L * 100), "%")
    y_peak   <- max(y_grid)

    pc_rgb       <- col2rgb(primary_color())[, 1]
    median_color <- paste0("rgba(", pc_rgb[1], ",", pc_rgb[2], ",", pc_rgb[3], ",0.65)")
    ann_low           <- list(
      x = low, y = dnorm(low, mean = estimate, sd = SE_pred) * 2.2,
      xref = "x", yref = "y",
      text = paste0("<b>", fmt(low), "</b><br><span style='font-size:10px;'>", pi_label, " Low</span>"),
      showarrow = FALSE, xanchor = "center", yanchor = "bottom",
      font = list(color = "#2F6FE4", size = 11)
    )
    ann_high          <- list(
      x = high, y = dnorm(high, mean = estimate, sd = SE_pred) * 2.2,
      xref = "x", yref = "y",
      text = paste0("<b>", fmt(high), "</b><br><span style='font-size:10px;'>", pi_label, " High</span>"),
      showarrow = FALSE, xanchor = "center", yanchor = "bottom",
      font = list(color = "#2F6FE4", size = 11)
    )
    gap_frac <- abs(median_est - estimate) / (x_max - x_min)
    mean_y   <- if (gap_frac < 0.08) y_peak * 1.24 else y_peak * 1.05
    ann_mean_center   <- list(
      x = estimate, y = mean_y,
      xref = "x", yref = "y",
      text = paste0("<b>", fmt(estimate), "</b><br><span style='font-size:10px;'>Mean</span>"),
      showarrow = FALSE, xanchor = "center", yanchor = "bottom",
      font = list(color = primary_color(), size = 11)
    )
    ann_median_center <- list(
      x = median_est, y = y_peak * 1.05,
      xref = "x", yref = "y",
      text = paste0("<b>", fmt(median_est), "</b><br><span style='font-size:10px;'>Median</span>"),
      showarrow = FALSE, xanchor = "center", yanchor = "bottom",
      font = list(color = median_color, size = 11)
    )

    plot_ly() %>%
      add_trace(
        x = x_shade, y = y_shade,
        type = "scatter", mode = "lines",
        fill = "tozeroy",
        fillcolor = "rgba(47, 111, 228, 0.18)",
        line = list(color = "transparent"),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      add_trace(
        x = x_grid, y = y_grid,
        type = "scatter", mode = "lines",
        line = list(color = "#2F6FE4", width = 2.5),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      add_segments(
        x = median_est, xend = median_est,
        y = 0, yend = y_peak * 1.05,
        line = list(color = median_color, width = 2, dash = "dash"),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      add_segments(
        x = estimate, xend = estimate,
        y = 0, yend = y_peak * 1.05,
        line = list(color = primary_color(), width = 2, dash = "dash"),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      add_segments(
        x = low, xend = low,
        y = 0, yend = dnorm(low, mean = estimate, sd = SE_pred) * 2,
        line = list(color = "#2F6FE4", width = 1.5, dash = "dot"),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      add_segments(
        x = high, xend = high,
        y = 0, yend = dnorm(high, mean = estimate, sd = SE_pred) * 2,
        line = list(color = "#2F6FE4", width = 1.5, dash = "dot"),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      layout(
        xaxis = list(title = input$ols_response, zeroline = FALSE, showgrid = FALSE),
        yaxis = list(title = "Density", zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE),
        margin = list(t = 55, r = 20, b = 50, l = 40),
        annotations = list(ann_low, ann_mean_center, ann_median_center, ann_high),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$ols_dist_stats <- renderUI({
    req(rv$ols_model)
    validate(need(input$ols_penalty == "None",
                  "Distribution stats are available for OLS with no penalty only."))
    req(input$ols_pi_level)

    new_data <- ols_new_data()

    estimate      <- tryCatch(predict(rv$ols_model, new_data)[1], error = function(e) NA_real_)
    req(is.finite(estimate))
    SE_pred <- tryCatch({
      pred_ref      <- ols_classical_pi(rv$ols_model, new_data, 0.95)
      (pred_ref[1, "fit"] - pred_ref[1, "lwr"]) / qt(0.975, df.residual(rv$ols_model$finalModel))
    }, error = function(e) NA_real_)
    req(is.finite(SE_pred))
    L             <- input$ols_pi_level / 100
    t_L           <- qt(1 - (1 - L) / 2, df.residual(rv$ols_model$finalModel))
    low           <- estimate - t_L * SE_pred
    high          <- estimate + t_L * SE_pred

    rq_fit_ols_s <- tryCatch(
      ceres_ns("quantreg")$rq(formula(rv$ols_model$finalModel),
                   data = rv$ols_model$finalModel$model,
                   tau  = 0.5),
      error = function(e) NULL
    )
    median_est <- if (is.null(rq_fit_ols_s)) estimate else {
      tryCatch(predict(rq_fit_ols_s, newdata = new_data)[[1]], error = function(e) estimate)
    }

    fmt      <- function(x) formatC(x, format = "f", digits = 3, big.mark = ",")
    pi_label <- paste0(round(L * 100), "%")

    tags$div(
      style = paste0(
        "display:flex;",
        "background:white; border:1px solid #dee2e6;",
        "border-radius:5px; padding:8px 0; margin-top:8px;",
        "box-shadow:0 1px 3px rgba(0,0,0,.05);"
      ),
      tags$div(
        style = "flex:1; text-align:center; padding:0 8px;",
        tags$div(style = "font-size:15px; font-weight:700; color:#2F6FE4;", fmt(low)),
        tags$div(style = "font-size:10px; color:#888; margin-top:2px;", paste0(pi_label, " Low"))
      ),
      tags$div(
        style = "flex:1; text-align:center; border-left:1px solid #eee; padding:0 8px;",
        tags$div(style = paste0("font-size:16px; font-weight:700; color:", primary_color(), ";"), fmt(estimate)),
        tags$div(style = "font-size:10px; color:#888; margin-top:2px;", "Mean")
      ),
      tags$div(
        style = paste0("flex:1; text-align:center; border-left:1px solid #eee; padding:0 8px; color:", paste0(primary_color(), "A6"), ";"),
        tags$div(style = paste0("font-size:16px; font-weight:700; color:", paste0(primary_color(), "A6"), ";"), fmt(median_est)),
        tags$div(style = "font-size:10px; color:#888; margin-top:2px;", "Median")
      ),
      tags$div(
        style = "flex:1; text-align:center; border-left:1px solid #eee; padding:0 8px;",
        tags$div(style = "font-size:15px; font-weight:700; color:#2F6FE4;", fmt(high)),
        tags$div(style = "font-size:10px; color:#888; margin-top:2px;", paste0(pi_label, " High"))
      )
    )
  })

  output$ols_breakdown <- renderPlotly({
    req(rv$ols_model)
    req(rv$ols_train_data)
    req(length(input$ols_predictors) > 1)
    
    new_data <- ols_new_data()

    explain_ols <- ceres_ns("DALEX")$explain(rv$ols_model,
                                  data = rv$ols_train_data,
                                  y = rv$ols_train_y %||% rv$active_dataset[rv$train_index,input$ols_response],
                                  label = "OLS",
                                  verbose = FALSE)
    
    ols_la <- ceres_ns("iBreakDown")$local_attributions(explain_ols, new_data)
    
    x <- ols_la$contribution
    y_raw <- as.character(ols_la$variable)
    y_raw[y_raw == "prediction"] <- "Prediction"
    y <- factor(y_raw, levels = y_raw)
    measure <- c(rep("relative", length(x)-1), "total")

    data = data.frame(x, y, measure)

    plot_ly(data, x = ~x, y = ~y, measure = ~measure, type = "waterfall", name = "New Prediction",
            orientation = "h",
            totals = list(marker = list(color = primary_color())),
            connector = list(mode = "between", line = list(width = 4, color = "rgb(0, 0, 0)", dash = 0))) %>%
      layout(title = "Local Attributions",
             xaxis = list(title = "Marginal Contribution to Estimate",
                          tickfont = list(size = 16), ticks = "outside", type = "linear"),
             yaxis = list(title = "", type = "category", autorange = "reversed"),
             showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
    
  })
 
  
  observeEvent(input$nl_train, {
    req(rv$active_dataset, input$nl_equation, input$nl_response)
    shinyjs::disable("nl_train")
    shinyjs::html("nl_train", "Fitting...")
    on.exit({
      shinyjs::enable("nl_train")
      shinyjs::html("nl_train", "Run Non-Linear Model")
    })
    sync_cer_generation_data()

    if (input$nl_equation == "Custom") {
      req(input$nl_custom_eq, nchar(trimws(input$nl_custom_eq)) > 0)

      eq_err <- validate_nl_custom_eq(input$nl_custom_eq)
      if (!is.null(eq_err)) {
        showNotification(paste0("Equation rejected: ", eq_err), type = "error", duration = 10)
        return()
      }

      params <- nl_custom_params()

      all_col_names <- names(rv$active_dataset)
      san_to_orig   <- setNames(all_col_names,
                                vapply(all_col_names, sanitize_nl_colname, character(1)))
      parsed_syms  <- tryCatch(all.vars(parse(text = input$nl_custom_eq)), error = function(e) character(0))
      x_vars_in_eq <- unique(c(
        intersect(parsed_syms, all_col_names),
        san_to_orig[intersect(parsed_syms, names(san_to_orig))]
      ))

      formula_rhs_san <- input$nl_custom_eq
      for (raw_col in x_vars_in_eq) {
        san_col <- sanitize_nl_colname(raw_col)
        formula_rhs_san <- gsub(paste0("`", raw_col, "`"), san_col, formula_rhs_san, fixed = TRUE)
        formula_rhs_san <- gsub(paste0("\\b", gsub("([.\\[\\]^$*+?|({})])", "\\\\\\1", raw_col, perl=TRUE), "\\b"),
                                san_col, formula_rhs_san, perl = TRUE)
      }
      lhs_name    <- sanitize_nl_colname(input$nl_response)
      formula_str <- paste0(lhs_name, " ~ ", formula_rhs_san)

      start_vals <- setNames(as.list(rep(1, length(params))), params)
      for (p in params) {
        override_id <- paste0("nl_custom_start_", p)
        v <- input[[override_id]]
        if (is.numeric(v) && length(v) == 1 && is.finite(v)) start_vals[[p]] <- v
      }

      cols_needed <- c(input$nl_response, x_vars_in_eq)
      nl_complete_rows <- complete.cases(rv$active_dataset[, cols_needed, drop = FALSE])
      df_raw      <- rv$active_dataset[nl_complete_rows, cols_needed, drop = FALSE]
      san_map     <- setNames(vapply(names(df_raw), sanitize_nl_colname, character(1)), names(df_raw))
      nl_amb  <- intersect(parsed_syms, names(san_to_orig)[duplicated(names(san_to_orig))])
      nl_coll <- nl_name_collisions(unique(c(names(df_raw), all_col_names[names(san_to_orig) %in% nl_amb])))
      if (!is.null(nl_coll)) {
        showNotification(nl_coll, type = "error", duration = 12)
        return()
      }
      df          <- df_raw
      names(df)   <- san_map[names(df)]

      san_lhs <- san_map[[input$nl_response]]
      san_x_vars <- san_map[x_vars_in_eq]
      applied_log_y <- FALSE
      applied_log_x_vars <- character(0)
      if (isTRUE(input$nl_log_y)) {
        if (any(df[[san_lhs]] <= 0, na.rm = TRUE)) {
          showNotification("Log Y: response variable contains non-positive values. Log transform skipped.", type = "warning")
        } else {
          df[[san_lhs]] <- log(df[[san_lhs]])
          applied_log_y <- TRUE
        }
      }
      if (isTRUE(input$nl_log_x)) {
        for (xv in san_x_vars) {
          if (any(df[[xv]] <= 0, na.rm = TRUE)) {
            showNotification(paste0("Log X: predictor '", xv, "' contains non-positive values. Log transform skipped for this variable."), type = "warning")
          } else {
            df[[xv]] <- log(df[[xv]])
            applied_log_x_vars <- c(applied_log_x_vars, xv)
          }
        }
      }

      if (nrow(df) <= length(params)) {
        showNotification(
          "Not enough observations to estimate the parameters. Need more rows than parameters.",
          type = "error", duration = 10
        )
        return()
      }

      if (input$ols_holdout == OLS_HOLDOUT_CHOICES[1] || input$ols_holdout %in% OLS_HOLDOUT_CHOICES[5:7]) {
        train_index <- seq_len(nrow(df)); data_train <- df; data_test <- df
      } else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[2]) {
        train_index <- withr::with_seed(123, ceres_ns("caret")$createDataPartition(df[[lhs_name]], p = 0.8)[[1]])
        data_train <- df[train_index, ]; data_test <- df[-train_index, ]
      } else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[3]) {
        train_index <- withr::with_seed(123, ceres_ns("caret")$createDataPartition(df[[lhs_name]], p = 0.5)[[1]])
        data_train <- df[train_index, ]; data_test <- df[-train_index, ]
      } else {
        test_index <- match(rv$manual_test_set, which(nl_complete_rows))
        test_index <- test_index[!is.na(test_index)]
        if (length(test_index) == 0) {
          train_index <- seq_len(nrow(df)); data_train <- df; data_test <- df
        } else if (length(test_index) >= nrow(df)) {
          train_index <- seq_len(nrow(df)); data_train <- df; data_test <- df
          showNotification(
            "Manual test set contains every usable row — no rows left to train on. Training on all data instead.",
            type = "warning", duration = 8
          )
        } else {
          all_index  <- seq_len(nrow(df))
          train_index <- all_index[-test_index]
          data_train <- df[train_index, ]; data_test <- df[-train_index, ]
        }
      }

      log_linear_seeds <- NULL
      if (length(x_vars_in_eq) > 0 && all(data_train[[lhs_name]] > 0, na.rm = TRUE)) {
        log_linear_seeds <- tryCatch({
          san_pred_names <- vapply(x_vars_in_eq, function(v) san_map[[v]], character(1))
          lm_data <- cbind(
            data.frame(.log_y = log(data_train[[lhs_name]])),
            data_train[, san_pred_names, drop = FALSE]
          )
          lm_fit   <- lm(.log_y ~ ., data = lm_data)
          lm_coefs <- coef(lm_fit)
          intercept_val <- unname(lm_coefs[1])
          slope_vals    <- unname(lm_coefs[-1])
          seed_ols <- setNames(as.list(rep(1, length(params))), params)
          if (length(params) >= 1)
            seed_ols[[params[1]]] <- exp(intercept_val)
          for (i in seq_len(min(length(slope_vals), length(params) - 1L)))
            seed_ols[[params[i + 1L]]] <- slope_vals[i]
          list(seed_ols)
        }, error = function(e) NULL)
      }
      logistic_seeds <- NULL
      if (grepl("/\\s*\\(\\s*1\\s*\\+\\s*exp\\s*\\(", formula_str)) {
        logistic_seeds <- tryCatch({
          rhs           <- trimws(strsplit(formula_str, "~")[[1]][2])
          numerator_str <- trimws(sub("/\\s*\\(\\s*1\\s*\\+\\s*exp.*", "", rhs))
          numer_tokens  <- regmatches(numerator_str,
                                      gregexpr("[a-zA-Z][a-zA-Z0-9_]*", numerator_str))[[1]]
          asym_param    <- params[params %in% numer_tokens]
          asym_param    <- if (length(asym_param) == 1) asym_param else NULL

          san_pred_names <- vapply(x_vars_in_eq, function(v) san_map[[v]], character(1))
          slope_map <- list()
          for (sp in san_pred_names) {
            for (p in params) {
              if (grepl(paste0("\\b", p, "\\s*\\*\\s*", sp, "\\b"), formula_rhs_san, perl = TRUE) ||
                  grepl(paste0("\\b", sp, "\\b\\s*\\*\\s*", p, "\\b"),  formula_rhs_san, perl = TRUE)) {
                slope_map[[p]] <- sp
                break
              }
            }
          }
          shift_params <- setdiff(params, c(asym_param, names(slope_map)))

          a_guess   <- max(data_train[[lhs_name]], na.rm = TRUE) * 1.05
          y_clamped <- pmax(pmin(data_train[[lhs_name]], a_guess * 0.9999), 1e-6)
          lin_y     <- log(a_guess / y_clamped - 1)
          lm_data   <- cbind(data.frame(.lin_y = lin_y),
                             data_train[, san_pred_names, drop = FALSE])
          lm_fit    <- lm(.lin_y ~ ., data = lm_data)
          lm_coefs  <- coef(lm_fit)

          seed <- setNames(as.list(rep(1, length(params))), params)
          if (!is.null(asym_param))
            seed[[asym_param]] <- a_guess
          for (sp in shift_params)
            seed[[sp]] <- -unname(lm_coefs[1]) / max(length(shift_params), 1L)
          for (p in names(slope_map)) {
            san_pred <- slope_map[[p]]
            if (san_pred %in% names(lm_coefs))
              seed[[p]] <- -unname(lm_coefs[san_pred])
          }
          list(seed)
        }, error = function(e) NULL)
      }

      start_list <- c(log_linear_seeds, logistic_seeds, make_nl_start_list(params, start_vals))

      nl_is_mupe <- identical(input$nl_error_term, "MUPE")
      est_coeff <- tryCatch(
        nl_fit_builtin(formula_str, data_train, "Custom", character(0), input$nl_error_term,
                       extra_starts = start_list, grid = FALSE),
        error = function(e) e
      )

      if (inherits(est_coeff, "error")) {
        msg <- conditionMessage(est_coeff)
        showNotification(nl_cb_fail_message(msg, "Custom"),
                         type = if (grepl("singular|converge|step factor", msg, ignore.case = TRUE)) "warning" else "error",
                         duration = 12)
        rv$nl_custom_show_startvals <- TRUE
        return()
      }

      yhat     <- predict(est_coeff, data_train)
      Z        <- get_derivative(formula_str, data_train, coef(est_coeff))
      df_resid <- nrow(data_train) - length(coef(est_coeff))
      if (nl_is_mupe) {
        w_vec    <- 1 / pmax(abs(yhat), 1e-10)^2
        SSPE     <- sum(((data_train[[lhs_name]] - yhat) / pmax(abs(yhat), 1e-10))^2)
        root_MSE <- sqrt(SSPE / df_resid)
        possible_error <- try(scaled_ginv(crossprod(Z * sqrt(w_vec))), silent = TRUE)
      } else {
        SSE      <- sum((data_train[[lhs_name]] - yhat)^2)
        root_MSE <- sqrt(SSE / df_resid)
        possible_error <- try(scaled_ginv(crossprod(Z)), silent = TRUE)
      }
      if (inherits(possible_error, "try-error")) {
        showNotification("Gradient matrix inversion failed. Try a simpler equation or more data.", type = "error")
        return()
      }
      var_covar <- root_MSE^2 * possible_error
      yhat_test <- predict(est_coeff, data_test)

      nl_run_cv     <- input$ols_holdout %in% OLS_HOLDOUT_CHOICES[5:7]
      cv_indices    <- if (nl_run_cv) withr::with_seed(1, ceres_ns("caret")$createMultiFolds(data_train[[lhs_name]], k = 10, times = 1)) else NULL
      nl_cv_results <- if (nl_run_cv) numeric(length(cv_indices)) else NULL
      nl_cv_details <- list()
      if (nl_run_cv) withProgress(message = 'Performing Cross Validation', value = 1, {
        for (i in seq_along(cv_indices)) {
          data_train_fold <- data_train[cv_indices[[i]], ]
          data_test_fold  <- data_train[-cv_indices[[i]], ]
          if (nl_is_mupe) {
            fold_fit <- tryCatch(fit_mupe(formula_str, data_train_fold, as.list(coef(est_coeff))), error = function(e) NULL)
            if (is.null(fold_fit)) fold_fit <- tryCatch(fit_nlls_multistart(formula_str, data_train_fold, start_list, fit_fn = fit_mupe), error = function(e) NULL)
          } else {
            fold_fit <- tryCatch(fit_nlls(formula_str, data_train_fold, as.list(coef(est_coeff))), error = function(e) NULL)
            if (is.null(fold_fit)) fold_fit <- tryCatch(fit_nlls_multistart(formula_str, data_train_fold, start_list), error = function(e) NULL)
          }
          if (!is.null(fold_fit)) {
            yhat_fold <- predict(fold_fit, data_test_fold)
            nl_cv_results[i] <- if (nl_is_mupe)
              sqrt(sum(((data_test_fold[[lhs_name]] - yhat_fold) / pmax(abs(yhat_fold), 1e-10))^2) / length(yhat_fold))
              else ceres_ns("caret")$RMSE(yhat_fold, data_test_fold[[lhs_name]])
            nl_cv_details[[i]] <- data.frame(
              "Selected Row" = rep(names(cv_indices)[i], length(yhat_fold)),
              "Name"         = row.names(data_test_fold),
              "Predicted"    = yhat_fold,
              "Actual"       = data_test_fold[[lhs_name]]
            )
          } else {
            nl_cv_results[i] <- NA
          }
          incProgress(1/length(cv_indices), detail = paste0(round(100*(i/length(cv_indices)), 1), "% Complete"))
        }
      })
      n_failed_cv <- sum(is.na(nl_cv_results))
      if (n_failed_cv > 0.2 * length(cv_indices)) {
        showNotification(
          paste0(n_failed_cv, " of ", length(cv_indices), " CV folds failed to converge. ",
                 "CV statistics are based on the remaining ", length(cv_indices) - n_failed_cv, " folds only."),
          type = "warning", duration = 15
        )
      }

      TSS           <- sum((data_train[[lhs_name]] - mean(data_train[[lhs_name]]))^2)
      pseudo_r2     <- 1 - (sum((data_train[[lhs_name]] - yhat)^2) / TSS)
      n_train       <- nrow(data_train)
      p_coeff       <- length(coef(est_coeff))
      adj_pseudo_r2 <- 1 - (1 - pseudo_r2) * (n_train - 1) / (n_train - p_coeff)
      t_vals        <- summary(est_coeff)$parameters[, "t value"]
      prop_sig      <- mean(abs(t_vals) >= 2)
      ybar_nl <- mean(data_train[[lhs_name]])
      cv_val  <- if (is.finite(ybar_nl) && abs(ybar_nl) > 1e-12) root_MSE / abs(ybar_nl) else NA_real_
      mape_val      <- mean(abs(data_train[[lhs_name]] - yhat) / pmax(abs(data_train[[lhs_name]]), 1e-10))

      results <- data.frame(
        Num_Variables      = length(x_vars_in_eq),
        R_Squared          = pseudo_r2,
        Adj_R_Squared      = adj_pseudo_r2,
        Standard_Error     = root_MSE,
        Coeff_Significance = prop_sig,
        CV                 = cv_val,
        MAPE               = mape_val
      )
      colnames(results) <- c("# Variables", "R-Squared", "Adj. R-Squared", "Standard Error",
                              "Coeff Significance (%)", "CV (%)", "MAPE (%)")
      rownames(results) <- "Custom"

      diag_vcov    <- diag(var_covar)
      if (any(diag_vcov < 0)) {
        showNotification(
          "Warning: The variance-covariance matrix for this model contains negative diagonal values. This indicates numerical instability in the Hessian approximation — typically caused by an overparameterized equation, near-collinear parameters, or convergence to a saddle point. Standard errors and confidence intervals shown below may be unreliable.",
          type = "warning", duration = 15
        )
        diag_vcov <- abs(diag_vcov)
      }
      se_coeff     <- sqrt(diag_vcov)
      t_mult       <- qt(0.975, nrow(data_train) - p_coeff)
      coeff_result <- cbind(coef(est_coeff), se_coeff,
                            coef(est_coeff) - se_coeff * t_mult,
                            coef(est_coeff) + se_coeff * t_mult)
      names(yhat)      <- row.names(data_train)
      names(yhat_test) <- row.names(data_test)

      rv$nl_results          <- results
      rv$nl_coeffs           <- coeff_result
      rv$nl_model            <- est_coeff
      rv$nl_cv_results       <- nl_cv_results
      rv$nl_cv_indices       <- cv_indices
      rv$nl_cv_details       <- nl_cv_details
      rv$nl_sets             <- list(yhat_train = yhat, yhat_test = yhat_test,
                                     y_train = data_train[[lhs_name]], y_test = data_test[[lhs_name]],
                                     data_train = data_train)
      rv$train_index         <- train_index
      rv$nl_custom_eq_text        <- input$nl_custom_eq
      rv$nl_custom_fitted_eq      <- input$nl_custom_eq
      rv$nl_builder_render_token  <- rv$nl_builder_render_token + 1L
      rv$nl_custom_x_vars         <- x_vars_in_eq
      rv$nl_custom_fitted         <- TRUE
      rv$nl_custom_show_startvals <- FALSE
      rv$nl_fit_log_y             <- applied_log_y
      rv$nl_fit_log_x_vars        <- applied_log_x_vars
      rv$nl_custom_fitted_formula <- formula_str
      rv$nl_fit_response   <- input$nl_response
      rv$nl_fit_eq_name    <- "Custom"
      rv$nl_fit_formula    <- paste0(sanitize_nl_colname(input$nl_response), " ~ ", input$nl_custom_eq)
      rv$nl_fit_predictors <- x_vars_in_eq
      rv$nl_fit_error_term <- input$nl_error_term
      return()
    }

    req(input$nl_predictors)
    tryCatch({
      formula_test <- nlls_formula_string()
      if (formula_test == "Awaiting variables...") stop("Please select predictors.")
      
      reg <- nl_registry[[input$nl_equation]]
      x_vars <- input$nl_predictors
      if (reg$type == "univariate" && length(x_vars) > 1) x_vars <- x_vars[1]

      lhs_name <- input$nl_response
      y <- rv$active_dataset[, lhs_name]
      x <- rv$active_dataset[, x_vars, drop = FALSE]

      nzv_idx <- ceres_ns("caret")$nearZeroVar(x)
      if (length(nzv_idx) > 0) {
        showNotification("Removing zero-variance predictors from NL model.", type = "warning")
        x_vars <- x_vars[-nzv_idx]
        x <- x[, x_vars, drop = FALSE]
        if (length(x_vars) == 0) stop("All predictors have zero variance. Aborting.")
      }

      data <- data.frame(y, x, check.names = FALSE)
      colnames(data)[1] <- lhs_name
      nl_complete_rows <- complete.cases(data)
      data <- data[nl_complete_rows, , drop = FALSE]

      if(nrow(data) == 0) stop("No valid data rows available.")

      san_map  <- setNames(vapply(names(data), sanitize_nl_colname, character(1)), names(data))
      nl_coll <- nl_name_collisions(names(data))
      if (!is.null(nl_coll)) stop(nl_coll)
      names(data) <- san_map[names(data)]
      lhs_name <- sanitize_nl_colname(lhs_name)
      nl_orig_x_vars <- unname(x_vars)
      x_vars   <- vapply(x_vars, sanitize_nl_colname, character(1))

      applied_log_y <- FALSE
      applied_log_x_vars <- character(0)
      if (isTRUE(input$nl_log_y)) {
        if (any(data[[lhs_name]] <= 0, na.rm = TRUE)) {
          showNotification("Log Y: response variable contains non-positive values. Log transform skipped.", type = "warning")
        } else {
          data[[lhs_name]] <- log(data[[lhs_name]])
          applied_log_y <- TRUE
        }
      }
      if (isTRUE(input$nl_log_x)) {
        for (xv in x_vars) {
          if (any(data[[xv]] <= 0, na.rm = TRUE)) {
            showNotification(paste0("Log X: predictor '", xv, "' contains non-positive values. Log transform skipped for this variable."), type = "warning")
          } else {
            data[[xv]] <- log(data[[xv]])
            applied_log_x_vars <- c(applied_log_x_vars, xv)
          }
        }
      }

      expected_params <- reg$params_base
      if (reg$type == "multivariate") expected_params <- c(expected_params, paste0(reg$param_prefix, x_vars))

      init_coef <- numeric(length(expected_params))
      names(init_coef) <- expected_params

      guesses_fallback <- isolate(nl_guesses())
      for (p in expected_params) {
        val <- input[[paste0("nlls_init_", p)]]
        if (is.null(val)) val <- guesses_fallback[[p]]
        if (is.null(val) || is.na(val)) val <- 1.0
        init_coef[p] <- as.numeric(val)
      }

      n_params_eq <- length(nl_registry[[input$nl_equation]]$params_base)
      validation_msg <- validate_nl_data(data, x_vars, lhs_name, input$nl_equation, n_params_eq)
      if (!is.null(validation_msg)) {
        showNotification(validation_msg, type = "error", duration = 10)
        return(NULL)
      }

      if (input$ols_holdout == OLS_HOLDOUT_CHOICES[1] || input$ols_holdout %in% OLS_HOLDOUT_CHOICES[5:7]){
        train_index <- seq_along(data[[lhs_name]]); y_test <- data[[lhs_name]]; data_test <- data; data_train <- data
      } else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[2]){
        train_index <- withr::with_seed(123, ceres_ns("caret")$createDataPartition(data[[lhs_name]], p = 0.8)[[1]]); y_test <- data[[lhs_name]][-train_index]; data_test <- data[-train_index,]; data_train <- data[train_index,]
      } else if (input$ols_holdout == OLS_HOLDOUT_CHOICES[3]){
        train_index <- withr::with_seed(123, ceres_ns("caret")$createDataPartition(data[[lhs_name]], p = 0.5)[[1]]); y_test <- data[[lhs_name]][-train_index]; data_test <- data[-train_index,]; data_train <- data[train_index,]
      } else {
        test_index <- match(rv$manual_test_set, which(nl_complete_rows))
        test_index <- test_index[!is.na(test_index)]
        if (length(test_index) == 0){ train_index <- seq_along(data[[lhs_name]]); y_test <- data[[lhs_name]]; data_test <- data; data_train <- data }
        else if (length(test_index) >= nrow(data)) {
          train_index <- seq_along(data[[lhs_name]]); y_test <- data[[lhs_name]]; data_test <- data; data_train <- data
          showNotification(
            "Manual test set contains every usable row — no rows left to train on. Training on all data instead.",
            type = "warning", duration = 8
          )
        }
        else { all_index <- seq_along(data[[lhs_name]]); train_index <- all_index[-test_index]; y_test <- data[[lhs_name]][-train_index]; data_test <- data[-train_index,]; data_train <- data[train_index,] }
      }
      
      nl_run_cv  <- input$ols_holdout %in% OLS_HOLDOUT_CHOICES[5:7]
      cv_indices <- if (nl_run_cv) withr::with_seed(1, ceres_ns("caret")$createMultiFolds(data_train[[lhs_name]], k = 10, times = 1)) else NULL
      nl_cv_results <- if (nl_run_cv) numeric(length(cv_indices)) else NULL
      nl_cv_details <- list()

      nl_extra_starts <- NULL
      if (input$nl_equation == "Weibull (Type 2)") {
        safe_max_y <- max(data_train[[lhs_name]], na.rm = TRUE)
        safe_max_x <- max(data_train[[x_vars[1]]], na.rm = TRUE)
        nl_extra_starts <- list(
          list(a = safe_max_y,       b = 0.5,          c = 1.0),
          list(a = safe_max_y,       b = 0.1,          c = 1.5),
          list(a = safe_max_y,       b = 1/safe_max_x, c = 2.0),
          list(a = safe_max_y * 1.1, b = 0.05,         c = 0.8)
        )
      }
      nl_fit_eq <- function(dat, term) nl_fit_builtin(formula_test, dat, input$nl_equation, unname(x_vars), term,
                                                      seed = init_coef, extra_starts = nl_extra_starts)

      if (input$nl_error_term == "Least Squares") {
        est_coeff <- tryCatch(
          nl_fit_eq(data_train, "Least Squares"),
          error = function(e) {
            if (grepl("singular|step factor", e$message, ignore.case = TRUE)) stop(nl_cb_fail_message(e$message, input$nl_equation))
            else stop(e$message)
          })

        yhat <- predict(est_coeff, data_train)
        Z <- get_derivative(formula_test, data_train, coef(est_coeff))
        SSE <- sum((data_train[[lhs_name]] - yhat)^2)
        root_MSE <- sqrt(SSE/(nrow(data_train) - length(coef(est_coeff))))

        possible_error <- try(scaled_ginv(crossprod(Z)), silent = TRUE)
        if(inherits(possible_error, "try-error")) stop("Gradient matrix inversion failed critically.")
        var_covar <- root_MSE^2 * possible_error

        yhat_test <- predict(est_coeff, data_test)

        best_coeff_cv <- as.list(coef(est_coeff))
        if (nl_run_cv) withProgress(message = 'Performing Cross Validation', value = 1, {
          for (i in seq_along(cv_indices)){
            data_train_fold <- data_train[cv_indices[[i]],]; data_test_fold <- data_train[-cv_indices[[i]],]
            nlls_coeff_fold <- tryCatch(fit_nlls(formula_test, data_train_fold, best_coeff_cv), error = function(e) NULL)
            if (is.null(nlls_coeff_fold)) nlls_coeff_fold <- tryCatch(nl_fit_eq(data_train_fold, "Least Squares"), error = function(e) NULL)
            if(!is.null(nlls_coeff_fold)) {
              yhat_fold <- predict(nlls_coeff_fold, data_test_fold)
              nl_cv_results[i] <- ceres_ns("caret")$RMSE(yhat_fold, data_test_fold[[lhs_name]])
              nl_cv_details[[i]] <- data.frame("Selected Row" = rep(names(cv_indices)[i], length(yhat_fold)), "Name" = row.names(data_test_fold), "Predicted" = yhat_fold, "Actual" = data_test_fold[[lhs_name]])
            } else { nl_cv_results[i] <- NA }
            incProgress(1/length(cv_indices), detail = paste(round(100*(i/length(cv_indices)),3),"% Complete", sep = ""))
          }
        })
        n_failed_cv <- sum(is.na(nl_cv_results))
        if (n_failed_cv > 0.2 * length(cv_indices)) {
          showNotification(
            paste0(n_failed_cv, " of ", length(cv_indices), " CV folds failed to converge. ",
                   "CV statistics are based on the remaining ", length(cv_indices) - n_failed_cv, " folds only."),
            type = "warning", duration = 15
          )
        }

      } else if (input$nl_error_term == "MUPE") {
        est_coeff <- tryCatch(
          nl_fit_eq(data_train, "MUPE"),
          error = function(e) {
            if (grepl("singular|step factor", e$message, ignore.case = TRUE)) stop(nl_cb_fail_message(e$message, input$nl_equation))
            else stop(e$message)
          })

        yhat <- predict(est_coeff, data_train)
        w_vec <- 1/pmax(abs(yhat), 1e-10)^2
        Z <- get_derivative(formula_test, data_train, coef(est_coeff))
        SSPE <- sum(((data_train[[lhs_name]] - yhat) / pmax(abs(yhat), 1e-10))^2)
        root_MSE <- sqrt(SSPE/(nrow(data_train) - length(coef(est_coeff))))

        possible_error <- try(scaled_ginv(crossprod(Z * sqrt(w_vec))), silent = TRUE)
        if(inherits(possible_error, "try-error")) stop("Gradient matrix inversion failed during MUPE variance calculation.")
        var_covar <- root_MSE^2 * possible_error
        yhat_test <- predict(est_coeff, data_test)

        best_coeff_cv <- as.list(coef(est_coeff))
        if (nl_run_cv) withProgress(message = 'Performing Cross Validation', value = 1, {
          for (i in seq_along(cv_indices)){
            data_train_fold <- data_train[cv_indices[[i]],]; data_test_fold <- data_train[-cv_indices[[i]],]
            mupe_coeff_fold <- tryCatch(fit_mupe(formula_test, data_train_fold, best_coeff_cv), error = function(e) NULL)
            if (is.null(mupe_coeff_fold)) mupe_coeff_fold <- tryCatch(nl_fit_eq(data_train_fold, "MUPE"), error = function(e) NULL)
            if(!is.null(mupe_coeff_fold)) {
              yhat_fold <- predict(mupe_coeff_fold, data_test_fold)
              nl_cv_results[i] <- sqrt(sum(((data_test_fold[[lhs_name]] - yhat_fold) / pmax(abs(yhat_fold), 1e-10))^2)/(length(yhat_fold)))
              nl_cv_details[[i]] <- data.frame("Selected Row" = rep(names(cv_indices)[i], length(yhat_fold)), "Name" = row.names(data_test_fold), "Predicted" = yhat_fold, "Actual" = data_test_fold[[lhs_name]])
            } else { nl_cv_results[i] <- NA }
            incProgress(1/length(cv_indices), detail = paste(round(100*(i/length(cv_indices)),3),"% Complete", sep = ""))
          }
        })
        n_failed_cv <- sum(is.na(nl_cv_results))
        if (n_failed_cv > 0.2 * length(cv_indices)) {
          showNotification(
            paste0(n_failed_cv, " of ", length(cv_indices), " CV folds failed to converge. ",
                   "CV statistics are based on the remaining ", length(cv_indices) - n_failed_cv, " folds only."),
            type = "warning", duration = 15
          )
        }
      }
      
      TSS <- sum((data_train[[lhs_name]] - mean(data_train[[lhs_name]]))^2)
      SSE_metrics <- sum((data_train[[lhs_name]] - yhat)^2)
      pseudo_r2 <- 1 - (SSE_metrics/TSS)
      n_train   <- nrow(data_train)
      p_coeff   <- length(coef(est_coeff))
      adj_pseudo_r2 <- 1 - (1 - pseudo_r2) * (n_train - 1) / (n_train - p_coeff)
      t_vals <- summary(est_coeff)$parameters[, "t value"]
      prop_sig <- mean(abs(t_vals) >= 2)
      ybar_nl <- mean(data_train[[lhs_name]])
      cv_val  <- if (is.finite(ybar_nl) && abs(ybar_nl) > 1e-12) root_MSE / abs(ybar_nl) else NA_real_
      actuals  <- data_train[[lhs_name]]
      mape_val <- mean(abs(actuals - yhat) / pmax(abs(actuals), 1e-10))

      results <- data.frame(
        Num_Variables = length(x_vars),
        R_Squared = pseudo_r2,
        Adj_R_Squared = adj_pseudo_r2,
        Standard_Error = root_MSE,
        Coeff_Significance = prop_sig,
        CV = cv_val,
        MAPE = mape_val
      )
      colnames(results) <- c("# Variables", "R-Squared", "Adj. R-Squared", "Standard Error", "Coeff Significance (%)", "CV (%)", "MAPE (%)")
      rownames(results) <- input$nl_equation
      rv$nl_results <- results
      
      diag_vcov <- diag(var_covar)
      if (any(diag_vcov < 0)) {
        showNotification(
          "Warning: The variance-covariance matrix for this model contains negative diagonal values. This indicates numerical instability in the Hessian approximation — typically caused by an overparameterized equation, near-collinear parameters, or convergence to a saddle point. Standard errors and confidence intervals shown below may be unreliable.",
          type = "warning", duration = 15
        )
        diag_vcov <- abs(diag_vcov)
      }
      se_coeff <- sqrt(diag_vcov)
      t_mult <- qt(0.975, nrow(data_train) - length(coef(est_coeff)))
      coeff_result <- cbind(coef(est_coeff), se_coeff, coef(est_coeff) - se_coeff*t_mult, coef(est_coeff) + se_coeff*t_mult)
      
      names(yhat) <- row.names(data_train)
      names(yhat_test) <- row.names(data_test)
      rv$nl_coeffs <- coeff_result
      rv$nl_model <- est_coeff
      rv$nl_cv_results <- nl_cv_results
      rv$nl_cv_indices <- cv_indices
      rv$nl_cv_details <- nl_cv_details
      rv$nl_sets <- list("yhat_train" = yhat, "yhat_test" = yhat_test, "y_train" = data_train[[lhs_name]], "y_test" = data_test[[lhs_name]], "data_train" = data_train)
      rv$train_index <- train_index
      rv$nl_fit_log_y             <- applied_log_y
      rv$nl_fit_log_x_vars        <- applied_log_x_vars
      rv$nl_custom_fitted_formula <- NULL
      rv$nl_fit_response   <- input$nl_response
      rv$nl_fit_eq_name    <- input$nl_equation
      rv$nl_fit_formula    <- formula_test
      rv$nl_fit_predictors <- nl_orig_x_vars
      rv$nl_fit_error_term <- input$nl_error_term

    }, error = function(e) {
      msg <- e$message
      friendly <- if (grepl("element.*is zero|singular gradient|singular matrix|computationally singular", msg, ignore.case = TRUE)) {
        paste0("The '", input$nl_equation, "' curve could not be identified with this data. ",
               "This usually means the data does not follow the selected curve shape, or the ",
               "starting values led to a degenerate solution. Try: (1) choosing a different curve, ",
               "(2) checking that X values are positive and span a meaningful range, ",
               "(3) ensuring you have at least 3-4x more data points than model parameters.")
      } else if (grepl("step factor|maxiter|Number of iterations", msg, ignore.case = TRUE)) {
        paste0("'", input$nl_equation, "' did not converge within 500 iterations. ",
               "Consider choosing a simpler curve or verifying the data trend.")
      } else {
        paste0("NLS Fit Failed: ", msg)
      }
      showNotification(friendly, type = "error", duration = 12)
      return(NULL)
    })
  })
  
  output$nl_coeffs <- renderDT({
    req(rv$active_dataset)
    req(rv$nl_coeffs)
    
    nl_coeffs_clean <- rv$nl_coeffs
    rownames(nl_coeffs_clean) <- unbt(rownames(nl_coeffs_clean))
    
    datatable(nl_coeffs_clean, selection = "none", colnames = c("Coefficients" = 1, "Est. Value" = 2, "Asym. SE" = 3, "Approx. 95% Low" = 4, "Approx. 95% High" = 5), options = list(dom = "t", pageLength = nrow(nl_coeffs_clean))) %>%
      formatCurrency("Est. Value",currency = "", interval = 3, mark = ",", digits = 5) %>%
      formatCurrency("Asym. SE",currency = "", interval = 3, mark = ",", digits = 5) %>%
      formatCurrency("Approx. 95% Low",currency = "", interval = 3, mark = ",", digits = 5) %>%
      formatCurrency("Approx. 95% High",currency = "", interval = 3, mark = ",", digits = 5)
  })

  output$nl_results <- renderDT({
    req(rv$active_dataset)
    req(rv$nl_results)

    tmp <- rv$nl_results

    pct_cols <- intersect(c("Coeff Significance (%)", "CV (%)", "MAPE (%)"), colnames(tmp))
    tmp[, pct_cols] <- tmp[, pct_cols] * 100

    r2_cols <- intersect(c("R-Squared", "Adj. R-Squared"), colnames(tmp))
    tmp[, r2_cols] <- tmp[, r2_cols] * 100

    hdr <- if (!is.null(rv$nl_cv_results)) "In-sample fit (10-fold CV: click the row)" else "In-sample fit"
    datatable(round(tmp,3), options = list(dom = "t", ordering = FALSE), colnames = setNames(1L, hdr), selection = "single") %>%
      formatString(pct_cols, suffix = "%") %>%
      formatString(r2_cols, suffix = "%")

  })

  observeEvent(input$nl_results_rows_selected,{
    sync_cer_generation_data()

    update_dialog <- modalDialog(
      div(
        tabBox(width = 12,
               tabPanel("Error Distribution",
                        withSpinner(plotlyOutput("nl_cv_boxplot", height = 450),
                                    color = getOption("spinner.color", default = primary_color()),
                                    type = 1)
               ),
               tabPanel("Error by Fold",
                        fluidRow(
                          column(width = 6,
                                 div(align = "center", h4("Cross-Validation by Fold")),
                                 DTOutput("nl_resample_table")
                          ),
                          column(width = 6,
                                 div(align = "center", h4("Held Out Data from Fold")),
                                 DTOutput("nl_single_fold_table")
                          )
                        )
               )
        )
      ),
      title = "Cross Validation Results", size = c("xl"), easyClose = TRUE, footer = "", fade = TRUE)
    showModal(update_dialog)

  })

  output$nl_cv_boxplot <- renderPlotly({
    validate(
      need(!is.null(rv$nl_cv_results),
        "Cross-validation was not run for this fit. Select a CV holdout (5-Fold, 10-Fold, or LOOCV) under 'Select Data to Train On' and re-fit to see fold metrics."),
      need(any(is.finite(rv$nl_cv_results)),
        "Cross-validation ran, but every fold failed to converge — no fold metrics to plot. Try different start values or a simpler equation.")
    )

    data <- rv$nl_cv_results

    if (input$nl_error_term == "Least Squares"){
      plot_name <- "RMSE by Fold"
      title_name <- "\nFinal Model RMSE by Fold"
    } else if (input$nl_error_term == "MUPE") {
      plot_name <- "SPE by Fold"
      title_name <- "\nFinal Model SPE by Fold"
    }

    boxplot <- plot_ly(x = data, name = plot_name, type = "box", boxpoints = "all", jitter = 0.3, fillcolor = secondary_color(), pointpos = 0, boxmean = TRUE,
                       marker = list(color = secondary_color(), line = list(color = primary_color())),
                       line = list(color = primary_color()))

    histogram <- plot_ly(x = data, type = "histogram", name = plot_name,
                         marker = list(color = secondary_color(), line = list(color = primary_color(), width = 2)))

    subplot(boxplot,histogram,nrows = 2, shareX = TRUE) %>%  layout(title = title_name,
                                                                    xaxis = list(title = "", zeroline = FALSE),
                                                                    yaxis = list(title = "", zeroline = FALSE),
                                                                    showlegend = FALSE) %>% config(displayModeBar = FALSE)
  })

  output$nl_resample_table <- renderDT({
    validate(
      need(!is.null(rv$nl_cv_results),
        "Cross-validation was not run for this fit — select a CV holdout and re-fit."),
      need(any(is.finite(rv$nl_cv_results)),
        "Cross-validation ran, but every fold failed to converge — no fold metrics to show.")
    )
    req(rv$nl_cv_indices)

    tmp <- data.frame(names(rv$nl_cv_indices), round(rv$nl_cv_results,5))

    datatable(tmp, colnames = c("Fold" = 1, "Error" = 2), rownames = FALSE, selection = "single", options = list(dom = "tip"))

  }, server = FALSE)

  output$nl_single_fold_table <- renderDT({
    req(rv$nl_cv_details)
    req(input$nl_resample_table_rows_selected)

    tmp <- rv$nl_cv_details[[input$nl_resample_table_rows_selected]]

    if (is.null(tmp) || !is.data.frame(tmp) || nrow(tmp) == 0) {
      return(datatable(
        data.frame(Message = "This fold failed to fit — no per-row predictions available."),
        rownames = FALSE, selection = "none", options = list(dom = "t")))
    }

    datatable(tmp, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = nrow(tmp))) %>%
      formatCurrency('Predicted',currency = "", interval = 3, mark = ",", digits = 5) %>%
      formatCurrency('Actual',currency = "", interval = 3, mark = ",", digits = 5)

  }, server = FALSE)

  output$nl_actuals_vs_predicted <- renderPlotly({
    req(rv$nl_sets)

    yhat <- rv$nl_sets[["yhat_train"]]
    y <- rv$nl_sets[["y_train"]]
    yhat_test <- rv$nl_sets[["yhat_test"]]
    y_test <- rv$nl_sets[["y_test"]]

    p <- plot_ly(x = yhat, y = y, type = "scatter", mode = "markers", marker = list(color = primary_color(), size = 10), name = "Training Data",
                 text = paste0("Row Name: ", names(yhat), "<br>Actual: ", round(y, 3), "<br>Predicted: ", round(yhat, 3)),
                 hoverinfo = "text") %>%
      add_trace(inherit = FALSE, x = sort(y), y = sort(y), type = "scatter", mode = "lines", line = list(color = "#000000", dash = 'dash', width = 2), name = "y = x", hoverinfo = "none") %>%
      layout(title = input$nl_response, xaxis = list(title = "Predicted Values"), yaxis = list(title = "Actuals")) %>%
      config(displayModeBar = FALSE)

    if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[2:4]){
      p <- p %>% add_trace(inherit = FALSE, x = yhat_test, y = y_test, type = "scatter", mode = "markers", marker = list(color = graph_color_1, size = 10), name = "Test Data",
                           text = paste0("Row Name: ", names(yhat_test), "<br>Actual: ", round(y_test, 3), "<br>Predicted: ", round(yhat_test, 3)),
                           hoverinfo = "text")
    }

    p
  })

  output$nl_residuals_vs_predicted <- renderPlotly({
    req(rv$nl_sets)

    yhat <- rv$nl_sets[["yhat_train"]]
    epsilon <- rv$nl_sets[["y_train"]] - yhat
    yhat_test <- rv$nl_sets[["yhat_test"]]
    epsilon_test <- rv$nl_sets[["y_test"]] - yhat_test

    p <- plot_ly(x = yhat, y = epsilon, type = "scatter", mode = "markers", marker = list(color = primary_color(), size = 10), name = "Train Residuals",
                 text = paste0("Row Name: ", names(yhat), "<br>Predicted: ", round(yhat, 3), "<br>Residual: ", round(epsilon, 3)),
                 hoverinfo = "text") %>%
      layout(title = paste("Residuals vs Predicted:", input$nl_response), xaxis = list(title = "Predicted Values"), yaxis = list(title = "Residuals", zeroline = FALSE),
             shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0, line = list(color = "rgba(100,116,139,0.7)", width = 1.5, dash = "dash")))) %>%
      config(displayModeBar = FALSE)

    if (input$ols_holdout %in% OLS_HOLDOUT_CHOICES[2:4]){
      p <- p %>% add_trace(inherit = FALSE, x = yhat_test, y = epsilon_test, type = "scatter", mode = "markers", marker = list(color = graph_color_1, size = 10), name = "Test Residuals",
                           text = paste0("Row Name: ", names(yhat_test), "<br>Predicted: ", round(yhat_test, 3), "<br>Residual: ", round(epsilon_test, 3)),
                           hoverinfo = "text")
    }

    p
  })

  output$nl_residual_normality <- renderPlotly({
    req(rv$nl_sets)

    yhat <- rv$nl_sets[["yhat_train"]]
    y <- rv$nl_sets[["y_train"]]
    epsilon <- y - yhat

    fit <- density(epsilon)
    emp_normal <- dnorm(fit$x, mean = 0, sd = sd(epsilon))

    p1 <- plot_ly(x = epsilon, type = "histogram", name = "Resid. Histogram", nbinsx = max(5L, floor(sqrt(length(epsilon)))), marker = list(color = secondary_color(),line = list(color = primary_color(), width = 2))) %>%
      add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Resid. Density", inherit = FALSE, line = list(color = primary_color(), width = 2.5), fillcolor = paste0(primary_color(), "1A")) %>%
      add_trace(x = fit$x, y = emp_normal, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Normal Density", inherit = FALSE, line = list(color = graph_color_2, width = 2, dash = "dash"), fillcolor = "rgba(0,0,0,0)") %>%
      layout(title = "Histogram/Density Plot", xaxis = list(title = "Residuals"), yaxis = list(title = "Frequency"), yaxis2 = list(showticklabels = FALSE, showgrid = FALSE, overlaying = "y", side = "right")) %>%
      config(displayModeBar = FALSE)

    sorted_data <- sort(epsilon)
    n <- length(sorted_data)
    percentiles <- (seq_along(sorted_data) - 0.5)/length(sorted_data)
    theoretical_quantiles <- qnorm(percentiles)
    k <- 0.895/(sqrt(n) * (1 - 0.01/sqrt(n) + 0.85/n))
    LCL <- qnorm(pmax(percentiles - k, 1e-10))
    UCL <- qnorm(pmin(percentiles + k, 1 - 1e-10))

    equality_data <- data.frame(x = c(qnorm(0.25), qnorm(0.75)), y = as.numeric(quantile(epsilon,c(0.25, 0.75))))
    fit <- lm(y ~ x, data = equality_data)

    domain <- seq(from = min(theoretical_quantiles), to = max(theoretical_quantiles), length.out = 100)

    new_data <- data.frame(x = domain)
    yhat <- predict(fit, new_data)

    p2 <- plot_ly(x = theoretical_quantiles, y = sorted_data, type = "scatter", mode = "markers", marker = list(color = primary_color()), name = "Data") %>%
      add_trace(x = domain, y = yhat, type = "scatter", mode = "lines", name = "Expected Quantiles", inherit = FALSE, line = list(color = graph_color_2, width = 2, dash = "dash")) %>%
      add_trace(x = UCL, y = sorted_data, type = "scatter", mode = "lines", name = "Upper 95% CI", inherit = FALSE, line = list(color = "rgba(100,116,139,0.6)", width = 1.2, dash = "dot")) %>%
      add_trace(x = LCL, y = sorted_data, type = "scatter", mode = "lines", name = "Lower 95% CI", inherit = FALSE, line = list(color = "rgba(100,116,139,0.6)", width = 1.2, dash = "dot")) %>%
      layout(title = "Q-Q Plot", xaxis = list(title = "Theoretical Normal Quantiles", range = range(theoretical_quantiles) + c(-0.35, 0.35)), yaxis = list(title = "Sample Quantiles")) %>%
      config(displayModeBar = FALSE)

    subplot(p1, p2, nrows = 1, titleX = TRUE, titleY = TRUE) %>% layout(showlegend = FALSE, title = "Residual Normality")
  })

  nl_pred_ids <- function(vars) paste0("nl_predin_", make.unique(gsub("[^A-Za-z0-9_.]", "_", vars), sep = "_"))

  output$nl_predictions <- renderUI({
    req(rv$nl_model)
    pred_vars <- rv$nl_fit_predictors %||% nl_active_predictors()
    req(length(pred_vars) > 0)

    dt_nl      <- rv$nl_sets$data_train
    req(!is.null(dt_nl))
    log_x_vars <- rv$nl_fit_log_x_vars %||% character(0)

    out <- tagList()
    ids <- nl_pred_ids(pred_vars)
    for (i in seq_along(pred_vars)) {
      var <- pred_vars[i]
      sv  <- sanitize_nl_colname(var)
      col <- dt_nl[[sv]]
      req(!is.null(col))
      if (is.numeric(col)) {
        med <- median(col, na.rm = TRUE)
        if (sv %in% log_x_vars) med <- exp(med)
        out[[i]] <- numericInput(
          ids[i], var,
          value = if (is.finite(med) && med >= 1) round(med, 0) else med
        )
      } else {
        most_common <- names(sort(table(col), decreasing = TRUE))[1]
        out[[i]] <- selectInput(
          ids[i], var,
          choices  = levels(col) %||% sort(unique(as.character(col))),
          selected = most_common
        )
      }
    }
    out
  })

  output$nl_predict_table <- renderDT({
    req(rv$nl_model)
    pred_vars <- rv$nl_fit_predictors %||% nl_active_predictors()
    req(length(pred_vars) > 0)

    san_vars <- vapply(pred_vars, sanitize_nl_colname, character(1))

    dt_nl <- rv$nl_sets$data_train
    req(!is.null(dt_nl))
    new_data <- data.frame(
      lapply(seq_along(pred_vars), function(i) {
        id       <- nl_pred_ids(pred_vars)[i]
        req(!is.null(input[[id]]))
        val <- input[[id]]
        col <- dt_nl[[san_vars[i]]]
        if (is.numeric(col)) as.numeric(val)
        else factor(val, levels = levels(col) %||% sort(unique(as.character(col))))
      }),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
    colnames(new_data) <- san_vars

    if (nrow(new_data) != 1) return(NULL)

    log_x_vars <- rv$nl_fit_log_x_vars %||% character(0)
    log_y_fit  <- isTRUE(rv$nl_fit_log_y)
    for (cn in intersect(colnames(new_data), log_x_vars)) {
      v <- new_data[[cn]]
      new_data[[cn]] <- if (is.numeric(v) && all(v > 0, na.rm = TRUE)) log(v) else NA_real_
    }

    estimate <- tryCatch(
      as.numeric(predict(rv$nl_model, newdata = new_data))[[1]],
      error = function(e) NA_real_
    )
    req(!is.na(estimate))
    if (log_y_fit) estimate <- exp(estimate)

    fit_eq <- rv$nl_fit_eq_name %||% input$nl_equation
    formula_str <- if (identical(fit_eq, "Custom") && !is.null(rv$nl_custom_fitted_formula))
      rv$nl_custom_fitted_formula else (rv$nl_fit_formula %||% nlls_formula_string())
    req(formula_str != "Awaiting variables...")

    fit_err <- rv$nl_fit_error_term %||% input$nl_error_term
    pi_fn <- if (identical(fit_err, "Least Squares")) nlls_pi else mupe_pi

    pi90 <- tryCatch(pi_fn(formula_str, rv$nl_model, new_data, alpha = 0.10), error = function(e) NULL)
    pi95 <- tryCatch(pi_fn(formula_str, rv$nl_model, new_data, alpha = 0.05), error = function(e) NULL)

    get_bound <- function(res, col) {
      if (!is.null(res) && is.matrix(res) && col %in% colnames(res)) res[1, col] else NA_real_
    }
    pi_bound <- function(res, col) {
      b <- get_bound(res, col)
      if (log_y_fit) exp(b) else b
    }

    results <- matrix(
      c(estimate,
        pi_bound(pi90, "lwr"), pi_bound(pi90, "upr"),
        pi_bound(pi95, "lwr"), pi_bound(pi95, "upr")),
      nrow = 1
    )
    colnames(results) <- c("Estimate", "90% Low PI", "90% High PI", "95% Low PI", "95% High PI")
    rownames(results) <- fit_eq

    datatable(results,
              colnames  = c("", "Estimate", "90% Low PI", "90% High PI", "95% Low PI", "95% High PI"),
              options   = list(dom = "t", ordering = FALSE),
              selection = "none") %>%
      formatCurrency(c("Estimate", "90% Low PI", "90% High PI", "95% Low PI", "95% High PI"),
                     currency = "", interval = 3, mark = ",", digits = 3)
  })

  output$nl_pred_chart <- renderPlotly({
    req(rv$nl_model, rv$nl_sets)
    pred_vars <- rv$nl_fit_predictors %||% nl_active_predictors()
    fit_resp  <- rv$nl_fit_response   %||% input$nl_response
    req(length(pred_vars) > 0, !is.null(fit_resp))

    y_train    <- rv$nl_sets[["y_train"]]
    yhat_train <- rv$nl_sets[["yhat_train"]]
    san_vars   <- vapply(pred_vars, sanitize_nl_colname, character(1))

    input_vals <- setNames(
      lapply(nl_pred_ids(pred_vars), function(id) input[[id]]),
      pred_vars
    )
    req(!any(vapply(input_vals, is.null, logical(1L))))

    dt_nl <- rv$nl_sets$data_train
    req(!is.null(dt_nl))
    new_data <- data.frame(
      lapply(seq_along(pred_vars), function(i) {
        var <- pred_vars[i]
        val <- input_vals[[var]]
        col <- dt_nl[[san_vars[i]]]
        if (is.numeric(col)) as.numeric(val)
        else factor(val, levels = levels(col) %||% sort(unique(as.character(col))))
      }),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
    colnames(new_data) <- san_vars

    if (nrow(new_data) != 1) return(NULL)

    log_x_vars <- rv$nl_fit_log_x_vars %||% character(0)
    log_y_fit  <- isTRUE(rv$nl_fit_log_y)
    for (cn in intersect(colnames(new_data), log_x_vars)) {
      v <- new_data[[cn]]
      new_data[[cn]] <- if (is.numeric(v) && all(v > 0, na.rm = TRUE)) log(v) else NA_real_
    }
    y_new <- tryCatch(as.numeric(predict(rv$nl_model, newdata = new_data))[[1]], error = function(e) NA_real_)
    req(!is.na(y_new))
    if (log_y_fit) y_new <- exp(y_new)

    pt_color <- default_colorway[2]

    if (length(pred_vars) == 1) {
      x_col   <- san_vars[1]
      x_log   <- x_col %in% log_x_vars
      x_train <- rv$nl_sets[["data_train"]][[x_col]]

      x_seq    <- seq(min(x_train, na.rm = TRUE), max(x_train, na.rm = TRUE), length.out = 200)
      curve_nd <- setNames(data.frame(x_seq), x_col)
      y_curve  <- tryCatch(predict(rv$nl_model, newdata = curve_nd), error = function(e) NULL)

      if (x_log)   { x_train <- exp(x_train); x_seq <- exp(x_seq) }
      if (log_y_fit) y_train <- exp(y_train)
      if (!is.null(y_curve) && log_y_fit) y_curve <- exp(y_curve)

      x_new <- as.numeric(input_vals[[pred_vars[1]]])

      row_labels <- paste0("Row Name: ", names(yhat_train), "<br>",
                            pred_vars[1], ": ", round(x_train, 3), "<br>",
                            fit_resp, ": ", round(y_train, 3))

      p <- plot_ly() %>%
        add_trace(x = x_train, y = y_train,
                  type = "scatter", mode = "markers",
                  marker = list(color = primary_color(), size = 9),
                  text = row_labels, hoverinfo = "text",
                  name = "Training Data")

      if (!is.null(y_curve)) {
        p <- p %>% add_trace(x = x_seq, y = y_curve,
                             type = "scatter", mode = "lines",
                             line = list(color = "#555555", width = 2),
                             hoverinfo = "none",
                             name = "Fitted Curve")
      }

      p <- p %>%
        add_trace(x = x_new, y = y_new,
                  type = "scatter", mode = "markers",
                  marker = list(color = pt_color, size = 14, symbol = "star",
                                line = list(color = "#FFFFFF", width = 1.5)),
                  text = "New Estimate", hoverinfo = "text",
                  name = "New Estimate") %>%
        layout(title  = paste(fit_resp, "vs", pred_vars[1]),
               xaxis  = list(title = pred_vars[1]),
               yaxis  = list(title = fit_resp)) %>%
        config(displayModeBar = FALSE)

    } else {
      if (log_y_fit) { y_train <- exp(y_train); yhat_train <- exp(yhat_train) }
      ref_range <- range(c(y_train, yhat_train, y_new), na.rm = TRUE)
      row_labels <- paste0("Row Name: ", names(yhat_train), "<br>",
                            "Predicted: ", round(yhat_train, 3), "<br>",
                            "Actual: ", round(y_train, 3))

      p <- plot_ly() %>%
        add_trace(x = yhat_train, y = y_train,
                  type = "scatter", mode = "markers",
                  marker = list(color = primary_color(), size = 9),
                  text = row_labels, hoverinfo = "text",
                  name = "Training Data") %>%
        add_trace(x = ref_range, y = ref_range,
                  type = "scatter", mode = "lines",
                  line = list(color = "#000000", dash = "dash", width = 1.5),
                  hoverinfo = "none",
                  name = "y = x") %>%
        add_trace(x = y_new, y = y_new,
                  type = "scatter", mode = "markers",
                  marker = list(color = pt_color, size = 14, symbol = "star",
                                line = list(color = "#FFFFFF", width = 1.5)),
                  text = "New Estimate", hoverinfo = "text",
                  name = "New Estimate") %>%
        layout(title  = paste("Actuals vs Predicted:", fit_resp),
               xaxis  = list(title = "Predicted"),
               yaxis  = list(title = "Actual")) %>%
        config(displayModeBar = FALSE)
    }

    p
  })

  observeEvent(list(input$ols_response, input$ols_predictors, input$ols_holdout, input$ols_penalty, input$ols_interact, input$loglin_interact, input$linear_simple_interact, input$log_linear_error, input$linear_error, input$data_select_cer_generation),{
    
    rv$ols_model <- NULL
    rv$ols_results <- NULL
    rv$ols_table_second <- NULL
    rv$model_list_linear_simple <- NULL
    rv$predictors_linear_simple <- NULL
    rv$yhat_test_linear_simple <- NULL
    rv$y_test_second <- NULL
    rv$ols_table          <- NULL
    rv$model_list_loglin  <- NULL
    rv$predictors_loglin  <- NULL
    rv$yhat_test_loglin   <- NULL
    rv$y_test             <- NULL
    rv$loglin_influence_data  <- NULL
    rv$loglin_influence_n_par <- NULL
    rv$loglin_ui_exclusions   <- character(0)
    rv$linear_influence_data  <- NULL
    rv$linear_influence_n_par <- NULL
    rv$linear_ui_exclusions   <- character(0)
    rv$loglin_ui_exclusions_by_model   <- NULL
    rv$loglin_ui_exclusions_last_model <- NULL
    rv$loglin_tuning_pushed_model      <- NULL
    rv$linear_ui_exclusions_by_model   <- NULL
    rv$linear_ui_exclusions_last_model <- NULL
    rv$linear_tuning_pushed_model      <- NULL

    DT::selectRows(DT::dataTableProxy("selected_loglin_models"), NULL)
    DT::selectRows(DT::dataTableProxy("selected_linear_simple_models"), NULL)

    removeUI(selector = "#ols_train")
    insertUI(selector = "#ols_train_bttn", ui = div(align = "center", actionButton("ols_train", "Run OLS Model")))
    
    removeUI(selector = "#loglin_train")
    insertUI(selector = "#best_subsets_train_bttn", ui = div(align = "center", actionButton("loglin_train", "Run Log-Linear Model")))
    
    removeUI(selector = "#linear_simple_train")
    insertUI(selector = "#best_subsets_train_second_bttn", ui = div(align = "center", actionButton("linear_simple_train", "Run Linear Model")))
    
  })
  
  observeEvent(list(input$nl_error_term, input$ols_holdout),{
    
    rv$nl_results <- NULL
    rv$nl_coeffs <- NULL
    
    removeUI(selector = "#nl_train")
    insertUI(selector = "#nl_train_bttn", ui = div(align = "center", actionButton("nl_train", "Run Non-Linear Model")))
    
  })
  
  n_decimals <- 4

  XL_NUM <- paste0("0.", strrep("0", n_decimals))
  XL_PCT <- paste0(XL_NUM, "%")
  XL_SCI <- paste0(XL_NUM, "E+00")
  XL_INT <- "0"
  xl_eqn_fmt <- function(x) ifelse(is.finite(x) & x != 0 & (abs(x) < 1e-3 | abs(x) >= 1e6), XL_SCI, XL_NUM)

  xl_numfmt <- function(df, ...) {
    f <- attr(df, "xl_numfmt") %||% list()
    a <- list(...)
    f[names(a)] <- a
    attr(df, "xl_numfmt") <- f
    df
  }
  xl_numfmt_numeric <- function(df, fmt) {
    if (is.null(df) || !ncol(df)) return(df)
    cols <- names(df)[vapply(df, is.numeric, logical(1L))]
    do.call(xl_numfmt, c(list(df), setNames(as.list(rep(fmt, length(cols))), cols)))
  }

  fmt_num <- function(x) if (is.numeric(x)) as.numeric(x) else x
  fmt_pct <- function(x) fmt_num(x)

  fmt_eqn <- function(x) {
    if(is.na(x)) return("")
    abs_x <- abs(x)
    if(abs_x == 0) return("0")
    if(abs_x >= 1e6 || abs_x < 1e-3) {
      formatC(abs_x, format = "e", digits = n_decimals)
    } else {
      formatC(abs_x, format = "f", digits = n_decimals)
    }
  }

  clean_df <- function(df) {
    for (j in seq_along(df)) {
      v <- df[[j]]
      if (is.numeric(v)) v[!is.finite(v)] <- NA
      else if (is.character(v)) v[!is.na(v) & v == "NA"] <- NA
      df[[j]] <- v
    }
    df
  }

  style_range_borders <- function(wb, rows, cols) {
    if(length(rows) == 0 || length(cols) == 0) return(wb)

    first_row <- min(rows); last_row  <- max(rows)
    first_col <- min(cols); last_col  <- max(cols)
    body_rows <- rows[rows < last_row]
    body_cols <- cols[cols < last_col]

    if(length(body_rows) > 0 && length(body_cols) > 0) {
      wb$add_border(dims = ceres_ns("openxlsx2")$wb_dims(rows = body_rows, cols = body_cols),
                    bottom_border = "thin", top_border = "thin", left_border = "thin", right_border = "thin",
                    inner_hgrid = "thin", inner_vgrid = "thin")
    }
    if(length(body_rows) > 0) {
      wb$add_border(dims = ceres_ns("openxlsx2")$wb_dims(rows = body_rows, cols = last_col),
                    right_border = "thick", top_border = "thin", bottom_border = "thin", left_border = "thin",
                    inner_hgrid = "thin")
    }
    if(length(body_cols) > 0) {
      wb$add_border(dims = ceres_ns("openxlsx2")$wb_dims(rows = last_row, cols = body_cols),
                    bottom_border = "thick", top_border = "thin", left_border = "thin", right_border = "thin",
                    inner_vgrid = "thin")
    }
    wb$add_border(dims = ceres_ns("openxlsx2")$wb_dims(rows = last_row, cols = last_col),
                  bottom_border = "thick", right_border = "thick", top_border = "thin", left_border = "thin")
    return(wb)
  }

  style_table_borders <- function(wb, sheet, start_row, start_col, df) {
    if(is.null(df) || nrow(df) == 0) return(wb)

    n_rows <- nrow(df)
    n_cols <- ncol(df)

    rows_all <- start_row:(start_row + n_rows)
    cols_all <- start_col:(start_col + n_cols - 1)

    row_header <- start_row
    rows_body  <- (start_row + 1):(start_row + n_rows)

    col_first <- start_col
    cols_data <- (start_col + 1):(start_col + n_cols - 1)

    style_range_borders(wb, rows_all, cols_all)

    wb$add_font(dims = ceres_ns("openxlsx2")$wb_dims(rows = row_header, cols = cols_all), bold = "1")

    if(length(rows_body) > 0) {
      wb$add_font(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_body, cols = col_first), bold = "1")
    }

    if(length(cols_data) > 0) {
      wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = row_header, cols = cols_data), horizontal = "center")
    }

    if(length(rows_body) > 0 && length(cols_data) > 0) {
      wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_body, cols = cols_data), horizontal = "right")
    }

    fmts <- attr(df, "xl_numfmt")
    for (cn in intersect(names(fmts), names(df))) {
      fm  <- rep_len(fmts[[cn]], n_rows)
      col <- start_col + match(cn, names(df)) - 1L
      run_start <- 1L
      for (i in seq_len(n_rows)) {
        if (i == n_rows || fm[i + 1L] != fm[i]) {
          wb$add_numfmt(dims = ceres_ns("openxlsx2")$wb_dims(rows = start_row + (run_start:i), cols = col), numfmt = fm[i])
          run_start <- i + 1L
        }
      }
    }

    return(wb)
  }

  cer_report_dt_cell <- function(col, x) {
    if (!is.numeric(x)) return(gsub("<[^>]+>", "", as.character(x)))
    if (is.na(x)) return("—")
    pct3 <- c("R-Squared", "Adj. R-Squared", "Coeff Significance (%)", "Geometric CV",
              "Multiplicative Error", "CV (%)", "MAPE (%)")
    if (col %in% pct3) return(paste0(formatC(x * 100, format = "f", digits = 3, big.mark = ","), "%"))
    if (col %in% c("Standard Error", "Weighted IC (%)"))
      return(formatC(x, format = "f", digits = 3, big.mark = ","))
    if (col == "Max VIF") return(formatC(x, format = "f", digits = 2, big.mark = ","))
    format(x, scientific = FALSE, digits = 15, trim = TRUE)
  }

  cer_report_data_parts <- function(rn, dep_var, preds, fallback, test_rn = character(0),
                                    tuning_excl = character(0), holdout = FALSE) {
    key <- input$data_select_cer_generation %||% ""
    ds  <- rv$dataset_library[[key]]
    matched <- !is.null(ds) && !is.null(rn) && length(rn) > 0L && all(rn %in% rownames(ds))
    get_col <- function(cn) {
      if (matched && cn %in% names(ds)) ds[rn, cn] else fallback(cn)
    }
    cols <- c(dep_var, preds)
    vals <- setNames(lapply(cols, get_col), cols)
    programs <- data.frame(Program = rn %||% as.character(seq_along(vals[[1]])),
                           check.names = FALSE, stringsAsFactors = FALSE)
    for (cn in cols) programs[[cn]] <- ceres_report_fmt_value(vals[[cn]])

    ranges <- do.call(rbind, lapply(preds, function(p) {
      v <- vals[[p]]
      u <- ceres_report_unit(p)
      if (is.numeric(v) && !all(v %in% c(0, 1))) {
        data.frame(Predictor = p, Unit = if (is.na(u)) "—" else u,
                   From = ceres_report_fmt_value(min(v, na.rm = TRUE)),
                   To   = ceres_report_fmt_value(max(v, na.rm = TRUE)),
                   Note = paste0(sum(!is.na(v)), " programs"),
                   check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        tb <- table(as.character(v))
        data.frame(Predictor = p, Unit = "category", From = "—", To = "—",
                   Note = paste0("Values in the data: ",
                                 paste0(names(tb), " (", as.integer(tb), ")", collapse = ", ")),
                   check.names = FALSE, stringsAsFactors = FALSE)
      }
    }))

    excl <- data.frame(Program = character(0), Reason = character(0), stringsAsFactors = FALSE)
    if (length(tuning_excl))
      excl <- rbind(excl, data.frame(Program = tuning_excl,
                                     Reason = "Excluded in CER Tuning (no reason recorded)",
                                     stringsAsFactors = FALSE))
    if (length(test_rn))
      excl <- rbind(excl, data.frame(Program = test_rn,
                                     Reason = "Held out as test data (not used to fit)",
                                     stringsAsFactors = FALSE))
    if (!is.null(ds)) {
      rest <- setdiff(rownames(ds), c(rn, excl$Program))
      if (length(rest)) {
        cc <- intersect(cols, names(ds))
        miss <- if (length(cc)) !stats::complete.cases(ds[rest, cc, drop = FALSE]) else rep(FALSE, length(rest))
        excl <- rbind(excl, data.frame(Program = rest,
                                       Reason = ifelse(miss, "Missing the response or a predictor value",
                                                       if (holdout) "Held out as test data (not used to fit)" else "Not in the fitting data"),
                                       stringsAsFactors = FALSE))
      }
    }
    list(programs = programs, ranges = ranges, excluded = excl, vals = vals,
         matched = matched, dataset = key)
  }

  cer_report_usage <- function(eq_text, dep_var, preds, ranges, category_terms, extra = character(0)) {
    u_resp <- ceres_report_unit(dep_var)
    num_r  <- ranges[ranges$Unit != "category", , drop = FALSE]
    units  <- vapply(preds, function(p) { u <- ceres_report_unit(p); if (is.na(u)) NA_character_ else paste0(p, " in ", u) },
                     character(1))
    units  <- units[!is.na(units)]
    c(paste0("Equation: ", eq_text),
      paste0("The result is ", dep_var,
             if (!is.na(u_resp)) paste0(", in ", u_resp) else "", ". ",
             if (length(units)) paste0("Enter each input in the units of its data column (",
                                       paste(units, collapse = "; "), ").")
             else "Enter each input in the units of its data column."),
      if (length(category_terms))
        paste0("Category terms (", paste(category_terms, collapse = ", "),
               ") are 1 when the program is in that category and 0 otherwise."),
      if (nrow(num_r))
        paste0("Validity range: use this CER only for inputs inside the data it was fitted on - ",
               paste0(num_r$Predictor, " ", num_r$From, " to ", num_r$To, collapse = "; "),
               ". Outside that range the result is an extrapolation."),
      extra)
  }

  cer_report_meta <- function(dataset, model_lbl, err_lbl) {
    v <- ceres_app_version()
    c(Dataset = if (nzchar(dataset)) dataset else "—",
      Model = model_lbl, `Error term` = err_lbl,
      Training = input$ols_holdout %||% "—",
      Date = format(Sys.time(), "%d %b %Y, %H:%M"),
      CERES = if (nzchar(v)) v else "—")
  }

  cer_report_parts_lm <- function(mode) {
    is_log <- identical(mode, "loglin")
    model  <- if (is_log) selected_log_linear_model() else selected_linear_model()
    req(model)
    sel    <- input[[if (is_log) "selected_loglin_models_rows_selected" else "selected_linear_simple_models_rows_selected"]]
    fdf    <- tryCatch(if (is_log) filtered_log_linear_models_data() else filtered_linear_models_data_second(),
                       error = function(e) NULL)
    dep_var <- input$ols_response %||% "y"
    err     <- attr(model, "error_type") %||% "OLS"
    pushed  <- isTRUE(attr(model, "tuning_pushed"))
    log1p_cols <- attr(model, "log1p_cols") %||% character(0)

    mod_sum <- summary(model)
    rownames(mod_sum$coefficients) <- unbt(rownames(mod_sum$coefficients))
    coef_df <- as.data.frame(mod_sum$coefficients)
    fstat   <- mod_sum$fstatistic
    eq_parts <- cer_equation(coef(model), model$model, model$xlevels, dep_var, mode,
                             log1p_cols, cer_fmt_eq, cer_source_cols())
    conv_note <- if (isFALSE(attr(model, "irls_converged")))
      paste0("Did not converge (", attr(model, "irls_iterations"), " iterations) - the coefficients are the last iterate.")
    f_label <- if (!is.null(fstat)) paste0("F(", round(fstat[2], 0), ", ", round(fstat[3], 0), " dim.)") else "F-stat"
    f_val   <- if (!is.null(fstat)) formatC(fstat[1], digits = 3, format = "g") else "—"
    space   <- if (is_log) "log space" else NA_character_
    stats <- data.frame(
      label = c("Observations", "R²", "Adj. R²", "Residual SE", f_label),
      value = c(as.character(nrow(model$model)),
                formatC(mod_sum$r.squared,     digits = 4, format = "f"),
                formatC(mod_sum$adj.r.squared, digits = 4, format = "f"),
                formatC(mod_sum$sigma,         digits = 4, format = "g"),
                f_val),
      sub = c(NA, space, space, space, NA), stringsAsFactors = FALSE)

    vn <- rownames(coef_df)
    coef_tab <- data.frame(Variable = vn, check.names = FALSE, stringsAsFactors = FALSE)
    coef_tab[["Coefficient Estimate"]] <- vapply(coef_df[, 1], cer_fmt_num, character(1))
    if (is_log) coef_tab[["Unit-Space Value"]] <- vapply(vn, function(v) {
      u <- unname(eq_parts$values[v]); if (is.na(u)) "—" else cer_fmt_eq(u) }, character(1))
    coef_tab[["Std. Error"]] <- vapply(coef_df[, 2], cer_fmt_num, character(1))
    coef_tab[["T-value"]]    <- vapply(coef_df[, 3], cer_fmt_num, character(1))
    coef_tab[["P-value"]]    <- vapply(coef_df[, 4], cer_fmt_p,   character(1))
    coef_tab[["Sig."]]       <- vapply(coef_df[, 4], cer_sig_stars, character(1))

    resid_q <- quantile(residuals(model), probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    resid_tab <- as.data.frame(as.list(setNames(vapply(resid_q, cer_fmt_num, character(1)),
                                                c("Min", "1Q", "Median", "3Q", "Max"))),
                               check.names = FALSE, stringsAsFactors = FALSE)
    diag_df <- cer_lm_diagnostics(model)
    diagnostics <- data.frame(Test = diag_df$Test, Result = diag_df$Result,
                              Status = ifelse(is.na(diag_df$pass), "—",
                                              ifelse(diag_df$pass, "Pass", "Check")),
                              Note = diag_df$Note, stringsAsFactors = FALSE)

    metrics <- NULL
    if (!pushed && !is.null(fdf) && length(sel) == 1L && sel <= nrow(fdf)) {
      row <- fdf[sel, , drop = FALSE]
      metrics <- data.frame(label = c("Variable Subsets", names(row)),
                            value = c(gsub("<[^>]+>", "", rownames(row)),
                                      vapply(seq_along(row), function(j) cer_report_dt_cell(names(row)[j], row[[j]]),
                                             character(1))),
                            stringsAsFactors = FALSE)
    }

    mf    <- model$model
    pcols <- setdiff(names(mf)[-1], "(weights)")
    preds <- pcols[!is_interaction_col(pcols, source = cer_source_cols())]
    cer_key    <- input$data_select_cer_generation
    pre_logged <- if (!is.null(cer_key) && nzchar(cer_key)) rv$log_transformed_by_key[[cer_key]] %||% character(0) else character(0)
    fallback <- function(cn) {
      col <- if (identical(cn, dep_var)) mf[[1]] else mf[[cn]]
      if (is.null(col)) return(rep(NA_real_, nrow(mf)))
      if (is_log && is.numeric(col) && !all(col %in% c(0, 1))) {
        if (cn %in% log1p_cols) expm1(col) else if (cn %in% pre_logged) exp(exp(col)) else exp(col)
      } else col
    }
    holdout  <- isTRUE(input$ols_holdout %in% OLS_HOLDOUT_CHOICES[2:4])
    y_test   <- if (holdout) rv[[if (is_log) "y_test" else "y_test_second"]] else NULL
    yhat_test <- NULL
    if (holdout && !is.null(fdf) && length(sel) == 1L) {
      keep_idxs <- match(rownames(fdf), names(rv[[if (is_log) "model_list_loglin" else "model_list_linear_simple"]]))
      keep_idxs <- keep_idxs[!is.na(keep_idxs)]
      if (length(keep_idxs) >= sel)
        yhat_test <- rv[[if (is_log) "yhat_test_loglin" else "yhat_test_linear_simple"]][[keep_idxs[sel]]]
    }
    if (is.null(yhat_test) || length(yhat_test) != length(y_test)) { yhat_test <- NULL; y_test <- NULL }
    tuning_excl <- if (pushed) attr(model, "tuning_excluded_labels") %||% character(0) else character(0)
    dp <- cer_report_data_parts(rownames(mf), dep_var, preds, fallback,
                                test_rn = names(yhat_test) %||% names(y_test) %||% character(0), tuning_excl = tuning_excl, holdout = holdout)

    yhat <- as.numeric(model$fitted.values); y <- as.numeric(mf[[1]])
    lab  <- if (is_log) paste0("log(", dep_var, ")") else dep_var
    plots <- list(
      list(title = "Actuals vs Predicted",
           src = ceres_report_scatter_png(yhat, y, paste("Predicted", lab), paste("Actual", lab), "identity",
                                          as.numeric(yhat_test), as.numeric(y_test)),
           caption = if (is_log) "log space, as in the Model Summary" else "as in the Model Summary"),
      list(title = "Residuals vs Predicted",
           src = ceres_report_scatter_png(yhat, y - yhat, paste("Predicted", lab), "Residual (actual - predicted)", "zero",
                                          as.numeric(yhat_test),
                                          if (!is.null(y_test)) as.numeric(y_test) - as.numeric(yhat_test)),
           caption = if (is_log) "log space" else "unit space"))
    plots <- Filter(function(p) !is.na(p$src), plots)

    model_lbl <- if (is_log) "Log-linear" else "Linear"
    cat_terms <- names(eq_parts$values)[!names(eq_parts$values) %in% c("(Intercept)", names(mf)[vapply(mf, is.numeric, logical(1))])]
    if (is_log) cat_terms <- names(eq_parts$exp_form)[eq_parts$exp_form & names(eq_parts$exp_form) != "(Intercept)"]
    cat_terms <- cat_terms[!is_interaction_col(cat_terms, pool = pcols, source = cer_source_cols())]
    extra <- c(
      if (is_log) "R², Adj. R² and the residual SE are in log space, where the CER was fitted; the equation is in unit space.",
      if (pushed) paste0("This is the CER Tuning refit: ", length(tuning_excl), " program(s) excluded and the model refit with the same error term."))
    parts <- list(
      title    = paste0(dep_var, " — ", model_lbl, ", ", err),
      subtitle = paste0("Predictors: ", paste(preds, collapse = ", ")),
      meta     = cer_report_meta(dp$dataset, model_lbl, err),
      equation = eq_parts$text,
      equation_label = if (is_log) "Log-Linear CER Equation" else "Linear CER Equation",
      warning  = conv_note,
      stats    = stats,
      stats_note = if (is_log) "Log-linear fit statistics are in log space." else NULL,
      metrics  = metrics,
      metrics_note = if (!is.null(metrics)) "The selected row of the model search table." else NULL,
      coefficients = coef_tab,
      coef_note = paste0(if (is_log) "Coefficient Estimate, SE, t and p are in log space; Unit-Space Value is the number the equation prints for the term. " else "",
                         "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"),
      residuals = resid_tab,
      diagnostics = diagnostics,
      programs = dp$programs,
      programs_note = if (dp$matched) "Values as loaded in the dataset." else
        "Values back-transformed from the fitting data (the dataset rows could not be matched by name).",
      excluded = dp$excluded,
      excluded_note = if (length(tuning_excl)) "CERES does not record a reason when a point is excluded in CER Tuning." else NULL,
      ranges   = dp$ranges,
      ranges_note = "The range of each predictor over the programs used. The CER is supported inside these ranges.",
      plots    = plots,
      usage    = cer_report_usage(eq_parts$text, dep_var, preds, dp$ranges, cat_terms, extra),
      notes    = c("Generated by CERES from the model in the Model Summary; every number is the Model Summary's own.",
                   "Plots show the training programs (and test programs when a holdout was used).")
    )
    parts
  }

  cer_report_parts_nl <- function() {
    req(rv$nl_model, rv$nl_coeffs, rv$nl_sets, rv$nl_results)
    model   <- rv$nl_model
    dep_var <- rv$nl_fit_response %||% input$nl_response %||% "Response"
    eq_name <- rv$nl_fit_eq_name  %||% input$nl_equation %||% "Custom"
    err     <- rv$nl_fit_error_term %||% input$nl_error_term %||% "Least Squares"
    log_y   <- isTRUE(rv$nl_fit_log_y)
    preds   <- rv$nl_fit_predictors %||% character(0)
    sets    <- rv$nl_sets
    n_obs   <- length(sets[["y_train"]])

    eq <- tryCatch(nl_fitted_equation(), error = function(e) NA_character_)
    if (is.null(eq) || length(eq) != 1L || is.na(eq)) eq <- "(equation unavailable)"
    eq <- unbt(eq)
    if (!identical(eq, "(equation unavailable)")) {
      eq <- nl_display_names(eq, c(dep_var, preds))
      eq <- sub("\\s*~\\s*", " = ", eq)
    }

    res <- rv$nl_results
    dt_val <- function(cn) {
      x <- res[1, cn]
      if (cn %in% c("Coeff Significance (%)", "CV (%)", "MAPE (%)", "R-Squared", "Adj. R-Squared"))
        paste0(format(round(x * 100, 3), scientific = FALSE, digits = 15, trim = TRUE), "%")
      else format(round(x, 3), scientific = FALSE, digits = 15, trim = TRUE)
    }
    space <- if (log_y) "log space" else NA_character_
    stats <- data.frame(label = c("Observations", "R²", "Adj. R²", "Standard Error"),
                        value = c(as.character(n_obs), dt_val("R-Squared"), dt_val("Adj. R-Squared"),
                                  dt_val("Standard Error")),
                        sub = c(NA, "pseudo", "pseudo", if (identical(err, "MUPE")) "MUPE root MSE" else space),
                        stringsAsFactors = FALSE)
    other <- setdiff(colnames(res), c("R-Squared", "Adj. R-Squared", "Standard Error"))
    metrics <- data.frame(label = other, value = vapply(other, dt_val, character(1)), stringsAsFactors = FALSE)

    co <- rv$nl_coeffs
    est <- as.numeric(co[, 1]); se <- as.numeric(co[, 2])
    df_res <- tryCatch(stats::df.residual(model), error = function(e) NA_integer_)
    if (!is.finite(df_res) || df_res <= 0) df_res <- max(n_obs - nrow(co), 1L)
    t_stat <- ifelse(is.finite(se) & se > 0, est / se, NA_real_)
    p_val  <- 2 * pt(-abs(t_stat), df_res)
    f5 <- function(x) ifelse(is.na(x), "—", formatC(x, format = "f", digits = 5, big.mark = ","))
    coef_tab <- data.frame(Coefficients = unbt(rownames(co)), `Est. Value` = f5(est), `Asym. SE` = f5(se),
                           `T-value` = vapply(t_stat, cer_fmt_num, character(1)),
                           `P-value` = vapply(p_val, cer_fmt_p, character(1)),
                           `Sig.` = vapply(p_val, cer_sig_stars, character(1)),
                           `Approx. 95% Low` = f5(as.numeric(co[, 3])),
                           `Approx. 95% High` = f5(as.numeric(co[, 4])),
                           check.names = FALSE, stringsAsFactors = FALSE)

    dtr <- sets[["data_train"]]
    rn  <- names(sets[["yhat_train"]]) %||% rownames(dtr)
    fallback <- function(cn) {
      sc  <- tryCatch(sanitize_nl_colname(cn), error = function(e) cn)
      col <- dtr[[sc]] %||% dtr[[cn]]
      if (is.null(col) && identical(cn, dep_var)) col <- sets[["y_train"]]
      if (is.null(col)) rep(NA_real_, n_obs) else col
    }
    holdout <- isTRUE(input$ols_holdout %in% OLS_HOLDOUT_CHOICES[2:4])
    yhat_test <- if (holdout) sets[["yhat_test"]] else NULL
    y_test    <- if (holdout) sets[["y_test"]]    else NULL
    if (length(yhat_test) != length(y_test)) { yhat_test <- NULL; y_test <- NULL }
    dp <- cer_report_data_parts(rn, dep_var, preds, fallback,
                                test_rn = names(yhat_test) %||% character(0), holdout = holdout)

    yhat <- as.numeric(sets[["yhat_train"]]); y <- as.numeric(sets[["y_train"]])
    lab  <- if (log_y) paste0("log(", dep_var, ")") else dep_var
    plots <- list(
      list(title = "Actuals vs Predicted",
           src = ceres_report_scatter_png(yhat, y, paste("Predicted", lab), paste("Actual", lab), "identity",
                                          as.numeric(yhat_test), as.numeric(y_test)),
           caption = "as on the Non-Linear results"),
      list(title = "Residuals vs Predicted",
           src = ceres_report_scatter_png(yhat, y - yhat, paste("Predicted", lab), "Residual (actual - predicted)", "zero",
                                          as.numeric(yhat_test),
                                          if (!is.null(y_test)) as.numeric(y_test) - as.numeric(yhat_test)),
           caption = if (log_y) "log space" else "unit space"))
    plots <- Filter(function(p) !is.na(p$src), plots)

    model_lbl <- paste0("Non-linear (", eq_name, ")")
    list(
      title    = paste0(dep_var, " — ", model_lbl, ", ", err),
      subtitle = paste0("Predictors: ", paste(preds, collapse = ", ")),
      meta     = cer_report_meta(dp$dataset, model_lbl, err),
      equation = eq,
      equation_label = "Non-Linear CER Equation",
      stats    = stats,
      stats_note = paste0("R² for a non-linear fit is a pseudo R² (1 - SSE / SST)",
                          if (log_y) ", in log space" else "", "."),
      metrics  = metrics,
      metrics_note = "The Non-Linear results table.",
      coefficients = coef_tab,
      coef_note = paste0("Estimate, asymptotic SE and approximate 95% bounds as on screen; t and p as in the Excel export ",
                         "(t = estimate / SE on ", df_res, " residual df). Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"),
      programs = dp$programs,
      programs_note = if (dp$matched) "Values as loaded in the dataset." else "Values from the fitting data.",
      excluded = dp$excluded,
      excluded_note = "CER Tuning is not available for Non-Linear models. Regression diagnostics (VIF, influence) are not defined for non-linear least squares.",
      ranges   = dp$ranges,
      ranges_note = "The range of each predictor over the programs used. The CER is supported inside these ranges.",
      plots    = plots,
      usage    = cer_report_usage(eq, dep_var, preds, dp$ranges, character(0),
                                  if (log_y) "The response was fitted in log space."),
      notes    = c("Generated by CERES from the fitted Non-Linear model; every number is the one shown on the Non-Linear results.",
                   "Plots show the training programs (and test programs when a holdout was used).")
    )
  }

  cer_report_filename <- function(dep_var, kind) {
    stem <- gsub("[^A-Za-z0-9]+", "_", dep_var %||% "CER")
    stem <- gsub("^_+|_+$", "", stem)
    paste0("CER_report_", if (nzchar(stem)) stem else "CER", "_", kind, "_",
           format(Sys.Date(), "%Y%m%d"), ".html")
  }

  cer_report_write <- function(file, parts_fn) {
    html <- tryCatch(ceres_cer_report_html(parts_fn()), error = function(e) {
      showNotification(paste("CER report failed:", conditionMessage(e)), type = "error", duration = 10)
      paste0("<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>CER report</title></head><body>",
             "<p>The CER report could not be built: ", ceres_html_esc(conditionMessage(e)), "</p></body></html>")
    })
    writeBin(charToRaw(enc2utf8(html)), file)
  }

  output$report_log_linear <- downloadHandler(
    filename = function() cer_report_filename(input$ols_response, "Log_Linear"),
    content  = function(file) cer_report_write(file, function() cer_report_parts_lm("loglin")),
    contentType = "text/html"
  )
  output$report_linear <- downloadHandler(
    filename = function() cer_report_filename(input$ols_response, "Linear"),
    content  = function(file) cer_report_write(file, function() cer_report_parts_lm("linear")),
    contentType = "text/html"
  )
  output$report_nl <- downloadHandler(
    filename = function() cer_report_filename(rv$nl_fit_response %||% input$nl_response, "NonLinear"),
    content  = function(file) cer_report_write(file, cer_report_parts_nl),
    contentType = "text/html"
  )

  output$export_log_linear_excel <- downloadHandler(

    filename = function() {
      data_source <- tools::file_path_sans_ext(input$data_upload$name %||% "Dataset")
      timestamp <- format(Sys.time(), "%H%M%S")
      date_stamp <- format(Sys.Date(), "%Y%m%d")
      paste0(data_source, "_Log_Linear_Statistics_", date_stamp, "_", timestamp, ".xlsx")
    },
    
    content = function(file) {
      
      tryCatch({
        req(selected_log_linear_model())
        model <- selected_log_linear_model()
        log_data <- model$model
        
        real_response_name <- input$ols_response
        
        names(log_data)[names(log_data) == "y"] <- real_response_name
        
        y_name <- real_response_name
        
        y_log_actual <- log_data[, real_response_name]
        y_log_pred <- model$fitted.values
        
        y_unit_actual <- exp(y_log_actual)
        y_unit_pred <- exp(y_log_pred)
        
        s <- summary(model)
        coeffs <- coef(model)
        n_obs <- length(y_unit_actual)
        n_params <- length(coeffs)
        n_predictors <- n_params - 1
        
        intercept_unit <- exp(coeffs["(Intercept)"])
        if(is.na(intercept_unit)) intercept_unit <- 1

        int_sign <- if(intercept_unit < 0) "-" else ""
        int_str <- fmt_eqn(intercept_unit)
        eq_parts <- c(paste0(int_sign, int_str))
        unit_val <- c("(Intercept)" = unname(intercept_unit)); unit_exp <- c("(Intercept)" = TRUE)

        predictors <- names(coeffs)[names(coeffs) != "(Intercept)"]
        model_cols <- if (!is.null(model$model)) names(model$model) else character(0)
        get_col_data <- function(nm) if (nm %in% model_cols) model$model[[nm]] else NULL
        log1p_cols   <- attr(model, "log1p_cols") %||% character(0)
        for(var in predictors) {
          val <- coeffs[var]
          if (!is.finite(val)) next
          clean_var <- unbt(var)
          dt_term <- loglin_design_term(clean_var, unname(val), model$model, log1p_cols,
                                        function(x) paste0(if (x < 0) "-" else "", fmt_eqn(x)), " * ",
                                        cer_source_cols())
          if (!is.null(dt_term)) {
            eq_parts <- c(eq_parts, dt_term$text)
            unit_val[clean_var] <- dt_term$value; unit_exp[clean_var] <- dt_term$exp_form
            next
          }

          is_continuous <- FALSE
          col_data <- get_col_data(clean_var)
          if (!is.null(col_data)) {
            is_continuous <- is.numeric(col_data)
          } else if (grepl(":", clean_var, fixed = TRUE)) {
            parts <- strsplit(clean_var, ":", fixed = TRUE)[[1]]
            parts_data <- lapply(parts, get_col_data)
            is_continuous <- length(parts_data) > 0 &&
              all(vapply(parts_data, function(d) !is.null(d) && is.numeric(d), logical(1)))
          } else if (!is.null(input$ols_predictors) && clean_var %in% input$ols_predictors) {
            is_continuous <- TRUE
          }

          if (isTRUE(is_continuous)) {
            val_str   <- fmt_eqn(val)
            real_sign <- if (val < 0) "-" else ""
            final_val <- paste0(real_sign, val_str)
            base_token <- if (clean_var %in% log1p_cols) paste0("(", clean_var, " + 1)") else clean_var
            eq_parts  <- c(eq_parts, paste0(base_token, " ^ ", final_val))
            unit_val[clean_var] <- unname(val); unit_exp[clean_var] <- FALSE
          } else {
            base_val <- exp(val)
            base_str <- fmt_eqn(base_val)
            eq_parts <- c(eq_parts, paste0(base_str, " ^ ", clean_var))
            unit_val[clean_var] <- unname(base_val); unit_exp[clean_var] <- TRUE
          }
        }
        equation_str <- paste(y_name, "=", paste(eq_parts, collapse = " * "))

        coef_table <- as.data.frame(s$coefficients)
        rownames(coef_table) <- unbt(rownames(coef_table))
        coef_table <- cbind(Variable = rownames(coef_table), coef_table)
        df_resid   <- s$df[2]
        t_crit     <- qt(0.975, df_resid)
        coef_table$Lower95 <- coef_table[, "Estimate"] - (t_crit * coef_table[, "Std. Error"])
        coef_table$Upper95 <- coef_table[, "Estimate"] + (t_crit * coef_table[, "Std. Error"])
        u_exp <- unname(unit_exp[rownames(coef_table)])
        u_val <- unname(unit_val[rownames(coef_table)])

        coef_out <- data.frame(
          Variable = rownames(coef_table),
          Estimate = coef_table$Estimate,
          Std_Error = coef_table$`Std. Error`,
          t_value = coef_table$`t value`,
          Pr_t = coef_table$`Pr(>|t|)`,
          Lower95 = coef_table$Lower95,
          Upper95 = coef_table$Upper95,
          Unit = u_val,
          Unit_Lo = ifelse(u_exp, exp(coef_table$Lower95), coef_table$Lower95),
          Unit_Hi = ifelse(u_exp, exp(coef_table$Upper95), coef_table$Upper95),
          stringsAsFactors = FALSE
        )
        colnames(coef_out) <- c("Variable", "Coefficient Estimate", "Approximate Std Error", "t value", "Pr(>|t|)", "Lower 95% CI", "Upper 95% CI",
                                "Unit-Space Value", "Unit-Space Lower 95%", "Unit-Space Upper 95%")
        
        ss_resid <- sum(s$residuals^2)
        df_resid <- s$df[2]
        ms_resid <- ss_resid / df_resid
        ybar_log_w <- sum(y_log_actual) / n_obs
        ss_total <- sum((y_log_actual - ybar_log_w)^2)
        df_total <- n_obs - 1
        
        anova_df <- data.frame(
          Source = c("Residual (Error)", "Total (Corrected)"),
          DF = c(df_resid, df_total),
          SS = c(fmt_num(ss_resid), fmt_num(ss_total)),
          MeanSQ = c(fmt_num(ms_resid), NA)
        )
        
        gof_df <- data.frame(
          Metric = c("Std. Error (SE)", "Approx. R-Squared", "Approx. R-Squared (Adj)"),
          Value = c(fmt_num(s$sigma), fmt_pct(s$r.squared), fmt_pct(s$adj.r.squared))
        )
        
        s_corr <- summary(model, correlation = TRUE)
        if(!is.null(s_corr$correlation)) {
          corr_mat <- as.data.frame(s_corr$correlation)
          
          rownames(corr_mat) <- unbt(rownames(corr_mat))
          colnames(corr_mat) <- unbt(colnames(corr_mat))
          
          corr_mat_fmt <- apply(corr_mat, 2, function(x) fmt_num(x))
          corr_out <- as.data.frame(corr_mat_fmt)
          corr_out <- cbind(Variable = rownames(corr_mat), corr_out)
        } else {
          corr_out <- data.frame(Message = "Correlation matrix not available.")
        }
        
        mm <- model.matrix(model) 
        if ("(Intercept)" %in% colnames(mm)) {
          predictors_only <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
        } else { predictors_only <- mm }
        corr_data <- cbind(y_log_actual, predictors_only)
        colnames(corr_data)[1] <- y_name
        
        pairwise_out <- data.frame()
        if (ncol(corr_data) > 1) {
          pairwise_corr <- cor(corr_data, use = "pairwise.complete.obs")
          
          rownames(pairwise_corr) <- unbt(rownames(pairwise_corr))
          colnames(pairwise_corr) <- unbt(colnames(pairwise_corr))
          
          pairwise_out <- as.data.frame(apply(pairwise_corr, 2, fmt_num))
          pairwise_out <- cbind(Variable = rownames(pairwise_corr), pairwise_out)
        }
        
        vif_out <- data.frame()
        if (n_predictors > 1) {
          try({
            {
              v_u <- vif_unweighted(model)
              if (any(v_u$Df > 1)) {
                vif_raw <- cbind(GVIF = v_u$GVIF, Df = v_u$Df); rownames(vif_raw) <- v_u$term
              } else vif_raw <- setNames(v_u$GVIF, v_u$term)
              if (is.matrix(vif_raw)) {
                nm_vif <- rownames(vif_raw); vif_vec <- as.numeric(vif_raw[, 1]); vif_df <- as.numeric(vif_raw[, 2])
              } else {
                nm_vif <- names(vif_raw); vif_vec <- as.numeric(vif_raw); vif_df <- rep(1, length(vif_vec))
              }
              multi_df <- vif_df > 1
              r_sq_indiv <- 1 - (1 / vif_vec)
              df_num <- n_predictors - 1
              df_den <- n_obs - n_predictors
              
              f_stats_vif <- (r_sq_indiv / df_num) / ((1 - r_sq_indiv) / df_den)
              f_stats_vif[is.nan(f_stats_vif) | is.infinite(f_stats_vif)] <- 0
              r_sq_indiv[multi_df]  <- NA_real_
              f_stats_vif[multi_df] <- NA_real_
              sev_vif <- vif_vec^(1 / vif_df)
              
              vif_out <- data.frame(
                Variable = unbt(nm_vif),
                Indiv_R_Squared = fmt_pct(r_sq_indiv),
                F_Statistic = fmt_num(f_stats_vif),
                VIF = fmt_num(vif_vec),
                stringsAsFactors = FALSE
              )
              if (any(multi_df)) {
                vif_out$Df <- vif_df
                vif_out[["GVIF^(1/Df)"]] <- vif_vec^(1 / vif_df)
              }
              vif_out$Severity <- ifelse(sev_vif < 5, "Low", ifelse(sev_vif < 10, "Moderate", "High"))
            }
          }, silent = TRUE)
        }
        
        cooks_d <- cooks.distance(model)
        stud_res <- rstudent(model)
        dffits_val <- dffits(model)
        hat_val <- hatvalues(model)
        thresh_cooks <- 4 / n_obs
        thresh_stud <- 2.0 
        thresh_dffits <- 2 * sqrt(n_params / n_obs)
        thresh_hat <- 3 * n_params / n_obs 
        is_cooks <- cooks_d > thresh_cooks
        is_stud <- abs(stud_res) > thresh_stud
        is_dffits <- abs(dffits_val) > thresh_dffits
        is_hat <- hat_val > thresh_hat
        
        outlier_summary <- data.frame(
          Metric = c("Cook's Distance", "Studentized Residuals", "DFFITS", "Leverage (Hat Values)"),
          Threshold_Formula = c("4 / n", "> 2.0 (Abs)", "2 * sqrt(p/n)", "3 * p / n"),
          Threshold_Value = c(fmt_num(thresh_cooks), fmt_num(thresh_stud), fmt_num(thresh_dffits), fmt_num(thresh_hat)),
          Count_Flagged = c(sum(is_cooks, na.rm=TRUE), sum(is_stud, na.rm=TRUE), sum(is_dffits, na.rm=TRUE), sum(is_hat, na.rm=TRUE))
        )
        row_ids <- rownames(log_data)
        if(is.null(row_ids)) row_ids <- seq_len(n_obs)
        outlier_table <- data.frame(
          Obs = seq_len(n_obs), Name = row_ids, Cooks_D = fmt_num(cooks_d), Studentized_Res = fmt_num(stud_res),
          DFFITS = fmt_num(dffits_val), Leverage = fmt_num(hat_val),
          Flag_Cooks = ifelse(is_cooks, "*", ""), Flag_StudRes = ifelse(is_stud, "*", ""),
          Flag_DFFITS = ifelse(is_dffits, "*", ""), Flag_Leverage = ifelse(is_hat, "*", "")
        )
        
        wts <- attr(model, "mupe_weights")
        if(is.null(wts)) wts <- rep(1, n_obs)
        residuals_unit <- y_unit_actual - y_unit_pred
        pct_error <- (y_unit_pred - y_unit_actual) / y_unit_pred
        std_res <- (y_log_actual - y_log_pred) / s$sigma
        flags <- ifelse(abs(std_res) > 2, "*", "")
        
        predict_table <- data.frame(
          Obs = seq_len(n_obs), Name = row_ids, Weight = wts, Actuals = fmt_num(y_unit_actual),
          `Predicted (Median)` = fmt_num(y_unit_pred), Residuals = fmt_num(residuals_unit),
          Pct_Errors = fmt_pct(pct_error), Flags = flags, check.names = FALSE
        )
        
        avg_act_unit <- mean(y_unit_actual)
        rmse_unit <- sqrt(mean((y_unit_pred - y_unit_actual)^2))
        mad_pct <- mean(abs(pct_error))
        cv_se <- if (abs(avg_act_unit) > 1e-12) rmse_unit / abs(avg_act_unit) else NA_real_
        mad_res_unit <- mean(abs(residuals_unit))
        cv_mad <- if (abs(avg_act_unit) > 1e-12) mad_res_unit / abs(avg_act_unit) else NA_real_
        w_unit  <- attr(model, "mupe_weights")
        if (length(w_unit) != n_obs) w_unit <- rep(1, n_obs)
        sw_u    <- sum(w_unit)
        ybar_wu <- sum(w_unit * y_unit_actual) / sw_u
        ss_res_unit <- sum(w_unit * (y_unit_actual - y_unit_pred)^2)
        ss_tot_unit <- sum(w_unit * (y_unit_actual - ybar_wu)^2)
        r2_unit <- if (is.finite(ss_tot_unit) && ss_tot_unit > 0) 1 - (ss_res_unit / ss_tot_unit) else NA_real_
        adj_r2_unit <- if (is.finite(r2_unit))
          1 - (1 - r2_unit) * ((n_obs - 1) / max(n_obs - n_params, 1)) else NA_real_
        pbar_wu <- sum(w_unit * y_unit_pred) / sw_u
        cov_wu  <- sum(w_unit * (y_unit_actual - ybar_wu) * (y_unit_pred - pbar_wu))
        var_wpu <- sum(w_unit * (y_unit_pred - pbar_wu)^2)
        pearson_r <- if (is.finite(ss_tot_unit) && is.finite(var_wpu) && ss_tot_unit > 0 && var_wpu > 0)
          cov_wu / sqrt(ss_tot_unit * var_wpu) else NA_real_
        
        predict_summary <- data.frame(
          Metric = c("Average Actual in Unit Space", "Root Mean Square Error Unit", "Root Mean Square of % Errors", "Mean Absolute Deviation of % Errors", "Coef of Variation (SE)", "Coef of Variation (MAD)", "Pearson's Correlation", "Adjusted R-Squared in Unit Space"),
          Value = c(fmt_num(avg_act_unit), fmt_num(rmse_unit), fmt_pct(sqrt(mean(pct_error^2))), fmt_pct(mad_pct), fmt_pct(cv_se), fmt_pct(cv_mad), fmt_pct(pearson_r), fmt_pct(adj_r2_unit))
        )
        
        coef_out <- clean_df(coef_out)
        anova_df <- clean_df(anova_df)
        gof_df <- clean_df(gof_df)
        corr_out <- clean_df(corr_out)
        if(nrow(pairwise_out) > 0) pairwise_out <- clean_df(pairwise_out)
        vif_out <- clean_df(vif_out)
        outlier_summary <- clean_df(outlier_summary)
        outlier_table <- clean_df(outlier_table)
        predict_table <- clean_df(predict_table)
        predict_summary <- clean_df(predict_summary)

        coef_out <- xl_numfmt(coef_out,
          `Coefficient Estimate` = xl_eqn_fmt(coef_out[["Coefficient Estimate"]]),
          `Approximate Std Error` = XL_NUM, `t value` = XL_NUM, `Pr(>|t|)` = XL_NUM,
          `Lower 95% CI` = XL_NUM, `Upper 95% CI` = XL_NUM,
          `Unit-Space Value` = xl_eqn_fmt(coef_out[["Unit-Space Value"]]),
          `Unit-Space Lower 95%` = xl_eqn_fmt(coef_out[["Unit-Space Lower 95%"]]),
          `Unit-Space Upper 95%` = xl_eqn_fmt(coef_out[["Unit-Space Upper 95%"]]))
        anova_df <- xl_numfmt(anova_df, SS = XL_NUM, MeanSQ = XL_NUM)
        gof_df   <- xl_numfmt(gof_df, Value = c(XL_NUM, XL_PCT, XL_PCT))
        corr_out        <- xl_numfmt_numeric(corr_out, XL_NUM)
        pairwise_out    <- xl_numfmt_numeric(pairwise_out, XL_NUM)
        if (nrow(vif_out) > 0)
          vif_out <- xl_numfmt(vif_out, Indiv_R_Squared = XL_PCT, F_Statistic = XL_NUM, VIF = XL_NUM,
                               `GVIF^(1/Df)` = XL_NUM)
        outlier_summary <- xl_numfmt(outlier_summary, Threshold_Value = XL_NUM)
        outlier_table   <- xl_numfmt(outlier_table, Cooks_D = XL_NUM, Studentized_Res = XL_NUM,
                                     DFFITS = XL_NUM, Leverage = XL_NUM)
        predict_table   <- xl_numfmt(predict_table, Weight = xl_eqn_fmt(predict_table$Weight),
                                     Actuals = XL_NUM, `Predicted (Median)` = XL_NUM,
                                     Residuals = XL_NUM, Pct_Errors = XL_PCT)
        predict_summary <- xl_numfmt(predict_summary, Value = c(XL_NUM, XL_NUM, rep(XL_PCT, 6)))
        
        wb <- ceres_ns("openxlsx2")$wb_workbook() %>% ceres_ns("openxlsx2")$wb_add_worksheet("Model Statistics")
        
        wb$add_data(x = "Log-Linear Analysis Output", dims = "B2")
        wb$add_data(x = paste("Date:", Sys.time()), dims = "B3")
        if (isFALSE(attr(model, "irls_converged")))
          wb$add_data(x = paste0("WARNING: the fit did not converge (", attr(model, "irls_iterations"),
                                 " iterations) - every statistic below is for the last iterate."), dims = "B4")
        
        wb$add_data(x = "I. Equation Form & Error Term", dims = "B5")
        wb$add_font(dims = "B5", bold = "1")
        
        wb$merge_cells(dims = "C7:K7")
        wb$merge_cells(dims = "C8:K8")
        wb$merge_cells(dims = "C9:K9")
        wb$merge_cells(dims = "C10:K10")
        
        wb$add_data(x = "Model Form:", dims = "B7")
        wb$add_data(x = "Log-Linear Model", dims = "C7")
        
        wb$add_data(x = "Number of Observations Used:", dims = "B8")
        wb$add_data(x = n_obs, dims = "C8")
        wb$add_cell_style(dims = "C8", horizontal = "left")
        
        wb$add_data(x = "Equation:", dims = "B9")
        wb$add_data(x = equation_str, dims = "C9")
        wb$add_cell_style(dims = "C9", horizontal = "left") 
        
        wb$add_data(x = "Error Term:", dims = "B10")
        err_term <- if(!is.null(input$log_linear_error)) input$log_linear_error else "Log-Linear"
        wb$add_data(x = err_term, dims = "C10")
        wb$add_cell_style(dims = "C10", horizontal = "left")
        
        style_range_borders(wb, rows = 7:10, cols = 2:11)
        wb$add_font(dims = "B7:B10", bold = "1")
        
        wb$add_data(x = "II. Fit Measures (in Fit Space)", dims = "B12")
        wb$add_font(dims = "B12", bold = "1")
        
        wb$add_data(x = "Coefficient Statistics Summary", dims = "B14")
        wb$add_font(dims = "B14", bold = "1")
        wb$add_data(x = coef_out, dims = "B15", na = NULL)
        style_table_borders(wb, 1, 15, 2, coef_out)
        
        curr_row <- 15 + nrow(coef_out) + 2
        
        wb$add_data(x = "Least Squares Minimization Summary", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = anova_df, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, anova_df)
        
        curr_row <- curr_row + nrow(anova_df) + 3
        
        wb$add_data(x = "Goodness-of-Fit Statistics", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = gof_df, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, gof_df)
        
        curr_row <- curr_row + nrow(gof_df) + 3
        
        wb$add_data(x = "Correlation Matrix of Coefficients", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = corr_out, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, corr_out)
        
        curr_row <- curr_row + nrow(corr_out) + 3
        
        wb$add_data(x = "Pairwise Correlation Matrix", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        
        if (nrow(pairwise_out) > 0) {
          wb$add_data(x = pairwise_out, dims = paste0("B", curr_row + 1), na = NULL)
          style_table_borders(wb, 1, curr_row + 1, 2, pairwise_out)
          curr_row <- curr_row + nrow(pairwise_out) + 3
        } else {
          wb$add_data(x = "N/A (Not enough variables)", dims = paste0("B", curr_row + 1))
          curr_row <- curr_row + 2
        }
        
        wb$add_data(x = "Multicollinearity Analysis (VIF)", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        
        if (nrow(vif_out) > 0) {
          wb$add_data(x = vif_out, dims = paste0("B", curr_row + 1), na = NULL)
          style_table_borders(wb, 1, curr_row + 1, 2, vif_out)
          curr_row <- curr_row + nrow(vif_out) + 1
          if ("Df" %in% names(vif_out)) {
            wb$add_data(x = "Rows with Df > 1 (a factor with 3+ levels): VIF is the generalised VIF (GVIF) of the whole factor; it is graded as GVIF^(1/Df) on the usual 5 / 10 VIF scale (Severity, and Max VIF everywhere in CERES); Indiv R-Squared and F are not defined and are left empty.",
                        dims = paste0("B", curr_row + 2))
            curr_row <- curr_row + 2
          }
        } else {
          wb$add_data(x = "N/A (Not enough predictors)", dims = paste0("B", curr_row + 1))
          curr_row <- curr_row + 2
        }
        
        curr_row <- curr_row + 2
        
        wb$add_data(x = "Potential Outlier Analysis Summary", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = outlier_summary, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, outlier_summary)
        
        curr_row <- curr_row + nrow(outlier_summary) + 3
        
        wb$add_data(x = "Potential Outlier Analysis Table (All Observations)", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = outlier_table, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, outlier_table)
        
        rows_outlier <- (curr_row + 2):(curr_row + 1 + nrow(outlier_table))
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_outlier, cols = 2), horizontal = "center")
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_outlier, cols = 8:11), horizontal = "center")
        wb$add_font(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_outlier, cols = 8:11), bold = "1")
        
        curr_row <- curr_row + nrow(outlier_table) + 3
        wb$add_data(x = "III. Predictive Measures (in Unit Space)", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        
        curr_row <- curr_row + 1
        
        wb$add_data(x = "Percentage Error Table", dims = paste0("B", curr_row + 1))
        wb$add_font(dims = paste0("B", curr_row + 1), bold = "1")
        
        wb$add_data(x = predict_table, dims = paste0("B", curr_row + 2), na = NULL)
        style_table_borders(wb, 1, curr_row + 2, 2, predict_table)
        
        rows_pred <- (curr_row + 3):(curr_row + 2 + nrow(predict_table))
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_pred, cols = 2), horizontal = "center")
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_pred, cols = 9), horizontal = "center")
        wb$add_font(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_pred, cols = 9), bold = "1")
        
        wb$add_data(x = "Predicted (Median) = exp(Xb), the median estimate in unit space (no smearing correction). Residuals = Actuals - Predicted. Pct_Errors = (Predicted - Actuals) / Predicted.",
                    dims = paste0("B", curr_row + 2 + nrow(predict_table) + 2))
        curr_row <- curr_row + 2 + nrow(predict_table) + 4
        
        wb$add_data(x = "Summary of Predictive Measures", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = predict_summary, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, predict_summary)
        
        wb$set_col_widths(cols = 2, widths = 32)
        wb$set_col_widths(cols = 3:11, widths = 19.22)

        wb <- wb %>% ceres_ns("openxlsx2")$wb_add_worksheet("Excluded Data Points")

        excl_labels_for_export   <- attr(model, "tuning_excluded_labels")
        excl_snapshot_for_export <- attr(model, "tuning_excluded_rows_df")
        was_pushed_for_export    <- isTRUE(attr(model, "tuning_pushed"))
        if (is.null(excl_labels_for_export) || !was_pushed_for_export) {
          excl_labels_for_export <- character(0)
        }

        wb$add_data(x = "Excluded Data Points (CER Tuning)", dims = "B2")
        wb$add_font(dims = "B2", bold = "1")
        wb$add_data(x = paste("Date:", Sys.time()), dims = "B3")

        wb$add_data(x = "Source:", dims = "B5")
        wb$add_data(x = if (was_pushed_for_export)
          "Pushed Refit (exclusions captured at push time)"
          else "No exclusions applied to this run (live CER Tuning toggles were not pushed)",
          dims = "C5")
        wb$add_data(x = "Number Excluded:", dims = "B6")
        wb$add_data(x = length(excl_labels_for_export), dims = "C6")
        wb$add_cell_style(dims = "C6", horizontal = "left")
        wb$add_font(dims = "B5:B6", bold = "1")

        if (length(excl_labels_for_export) > 0L) {
          if (!is.null(excl_snapshot_for_export) && nrow(excl_snapshot_for_export) > 0L) {
            src_df_excl <- excl_snapshot_for_export
          } else {
            rn_m <- rownames(model$model)
            keep_mask_excl <- !is.null(rn_m) & (rn_m %in% excl_labels_for_export)
            src_df_excl <- if (any(keep_mask_excl)) model$model[keep_mask_excl, , drop = FALSE] else data.frame()
          }
          if (nrow(src_df_excl) > 0L) {
            disp_df_excl <- src_df_excl
            disp_df_excl <- disp_df_excl[, setdiff(names(disp_df_excl), "(weights)"),
                                         drop = FALSE]
            log1p_cols_excl <- attr(model, "log1p_cols") %||% character(0)
            disp_df_excl <- disp_df_excl[, !is_interaction_col(names(disp_df_excl)), drop = FALSE]
            dummy_cols_excl <- names(disp_df_excl)[vapply(names(disp_df_excl), function(cn)
              is_dummy01(disp_df_excl[[cn]]) &&
                (is.null(model$model[[cn]]) || is_dummy01(model$model[[cn]])), logical(1L))]
            num_cols_excl   <- setdiff(
              names(disp_df_excl)[vapply(disp_df_excl, is.numeric, logical(1L))], c("y", dummy_cols_excl))
            for (cn in intersect(names(disp_df_excl), num_cols_excl)) {
              disp_df_excl[[cn]] <- if (cn %in% log1p_cols_excl)
                expm1(disp_df_excl[[cn]]) else exp(disp_df_excl[[cn]])
            }
            if ("y" %in% names(disp_df_excl)) {
              disp_df_excl[["y"]] <- exp(disp_df_excl[["y"]])
              names(disp_df_excl)[names(disp_df_excl) == "y"] <- real_response_name
            }
            excl_table_out <- data.frame(
              Obs = seq_len(nrow(disp_df_excl)),
              Name = rownames(disp_df_excl),
              stringsAsFactors = FALSE
            )
            for (cn in names(disp_df_excl)) {
              col_vals <- disp_df_excl[[cn]]
              excl_table_out[[cn]] <- if (is.numeric(col_vals)) fmt_num(col_vals) else as.character(col_vals)
            }
            excl_table_out <- clean_df(excl_table_out)
            excl_table_out <- xl_numfmt_numeric(excl_table_out, XL_NUM)
            attr(excl_table_out, "xl_numfmt")[["Obs"]] <- NULL
            wb$add_data(x = "Excluded Observations", dims = "B8")
            wb$add_font(dims = "B8", bold = "1")
            wb$add_data(x = excl_table_out, dims = "B9", na = NULL)
            style_table_borders(wb, 2, 9, 2, excl_table_out)
            rows_excl <- 10:(9 + nrow(excl_table_out))
            wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_excl, cols = 2), horizontal = "center")
          } else {
            wb$add_data(x = "Labels were recorded but matching rows could not be retrieved.",
                        dims = "B8")
          }
        } else {
          wb$add_data(x = "No data points were excluded for this run.", dims = "B8")
        }
        wb$set_col_widths(cols = 2, widths = 14)
        wb$set_col_widths(cols = 3, widths = 22)
        wb$set_col_widths(cols = 4:11, widths = 19.22)

        wb$save(file)

      }, error = function(e) {
        wb <- ceres_ns("openxlsx2")$wb_workbook() %>% ceres_ns("openxlsx2")$wb_add_worksheet("Error Log")
        wb$add_data(x = "An error occurred during export:", dims = "A1")
        wb$add_data(x = as.character(e), dims = "A2")
        wb$save(file)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$export_linear_excel <- downloadHandler(

    filename = function() {
      data_source <- tools::file_path_sans_ext(input$data_upload$name %||% "Dataset")
      timestamp <- format(Sys.time(), "%H%M%S")
      date_stamp <- format(Sys.Date(), "%Y%m%d")
      paste0(data_source, "_Linear_Statistics_", date_stamp, "_", timestamp, ".xlsx")
    },
    
    content = function(file) {
      
      tryCatch({
        req(selected_linear_model())
        model <- selected_linear_model()
        data_df <- model$model
        
        real_response_name <- input$ols_response
        
        names(data_df)[names(data_df) == "y"] <- real_response_name
        
        y_name <- real_response_name
        
        y_actual <- data_df[, real_response_name]
        y_pred <- model$fitted.values
        residuals_val <- model$residuals
        n_obs <- length(y_actual)
        
        s <- summary(model)
        coeffs <- coef(model)
        n_params <- length(coeffs)
        n_predictors <- n_params - 1
        wts <- weights(model)
        has_weights <- !is.null(wts)
        model_type_str <- if(has_weights) "Weighted Linear model" else "Unweighted Linear model"
        
        intercept_val <- coeffs["(Intercept)"]
        if(is.na(intercept_val)) intercept_val <- 0
        int_sign <- if(intercept_val < 0) "-" else ""
        int_str <- fmt_eqn(intercept_val)
        eq_parts <- c(paste0(int_sign, int_str))
        predictors <- names(coeffs)[names(coeffs) != "(Intercept)"]
        for(var in predictors) {
          val <- coeffs[var]
          if (!is.finite(val)) next
          clean_var <- unbt(var)
          operator <- if(val >= 0) " + " else " - "
          val_str <- fmt_eqn(val)
          eq_parts <- c(eq_parts, paste0(operator, val_str, " * ", clean_var))
        }
        equation_str <- paste(y_name, "=", paste(eq_parts, collapse = ""))
        
        coef_base <- as.data.frame(s$coefficients) 
        
        sd_y <- sd(y_actual, na.rm = TRUE)
        beta_values <- numeric(nrow(coef_base))
        
        mm_beta <- model.matrix(model)
        for(i in seq_len(nrow(coef_base))) {
          rname <- rownames(coef_base)[i]
          beta_values[i] <- if (rname == "(Intercept)" || !(rname %in% colnames(mm_beta))) NA_real_ else
            coef_base[i, "Estimate"] * (sd(mm_beta[, rname]) / sd_y)
        }
        
        rownames(coef_base) <- unbt(rownames(coef_base))
        
        t_crit_lin <- qt(0.975, s$df[2])
        coef_out <- data.frame(
          Variable = rownames(coef_base),
          Coefficient = fmt_num(coef_base$Estimate),
          Std_Dev_of_Coef = fmt_num(coef_base$`Std. Error`),
          Beta_Value = fmt_num(beta_values),
          T_Statistic = fmt_num(coef_base$`t value`),
          P_Value = fmt_num(coef_base$`Pr(>|t|)`),
          Prob_Not_Zero = fmt_num(1 - coef_base$`Pr(>|t|)`),
          Unit_Space_Value = coef_base$Estimate,
          Unit_Space_Lower_95 = coef_base$Estimate - t_crit_lin * coef_base$`Std. Error`,
          Unit_Space_Upper_95 = coef_base$Estimate + t_crit_lin * coef_base$`Std. Error`,
          stringsAsFactors = FALSE
        )
        names(coef_out)[8:10] <- c("Unit-Space Value", "Unit-Space Lower 95%", "Unit-Space Upper 95%")
        
        h_ii    <- hatvalues(model)
        w_vec   <- if (has_weights) wts else rep(1, n_obs)
        sw      <- sum(w_vec)

        press_stat   <- sum(w_vec * (residuals_val / (1 - h_ii))^2)
        y_bar_w      <- sum(w_vec * y_actual) / sw
        sst_w        <- sum(w_vec * (y_actual - y_bar_w)^2)
        r2_pred_val  <- if (is.finite(sst_w) && sst_w > 0) 1 - (press_stat / sst_w) else NA_real_
        pbar_w       <- sum(w_vec * y_pred) / sw
        cov_w        <- sum(w_vec * (y_actual - y_bar_w) * (y_pred - pbar_w))
        var_wp       <- sum(w_vec * (y_pred - pbar_w)^2)
        pearson_r    <- if (is.finite(sst_w) && is.finite(var_wp) && sst_w > 0 && var_wp > 0)
          cov_w / sqrt(sst_w * var_wp) else NA_real_
        gof_df <- data.frame(
          Metric = c("Std Error (SE)", "R-Squared", "R-Squared (Adj)", "Pearson's Corr Coef",
                     if (is.finite(press_stat)) "PRESS" else "PRESS (infinite: a row has leverage 1)", "R-Squared (Predicted)"),
          Value = c(fmt_num(s$sigma), fmt_pct(s$r.squared), fmt_pct(s$adj.r.squared), fmt_pct(pearson_r), press_stat, fmt_pct(r2_pred_val))
        )

        ss_resid <- sum(w_vec * residuals_val^2)
        ss_total <- sst_w
        ss_reg   <- max(ss_total - ss_resid, 0)
        df_resid <- model$df.residual
        df_total <- n_obs - 1
        df_reg   <- df_total - df_resid
        ms_reg   <- ss_reg / df_reg
        ms_resid <- ss_resid / df_resid
        f_stat   <- if (is.finite(ms_resid) && ms_resid > 0) ms_reg / ms_resid else NA_real_
        p_val_anova <- if (is.finite(f_stat)) pf(f_stat, df_reg, df_resid, lower.tail = FALSE) else NA_real_
        prob_not_zero_anova <- if (is.finite(p_val_anova)) 1 - p_val_anova else NA_real_
        anova_df <- data.frame(
          Due_To = c("Regression (Model)", "Residual (Error)", "Total"),
          DF = c(df_reg, df_resid, df_total),
          Sum_of_Sqr = c(fmt_num(ss_reg), fmt_num(ss_resid), fmt_num(ss_total)),
          Mean_SQ = c(fmt_num(ms_reg), fmt_num(ms_resid), NA),
          F_Statistic = c(fmt_num(f_stat), NA, NA),
          P_Value = c(fmt_num(p_val_anova), NA, NA),
          Prob_Not_Zero = c(fmt_num(prob_not_zero_anova), NA, NA)
        )
        
        mm <- model.matrix(model)
        if("(Intercept)" %in% colnames(mm)) mm <- mm[, -1, drop = FALSE]
        
        corr_data <- cbind(data_df[, 1, drop=FALSE], mm)
        names(corr_data)[1] <- y_name
        cor_mat <- cor(corr_data, use = "pairwise.complete.obs")
        
        rownames(cor_mat) <- unbt(rownames(cor_mat))
        colnames(cor_mat) <- unbt(colnames(cor_mat))
        
        cor_mat_fmt <- apply(cor_mat, 2, function(x) fmt_num(x))
        cor_out <- as.data.frame(cor_mat_fmt)
        cor_out <- cbind(Variable = rownames(cor_mat), cor_out)
        
        vif_out <- data.frame()
        if (n_predictors > 1) {
          try({
            {
              v_u <- vif_unweighted(model)
              if (any(v_u$Df > 1)) {
                vif_raw <- cbind(GVIF = v_u$GVIF, Df = v_u$Df); rownames(vif_raw) <- v_u$term
              } else vif_raw <- setNames(v_u$GVIF, v_u$term)
              if (is.matrix(vif_raw)) {
                nm_vif <- rownames(vif_raw); vif_vec <- as.numeric(vif_raw[, 1]); vif_df <- as.numeric(vif_raw[, 2])
              } else {
                nm_vif <- names(vif_raw); vif_vec <- as.numeric(vif_raw); vif_df <- rep(1, length(vif_vec))
              }
              multi_df <- vif_df > 1
              r_sq_indiv <- 1 - (1 / vif_vec)
              df_num <- n_predictors - 1
              df_den <- n_obs - n_predictors
              
              f_stats_vif <- (r_sq_indiv / df_num) / ((1 - r_sq_indiv) / df_den)
              f_stats_vif[is.nan(f_stats_vif) | is.infinite(f_stats_vif)] <- 0
              r_sq_indiv[multi_df]  <- NA_real_
              f_stats_vif[multi_df] <- NA_real_
              sev_vif <- vif_vec^(1 / vif_df)
              
              vif_out <- data.frame(
                Variable = unbt(nm_vif),
                Indiv_R_Squared = fmt_pct(r_sq_indiv),
                F_Statistic = fmt_num(f_stats_vif),
                VIF = fmt_num(vif_vec),
                stringsAsFactors = FALSE
              )
              if (any(multi_df)) {
                vif_out$Df <- vif_df
                vif_out[["GVIF^(1/Df)"]] <- vif_vec^(1 / vif_df)
              }
              vif_out$Severity <- ifelse(sev_vif < 5, "Low", ifelse(sev_vif < 10, "Moderate", "High"))
            }
          }, silent = TRUE)
        }
        
        cooks_d <- cooks.distance(model)
        stud_res <- rstudent(model)
        dffits_val <- dffits(model)
        hat_val <- hatvalues(model)
        thresh_cooks <- 4 / n_obs
        thresh_stud <- 2.0 
        thresh_dffits <- 2 * sqrt(n_params / n_obs)
        thresh_hat <- 3 * n_params / n_obs 
        is_cooks <- cooks_d > thresh_cooks
        is_stud <- abs(stud_res) > thresh_stud
        is_dffits <- abs(dffits_val) > thresh_dffits
        is_hat <- hat_val > thresh_hat
        
        outlier_summary <- data.frame(
          Metric = c("Cook's Distance", "Studentized Residuals", "DFFITS", "Leverage (Hat Values)"),
          Threshold_Formula = c("4 / n", "> 2.0 (Abs)", "2 * sqrt(p/n)", "3 * p / n"),
          Threshold_Value = c(fmt_num(thresh_cooks), fmt_num(thresh_stud), fmt_num(thresh_dffits), fmt_num(thresh_hat)),
          Count_Flagged = c(sum(is_cooks, na.rm=TRUE), sum(is_stud, na.rm=TRUE), sum(is_dffits, na.rm=TRUE), sum(is_hat, na.rm=TRUE))
        )
        row_names <- rownames(data_df)
        if(is.null(row_names)) row_names <- seq_len(n_obs)
        outlier_table <- data.frame(
          Obs = seq_len(n_obs), Name = row_names, Cooks_D = fmt_num(cooks_d), Studentized_Res = fmt_num(stud_res),
          DFFITS = fmt_num(dffits_val), Leverage = fmt_num(hat_val),
          Flag_Cooks = ifelse(is_cooks, "*", ""), Flag_StudRes = ifelse(is_stud, "*", ""),
          Flag_DFFITS = ifelse(is_dffits, "*", ""), Flag_Leverage = ifelse(is_hat, "*", "")
        )
        
        pct_error <- (y_pred - y_actual) / y_pred
        std_res_simple <- sqrt(w_vec) * residuals_val / s$sigma
        flags <- ifelse(abs(std_res_simple) > 2, "*", "")
        predict_table <- data.frame(
          Obs = seq_len(n_obs), Name = row_names, Actuals = fmt_num(y_actual), Predicted = fmt_num(y_pred),
          Residuals = fmt_num(residuals_val), Pct_Errors = fmt_pct(pct_error), Flags = flags
        )
        avg_act <- mean(y_actual)
        rmse <- sqrt(mean(residuals_val^2))
        mad_res <- mean(abs(residuals_val))
        avg_pct_err_arith <- mean(pct_error)
        avg_pct_err_abs <- mean(abs(pct_error))
        rms_pct_error <- sqrt(mean(pct_error^2))
        cv_val <- if (abs(avg_act) > 1e-12) rmse / abs(avg_act) else NA_real_
        w_unit  <- if (has_weights) wts else rep(1, n_obs)
        sw_u    <- sum(w_unit)
        ybar_wu <- sum(w_unit * y_actual) / sw_u
        ss_res_unit <- sum(w_unit * (y_actual - y_pred)^2)
        ss_tot_unit <- sum(w_unit * (y_actual - ybar_wu)^2)
        r2_unit <- if (is.finite(ss_tot_unit) && ss_tot_unit > 0) 1 - (ss_res_unit / ss_tot_unit) else NA_real_
        adj_r2_unit <- if (is.finite(r2_unit))
          1 - (1 - r2_unit) * ((n_obs - 1) / max(n_obs - n_params, 1)) else NA_real_
        predict_summary <- data.frame(
          Metric = c("Average Actual", "RMSE", "Avg Residual (Absolute)", "Avg % Error (Arithmetic)", "Avg % Error (Absolute)", "RMS % of Errors", "Coef of Variation", "Pearson's Correlation", "Adjusted R-Squared in Unit Space"),
          Value = c(fmt_num(avg_act), fmt_num(rmse), fmt_num(mad_res), fmt_pct(avg_pct_err_arith), fmt_pct(avg_pct_err_abs), fmt_pct(rms_pct_error), fmt_pct(cv_val), fmt_pct(pearson_r), fmt_pct(adj_r2_unit))
        )
        
        coef_out <- clean_df(coef_out)
        gof_df <- clean_df(gof_df)
        anova_df <- clean_df(anova_df)
        cor_out <- clean_df(cor_out)
        vif_out <- clean_df(vif_out)
        outlier_summary <- clean_df(outlier_summary)
        outlier_table <- clean_df(outlier_table)
        predict_table <- clean_df(predict_table)
        predict_summary <- clean_df(predict_summary)

        coef_out <- xl_numfmt(coef_out,
          Coefficient = xl_eqn_fmt(coef_out$Coefficient),
          Std_Dev_of_Coef = XL_NUM, Beta_Value = XL_NUM, T_Statistic = XL_NUM,
          P_Value = XL_NUM, Prob_Not_Zero = XL_NUM,
          `Unit-Space Value` = xl_eqn_fmt(coef_out[["Unit-Space Value"]]),
          `Unit-Space Lower 95%` = xl_eqn_fmt(coef_out[["Unit-Space Lower 95%"]]),
          `Unit-Space Upper 95%` = xl_eqn_fmt(coef_out[["Unit-Space Upper 95%"]]))
        gof_df   <- xl_numfmt(gof_df, Value = c(XL_NUM, XL_PCT, XL_PCT, XL_PCT, XL_SCI, XL_PCT))
        anova_df <- xl_numfmt(anova_df, Sum_of_Sqr = XL_NUM, Mean_SQ = XL_NUM, F_Statistic = XL_NUM,
                              P_Value = XL_NUM, Prob_Not_Zero = XL_NUM)
        cor_out  <- xl_numfmt_numeric(cor_out, XL_NUM)
        if (nrow(vif_out) > 0)
          vif_out <- xl_numfmt(vif_out, Indiv_R_Squared = XL_PCT, F_Statistic = XL_NUM, VIF = XL_NUM,
                               `GVIF^(1/Df)` = XL_NUM)
        outlier_summary <- xl_numfmt(outlier_summary, Threshold_Value = XL_NUM)
        outlier_table   <- xl_numfmt(outlier_table, Cooks_D = XL_NUM, Studentized_Res = XL_NUM,
                                     DFFITS = XL_NUM, Leverage = XL_NUM)
        predict_table   <- xl_numfmt(predict_table, Actuals = XL_NUM, Predicted = XL_NUM,
                                     Residuals = XL_NUM, Pct_Errors = XL_PCT)
        predict_summary <- xl_numfmt(predict_summary, Value = c(XL_NUM, XL_NUM, XL_NUM, rep(XL_PCT, 6)))
        
        wb <- ceres_ns("openxlsx2")$wb_workbook() %>% ceres_ns("openxlsx2")$wb_add_worksheet("Linear Statistics")
        
        wb$add_data(x = "Linear Analysis Output", dims = "B2")
        wb$add_data(x = paste("Date:", Sys.time()), dims = "B3")
        if (isFALSE(attr(model, "irls_converged")))
          wb$add_data(x = paste0("WARNING: the fit did not converge (", attr(model, "irls_iterations"),
                                 " iterations) - every statistic below is for the last iterate."), dims = "B4")
        
        wb$add_data(x = "I. Model Form and Equation Table", dims = "B5")
        wb$add_font(dims = "B5", bold = "1")
        
        wb$merge_cells(dims = "C7:K7")
        wb$merge_cells(dims = "C8:K8")
        wb$merge_cells(dims = "C9:K9")

        wb$add_data(x = "Model Form:", dims = "B7")
        wb$add_data(x = model_type_str, dims = "C7")
        wb$add_data(x = "Number of Observations Used:", dims = "B8")
        wb$add_data(x = n_obs, dims = "C8")
        wb$add_cell_style(dims = "C8", horizontal = "left")
        wb$add_data(x = "Equation in Unit Space:", dims = "B9")
        wb$add_data(x = equation_str, dims = "C9")
        
        style_range_borders(wb, rows = 7:9, cols = 2:11)
        wb$add_font(dims = "B7:B9", bold = "1") 
        
        wb$add_data(x = "II. Fit Measures (in Fit Space)", dims = "B11")
        wb$add_font(dims = "B11", bold = "1")
        
        wb$add_data(x = "Coefficient Statistics Summary", dims = "B13")
        wb$add_font(dims = "B13", bold = "1")
        wb$add_data(x = coef_out, dims = "B14", na = NULL)
        style_table_borders(wb, 1, 14, 2, coef_out) 
        
        curr_row <- 14 + nrow(coef_out) + 2
        
        wb$add_data(x = "Goodness-of-Fit Statistics", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = gof_df, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, gof_df)
        if (!is.finite(press_stat) && !is.na(press_stat))
          wb$add_data(x = "Inf", dims = paste0("C", curr_row + 1 + 5))
        
        curr_row <- curr_row + nrow(gof_df) + 3
        
        wb$add_data(x = "Analysis of Variance", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = anova_df, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, anova_df)
        
        curr_row <- curr_row + nrow(anova_df) + 3
        
        wb$add_data(x = "Pairwise Correlation Matrix", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = cor_out, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, cor_out)
        
        curr_row <- curr_row + nrow(cor_out) + 3
        
        wb$add_data(x = "Multicollinearity Analysis (VIF)", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        
        if (nrow(vif_out) > 0) {
          wb$add_data(x = vif_out, dims = paste0("B", curr_row + 1), na = NULL)
          style_table_borders(wb, 1, curr_row + 1, 2, vif_out)
          curr_row <- curr_row + nrow(vif_out) + 1
          if ("Df" %in% names(vif_out)) {
            wb$add_data(x = "Rows with Df > 1 (a factor with 3+ levels): VIF is the generalised VIF (GVIF) of the whole factor; it is graded as GVIF^(1/Df) on the usual 5 / 10 VIF scale (Severity, and Max VIF everywhere in CERES); Indiv R-Squared and F are not defined and are left empty.",
                        dims = paste0("B", curr_row + 2))
            curr_row <- curr_row + 2
          }
        } else {
          wb$add_data(x = "N/A (Not enough predictors)", dims = paste0("B", curr_row + 1))
          curr_row <- curr_row + 2
        }
        
        curr_row <- curr_row + 2 
        
        wb$add_data(x = "Potential Outlier Analysis Summary", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = outlier_summary, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, outlier_summary)
        
        curr_row <- curr_row + nrow(outlier_summary) + 3
        
        wb$add_data(x = "Potential Outlier Analysis Table (All Observations)", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = outlier_table, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, outlier_table)
        
        rows_outlier <- (curr_row + 2):(curr_row + 1 + nrow(outlier_table))
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_outlier, cols = 2), horizontal = "center")
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_outlier, cols = 8:11), horizontal = "center")
        wb$add_font(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_outlier, cols = 8:11), bold = "1")
        
        curr_row <- curr_row + nrow(outlier_table) + 3
        
        wb$add_data(x = "III. Predictive Measures (in Unit Space)", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        
        curr_row <- curr_row + 1
        
        wb$add_data(x = "Percentage Error Table", dims = paste0("B", curr_row + 1)) 
        wb$add_font(dims = paste0("B", curr_row + 1), bold = "1")
        
        wb$add_data(x = predict_table, dims = paste0("B", curr_row + 2), na = NULL)
        style_table_borders(wb, 1, curr_row + 2, 2, predict_table)
        
        rows_pred <- (curr_row + 3):(curr_row + 2 + nrow(predict_table))
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_pred, cols = 2), horizontal = "center")
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_pred, cols = 8), horizontal = "center")
        wb$add_font(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_pred, cols = 8), bold = "1")
        
        wb$add_data(x = "Residuals = Actuals - Predicted. Pct_Errors = (Predicted - Actuals) / Predicted.",
                    dims = paste0("B", curr_row + 2 + nrow(predict_table) + 2))
        curr_row <- curr_row + 2 + nrow(predict_table) + 4
        
        wb$add_data(x = "Summary of Predictive Measures", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = predict_summary, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, predict_summary)
        
        wb$set_col_widths(cols = 2, widths = 32)
        wb$set_col_widths(cols = 3:11, widths = 19.22)

        wb <- wb %>% ceres_ns("openxlsx2")$wb_add_worksheet("Excluded Data Points")

        excl_labels_for_export   <- attr(model, "tuning_excluded_labels")
        excl_snapshot_for_export <- attr(model, "tuning_excluded_rows_df")
        was_pushed_for_export    <- isTRUE(attr(model, "tuning_pushed"))
        if (is.null(excl_labels_for_export) || !was_pushed_for_export) {
          excl_labels_for_export <- character(0)
        }

        wb$add_data(x = "Excluded Data Points (CER Tuning)", dims = "B2")
        wb$add_font(dims = "B2", bold = "1")
        wb$add_data(x = paste("Date:", Sys.time()), dims = "B3")

        wb$add_data(x = "Source:", dims = "B5")
        wb$add_data(x = if (was_pushed_for_export)
          "Pushed Refit (exclusions captured at push time)"
          else "No exclusions applied to this run (live CER Tuning toggles were not pushed)",
          dims = "C5")
        wb$add_data(x = "Number Excluded:", dims = "B6")
        wb$add_data(x = length(excl_labels_for_export), dims = "C6")
        wb$add_cell_style(dims = "C6", horizontal = "left")
        wb$add_font(dims = "B5:B6", bold = "1")

        if (length(excl_labels_for_export) > 0L) {
          if (!is.null(excl_snapshot_for_export) && nrow(excl_snapshot_for_export) > 0L) {
            src_df_excl <- excl_snapshot_for_export
          } else {
            rn_m <- rownames(model$model)
            keep_mask_excl <- !is.null(rn_m) & (rn_m %in% excl_labels_for_export)
            src_df_excl <- if (any(keep_mask_excl)) model$model[keep_mask_excl, , drop = FALSE] else data.frame()
          }
          if (nrow(src_df_excl) > 0L) {
            disp_df_excl <- src_df_excl
            disp_df_excl <- disp_df_excl[, setdiff(names(disp_df_excl), "(weights)"),
                                         drop = FALSE]
            if ("y" %in% names(disp_df_excl)) {
              names(disp_df_excl)[names(disp_df_excl) == "y"] <- real_response_name
            }
            excl_table_out <- data.frame(
              Obs = seq_len(nrow(disp_df_excl)),
              Name = rownames(disp_df_excl),
              stringsAsFactors = FALSE
            )
            for (cn in names(disp_df_excl)) {
              col_vals <- disp_df_excl[[cn]]
              excl_table_out[[cn]] <- if (is.numeric(col_vals)) fmt_num(col_vals) else as.character(col_vals)
            }
            excl_table_out <- clean_df(excl_table_out)
            excl_table_out <- xl_numfmt_numeric(excl_table_out, XL_NUM)
            attr(excl_table_out, "xl_numfmt")[["Obs"]] <- NULL
            wb$add_data(x = "Excluded Observations", dims = "B8")
            wb$add_font(dims = "B8", bold = "1")
            wb$add_data(x = excl_table_out, dims = "B9", na = NULL)
            style_table_borders(wb, 2, 9, 2, excl_table_out)
            rows_excl <- 10:(9 + nrow(excl_table_out))
            wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_excl, cols = 2), horizontal = "center")
          } else {
            wb$add_data(x = "Labels were recorded but matching rows could not be retrieved.",
                        dims = "B8")
          }
        } else {
          wb$add_data(x = "No data points were excluded for this run.", dims = "B8")
        }
        wb$set_col_widths(cols = 2, widths = 14)
        wb$set_col_widths(cols = 3, widths = 22)
        wb$set_col_widths(cols = 4:11, widths = 19.22)

        wb$save(file)

      }, error = function(e) {
        wb <- ceres_ns("openxlsx2")$wb_workbook() %>% ceres_ns("openxlsx2")$wb_add_worksheet("Error Log")
        wb$add_data(x = "An error occurred during export:", dims = "A1")
        wb$add_data(x = as.character(e), dims = "A2")
        wb$save(file)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  nl_fitted_equation <- function() {
    form  <- rv$nl_fit_formula %||%
             tryCatch(nlls_formula_string(), error = function(e) NA_character_)
    model <- rv$nl_model
    if (is.null(model) || length(form) != 1L || is.na(form)) return(form)
    co <- coef(model)
    for (p in names(co)[order(nchar(names(co)), decreasing = TRUE)]) {
      v <- co[[p]]
      if (!is.finite(v)) next
      rep <- if (v < 0) paste0("(-", fmt_eqn(v), ")") else fmt_eqn(v)
      p_rx <- gsub("([^A-Za-z0-9_])", "\\\\\\1", p)
      form <- gsub(paste0("\\b", p_rx, "\\b"), rep, form)
    }
    form
  }

  build_nl_workbook <- function(file) {

      tryCatch({
        req(rv$nl_model, rv$nl_sets, rv$nl_coeffs)
        model   <- rv$nl_model
        y_name  <- rv$nl_fit_response %||% input$nl_response %||% "Response"
        eq_name <- rv$nl_fit_eq_name  %||% input$nl_equation %||% "Custom"
        log_y   <- isTRUE(rv$nl_fit_log_y)
        model_type_str <- if (identical(eq_name, "Custom")) "Non-Linear model (Custom equation)"
                          else paste0("Non-Linear model (", eq_name, ")")
        if (log_y) model_type_str <- paste0(model_type_str, " — response fit in log space")

        y_fit      <- as.numeric(rv$nl_sets[["y_train"]])
        yhat_fit   <- as.numeric(rv$nl_sets[["yhat_train"]])
        data_train <- rv$nl_sets[["data_train"]]
        n_obs      <- length(y_fit)
        row_names  <- names(rv$nl_sets[["yhat_train"]])
        if (is.null(row_names) || length(row_names) != n_obs) row_names <- rownames(data_train)
        if (is.null(row_names) || length(row_names) != n_obs) row_names <- seq_len(n_obs)

        y_actual <- if (log_y) exp(y_fit)    else y_fit
        y_pred   <- if (log_y) exp(yhat_fit) else yhat_fit
        residuals_val <- y_actual - y_pred

        equation_str <- tryCatch(nl_fitted_equation(), error = function(e) NA_character_)
        if (is.null(equation_str) || length(equation_str) != 1L || is.na(equation_str))
          equation_str <- "(equation unavailable)"
        equation_str <- nl_display_names(equation_str,
                                         names(rv$dataset_library[[input$data_select_cer_generation]] %||% list()))

        co_mat   <- rv$nl_coeffs
        n_params <- nrow(co_mat)
        df_res   <- tryCatch(stats::df.residual(model), error = function(e) NA_integer_)
        if (!is.finite(df_res) || df_res <= 0) df_res <- max(n_obs - n_params, 1L)
        est_v  <- as.numeric(co_mat[, 1])
        se_v   <- as.numeric(co_mat[, 2])
        t_stat <- ifelse(is.finite(se_v) & se_v > 0, est_v / se_v, NA_real_)
        p_val  <- 2 * pt(-abs(t_stat), df_res)
        coef_out <- data.frame(
          Parameter       = rownames(co_mat),
          Coefficient     = fmt_num(est_v),
          Std_Dev_of_Coef = fmt_num(se_v),
          T_Statistic     = fmt_num(t_stat),
          P_Value         = fmt_num(p_val),
          Prob_Not_Zero   = fmt_num(1 - p_val),
          CI_95_Low       = fmt_num(as.numeric(co_mat[, 3])),
          CI_95_High      = fmt_num(as.numeric(co_mat[, 4])),
          Unit_Space_Value    = est_v,
          Unit_Space_Lower_95 = as.numeric(co_mat[, 3]),
          Unit_Space_Upper_95 = as.numeric(co_mat[, 4]),
          stringsAsFactors = FALSE
        )
        names(coef_out)[9:11] <- c("Unit-Space Value", "Unit-Space Lower 95%", "Unit-Space Upper 95%")

        sigma_fit <- tryCatch(stats::sigma(model), error = function(e) NA_real_)
        ss_res  <- sum(residuals_val^2)
        y_bar   <- mean(y_actual)
        ss_tot  <- sum((y_actual - y_bar)^2)
        r2_unit <- if (is.finite(ss_tot) && ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
        adj_r2_unit <- if (is.finite(r2_unit))
          1 - (1 - r2_unit) * ((n_obs - 1) / max(n_obs - n_params, 1)) else NA_real_
        pearson_r <- tryCatch(cor(y_actual, y_pred), error = function(e) NA_real_)
        gof_df <- data.frame(
          Metric = c("Std Error (SE, fit space)", "Pseudo R-Squared (Unit Space)",
                     "Pseudo R-Squared (Adj)", "Pearson's Corr Coef", "Residual DF"),
          Value  = c(fmt_num(sigma_fit), fmt_pct(r2_unit), fmt_pct(adj_r2_unit),
                     fmt_pct(pearson_r), df_res)
        )

        num_ct  <- vapply(data_train, is.numeric, logical(1))
        cor_out <- data.frame()
        if (sum(num_ct) > 1L) {
          cor_mat <- tryCatch(cor(data_train[, num_ct, drop = FALSE],
                                  use = "pairwise.complete.obs"),
                              error = function(e) NULL)
          if (!is.null(cor_mat)) {
            cor_mat_fmt <- apply(cor_mat, 2, function(x) fmt_num(x))
            cor_out <- as.data.frame(cor_mat_fmt)
            cor_out <- cbind(Variable = rownames(cor_mat), cor_out)
          }
        }

        pct_error  <- (y_pred - y_actual) / ifelse(abs(y_pred) > 1e-9, y_pred, 1e-9)
        sigma_unit <- sd(residuals_val)
        std_res_simple <- if (is.finite(sigma_unit) && sigma_unit > 0)
          residuals_val / sigma_unit else rep(0, n_obs)
        flags <- ifelse(abs(std_res_simple) > 2, "*", "")
        predict_table <- data.frame(
          Obs = seq_len(n_obs), Name = row_names, Actuals = fmt_num(y_actual),
          Predicted = fmt_num(y_pred), Residuals = fmt_num(residuals_val),
          Pct_Errors = fmt_pct(pct_error), Flags = flags
        )
        avg_act  <- mean(y_actual)
        rmse     <- sqrt(mean(residuals_val^2))
        mad_res  <- mean(abs(residuals_val))
        avg_pct_err_arith <- mean(pct_error)
        avg_pct_err_abs   <- mean(abs(pct_error))
        rms_pct_error     <- sqrt(mean(pct_error^2))
        cv_val   <- if (abs(avg_act) > 1e-12) rmse / abs(avg_act) else NA_real_
        predict_summary <- data.frame(
          Metric = c("Average Actual", "RMSE", "Avg Residual (Absolute)",
                     "Avg % Error (Arithmetic)", "Avg % Error (Absolute)",
                     "RMS % of Errors", "Coef of Variation", "Pearson's Correlation",
                     "Adjusted R-Squared in Unit Space"),
          Value  = c(fmt_num(avg_act), fmt_num(rmse), fmt_num(mad_res),
                     fmt_pct(avg_pct_err_arith), fmt_pct(avg_pct_err_abs),
                     fmt_pct(rms_pct_error), fmt_pct(cv_val), fmt_pct(pearson_r),
                     fmt_pct(adj_r2_unit))
        )

        coef_out        <- clean_df(coef_out)
        gof_df          <- clean_df(gof_df)
        cor_out         <- clean_df(cor_out)
        predict_table   <- clean_df(predict_table)
        predict_summary <- clean_df(predict_summary)

        coef_out <- xl_numfmt(coef_out,
          Coefficient = xl_eqn_fmt(coef_out$Coefficient),
          Std_Dev_of_Coef = XL_NUM, T_Statistic = XL_NUM, P_Value = XL_NUM,
          Prob_Not_Zero = XL_NUM, CI_95_Low = XL_NUM, CI_95_High = XL_NUM,
          `Unit-Space Value` = xl_eqn_fmt(coef_out[["Unit-Space Value"]]),
          `Unit-Space Lower 95%` = xl_eqn_fmt(coef_out[["Unit-Space Lower 95%"]]),
          `Unit-Space Upper 95%` = xl_eqn_fmt(coef_out[["Unit-Space Upper 95%"]]))
        gof_df          <- xl_numfmt(gof_df, Value = c(XL_NUM, XL_PCT, XL_PCT, XL_PCT, XL_INT))
        cor_out         <- xl_numfmt_numeric(cor_out, XL_NUM)
        predict_table   <- xl_numfmt(predict_table, Actuals = XL_NUM, Predicted = XL_NUM,
                                     Residuals = XL_NUM, Pct_Errors = XL_PCT)
        predict_summary <- xl_numfmt(predict_summary, Value = c(XL_NUM, XL_NUM, XL_NUM, rep(XL_PCT, 6)))

        wb <- ceres_ns("openxlsx2")$wb_workbook() %>% ceres_ns("openxlsx2")$wb_add_worksheet("Non-Linear Statistics")

        wb$add_data(x = "Non-Linear Analysis Output", dims = "B2")
        wb$add_data(x = paste("Date:", Sys.time()), dims = "B3")

        wb$add_data(x = "I. Model Form and Equation Table", dims = "B5")
        wb$add_font(dims = "B5", bold = "1")

        wb$merge_cells(dims = "C7:K7")
        wb$merge_cells(dims = "C8:K8")
        wb$merge_cells(dims = "C9:K9")

        wb$add_data(x = "Model Form:", dims = "B7")
        wb$add_data(x = model_type_str, dims = "C7")
        wb$add_data(x = "Number of Observations Used:", dims = "B8")
        wb$add_data(x = n_obs, dims = "C8")
        wb$add_cell_style(dims = "C8", horizontal = "left")
        wb$add_data(x = "Fitted Equation:", dims = "B9")
        wb$add_data(x = equation_str, dims = "C9")

        style_range_borders(wb, rows = 7:9, cols = 2:11)
        wb$add_font(dims = "B7:B9", bold = "1")

        wb$add_data(x = "II. Fit Measures", dims = "B11")
        wb$add_font(dims = "B11", bold = "1")

        wb$add_data(x = "Coefficient Statistics Summary", dims = "B13")
        wb$add_font(dims = "B13", bold = "1")
        wb$add_data(x = coef_out, dims = "B14", na = NULL)
        style_table_borders(wb, 1, 14, 2, coef_out)

        curr_row <- 14 + nrow(coef_out) + 2

        wb$add_data(x = "Goodness-of-Fit Statistics", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = gof_df, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, gof_df)

        curr_row <- curr_row + nrow(gof_df) + 3

        wb$add_data(x = "Pairwise Correlation Matrix", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        if (nrow(cor_out) > 0) {
          wb$add_data(x = cor_out, dims = paste0("B", curr_row + 1), na = NULL)
          style_table_borders(wb, 1, curr_row + 1, 2, cor_out)
          curr_row <- curr_row + nrow(cor_out) + 3
        } else {
          wb$add_data(x = "N/A (not enough numeric variables)", dims = paste0("B", curr_row + 1))
          curr_row <- curr_row + 4
        }

        wb$add_data(x = "ANOVA / VIF / influence diagnostics are not defined for non-linear least-squares fits and are omitted.",
                    dims = paste0("B", curr_row))
        curr_row <- curr_row + 2

        wb$add_data(x = "III. Predictive Measures (in Unit Space)", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")

        curr_row <- curr_row + 1

        wb$add_data(x = "Percentage Error Table", dims = paste0("B", curr_row + 1))
        wb$add_font(dims = paste0("B", curr_row + 1), bold = "1")

        wb$add_data(x = predict_table, dims = paste0("B", curr_row + 2), na = NULL)
        style_table_borders(wb, 1, curr_row + 2, 2, predict_table)

        rows_pred <- (curr_row + 3):(curr_row + 2 + nrow(predict_table))
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_pred, cols = 2), horizontal = "center")
        wb$add_cell_style(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_pred, cols = 8), horizontal = "center")
        wb$add_font(dims = ceres_ns("openxlsx2")$wb_dims(rows = rows_pred, cols = 8), bold = "1")

        wb$add_data(x = "Residuals = Actuals - Predicted. Pct_Errors = (Predicted - Actuals) / Predicted.",
                    dims = paste0("B", curr_row + 2 + nrow(predict_table) + 2))
        curr_row <- curr_row + 2 + nrow(predict_table) + 4

        wb$add_data(x = "Summary of Predictive Measures", dims = paste0("B", curr_row))
        wb$add_font(dims = paste0("B", curr_row), bold = "1")
        wb$add_data(x = predict_summary, dims = paste0("B", curr_row + 1), na = NULL)
        style_table_borders(wb, 1, curr_row + 1, 2, predict_summary)

        wb$set_col_widths(cols = 2, widths = 32)
        wb$set_col_widths(cols = 3:12, widths = 19.22)

        wb <- wb %>% ceres_ns("openxlsx2")$wb_add_worksheet("Excluded Data Points")
        wb$add_data(x = "Excluded Data Points (CER Tuning)", dims = "B2")
        wb$add_font(dims = "B2", bold = "1")
        wb$add_data(x = paste("Date:", Sys.time()), dims = "B3")
        wb$add_data(x = "Source:", dims = "B5")
        wb$add_data(x = "N/A — CER Tuning exclusions are not available for Non-Linear models", dims = "C5")
        wb$add_data(x = "Number Excluded:", dims = "B6")
        wb$add_data(x = 0L, dims = "C6")
        wb$add_cell_style(dims = "C6", horizontal = "left")
        wb$add_font(dims = "B5:B6", bold = "1")
        wb$set_col_widths(cols = 2, widths = 14)
        wb$set_col_widths(cols = 3, widths = 22)

        wb$save(file)
        TRUE

      }, error = function(e) {
        wb <- ceres_ns("openxlsx2")$wb_workbook() %>% ceres_ns("openxlsx2")$wb_add_worksheet("Error Log")
        wb$add_data(x = "An error occurred during export:", dims = "A1")
        wb$add_data(x = as.character(e), dims = "A2")
        wb$save(file)
        FALSE
      })
  }

  output$export_nl_excel <- downloadHandler(
    filename = function() {
      data_source <- tools::file_path_sans_ext(input$data_upload$name %||% "Dataset")
      timestamp <- format(Sys.time(), "%H%M%S")
      date_stamp <- format(Sys.Date(), "%Y%m%d")
      paste0(data_source, "_NonLinear_Statistics_", date_stamp, "_", timestamp, ".xlsx")
    },
    content = function(file) build_nl_workbook(file),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  outputOptions(output, "export_nl_excel", suspendWhenHidden = FALSE)
  outputOptions(output, "report_nl", suspendWhenHidden = FALSE)

  if (!CERES_IN_BROWSER) {
  precompile_quiet_from <- Sys.time() + 3
  precompile_stop <- session$onFlush(function() precompile_quiet_from <<- Sys.time() + 2, once = FALSE)
  precompile_tick <- function() {
    if (session$isClosed()) return(invisible())
    wait <- as.numeric(difftime(precompile_quiet_from, Sys.time(), units = "secs"))
    if (wait > 0) return(later::later(precompile_tick, wait + 0.05))
    precompile_stop()
    run_cer_fitting <<- ceres_precompile(run_cer_fitting)
  }
  later::later(precompile_tick, 3)
  }
  ceres_mark("session server() done")
  session$onFlushed(function() ceres_mark("first output flushed"), once = TRUE)

  if (CERES_IN_BROWSER) {
    preload <- c("rpart", "rpart.plot", "nortest", "MASS", "Deriv", "minpack.lm", "bnlearn",
                 "quantreglite", "iBreakDownlite", "DALEXlite", "openxlsx2")
    observeEvent(input$ols_penalty, {
      if (!identical(input$ols_penalty, "None") && !isNamespaceLoaded("glmnet"))
        later::later(function() try(ceres_ns_full("glmnet"), silent = TRUE), 0.2)
    }, ignoreInit = TRUE)
    observeEvent(input$ceres_preload, {
      i <- suppressWarnings(as.integer(input$ceres_preload))
      if (length(i) == 1L && !is.na(i) && i >= 1L && i <= length(preload))
        try(ceres_ns_full(preload[i]), silent = TRUE)
      if (identical(i, length(preload))) ceres_mark("background packages loaded")
      session$sendCustomMessage("ceres_preload", list(nxt = i + 1L, n = length(preload)))
    })
    session$onFlushed(function() session$sendCustomMessage("ceres_preload", list(nxt = 1L, n = length(preload))), once = TRUE)
  }
}

CERES_APP_DIR <- normalizePath(getwd(), winslash = "/")
ceres_resource_map <- function() {
  rp   <- shiny::resourcePaths()
  libs <- normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)
  libs <- libs[order(-nchar(libs))]
  lapply(names(rp), function(pf) {
    d <- normalizePath(rp[[pf]], winslash = "/", mustWork = FALSE)
    for (l in libs) if (startsWith(d, paste0(l, "/"))) {
      rest <- substring(d, nchar(l) + 2)
      return(list(prefix = pf, kind = "package", package = sub("/.*", "", rest),
                  sub = if (grepl("/", rest)) sub("^[^/]+/", "", rest) else ""))
    }
    if (startsWith(d, paste0(CERES_APP_DIR, "/")))
      return(list(prefix = pf, kind = "app", sub = substring(d, nchar(CERES_APP_DIR) + 2)))
    if (!startsWith(d, paste0(normalizePath(tempdir(), winslash = "/"), "/")))
      return(list(prefix = pf, kind = "unknown"))
    fs <- list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    list(prefix = pf, kind = "other",
         files = lapply(fs, function(f) list(name = f, b64 = jsonlite::base64_enc(readBin(file.path(d, f), "raw", file.size(file.path(d, f)))))))
  })
}
ceres_prerendered_ui <- function(build_ui, dir = "ceres-prerender") {
  page <- file.path(dir, "page.html"); map <- file.path(dir, "paths.json")
  if (!CERES_IN_BROWSER || !file.exists(page) || !file.exists(map)) return(build_ui())
  m <- tryCatch(jsonlite::fromJSON(map, simplifyVector = FALSE), error = function(e) NULL)
  html <- tryCatch(readChar(page, file.size(page), useBytes = TRUE), error = function(e) NULL)
  if (is.null(m) || !is.character(html) || !nzchar(html) || !identical(m$app_md5, unname(tools::md5sum("app.R")))) {
    message("CERES: the saved start page is unreadable or for other code; building it instead")
    return(build_ui())
  }
  Encoding(html) <- "UTF-8"
  ready <- NA; built <- NULL
  register <- function() {
    for (p in m$paths) {
      d <- switch(p$kind,
                  package = { r <- find.package(p$package, quiet = TRUE); if (length(r)) file.path(r[1], p$sub) else "" },
                  app = file.path(CERES_APP_DIR, p$sub),
                  "")
      if (!dir.exists(d)) return(FALSE)
      shiny::addResourcePath(p$prefix, d)
    }
    shiny::shinyOptions(bootstrapTheme = light_theme)
    all(vapply(m$paths, function(p) p$prefix %in% names(shiny::resourcePaths()), TRUE))
  }
  function(req) {
    if (is.na(ready)) {
      ready <<- isTRUE(tryCatch(register(), error = function(e) { message("CERES: ", conditionMessage(e)); FALSE }))
      if (!ready) message("CERES: the saved start page could not be used; building it instead")
    }
    if (ready) return(shiny::httpResponse(200, content = html))
    if (is.null(built)) built <<- build_ui()
    built
  }
}
ui <- ceres_prerendered_ui(ceres_build_ui)

shinyApp(ui = ui, server = server, options = list(height = 1000))
