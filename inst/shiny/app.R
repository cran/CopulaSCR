library(shiny)
library(CopulaSCR)
source("app_helpers.R", local = TRUE)
source("plot_colors.R", local = TRUE)
source("parameter_help.R", local = TRUE)
source("model_storage.R", local = TRUE)
source("prediction_scenarios.R", local = TRUE)
options(shiny.maxRequestSize = 50 * 1024^2)

steps <- c("1  Import data" = "import", "2  Map variables" = "mapping",
  "3  Association analysis" = "association", "4  Marginal analysis" = "marginal",
  "5  Survival prediction" = "prediction")

ui <- fluidPage(
  tags$head(parameter_help_assets()),
  tags$head(color_reset_script()),
  tags$head(tags$style(HTML("
    body{background:#f7f8fa;color:#20282f;font-family:Arial,sans-serif}
    *{letter-spacing:0!important}
    .app-header{background:white;border-bottom:1px solid #dbe0e5;padding:18px 26px;
      margin:0 -15px;display:flex;align-items:center;justify-content:space-between;
      gap:18px;flex-wrap:wrap}
    .app-header h1{font-size:25px;margin:0;font-weight:700}
    .app-header span{color:#5c6773;font-size:13px}
    .workspace{display:grid;grid-template-columns:290px minmax(0,1fr);
      min-height:calc(100vh - 66px);margin:0 -15px}
    aside{background:white;border-right:1px solid #dbe0e5;padding:24px 20px}
    main{min-width:0;padding:28px 34px}
    h2{font-size:22px;margin:0 0 22px}h4{font-size:14px;margin-top:22px}
    .stepnav .radio{margin:0}
    .stepnav .radio label{display:block;padding:13px 12px;margin-bottom:5px;
      border-left:3px solid transparent;font-weight:500}
    .stepnav input{position:absolute;opacity:0}
    .stepnav label:has(input:checked){background:#e9f4f1;color:#08725e;
      border-left-color:#08725e;font-weight:700}
    .stepnav label:focus-within{outline:2px solid #08725e}
    .controls{border-top:1px solid #e4e7eb;padding-top:20px;margin-top:22px}
    .btn{border-radius:4px;white-space:normal}
    .btn-primary{background:#08725e;border-color:#08725e}
    .btn-primary:hover,.btn-primary:focus{background:#075b4c;border-color:#075b4c}
    .form-control,.selectize-input{border-radius:4px}
    .prediction-sample{display:flex;gap:6px;align-items:flex-end;margin-bottom:10px}
    .prediction-condition{flex:1;min-width:0}.prediction-condition .form-group{margin-bottom:0}
    .sample-button{flex:0 0 30px;width:30px;height:34px;padding:0;font-size:20px;line-height:30px}
    .nav-tabs{margin-bottom:22px;border-bottom:1px solid #dbe0e5}
    .nav-tabs>li>a{color:#55616d}.nav-tabs>li.active>a{color:#08725e;font-weight:600}
    .result-body{background:white;padding:22px;border-top:2px solid #dbe0e5}
    .empty{color:#6a7580;padding:65px 10px;text-align:center}
    .metrics{display:flex;gap:38px;padding:12px 0 24px;flex-wrap:wrap}
    .metric b{display:block;font-size:25px;color:#08725e}
    .metric span{color:#63717c;font-size:12px}.table-wrap{overflow-x:auto}
    table{background:white;font-size:13px}th{color:#59656f}
    pre{background:#f4f6f8;border:0;border-radius:4px;font-size:12px}
    .shiny-output-error-validation{color:#986024;padding:18px 0}
    .download-row{display:flex;gap:10px;margin:18px 0;flex-wrap:wrap}
    .notice{color:#697680;font-size:12px;margin:12px 0}
    .plot-colors{border-top:1px solid #e4e7eb;margin-top:22px;padding-top:12px}
    .color-section{margin:12px 0}.color-section summary{cursor:pointer;font-weight:600;font-size:13px}
    .color-swatches{display:grid;grid-template-columns:1fr 1fr;gap:8px;margin:10px 0}
    .color-swatch{display:flex;align-items:center;gap:6px;font-size:11px;font-weight:400;min-width:0}
    .color-swatch span{overflow-wrap:anywhere}
    .color-swatch input[type=color]{width:30px;height:28px;flex:0 0 30px;padding:1px;
      background:white;border:1px solid #ccd2d8;border-radius:3px;cursor:pointer}
    @media(max-width:900px){.workspace{grid-template-columns:245px minmax(0,1fr)}
      main{padding:22px 16px}aside{padding:20px 14px}}
    @media(max-width:650px){.workspace{display:block}
      aside{border-right:0;border-bottom:1px solid #dbe0e5}
      .stepnav .shiny-options-group{display:grid;grid-template-columns:1fr 1fr}
      .stepnav .radio label{padding:10px 5px;font-size:12px}
      .app-header{padding:15px 20px}.result-body{padding:12px}}
  "))),
  tags$header(class = "app-header", h1("CopulaSCR"), span("Analysis workspace")),
  div(class = "workspace",
    tags$aside(
      div(class = "stepnav", radioButtons("step", NULL, steps, selected = "import")),
      div(class = "controls", uiOutput("controls")),
      tags$details(class = "controls", tags$summary("Saved models"),
        p(class = "notice", "Choose whether to cache when you click Fit model. The cache includes the fit, dataset and variable mapping; predictions and images are not cached."),
        cache_directory_control("cache_directory", "choose_cache_directory", model_cache_directory),
        checkboxInput("reuse_model", parameter_label("reuse_model", "Reuse matching cached fits"), TRUE),
        actionButton("restore_cached_model", "Restore latest cached model"),
        fileInput("model_file", parameter_label("model_file", "Import analysis model RDS"), accept = ".rds"),
        textOutput("model_status")),
      conditionalPanel("['association','marginal','prediction'].includes(input.step)",
        div(class = "plot-colors", h4("Plot colors"), uiOutput("color_controls"))),
      div(class = "notice", textOutput("fit_status"))),
    tags$main(uiOutput("result_view")))
)

server <- function(input, output, session) {
  cache_enabled <- reactiveVal(TRUE)
  pending_fit <- reactiveVal(NULL)
  selected_cache_directory <- function() {
    value <- input$cache_directory
    if (is.null(value) || !nzchar(trimws(value))) model_cache_directory else trimws(value)
  }
  select_cache_folder <- function(id, default) {
    selected <- tryCatch(choose_cache_directory(default), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 15)
      NULL
    })
    if (!is.null(selected)) updateTextInput(session, id, value = selected)
  }
  observeEvent(input$choose_cache_directory, {
    select_cache_folder("cache_directory", selected_cache_directory())
  }, ignoreInit = TRUE)
  observeEvent(input$choose_fit_cache_directory, {
    directory <- input$fit_cache_directory
    if (is.null(directory)) directory <- selected_cache_directory()
    select_cache_folder("fit_cache_directory", directory)
  }, ignoreInit = TRUE)
  state <- reactiveValues(mapped = NULL, assoc = NULL, marginal = NULL,
    prediction = NULL, config = NULL, prediction_times = NULL,
    prediction_type = NULL, prediction_marginal = NULL,
    prediction_marginal_settings = NULL,
    restored_raw = NULL, restored_settings = NULL, model_origin = NULL,
    model_status = "No saved model loaded.")
  setting <- function(id, default) {
    value <- state$restored_settings[[id]]
    if (is.null(value)) default else value
  }
  clear_results <- function() {
    state$assoc <- state$marginal <- state$prediction <- NULL
    state$config <- NULL
    state$model_origin <- NULL
    state$prediction_marginal <- NULL
    state$prediction_marginal_settings <- NULL
  }
  uploaded_tables <- reactive({
    req(input$file)
    read_analysis_upload(input$file$datapath,
      if (is.null(input$file$name)) input$file$datapath else input$file$name,
      if (is.null(input$sep)) "," else input$sep,
      if (is.null(input$encoding)) "UTF-8" else input$encoding)
  })
  raw_data <- reactive({
    if (!is.null(state$restored_raw)) return(state$restored_raw)
    tables <- uploaded_tables()
    selected <- if (length(tables) == 1L) names(tables)[1] else input$data_object
    validate(need(!is.null(selected) && selected %in% names(tables), "Select a dataset object."))
    validate_analysis_table(tables[[selected]])
  })
  observeEvent(raw_data(), {
    if (!is.null(state$restored_raw)) return()
    k <- attr(raw_data(), "simulation_event_count")
    if (is.null(k)) return()
    updateRadioButtons(session, "mode", selected = if (k > 1L) "multiple" else "single")
    updateRadioButtons(session, "record_format", selected = "observed")
    if (k > 1L) updateNumericInput(session, "event_count", value = k)
  })
  observeEvent(list(input$file, input$sep, input$encoding, input$data_object), {
    state$restored_raw <- NULL
    state$restored_settings <- NULL
    state$mapped <- NULL
    clear_results()
  }, ignoreInit = TRUE)
  model_input_settings <- function() {
    ids <- c("mode", "record_format", "event_count", "terminal_time", "terminal_status",
      "death_time", "group", "group_binary", "family", "B", "se_method", "fit_seed",
      "marg_method", "km", "conf", paste0("time_", seq_len(state$mapped$k)),
      paste0("status_", seq_len(state$mapped$k)))
    values <- isolate(reactiveValuesToList(input))
    settings <- utils::modifyList(if (is.null(state$restored_settings)) list() else state$restored_settings,
      values[intersect(ids, names(values))])
    x <- state$mapped
    settings$mode <- if (x$k > 1) "multiple" else if (x$stratified) "stratified" else "single"
    settings$event_count <- x$k
    if (!is.null(x$mapping)) {
      settings <- utils::modifyList(settings, x$mapping[setdiff(names(x$mapping), "pairs")])
      for (j in seq_len(x$k)) {
        settings[[paste0("time_", j)]] <- x$mapping$pairs[[j]]$time
        settings[[paste0("status_", j)]] <- x$mapping$pairs[[j]]$status
      }
    }
    settings$family <- state$config$family
    settings$B <- state$config$B
    settings$fit_seed <- state$config$seed
    settings$se_method <- state$config$se_method
    settings
  }
  current_model_bundle <- function() make_model_bundle(state$mapped, state$assoc,
    state$marginal, state$config, model_input_settings(),
    state$prediction_marginal, state$prediction_marginal_settings)
  cache_current_model <- function(directory = selected_cache_directory()) {
    if (!cache_enabled()) {
      state$model_status <- "Model is available in this session only; no cache was written. You can export an Analysis model RDS."
      return(invisible(NULL))
    }
    tryCatch({
      bundle <- make_model_bundle(state$mapped, state$assoc, NULL,
        state$config, model_input_settings())
      path <- write_model_cache(bundle, directory = directory)
      state$model_status <- paste("Model cached at:", path)
    }, error = function(e) {
      state$model_status <- paste("Model is available in this session; cache save failed:", conditionMessage(e))
      showNotification(state$model_status, type = "warning", duration = 12)
    })
  }
  restore_model <- function(bundle, origin) {
    bundle <- validate_model_bundle(bundle)
    clear_results()
    state$restored_raw <- bundle$mapped$raw
    state$restored_settings <- bundle$input_settings
    state$restored_settings$family <- bundle$config$family
    state$restored_settings$B <- bundle$config$B
    state$restored_settings$fit_seed <- bundle$config$seed
    state$restored_settings$se_method <- bundle$config$se_method
    state$restored_settings$mode <- if (bundle$mapped$k > 1) "multiple" else
      if (bundle$mapped$stratified) "stratified" else "single"
    state$restored_settings$event_count <- bundle$mapped$k
    state$mapped <- bundle$mapped
    state$config <- bundle$config
    state$assoc <- bundle$association
    state$marginal <- bundle$marginal
    state$prediction_marginal <- bundle$prediction_marginal
    state$prediction_marginal_settings <- bundle$prediction_marginal_settings
    state$model_origin <- origin
    state$model_status <- paste("Restored", nrow(bundle$mapped$data), "records; saved", bundle$saved_at)
    updateRadioButtons(session, "step", selected = "association")
    showNotification("Model restored. Continue with marginal analysis or prediction.", type = "message")
  }
  observeEvent(input$model_file, {
    bundle <- notify_run("Loading saved analysis", read_model_bundle(input$model_file$datapath))
    if (!is.null(bundle)) restore_model(bundle, "imported")
  }, ignoreInit = TRUE)
  observeEvent(input$restore_cached_model, {
    bundle <- notify_run("Restoring cached model", {
      result <- read_cached_model(directory = selected_cache_directory())
      if (is.null(result)) stop("No cached model is available yet. Fit or import a model first.")
      result
    })
    if (!is.null(bundle)) restore_model(bundle, "cached")
  })
  output$model_status <- renderText(state$model_status)
  notify_run <- function(message, code) {
    tryCatch(withProgress(message = message, value = 0.2, {
      value <- force(code)
      incProgress(0.8)
      value
    }), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 15)
      NULL
    })
  }
  mapped <- reactive({
    validate(need(!is.null(state$mapped), "No validated dataset. Complete Step 2."))
    state$mapped
  })
  assoc <- reactive({
    validate(need(!is.null(state$assoc), "No fitted model. Run Step 3."))
    state$assoc
  })
  marginal <- reactive({
    validate(need(!is.null(state$marginal), "No marginal results. Run Step 4."))
    state$marginal
  })
  choose_column <- function(nms, candidates, fallback) {
    hits <- candidates[candidates %in% nms]
    if (length(hits)) hits[1] else nms[min(fallback, length(nms))]
  }
  output$color_controls <- renderUI({
    schemas <- color_schemas(state$mapped)
    tagList(lapply(names(schemas), function(key) color_picker_ui(key, schemas[[key]])))
  })
  outputOptions(output, "color_controls", suspendWhenHidden = FALSE)
  for (key in names(color_schemas())) local({
    color_key <- key
    observeEvent(input[[paste0("reset_colors_", color_key)]], {
      spec <- color_schemas(state$mapped)[[color_key]]
      updateCheckboxInput(session, paste0("custom_", color_key), value = FALSE)
      session$sendCustomMessage("reset-plot-colors",
        list(key = color_key, colors = unname(spec$colors)))
    }, ignoreInit = TRUE)
  })
  plot_colors <- function(key) selected_plot_colors(input, key, color_schemas(state$mapped)[[key]])
  marginal_palette <- function(key) {
    colors <- plot_colors(key)
    if (is.null(colors)) list() else list(palette = colors)
  }
  controls_for <- function(step) switch(step,
    import = tagList(
      fileInput("file", parameter_label("file", "Dataset"), accept = c(".csv", ".tsv", ".txt", ".RData", ".rdata", ".rda", ".rds")),
      uiOutput("data_object_control"),
      conditionalPanel("!input.file || !/\\.(rdata|rda|rds)$/i.test(input.file.name)",
      selectInput("sep", parameter_label("sep", "Delimiter"), c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";")),
      selectInput("encoding", parameter_label("encoding", "Encoding"), c("UTF-8", "GB18030", "latin1"))),
      actionButton("to_mapping", "Map variables", icon = icon("arrow-right"), class = "btn-primary")),
    mapping = {
      dat <- tryCatch(raw_data(), error = function(e) NULL)
      if (is.null(dat)) return(div("No dataset imported."))
      tagList(
        radioButtons("mode", parameter_label("mode", "Analysis"), c("Single intermediate event" = "single",
          "Stratified single event" = "stratified", "Multiple intermediate events" = "multiple"),
          selected = setting("mode", "single")),
        radioButtons("record_format", parameter_label("record_format", "Data format"), c(
          "Observed times + 0/1 status" = "observed",
          "Event times; NA if unobserved" = "missing"), selected = setting("record_format", "observed")),
        conditionalPanel("input.mode == 'multiple'",
          numericInput("event_count", parameter_label("event_count", "Intermediate events"), setting("event_count", 3), min = 2, step = 1)),
        uiOutput("mapping_controls"),
        actionButton("validate_data", "Validate data", icon = icon("check"), class = "btn-primary"))
    },
    association = {
      x <- state$mapped
      tagList(
        selectInput("family", parameter_label("family", "Copula family"), c("frank", "clayton", "gumbel", "joe", "amh"),
          selected = setting("family", "frank")),
        if (!is.null(x) && x$k == 1) tagList(
          numericInput("B", parameter_label("B", "Resampling replicates"), setting("B", 10), min = 0, max = 2000, step = 10),
          selectInput("se_method", parameter_label("se_method", "SE method"), c("resampling", "bootstrap"),
            selected = setting("se_method", "resampling"))),
        numericInput("fit_seed", parameter_label("fit_seed", "Random seed"), setting("fit_seed", 12345), min = 1, step = 1),
        actionButton("run_fit", "Fit model", icon = icon("play"), class = "btn-primary"),
        if (!is.null(x) && x$k > 1) tagList(h4("Association plot"),
          numericInput("association_n", parameter_label("association_n", "Reconstruction sample size"), 200, min = 20, max = 5000),
          numericInput("association_seed", parameter_label("association_seed", "Reconstruction seed"), 12345, min = 1)))
    },
    marginal = {
      x <- state$mapped
      tagList(
        if (!is.null(x) && x$k == 1) tagList(
          selectInput("marg_method", parameter_label("marg_method", "Marginal estimator"), c("JFKC", "LRA", "FJC", "Ker"),
            selected = setting("marg_method", "JFKC")),
          checkboxInput("km", parameter_label("km", "Kaplan-Meier terminal margin"), setting("km", FALSE)),
          checkboxInput("conf", parameter_label("conf", "Confidence intervals"), setting("conf", TRUE))),
        if (!is.null(x) && x$k > 1) tagList(
          checkboxInput("separate", parameter_label("separate", "Separate intermediate curves"), FALSE),
          selectInput("margin_event", parameter_label("margin_event", "Individual marginal fit"), setNames(seq_len(x$k), x$names))),
        actionButton("run_marginal", "Compute marginals", icon = icon("play"), class = "btn-primary"))
    },
    prediction = {
      x <- state$mapped
      multi <- !is.null(x) && x$k > 1
      tagList(
        selectInput("pred_type", parameter_label("pred_type", "Prediction"), if (multi)
          c("Survival" = "surv", "Restricted mean survival time" = "rmst",
            "Quantile survival time" = "qst", "Residual mean lifetime" = "rrms",
            "Quantile residual lifetime" = "qrl") else
          c("Conditional survival" = "csurv", "Marginal survival" = "msurv",
            "Joint survival" = "jsurv", "Cross-hazard ratio" = "chr")),
        if (!is.null(x) && x$stratified) selectInput("pred_group", parameter_label("pred_group", "Stratum"),
          c("All strata" = "__all__", setNames(levels(x$data$stratum), levels(x$data$stratum)))),
        conditionalPanel("input.pred_type == 'csurv' || input.mode == 'multiple'",
          uiOutput("prediction_samples")),
        if (!multi && !is.null(x)) helpText(sprintf(
          "Observed terminal times: %.5g to %.5g. Use the same time unit for prediction.",
          min(x$data$D), max(x$data$D))),
        numericInput("horizon", parameter_label("horizon", "Prediction horizon"), if (is.null(x)) 10 else max(x$data$D), min = 0.001),
        numericInput("grid_size", parameter_label("grid_size", "Time grid points"), 50, min = 10, max = 200),
        if (multi) tagList(
          conditionalPanel("input.pred_type == 'qst' || input.pred_type == 'qrl'",
            numericInput("tau", parameter_label("tau", "Quantile level"), 0.5, min = 0.001, max = 0.999, step = 0.05)),
          numericInput("nsim", parameter_label("nsim", "Monte Carlo sample size"), 1000, min = 100, max = 10000, step = 100)),
        numericInput("pred_seed", parameter_label("pred_seed", "Random seed"), 12345, min = 1),
        actionButton("run_prediction", "Predict", icon = icon("play"), class = "btn-primary"))
    }
  )
  output$controls <- renderUI(tagList(lapply(unname(steps), function(step) {
    conditionalPanel(paste0("input.step == '", step, "'"),
      uiOutput(paste0("controls_", step)))
  })))
  for (step in unname(steps)) local({
    key <- step
    output[[paste0("controls_", key)]] <- renderUI(controls_for(key))
    outputOptions(output, paste0("controls_", key), suspendWhenHidden = FALSE)
  })
  sample_ids <- reactiveVal(1L)
  next_sample_id <- 1L
  register_sample_label <- function(id) {
    force(id)
    output[[paste0("scenario_label_", id)]] <- renderText({
      position <- match(id, sample_ids())
      if (!is.na(position)) paste("Sample", position) else ""
    })
  }
  register_sample_label(1L)
  output$prediction_samples <- renderUI({
    x <- mapped()
    isolate({ sample_ids(1L); next_sample_id <<- 1L })
    tagList(h4("Prediction samples"),
      helpText(if (x$k > 1) paste("Event order:", paste(x$names, collapse = ", ")) else
        paste("Event:", x$names)),
      helpText("Enter event times or NA. NA excludes an event from the conditions. Click Predict after editing samples."),
      div(id = "prediction_sample_rows",
        prediction_sample_ui(1L, default_prediction_condition(x), x)))
  })
  outputOptions(output, "prediction_samples", suspendWhenHidden = FALSE)
  observeEvent(input$add_prediction_sample, {
    req(state$mapped)
    after <- as.integer(input$add_prediction_sample)
    ids <- sample_ids()
    if (!after %in% ids) return()
    next_sample_id <<- next_sample_id + 1L
    id <- next_sample_id
    register_sample_label(id)
    insertUI(selector = paste0("#prediction_sample_", after), where = "afterEnd",
      ui = prediction_sample_ui(id, default_prediction_condition(state$mapped), state$mapped))
    sample_ids(append(ids, id, after = match(after, ids)))
  }, ignoreInit = TRUE)
  observeEvent(input$remove_prediction_sample, {
    id <- as.integer(input$remove_prediction_sample)
    ids <- sample_ids()
    if (!id %in% ids) return()
    if (length(ids) == 1L) {
      showNotification("Keep at least one prediction sample.", type = "message")
      return()
    }
    removeUI(selector = paste0("#prediction_sample_", id))
    sample_ids(ids[ids != id])
  }, ignoreInit = TRUE)
  output$data_object_control <- renderUI({
    req(input$file)
    tables <- uploaded_tables()
    if (length(tables) > 1L)
      selectInput("data_object", parameter_label("data_object", "Dataset object"), names(tables))
  })
  output$mapping_controls <- renderUI({
    nms <- names(raw_data())
    k <- if (identical(input$mode, "multiple")) {
      analysis_event_count(input$event_count)
    } else 1L
    tagList(
      selectInput("terminal_time", parameter_label("terminal_time", "Terminal observation time"), nms,
        selected = setting("terminal_time", choose_column(nms, c("D", "last_followup", "observed_death_time", "T2"), 2))),
      if (identical(input$record_format, "missing"))
        selectInput("death_time", parameter_label("death_time", "Death event time (NA if censored)"), nms,
          selected = setting("death_time", choose_column(nms, c("T_death", "death_time", "DTH"), 2)))
      else selectInput("terminal_status", parameter_label("terminal_status", "Terminal status (0 / 1)"), nms,
        selected = setting("terminal_status", choose_column(nms, c("eventD", "dD", "death_status", "event2"), 4))),
      lapply(seq_len(k), function(j) tagList(h4(paste("Intermediate event", j)),
        selectInput(paste0("time_", j), parameter_label(paste0("time_", j), "Observed time"), nms,
          selected = setting(paste0("time_", j), choose_column(nms, c(paste0("T", j), "observed_disease_time"), 1))),
        if (!identical(input$record_format, "missing"))
          selectInput(paste0("status_", j), parameter_label(paste0("status_", j), "Status (0 / 1)"), nms,
            selected = setting(paste0("status_", j), choose_column(nms, c(paste0("d", j), paste0("event", j), "disease_status"), 3))))),
      if (identical(input$mode, "stratified")) tagList(
        selectInput("group", parameter_label("group", "Stratum"), nms,
          selected = setting("group", choose_column(nms, c("charlson", "tr"), 1))),
        checkboxInput("group_binary", parameter_label("group_binary", "Group 0 versus nonzero (Charlson)"), setting("group_binary", FALSE)))
    )
  })
  observeEvent(input$to_mapping, updateRadioButtons(session, "step", selected = "mapping"))
  observeEvent(input$validate_data, {
    state$mapped <- NULL
    clear_results()
    result <- notify_run("Validating data", {
      k <- if (input$mode == "multiple") analysis_event_count(input$event_count) else 1L
      pairs <- lapply(seq_len(k), function(j) list(
        time = input[[paste0("time_", j)]], status = input[[paste0("status_", j)]]))
      group <- if (input$mode == "stratified") input$group else ""
      dat <- raw_data()
      if (nzchar(group) && isTRUE(input$group_binary)) {
        if (!is.numeric(dat[[group]])) stop("Binary grouping requires a numeric stratum column.")
        dat[[group]] <- ifelse(dat[[group]] == 0, 0, 1)
      }
      if (identical(input$record_format, "missing"))
        prepare_event_time_analysis(dat, vapply(pairs, `[[`, "", "time"),
          input$terminal_time, input$death_time, group)
      else prepare_analysis(dat, pairs, input$terminal_time, input$terminal_status, group)
    })
    if (!is.null(result)) {
      state$mapped <- result
      showNotification("Dataset validated.", type = "message")
    }
  })
  observeEvent(input$run_fit, {
    req(state$mapped)
    x <- state$mapped
    cfg <- list(family = input$family,
      B = if (x$k == 1) as.integer(input$B) else 0L, seed = input$fit_seed,
      se_method = if (x$k == 1) input$se_method else "resampling")
    pending_fit(list(mapped = x, config = cfg))
    showModal(modalDialog(title = "Fit model and cache settings",
      checkboxInput("fit_cache_enabled", "Save the fitted model to disk", cache_enabled()),
      cache_directory_control("fit_cache_directory", "choose_fit_cache_directory", selected_cache_directory()),
      helpText("The cache contains the fitted model, dataset and variable mapping. Prediction results and images are not cached. Uncheck to keep this fit in the current session only."),
      footer = tagList(modalButton("Cancel"),
        actionButton("confirm_fit", "Continue", class = "btn-primary")), easyClose = FALSE))
  })
  observeEvent(input$confirm_fit, {
    pending <- pending_fit()
    req(pending, state$mapped)
    if (!identical(pending$mapped, state$mapped)) {
      removeModal()
      pending_fit(NULL)
      showNotification("The dataset changed. Click Fit model again.", type = "warning")
      return()
    }
    directory <- if (is.null(input$fit_cache_directory)) selected_cache_directory() else
      trimws(input$fit_cache_directory)
    enabled <- isTRUE(input$fit_cache_enabled)
    if (enabled) {
      directory <- tryCatch(validate_cache_directory(directory), error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 15)
        NULL
      })
      if (is.null(directory)) return()
    } else if (!nzchar(directory)) directory <- selected_cache_directory()
    updateTextInput(session, "cache_directory", value = directory)
    cache_enabled(enabled)
    removeModal()
    pending_fit(NULL)
    clear_results()
    x <- pending$mapped
    cfg <- pending$config
    cached <- if (!identical(input$reuse_model, FALSE)) tryCatch(
      read_cached_model(model_cache_key(x, cfg), directory = directory), error = function(e) {
        showNotification(paste("Cached model could not be read; fitting again:", conditionMessage(e)),
          type = "warning", duration = 12)
        NULL
      }) else NULL
    result <- if (!is.null(cached)) cached$association else
      notify_run("Fitting association model", analysis_association(
        x, cfg$family, cfg$B, cfg$seed, cfg$se_method))
    if (!is.null(result)) {
      state$assoc <- result
      state$config <- cfg
      state$model_origin <- if (is.null(cached)) "fitted" else "cached"
      if (!is.null(cached)) {
        state$marginal <- cached$marginal
        state$prediction_marginal <- cached$prediction_marginal
        state$prediction_marginal_settings <- cached$prediction_marginal_settings
        showNotification("Matching cached model loaded; no refitting needed.", type = "message")
      }
      cache_current_model(directory)
    }
  })
  observeEvent(input$run_marginal, {
    req(state$assoc, state$mapped)
    state$marginal <- state$prediction <- NULL
    x <- state$mapped
    cfg <- state$config
    result <- notify_run("Computing marginal survival", {
      if (x$k == 1 && isTRUE(input$conf) && cfg$B < 2)
        stop("Refit Step 3 with at least two resampling replicates for confidence intervals.")
      analysis_marginal(state$assoc, x, if (x$k == 1) input$marg_method else "JFKC",
        isTRUE(input$km), isTRUE(input$conf), cfg$B, cfg$se_method, cfg$seed)
    })
    if (!is.null(result)) {
      state$marginal <- result
    }
  })
  observeEvent(input$run_prediction, {
    req(state$mapped)
    state$prediction <- NULL
    x <- state$mapped
    if (is.null(state$assoc)) {
      showNotification("Fit or import an association model first (Step 3).", type = "error")
      return()
    }
    fit <- if (x$k > 1) state$assoc else state$marginal
    if (x$k == 1 && is.null(fit)) {
      margin_settings <- list(method = if (is.null(input$marg_method)) "JFKC" else input$marg_method,
        km = isTRUE(input$km))
      if (is.null(state$prediction_marginal) ||
          !identical(state$prediction_marginal_settings, margin_settings)) {
        fit <- notify_run("Preparing marginal estimates for prediction", analysis_marginal(
          state$assoc, x, method = margin_settings$method, km = margin_settings$km,
          conf = FALSE, B = 0L, seed = state$config$seed))
        if (is.null(fit)) return()
        state$prediction_marginal <- fit
        state$prediction_marginal_settings <- margin_settings
      } else fit <- state$prediction_marginal
    }
    times <- seq(0, input$horizon, length.out = as.integer(input$grid_size))
    result <- notify_run("Computing prediction", {
      if (x$k > 1 || input$pred_type == "csurv") {
        ids <- sample_ids()
        texts <- vapply(ids, function(id) {
          value <- input[[paste0("scenario_condition_", id)]]
          if (is.null(value)) default_prediction_condition(x) else value
        }, "")
        predict_scenario_set(fit, x, input$pred_type, texts, paste("Sample", seq_along(ids)), times,
          tau = if (is.null(input$tau)) .5 else input$tau,
          nsim = if (is.null(input$nsim)) 1000L else input$nsim,
          group = input$pred_group, seed = input$pred_seed)
      } else analysis_predict(fit, x, input$pred_type, 1L, times, .5, 1000L,
        input$pred_group, event_time = 0, seed = input$pred_seed)
    })
    if (!is.null(result)) {
      state$prediction <- result
      state$prediction_times <- times
      state$prediction_type <- input$pred_type
    }
  })
  output$fit_status <- renderText({
    if (!is.null(state$assoc)) paste0("Model (", state$model_origin, "): ",
      state$config$family, " | ", nrow(state$mapped$data), " records")
    else if (!is.null(state$mapped)) paste(nrow(state$mapped$data), "validated records")
    else "No fitted model"
  })
  output$result_view <- renderUI(switch(input$step,
    import = tagList(h2("Import data"), div(class = "result-body",
      uiOutput("data_metrics"), div(class = "table-wrap", tableOutput("raw_preview")))),
    mapping = tagList(h2("Variable mapping"), div(class = "result-body",
      tableOutput("validation_summary"), tableOutput("event_summary"))),
    association = tagList(h2("Association analysis"), div(class = "result-body",
      tabsetPanel(tabPanel("Estimates", tableOutput("estimates")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Observed data", plotOutput("observed_plot", height = "570px")),
        tabPanel("Association structure", plotOutput("association_plot", height = "620px"),
          div(class = "notice", "The association plot is reconstructed from the fitted model. A progress indicator appears while it is being prepared."))),
      div(class = "download-row", downloadButton("download_estimates", "Estimates CSV"),
        downloadButton("download_fit", "Analysis model RDS")))),
    marginal = tagList(h2("Marginal survival"), div(class = "result-body",
      tabsetPanel(tabPanel("Survival curves", plotOutput("marginal_plot", height = "620px")),
        tabPanel("Individual fit", plotOutput("individual_plot", height = "620px")),
        tabPanel("Summary", verbatimTextOutput("marginal_summary"))))),
    prediction = tagList(h2("Survival prediction"), div(class = "result-body",
      tabsetPanel(tabPanel("Results", div(class = "notice", textOutput("prediction_table_note")),
          tableOutput("prediction_table")),
        tabPanel("Plot", plotOutput("prediction_plot", height = "600px")),
        tabPanel("Conditions", tableOutput("prediction_record"))),
      downloadButton("download_prediction", "Prediction CSV")))
  ))
  output$data_metrics <- renderUI({
    if (is.null(input$file) && is.null(state$restored_raw)) return(div(class = "empty", "No dataset imported"))
    dat <- raw_data()
    div(class = "metrics",
      div(class = "metric", tags$b(nrow(dat)), span("Records")),
      div(class = "metric", tags$b(ncol(dat)), span("Variables")),
      div(class = "metric", tags$b(sum(is.na(dat))), span("Missing values")))
  })
  output$raw_preview <- renderTable({ req(!is.null(input$file) || !is.null(state$restored_raw)); head(display_analysis_table(raw_data()), 15) },
    striped = TRUE, rownames = TRUE)
  output$validation_summary <- renderTable({
    x <- mapped()
    data.frame(Status = "Validated", Records = nrow(x$data),
      Intermediate_events = x$k, Strata = if (x$stratified) nlevels(x$data$stratum) else 1)
  })
  output$event_summary <- renderTable(event_overview(mapped()), digits = 3)
  output$summary <- renderPrint(print(summary(assoc())))
  output$estimates <- renderTable(association_estimates(assoc()), rownames = TRUE, digits = 4)
  output$observed_plot <- renderPlot({
    fit <- assoc()
    colors <- plot_colors("observed")
    if (mapped()$k > 1) draw_structure_diagram(fit, colors)
    else if (is.null(colors)) plot(fit, type = "data")
    else plot(fit, type = "data", colEvent = colors[1], colImput = colors[2])
  }, res = 100)
  wedge_values <- reactive({
    fit <- assoc()
    req(mapped()$k > 1)
    withProgress(message = "Preparing association structure", value = .2, {
      values <- reconstruct_wedge_values(fit,
        n = if (is.null(input$association_n)) 200 else input$association_n,
        seed = if (is.null(input$association_seed)) 12345 else input$association_seed)
      incProgress(.8)
      values
    })
  })
  # Reconstruct on its own device before a Shiny plot starts recording graphics.
  observe({
    req(state$assoc, state$mapped, state$mapped$k > 1)
    tryCatch(wedge_values(), error = function(e)
      showNotification(paste("Association reconstruction failed:", conditionMessage(e)),
        type = "error", duration = 15))
  }, priority = 100)
  output$association_plot <- renderPlot({
    fit <- assoc()
    colors <- plot_colors("association")
    if (mapped()$k == 1) {
      if (is.null(colors)) plot(fit, type = "scrassonp")
      else plot(fit, type = "scrassonp", col = colors[1])
    } else draw_association_wedge(wedge_values(), colors)
  }, res = 100, execOnResize = TRUE)
  output$marginal_plot <- renderPlot({
    fit <- marginal()
    args <- if (mapped()$k > 1)
      list(x = fit, type = "msurv", nt.seperate = isTRUE(input$separate)) else
      list(x = fit, conf.int = isTRUE(input$conf))
    print(do.call(plot, c(args, marginal_palette("marginal"))))
  }, res = 100)
  output$individual_plot <- renderPlot({
    fit <- marginal()
    validate(need(mapped()$k > 1, "The single-event fit is displayed in Survival curves."))
    print(do.call(plot, c(list(x = marginal_fit(fit,
      event = if (is.null(input$margin_event)) 1L else as.integer(input$margin_event)),
      conf.int = FALSE), marginal_palette("individual"))))
  }, res = 100)
  output$marginal_summary <- renderPrint(print(summary(marginal())))
  prediction_table <- reactive({
    validate(need(!is.null(state$prediction), "No prediction results. Run Predict."))
    if (inherits(state$prediction, "workspace_scenario_predictions")) return(state$prediction$table)
    prediction_results(state$prediction, mapped(), state$prediction_type,
      state$prediction_times)
  })
  output$prediction_table <- renderTable({
    table <- prediction_table()
    if (all(c("Sample", "Stratum", "Time") %in% names(table))) {
      keys <- paste(table$Sample, table$Stratum, sep = "\r")
      return(do.call(rbind, lapply(unique(keys), function(key)
        head(table[keys == key, , drop = FALSE], 10))))
    }
    head(table, 100)
  }, digits = 5)
  output$prediction_table_note <- renderText({
    table <- prediction_table()
    if (all(c("Sample", "Time") %in% names(table)))
      "Preview: first 10 time points per sample and stratum. Prediction CSV includes all time points."
  })
  output$prediction_record <- renderTable({
    req(state$prediction)
    if (inherits(state$prediction, "workspace_scenario_predictions"))
      state$prediction$conditions
    else data.frame(Information = "This prediction does not use sample event conditions.")
  }, na = "NA")
  output$prediction_plot <- renderPlot({
    req(state$prediction)
    if (inherits(state$prediction, "workspace_scenario_predictions")) {
      validate(need(length(state$prediction$curves) > 0,
        "This prediction is reported as a numerical result in Results."))
      draw_scenario_predictions(state$prediction, plot_colors("prediction"))
      return(invisible())
    }
    req(state$prediction)
    x <- mapped()
    type <- state$prediction_type
    times <- state$prediction_times
    draw_single_prediction(state$prediction, x, type, times,
      colors = plot_colors("prediction"), surface_colors = plot_colors("surface"),
      contour_color = plot_colors("contour"))
  }, res = 100)
  output$download_estimates <- downloadHandler(filename = function() "association_estimates.csv",
    content = function(file) write.csv(association_estimates(assoc()), file, row.names = TRUE))
  output$download_fit <- downloadHandler(filename = function() "CopulaSCR_fit.rds",
    content = function(file) { assoc(); saveRDS(current_model_bundle(), file) })
  output$download_prediction <- downloadHandler(filename = function() "predictions.csv",
    content = function(file) write.csv(prediction_table(), file, row.names = FALSE))
}

shinyApp(ui, server)
