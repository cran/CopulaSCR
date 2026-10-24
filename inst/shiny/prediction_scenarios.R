parse_prediction_condition <- function(text, x, sample = "Sample") {
  parts <- trimws(strsplit(paste0(text, ",__end__"), ",", fixed = TRUE)[[1]])
  parts <- head(parts, -1L)
  if (length(parts) != x$k || any(!nzchar(parts)))
    stop(sprintf("%s: enter exactly %d event time(s), separated by commas. Write NA for an event not used as a condition.", sample, x$k))
  missing <- toupper(parts) == "NA"
  values <- suppressWarnings(as.numeric(parts))
  if (any(!missing & (!is.finite(values) | values < 0)))
    stop(paste0(sample, ": use nonnegative numeric times or NA; other text, Inf and negative times are not valid."))
  values[missing] <- NA_real_
  setNames(values, x$names)
}

default_prediction_condition <- function(x) {
  if (x$k == 1L) return(trimws(strsplit(default_conditional_times(x), ",", fixed = TRUE)[[1]][1]))
  paste(rep("NA", x$k), collapse = ", ")
}

prediction_sample_ui <- function(id, value, x) {
  shiny::div(id = paste0("prediction_sample_", id), class = "prediction-sample",
    shiny::div(class = "prediction-condition",
      shiny::textInput(paste0("scenario_condition_", id),
        parameter_label("scenario_condition",
          shiny::textOutput(paste0("scenario_label_", id), inline = TRUE)), value = value,
        placeholder = if (x$k == 1) "0.2 or NA" else paste(rep("NA", x$k), collapse = ", "))),
    shiny::tags$button(type = "button", class = "btn btn-default sample-button",
      title = "Add a prediction sample", `aria-label` = "Add sample after this sample",
      onclick = sprintf("Shiny.setInputValue('add_prediction_sample',%d,{priority:'event'})", id), "+"),
    shiny::tags$button(type = "button", class = "btn btn-default sample-button",
      title = "Remove this prediction sample", `aria-label` = "Remove this sample",
      onclick = sprintf("Shiny.setInputValue('remove_prediction_sample',%d,{priority:'event'})", id), "\u2212"))
}

predict_scenario_set <- function(fit, x, type, texts, labels, times,
                                 tau = .5, nsim = 1000L, group = NULL, seed = 12345L) {
  conditions <- lapply(seq_along(texts), function(i) parse_prediction_condition(texts[i], x, labels[i]))
  landmarks <- vapply(conditions, function(z) max(c(0, z), na.rm = TRUE), 0)
  if (any(landmarks >= max(times)))
    stop("The prediction horizon must exceed the latest event time in every sample.")
  curves <- list(); rows <- list(); objects <- list()
  for (i in seq_along(conditions)) {
    obs <- conditions[[i]]
    condition <- paste(ifelse(is.na(obs), "NA", format(obs, trim = TRUE)), collapse = ", ")
    result <- tryCatch({
      set.seed(seed)
      if (x$k > 1L) {
        prediction <- predict(fit, t1obs = matrix(obs, nrow = 1L,
          dimnames = list(NULL, x$names)), times = times, type = type,
          tu = max(times), tau = tau, nsim = nsim)
        table <- prediction_results(prediction, x, type, times)
      } else if (!is.na(obs[1])) {
        prediction <- analysis_predict(fit, x, "csurv", 1L, times, .5, nsim,
          group, event_time = unname(obs[1]), seed = seed)
        table <- prediction_results(prediction, x, "csurv", times)
      } else {
        args <- list(object = fit, type = "msurv", t2 = times)
        if (x$stratified) {
          groups <- if (identical(group, "__all__")) levels(x$data$stratum) else group
          args$newdata <- data.frame(stratum = factor(groups, levels = levels(x$data$stratum)))
        }
        prediction <- do.call(predict, args)
        terminal <- prediction$msurv2
        if (!is.list(terminal)) terminal <- list(Overall = terminal)
        table <- do.call(rbind, lapply(names(terminal), function(name)
          data.frame(Stratum = name, Time = times, Survival = as.numeric(terminal[[name]]))))
      }
      list(prediction = prediction, table = table)
    }, error = function(e) stop(paste0(labels[i], ": ", conditionMessage(e)), call. = FALSE))
    table <- result$table
    if ("Scenario" %in% names(table)) table$Scenario <- NULL
    if (!"Stratum" %in% names(table)) table$Stratum <- "Overall"
    if ("Survival" %in% names(table)) {
      for (stratum in unique(table$Stratum)) {
        curve <- table[table$Stratum == stratum, , drop = FALSE]
        if (sum(is.finite(curve$Survival)) < 2L)
          stop(paste0(labels[i], " (", stratum, "): no drawable survival curve. Check event times and increase the time grid resolution."))
        curves[[length(curves) + 1L]] <- list(sample = labels[i], stratum = stratum,
          times = curve$Time, surv = curve$Survival)
      }
    }
    rows[[i]] <- cbind(data.frame(Sample = labels[i], Condition = condition,
      Landmark = landmarks[i]), table)
    objects[[i]] <- result$prediction
  }
  condition_table <- data.frame(Sample = labels, do.call(rbind, conditions),
    Landmark = landmarks, check.names = FALSE)
  structure(list(table = do.call(rbind, rows), conditions = condition_table,
    curves = curves, predictions = objects, type = type), class = "workspace_scenario_predictions")
}

draw_scenario_predictions <- function(result, colors = NULL) {
  if (!length(result$curves)) stop("This prediction is reported as a numerical result in Results.")
  strata <- unique(vapply(result$curves, `[[`, "", "stratum"))
  samples <- unique(vapply(result$curves, `[[`, "", "sample"))
  palette <- setNames(series_colors(colors, length(samples)), samples)
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old))
  graphics::par(mfrow = c(ceiling(length(strata) / 3), min(3, length(strata))))
  for (stratum in strata) {
    curves <- Filter(function(z) z$stratum == stratum, result$curves)
    end <- max(vapply(curves, function(z) max(z$times), 0))
    graphics::plot(c(0, end), c(0, 1), type = "n", xlab = "Time",
      ylab = "Survival probability", main = stratum)
    for (curve in curves) graphics::lines(curve$times, curve$surv,
      col = palette[[curve$sample]], lwd = 2, type = "s")
    graphics::legend("topright", vapply(curves, `[[`, "", "sample"),
      col = palette[vapply(curves, `[[`, "", "sample")], lty = 1, lwd = 2,
      bty = "n", cex = .85)
  }
}
