library(CopulaSCR)

analysis_event_count <- function(value) {
  if (is.null(value)) return(3L)
  if (length(value) != 1L || !is.numeric(value) || !is.finite(value) ||
      value < 2 || value != floor(value) || value > .Machine$integer.max)
    stop("The number of intermediate events must be a whole number of at least 2.")
  as.integer(value)
}

read_analysis_csv <- function(path, sep = ",", encoding = "UTF-8") {
  columns <- names(read.table(path, header = TRUE, nrows = 0, sep = sep,
    fileEncoding = encoding, check.names = FALSE, comment.char = "", quote = "\""))
  classes <- rep(NA_character_, length(columns))
  classes[grepl("(^id$|_id$)", tolower(columns))] <- "character"
  dat <- read.table(
    path, header = TRUE, sep = sep, fileEncoding = encoding,
    check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = c("", "NA", "N/A"), comment.char = "", quote = "\"",
    row.names = NULL, colClasses = classes
  )
  validate_analysis_table(dat)
}

is_simulation_analysis_data <- function(value) {
  is.list(value) && !is.data.frame(value) &&
    (inherits(value, "mscrData") || all(c("T1", "T2", "event1", "event2") %in% names(value)))
}

simulation_analysis_table <- function(value) {
  required <- c("T1", "T2", "event1", "event2")
  if (!all(required %in% names(value)))
    stop("Simulation data must contain T1, T2, event1 and event2.")
  times <- value$T1
  statuses <- value$event1
  if (!is.matrix(times) || !is.numeric(times) || !nrow(times) || !ncol(times) ||
      !is.matrix(statuses) || !is.numeric(statuses) || !identical(dim(times), dim(statuses)))
    stop("Simulation T1 and event1 must be numeric matrices with the same nonempty dimensions.")
  n <- nrow(times)
  k <- ncol(times)
  if (!is.numeric(value$T2) || !is.null(dim(value$T2)) || length(value$T2) != n ||
      !is.numeric(value$event2) || !is.null(dim(value$event2)) || length(value$event2) != n)
    stop("Simulation T2 and event2 must be numeric vectors matching the rows of T1.")
  if (any(!is.finite(times)) || any(!is.finite(value$T2)) ||
      any(times < 0) || any(value$T2 < 0))
    stop("Simulation observation times must be finite and nonnegative.")
  if (any(!statuses %in% c(0, 1)) || any(!value$event2 %in% c(0, 1)))
    stop("Simulation event1 and event2 must use 0/1 status indicators.")
  if (any(times > value$T2))
    stop("Simulation intermediate observation times must not exceed T2.")
  # T1's second column may itself be named T2: use D for the terminal time.
  out <- data.frame(D = value$T2, dD = value$event2)
  for (j in seq_len(k)) {
    out[[paste0("T", j)]] <- times[, j]
    out[[paste0("d", j)]] <- statuses[, j]
  }
  if (!is.null(value$T2true)) {
    if (!is.numeric(value$T2true) || !is.null(dim(value$T2true)) || length(value$T2true) != n)
      stop("Simulation T2true must be a numeric vector matching the rows of T1.")
    out$T2true <- value$T2true
  }
  attr(out, "simulation_event_count") <- k
  out
}

validate_analysis_table <- function(dat) {
  if (is_simulation_analysis_data(dat)) dat <- simulation_analysis_table(dat)
  if (is.matrix(dat) && is.atomic(dat)) {
    if (is.null(colnames(dat))) stop("The matrix needs column names.")
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(dat))
    stop("Select a data frame, a matrix with column names, or simSCRmul simulation data.")
  if (!nrow(dat) || ncol(dat) < 4L)
    stop("The file needs data rows and at least four columns.")
  if (any(!nzchar(names(dat))) || anyDuplicated(names(dat)))
    stop("Column names must be nonempty and unique.")
  as.data.frame(dat)
}

display_analysis_table <- function(dat) {
  for (name in names(dat)) {
    if (inherits(dat[[name]], "integer64")) {
      if (!requireNamespace("bit64", quietly = TRUE))
        stop("Displaying integer64 identifiers requires the bit64 package.")
      dat[[name]] <- as.character(dat[[name]])
    }
  }
  dat
}

read_analysis_upload <- function(path, filename = path, sep = ",", encoding = "UTF-8") {
  extension <- tolower(tools::file_ext(filename))
  if (extension %in% c("rdata", "rda")) {
    # Keep uploaded workspace objects separate from the app and global environment.
    workspace <- new.env(parent = emptyenv())
    objects <- load(path, envir = workspace)
    tables <- objects[vapply(objects, function(name) {
      value <- get(name, envir = workspace, inherits = FALSE)
      is.data.frame(value) || (is.matrix(value) && is.atomic(value)) ||
        is_simulation_analysis_data(value)
    }, logical(1))]
    if (!length(tables))
      stop("No data frame, matrix or simSCRmul simulation data was found in this RData file.")
    return(setNames(lapply(tables, get, envir = workspace, inherits = FALSE), tables))
  }
  if (extension == "rds") return(list(Dataset = readRDS(path)))
  if (!extension %in% c("csv", "tsv", "txt"))
    stop("Choose a CSV, TSV, TXT, RData, RDA or RDS file.")
  list(Dataset = read_analysis_csv(path, sep, encoding))
}

prepare_analysis <- function(dat, pairs, terminal_time, terminal_status, group = "") {
  # Use data.frame column-selection semantics even for keyed data.table input.
  dat <- as.data.frame(dat)
  columns <- c(unlist(pairs, use.names = FALSE), terminal_time, terminal_status)
  if (!length(pairs) || any(!columns %in% names(dat)) || anyDuplicated(columns))
    stop("Select distinct time and status columns for every event.")
  if (nzchar(group) && (!group %in% names(dat) || group %in% columns))
    stop("The stratum column must be separate from the event columns.")
  for (column in columns) {
    if (!is.numeric(dat[[column]]) || any(!is.finite(dat[[column]])))
      stop(paste0("Column '", column, "' needs finite numeric values without missing data."))
  }
  status_cols <- c(vapply(pairs, `[[`, "", "status"), terminal_status)
  if (any(!as.matrix(dat[status_cols]) %in% c(0, 1)))
    stop("Status columns must use 0 for censoring and 1 for an observed event.")
  time_cols <- c(vapply(pairs, `[[`, "", "time"), terminal_time)
  if (any(as.matrix(dat[time_cols]) < 0)) stop("Times must be nonnegative.")
  if (nrow(dat) < 10L) stop("At least 10 complete records are required.")
  for (pair in pairs) {
    if (any(dat[[pair$time]] > dat[[terminal_time]]))
      stop(paste0("Observed time '", pair$time, "' exceeds the terminal observation time."))
  }
  if (any(colSums(dat[status_cols]) < 2L))
    stop("Each event needs at least two observed occurrences.")
  if (nzchar(group) && (anyNA(dat[[group]]) || any(!nzchar(as.character(dat[[group]])))))
    stop("Stratum values must not be missing.")
  canonical <- data.frame(D = dat[[terminal_time]], dD = dat[[terminal_status]])
  for (j in seq_along(pairs)) {
    canonical[[paste0("T", j)]] <- dat[[pairs[[j]]$time]]
    canonical[[paste0("d", j)]] <- dat[[pairs[[j]]$status]]
  }
  if (nzchar(group)) canonical$stratum <- factor(dat[[group]])
  list(raw = dat, data = canonical, k = length(pairs), stratified = nzchar(group),
    names = vapply(pairs, `[[`, "", "time"), terminal_name = terminal_time,
    mapping = list(record_format = "observed", pairs = pairs,
      terminal_time = terminal_time, terminal_status = terminal_status, group = group))
}

event_overview <- function(x) {
  times <- c(paste0("T", seq_len(x$k)), "D")
  statuses <- c(paste0("d", seq_len(x$k)), "dD")
  data.frame(
    Event = c(x$names, x$terminal_name), Records = nrow(x$data),
    Observed = vapply(statuses, function(n) sum(x$data[[n]]), 0),
    Censored = vapply(statuses, function(n) sum(x$data[[n]] == 0), 0),
    Median_time = vapply(times, function(n) median(x$data[[n]]), 0)
  )
}

prepare_event_time_analysis <- function(dat, event_columns, followup, death_time, group = "") {
  dat <- as.data.frame(dat)
  required <- c(event_columns, followup, death_time)
  if (any(!required %in% names(dat)) || anyDuplicated(required))
    stop("Select distinct intermediate event, death and last-follow-up columns.")
  for (nm in required) {
    if (!is.numeric(dat[[nm]]))
      stop(paste0("Column '", nm, "' must be numeric."))
  }
  death <- dat[[death_time]]
  if (any(!is.na(death) & (!is.finite(death) | death < 0)))
    stop("Observed death times must be finite and nonnegative.")
  if (any(death > dat[[followup]], na.rm = TRUE))
    stop("Death times must not exceed the last follow-up time.")
  work <- dat
  generated <- make.unique(c(names(dat), paste0(".__event_status_", seq_along(event_columns)),
    ".__death_status"))
  generated <- tail(generated, length(event_columns) + 1L)
  pairs <- vector("list", length(event_columns))
  for (j in seq_along(event_columns)) {
    name <- event_columns[j]
    work[[generated[j]]] <- as.numeric(!is.na(dat[[name]]))
    work[[name]] <- ifelse(is.na(dat[[name]]), dat[[followup]], dat[[name]])
    pairs[[j]] <- list(time = name, status = generated[j])
  }
  work[[tail(generated, 1)]] <- as.numeric(!is.na(dat[[death_time]]))
  x <- prepare_analysis(work, pairs, followup, tail(generated, 1), group)
  x$raw <- dat
  x$death_time <- dat[[death_time]]
  x$mapping <- list(record_format = "missing", pairs = lapply(event_columns,
    function(column) list(time = column)), terminal_time = followup,
    death_time = death_time, group = group)
  x
}

analysis_association <- function(x, family, B = 10L, seed = 12345L,
                                 se_method = "resampling") {
  set.seed(seed)
  if (x$k > 1L) {
    times <- as.matrix(x$data[paste0("T", seq_len(x$k))])
    statuses <- as.matrix(x$data[paste0("d", seq_len(x$k))])
    colnames(times) <- colnames(statuses) <- x$names
    return(mscr(mT1 = times, T2 = x$data$D, mevent1 = statuses,
      event2 = x$data$dD, copulafam = family, ncore = 1))
  }
  rhs <- if (x$stratified) "stratum" else "1"
  scrassonp(
    t1.formula = as.formula(paste("survival::Surv(T1, d1) ~", rhs)),
    t2.formula = as.formula(paste("survival::Surv(D, dD) ~", rhs)),
    data = x$data, copulafam = family,
    B = B, seed = seed, se = B > 0, se.method = se_method
  )
}

analysis_marginal <- function(assoc, x, method = "JFKC", km = FALSE,
                              conf = FALSE, B = 10L, se_method = "resampling",
                              seed = 12345L) {
  if (x$k > 1L) return(assoc)
  scrsurv(fit = assoc, method = method, surv2km = km, conf.int = conf,
    conftype = if (B > 0 && conf) 3 else 1,
    B = B, se.method = se_method, seed = seed)
}

flatten_result <- function(value, prefix = "value") {
  if (is.data.frame(value) || is.matrix(value)) return(as.data.frame(value))
  if (is.list(value)) {
    nms <- names(value)
    if (is.null(nms)) nms <- as.character(seq_along(value))
    return(do.call(rbind, lapply(seq_along(value), function(j) {
      z <- flatten_result(value[[j]], paste(prefix, nms[j], sep = "."))
      data.frame(Component = paste(prefix, nms[j], sep = "."),
        Index = seq_len(nrow(z)),
        Value = apply(z, 1, function(row) paste(row, collapse = ", ")),
        stringsAsFactors = FALSE)
    })))
  }
  data.frame(Index = seq_along(value), Value = as.numeric(value))
}

default_conditional_times <- function(x = NULL) {
  if (is.null(x)) return("0.2, 0.5, 1")
  limit <- min(max(x$data$T1), max(x$data$D))
  observed <- x$data$T1[x$data$d1 == 1 & x$data$T1 < limit]
  observed <- observed[is.finite(observed) & observed >= 0]
  values <- if (length(observed)) as.numeric(stats::quantile(observed,
    c(.25, .5, .75), type = 1, names = FALSE)) else limit * c(.1, .25, .5)
  values <- unique(pmax(0, pmin(signif(values, 5), limit * (1 - 1e-6))))
  paste(format(values, trim = TRUE, scientific = FALSE), collapse = ", ")
}

validate_conditional_times <- function(x, times, event_time) {
  limit <- min(max(x$data$T1), max(x$data$D))
  if (!length(event_time) || any(!is.finite(event_time)) || any(event_time < 0) ||
      any(event_time >= limit))
    stop(sprintf(paste0("Conditional event times must be nonnegative and below %.6g ",
      "(the observed time limit). Use the dataset's time unit. Suggested times: %s."),
      limit, default_conditional_times(x)))
  if (any(event_time >= max(times)))
    stop("Conditional event times must be below the prediction horizon.")
  in_range <- vapply(event_time, function(value)
    sum(times >= value & times <= max(x$data$D)), integer(1))
  if (any(in_range < 2L))
    stop(sprintf(paste0("The prediction grid has fewer than two evaluation times between a ",
      "conditional event time and the last terminal observation (%.6g). ",
      "Reduce the prediction horizon or increase Time grid points, using the dataset's time unit."),
      max(x$data$D)))
}

analysis_predict <- function(fit, x, type, row, times, tau, nsim, group = NULL,
                             event_time = 0, seed = 12345L) {
  set.seed(seed)
  if (x$k > 1L) {
    obs <- as.matrix(x$data[row, paste0("T", seq_len(x$k)), drop = FALSE])
    colnames(obs) <- x$names
    status <- as.matrix(x$data[row, paste0("d", seq_len(x$k)), drop = FALSE])
    obs[status == 0] <- NA
    landmark <- max(c(0, obs), na.rm = TRUE)
    if (max(times) <= landmark)
      stop("The prediction horizon must exceed the latest observed intermediate event.")
    return(predict(fit, t1obs = obs, times = times, type = type,
      tu = max(times), tau = tau, nsim = nsim))
  }
  args <- list(object = fit, type = type, t1 = times, t2 = times)
  if (type == "csurv") {
    validate_conditional_times(x, times, event_time)
    args$t1 <- event_time
    args$s2 <- event_time
    args$t1equal <- TRUE
  }
  if (x$stratified) {
    groups <- if (identical(group, "__all__")) levels(x$data$stratum) else group
    args$newdata <- data.frame(stratum = factor(groups, levels = levels(x$data$stratum)))
  }
  result <- do.call(predict, args)
  if (type == "csurv") {
    values <- predicted_values(result)
    groups <- if (x$stratified) single_prediction_groups(values, type) else list(Overall = values)
    invalid <- unlist(lapply(names(groups), function(group_name) {
      curves <- groups[[group_name]]
      bad <- vapply(seq_along(curves), function(j)
        sum(is.finite(as.matrix(curves[[j]])[, j])) < 2L, logical(1))
      if (!any(bad)) return(character(0))
      paste(group_name, names(curves)[bad], sep = ": ")
    }), use.names = FALSE)
    if (length(invalid))
      stop(paste0("No drawable conditional survival curve for ", paste(invalid, collapse = ", "),
        ". Choose earlier conditional times within the observed data and a finer time grid."))
  }
  result
}

prediction_results <- function(prediction, x, type, times) {
  values <- predicted_values(prediction)
  if (x$k > 1L) {
    if (type == "surv") {
      record <- prediction$surv[[1]]
      return(data.frame(Time = record$times, Survival = record$surv))
    }
    return(data.frame(Quantity = type, Estimate = as.numeric(unlist(values))))
  }
  if (x$stratified) {
    grouped <- single_prediction_groups(values, type)
    return(do.call(rbind, lapply(names(grouped), function(group) {
      table <- single_prediction_table(grouped[[group]], type, times)
      data.frame(Stratum = group, table, check.names = FALSE)
    })))
  }
  single_prediction_table(values, type, times)
}

single_prediction_table <- function(values, type, times) {
  if (type %in% c("jsurv", "chr")) {
    out <- expand.grid(Intermediate_time = times, Terminal_time = times)
    out$Estimate <- as.vector(do.call(rbind, values))
    return(out)
  }
  if (type == "csurv") {
    return(do.call(rbind, lapply(seq_along(values), function(j) {
      curve <- as.matrix(values[[j]])
      # RealDataExample plots the diagonal pairing of t1 and s2 scenarios.
      data.frame(Scenario = names(values)[j], Time = times, Survival = curve[, j])
    })))
  }
  curves <- values
  out <- data.frame(Time = times)
  for (j in seq_along(curves)) {
    nm <- if (!is.null(names(curves))) names(curves)[j] else paste0("Curve", j)
    out[[nm]] <- as.numeric(curves[[j]])
  }
  out
}

single_prediction_groups <- function(values, type) {
  if (type != "msurv") return(values)
  groups <- names(values$nonterminal)
  setNames(lapply(groups, function(group) {
    list(nonterminal = values$nonterminal[[group]], terminal = values$terminal[[group]])
  }), groups)
}

draw_single_prediction <- function(prediction, x, type, times, colors = NULL,
                                   surface_colors = NULL, contour_color = NULL) {
  if (inherits(prediction, "workspace_scenario_predictions"))
    return(draw_scenario_predictions(prediction, colors))
  values <- predicted_values(prediction)
  groups <- if (x$stratified) single_prediction_groups(values, type) else list(Overall = values)
  if (type %in% c("jsurv", "chr"))
    return(draw_prediction_surfaces(groups, type, times, surface_colors, contour_color))
  old <- par(no.readonly = TRUE)
  on.exit(par(old))
  par(mfrow = c(ceiling(length(groups) / 3), min(3, length(groups))))
  for (group in names(groups)) {
    value <- groups[[group]]
      ys <- if (type == "csurv") lapply(seq_along(value),
        function(j) as.matrix(value[[j]])[, j]) else value
      if (!any(is.finite(unlist(ys))))
        stop("No finite survival estimates to plot. Choose conditional times within the observed data range and run Predict again.")
      cols <- series_colors(colors, length(ys))
      matplot(times, do.call(cbind, ys), type = "s", lty = 1, lwd = 2,
        col = cols, ylim = c(0, 1), xlab = "Time",
        ylab = "Survival probability", main = group)
      legend("topright", names(value), col = cols,
        lty = 1, lwd = 2, bty = "n", cex = .8)
  }
}

analysis_personalized <- function(fit, x, seed = 12345L, nsim = 1000L) {
  obs <- as.matrix(x$data[paste0("T", seq_len(x$k))])
  indicators <- as.matrix(x$data[paste0("d", seq_len(x$k))])
  obs[indicators == 0] <- NA
  eligible <- x$data$dD == 1 & rowSums(obs != 0, na.rm = TRUE) > 0
  if (!any(eligible)) stop("No observed deaths with nonzero intermediate-event information.")
  obs <- obs[eligible, , drop = FALSE]
  colnames(obs) <- x$names
  set.seed(seed)
  survival <- predict(fit, t1obs = obs, type = "surv", nsim = nsim)
  prediction <- function(type, tau = .5) predicted_values(
    predict(fit, t1obs = obs, type = type, tau = tau, nsim = nsim))
  cmst <- as.numeric(unlist(prediction("rmst")))
  q025 <- as.numeric(unlist(prediction("qst", .025)))
  q50 <- as.numeric(unlist(prediction("qst", .5)))
  q975 <- as.numeric(unlist(prediction("qst", .975)))
  out <- data.frame(Record = which(eligible), obs, check.names = FALSE)
  out$DTH <- if (!is.null(x$death_time)) x$death_time[eligible] else x$data$D[eligible]
  out$CMST <- round(cmst)
  out$CQST0.025 <- round(q025)
  out$CQST0.5 <- round(q50)
  out$CQST0.975 <- round(q975)
  attr(out, "survival_prediction") <- survival
  out
}
