color_schemas <- function(x = NULL) {
  multi <- !is.null(x) && x$k > 1L
  curve_n <- if (is.null(x)) 1L else if (multi) x$k else
    if (x$stratified) nlevels(x$data$stratum) else 1L
  named <- function(colors, labels) setNames(colors, labels)
  list(
    observed = list(step = "association", title = if (multi) "Structure diagram" else "Observed data",
      colors = if (multi) named(c("#B3B3B3", "#000000", "#FF0000", "#FF0000", "#000000"),
        c("Entry", "Intermediate events", "Death", "Annotations", "Edges")) else
        named(c("#000000", "#BEBEBE"), c("Observed events", "Censored events"))),
    association = list(step = "association", title = if (multi) "Association wedge" else "Copula contours",
      colors = if (multi) named(c("#995300", "#D9B15D", "#F8E9BF", "#B9E2DE", "#53AFA7", "#006D5B"),
        paste("Low to high", 1:6)) else c(Contours = "#000000")),
    marginal = list(step = "marginal", title = "Marginal curves",
      colors = named(scales::hue_pal()(curve_n), paste("Curve", seq_len(curve_n)))),
    individual = list(step = "marginal", title = "Individual marginal fit",
      colors = c(Curve = "#F8766D")),
    prediction = list(step = "prediction", title = "Prediction curves",
      colors = named(grDevices::palette(), paste("Series", 1:8))),
    surface = list(step = "prediction", title = "Prediction surface",
      colors = named(grDevices::hcl.colors(6, "YlOrRd", rev = TRUE), paste("Low to high", 1:6))),
    contour = list(step = "prediction", title = "Prediction contour lines",
      colors = c(Contours = "#000000"))
  )
}

color_picker_ui <- function(key, spec) {
  shiny::conditionalPanel(paste0("input.step == '", spec$step, "'"),
    shiny::tags$details(class = "color-section",
      shiny::tags$summary(spec$title),
      shiny::checkboxInput(paste0("custom_", key), parameter_label("custom_colors", "Custom colors"), FALSE),
      shiny::conditionalPanel(paste0("input.custom_", key),
        shiny::div(class = "color-swatches",
          lapply(seq_along(spec$colors), function(j) {
            id <- paste0("color_", key, "_", j)
            shiny::tags$label(`for` = id, class = "color-swatch",
              shiny::tags$input(id = id, type = "color", value = spec$colors[j],
                `data-default-color` = spec$colors[j],
                oninput = "Shiny.setInputValue(this.id,this.value,{priority:'event'})"),
              shiny::tags$span(parameter_label("color", names(spec$colors)[j])))
          })),
        shiny::actionButton(paste0("reset_colors_", key), "Reset",
          icon = shiny::icon("rotate-left"), class = "btn-default btn-sm",
          title = "Restore default colors")))
  )
}

color_reset_script <- function() shiny::tags$script(shiny::HTML("
  $(function(){Shiny.addCustomMessageHandler('reset-plot-colors',function(message){
    message.colors.forEach(function(color,index){
      var id='color_'+message.key+'_'+(index+1),el=document.getElementById(id);
      if(el){el.value=color;Shiny.setInputValue(id,color);}
    });
  });});
"))

selected_plot_colors <- function(input, key, spec) {
  if (!isTRUE(input[[paste0("custom_", key)]])) return(NULL)
  vapply(seq_along(spec$colors), function(j) {
    value <- input[[paste0("color_", key, "_", j)]]
    if (is.null(value) || !grepl("^#[0-9a-fA-F]{6}$", value)) spec$colors[j] else value
  }, "")
}

series_colors <- function(colors, n) {
  rep(if (is.null(colors)) grDevices::palette() else colors, length.out = n)
}

draw_prediction_surfaces <- function(groups, type, times, colors = NULL, line_color = NULL) {
  panels <- lapply(names(groups), function(name) {
    z <- do.call(rbind, groups[[name]])
    levels <- pretty(range(z, finite = TRUE), n = 30)
    if (length(levels) < 2L) levels <- c(min(z) - .01, max(z) + .01)
    fills <- if (is.null(colors)) grDevices::hcl.colors(length(levels) - 1L, "YlOrRd", rev = TRUE) else
      grDevices::colorRampPalette(colors)(length(levels) - 1L)
    capture_color_panel(function() {
      graphics::filled.contour(times, times, z, levels = levels, col = fills,
        xlab = "t1", ylab = "t2",
        main = paste0(if (type == "chr") "cross-hazard ratio" else "joint survival function",
          " (", name, ")"),
        plot.axes = {
          graphics::axis(1)
          graphics::axis(2)
          graphics::contour(times, times, z, add = TRUE, lwd = 2,
            col = if (is.null(line_color)) "black" else line_color[1])
        })
    })
  })
  print(cowplot::plot_grid(plotlist = panels, ncol = min(length(panels), 2L)))
}

capture_color_panel <- function(draw) {
  path <- tempfile("surface-", fileext = ".png")
  device <- NULL
  on.exit({
    if (!is.null(device) && device %in% grDevices::dev.list()) grDevices::dev.off(device)
    unlink(path)
  })
  grDevices::png(path, width = 1100, height = 850, res = 120)
  device <- grDevices::dev.cur()
  # filled.contour owns its layout, so capture each panel on its own device.
  draw()
  grDevices::dev.off(device)
  device <- NULL
  grid::rasterGrob(png::readPNG(path), interpolate = FALSE)
}

draw_structure_diagram <- function(fit, colors = NULL) {
  if (is.null(colors)) return(plot(fit, type = "data"))
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old))
  variables <- names(fit$mar.fits)
  nodes <- c("Entry", variables, "DTH")
  edges <- list(Entry = list(edges = c(variables, "DTH")))
  for (name in variables) edges[[name]] <- list(edges = c(setdiff(variables, name), "DTH"))
  edges$DTH <- list(edges = NULL)
  graph <- graph::graphNEL(nodes = nodes, edgeL = edges, edgemode = "directed")
  attrs <- list(
    fontsize = setNames(rep(10, length(nodes)), nodes),
    fixedsize = setNames(rep(FALSE, length(nodes)), nodes),
    height = setNames(rep(7, length(nodes)), nodes),
    color = setNames(c(colors[1], rep(colors[2], length(variables)), colors[3]), nodes))
  Rgraphviz::plot(graph, nodeAttrs = attrs, attrs = list(edge = list(color = colors[5])),
    main = paste0("Under the ", tools::toTitleCase(fit$copulafam), " copula structure"))
  graphics::par(new = TRUE, bty = "n")
  graphics::plot(0:10, 0:10, type = "n", yaxt = "n", xaxt = "n")
  graphics::text(2, 8, paste0("tau.theta= \n (", paste(round(fit$tau.theta, 2), collapse = ","),
    ")\n\ntau.alpha=\n", round(fit$tau.alpha, 2)), col = colors[4])
}

reconstruct_wedge_values <- function(fit, n, seed) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  plot(fit, type = "association", n = n, seed = seed)
}

draw_association_wedge <- function(values, colors = NULL) {
  if (is.null(colors))
    return(utils::getFromNamespace("mscr_wedge_plot", "CopulaSCR")(
      values, caption = "mscr reconstructed association"))
  pairs <- which(upper.tri(values), arr.ind = TRUE)
  numbers <- values[pairs]
  limits <- c(min(.15, numbers), max(.85, numbers))
  scaled <- (numbers - limits[1]) / diff(limits)
  index <- floor(pmin(pmax(scaled, 0), 1 - 1e-16) * length(colors)) + 1L
  n <- ncol(values)
  graphics::plot.new()
  graphics::plot.window(xlim = c(1.1, 2 * n + 2.6), ylim = c(-1.1, n + .9),
    asp = 1, xaxs = "i", yaxs = "i")
  for (p in seq_len(nrow(pairs))) {
    i <- pairs[p, 1]; j <- pairs[p, 2]
    x <- i + j; y <- j - i
    graphics::polygon(x + c(-1, 0, 1, 0), y + c(0, 1, 0, -1),
      col = "white", border = "grey70", lwd = .65)
    radius <- sqrt(scaled[p])
    graphics::polygon(x + radius * c(-1, 0, 1, 0), y + radius * c(0, 1, 0, -1),
      col = colors[index[p]], border = NA)
    graphics::text(x, y, sprintf("%.2f", numbers[p]), cex = .85)
  }
  labels <- lapply(seq_len(n), function(i) if (i == n) expression(D) else bquote(tilde(T)[.(i)]))
  for (j in 2:n) graphics::text(j + .1, j - .1, labels[[j]], srt = 45, adj = c(1, .5), cex = 1.05)
  for (i in seq_len(n - 1L))
    graphics::text(n + i + .9, n - i + .9, labels[[i]], srt = -45, adj = c(0, .5), cex = 1.05)
  graphics::text(n + 1, -.7, "mscr reconstructed association", font = 2, cex = 1.15)
  breaks <- seq(0, n, length.out = length(colors) + 1L)
  for (i in seq_along(colors))
    graphics::rect(2 * n + 1.2, breaks[i], 2 * n + 1.45, breaks[i + 1L], col = colors[i], border = NA)
  graphics::rect(2 * n + 1.2, 0, 2 * n + 1.45, n, border = "black", lwd = .7)
  graphics::text(2 * n + 1.6, breaks, sprintf("%.2f", seq(limits[1], limits[2],
    length.out = length(breaks))), adj = c(0, .5), cex = .8)
  invisible(values)
}
