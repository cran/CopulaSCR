#' Print an 'scrsurv' Object
#'
#' Pretty printing of objects created with the functionality of the `scrsurv'
#' library.
#'
#'
#' @aliases print.scrsurv
#' @param x Object of class \code{scrsurv}.
#' @param \dots Not used.
#' @seealso \code{\link{scrsurv}}, \code{\link{scrassonp}}, \code{\link{plot.scrsurv}}, \code{\link{print.scrassonp}}
#' @keywords survival
#' @return No return value.
#' @export
#'
print.scrsurv <- function(x,...) {
  cat("\n")
  cat("Call: ")
  print(x$Call)
  cat("\n")

  message(switch(x$t1.surv$covariate.type,"nonparametric estimator",
                 "Stratified nonparametric estimator",
                 "Stone-Beran-type estimator",
                 "Stratified Stone-Beran-type estimator")," for the ",
          ifelse(x$covariate.type==1," "," conditional "),
          "survival functions of nonterminal and terminal event times in the semi-competing risks data. ")

  cat("\n")
  ##   discrete.predictors <- extract.name.from.special(grep("strata.",names(x$X),value=TRUE),pattern="strata\\.")
  ##   continuous.predictors <- extract.name.from.special(grep("NN.",names(x$X),value=TRUE),pattern="NN\\.")
  discrete.predictors <- x$t1.surv$discrete.predictors
  continuous.predictors <- x$t1.surv$continuous.predictors

  message(#"Predictor space:\n\n",
    switch(x$t1.surv$covariate.type,
           "No covariates",{
             if (length(discrete.predictors)==1){
               c("Discrete predictor variable: ", discrete.predictors)
             }else{
               c("Discrete predictor variables:\n",
                 sapply(discrete.predictors,function(x)paste("\n - ",x)))
             }},
           c("Continuous predictors: ",continuous.predictors),
           c("  Discrete predictor variables: ",
             paste(discrete.predictors,collapse=", "),
             "\nContinuous predictor variables: ",
             continuous.predictors)))


  nonterminal<- c("n"=as.character(length(x$t1.surv$model.response[,2])),
                  "events"=as.character(sum(x$t1.surv$model.response[,2])),
                  "maxtime" = round(max(x$t1.surv$time),2))
  terminal<- c("n"= as.character(length(x$t2.surv$model.response[,2])),
                  "events"=as.character(sum(x$t2.surv$model.response[,2])),
                  "maxtime" = round(max(x$t2.surv$time),2))

  print(rbind(nonterminal,terminal),quote = FALSE)


  if (!is.null(x$na.action)){
    cat("\n",
        length(x$na.action),
        ifelse(length(x$na.action)==1,
               " observation",
               " observations")," deleted due to missing values.\n",sep="")
  }
  invisible(x)

}


#' Print an 'scrassonp' Object
#'
#' Pretty printing of objects created with the functionality of the `scrassonp'
#' library.
#'
#'
#' @aliases print.scrassonp
#' @param x Object of class \code{scrassonp}.
#' @param \dots Not used.
#' @seealso \code{\link{scrassonp}}, \code{\link{scrsurv}}
#' @keywords survival
#' @return No return value.
#' @export
#'
print.scrassonp <- function(x,...) {
  cat("\n")
  cat("Call: ")
  print(x$Call)
  cat("\n")

  if(x$t1data$covariate.type==1){
    cat("Association analysis of semi-competing risks data.\n")
  }
  if(x$t1data$covariate.type>1){
    cat("Stratified association analysis of semi-competing risks data.\n")
  }

  ##   discrete.predictors <- extract.name.from.special(grep("strata.",names(x$X),value=TRUE),pattern="strata\\.")
  ##   continuous.predictors <- extract.name.from.special(grep("NN.",names(x$X),value=TRUE),pattern="NN\\.")
  discrete.predictors <- x$t1data$discrete.predictors
  continuous.predictors <- x$t1data$continuous.predictors

  message(#"Predictor space:\n\n",
    switch(x$t1data$covariate.type,
           "",{
             if (length(discrete.predictors)==1){
               c("Discrete predictor variable: ", discrete.predictors)
             }else{
               c("Discrete predictor variables:\n",
                 sapply(discrete.predictors,function(x)paste("\n - ",x)))
             }},
           c("Continuous predictors: ",continuous.predictors),
           c("  Discrete predictor variables: ",
             paste(discrete.predictors,collapse=", "),
             "\nContinuous predictor variables: ",
             continuous.predictors)))

  cat("Kendall's tau in the", tools::toTitleCase(tolower(x$copulafam)),
      "copula structure:",
      round(x$tau,3),"\n")

  if(isTRUE(x$se)){
    cat("Standard error:", round(x$tau.se,3),"\n")
  }


  if (!is.null(x$t1data$na.action)){
    cat("\n",
        length(x$t1data$na.action),
        ifelse(length(x$t1data$na.action)==1,
               " observation",
               " observations")," deleted due to missing values.\n",sep="")
  }
  invisible(x)

}


#' Print an 'mscr' Object
#'
#' Pretty printing of objects created with the functionality of the `scrassonp'
#' library.
#'
#'
#' @aliases print.mscr
#' @param x Object of class \code{mscr}.
#' @param \dots Not used.
#' @seealso \code{\link{mscr}}, \code{\link{scrsurv}}
#' @keywords survival
#' @return No return value.
#' @export
#'
print.mscr <- function(x,...) {
  cat("\n")
  cat("Call: ")
  print(x$call)
  cat("\n")
  cat(length(x$mar.fits), "intermediate events, 1 terminal event.\n")
  cat("-------\n")
  message("Association analyses")
  cat("Kendall's tau in the", tools::toTitleCase(tolower(x$copulafam)),
      "copula structure\n")
  cat("tau.theta:\n")
  print(round(x$tau.theta,3))

  cat("tau.alpha:\n")
  cat(round(x$tau.alpha, 3), "\n")
  cat("-------\n")
  message("Marginal analyses (survival curves)")
  cat("intermediate event times: A list of", length(x$mar.fits), "scrsurv objects.\n")
  cat("terminal event time:\n")
  summary(x$S_D)


  invisible(x)

}

#' Print a 'predmscr' Object
#'
#' Prints a compact summary of predictions returned by \code{predict.mscr()}.
#'
#' @param x An object of class \code{"predmscr"}.
#' @param max.print Maximum number of scalar prediction values to print.
#' @param ... Additional arguments passed to \code{print()} when scalar
#'   prediction values are displayed.
#'
#' @return The object \code{x}, invisibly.
#' @seealso \code{\link{predict.mscr}}, \code{\link{predicted_values}}
#' @export
print.predmscr <- function(x, max.print = 6L, ...) {
  type <- attr(x, "type", exact = TRUE)
  
  cat("\nPredictions from an 'mscr' object\n")
  cat("Type:", if (is.null(type)) "unknown" else type, "\n")
  
  if (identical(type, "msurv")) {
    event.names <- names(x)
    
    if (is.null(event.names)) {
      event.names <- paste("Event", seq_along(x))
    }
    
    n.times <- vapply(
      x,
      function(z) {
        if (is.null(z$t1)) NA_integer_ else length(z$t1)
      },
      integer(1)
    )
    
    print(
      data.frame(
        event = event.names,
        evaluation.times = n.times,
        check.names = FALSE
      ),
      row.names = FALSE
    )
  } else {
    values <- predicted_values(x)
    
    if (is.atomic(values)) {
      n.values <- length(values)
      print(utils::head(values, max.print), ...)
      
      if (n.values > max.print) {
        cat("...", n.values - max.print,
            "additional value(s) omitted.\n")
      }
    } else {
      cat("Prediction sets:", length(values), "\n")
    }
  }
  
  cat("Use predicted_values() to extract the prediction values.\n")
  invisible(x)
}
