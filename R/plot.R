#' Plot of Marginal Survival Curves for Nonterminal and Terminal Event Times
#' @param x Object of class \code{scrsurv}.
#' @param linetype 	line types. Allowed values includes i) "strata" for changing
#'   linetypes by strata (i.e. groups); ii) a numeric vector (e.g., c(1, 2)) or
#'   a character vector c("solid", "dashed").
#' @param conf.int logical value. If TRUE, plots confidence interval.
#' @param censor 	logical value. If TRUE, censors will be drawn.
#' @param xlab axis labels
#' @param ylab axis labels
#' @param align Specifies whether graphs in the grid should be horizontally ("h")
#'    or vertically ("v") placed.
#' @param ... other arguments
## #' @import survminer
## #' @import cowplot
#' @seealso \code{\link{scrsurv}}
#' @return No return value.
#' @export
"plot.scrsurv"<- function (x,linetype = "strata",
                         conf.int = FALSE,
                         censor = FALSE,xlab = "Time",
                         ylab = "Survival probability",align="h",
                         ...)
{
  if(!is.null(x$stratas)){

    strata1<- rep(x$t1.surv[["X"]][[1]],times=x$t1.surv$size.strata)
    strata2<- rep(x$t2.surv[["X"]][[1]],times=x$t2.surv$size.strata)

    object<- x$t1.surv
    if(conf.int&!is.null(object$std.err)){

    survmat1<- data.frame(time=object$time,surv=object$surv,
                          n.event = object$n.event,
                          n.censor=object$n.censor,
                          std.err= object$std.err,
                          n.risk = object$n.risk,
                          upper=object$upper,
                          lower=object$lower,
                          strata=strata1)

    object<- x$t2.surv
    survmat2<- data.frame(time=object$time,surv=object$surv,
                          n.event = object$n.event,
                          n.censor=object$n.censor,
                          std.err= object$std.err,
                          n.risk = object$n.risk,
                          upper=object$upper,
                          lower=object$lower,
                          strata=strata2)
    }else{
      survmat1<- data.frame(time=object$time,surv=object$surv,
                            n.event = object$n.event,
                            n.censor=object$n.censor,
                            n.risk = object$n.risk,
                            strata=strata1)

      object<- x$t2.surv
      survmat2<- data.frame(time=object$time,surv=object$surv,
                            n.event = object$n.event,
                            n.censor=object$n.censor,
                            n.risk = object$n.risk,
                            strata=strata2)
      conf.int<- FALSE
    }

  }else{
    if(linetype == "strata")linetype = 1

    object<- x$t1.surv
    if(conf.int&!is.null(object$std.err)){

      survmat1<- data.frame(time=object$time,surv=object$surv,
                            n.event = object$n.event,
                            n.censor=object$n.censor,
                            std.err= object$std.err,
                            n.risk = object$n.risk,
                            upper=object$upper,
                            lower=object$lower)

      object<- x$t2.surv
      survmat2<- data.frame(time=object$time,surv=object$surv,
                            n.event = object$n.event,
                            n.censor=object$n.censor,
                            std.err= object$std.err,
                            n.risk = object$n.risk,
                            upper=object$upper,
                            lower=object$lower)
    }else{
      object<- x$t1.surv
      survmat1<- data.frame(time=object$time,surv=object$surv,
                            n.event = object$n.event,
                            n.censor=object$n.censor,
                            n.risk = object$n.risk)

      object<- x$t2.surv
      survmat2<- data.frame(time=object$time,surv=object$surv,
                            n.event = object$n.event,
                            n.censor=object$n.censor,
                            n.risk = object$n.risk)
      conf.int<- FALSE
    }



  }

  survfig<-list()
  survfig[[1]]<- survminer::ggsurvplot(survmat1,
                                       font.main = 15,font.legend=8,
                                       font.x =  10, font.y = 10,
                                       font.tickslab = 8,
                                       title="nonterminal event",
                                       linetype = linetype,conf.int = conf.int,
                                       censor = censor,xlab = xlab,ylab = ylab,
                                       ...)

  survfig[[2]]<- survminer::ggsurvplot(survmat2,
                                       font.main = 15,font.legend=8,
                                       font.x =  10, font.y = 10,
                                       font.tickslab = 8,
                                       title="terminal event",
                                       linetype = linetype,conf.int = conf.int,
                                       censor = censor,xlab = xlab,ylab = ylab,
                                       ...)
  # survfig[[2]]<- survfig[[2]]+ggplot2::ggtitle("terminal event")
  if(align=="h"){
    ncol<- 2
  }else{
    ncol<- 1
  }
  cowplot::plot_grid(plotlist=survfig,ncol=ncol)

}


#' Plot an 'mscr' Object
#' @param x Object of class \code{mscr}.
#' @param type Display the data structure if \code{type = "data"}, and
#'   display the marginal survival curve of each event time if
#'   \code{type = "msurv"}, or display the reconstructed association
#'   structure if \code{type = "association"}.
#' @param nt.seperate logical value. If FALSE, place all survival curves of
#'   intermediate event times in a single plot
#' @param n Number of complete event-time sets simulated for the association
#'   structure plot.
#' @param seed Optional random seed for the association structure plot.
#' @param col.lim Optional two-element color range. By default, the range
#'   expands the reference range \code{0.15--0.85} when needed.
#' @param linetype 	line types. Allowed values includes i) "strata" for changing
#'   linetypes by strata (i.e. groups); ii) a numeric vector (e.g., c(1, 2)) or
#'   a character vector c("solid", "dashed").
#' @param censor 	logical value. If TRUE, censors will be drawn.
#' @param xlab axis labels
#' @param ylab axis labels
#' @param fontsize Font size, in points, for text in the plot of the data structure.
#' @param height Height of the node in the plot of the data structure, in inches.
#' @param ... other arguments
#' @seealso \code{\link{mscr}}
#' @return No return value.
#' @export
"plot.mscr"<- function (x,type = c("msurv","data","association"),
                      nt.seperate = FALSE, linetype = "strata",
                      censor = FALSE, xlab = "Time",
                      ylab = "Survival probability",
                      fontsize=10,height=7,n = 200,seed = NULL,
                      col.lim = NULL,...)
{
  type<- match.arg(type)
  if(type=="data"){
    mscr_dataplot(tau.alpha=x$tau.alpha,tau.theta= x$tau.theta,
                  copulafam=x$copulafam,varnames = names(x$mar.fits),
                  fontsize=  fontsize,height=height)

  }


  if(type=="msurv"){
    mscr_msurvplot(objlist=x$mar.fits,nt.seperate=nt.seperate,
                   linetype = linetype,censor =censor,xlab = xlab,ylab = ylab,...)
  }

  if(type=="association"){
    out <- mscr_association_plot(
      object = x, n = n, seed = seed, col.lim = col.lim, ...
    )
    return(invisible(out))
  }

}

#' @noRd
#' @importFrom utils getFromNamespace
mscr_association_plot <- function(object, n = 200, seed = NULL,
                                  col.lim = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  K <- length(object$tau.theta)
  if (K < 2L) stop("At least two intermediate events are required.")

  inverse_survival <- function(surv_fit, u) {
    surv <- surv_fit$surv
    time <- surv_fit$time
    vapply(u, function(value) {
      hit <- which(surv <= value)
      if (length(hit)) time[hit[1]] else max(time)
    }, numeric(1))
  }

  alpha <- Calitau(object$copulafam, object$tau.alpha)
  theta <- vapply(
    object$tau.theta, Calitau, numeric(1),
    copulafam = object$copulafam
  )
  U <- copula::rCopula(
    n,
    copula::archmCopula(
      family = object$copulafam, param = alpha, dim = K
    )
  )
  death_fit <- object$mar.fits[[1]]$t2.surv
  death_u <- stats::runif(n)
  death <- inverse_survival(death_fit, death_u)
  p_death <- object$S_D(death)
  dev_copula <- getFromNamespace("dev_Copula", "CopulaSCR")
  intermediate <- matrix(NA_real_, nrow = n, ncol = K)

  for (k in seq_len(K)) {
    u <- vapply(seq_len(n), function(i) {
      stats::uniroot(function(xx) {
        dev_copula(
          copulafam = object$copulafam, param = theta[k],
          p1 = xx, p2 = p_death[i], mode = "2"
        ) - U[i, k]
      }, interval = c(0, 1))$root
    }, numeric(1))
    intermediate[, k] <- inverse_survival(
      object$mar.fits[[k]]$t1.surv, u
    )
  }

  colnames(intermediate) <- names(object$S_k)
  values <- stats::cor(
    data.frame(intermediate, D = death), method = "kendall"
  )
  mscr_wedge_plot(
    values, caption = "mscr reconstructed association",
    col.lim = col.lim, ...
  )
  invisible(values)
}

#' @noRd
mscr_wedge_plot <- function(values, caption, col.lim = NULL, ...) {
  colors <- grDevices::colorRampPalette(c(
    "#995300", "#D9B15D", "#F8E9BF",
    "#B9E2DE", "#53AFA7", "#006D5B"
  ))(6)
  pairs <- which(upper.tri(values), arr.ind = TRUE)
  numbers <- values[pairs]
  if (any(!is.finite(numbers))) stop("Association values must be finite.")
  if (is.null(col.lim)) {
    col.lim <- c(
      min(0.15, min(numbers)),
      max(0.85, max(numbers))
    )
  }
  if (length(col.lim) != 2L || col.lim[1] >= col.lim[2] ||
      any(numbers < col.lim[1] | numbers > col.lim[2])) {
    stop("col.lim must cover all off-diagonal association values.")
  }
  scaled <- (numbers - col.lim[1]) / diff(col.lim)
  color_index <- floor(pmin(pmax(scaled, 0), 1 - 1e-16) * 6) + 1L
  n <- ncol(values)

  graphics::plot.new()
  graphics::plot.window(
    xlim = c(1.1, 2 * n + 2.6), ylim = c(-1.1, n + 0.9),
    asp = 1, xaxs = "i", yaxs = "i"
  )
  for (p in seq_len(nrow(pairs))) {
    i <- pairs[p, 1]
    j <- pairs[p, 2]
    x <- i + j
    y <- j - i
    graphics::polygon(
      x + c(-1, 0, 1, 0), y + c(0, 1, 0, -1),
      col = "white", border = "grey70", lwd = 0.65
    )
    radius <- sqrt(scaled[p])
    graphics::polygon(
      x + radius * c(-1, 0, 1, 0),
      y + radius * c(0, 1, 0, -1),
      col = colors[color_index[p]], border = NA
    )
    graphics::text(x, y, sprintf("%.2f", numbers[p]), cex = 0.85)
  }

  labels <- lapply(seq_len(n), function(i) {
    if (i == n) expression(D) else bquote(tilde(T)[.(i)])
  })
  for (j in 2:n) {
    graphics::text(
      j + 0.1, j - 0.1, labels = labels[[j]],
      srt = 45, adj = c(1, 0.5), cex = 1.05
    )
  }
  for (i in seq_len(n - 1L)) {
    graphics::text(
      n + i + 0.9, n - i + 0.9, labels = labels[[i]],
      srt = -45, adj = c(0, 0.5), cex = 1.05
    )
  }
  graphics::text(n + 1, -0.7, caption, font = 2, cex = 1.15)
  breaks <- seq(0, n, length.out = length(colors) + 1L)
  for (i in seq_along(colors)) {
    graphics::rect(
      2 * n + 1.2, breaks[i], 2 * n + 1.45, breaks[i + 1L],
      col = colors[i], border = NA
    )
  }
  graphics::rect(
    2 * n + 1.2, 0, 2 * n + 1.45, n,
    border = "black", lwd = 0.7
  )
  tick_values <- seq(col.lim[1], col.lim[2],
                     length.out = length(breaks))
  graphics::text(
    2 * n + 1.6, breaks, sprintf("%.2f", tick_values),
    adj = c(0, 0.5), cex = 0.8
  )
  invisible(values)
}

#' @noRd
mscr_msurvplot<- function(objlist,nt.seperate=TRUE,linetype,censor,xlab,ylab,...){
  survfig<-list()

  if(isTRUE(nt.seperate)){
    if(linetype == "strata")linetype = 1
    for (k in 1:length(objlist)) {
      object<- objlist[[k]]$t1.surv
      survmat1<- data.frame(time=object$time,surv=object$surv,
                            n.event = object$n.event,
                            n.censor=object$n.censor,
                            n.risk = object$n.risk)
      survfig[[k]]<-
        survminer::ggsurvplot(survmat1,
                              font.main = 15,font.legend=8,
                              font.x =  10, font.y = 10,
                              font.tickslab = 8,
                              title=names(objlist)[k],
                              linetype = linetype,conf.int = FALSE,
                              censor = censor,xlab = xlab,ylab = ylab,
                              legend = "none",
                              ...)
    }

  }else{
    survmat1<- list()
    for (k in 1:length(objlist)) {
      object<- objlist[[k]]$t1.surv
      survmat1[[k]]<-
        data.frame(time=object$time,surv=object$surv,
                   n.event = object$n.event,
                   n.censor=object$n.censor,
                   n.risk = object$n.risk,
                   strata = rep(names(objlist)[k],length(object$time)))
    }

    survmat1<- do.call(rbind,survmat1)
    survfig[[1]]<-
      survminer::ggsurvplot(survmat1,
                            font.main = 15,font.legend=8,
                            font.x =  10, font.y = 10,
                            font.tickslab = 8,
                            title="nonterminal events",
                            linetype = linetype,conf.int = FALSE,
                            censor = censor,xlab = xlab,ylab = ylab,
                            legend.title = "nonterminal events",
                            ...)
    }

  object<- objlist[[1]]$t2.surv

  survmat2<- data.frame(time=object$time,surv=object$surv,
                        n.event = object$n.event,
                        n.censor=object$n.censor,
                        n.risk = object$n.risk)




  survfig[[length(survfig)+1]]<-
    survminer::ggsurvplot(survmat2,
                          font.main = 15,font.legend=8,
                          font.x =  10, font.y = 10,
                          font.tickslab = 8,
                          title="terminal event",
                          linetype = linetype,conf.int = FALSE,
                          censor = censor,xlab = xlab,ylab = ylab,
                          legend = "none",
                          ...)

  p <- cowplot::plot_grid(plotlist = survfig)
  print(p)
  invisible(p)
  
}



#' @noRd
mscr_dataplot<- function(tau.alpha,tau.theta,copulafam,varnames,
                         fontsize=10,height=7){
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)

  Nodes <- c("Entry",varnames,"DTH")
  Edges <- list("Entry"=list(edges=c(varnames,"DTH")))
  for (k in 1:length(varnames)) {
    Edges[[varnames[k]]]<- list(edges=c(varnames[-k],"DTH"))
    #weights= c(rep(tau.alpha,length(varnames)-1),tau.theta[k])
  }

  Edges[["DTH"]]<- list(edges=NULL)

  treeobj <- graph::graphNEL(nodes=Nodes, edgeL=Edges, edgemode="directed")

  nAttrs<-list()
  nAttrs$fontsize <- rep(fontsize, length(Nodes))
  nAttrs$fixedsize<- rep(FALSE, length(Nodes))
  nAttrs$height<- rep(height, length(Nodes))
  nAttrs$color<- c("gray70",rep("black",length(Nodes)-2),"red")


  names(nAttrs$fixedsize)<- names(nAttrs$fontsize)<-
    names(nAttrs$height)<- names(nAttrs$color)<- Nodes
  Rgraphviz::plot(treeobj,nodeAttrs=nAttrs,
               main = paste0("Under the ",
                             tools::toTitleCase(tolower(copulafam)),
                             " copula structure"))
  graphics::par(new = TRUE, bty = "n")
  plot(0:10,0:10,type="n",yaxt="n",xaxt="n")

  text(2,8,paste0("tau.theta= \n (",paste0(round(tau.theta,2),collapse = ","),")\n",
               "\ntau.alpha=\n",round(tau.alpha,2)),
        col="red")

}


#' Plot an 'scrassonp' Object
#'
#' @param x Object of class \code{scrassonp}.
#' @param type Plot the observed bivariate survival times if
#'   \code{type = "data"}, or display the fitted association structure if
#'   \code{type = "scrassonp"}.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param ... Other arguments passed to the plotting function.
#'
#' @seealso \code{\link{scrassonp}}
#' @return No return value.
#' @export
plot.scrassonp <- function(x,
                           type = c("data", "scrassonp"),
                           xlab = NULL,
                           ylab = NULL,
                           ...) {
  
  type <- match.arg(type)
  
  if (type == "data") {
    
    if (is.null(xlab)) xlab <- "T1"
    if (is.null(ylab)) ylab <- "T2"
    
    nstrata <- x$t1data$nstrata
    
    draw_data <- function(ind, strata.name = NULL) {
      
      plotdata <- data.frame(
        T1 = x$t1data$time[ind],
        T2 = x$t2data$time[ind],
        event1 = x$t1data$event[ind],
        event2 = x$t2data$event[ind]
      )
      
      obj <- SurvCorr::survcorr(
        formula1 = survival::Surv(T2, event2) ~ 1,
        formula2 = survival::Surv(T1, event1) ~ 1,
        data = plotdata
      )
      
      graphics::plot(
        obj,
        "times",
        xlab = xlab,
        ylab = ylab,
        main = strata.name,
        ...
      )
    }
    
    if (nstrata == 1) {
      
      draw_data(seq_along(x$t1data$time))
      
    } else {
      
      oldpar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldpar))
      
      nr <- ceiling(sqrt(nstrata))
      nc <- ceiling(nstrata / nr)
      graphics::par(mfrow = c(nr, nc))
      
      size.strata <- x$t1data$size.strata
      end.strata <- cumsum(size.strata)
      start.strata <- c(1, utils::head(end.strata, -1) + 1)
      
      strata.names <- as.character(x$stratas[[1]])
      
      for (k in seq_len(nstrata)) {
        
        ind <- start.strata[k]:end.strata[k]
        
        draw_data(
          ind = ind,
          strata.name = strata.names[k]
        )
      }
    }
  }
  
  if (type == "scrassonp") {
    
    if (is.null(xlab)) xlab <- "u"
    if (is.null(ylab)) ylab <- "v"
    
    u <- seq(0.001, 0.999, length.out = 100)
    v <- seq(0.001, 0.999, length.out = 100)
    
    params <- x$copulaparam
    taus <- x$tau
    
    draw_contour <- function(param, tau, strata.name = NULL,
                             derivative = FALSE) {
      
      uv <- expand.grid(u = u, v = v)
      
      if (!derivative) {
        
        z <- matrix(
          Copulafn(
            copulafam = x$copulafam,
            param = param,
            p1 = uv$u,
            p2 = uv$v
          ),
          nrow = length(u),
          ncol = length(v)
        )
        
        main <- paste0(
          toupper(x$copulafam),
          " copula"
        )
        
      } else {
        
        z <- matrix(
          dev_Copula(
            copulafam = x$copulafam,
            param = param,
            p1 = uv$u,
            p2 = uv$v,
            mode = "12"
          ),
          nrow = length(u),
          ncol = length(v)
        )
        
        main <- paste0(
          toupper(x$copulafam),
          " copula density"
        )
      }
      
      if (!is.null(strata.name)) {
        main <- paste0(main, " - ", strata.name)
      }
      
      main <- paste0(
        main,
        "\nKendall's tau = ",
        round(tau, 3)
      )
      
      graphics::contour(
        x = u,
        y = v,
        z = z,
        xlab = xlab,
        ylab = ylab,
        main = main,
        ...
      )
    }
    
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    
    if (length(params) == 1) {
      
      graphics::par(mfrow = c(1, 2))
      
      draw_contour(
        param = params,
        tau = taus,
        derivative = FALSE
      )
      
      draw_contour(
        param = params,
        tau = taus,
        derivative = TRUE
      )
      
    } else {
      
      graphics::par(mfrow = c(length(params), 2))
      
      strata.names <- as.character(x$stratas[[1]])
      
      for (k in seq_along(params)) {
        
        draw_contour(
          param = params[k],
          tau = taus[k],
          strata.name = strata.names[k],
          derivative = FALSE
        )
        
        draw_contour(
          param = params[k],
          tau = taus[k],
          strata.name = strata.names[k],
          derivative = TRUE
        )
      }
    }
  }
  invisible(NULL)
}
