# Explanations shared by static and dynamically generated parameter labels.
parameter_help <- c(
  scenario_condition = "One prediction sample per box. For a single event, enter one time or NA. For multiple events, enter one comma-separated time or NA per event, in the displayed event order. NA means the event is not included in the conditioning information; it does not mean the event is known to be absent up to the landmark. The landmark is the latest entered time, or 0 if all entries are NA. Plus adds another sample; minus removes this sample. Click Predict to compare the samples.",
  reuse_model = "When Fit model is clicked, reuse a locally saved fit only if the validated dataset, variable mapping, fitting settings and CopulaSCR version match. Turn this off to force a new fit. Successful fits are saved automatically on this computer.",
  model_file = "Import an Analysis model RDS exported by this app. It restores the dataset, variable mapping, fitted association model, fitting settings and saved marginal result. Only import trusted RDS files. You do not need to upload the original dataset or fit again.",
  file = "Upload a table with column headers (CSV, TSV or TXT), an RData/RDA workspace, or an RDS data frame or matrix. R files must come from a trusted source. Changing the upload clears the current analysis.",
  sep = "Choose the separator used in your text file. This setting is not used for RData, RDA or RDS files. Changing it clears the current analysis.",
  encoding = "Choose the text file's character encoding. Try GB18030 for Chinese CSV files if UTF-8 displays names incorrectly. This setting is not used for R files.",
  data_object = "Choose the data frame or matrix to analyze when an RData workspace contains several tables. Changing the table clears the current analysis.",
  mode = "Single: one intermediate event and a terminal event. Stratified single: the same analysis by groups. Multiple: at least two intermediate events and a terminal event. After changing this choice, validate the mapping and fit the model again.",
  record_format = "Observed times: supply time and 0/1 status columns for each event. Event times: supply event times with NA meaning the event was not observed, a last-follow-up time, and a death-event-time column. Do not use NA for an unknown measurement. Validate and refit after changing the format.",
  event_count = "Number of distinct intermediate event types: a whole number of at least 2, with no fixed upper limit in the app. Each event needs its own time column and, in observed-time format, a status column. Computation may take longer as the number of events increases. Validate and refit after changing this number.",
  terminal_time = "Terminal observation time: death time if death was observed, otherwise the censoring or last-follow-up time. In event-time format, select last follow-up. Use the same time unit and origin for all events. Validate and refit after changing the mapping.",
  terminal_status = "Terminal event indicator: 1 means the terminal event (usually death) occurred; 0 means it was censored. Select a numeric column containing only 0 and 1. Validate and refit after changing the mapping.",
  death_time = "Actual death-event time, with NA for patients whose death was not observed. The app derives the death indicator from missingness. Observed death times must not exceed last follow-up. Validate and refit after changing the mapping.",
  time = "Time column for this intermediate event. In observed-time format, include its censoring time when the event was not observed. In event-time format, use NA for an unobserved event; the app fills it with last follow-up and assigns status 0. Times must not exceed terminal observation time. Validate and refit after changing the mapping.",
  status = "Indicator for this intermediate event: 1 means it occurred at the selected time; 0 means it was censored. Select a numeric column containing only 0 and 1. Validate and refit after changing the mapping.",
  group = "Column defining the strata for a single-intermediate-event analysis. Each nonmissing value identifies a group. Select a column separate from the event time and status columns, then validate and refit.",
  group_binary = "Convert a numeric stratum column to two groups: 0 stays 0; every nonzero value becomes 1. Useful for Charlson score 0 versus greater than 0. Leave off to retain the original groups. Validate and refit after changing this setting.",
  family = "Archimedean copula used to model dependence between event times. The family determines the form of dependence; it is not the marginal time distribution. Available choices are frank, clayton, gumbel, joe and amh. Click Fit model after changing it.",
  B = "Number of replicates for uncertainty estimation in single-event analyses. Set 0 for point estimates without resampling. This app requires at least 2 for marginal confidence intervals. The default 10 is suitable for a quick trial; more replicates increase computation and usually reduce Monte Carlo variability. Click Fit model after changing it.",
  se_method = "Method for estimating standard errors (SE): the package's resampling procedure or bootstrap sampling of records with replacement. Used with the chosen number of replicates for the association and marginal analyses. Click Fit model after changing it.",
  fit_seed = "Seed controlling random computations during model fitting and single-event marginal resampling. Keep the same seed, data and settings to reproduce a run in the same software environment. Click Fit model after changing it.",
  association_n = "Number of simulated records used to reconstruct the multiple-event association wedge. More records usually reduce simulation variability but take longer. Changes redraw the association plot without refitting the model.",
  association_seed = "Random seed for reconstruction of the association wedge. Keep it fixed to compare plots with the same simulated reconstruction. Changes redraw the plot without refitting the model.",
  marg_method = "Estimator of the intermediate-event marginal survival curve: JFKC (Jiang et al.), LRA (Lakhal et al.), FJC (Fine et al.), or Ker (kernel-based). With LRA, FJC or Ker, the terminal curve uses Kaplan-Meier. Click Compute marginals after changing it.",
  km = "Use Kaplan-Meier for the terminal-event survival curve. With JFKC and this option off, the terminal curve uses JFKC; other marginal estimators use Kaplan-Meier regardless of this option. Click Compute marginals after changing it.",
  conf = "Compute and display 95% confidence intervals for single-event marginal survival curves. Requires an association fit with at least 2 resampling replicates. Click Compute marginals to calculate intervals if the current marginal result has none.",
  separate = "Display intermediate-event marginal curves in separate panels rather than together. Changes the plot only; no model refit is needed.",
  margin_event = "Choose which intermediate event to display in the Individual fit tab. Changes the displayed marginal fit only; no model refit is needed.",
  prediction_scope = "Cohort: predict for observed deaths with at least one nonzero observed intermediate event; report event history, DTH, CMST and survival-time quantiles. Selected record: choose one data row and a prediction quantity. Click Predict after changing this choice.",
  pred_type = "Single event: conditional, marginal or joint survival, or cross-hazard ratio (dependence, not a probability). Multiple events: terminal survival, restricted mean survival time, survival-time quantile, restricted residual mean lifetime, or residual-lifetime quantile. Residual quantities measure time from the landmark (latest observed intermediate event). Click Predict after changing it.",
  patient = "Row number in the imported dataset, starting at 1; this is not a patient ID. Prediction uses this row's observed intermediate-event history. Click Predict after changing it.",
  pred_group = "Stratum for the single-event prediction. Choose All strata to display the groups side by side. Click Predict after changing it.",
  event_times = "Comma-separated intermediate-event times for conditional survival scenarios. Suggested defaults are based on the observed event times in your dataset. Values must be nonnegative, within the observed time range and strictly below the prediction horizon, in the dataset's time unit. A value such as 100 is inappropriate if the data only span 0 to 3. Used only for conditional survival. Click Predict after changing them.",
  horizon = "End of the prediction time grid and restriction limit for mean calculations. Use the dataset's time unit and origin. It must exceed the latest entered event time in every prediction sample. This is an absolute follow-up time, not time since the latest event. Click Predict after changing it.",
  grid_size = "Number of equally spaced evaluation times from 0 to the prediction horizon. A larger grid gives finer curve or surface resolution and may take longer; it does not add observations. Click Predict after changing it.",
  tau = "Quantile probability between 0 and 1: 0.5 requests the median, 0.025 and 0.975 request the lower and upper quantiles. Used for quantile survival time and quantile residual lifetime, not mean or survival-curve predictions. Click Predict after changing it.",
  nsim = "Monte Carlo sample size for numerical calculations in multiple-event predictions. More samples usually reduce simulation variability but take longer. This is not the number of patients. Click Predict after changing it.",
  pred_seed = "Random seed for prediction calculations. Relevant to Monte Carlo calculations for multiple events; it does not alter deterministic single-event predictions. Click Predict after changing it.",
  custom_colors = "Enable the selected colors for this plot. Disable to restore the original palette. Color changes redraw existing results without refitting the model or rerunning prediction.",
  color = "Choose the color for this plotted component. The change redraws existing results without refitting the model or rerunning prediction. Reset restores the original swatches and turns custom colors off."
)

parameter_label <- function(id, label) {
  key <- if (grepl("^time_[0-9]+$", id)) "time" else
    if (grepl("^status_[0-9]+$", id)) "status" else id
  explanation <- parameter_help[[key]]
  if (is.null(explanation)) stop(paste("Missing parameter explanation:", id))
  shiny::tagList(label, " ", shiny::tags$span(class = "parameter-help", tabindex = "0",
    role = "button", `aria-label` = paste("Help:", label),
    `data-help` = explanation, shiny::tags$span(`aria-hidden` = "true", "?")))
}

parameter_help_assets <- function() shiny::tagList(
  shiny::tags$style(shiny::HTML("
    .parameter-help{display:inline-flex;align-items:center;justify-content:center;
      width:15px;height:15px;border:1px solid #71808c;border-radius:50%;
      color:#596d7a;font:600 11px/1 Arial,sans-serif;cursor:help;vertical-align:middle;
      margin-left:3px;flex-shrink:0}
    .parameter-help:hover,.parameter-help:focus{color:#08725e;border-color:#08725e;
      outline:2px solid #c4e4dc;outline-offset:2px}
    #parameter-help-popup{position:fixed;z-index:10000;max-width:310px;
      width:max-content;padding:12px 14px;background:#24343f;color:white;
      border-radius:6px;box-shadow:0 4px 16px #0003;font:13px/1.55 Arial,sans-serif;
      font-weight:400;white-space:normal;pointer-events:none}
    #parameter-help-popup[hidden]{display:none}
  ")),
  shiny::tags$script(shiny::HTML("
    (function(){
      var active=null, popup=null;
      function hide(){if(popup)popup.hidden=true;if(active)active.removeAttribute('aria-describedby');active=null;}
      function show(el){
        hide();active=el;
        if(!popup){popup=document.createElement('div');popup.id='parameter-help-popup';
          popup.setAttribute('role','tooltip');document.body.appendChild(popup);}
        popup.textContent=el.getAttribute('data-help');popup.hidden=false;
        popup.style.maxWidth=Math.min(310,window.innerWidth-24)+'px';
        var r=el.getBoundingClientRect(),p=popup.getBoundingClientRect();
        var left=r.right+10,top=r.top-8;
        if(left+p.width>window.innerWidth-12){left=Math.max(12,Math.min(r.left,window.innerWidth-p.width-12));top=r.bottom+10;}
        top=Math.max(12,Math.min(top,window.innerHeight-p.height-12));
        popup.style.left=left+'px';popup.style.top=top+'px';el.setAttribute('aria-describedby',popup.id);
      }
      document.addEventListener('mouseover',function(e){var el=e.target.closest('.parameter-help');if(el&&el!==active)show(el);});
      document.addEventListener('mouseout',function(e){var el=e.target.closest('.parameter-help');if(el&&!el.contains(e.relatedTarget))hide();});
      document.addEventListener('focusin',function(e){var el=e.target.closest('.parameter-help');if(el)show(el);});
      document.addEventListener('focusout',function(e){if(e.target.closest('.parameter-help'))hide();});
      document.addEventListener('click',function(e){var el=e.target.closest('.parameter-help');if(el){e.preventDefault();show(el);}});
      document.addEventListener('keydown',function(e){if(e.key==='Escape')hide();});
      window.addEventListener('scroll',hide,true);window.addEventListener('resize',hide);
      if(window.jQuery)window.jQuery(document).on('shiny:value',hide);
    })();
  "))
)
