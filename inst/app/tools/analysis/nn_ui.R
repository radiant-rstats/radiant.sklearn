reg_predict <- c(
  "None" = "none",
  "Data" = "data",
  "Command" = "cmd",
  "Data & Command" = "datacmd"
)

neural_network_plots <- c(
  "None" = "none",
  "Network" = "net",
  "Olden" = "olden",
  "Garson" = "garson",
  "Partial Dependence" = "pdp",
  "Dashboard" = "dashboard"
)

## list of function arguments
neural_network_args <- as.list(formals(neural_network))

## list of function inputs selected by user
neural_network_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  neural_network_args$data_filter <- if (input$show_filter) input$data_filter else ""
  neural_network_args$dataset <- input$dataset
  for (i in r_drop(names(neural_network_args)))
    neural_network_args[[i]] <- input[[paste0("neural_network_", i)]]
  neural_network_args
})

neural_network_pred_args <- as.list(if (exists("predict.nn")) {
  formals(predict.nn)
} else {
  formals(radiant.model:::predict.nn)
})

# list of function inputs selected by user
neural_network_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(neural_network_pred_args))
    neural_network_pred_args[[i]] <- input[[paste0("neural_network_", i)]]

  neural_network_pred_args$pred_cmd <- neural_network_pred_args$pred_data <- ""
  if (input$neural_network_predict == "cmd") {
    neural_network_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$neural_network_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$neural_network_predict == "data") {
    neural_network_pred_args$pred_data <- input$neural_network_pred_data
  } else if (input$neural_network_predict == "datacmd") {
    neural_network_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$neural_network_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    neural_network_pred_args$pred_data <- input$neural_network_pred_data
  }
  neural_network_pred_args
})

neural_network_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
  formals(plot.model.predict)
} else {
  formals(radiant.model:::plot.model.predict)
} )

# list of function inputs selected by user
neural_network_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(neural_network_pred_plot_args))
    neural_network_pred_plot_args[[i]] <- input[[paste0("neural_network_", i)]]
  neural_network_pred_plot_args
})

output$ui_neural_network_rvar <- renderUI({
  req(input$neural_network_type)

  withProgress(message = "Acquiring variable information", value = 1, {
    if (input$neural_network_type == "classification") {
      vars <- two_level_vars()
    } else {
      isNum <- .get_class() %in% c("integer", "numeric", "ts")
      vars <- varnames()[isNum]
    }
  })

  init <- isolate(input$neural_network_rvar)

  selectInput(
    inputId = "neural_network_rvar",
    label = "Response variable:",
    choices = vars,
    selected = state_single("neural_network_rvar", vars, init),
    multiple = FALSE
  )
})

output$ui_neural_network_lev <- renderUI({
  req(input$neural_network_type == "classification")
  req(available(input$neural_network_rvar))
  levs <- .get_data()[[input$neural_network_rvar]] %>%
    as_factor() %>%
    levels()

  init <- isolate(input$neural_network_lev)
  selectInput(
    inputId = "neural_network_lev", label = "Choose level:",
    choices = levs,
    selected = state_init("neural_network_lev", init)
  )
})

output$ui_neural_network_evar <- renderUI({
  if (not_available(input$neural_network_rvar)) return()
  vars <- varnames()
  if (length(vars) > 0) {
    vars <- vars[-which(vars == input$neural_network_rvar)]
  }

  init <- isolate(input$neural_network_evar)
  selectInput(
    inputId = "neural_network_evar",
    label = "Explanatory variables:",
    choices = vars,
    selected = state_multiple("neural_network_evar", vars, init),
    multiple = TRUE,
    size = min(10, length(vars)),
    selectize = FALSE
  )
})

output$ui_neural_network_wts <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$neural_network_evar)) {
    vars <- base::setdiff(vars, input$neural_network_evar)
    names(vars) <- varnames() %>%
      {.[match(vars, .)]} %>%
      names()
  }
  vars <- c("None", vars)

  selectInput(
    inputId = "neural_network_wts", label = "Weights:", choices = vars,
    selected = state_single("neural_network_wts", vars),
    multiple = FALSE
  )
})

output$ui_neural_network_store_pred_name <- renderUI({
  init <- state_init("neural_network_store_pred_name", "pred_nn") %>%
    sub("\\d{1,}$", "", .) %>%
    paste0(., ifelse(is_empty(input$neural_network_size), "", input$neural_network_size))
  textInput(
    "neural_network_store_pred_name",
    "Store predictions:",
    init
  )
})

output$ui_neural_network_store_res_name <- renderUI({
  req(input$dataset)
  textInput("neural_network_store_res_name", "Store residuals:", "", placeholder = "Provide variable name")
})

## reset prediction and plot settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "neural_network_predict", selected = "none")
  updateSelectInput(session = session, inputId = "neural_network_plots", selected = "none")
})

## reset prediction settings when the model type changes
observeEvent(input$neural_network_type, {
  updateSelectInput(session = session, inputId = "neural_network_predict", selected = "none")
  updateSelectInput(session = session, inputId = "neural_network_plots", selected = "none")
})

output$ui_neural_network_predict_plot <- renderUI({
  predict_plot_controls("nn")
})

output$ui_neural_network_plots <- renderUI({
  req(input$neural_network_type)
  if (input$neural_network_type != "regression") {
    neural_network_plots <- head(neural_network_plots, -1)
  }
  selectInput(
    "neural_network_plots", "Plots:", choices = neural_network_plots,
    selected = state_single("neural_network_plots", neural_network_plots)
  )
})

output$ui_neural_network_nrobs <- renderUI({
  nrobs <- nrow(.get_data())
  choices <- c("1,000" = 1000, "5,000" = 5000, "10,000" = 10000, "All" = -1) %>%
    .[. < nrobs]
  selectInput(
    "neural_network_nrobs", "Number of data points plotted:",
    choices = choices,
    selected = state_single("neural_network_nrobs", choices, 1000)
  )
})

## add a spinning refresh icon if the model needs to be (re)estimated
run_refresh(neural_network_args, "nn", tabs = "tabs_nn", label = "Estimate model", relabel = "Re-estimate model")

output$ui_nn <- renderUI({
  req(input$dataset)
  tagList(
    conditionalPanel(condition = "input.tabs_nn == 'Summary'",
      wellPanel(
        actionButton("neural_network_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
      )
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_nn == 'Summary'",
        radioButtons(
          "neural_network_type", label = NULL, c("classification", "regression"),
          selected = state_init("neural_network_type", "classification"),
          inline = TRUE
        ),
        uiOutput("ui_neural_network_rvar"),
        uiOutput("ui_neural_network_lev"),
        uiOutput("ui_neural_network_evar"),
        uiOutput("ui_neural_network_wts"),
        tags$table(
          tags$td(numericInput(
            "neural_network_size", label = "Size:", min = 1, max = 20,
            value = state_init("neural_network_size", 1), width = "77px"
          )),
          tags$td(numericInput(
            "neural_network_decay", label = "Decay:", min = 0, max = 1,
            step = .1, value = state_init("neural_network_decay", .5), width = "77px"
          )),
          tags$td(numericInput(
            "neural_network_seed", label = "Seed:",
            value = state_init("neural_network_seed", 1234), width = "77px"
          ))
        )
      ),
      conditionalPanel(
        condition = "input.tabs_nn == 'Predict'",
        selectInput(
          "neural_network_predict", label = "Prediction input type:", reg_predict,
          selected = state_single("neural_network_predict", reg_predict, "none")
        ),
        conditionalPanel(
          "input.neural_network_predict == 'data' | input.neural_network_predict == 'datacmd'",
          selectizeInput(
            inputId = "neural_network_pred_data", label = "Prediction data:",
            choices = c("None" = "", r_info[["datasetlist"]]),
            selected = state_single("neural_network_pred_data", c("None" = "", r_info[["datasetlist"]])),
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.neural_network_predict == 'cmd' | input.neural_network_predict == 'datacmd'",
          returnTextAreaInput(
            "neural_network_pred_cmd", "Prediction command:",
            value = state_init("neural_network_pred_cmd", ""),
            rows = 3,
            placeholder = "Type a formula to set values for model variables (e.g., carat = 1; cut = 'Ideal') and press return"
          )
        ),
        conditionalPanel(
          condition = "input.neural_network_predict != 'none'",
          checkboxInput("neural_network_pred_plot", "Plot predictions", state_init("neural_network_pred_plot", FALSE)),
          conditionalPanel(
            "input.neural_network_pred_plot == true",
            uiOutput("ui_neural_network_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel(
          "input.neural_network_predict == 'data' | input.neural_network_predict == 'datacmd'",
          tags$table(
            tags$td(uiOutput("ui_neural_network_store_pred_name")),
            tags$td(actionButton("neural_network_store_pred", "Store", icon = icon("plus")), style = "padding-top:30px;")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_nn == 'Plot'",
        uiOutput("ui_neural_network_plots"),
        conditionalPanel(
          condition = "input.neural_network_plots == 'dashboard'",
          uiOutput("ui_neural_network_nrobs")
        )
      ),
      conditionalPanel(
        condition = "input.tabs_nn == 'Summary'",
        tags$table(
          tags$td(uiOutput("ui_neural_network_store_res_name")),
          tags$td(actionButton("neural_network_store_res", "Store", icon = icon("plus")), style = "padding-top:30px;")
        )
      )
    ),
    help_and_report(
      modal_title = "Neural Network",
      fun_name = "nn",
      help_file = inclMD(file.path(getOption("radiant.path.sklearn"), "app/tools/help/nn.md"))
    )
  )
})

neural_network_plot <- reactive({
  if (neural_network_available() != "available") return()
  if (is_empty(input$neural_network_plots, "none")) return()
  res <- .nn()
  if (is.character(res)) return()
  if ("dashboard" %in% input$neural_network_plots) {
    plot_height <- 750
  } else if ("pdp" %in% input$neural_network_plots) {
    plot_height <- max(500, ceiling(length(res$evar) / 2) * 250)
  } else {
    mlt <- if ("net" %in% input$neural_network_plots) 45 else 30
    plot_height <- max(500, length(res$model$coefnames) * mlt)
  }
  list(plot_width = 650, plot_height = plot_height)
})

neural_network_plot_width <- function()
  neural_network_plot() %>% {if (is.list(.)) .$plot_width else 650}

neural_network_plot_height <- function()
  neural_network_plot() %>% {if (is.list(.)) .$plot_height else 500}

neural_network_pred_plot_height <- function()
  if (input$neural_network_pred_plot) 500 else 1

## output is called from the main radiant ui.R
output$nn <- renderUI({
  register_print_output("summary_nn", ".summary_nn")
  register_print_output("predict_nn", ".predict_print_nn")
  register_plot_output(
    "predict_plot_nn", ".predict_plot_nn",
    height_fun = "neural_network_pred_plot_height"
  )
  register_plot_output(
    "plot_nn", ".plot_nn",
    height_fun = "neural_network_plot_height",
    width_fun = "neural_network_plot_width"
  )

  ## three separate tabs
  neural_network_output_panels <- tabsetPanel(
    id = "tabs_nn",
    tabPanel(
      "Summary",
      verbatimTextOutput("summary_nn")
    ),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.neural_network_pred_plot == true",
        download_link("dlp_neural_network_pred"),
        plotOutput("predict_plot_nn", width = "100%", height = "100%")
      ),
      download_link("dl_neural_network_pred"), br(),
      verbatimTextOutput("predict_nn")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_nn"),
      plotOutput("plot_nn", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "sklearn > Estimate",
    tool = "Neural Network",
    tool_ui = "ui_nn",
    output_panels = neural_network_output_panels
  )
})

neural_network_available <- reactive({
  req(input$neural_network_type)
  if (not_available(input$neural_network_rvar)) {
    if (input$neural_network_type == "classification") {
      "This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>%
        suggest_data("titanic")
    } else {
      "This analysis requires a response variable of type integer\nor numeric and one or more explanatory variables.\nIf these variables are not available please select another dataset.\n\n" %>%
        suggest_data("diamonds")
    }
  } else if (not_available(input$neural_network_evar)) {
    if (input$neural_network_type == "classification") {
      "Please select one or more explanatory variables.\n\n" %>%
        suggest_data("titanic")
    } else {
      "Please select one or more explanatory variables.\n\n" %>%
        suggest_data("diamonds")
    }
  } else {
    "available"
  }
})

.nn <- eventReactive(input$neural_network_run, {
  nni <- neural_network_inputs()
  nni$envir <- r_data
  withProgress(
    message = "Estimating model", value = 1,
    do.call(nn, nni)
  )
})

.summary_nn <- reactive({
  if (not_pressed(input$neural_network_run)) return("** Press the Estimate button to estimate the model **")
  if (neural_network_available() != "available") return(neural_network_available())
  summary(.nn())
})

.predict_nn <- reactive({
  if (not_pressed(input$neural_network_run)) return("** Press the Estimate button to estimate the model **")
  if (neural_network_available() != "available") return(neural_network_available())
  if (is_empty(input$neural_network_predict, "none")) return("** Select prediction input **")

  if ((input$neural_network_predict == "data" || input$neural_network_predict == "datacmd") && is_empty(input$neural_network_pred_data)) {
    return("** Select data for prediction **")
  }
  if (input$neural_network_predict == "cmd" && is_empty(input$neural_network_pred_cmd)) {
    return("** Enter prediction commands **")
  }

  withProgress(message = "Generating predictions", value = 1, {
    nni <- neural_network_pred_inputs()
    nni$object <- .nn()
    nni$envir <- r_data
    do.call(predict, nni)
  })
})

.predict_print_nn <- reactive({
  .predict_nn() %>% {
    if (is.character(.)) cat(., "\n") else print(.)
  }
})

.predict_plot_nn <- reactive({
  req(
    pressed(input$neural_network_run), input$neural_network_pred_plot,
    available(input$neural_network_xvar),
    !is_empty(input$neural_network_predict, "none")
  )

  # if (not_pressed(input$neural_network_run)) return(invisible())
  # if (neural_network_available() != "available") return(neural_network_available())
  # req(input$neural_network_pred_plot, available(input$neural_network_xvar))
  # if (is_empty(input$neural_network_predict, "none")) return(invisible())
  # if ((input$neural_network_predict == "data" || input$neural_network_predict == "datacmd") && is_empty(input$neural_network_pred_data)) {
  #   return(invisible())
  # }
  # if (input$neural_network_predict == "cmd" && is_empty(input$neural_network_pred_cmd)) {
  #   return(invisible())
  # }

  withProgress(message = "Generating prediction plot", value = 1, {
    do.call(plot, c(list(x = .predict_nn()), neural_network_pred_plot_inputs()))
  })
})

.plot_nn <- reactive({
  if (not_pressed(input$neural_network_run)) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (neural_network_available() != "available") {
    return(neural_network_available())
  }
  req(input$neural_network_size)
  if (is_empty(input$neural_network_plots, "none")) {
    return("Please select a neural network plot from the drop-down menu")
  }
  pinp <- list(plots = input$neural_network_plots, shiny = TRUE)
  if (input$neural_network_plots == "dashboard") {
    req(input$neural_network_nrobs)
    pinp <- c(pinp, nrobs = as_integer(input$neural_network_nrobs))
  }

  if (input$neural_network_plots == "net") {
    .nn() %>% {if (is.character(.)) invisible() else capture_plot(do.call(plot, c(list(x = .), pinp)))}
  } else {
    withProgress(message = "Generating plots", value = 1, {
      do.call(plot, c(list(x = .nn()), pinp))
    })
  }
})

observeEvent(input$neural_network_store_res, {
  req(pressed(input$neural_network_run))
  robj <- .nn()
  if (!is.list(robj)) return()
  fixed <- fix_names(input$neural_network_store_res_name)
  updateTextInput(session, "neural_network_store_res_name", value = fixed)
  withProgress(
    message = "Storing residuals", value = 1,
    r_data[[input$dataset]] <- store(r_data[[input$dataset]], robj, name = fixed)
  )
})

observeEvent(input$neural_network_store_pred, {
  req(!is_empty(input$neural_network_pred_data), pressed(input$neural_network_run))
  pred <- .predict_nn()
  if (is.null(pred)) return()
  fixed <- fix_names(input$neural_network_store_pred_name)
  updateTextInput(session, "neural_network_store_pred_name", value = fixed)
  withProgress(
    message = "Storing predictions", value = 1,
    r_data[[input$neural_network_pred_data]] <- store(
      r_data[[input$neural_network_pred_data]], pred,
      name = fixed
    )
  )
})

observeEvent(input$neural_network_report, {
  if (is_empty(input$neural_network_evar)) return(invisible())

  outputs <- c("summary")
  inp_out <- list(list(prn = TRUE), "")
  figs <- FALSE

  if (!is_empty(input$neural_network_plots, "none")) {
    if (input$neural_network_type == "regression" && input$neural_network_plots == "dashboard") {
      inp_out[[2]] <- list(plots = input$neural_network_plots, nrobs = as_integer(input$neural_network_nrobs), custom = FALSE)
    } else {
      inp_out[[2]] <- list(plots = input$neural_network_plots, custom = FALSE)
    }
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }

  if (!is_empty(input$neural_network_store_res_name)) {
    fixed <- fix_names(input$neural_network_store_res_name)
    updateTextInput(session, "neural_network_store_res_name", value = fixed)
    xcmd <- paste0(input$dataset, " <- store(", input$dataset, ", result, name = \"", fixed, "\")\n")
  } else {
    xcmd <- ""
  }

  if (!is_empty(input$neural_network_predict, "none") &&
    (!is_empty(input$neural_network_pred_data) || !is_empty(input$neural_network_pred_cmd))) {
    pred_args <- clean_args(neural_network_pred_inputs(), neural_network_pred_args[-1])

    if (!is_empty(pred_args$pred_cmd)) {
      pred_args$pred_cmd <- strsplit(pred_args$pred_cmd, ";")[[1]]
    } else {
      pred_args$pred_cmd <- NULL
    }

    if (!is_empty(pred_args$pred_data)) {
      pred_args$pred_data <- as.symbol(pred_args$pred_data)
    } else {
      pred_args$pred_data <- NULL
    }

    inp_out[[2 + figs]] <- pred_args
    outputs <- c(outputs, "pred <- predict")
    xcmd <- paste0(xcmd, "print(pred, n = 10)")
    if (input$neural_network_predict %in% c("data", "datacmd")) {
      fixed <- fix_names(input$neural_network_store_pred_name)
      updateTextInput(session, "neural_network_store_pred_name", value = fixed)
      xcmd <- paste0(xcmd, "\n", input$neural_network_pred_data , " <- store(",
        input$neural_network_pred_data, ", pred, name = \"", fixed, "\")"
      )
    }

    if (input$neural_network_pred_plot && !is_empty(input$neural_network_xvar)) {
      inp_out[[3 + figs]] <- clean_args(neural_network_pred_plot_inputs(), neural_network_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  neural_network_inp <- neural_network_inputs()
  if (input$neural_network_type == "regression") {
    neural_network_inp$lev <- NULL
  }

  update_report(
    inp_main = clean_args(neural_network_inp, neural_network_args),
    fun_name = "nn",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = neural_network_plot_width(),
    fig.height = neural_network_plot_height(),
    xcmd = xcmd
  )
})

dl_neural_network_pred <- function(path) {
  if (pressed(input$neural_network_run)) {
    write.csv(.predict_nn(), file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_neural_network_pred",
  fun = dl_neural_network_pred,
  fn = function() paste0(input$dataset, "_neural_network_pred"),
  type = "csv",
  caption = "Save predictions"
)

download_handler(
  id = "dlp_neural_network_pred",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_neural_network_pred"),
  type = "png",
  caption = "Save neural network prediction plot",
  plot = .predict_plot_nn,
  width = plot_width,
  height = neural_network_pred_plot_height
)

download_handler(
  id = "dlp_nn",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_nn"),
  type = "png",
  caption = "Save neural network plot",
  plot = .plot_nn,
  width = neural_network_plot_width,
  height = neural_network_plot_height
)
