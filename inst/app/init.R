## urls for menu
r_url_list <- getOption("radiant.url.list")
r_url_list[["Neural Network"]] <-
  list("tabs_sknn" = list(
    "Summary" = "sklearn/nn/",
    "Predict" = "sklearn/nn/predict/",
    "Plot" = "sklearn/nn/plot/"
  ))
r_url_list[["Classification and regression trees"]] <-
  list("tabs_crtree" = list(
    "Summary" = "sklearn/crtree/",
    "Predict" = "sklearn/crtree/predict/",
    "Plot" = "sklearn/crtree/plot/"
  ))
r_url_list[["Random Forest"]] <-
  list("tabs_rf" = list(
    "Summary" = "sklearn/rf/",
    "Predict" = "sklearn/rf/predict/",
    "Plot" = "sklearn/rf/plot/"
  ))
r_url_list[["Gradient Boosted Trees"]] <-
  list("tabs_gbt" = list(
    "Summary" = "sklearn/gbtf/",
    "Predict" = "sklearn/gbt/predict/",
    "Plot" = "sklearn/gbt/plot/"
  ))
r_url_list[["Evaluate regression"]] <-
  list("tabs_evalreg" = list("Summary" = "sklearn/evalreg/"))
r_url_list[["Evaluate classification"]] <-
  list("tabs_evalbin" = list("Evaluate" = "sklearn/evalbin/", "Confusion" = "sklearn/evalbin/confusion/"))
r_url_list[["Collaborative Filtering"]] <-
  list("tabs_crs" = list("Summary" = "sklearn/crs/", "Plot" = "sklearn/crs/plot/"))
r_url_list[["Decision analysis"]] <-
  list("tabs_dtree" = list(
    "sklearn" = "sklearn/dtree/", "Plot" = "sklearn/dtree/plot/",
    "Sensitivity" = "sklearn/dtree/sensitivity"
  ))
r_url_list[["Simulate"]] <-
  list("tabs_simulate" = list("Simulate" = "sklearn/simulate/", "Repeat" = "sklearn/simulate/repeat/"))
options(radiant.url.list = r_url_list)
rm(r_url_list)

## sklearn menu
options(
  radiant.sklearn_ui =
    tagList(
      navbarMenu(
        "sklearn",
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "www_sklearn/style.css"),
          tags$script(src = "www_sklearn/js/store.js")
        ),
        "Estimate",
        tabPanel("Neural Network", uiOutput("nn")),
        "----", "Trees",
        tabPanel("Classification and regression trees", uiOutput("crtree")),
        tabPanel("Random Forest", uiOutput("rf")),
        tabPanel("Gradient Boosted Trees", uiOutput("gbt")),
        "----", "Evaluate",
        tabPanel("Evaluate regression", uiOutput("evalreg")),
        tabPanel("Evaluate classification", uiOutput("evalbin"))
      )
    )
)
