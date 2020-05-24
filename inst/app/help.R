help_sklearn <- c(
  "Neural Network" = "nn.md",
  "Classification and regression trees" = "crtree.md",
  "Random Forest" = "rf.md",
  "Gradient Boosted Trees" = "gbt.md",
  "Evaluate regression" = "evalreg.md",
  "Evaluate classification" = "evalbin.md"
)
output$help_sklearn <- reactive(append_help("help_sklearn", file.path(getOption("radiant.path.sklearn"), "app/tools/help/"), Rmd = TRUE))
observeEvent(input$help_sklearn_all, {
  help_switch(input$help_sklearn_all, "help_sklearn")
})
observeEvent(input$help_sklearn_none, {
  help_switch(input$help_sklearn_none, "help_sklearn", help_on = FALSE)
})

help_sklearn_panel <- tagList(
  wellPanel(
    HTML("<label>sklearn menu: <i id='help_sklearn_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_sklearn_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput(
      "help_sklearn", NULL, help_sklearn,
      selected = state_group("help_sklearn"), inline = TRUE
    )
  )
)
