## ui for model menu in radiant
navbar_proj(
  do.call(
    navbarPage,
    c(
      "Radiant",
      getOption("radiant.nav_ui"),
      getOption("radiant.sklearn_ui"),
      getOption("radiant.shared_ui"),
      help_menu("help_sklearn_ui")
    )
  )
)
