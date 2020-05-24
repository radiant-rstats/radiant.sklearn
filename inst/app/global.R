## sourcing from radiant.data
options(radiant.path.data = system.file(package = "radiant.data"))
source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = getOption("radiant.encoding", default = "UTF-8"), local = TRUE)

ifelse(grepl("radiant.sklearn", getwd()) && file.exists("../../inst"), "..", system.file(package = "radiant.sklearn")) %>%
  options(radiant.path.sklearn = .)

## setting path for figures in help files
addResourcePath("figures_sklearn", "tools/help/figures/")

## setting path for www resources
addResourcePath("www_sklearn", file.path(getOption("radiant.path.sklearn"), "app/www/"))

## loading urls and ui
source("init.R", encoding = getOption("radiant.encoding", "UTF-8"), local = TRUE)
options(radiant.url.patterns = make_url_patterns())

if (!"package:radiant.sklearn" %in% search() &&
  isTRUE(getOption("radiant.development")) &&
  getOption("radiant.path.sklearn") == "..") {
  options(radiant.from.package = FALSE)
} else {
  options(radiant.from.package = TRUE)
}
