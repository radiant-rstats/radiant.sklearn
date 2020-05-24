#' Neural Networks using sklearn
#'
#' @details See \url{https://radiant-rstats.github.io/docs/sklearn/neural_network.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the model
#' @param evar Explanatory variables in the model
#' @param type Model type (i.e., "classification" or "regression")
#' @param lev The level in the response variable defined as _success_
#' @param size Number of units (nodes) in the hidden layer
#' @param decay Parameter decay
#' @param wts Weights to use in estimation
#' @param seed Random seed to use as the starting point
#' @param check Optional estimation parameters ("standardize" is the default)
#' @param form Optional formula to use instead of rvar and evar
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list with all variables defined in nn as an object of class nn
#'
#' @export
neural_network <- function(
  dataset, rvar, evar,
  type = "classification", lev = "",
  size = 1, decay = .5, wts = "None",
  seed = NA, check = "standardize",
  form,
  data_filter = "", envir = parent.frame()
) {

}

#' Summary method for the nn function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{nn}}
#' @param prn Print list of weights
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.neural_network <- function(object, prn = TRUE, ...) {

}

#' Plot method for the nn function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{nn}}
#' @param shiny Did the function call originate inside a shiny app
#' @param plots Plots to produce for the specified Neural Network model. Use "" to avoid showing any plots (default). Options are "olden" or "garson" for importance plots, or "net" to depict the network structure
#' @param size Font size used
#' @param pad_x Padding for explanatory variable lables in the network plot. Default value is 0.9, smaller numbers (e.g., 0.5) increase the amount of padding
#' @param nrobs Number of data points to show in dashboard scatter plots (-1 for all)
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.neural_network <- function(
  x, plots = "garson", size = 12, pad_x = 0.9, nrobs = -1,
  shiny = FALSE, custom = FALSE, ...
) {

}

#' Predict method for the nn function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{nn}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., diamonds). The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable, create a vector of prediction strings, (e.g., c('pclass = levels(pclass)', 'age = seq(0,100,20)')
#' @param dec Number of decimals to show
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @export
predict.neural_network <- function(
  object, pred_data = NULL, pred_cmd = "",
  dec = 3, envir = parent.frame(), ...
) {

}

#' Cross-validation for a Neural Network
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
#'
#' @param object Object of type "nn" or "nnet"
#' @param K Number of cross validation passes to use
#' @param repeats Repeated cross validation
#' @param size Number of units (nodes) in the hidden layer
#' @param decay Parameter decay
#' @param seed Random seed to use as the starting point
#' @param trace Print progress
#' @param fun Function to use for model evaluation (i.e., auc for classification and RMSE for regression)
#' @param ... Additional arguments to be passed to 'fun'
#'
#' @return A data.frame sorted by the mean of the performance metric
#'
#' @export
cv.neural_network <- function(
  object, K = 5, repeats = 1, decay = seq(0, 1, .2), size = 1:5,
  seed = 1234, trace = TRUE, fun, ...
) {

}
