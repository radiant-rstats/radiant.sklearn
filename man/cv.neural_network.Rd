% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn.R
\name{cv.neural_network}
\alias{cv.neural_network}
\title{Cross-validation for a Neural Network}
\usage{
cv.neural_network(
  object,
  K = 5,
  repeats = 1,
  decay = seq(0, 1, 0.2),
  size = 1:5,
  seed = 1234,
  trace = TRUE,
  fun,
  ...
)
}
\arguments{
\item{object}{Object of type "nn" or "nnet"}

\item{K}{Number of cross validation passes to use}

\item{repeats}{Repeated cross validation}

\item{decay}{Parameter decay}

\item{size}{Number of units (nodes) in the hidden layer}

\item{seed}{Random seed to use as the starting point}

\item{trace}{Print progress}

\item{fun}{Function to use for model evaluation (i.e., auc for classification and RMSE for regression)}

\item{...}{Additional arguments to be passed to 'fun'}
}
\value{
A data.frame sorted by the mean of the performance metric
}
\description{
Cross-validation for a Neural Network
}
\details{
See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
}
