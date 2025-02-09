% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/best_model_fit.R
\name{best_model_fit}
\alias{best_model_fit}
\title{Return Model Fit for Plotting}
\usage{
best_model_fit(df = NULL, ModelBestFit = NULL, Scheme = NULL,
  Pop = NULL, Cores = NULL, Iterations = 10000, BurnIn = 5000,
  Alpha = 0.99, Treedepth = 10)
}
\arguments{
\item{df}{A dataframe of fitted model results}

\item{ModelBestFit}{A character string identifying the model to refit.}

\item{Scheme}{A character string identifying which scheme to refit the model for.}

\item{Pop}{A character string identifying the population to refit the model for.}

\item{Cores}{Numeric, defaults to the number of cores available minus 1.
How many cores to use for the analysis.}

\item{Iterations}{Numeric, the number of iterations to use for the MCMC sampler. Defaults to
10000.}

\item{BurnIn}{Numeric, the number of iterations to use for the burn in. Defaults to 5000.}

\item{Alpha}{Numeric, set the adaptive alpha value for the MCMC. Defaults to 0.99}

\item{Treedepth}{Numeric, set the maximum tree depth. Defaults to 10.}
}
\value{
Fitted model object
}
\description{
Extracts a given model and refits it, returning the fitted model object.
}
\examples{

}
\seealso{
Base_Model
}
