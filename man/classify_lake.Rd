% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify_lake.R
\name{classify_lake}
\alias{classify_lake}
\title{classify_lake}
\usage{
classify_lake(
  nutrient,
  temperature,
  nutrient_threshold_med = 5,
  nutrient_threshold_high = 8,
  temperature_threshold = 10
)
}
\arguments{
\item{nutrient}{concentraion (mg/L) (at least 5 days)}

\item{temperature}{(C) (at least 5 days)}

\item{nutrient_threshold_med}{(mg/L) (default 5)}

\item{nutrient_threshold_high}{(mg/L) (default 8)}

\item{temperature_threshold}{(mg/L) (default 10)}
}
\value{
risk (high,med, low), mean nutrient concentration (mg/L)
}
\description{
compute net present value
}
