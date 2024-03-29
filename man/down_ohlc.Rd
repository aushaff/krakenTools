% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/down_ohlc.R
\name{down_ohlc}
\alias{down_ohlc}
\title{download ohlc for an asset pair}
\usage{
down_ohlc(asset_pair, min_date, max_date, interval)
}
\arguments{
\item{asset_pair}{character of the asset pair as accepted by kraken api}

\item{interval}{the period in minutes for each candle}

\item{min_dt}{the datetime to start from as a unix timestamp}

\item{max_dt}{the datetime to end at as a unix timestamp}
}
\value{
a dataframe
}
\description{
download ohlc between specified dates, of a specified period for
the specified asset pair
}
