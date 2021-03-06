% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Readname.R
\name{Readname}
\alias{Readname}
\title{Read species names and check if querying a specific country}
\usage{
Readname(species)
}
\arguments{
\item{species}{a string containing all species name and country (if any) in the end, separate by ','.}
}
\value{
a list of all species name, how many species, indicater of specific country, country 2 letter code and the country name
}
\description{
Read species names and check if querying a specific country
}
\examples{
Readname('Pinus contorta,Danaus plexippus,canada')
}
