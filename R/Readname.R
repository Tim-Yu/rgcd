#' Read species names and check if querying a specific country
#'
#' @param species a string containing all species name and country (if any) in the end, separate by ','.
#' @return a list of all species name, how many species, indicater of specific country, country 2 letter code and the country name
#' @examples
#' Readname('Pinus contorta,Danaus plexippus,canada')


Readname <- function(species){
        #Read the names
        species_c <- strsplit(species, ",")
        species_c <- species_c[[1]]
        species_n <- length(species_c)
        #Check if user want see the occurrence data in specific country
        countryname <- species_c[species_n]
        country_code <- countrycode(countryname, 'country.name', 'iso2c')
        checkcountry <- !is.na(country_code)
        if(checkcountry == TRUE){
                species_n <- species_n - 1
        }
        list(species_c, species_n, checkcountry, country_code, countryname)
}

