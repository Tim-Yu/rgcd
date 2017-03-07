#' Get occdat via occ command
#'
#' @param species a string containing all species name and country (if any) in the end, separate by ','.
#' @return a list of occdat, exist species names, all species names, exist speices number, all species number, country name and the indicater of specific country
#' @examples
#' data <- Getoccdat('Pinus contorta,Danaus plexippus,Canada')
#' @import rgbif, spocc, countrycode
#' @export

Getoccdat <- function(species){
        ##Read the names
        #species_c <- strsplit(species, ",")
        #species_c <- species_c[[1]]
        #species_n <- length(species_c)
        ##Check if user want see the occurrence data in specific country
        #countryname <- species_c[species_n]
        #country_code <- countrycode(countryname, 'country.name', 'iso2c')
        #checkcountry <- !is.na(country_code)
        #if(checkcountry == TRUE){
        #}

        namelist <- Readname(species)

        #namelist contains the name "namelist[[1]]" the species number"namelist[[2]]"
        #the indicater of the country "namelist[[3]]" and the countrycode "namelist[[4]]"
        species_c <- namelist[[1]]
        species_n <- namelist[[2]]
        checkcountry <- namelist[[3]]
        country_code <- namelist[[4]]
        countryname <- namelist[[5]]

        #Searching for the occurrence data
        if(checkcountry == TRUE){
                species_c <- species_c[1:species_n]
                dat <- occ(query = species_c, from = 'gbif', limit = 300, has_coords = TRUE, gbifopts = list(country = country_code))
                #Checking if some species are missing in this area
                s_exist <- logical(length = species_n)
                for(i in seq_along(species_c)){
                        s_exist[i] <- if(length(dat$gbif$data[i][[1]]) == 0){FALSE}else{TRUE}
                }
                species_ce <- species_c[s_exist]
                species_ne <- length(species_ce)
        }else{
                dat <- occ(query = species_c, from = 'gbif', limit = 300, has_coords = TRUE)
        }

        #If no specie is in that area, programme will return the species occurrance data in whole world
        if(species_ne == 0){
                dat <- occ(query = species_c, from = 'gbif', limit = 300, has_coords = TRUE)
        }
        list(dat, species_ce, species_c, species_ne, species_n, countryname, checkcountry)
}
