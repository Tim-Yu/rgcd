#' Check the completeness of the date record
#'
#' @param species a string containing all species name and country (if any) in the end, separate by ','.
#' @param number how many record is going to be checked
#' @return the gbif data with a datequality column and a table showing the rate of missing whole eventdate, month and day.
#' @examples
#' Checkdate('Pinus contorta,Danaus plexippus,Canada', number = 20000)
#' @import rgbif
#' @export

Checkdate <- function(species, number = 10000 ){

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
                dat <- occ_search(scientificName = species_c, limit = number,country = country_code)
                #Checking if some species are missing in this area
                s_exist <- logical(length = species_n)
                for(i in seq_along(species_c)){
                        if(species_n == 1){
                                s_exist[i] <- if(length(dat$data) == 0){FALSE}else{TRUE}
                        }else{
                                s_exist[i] <- if(length(dat[[i]]$data) == 0){FALSE}else{TRUE}
                        }
                }
                species_ce <- species_c[s_exist]
                species_ne <- length(species_ce)
        }else{
                dat <- occ_search(scientificName = species_c, limit = number)
        }

        #If no specie is in that area, programme will return the species occurrance data in whole world
        if(species_ne == 0){
                dat <- occ_search(scientificName = species_c, limit = number)
        }
        #Searching for the missing value
        if(species_n == 1){ # Only one species
                dat$data$datequality <- NULL
                dat$data <- cbind(dat$data, datequality = c(""))
                dat$data$datequality <- as.character(dat$data$datequality)
                for(j in seq_along(dat$data$name)){
                        if(is.na(dat$data$year[j])) {
                                dat$data$datequality[j] <- c("Eventdate missing.")
                        }else if(is.na(dat$data$month[j])) {
                                dat$data$datequality[j] <- c("Only event year is known.")
                        }else if(is.null(dat$data$day[j])){

                        }else if(is.na(dat$data$day[j])) {
                                dat$data$datequality[j] <- c("No specific day record")
                        }
                        if(dat$data$datequality[j] == c("")) {
                                dat$data$datequality[j] <- c("Full record")
                        }
                }
                Nodaterate <- length(dat$data$name[is.na(dat$data$year)]) / length(dat$data$name)
                Nomrate <- length(dat$data$name[is.na(dat$data$month)]) / length(dat$data$name)
                if(is.null(dat$data$day[j])){
                        Nodrate <- 1
                }else{
                        Nodrate <- length(dat$data$name[is.na(dat$data$day)]) / length(dat$data$name)
                }
                table <- data.frame(species_c, Nodaterate, Nomrate, Nodrate)
                colnames(table) <- c("Species names", "No eventdate rate", "No month rate", "No day rate")
                print(table)
        }else{ #Multipel species
                rates <- matrix(nrow = length(species_ce), ncol = 3)
                for(i in seq_along(species_ce)){
                        dat[[i]]$data$datequality <- NULL
                        dat[[i]]$data <- cbind(dat[[i]]$data, datequality = c(""))
                        dat[[i]]$data$datequality <- as.character(dat[[i]]$data$datequality)
                        for(j in seq_along(dat[[i]]$data$name)){
                                if(is.na(dat[[i]]$data$year[j])) {
                                        dat[[i]]$data$datequality[j] <- c("Eventdate missing.")
                                }else if(is.na(dat[[i]]$data$month[j])) {
                                        dat[[i]]$data$datequality[j] <- c("Only event year is known.")
                                }else if(is.null(dat[[i]]$data$day[j])) {

                                }else if(is.na(dat[[i]]$data$day[j])) {
                                        dat[[i]]$data$datequality[j] <- c("No specific day record")
                                }
                                if(dat[[i]]$data$datequality[j] == c("")) {
                                        dat[[i]]$data$datequality[j] <- c("Full record")
                                }
                        }
                        rates[i,1] <- length(dat[[i]]$data$name[is.na(dat[[i]]$data$year)]) / length(dat[[i]]$data$name)
                        rates[i,2] <- length(dat[[i]]$data$name[is.na(dat[[i]]$data$month)]) / length(dat[[i]]$data$name)
                        if(is.null(dat[[i]]$data$day[j])) {
                                Nodrate <- 1
                        }else {
                                rates[i,3] <- length(dat[[i]]$data$name[is.na(dat[[i]]$data$day)]) / length(dat[[i]]$data$name)
                        }
                }
                table <- data.frame(species_ce, rates)
                colnames(table) <- c("Species names", "No eventdate rate", "No month rate", "No day rate")
                print(table)
        }

        invisible(dat)
}
