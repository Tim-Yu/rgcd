#' Generate centroid area WKT data
#'
#' @param country the name of interested country
#' @param buffer_range the disstance between the centroid and the border, buffer_range = 1 roughly equals to 118 km
#' @return centroid area WKT data
#' @examples
#' Getcentroid('china', 8)

#get adjustable centroid WKT data function
Getcentroid <- function(country,buffer_range){
        #read shap
        map_total <- readOGR(dsn = file.path('./data','TM_WORLD_BORDERS-0.3', fsep = .Platform$file.sep), layer = 'TM_WORLD_BORDERS-0.3')
        country_map <- map(database = map_total, regions = country)
        #from map to SP
        IDs <- sapply(strsplit(country_map$names, ":"), function(x) x[1])
        country_map_sp <- map2SpatialPolygons(country_map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
        #forming the adjustable centroid rigion
        centroid <- gBuffer(country_map_sp, width = -buffer_range)
        centroid_WKT <- writeWKT(centroid)
}
