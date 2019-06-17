## Define a generic method
asp <- function(object, ...){
    UseMethod("asp")
}

asp.default <- function(object){
    return(object)
}

asp.character <- function(object, tz = "GMT", ...){
    as.POSIXct(object, tz = tz, ...)
}

asp.POSIXct <- function(object){
    object
}

asp.POSIXlt <- function(object){
    as.POSIXct(object)
}

asp.numeric <- function(object){
    ISOdate(1970, 1, 1, 0) + object
}
