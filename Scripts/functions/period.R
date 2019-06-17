period <- function(tstart, time, tend=NA) {
    if (class(tstart)[1] == "character") 
        tstart <- asp(tstart)
    if (is.na(tend))
        tend <- time[length(time)]
    if (class(tend)[1] == "character") 
        tend <- asp(tend)
    asp(tstart) < time & time <= asp(tend)
}
