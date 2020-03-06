
check_is_integer <- function(var, varname) {
    if(!is.integer(var)) {
        stop(paste0(varname, " must be an integer"))
    }
}
