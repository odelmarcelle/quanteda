#' [Experimental] Create feature weight vector from weighted patterns
#' 
#' @param x named numeric vector as weighted patterns
#' @param y dfm for which weight vector is created
#' @examples
#' sent = c("good" = 1, "great*" = 1, "bad" = -1, "wors*" = -1)
#' mt <- dfm(c("Good and great, greatest are positive", 
#'             "Bad, worse and worst are negative"), tolower = FALSE)
#' weight <- as.weights(sent, mt, nomatch_weight = 0)
#' dfm_weight(mt, weights = weight)
#' @export
as.weights <- function(x, y, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE,
                       nomatch_weight = NULL) {
    
    valuetype <- match.arg(valuetype)
    if (is.null(names(x)))
        stop("x must be a named vector\n")
    if (!is.dfm(y))
        stop("y must be a dfm\n")
        
    weight <- unlist(mapply(weight_patterns, names(x), unname(x),
                            MoreArgs = list(featnames(y), valuetype, case_insensitive), 
                            USE.NAMES = FALSE, SIMPLIFY = FALSE))
    
    if (!is.null(nomatch_weight)) {
        nomatch <- setdiff(featnames(y), names(weight))
        weight_nomatch <- rep(nomatch_weight[1], length(nomatch))
        names(weight_nomatch) <- nomatch
        weight <- c(weight, weight_nomatch)[featnames(y)]
    }
    
    return(weight)
}

weight_patterns <- function(patterns, weight, type, valuetype, case_insensitive) {
    s <- unlist(pattern2fixed(patterns, type, valuetype, case_insensitive))
    v <- rep(weight, length(s))
    names(v) <- s
    return(v)
}
