## Helper function to capitalize
simpleCap <- function(x) {
    x <- gsub("(\\w)(\\w*)","\\U\\1\\L\\2", x, perl=T)
    .mgsub(c("And", "Of"), c("and", "of"), x)
}

is.list_o_vectors <- function (x) {
    is.list(x) && !is.data.frame(x) && all(sapply(x, is.vector))
}


.mgsub <- function (pattern, replacement, text.var, fixed = TRUE,
	order.pattern = fixed, ...) {

    if (fixed && order.pattern) {
        ord <- rev(order(nchar(pattern)))
        pattern <- pattern[ord]
        if (length(replacement) != 1) replacement <- replacement[ord]
    }
    if (length(replacement) == 1) replacement <- rep(replacement, length(pattern))

    for (i in seq_along(pattern)){
        text.var <- gsub(pattern[i], replacement[i], text.var, fixed = fixed, ...)
    }

    text.var
}

list_namer <- function (x) {
    nms <- names(x)
    if (is.null(nms))
        nms <- rep("", length(x))
    blanks <- nms == ""
    if (sum(blanks) == 0)
        return(x)
    singles <- sapply(x, length) == 1
    names(x)[blanks & singles] <- as.character(x[blanks & singles])
    blanks[blanks & singles] <- FALSE
    left_overs <- !singles & blanks
    if (sum(left_overs) != 0) {
        newnms <- paste0("X", 1:sum(left_overs))
        looptime <- 1
        while (newnms %in% names(x)) {
            newnms[newnms %in% names(x)] <- paste(newnms[newnms %in%
                names(x)], looptime, sep = ".")
            looptime <- 1 + 1
        }
        names(x)[left_overs] <- newnms
    }
    x
}
