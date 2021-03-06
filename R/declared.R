# Copyright (c) 2022, Adrian Dusa
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * The names of its contributors may NOT be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

`is.declared` <- function(x) {
    inherits(x, "declared")
}
`as.declared` <- function(x, ...) {
    UseMethod("as.declared")
}
`as.declared.default` <- function(x, ...) {
    interactive <- TRUE
    dots <- list(...)
    if (!is.null(dots$interactive)) {
        interactive <- dots$interactive
    }
    if (isTRUE(interactive)) {
        msg <- "There is no automatic class method conversion for this type of"
        if (!is.null(dots$vname_)) {
            msg <- paste0(dots$vname_, ": ", msg, " variable.")
        }
        else {
            msg <- paste(msg, "object.")
        }
        message(msg)
    }
    return(x)
}
`as.declared.haven_labelled` <- function(x, ...) {
    dots <- list(...)
    na_values <- attr(x, "na_values")
    na_range <- attr(x, "na_range")
    labels <- attr(x, "labels", exact = TRUE)
    label <- attr(x, "label", exact = TRUE)
    format_spss <- attr(x, "format.spss") 
    if (!inherits(x, "haven_labelled_spss")) {
        tagged <- hasTag_(x)
        attributes(x) <- NULL
        if (any(tagged)) {
            x[tagged] <- getTag_(x[tagged])
        }
        if (!is.null(labels)) {
            nms <- names(labels)
            tagged <- hasTag_(labels)
            if (any(tagged)) {
                labels[tagged] <- getTag_(labels[tagged])
                na_values <- sort(unname(labels[tagged]))
            }
            labels <- coerceMode_(labels)
            names(labels) <- nms
        }
        misvals <- na_values
    }
    else {
        misvals <- all_missing_values(unclass(x))
    }
    attributes(x) <- NULL
    missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]
    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels
    attr(x, "label") <- label
    attr(x, "format.spss") <- format_spss
    return(x)
}
`as.declared.factor` <- function(x, ...) {
    return(declared(x, ... = ...))
}
`as.declared.data.frame` <- function(x, ..., interactive = FALSE) {
    if (isFALSE(interactive)) {
        x[] <- lapply(x, as.declared, interactive = FALSE, ... = ...)
    }
    else {
        nms <- names(x)
        for (i in seq(length(nms))) {
            x[[i]] <- as.declared(x[[i]], vname_ = nms[i], ... = ...)
        }
    }
    return(x)
}
`undeclare` <- function(x, drop = FALSE, ...) {
    UseMethod("undeclare")
}
`undeclare.default` <- function(x, drop = FALSE, ...) {
    if (isTRUE(drop)) {
        attributes(x) <- NULL
    }
    return(x)
}
`undeclare.declared` <- function(x, drop = FALSE, ...) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)
    attributes(x) <- NULL 
    if (!is.null(na_index)) {
        x[na_index] <- names(na_index)
    }
    x <- coerceMode_(x)
    attrx$na_index <- NULL
    attrx$na_values <- NULL
    attrx$na_range <- NULL
    if (isFALSE(drop)) {
        attributes(x) <- attrx
    }
    return(x)
}
`undeclare.data.frame` <- function(x, drop = FALSE, ...) {
    declared <- vapply(x, is.declared, logical(1))
    x[declared] <- lapply(x[declared], undeclare, drop = drop)
    return(x)
}
`validate_declared` <- function(x = double(), labels = NULL, label = NULL,
                                na_values = NULL, na_range = NULL, ...) {
    if (!is.numeric(x) && !is.character(x) && !all(is.na(x))) {
        stopError_("`x` must be a numeric or a character vector.")
    }
    if (!is.null(labels)) {
        if (is.null(names(labels))) {
            stopError_("`labels` must have names.")
        }
        if (any(duplicated(stats::na.omit(labels)))) {
            stopError_("`labels` must be unique.")
        }
        if (is.factor(x)) {
            if (!identical(labels, levels(x))) {
                stopError_("`x` is a factor, and `labels` are different its levels.")
            }
        }
    }
    if (
        !is.null(label) &&
        (!is.atomic(label) || !is.character(label) || length(label) != 1)
    ) {
        stopError_("`label` must be a character vector of length one.")
    }
    if (!is.null(na_values)) {
        if (any(is.na(na_values))) {
            stopError_("`na_values` should not contain NA values.")
        }
    }
    if (!is.null(na_range)) {
        type_ok <-  all(is.na(x)) ||
            (is.character(x) && is.character(na_range)) ||
            (is.numeric(x) && is.numeric(na_range))
        if (!type_ok || length(na_range) != 2) {
            stopError_("`na_range` must be a vector of length two of the same type as `x`.")
        }
        if (any(is.na(na_range))) {
            stopError_("`na_range` can not contain missing values.")
        }
    }
}
`declared` <- function(
    x, labels = NULL, na_values = NULL, na_range = NULL, label = NULL, 
    measurement = NULL, ...
) {
    UseMethod("declared")
}
`declared.default` <- function(
    x, labels = NULL, na_values = NULL, na_range = NULL, label = NULL,
    measurement = NULL, ...
) {
    if (is.factor(x)) {
        nms <- levels(x)
        if (is.null(labels)) {
            labels <- seq(length(nms))
            names(labels) <- nms
        }
        wnms <- which(is.element(na_values, nms))
        if (length(wnms) > 0) {
            for (i in wnms) {
                na_values[i] <- which(nms == na_values[i])
            }
            if (possibleNumeric_(na_values)) {
                na_values <- asNumeric_(na_values)
            }
        }
    }
    xchar <- FALSE
    if (!is.null(labels)) {
        nms <- names(labels)
        if (possibleNumeric_(labels) && (possibleNumeric_(x) | all(is.na(x)))) {
            labels <- asNumeric_(labels)
        }
        else {
            x <- as.character(x)
            labels <- as.character(labels)
            xchar <- TRUE
            na_range <- NULL
        }
        names(labels) <- nms
    }
    if (!is.null(na_values)) {
        if (possibleNumeric_(na_values) & !xchar) {
            na_values <- asNumeric_(na_values)
        }
        else {
            na_values <- as.character(na_values)
        }
    }
    if ((possibleNumeric_(x) | all(is.na(x))) & !xchar) {
        x <- asNumeric_(x)
    }
    else {
        x <- as.character(x)
    }
    attributes(x) <- NULL
    validate_declared(x, labels, label, na_values, na_range)
    misvals <- all_missing_values(x, na_values, na_range, labels)
    if (!is.null(na_range)) {
        if (!is.atomic(na_range) || length(na_range) != 2 ) {
            stopError_("The 'na_range' argument should be an atomic vector of length 2.")
        }
        na_range <- sort(na_range)
    }
    missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]
    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels
    attr(x, "label") <- label
    attr(x, "measurement") <- check_measurement(measurement)
    return(x)
}
`as.character.declared` <- function(x, values = FALSE, ...) {
    labels <- names_values(x)
    x <- undeclare(x, drop = TRUE)
    if (isTRUE(values)) {
        return(as.character(x))
    }
    x[is.element(x, labels)] <- names(labels)[match(x[is.element(x, labels)], labels)]
    return(x)
}
`[.declared` <- function(x, i, ...) {
    attrx <- attributes(x)
    x <- undeclare(x)
    x <- NextMethod()
    declared(x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]])
}
`[<-.declared` <- function(x, i, value) {
    attrx <- attributes(x)
    value <- undeclare(value)
    x <- undeclare(x)
    x <- NextMethod()
    declared(x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]])
}
`c.declared` <- function(...) {
    dots <- list(...)
    declared <- unlist(lapply(dots, is.declared))
    na_values <- sort(unique(unlist(
        lapply(dots, function(x) attr(x, "na_values"))
    )))
    labels <- unlist(lapply(dots, function(x) {
        attr(x, "labels", exact = TRUE)
    }))
    duplicates <- duplicated(labels)
    if (length(wduplicates <- which(duplicates)) > 0) {
        for (i in seq(length(wduplicates))) {
            if (length(unique(names(
                labels[labels == labels[wduplicates[i]]]
            ))) > 1) {
                stopError_("Labels must be unique.")
            }
        }
    }
    labels <- sort(labels[!duplicates])
    na_range <- lapply(dots, function(x) attr(x, "na_range", exact = TRUE))
    nulls <- unlist(lapply(na_range, is.null))
    if (all(nulls)) {
        na_range <- NULL
    }
    else {
        if (sum(nulls) == length(na_range) - 1) {
            na_range <- unlist(na_range)
        }
        else {
            compatible <- logical(length(na_range))
            if (!is.null(na_range)) {
                for (i in seq(1, length(na_range) - 1)) {
                    nai <- na_range[[i]]
                    if (is.null(nai)) {
                        compatible[i] <- TRUE
                    }
                    else {
                        for (j in seq(2, length(na_range))) {
                            naj <- na_range[[j]]
                            if (is.null(naj)) {
                                compatible[j] <- TRUE
                            }
                            else {
                                if (any(is.element(seq(nai[1], nai[2]), seq(naj[1], naj[2]))) > 0) {
                                    compatible[i] <- TRUE
                                    compatible[j] <- TRUE
                                }
                            }
                        }
                    }
                }
            }
            if (any(!compatible)) {
                stopError_("Incompatible NA ranges.")
            }
            na_range <- range(unlist(na_range))
        }
    }
    dots <- unlist(lapply(dots, function(x) {
        if (is.declared(x)) x <- undeclare(x)
        attributes(x) <- NULL
        return(x)
    }))
    return(declared(
        dots,
        labels = labels,
        na_values = na_values,
        na_range = na_range,
        label = attr(dots[[which(declared)[1]]], "label", exact = TRUE)
    ))
}
`names<-.declared` <- function(x, value) {
    attr(x, "names") <- value
    x
}
`sort.declared` <- function(x, decreasing = FALSE, ...) {
    dots <- list(...)
    callist <- list(x = x, decreasing = decreasing)
    if (is.element("na.last", names(dots))) {
        callist$na.last <-  dots$na.last
    }
    if (is.element("method", names(dots))) {
        callist$method <-  dots$method
    }
    if (is.element("empty.last", names(dots))) {
        callist$empty.last <-  dots$empty.last
    }
    xorder <- do.call("order_declared", callist)
    return(x[xorder])
}
`duplicated.declared` <- function(x, incomparables = FALSE, ...) {
    x <- unclass(undeclare(x))
    NextMethod()
}
`unique.declared` <- function(x, incomparables = FALSE, ...) {
    x[!duplicated(x)]
}
`head.declared` <- function(x, n = 6L, ...) {
    lx <- length(x)
    if (n < 0) {
        n <- lx - abs(n)
    }
    n <- min(n, length(x))
    if (n < 1) {
        return(x[0])
    }
    x[seq(n)]
}
`tail.declared` <- function(x, n = 6L, ...) {
    if (n < 1) {
        n <- 6L
    }
    lx <- length(x)
    n <- min(n, lx)
    x[seq(lx - n + 1, lx)]
}
`na.omit.declared` <- function (object, ...)  {
    attrx <- attributes(object)
    attrx$na_index <- NULL
    object <- unclass(object)
    object <- NextMethod()
    attrx$na.action <- attr(object, "na.action")
    nms <- attrx$names
    if (!is.null(nms) && !is.null(attrx$na.action)) {
        nms <- nms[-attr(object, "na.action")]
        attrx$names <- nms
    }
    attributes(object) <- attrx
    return(object)
}
`na.fail.declared` <- function (object, ...)  {
    object <- unclass(object)
    NextMethod()
}
`na.exclude.declared` <- function (object, ...)  {
    attrx <- attributes(object)
    attrx$na_index <- NULL
    object <- unclass(object)
    object <- NextMethod()
    attrx$na.action <- attr(object, "na.action")
    nms <- attrx$names
    if (!is.null(nms) && !is.null(attrx$na.action)) {
        nms <- nms[-attr(object, "na.action")]
        attrx$names <- nms
    }
    attributes(object) <- attrx
    return(object)
}
`mean.declared` <- function(x, ...) {
    na_index <- attr(x, "na_index")
    if (!is.null(na_index)) {
        x <- x[-na_index]
    }
    x <- unclass(x)
    NextMethod()
}
`median.declared` <- function(x, na.rm = FALSE, ...) {
    na_index <- attr(x, "na_index")
    if (!is.null(na_index)) {
        x <- x[-na_index]
    }
    x <- unclass(x)
    NextMethod()
}
`summary.declared` <- function(object, ...) {
    na_index <- attr(object, "na_index")
    if (!is.null(na_index)) {
        object[na_index] <- NA
    }
    object <- unclass(object)
    NextMethod()
}
`all.equal.declared` <- function(target, current, ...) {
    na_index <- attr(target, "na_index")
    target <- undeclare(target, drop = TRUE)
    if (is.declared(current)) {
        current <- undeclare(current, drop = TRUE)
    }
    allna <- TRUE
    if (!is.null(na_index)) {
        allna <- all.equal(target[na_index], current[na_index])
        target <- target[-na_index]
        current <- current[-na_index]
    }
    allv <- all.equal(target, current)
    if (isTRUE(allv)) {
        if (isTRUE(allna)) {
            return(TRUE)
        }
        return(paste("Declared mising values", tolower(allna)))
    }
    return(allv)
}
`abs.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("abs")(x)
}
`sign.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("sign")(x)
}
`sqrt.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("sqrt")(x)
}
`floor.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("floor")(x)
}
`ceiling.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("ceiling")(x)
}
`trunc.declared` <- function(x, ...) {
    attributes(x) <- NULL
    .Primitive("trunc")(x, ...)
}
`round.declared` <- function(x, digits = 0) {
    attributes(x) <- NULL
    .Primitive("round")(x, digits)
}
`signif.declared` <- function(x, digits = 0) {
    attributes(x) <- NULL
    .Primitive("signif")(x, digits)
}
`exp.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("exp")(x)
}
`log.declared` <- function(x, base = exp(1)) {
    attributes(x) <- NULL
    .Primitive("log")(x, base)
}
`expm1.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("expm1")(x)
}
`log1p.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("log1p")(x)
}
`cos.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("cos")(x)
}
`sin.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("sin")(x)
}
`tan.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("tan")(x)
}
`cospi.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("cospi")(x)
}
`sinpi.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("sinpi")(x)
}
`tanpi.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("tanpi")(x)
}
`acos.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("acos")(x)
}
`asin.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("asin")(x)
}
`atan.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("atan")(x)
}
`lgamma.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("lgamma")(x)
}
`gamma.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("gamma")(x)
}
`digamma.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("digamma")(x)
}
`trigamma.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("trigamma")(x)
}
`cumsum.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("cumsum")(x)
}
`cumprod.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("cumprod")(x)
}
`cummax.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("cummax")(x)
}
`cummin.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("cummin")(x)
}
`+.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            attributes(e2) <- NULL
        }
    }
    .Primitive("+")(e1, e2)
}
`-.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            attributes(e2) <- NULL
        }
    }
    .Primitive("-")(e1, e2)
}
`*.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            attributes(e2) <- NULL
        }
    }
    .Primitive("*")(e1, e2)
}
`/.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            attributes(e2) <- NULL
        }
    }
    .Primitive("/")(e1, e2)
}
`^.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            attributes(e2) <- NULL
        }
    }
    .Primitive("^")(e1, e2)
}
`%%.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            attributes(e2) <- NULL
        }
    }
    .Primitive("%%")(e1, e2)
}
`%/%.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            attributes(e2) <- NULL
        }
    }
    .Primitive("%/%")(e1, e2)
}
`%*%.declared` <- function(x, y) {
    attributes(x) <- NULL
    if (!missing(y)) {
        if (is.declared(y)) {
            attributes(y) <- NULL
        }
    }
    .Primitive("%*%")(x, y)
}
`&.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            attributes(e2) <- NULL
        }
    }
    .Primitive("&")(e1, e2)
}
`|.declared` <- function(e1, e2) {
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            attributes(e2) <- NULL
        }
    }
    .Primitive("|")(e1, e2)
}
`!.declared` <- function(x) {
    attributes(x) <- NULL
    .Primitive("!")(x)
}
`==.declared` <- function(e1, e2) {
    le1 <- attr(e1, "labels", exact = TRUE)
    e1 <- undeclare(e1)
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            e2 <- undeclare(e2)
            attributes(e2) <- NULL
        }
        if (length(e2) == 1 && is.element(e2, names(le1)) && !is.element(e2, e1)) {
            e2 <- le1[names(le1) == e2]
        }
    }
    .Primitive("==")(e1, e2)
}
`!=.declared` <- function(e1, e2) {e1 <- unclass(undeclare(e1))
    le1 <- attr(e1, "labels", exact = TRUE)
    e1 <- undeclare(e1)
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            e2 <- undeclare(e2)
            attributes(e2) <- NULL
        }
        if (length(e2) == 1 && is.element(e2, names(le1)) && !is.element(e2, e1)) {
            e2 <- le1[names(le1) == e2]
        }
    }
    .Primitive("!=")(e1, e2)
}
`<.declared` <- function(e1, e2) {
    e1 <- undeclare(e1)
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            e2 <- undeclare(e2)
            attributes(e2) <- NULL
        }
    }
    .Primitive("<")(e1, e2)
}
`<=.declared` <- function(e1, e2) {
    e1 <- undeclare(e1)
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            e2 <- undeclare(e2)
            attributes(e2) <- NULL
        }
    }
    .Primitive("<=")(e1, e2)
}
`>=.declared` <- function(e1, e2) {
    e1 <- undeclare(e1)
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            e2 <- undeclare(e2)
            attributes(e2) <- NULL
        }
    }
    .Primitive(">=")(e1, e2)
}
`>.declared` <- function(e1, e2) {
    e1 <- undeclare(e1)
    attributes(e1) <- NULL
    if (!missing(e2)) {
        if (is.declared(e2)) {
            e2 <- undeclare(e2)
            attributes(e2) <- NULL
        }
    }
    .Primitive(">")(e1, e2)
}
`Arg.declared` <- function(z) {
    attributes(z) <- NULL
    .Primitive("Arg")(z)
}
`Conj.declared` <- function(z) {
    attributes(z) <- NULL
    .Primitive("Conj")(z)
}
`Im.declared` <- function(z) {
    attributes(z) <- NULL
    .Primitive("Im")(z)
}
`Mod.declared` <- function(z) {
    attributes(z) <- NULL
    .Primitive("Mod")(z)
}
`Re.declared` <- function(z) {
    attributes(z) <- NULL
    .Primitive("Re")(z)
}
