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

#' @title Coerce to haven / labelled objects
#' @description
#' Convert declared labelled objects to haven labelled objects
#' @details
#' This is a function that reverses the process of `as.declared()`, making
#' a round trip between `declared` and `haven_labelled_spss` classes.
#'
#' @return A labelled vector of class "haven_labelled_spss".
#' @examples
#'
#' x <- declared(
#'     c(1:5, -1),
#'     labels = c(Good = 1, Bad = 5, DK = -1),
#'     na_values = -1
#' )
#'
#' x
#'
#' as.haven(x)
#' @param x A declared labelled vector
#' @param ... Other arguments used by various methods
#' @export
#' @name as.haven
#' @export
`as.haven` <- function(x, ...) {
    UseMethod("as.haven")
}
#' @export
`as.haven.default` <- function(x, ...) {
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
#' @export
`as.haven.declared` <- function(x, ...) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)
    attributes(x) <- NULL 
    if (possibleNumeric_(x) || all(is.na(x))) {
        x <- as.double(x)
        attr(x, "class") <- "double"
    }
    if (!is.null(na_index)) {
        all_na_values <- names(na_index)
        if (is.numeric(x)) {
            all_na_values <- as.numeric(all_na_values)
        }
        x[na_index] <- all_na_values
    }
    na_values <- attrx$na_values
    pN_na_values <- possibleNumeric_(na_values)
    labels <- attrx$labels
    pN_labels <- possibleNumeric_(labels)
    all_num <- is.numeric(x)
    if (!is.null(na_values)) {
        all_num <- all_num & pN_na_values
    }
    if (!is.null(labels)) {
        all_num <- all_num & pN_labels
    }
    if (all_num) {
        if (!is.null(na_values)) {
            na_values <- as.numeric(na_values)
            names(na_values) <- names(attrx$na_values)
            attrx$na_values <- na_values
        }
        if (!is.null(labels)) {
            labels <- as.numeric(labels)
            names(labels) <- names(attrx$labels)
            attrx$labels <- labels
        }
    }
    else {
        x <- as.character(x)
        if (!is.null(na_values)) {
            na_values <- as.character(na_values)
            names(na_values) <- names(attrx$na_values)
            attrx$na_values <- na_values
        }
        if (!is.null(labels)) {
            labels <- as.character(labels)
            names(labels) <- names(attrx$labels)
            attrx$labels <- labels
        }
    }
    attrx$na_index <- NULL
    attrx$class <- unique(c(
            "haven_labelled_spss", "haven_labelled", "vctrs_vctr",
            setdiff(attrx$class, c("declared", "double", "integer", "character")),
            class(x)
    ))
    attributes(x) <- attrx
    return(x)
}
#' @export
`as.haven.data.frame` <- function(x, ..., only_declared = TRUE, interactive = FALSE) {
    if (only_declared) {
        xdeclared <- vapply(x, is.declared, logical(1))
        if (isFALSE(interactive)) {
            x[xdeclared] <- lapply(x[xdeclared], as.haven, interactive = FALSE, ... = ...)
        }
        else {
            nms <- names(x)[xdeclared]
            for (i in seq(length(nms))) {
                x[[nms[i]]] <- as.haven(x[[nms[i]]], vname_ = nms[i], ... = ...)
            }
        }
    } else {
        if (isFALSE(interactive)) {
            x[] <- lapply(x, as.haven, interactive = FALSE, ... = ...)
        }
        else {
            nms <- names(x)
            for (i in seq(length(nms))) {
                x[[i]] <- as.haven(x[[i]], vname_ = nms[i], ... = ...)
            }
        }
    }
    class(x) <- c("tbl", "tbl_df", "data.frame")
    return(x)
}
`as_factor.declared` <- function(
    x, levels = c("default", "labels", "values", "both"), ordered = FALSE, ...
) {
  haven::as_factor(
    as.haven(x),
    levels = levels,
    ordered = ordered,
    ... = ...
  )
}
`zap_labels.declared` <- function(x) {
    attr(x, "labels") <- NULL
    attr(x, "na_index") <- NULL
    attr(x, "na_values") <- NULL
    attr(x, "na_range") <- NULL
    class(x) <- NULL
    return(x)
}
`zap_missing.declared` <- function(x) {
    attr(x, "na_index") <- NULL
    attr(x, "na_values") <- NULL
    attr(x, "na_range") <- NULL
    return(x)
}
