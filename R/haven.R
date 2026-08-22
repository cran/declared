# Copyright (c) 2022 - 2026, Adrian Dusa
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
#     * The names of its contributors may NOT be used to endorse or promote
#       products derived from this software without specific prior written
#       permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
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
# See https://stackoverflow.com/questions/18512528/
#     how-to-export-s3-method-so-it-is-available-in-namespace
`as.haven` <- function (x, ...) {
    UseMethod ("as.haven")
}

#' @export
`as.haven.default` <- function (x, ...) {

    dots <- list (...)
    interactive <- !isFALSE(dots$interactive)

    if (interactive) {
        msg <- "There is no automatic class method conversion for this type of"
        if (!is.null (dots$vname_)) {
            msg <- paste0 (dots$vname_, ": ", msg, " variable.")
        }
        else {
            msg <- paste (msg, "object.")
        }
        message (msg)
    }

    return (x)
}

#' @export
`as.haven.haven_labelled` <- function (x, ...) {
    # do nothing
    return (x)
}

#' @export
`as.haven.haven_labelled_spss` <- function (x, ...) {
    # do nothing
    return (x)
}



#' @export
`as.haven.declared` <- function (x, ...) {
    x <- sanitize_na_index_ (x)
    attrx <- attributes (x)
    attrx$class <- c ("haven_labelled", "vctrs_vctr")

    declared_is_integer <- is.integer (x)
    xdate <- inherits(x, "Date")

    # this is necessary to replace those values
    # (because of the "[<-.declared" method)
    attributes (x) <- NULL # or x <- unclass (x), but I find this cleaner

    spss <- TRUE
    if (possibleNumeric_ (x) || all (is.na (x))) { # this excludes dates
        x <- as.double (x)
        attr (x, "class") <- "double"
    }

    na_index <- attrx$na_index
    has_na_index <- !is.null (na_index)

    na_values <- attrx$na_values
    has_na_values <- !is.null (na_values)

    na_range <- attrx$na_range
    has_na_range <- !is.null (na_range)
    pN_na_range <- possibleNumeric_ (na_range)

    labels <- attrx$labels
    has_labels <- !is.null(labels)

    tagged_na_values <- NULL
    pN_na_values <- possibleNumeric_ (na_values)

    if (has_na_values && pN_na_values) {
        na_values <- asNumeric_ (na_values)
    }

    pN_labels <- FALSE
    if (has_labels) {
        pN_labels <- TRUE
        if (length (setdiff (labels, na_values)) > 0) {
            pN_labels <- possibleNumeric_ (setdiff (labels, na_values))
            if (!pN_labels) {
                x <- as.character (x)
            }
        }
    }


    if (has_na_index) {
        na_index_values <- names (na_index)

        if (pN_na_values | pN_na_range) {
            na_index_values <- asNumeric_ (na_index_values)
        } else if (is.numeric (x)) {
            if (length (setdiff (na_values, c (letters, LETTERS))) > 0) {
                stopError_ ("Tagged NAs can only be created for single letter declared missing values.")
            }

            spss <- FALSE
            attrx$na_values <- NULL
            tagged_na_values <- makeTag_ (na_values)
            pN_na_values <- TRUE # tagged_na_values are now numeric
            na_index_values <- tagged_na_values[match(na_index_values, na_values)]
            attrx$class <- setdiff(attrx$class, "integer")
        }

        x[na_index] <- na_index_values
    }

    if (
        xdate | is.numeric (x) &
        (pN_labels | !has_labels) &
        (pN_na_values | is.null(na_values))
    ) {

        if (has_labels) {
            copy_labels <- numeric (length (labels))
            not_missing <- !is.element(labels, na_values)

            copy_labels[not_missing] <- asNumeric_ (labels[not_missing])

            if (has_na_values) {
                labels_missing <- setdiff(match(labels, na_values), NA)

                if (is.null(tagged_na_values)) {
                    copy_labels[!not_missing] <- asNumeric_ (na_values[labels_missing])
                }
                else {
                    copy_labels[!not_missing] <- tagged_na_values[labels_missing]
                }
            }

            names (copy_labels) <- names (attrx$labels)
            attrx$labels <- copy_labels
        }

        if (has_na_values & spss) {
            na_values <- as.numeric (na_values)
            names (na_values) <- names (attrx$na_values)
            attrx$na_values <- na_values
        }
    } else {
        x <- as.character (x)

        if (!is.null (na_values)) {
            na_values <- as.character (na_values)
            names (na_values) <- names (attrx$na_values)
            attrx$na_values <- na_values
        }

        if (!is.null (labels)) {
            labels <- as.character (labels)
            names (labels) <- names (attrx$labels)
            attrx$labels <- labels
        }
    }

    attrx$na_index <- NULL

    if (spss) { # deactivated above if tagged NAs
        attrx$class <- c ("haven_labelled_spss", attrx$class)

        if (declared_is_integer) {
            x <- as.integer (x)

            if (has_na_values) {
                # class (attrx$na_values) <- "integer"
                attrx$na_values <- as.integer (attrx$na_values)
            }

            if (has_labels) {
                nms <- names (attrx$labels)
                attrx$labels <- as.integer (attrx$labels)
                names (attrx$labels) <- nms
            }

            if (has_na_range && all(attrx$na_range > -Inf) && all(attrx$na_range < Inf)) {
                attrx$na_range <- as.integer (attrx$na_range)
            }
        }
    }

    attrx$class <- c (attrx$class, class (x))
    attrx$date <- NULL
    attributes (x) <- attrx
    return (x)
}

#' @export
`as.haven.data.frame` <- function (
    x, ..., only_declared = TRUE, interactive = FALSE
) {
    if (only_declared) {
        xdeclared <- vapply (x, is.declared, logical (1))
        if (isFALSE (interactive)) {
            x[xdeclared] <- lapply (
                x[xdeclared], as.haven, interactive = FALSE, ... = ...
            )
        }
        else {
            nms <- names (x)[xdeclared]
            for (i in seq (length (nms))) {
                x[[nms[i]]] <- as.haven (
                    x[[nms[i]]], vname_ = nms[i], ... = ...
                )
            }
        }
    } else {
        if (isFALSE (interactive)) {
            x[] <- lapply (x, as.haven, interactive = FALSE, ... = ...)
        }
        else {
            nms <- names (x)
            for (i in seq (length (nms))) {
                x[[i]] <- as.haven (x[[i]], vname_ = nms[i], ... = ...)
            }
        }
    }

    class (x) <- c ("tbl", "tbl_df", "data.frame")
    return (x)
}





# Dynamically exported, see onLoad.R
# using eval (parse ()) to avoid the huge dependency tree of vctrs, haven,
# labelled and pillar these functions will be registered when or if the package
# haven is loaded

`as_factor.declared` <- function (
    x, levels = c ("default", "labels", "values", "both"), ordered = FALSE, ...
) {
    eval(parse(text = paste(
        "haven::as_factor (",
        "    as.haven (x),",
        "    levels = levels,",
        "    ordered = ordered,",
        "    ... = ...",
        ")"
    )))
}

`zap_labels.declared` <- function (x) {
    attr (x, "labels") <- NULL
    attr (x, "na_index") <- NULL
    attr (x, "na_values") <- NULL
    attr (x, "na_range") <- NULL
    attr (x, "date") <- NULL
    class (x) <- NULL

    return (x)
}

`zap_missing.declared` <- function (x) {
    attr (x, "na_index") <- NULL
    attr (x, "na_values") <- NULL
    attr (x, "na_range") <- NULL
    attr (x, "date") <- NULL

    return (x)
}
