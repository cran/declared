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

#' @rdname weighted
#' @order 4
#' @details
#' The function `wmode()` returns the weighted mode of a variable. Unlike the
#' other functions where the prefix `w` signals a weighted version of the
#' base function with the same name, this has nothing to do with the base
#' function `mode()` which refers to the storage mode or type of an R object.
#' @export
`wmode` <- function (x, wt = NULL) {

    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    if (
        is.null (x) || !is.atomic (x) || !(
            is.numeric (x) || is.complex (x) || is.logical (x)
        )
    ) {
        warning ("'x' should be a numerical / logical vector: returning NA")
        return (NA_real_)
    }

    if (inherits (x, "declared")) {
        x <- sanitize_na_index_ (x)
        na_index <- attr (x, "na_index")
        if (length (na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index] # if wt is NULL, the result is still NULL
        }
        attributes (x) <- NULL
    }

    if (
        !is.null (wt) && !(
            is.atomic (wt) && all (is.finite (na.omit (wt)))
        )
    ) {
        stopError_ ("'wt' should be an atomic vector with finite values.")
    }

    tbl <- wtable (x, wt = wt)
    wm <- which.max (tbl)

    fmode <- names (tbl[wm])
    if (possibleNumeric_ (fmode)) {
        fmode <- asNumeric_ (fmode)
    }

    if (length (tbl[tbl == max (tbl)]) > 1) {
        message ("Multiple modes detected, only the first is returned.")
    }

    return (fmode)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_mode` <- function (...) {
    wmode(...)
}
