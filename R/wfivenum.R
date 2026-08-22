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
#' @order 9
#' @export
`wfivenum` <- function (
    x, wt = NULL, na.rm = FALSE
) {
    metacall <- as.list (match.call ())

    if (inherits (x, "haven_labelled")) {
        x <- as.declared (x)
    }

    if (
        is.null (x) || !(
            is.atomic (x) && is.numeric (x)
        )
    ) {
        stopError_ ("'x' should be an atomic numerical vector.")
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

    if (is.null(wt)) {
        fvn <- fivenum (x, na.rm = na.rm)
        names (fvn) <- c ("Min", "Q1", "Q2", "Q3", "Max")
        class (fvn) <- c ("fobject", class (fvn))
        return (fvn)
    }

    if (
        !is.null (wt) && !(
            is.atomic (wt) && all (is.finite (na.omit (wt)))
        )
    ) {
        stopError_ ("'wt' should be an atomic vector with finite values.")
    }

    if (length (x) != length (wt)) {
        stopError_ ("Lengths of 'x' and 'wt' differ.")
    }

    ok <- !is.na (x + wt)

    if (na.rm) {
        x <- x[ok]
        wt <- wt[ok]
    }
    else if (any (!ok)) {
        stopError_ ("Missing values and NaN's not allowed if `na.rm' is FALSE")
    }

    sumwt <- sum (wt)

    if (any (wt < 0) || sumwt == 0) {
        stopError_ ("'wt' must be non-negative and not all zero")
    }

    fvn <- fivenum (x * wt, na.rm = na.rm)
    names (fvn) <- c ("Min", "Q1", "Q2", "Q3", "Max")
    class (fvn) <- c ("fobject", class (fvn))
    return (fvn)
}

#' @rdname declared_internal
#' @keywords internal
#' @export
`w_fivenum` <- function (...) {
    wfivenum(...)
}
