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

#' @rdname weighted
#' @param probs Numeric vector of probabilities with values in \[0,1\]
#' @export
`w_quantile` <- function(
    x, wt = NULL, probs = seq(0, 1, 0.25), na.rm = TRUE, ...
) {
    metacall <- as.list(match.call())
    if (inherits(x, "haven_labelled")) {
        x <- as.declared(x)
    }
    if (!(is.atomic(x) && is.numeric(x))) {
        stopError_("'x' should be an atomic numerical vector.")
    }
    if (inherits(x, "declared")) {
        na_index <- attr(x, "na_index")
        if (length(na_index)) {
            x <- x[-na_index]
            wt <- wt[-na_index] 
        }
        attributes(x) <- NULL
    }
    if (is.null(wt)) {
        qs <- quantile(x, probs = probs, na.rm = na.rm, ... = ...)
        class(qs) <- c("fobject", class(qs))
        return(qs)
    }
    if (!(is.atomic(wt) && all(is.finite(na.omit(wt))))) {
        stopError_("'wt' should be an atomic vector with finite values.")
    }
    if (length(x) != length(wt)) {
        stopError_("Lengths of 'x' and 'wt' differ.")
    }
    ok <- !is.na(x + wt)
    if (na.rm) {
        x <- x[ok]
        wt <- wt[ok]
    }
    else if (any(!ok)) {
        stopError_("Missing values and NaN's not allowed if `na.rm' is FALSE")
    }
    if (any((p.ok <- !is.na(probs)) & (probs < 0 | probs > 1))) {
        stopError_("probs outside [0,1]")
    }
    if (na.p <- any(!p.ok)) {
        o.pr <- probs
        probs <- probs[p.ok]
        probs <- pmax(0, pmin(1, probs))
    }
    sumwt <- sum(wt)
    wt <- cumsum(tapply(wt, x, sum, na.rm = TRUE))
    x <- sort(unique(x))
    index <- 1 + (sumwt - 1) * probs
    left <- pmax(floor(index), 1)
    right <- pmin(left + 1, sumwt)
    index <- index%%1
    both <- approx(
        wt, x, xout = c(left, right),
        method = "constant", rule = 2, f = 1
    )$y
    lp <- length(probs)
    qs <- (1 - index) * both[seq(1, lp)] + index * both[-seq(1, lp)]
    qs <- coerceMode_(qs)
    names(qs) <- paste0(format(100 * probs, trim = TRUE), "%")
    class(qs) <- c("fobject", class(qs))
    return(qs)
}
