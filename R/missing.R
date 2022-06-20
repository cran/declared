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

`missing_values` <- function(x) {
    UseMethod("missing_values")
}
`missing_values.default` <- function(x) {
    NULL
}
`missing_values.haven_labelled_spss` <- function(x) {
  attr(x, "na_values", exact = TRUE)
}
`missing_values.declared` <- function(x) {
    attr(x, "na_values", exact = TRUE)
}
`missing_values.data.frame` <- function(x) {
  lapply(x, missing_values)
}
`missing_values<-` <- function(x, value) {
  UseMethod("missing_values<-")
}
`missing_values<-.default` <- function(x, value) {
    x
}
`missing_values<-.declared` <- function(x, value) {
    declared(undeclare(x),
        labels = attr(x, "labels", exact = TRUE),
        na_values = value,
        na_range = attr(x, "na_range", exact = TRUE),
        label = attr(x, "label", exact = TRUE)
    )
}
`missing_range` <- function(x) {
  UseMethod("missing_range")
}
`missing_range.default` <- function(x) {
    NULL
}
`missing_range.haven_labelled_spss` <- function(x) {
    attr(x, "na_range", exact = TRUE)
}
`missing_range.declared` <- function(x) {
    attr(x, "na_range", exact = TRUE)
}
`missing_range.data.frame` <- function(x) {
    lapply(x, missing_range)
}
`missing_range<-` <- function(x, value) {
    UseMethod("missing_range<-")
}
`missing_range<-.default` <- function(x, value) {
    x
}
`missing_range<-.declared` <- function(x, value) {
    if (length(value) != 2 || !is.numeric(value)) {
        cat("\n")
        stop("`value` should be a numeric vector of length 2.\n\n", call. = FALSE)
    }
    value <- sort(value)
    declared(undeclare(x),
        labels = attr(x, "labels", exact = TRUE),
        na_values = attr(x, "na_values", exact = TRUE),
        na_range = value,
        label = attr(x, "label", exact = TRUE)
    )
}
`missingValues` <- function(x) {
    mv <- rep(NA, length(x))
    if (is.declared(x)) {
        misvals <- attr(x, "na_index")
        mv[as.numeric(names(misvals))] <- misvals
    }
    return(mv)
}
`missingValues<-` <- function(x, value) {
    class(x) <- setdiff(class(x), "declared")
    other_classes <- setdiff(class(x), c("integer", "double", "character", "numeric", "complex", "haven_labelled", "haven_labelled_spss", "vctrs_vctr"))
    notna <- !is.na(value)
    x[notna] <- NA
    if (!all(is.na(x))) {
        x <- coerceMode_(x)
    }
    if (any(notna)) {
        na_index <- which(notna)
        names(na_index) <- value[notna]
        attr(x, "na_index") <- na_index
    }
    structure(x, class = c("declared", other_classes, class(x)))
}
