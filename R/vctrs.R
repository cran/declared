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

# Dynamically exported, see onLoad.R
# using eval(parse()) to avoid the huge dependency tree of vctrs, haven,
# labelled and pillar; these functions will be registered when or if the
# package vctrs is loaded

`vec_ptype_abbr.declared` <- function (x, ...) {
    command <- "vctrs::vec_ptype_abbr(vctrs::vec_data(unclass (undeclare (x))))"
    return (
        paste0 (eval (parse (text = command)), "+lbl")
    )
}

`vec_ptype_full.declared` <- function (x, ...) {
    command <- "vctrs::vec_ptype_full(vctrs::vec_data(unclass (undeclare (x))))"
    return (
        paste0 (
            "declared<",
            eval (parse (text = command)),
            ">"
        )
    )
}

`vec_proxy.declared` <- function (x, ...) {
    return (undeclare (x, drop = TRUE))
}

`vec_restore.declared` <- function(x, to, ...) {
    to <- sanitize_na_index_ (to)
    attrs <- attributes(to)
    todate <- isTRUE(attrs$date)

    misvals <- all_missing_values (
        x,
        attrs$na_values,
        attrs$na_range,
        attrs$labels
    )

    na_index <- which (is.element (x, misvals))

    if (length(na_index) > 0) {
        declared_nas <- x[na_index]

        if (todate) {
            declared_nas <- as.numeric (declared_nas)
        }

        x[na_index] <- NA
        names (na_index) <- declared_nas
    } else {
        na_index <- NULL
    }

    attrs$na_index <- na_index
    if (possibleNumeric_ (x)) {
        x <- as.numeric (x)
        if (inherits (to, "integer")) {
            x <- as.integer (x)
        }
    }
    attributes(x) <- attrs
    return (x)
}

# `vec_ptype2.declared` <- function (x, y, ...) {
#     command <- paste (
#         "vctrs::vec_ptype2(unclass (undeclare (x)),",
#         "vctrs::vec_data(unclass (undeclare (y))), ...)"
#     )
#     eval (parse (text = command))
# }
