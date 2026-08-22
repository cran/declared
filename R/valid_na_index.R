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

#' Validate the declared missing values index
#'
#' Check whether every position stored in the `na_index` attribute identifies a
#' genuine missing value in the vector. This can detect when external code has
#' changed a vector without refreshing its declared missing values metadata.
#'
#' The check examines only the positions in `na_index` and stops at the first
#' invalid position. A missing or empty `na_index` attribute is valid.
#'
#' @param x An atomic vector, possibly containing an `na_index` attribute.
#' @return A logical value. `TRUE` when every indexed value is a genuine `NA`;
#'   otherwise `FALSE`.
#' @examples
#' x <- declared(c(1, -1, 3), na_values = -1)
#'
#' valid_na_index(x)
#'
#' attr(x, "na_index") <- 1
#' valid_na_index(x)
#' @export
`valid_na_index` <- function (x) {
  if (!is.atomic (x)) {
    return (FALSE)
  }

  index <- attr (x, "na_index")

  if (is.null (index)) {
    return (TRUE)
  }

  return (.Call (
    "_allIndexedNA", x, index, PACKAGE = "declared"
  ))
}


`sanitize_na_index_` <- function (x) {
  if (is.atomic (x) && !valid_na_index (x)) {
    # The positional map cannot be repaired selectively once it is stale.
    attr (x, "na_index") <- NULL
    attr (x, "na_values") <- NULL
    attr (x, "na_range") <- NULL
  }

  return (x)
}
