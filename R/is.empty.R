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

#' @title
#' Test the presence of empty (undeclared) missing values
#'
#' @description
#' Functions that indicate which elements are empty `NA` missing values, in
#' contrast to declared missing values.
#'
#' @details
#' All missing values, declared or undeclared, as stored as regular `NA`
#' values, therefore the base function `is_na()` does not differentiate
#' between them.
#'
#' These functions are specifically adapted to objects of class `"declared"`,
#' to return a truth value only for those elements that are completely missing
#' with no reason.
#'
#' @return
#' A logical vector.
#'
#' @examples
#'
#' x <- declared(
#'     c(1:2, -91),
#'     labels = c(Good = 1, Bad = 2, Missing = -91),
#'     na_values = -91
#' )
#'
#' x
#'
#' is.empty(x) # FALSE FALSE FALSE
#'
#' x <- c(x, NA)
#'
#' is.empty(x) # FALSE FALSE FALSE  TRUE
#'
#' @param x A vector
#'
#' @export
`is.empty` <- function(x) {
  if (!is.atomic(x)) {
    stopError_("'x' should be an atomic vector.")
  }
  empty <- is.na(x)
  if (is.declared(x)) {
    na_index <- attr(x, "na_index")
    if (!is.null(na_index)) {
      empty[na_index] <- FALSE
    }
  }
  return(empty)
}
