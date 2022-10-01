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
#' Get / Declare missing values
#'
#' @description
#' Functions to extract information about the declared missing values, or to
#' declare such values if they are present in the data.
#'
#' @return
#'
#' \code{missing_values()} will return a vector of one or more values.
#'
#' \code{missing_range()} will return a numeric vector of length 2.
#'
#' @examples
#' x <- declared(c(-2, 1:5, -1),
#'     labels = c(Good = 1, Bad = 5, DK = -1, NotApplicable = -2),
#'     na_values = c(-1, -2)
#' )
#' x
#'
#' missing_values(x)
#'
#' missing_range(x) <- c(-10, -7)
#'
#' missing_range(x)
#'
#' @param x A vector.
#'
#' @param value Any vector of values that should be declared as missing
#' (for `missing_values`) or a numeric vector of length two giving the
#' (inclusive) extents of the range of missing values (for `missing_range`).
#'
#' @export
`missing_values` <- function(x) {
  UseMethod("missing_values")
}
#' @export
`missing_values.default` <- function(x) {
  NULL
}
#' @export
`missing_values.haven_labelled_spss` <- function(x) {
  attr(x, "na_values", exact = TRUE)
}
#' @export
`missing_values.declared` <- function(x) {
  attr(x, "na_values", exact = TRUE)
}
#' @export
`missing_values.data.frame` <- function(x) {
  lapply(x, missing_values)
}
#' @rdname missing_values
#'
#' @export
`missing_values<-` <- function(x, value) {
  UseMethod("missing_values<-")
}
#' @export
`missing_values<-.default` <- function(x, value) {
  x
}
#' @export
`missing_values<-.declared` <- function(x, value) {
  declared(undeclare(x),
           labels = attr(x, "labels", exact = TRUE),
           na_values = value,
           na_range = attr(x, "na_range", exact = TRUE),
           label = attr(x, "label", exact = TRUE)
  )
}
