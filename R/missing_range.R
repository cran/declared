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

#' @rdname missing_values
#'
#' @inherit missing_values examples
#'
#' @export
`missing_range` <- function(x) {
  UseMethod("missing_range")
}
#' @export
`missing_range.default` <- function(x) {
  NULL
}
#' @export
`missing_range.haven_labelled_spss` <- function(x) {
  attr(x, "na_range", exact = TRUE)
}
#' @export
`missing_range.declared` <- function(x) {
  attr(x, "na_range", exact = TRUE)
}
#' @export
`missing_range.data.frame` <- function(x) {
  lapply(x, missing_range)
}
#' @rdname missing_values
#'
#' @export
`missing_range<-` <- function(x, value) {
  UseMethod("missing_range<-")
}
#' @export
`missing_range<-.default` <- function(x, value) {
  x
}
#' @export
`missing_range<-.declared` <- function(x, value) {
  if (!is.null(value) && (length(value) != 2 || !is.numeric(value))) {
    stopError_("`value` should be a numeric vector of length 2.")
  }
  value <- sort(value)
  declared(undeclare(x),
           labels = attr(x, "labels", exact = TRUE),
           na_values = attr(x, "na_values", exact = TRUE),
           na_range = value,
           label = attr(x, "label", exact = TRUE)
  )
}
