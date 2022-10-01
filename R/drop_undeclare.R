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

#' @name drop_undeclare
#' @title Drop information / undeclare labelled objects
#' @description
#' A function to obtain a version of the object with all information about
#' declared missing values, dropped
#' @details
#' #' The function `undeclare()` replaces the NA entries into their original
#' numeric values, and drops all attributes related to missing values:
#' `na_values`, `na_range` and `na_index`, and it preserves the labels referring
#' to the missing values.
#'
#' The result can be a regular vector (dropping all attributes, including the
#' class "declared") by activating the argument `drop`.
#'
#' Function `drop_na()` transforms the declared missing values in regular empty
#' NAs, and the labels referring to the missing values are deleted by default.
#'
#' Function `drop()` deletes all attributes.
#' @family labelling functions
#' @return A declared labelled object.
#' @examples
#' x <- declared(
#'     c(-2, 1:5, -1),
#'     labels = c("Good" = 1, "Bad" = 5, "DK" = -1),
#'     na_values = c(-1, -2),
#'     label = "Test variable"
#' )
#'
#' x
#'
#' undeclare(x)
#'
#' drop_na(x)
#'
#' drop(x)
#'
#' undeclare(x, drop = TRUE)
#'
#' # similar to:
#' drop(undeclare(x))
#' @param x A labelled object with declared missing values
#' @param drop Logical, drop all attributes
#' @param drop_labels Logical, drop the labels for the declared missing values
#' @param ... Other internal arguments
#' @export
`undeclare` <- function(x, drop = FALSE, ...) {
  UseMethod("undeclare")
}
#' @export
`undeclare.default` <- function(x, drop = FALSE, ...) {
  return(x)
}
#' @export
`undeclare.declared` <- function(x, drop = FALSE, ...) {
  na_index <- attr(x, "na_index")
  attrx <- attributes(x)
  attributes(x) <- NULL 
  if (!is.null(na_index)) {
    x[na_index] <- names(na_index)
    x <- coerceMode_(x)
  }
  attrx$na_index <- NULL
  attrx$na_values <- NULL
  attrx$na_range <- NULL
  if (isFALSE(drop)) {
    attributes(x) <- attrx
  }
  return(x)
}
#' @export
`undeclare.data.frame` <- function(x, drop = FALSE, ...) {
  declared <- vapply(x, is.declared, logical(1))
  x[declared] <- lapply(x[declared], undeclare, drop = drop)
  return(x)
}
#' @rdname drop_undeclare
#' @export
`drop_na` <- function(x, drop_labels = TRUE) {
  UseMethod("drop_na")
}
#' @export
`drop_na.default` <- function(x, drop_labels = TRUE) {
  x
}
#' @export
`drop_na.haven_labelled_spss` <- function(x, drop_labels = TRUE) {
  attrx <- attributes(x)
  x[is.element(x, attrx$na_values)] <- NA
  if (isTRUE(drop_labels)) {
    attrx$labels <- attrx$labels[!is.element(attrx$labels, attrx$na_values)]
  }
  attrx$na_index <- NULL
  attrx$na_values <- NULL
  attrx$na_range <- NULL
  attributes(x) <- attrx
  return(x)
}
#' @export
`drop_na.declared` <- function(x, drop_labels = TRUE) {
  attrx <- attributes(x)
  if (isTRUE(drop_labels)) {
    attrx$labels <- attrx$labels[!is.element(attrx$labels, attrx$na_values)]
  }
  attrx$na_index <- NULL
  attrx$na_values <- NULL
  attrx$na_range <- NULL
  attributes(x) <- attrx
  return(x)
}
#' @export
`drop_na.data.frame` <- function(x, drop_labels = TRUE) {
  declared <- vapply(x, is.declared, logical(1))
  x[declared] <- lapply(x[declared], drop_na, drop_labels = drop_labels)
  return(x)
}
