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

#' @rdname declared
#'
#' @export
`as.declared` <- function(x, ...) {
  UseMethod("as.declared")
}
#' @export
`as.declared.default` <- function(x, ...) {
  dots <- list(...)
  interactive <- isTRUE(dots$interactive)
  if (interactive) {
    msg <- "There is no automatic class method conversion for this type of"
    if (!is.null(dots$vname_)) {
      msg <- paste0(dots$vname_, ": ", msg, " variable.")
    }
    else {
      msg <- paste(msg, "object.")
    }
    message(msg)
  }
  return(x)
}
#' @export
`as.declared.haven_labelled` <- function(x, ...) {
  dots <- list(...)
  na_values <- attr(x, "na_values")
  na_range <- attr(x, "na_range")
  labels <- attr(x, "labels", exact = TRUE)
  label <- attr(x, "label", exact = TRUE)
  format_spss <- attr(x, "format.spss") 
  if (!inherits(x, "haven_labelled_spss")) {
    tagged <- hasTag_(x)
    attributes(x) <- NULL
    if (any(tagged)) {
      x[tagged] <- getTag_(x[tagged])
    }
    if (!is.null(labels)) {
      nms <- names(labels)
      tagged <- hasTag_(labels)
      if (any(tagged)) {
        labels[tagged] <- getTag_(labels[tagged])
        na_values <- sort(unname(labels[tagged]))
      }
      labels <- coerceMode_(labels)
      names(labels) <- nms
    }
    misvals <- na_values
  }
  else {
    misvals <- all_missing_values(unclass(x))
  }
  attributes(x) <- NULL
  attr(x, "xchar") <- is.character(x)
  missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]
  attr(x, "na_values") <- na_values
  attr(x, "na_range") <- na_range
  attr(x, "labels") <- labels
  attr(x, "label") <- label
  attr(x, "format.spss") <- format_spss
  return(x)
}
#' @export
`as.declared.factor` <- function(x, ...) {
  return(declared(x, ... = ...))
}
#' @export
`as.declared.data.frame` <- function(x, ..., interactive = FALSE) {
  if (isFALSE(interactive)) {
    x[] <- lapply(x, as.declared, interactive = FALSE, ... = ...)
  }
  else {
    nms <- names(x)
    for (i in seq(length(nms))) {
      x[[i]] <- as.declared(x[[i]], vname_ = nms[i], ... = ...)
    }
  }
  return(x)
}
