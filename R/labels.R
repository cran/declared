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

`value_labels` <- function(x, prefixed = FALSE) {
    UseMethod("value_labels")
}
`value_labels.default` <- function(x, prefixed = FALSE) {
    NULL
}
`value_labels.haven_labelled_spss` <- function(x, prefixed = FALSE) {
    labels <- attr(x, "labels", exact = TRUE)
    if (prefixed)
        names(labels) <- paste0("[", labels, "] ", names(labels))
    labels
}
`value_labels.declared` <- function(x, prefixed = FALSE) {
    labels <- attr(x, "labels", exact = TRUE)
    if (prefixed) {
        names(labels) <- paste0("[", labels, "] ", names(labels))
    }
    return(labels)
}
`value_labels.data.frame` <- function(x, prefixed = FALSE) {
    lapply(x, value_labels, prefixed = prefixed)
}
`value_labels<-` <- function(x, value) {
  UseMethod("value_labels<-")
}
`value_labels<-.default` <- function(x, value) {
    x
}
`value_labels<-.declared` <- function(x, value) {
    attr(x, "labels") <- value
    return(x)
}
`variable_label` <- function(x) {
    UseMethod("variable_label")
}
`variable_label.default` <- function(x) {
    attr(x, "label", exact = TRUE)
}
`variable_label.haven_labelled_spss` <- function(x) {
    attr(x, "label", exact = TRUE)
}
`variable_label.declared` <- function(x) {
    attr(x, "label", exact = TRUE)
}
`variable_label.data.frame` <- function(x) {
    lapply(x, variable_label)
}
`variable_label<-` <- function(x, value) {
  UseMethod("variable_label<-")
}
`variable_label<-.default` <- function(x, value) {
    x
}
`variable_label<-.declared` <- function(x, value) {
    if (!is.null(value) && length(value) > 1) {
        stopError_("`value` should be a single character string or NULL.")
    }
    if (is.null(value)) {
        attr(x, "label") <- NULL
    }
    else {
        attr(x, "label") <- as.character(value)
    }
    return(x)
}
