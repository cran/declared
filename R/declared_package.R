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

#' @docType package
#'
#' @name declared_package
#'
#' @title Functions for Declared Missing Values
#'
#' @description A set of functions to declare labels and missing values, coupled with
#' associated functions to create (weighted) tables of frequencies and various
#' other summary measures.
#' Some of the base functions are rewritten to make use of the specific information
#' about the missing values, most importantly to distinguish between empty and
#' declared missing values.
#' Many functions have a similar functionality with the corresponding ones
#' from packages "haven" and "labelled". The aim is to ensure as much compatibility
#' as possible with these packages, while offering an alternative in the objects of
#' class "declared".
#'
#' @author Adrian Dusa
#'
#' Maintainer: Adrian Dusa (dusa.adrian@unibuc.ro)
#'
#' @details
#' \tabular{ll}{
#'   Package: \tab declared\cr
#'   Type: \tab Package\cr
#'   Version: \tab 0.18\cr
#'   Date: \tab 2022-09-27\cr
#'   License: \tab GPL-v3\cr
#' }
#'
#' @importFrom stats na.omit na.fail na.exclude median
#' @importFrom utils head tail capture.output
#' @useDynLib declared, .registration = TRUE
#'
NULL
