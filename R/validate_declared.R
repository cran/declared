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

`validate_declared` <- function(x = double(), labels = NULL, label = NULL,
                                na_values = NULL, na_range = NULL, ...) {
  if (!is.numeric(x) && !is.character(x) && !all(is.na(x))) {
    stopError_("`x` must be a numeric or a character vector.")
  }
  if (!is.null(labels)) {
    if (is.null(names(labels))) {
      stopError_("`labels` must have names.")
    }
    if (any(duplicated(stats::na.omit(labels)))) {
      stopError_("`labels` must be unique.")
    }
  }
  if (
    !is.null(label) &&
    (!is.atomic(label) || !is.character(label) || length(label) != 1)
  ) {
    stopError_("`label` must be a character vector of length one.")
  }
  if (!is.null(na_values)) {
    if (any(is.na(na_values))) {
      stopError_("`na_values` should not contain NA values.")
    }
  }
  if (!is.null(na_range)) {
    type_ok <-  all(is.na(x)) ||
      (is.character(x) && is.character(na_range)) ||
      (is.numeric(x) && is.numeric(na_range))
    if (!type_ok || length(na_range) != 2) {
      stopError_(
        "`na_range` must be a vector of length two of the same type as `x`."
      )
    }
    if (any(is.na(na_range))) {
      stopError_("`na_range` can not contain missing values.")
    }
  }
}
