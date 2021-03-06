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

`.onLoad` <- function(...) {
    load_library <- function(pkg) {
        if (pkg %in% loadedNamespaces() && !is.element(pkg, .packages())) {
            loc <- dirname(getNamespaceInfo(pkg, "path"))
            do.call(
                "library",
                list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
            )
        }
    }
    suppressPackageStartupMessages(
        lapply(c("stats", "utils"), load_library)
    )
    if (unlockEnvironment_(asNamespace("base"))) {
        env <- as.environment("package:base")
        do.call("unlockBinding", list(sym = "print.data.frame", env = env))
        env$`print.data.frame` <- function (x, ..., digits = NULL, quote = FALSE, 
            right = TRUE, row.names = TRUE, max = NULL) {
            n <- length(row.names(x))
            if (length(x) == 0L) {
                do.call("cat", list(
                    sprintf(ngettext(n, "data frame with 0 columns and %d row", 
                    "data frame with 0 columns and %d rows"), n),
                    "\n", 
                    sep = "")
                )
            }
            else if (n == 0L) {
                print.default(names(x), quote = FALSE)
                do.call("cat", list(
                    gettext("<0 rows> (or 0-length row.names)\n")
                    )
                )
            }
            else {
                if (is.null(max)) 
                    max <- getOption("max.print", 99999L)
                if (!is.finite(max)) 
                    stop("invalid 'max' / getOption(\"max.print\"): ", 
                        max)
                omit <- (n0 <- max%/%length(x)) < n
                m <- as.matrix(format.data.frame(if (omit) 
                    x[seq_len(n0), , drop = FALSE]
                else x, digits = digits, na.encode = FALSE))
                if (!isTRUE(row.names)) 
                    dimnames(m)[[1L]] <- if (isFALSE(row.names)) 
                        rep.int("", if (omit) 
                        n0
                        else n)
                    else row.names
                do.call("print", list(m, ..., quote = quote, right = right, max = max))
                if (omit) 
                    do.call("cat", list(
                        " [ reached 'max' / getOption(\"max.print\") -- omitted", 
                        n - n0, "rows ]\n"
                        )
                    )
            }
            invisible(x)
        }
        do.call("unlockBinding", list(sym = "format.data.frame", env = env))
        env$`format.data.frame` <- function (x, ..., justify = "none")
        {
            nc <- length(x)
            if (!nc) 
                return(x)
            nr <- .row_names_info(x, 2L)
            rval <- vector("list", nc)
            for (i in seq_len(nc)) {
                if (is.declared(x[[i]]) && any(is.na(x[[i]]))) {
                    rval[[i]] <- format_declared(x[[i]])
                }
                else {
                    rval[[i]] <- format(x[[i]], ..., justify = justify)
                }
            }
            lens <- vapply(rval, NROW, 1)
            if (any(lens != nr)) {
                warning("corrupt data frame: columns will be truncated or padded with NAs")
                for (i in seq_len(nc)) {
                    len <- NROW(rval[[i]])
                    if (len == nr) 
                        next
                    if (length(dim(rval[[i]])) == 2L) {
                        rval[[i]] <- if (len < nr) 
                        rbind(rval[[i]], matrix(NA, nr - len, ncol(rval[[i]])))
                        else rval[[i]][seq_len(nr), ]
                    }
                    else {
                        rval[[i]] <- if (len < nr) 
                        c(rval[[i]], rep.int(NA, nr - len))
                        else rval[[i]][seq_len(nr)]
                    }
                }
            }
            for (i in seq_len(nc)) {
                if (is.character(rval[[i]]) && inherits(rval[[i]], "character")) 
                    oldClass(rval[[i]]) <- "AsIs"
            }
            y <- as.data.frame.list(rval, row.names = seq_len(nr), col.names = names(x), 
                optional = TRUE, fix.empty.names = FALSE, cut.names = TRUE)
            attr(y, "row.names") <- row.names(x)
            return(y)
        }
        do.call("unlockBinding", list(sym = "order", env = env))
        env$order <- function (..., na.last = TRUE, decreasing = FALSE,
            method = c("auto", "shell", "radix"), empty.last = na.last) {
            z <- list(...)
            decreasing <- as.logical(decreasing)
            if (length(z) == 1L && is.numeric(x <- z[[1L]]) && !is.object(x) && length(x) > 0) {
                if (eval(parse(text = ".Internal(sorted_fpass(x, decreasing, na.last))"))) {
                    return(seq_along(x))
                }
            }
            method <- match.arg(method)
            if (any(vapply(z, function(x) {
                    is.object(x) && !is.declared(x)
                }, logical(1L)))) {
                z <- lapply(z, function(x) if (is.object(x)) 
                    as.vector(xtfrm(x))
                else x)
                return(do.call("order", c(z, list(na.last = na.last, 
                    decreasing = decreasing, method = method))))
            }
            if (method == "auto") {
                useRadix <- all(vapply(z, function(x) {
                    (is.numeric(x) || is.factor(x) || is.logical(x)) && is.integer(length(x))
                }, logical(1L)))
                method <- ifelse (useRadix, "radix", "shell")
            }
            if (length(z) == 1L && is.declared(x)) {
                return(order_declared(x, na.last = na.last, decreasing = decreasing, method = method, empty.last = empty.last))
            }
            if (method != "radix" && !is.na(na.last)) {
                return(eval(parse(text = ".Internal(order(na.last, decreasing, ...))")))
            }
            if (method == "radix") {
                decreasing <- rep_len(as.logical(decreasing), length(z))
                return(eval(parse(text = ".Internal(radixsort(na.last, decreasing, FALSE, TRUE, ...))")))
            }
            if (any(diff((l.z <- lengths(z)) != 0L))) {
                stop("argument lengths differ")
            }
            na <- vapply(z, is.na, rep.int(NA, l.z[1L]))
            ok <- if (is.matrix(na)) {
                rowSums(na) == 0L
            }
            else {
                !any(na)
            }
            if (all(!ok)) {
                return(integer())
            }
            z[[1L]][!ok] <- NA
            ans <- do.call("order", c(z, list(decreasing = decreasing)))
            ans[ok[ans]]
        }
        do.call("unlockBinding", list(sym = "as.factor", env = env))
        env$as.factor <- function(
            x,
            levels = c("default", "labels", "values", "both"),
            ordered = FALSE,
            ...
        ) {
            if (is.declared(x)) {
                levels <- match.arg(levels)
                label <- attr(x, "label", exact = TRUE)
                labels <- attr(x, "labels", exact = TRUE)
                if (levels == "both") {
                    names(labels) <- paste0("[", labels, "] ", names(labels))
                    attr(x, "labels") <- labels
                    vals <- sort(unique(x), na.last = TRUE)
                    x <- factor(
                        as.character(undeclare(x)),
                        levels = as.character(undeclare(vals)),
                        ordered = ordered
                    )
                }
                else if (levels %in% c("default", "labels")) {
                    levs <- unname(labels)
                    labs <- names(labels)
                    x <- factor(
                        as.character(undeclare(x)),
                        levels = sort(unique(labs)),
                        ordered = ordered
                    )
                }
                else if (levels == "values") {
                    levels <- unique(
                        undeclare(
                            sort(x, na.last = TRUE),
                            drop = TRUE
                        )
                    )
                    x <- factor(
                        undeclare(x, drop = TRUE),
                        levels,
                        ordered = ordered
                    )
                }
                return(x)
            }
            else {
                if (is.factor(x)) 
                    x
                else if (!is.object(x) && is.integer(x)) {
                    levels <- sort.int(unique.default(x))
                    f <- match(x, levels)
                    levels(f) <- as.character(levels)
                    if (!is.null(nx <- names(x))) 
                        names(f) <- nx
                    class(f) <- "factor"
                    f
                }
                else factor(x)
            }
        }
    }
    if (unlockEnvironment_(asNamespace("stats"))) {
        env <- as.environment("package:stats")
        do.call("unlockBinding", list(sym = "sd", env = env))
        env$sd <- function(x, na.rm = FALSE) {
            if (is.declared(x)) {
                na_index <- attr(x, "na_index")
                if (!is.null(na_index)) {
                    x <- x[-na_index]
                }
                class(x) <- setdiff(class(x), "declared")
            }
            sqrt(var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm = na.rm))
        }
        do.call("unlockBinding", list(sym = "var", env = env))
        env$var <- function(x, y = NULL, na.rm = FALSE, use) {
            if (is.declared(x)) {
                na_index <- attr(x, "na_index")
                if (!is.null(na_index)) {
                    x <- x[-na_index]
                }
                class(x) <- setdiff(class(x), "declared")
            }
            if (missing(use)) {
                use <- ifelse(na.rm, "na.or.complete", "everything")
            }
            na.method <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs", 
                "everything", "na.or.complete"))
            if (is.na(na.method)) {
                stop("invalid 'use' argument")
            }
            if (is.data.frame(x)) {
                x <- as.matrix(x)
            }
            else {
                stopifnot(is.atomic(x))
            }
            if (is.data.frame(y)) {
                y <- as.matrix(y)
            }
            else {
                stopifnot(is.atomic(y))
            }
            eval(parse(text = '.Call(stats:::C_cov, x, y, na.method, FALSE)'))
        }
        do.call("unlockBinding", list(sym = "fivenum", env = env))
        env$fivenum <- function(x, na.rm = FALSE) {
            if (is.declared(x)) {
                na_index <- attr(x, "na_index")
                if (!is.null(na_index)) {
                    x <- x[-na_index]
                }
                class(x) <- setdiff(class(x), "declared")
            }
            xna <- is.na(x)
            if (any(xna)) {
                if (na.rm) 
                    x <- x[!xna]
                else return(rep.int(NA, 5))
            }
            x <- sort(x)
            n <- length(x)
            if (n == 0) 
                rep.int(NA, 5)
            else {
                n4 <- floor((n + 3)/2)/2
                d <- c(1, n4, (n + 1)/2, n + 1 - n4, n)
                0.5 * (x[floor(d)] + x[ceiling(d)])
            }
        }
    }
    register_S3_method("labelled", "na_values", "declared")
    register_S3_method("labelled", "na_values<-", "declared")
    register_S3_method("labelled", "na_range", "declared")
    register_S3_method("labelled", "na_range<-", "declared")
    register_S3_method("labelled", "val_labels", "declared")
    register_S3_method("labelled", "val_labels<-", "declared")
    register_S3_method("labelled", "var_label", "declared")
    register_S3_method("labelled", "var_label<-", "declared")
    register_S3_method("labelled", "drop_unused_value_labels", "declared")
    register_S3_method("labelled", "val_label", "declared")
    register_S3_method("labelled", "val_label<-", "declared")
    register_S3_method("labelled", "sort_val_labels", "declared")
    register_S3_method("labelled", "nolabel_to_na", "declared")
    register_S3_method("labelled", "val_labels_to_na", "declared")
    register_S3_method("labelled", "remove_labels", "declared")
    register_S3_method("labelled", "remove_user_na", "declared")
    register_S3_method("labelled", "to_factor", "declared")
    register_S3_method("labelled", "to_character", "declared")
    register_S3_method("labelled", "copy_labels", "declared")
    register_S3_method("haven", "as_factor", "declared")
    register_S3_method("haven", "zap_labels", "declared")
    register_S3_method("pillar", "pillar_shaft", "declared")
    register_S3_method("vctrs", "vec_ptype_abbr", "declared")
    register_S3_method("vctrs", "vec_ptype_full", "declared")
    register_S3_method("vctrs", "vec_ptype2", "declared")
    register_S3_method("vroom", "output_column", "declared")
    invisible()
}
`register_S3_method` <- function(pkg, generic, class, fun = NULL) {
    stopifnot(is.character(pkg), length(pkg) == 1)
    stopifnot(is.character(generic), length(generic) == 1)
    stopifnot(is.character(class), length(class) == 1)
    if (is.null(fun)) {
        fun <- get(paste0(generic, ".", class), envir = parent.frame())
    }
    else {
        stopifnot(is.function(fun))
    }
    if (pkg %in% loadedNamespaces()) {
        registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
    setHook(
        packageEvent(pkg, "onLoad"),
        function(...) {
            registerS3method(generic, class, fun, envir = asNamespace(pkg))
        }
    )
}
