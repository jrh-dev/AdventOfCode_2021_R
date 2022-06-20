

inp = function(lh) assign(deparse(substitute(lh)), as.integer(lh), envir = globalenv())

add = function(lh, rh) assign(deparse(substitute(lh)), as.integer(lh) + as.integer(rh), envir = globalenv())

mul = function(lh, rh) assign(deparse(substitute(lh)), as.integer(lh) * as.integer(rh), envir = globalenv())

div = function(lh, rh) assign(deparse(substitute(lh)), as.integer(floor(lh / rh)), envir = globalenv())

mod = function(lh, rh) assign(deparse(substitute(lh)), as.integer(lh %% rh), envir = globalenv())

eql = function(lh, rh) assign(deparse(substitute(lh)), ifelse(lh == rh, 1, 0), envir = globalenv())

w <- x <- y <- z <- 0


x1 <- "inp"
x2 <- ""
x3