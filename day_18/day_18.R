
[[[[4,3],4],4],[7,[[8,4],9]]] + [1,1]

Combine = function (lhs, rhs) {
    return(paste0("[", lhs, ",", rhs,"]"))
}

# example s
Combine("[[[[4,3],4],4],[7,[[8,4],9]]]","[1,1]")

[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
#example e


input = Combine("[[[[4,3],4],4],[7,[[8,4],9]]]","[1,1]")

Explo <- function(input) {
    keep_running <- TRUE

    while (keep_running) {
        bkt_cnt <- 0
        to_exp <- NULL

        for (ii in 1:nchar(input)) {
            if (substr(input, ii, ii) == "[") bkt_cnt <- bkt_cnt + 1
            if (substr(input, ii, ii) == "]") bkt_cnt <- bkt_cnt - 1

            if (bkt_cnt >= 5 & grepl("^[0-9{2},0-9{2}]+$", substr(input, ii, ii + 4))) {
                to_exp <- substr(input, ii, ii + 4)
                at_pos <- ii
                gap <- 7
                break
            }

            if (bkt_cnt >= 5 & grepl("^[0-9{2},0-9{1}]+$", substr(input, ii, ii + 3))) {
                to_exp <- substr(input, ii, ii + 3)
                at_pos <- ii
                gap <- 6
                break
            }

            if (bkt_cnt >= 5 & grepl("^[0-9{1},0-9{2}]+$", substr(input, ii, ii + 3))) {
                to_exp <- substr(input, ii, ii + 3)
                at_pos <- ii
                gap <- 6
                break
            }

            if (bkt_cnt >= 5 & grepl("^[0-9{1},0-9{1}]+$", substr(input, ii, ii + 2))) {
                to_exp <- substr(input, ii, ii + 2)
                at_pos <- ii
                gap <- 5
                break
            }
        }

        if (!is.null(to_exp)) {
            input <- paste0(substr(input, 1, at_pos - 2), "0", substr(input, at_pos + gap - 1, nchar(input)))


            pre_is <- NULL
            pre_at_pos <- NULL
            prelen <- NULL

            for (jj in (at_pos - 2):1) {
                if (grepl("[1-9]{1}", substr(input, jj, jj))) {
                    pre_is <- substr(input, jj, jj)
                    pre_at_pos <- jj
                    prelen <- 1
                    break
                }
            }

            if (!is.null(pre_at_pos)) {
                if (grepl("[1-9]{2}", substr(input, pre_at_pos - 1, pre_at_pos))) {
                    pre_is <- substr(input, pre_at_pos - 1, pre_at_pos)
                    pre_at_pos <- pre_at_pos - 1
                    prelen <- 2
                }
            }

            if (is.null(pre_is)) {
                input <- paste0(substr(input, 1, at_pos - 2), substr(input, at_pos - 1, nchar(input)))
            }

            if (!is.null(pre_is)) {
                input <- paste0(substr(input, 1, pre_at_pos - 1), as.numeric(sub("\\,.*", "", to_exp)) + as.numeric(pre_is), substr(input, pre_at_pos + prelen, nchar(input)))
            }



            post_is <- NULL
            post_at_pos <- NULL
            postlen <- NULL

            for (jj in (at_pos + 1):nchar(input)) {
                if (grepl("[1-9]{1}", substr(input, jj, jj))) {
                    post_is <- substr(input, jj, jj)
                    post_at_pos <- jj
                    postlen <- 1
                    break
                }
            }

            if (!is.null(pre_at_pos)) {
                if (grepl("[1-9]{2}", substr(input, post_at_pos, post_at_pos + 1))) {
                    post_is <- substr(input, post_at_pos - 1, post_at_pos)
                    postlen <- 2
                }
            }

            if (is.null(post_is)) {
                input <- paste0(substr(input, 1, at_pos), ",0", substr(input, at_pos + 1, nchar(input)))
            }

            if (!is.null(post_is)) {
                input <- paste0(substr(input, 1, post_at_pos - 1), as.numeric(sub(".*\\,", "", to_exp)) + as.numeric(post_is), substr(input, post_at_pos + postlen, nchar(input)))
            }
        } else {
            keep_running <- FALSE
        }
    }
    return(input)
}


t1 = Explo(Combine("[[[[4,3],4],4],[7,[[8,4],9]]]","[1,1]"))



