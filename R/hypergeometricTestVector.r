# -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title Hypergeomatric Test of Terms and Genes
#'
#' \code{.hypergemetricTestVector} helps calculate the p value and adjusted p value
#' in the hypergeometric tests of intersection of one set to defined sets.
#'
#' @param drawn.ball vector of items to be tested, like drawn balls.
#' @param urns lists of items contained in different sets, like different urns
#' contains balls.
#' @param total.ball.num the background number of items.
#' @param adj.p.method adjusted p value methods in \code{p.adjust}, including
#' \code{c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")},
#' default to be "BH".
#'
#' @return the returned result will be a data.frame,
#' containing numbers of drawn, background number, p value and adjusted p value,
#' with the same row number as the length of input \code{urns}.
#' The order of results will be the same with \code{urns},
#' and the names will also be the same if has.
#' 
#' @details This function is used to do many hypergeometric tests,
#' and calculate both the p value and adjusted p value.
#' 
#' @seealso \code{\link{phyper}} \code{\link{p.adjust}}
#'
hypergeometricTestVector <- function(drawn.ball,
                                     urns,
                                     total.ball.num,
                                     adj.p.method = "BH") {
###{{{
    
    ## wa: white all
    ## wd: white drawn
    ## wn: white not drawn
    ## ba: black all
    ## bd: black drawn
    ## bn: black not drawn
    ## d:  drawn
    ## n:  not drawn
    ## t:  total
    hypergeometric.t  <- total.ball.num
    hypergeometric.d  <- length(drawn.ball)
    hypergeometric.n  <- total.ball.num - hypergeometric.d
    hypergeometric.wa <- unlist(
        lapply(
            urns,
            FUN = function(x) {
                return(
                    ifelse(
                        test = is.null(length(x)),
                        yes  = 0,
                        no   = length(x)
                    )
                )
            }
        )
    )
    hypergeometric.wd <- unlist(
        lapply(
            urns,
            FUN = function(x) {
                return(length(intersect(drawn.ball, x)))
            }
        )
    )
    hypergeometric.wn <- hypergeometric.wa - hypergeometric.wd
    hypergeometric.ba <- hypergeometric.t  - hypergeometric.wa
    hypergeometric.bd <- hypergeometric.d  - hypergeometric.wd
    hypergeometric.bn <- hypergeometric.ba - hypergeometric.bd
    ## hypergeometric test
    p.val <- phyper(
        q          = hypergeometric.wd,
        m          = hypergeometric.wa,
        n          = hypergeometric.ba,
        k          = hypergeometric.bd,
        lower.tail = FALSE
    )
    p.adj <- p.adjust(
        p.val,
        method = adj.p.method
    )
    result <- data.frame(
        drawn    = hypergeometric.wd,
        urn      = hypergeometric.wa,
        p.value  = p.val,
        p.adjust = p.adj
    )
    if (!is.null(names(urns))) {
        rownames(result) <- names(urns)
    } else {}
    result
    
###}}}
}

################################################################################
##                                 EOF                                        ##
################################################################################
