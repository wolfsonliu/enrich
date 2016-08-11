# -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title MSigDB data sets enrichment tests.
#'
#' \code{enrichMSigDB} will do the hypergeometric tests to get the p value and
#' adjusted p value of KEGG pathways.
#'
#' @param entrez.gene.list character vector, entrez gene IDs of DE genes.,
#' @param msigdb list of MSigDBSet.
#' If NA, the function will load data MSigDB of the package automatically.
#' @param whole.gene.number numeric, the total backgroud number of all genes.
#' @param threshold numeric, output tests' adjusted p value cut off, default 0.05.
#' @param adjust.p.method character, method used in \code{p.adjust}.
#' 
#' @return data.frame with columns,
#' Term (MSigDB set names),
#' Type (always MSigDB),
#' ID (MSigDB set SYSTEMATIC_NAME),
#' Input.number (number of input genes in the set),
#' Background.number (number of all genes in the set),
#' P.value,
#' adjusted.P.value. 
#'
#' @details \code{enrichMSigDB} is used for MSigDB sets enrichment.
#' The sets come from MSigDB web
#' (\url{http://software.broadinstitute.org/gsea/msigdb}).
#'
#' @seealso
#' \code{\link{enrichKEGGPathway}}
#' \code{\link{MSigDB}}
#' \code{\link{MSigDBSet}}
#'
#' @examples
#' data("deg")
#' 
#' de.entrez <- as.character(na.omit(deg$entrezgene))
#'
#' enrich.result <- enrichMSigDB(
#'     entrez.gene.list  = de.entrez,
#'     msigdb            = NA,
#'     whole.gene.number = 20000,
#'     threshold         = 0.05,
#'     adjust.p.method   = "BH"
#' )
#' 
#' enrich.result
#' 
#' @export
enrichMSigDB <- function(entrez.gene.list,
                         msigdb            = NA,
                         whole.gene.number = 22333,
                         threshold         = 0.05,
                         adjust.p.method   = "BH") {
###{{{ enrichMSigDB

    if (!is.character(entrez.gene.list)) {
        entrez.gene.list <- as.vector(entrez.gene.list)
    } else {}
    if (length(msigdb) == 1 && is.na(msigdb)) {
        data("msigdb")
        msigdb <- MSigDB
    } else if (!is.list(msigdb) || class(msigdb[[1]]) != "MSigDBSet"){
        stop("Please enter a list of MSigDBSet data in list class to msigdb parameter.")
    } else if (length(msigdb) < 1) {
        stop("Please enter MSigDBSet list with at list one element to msigdb parameter.")
    } else {}
    msigdb.genes <- lapply(
        msigdb,
        function(x) {
            return(as.character(unname(getMembers(x)[, "entrez"])))
        }
    )
    names(msigdb.genes) <- unlist(
        lapply(
            msigdb,
            function(x) {
                return(slot(x, "STANDARD_NAME"))
            }
        )
    )
    test.result <- hypergeometricTestVector(
        drawn.ball     = entrez.gene.list,
        urns           = msigdb.genes,
        total.ball.num = whole.gene.number,
        adj.p.method   = adjust.p.method
    )
    msigdb.result <- data.frame(
        Term              = unlist(
            lapply(
                msigdb,
                function(x) {
                    return(unname(slot(x, "STANDARD_NAME")))
                }
            )
        ),
        Type              = rep("MSigDB", length(msigdb)),
        ID                = unlist(
            lapply(
                msigdb,
                function(x) {
                    return(unname(slot(x, "SYSTEMATIC_NAME")))
                }
            )
        ),
        Input.number      = test.result$drawn,
        Background.number = test.result$urn,
        P.value           = test.result$p.value,
        adjusted.P.value  = test.result$p.adjust,
        stringsAsFactors  = FALSE
    )
    msigdb.output <- subset(
        msigdb.result,
        adjusted.P.value <= threshold &
        Background.number != 0 &
        Input.number != 0
    )
    return(msigdb.output[order(msigdb.output$adjusted.P.value),])
    
###}}}
}

################################################################################
##                                 EOF                                        ##
################################################################################
