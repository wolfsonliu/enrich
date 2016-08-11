
                                        # -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title KEGG pathway enrichment tests
#'
#' The function to do hypergeometric tests on KEGG pathways.
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
#' @seealso \code{\link{enrichKEGGPathway}}
enrichKEGGPathwayParent <- function() {
###{{{ enrichPathwayParent

    pathways.info <- list()
    function(entrez.gene.list,
             pathway.list      = NA,
             whole.gene.number = 22333,
             threshold         = 0.05,
             adjust.p.method   = "BH",
             organism          = "hsa") {
        if (!is.atomic(entrez.gene.list)) {
            entrez.gene.list <- as.vector(entrez.gene.list)
        } else {}
        if (!is.character(pathway.list)) {
            pathway.list <- listKEGGPathway(organism)$ID
        } else {}
        if (length(pathways.info) < 300) {
            pathways.info <<- viewKEGGPathwayInfo(pathway.list)
        } else {}
        pathway.genes <- lapply(
            pathways.info[pathway.list],
            function(x) {
                return(x$GENE$Entrez_id)
            }
        )
        names(pathway.genes) <- unlist(
            lapply(
                pathways.info[pathway.list],
                function(x) x$ENTRY
            )
        )
        test.result <- hypergeometricTestVector(
            drawn.ball     = entrez.gene.list,
            urns           = pathway.genes,
            total.ball.num = whole.gene.number,
            adj.p.method   = adjust.p.method
        )
        pathway.result <- data.frame(
            Term              = unlist(
                lapply(
                    pathways.info[pathway.list],
                    FUN = function(x) {
                        return(x$NAME)
                    }
                )
            ),
            Type              = rep("KEGG pathway", length(pathway.list)),
            ID                = pathway.list,
            Input.number      = test.result$drawn,
            Background.number = test.result$urn,
            P.value           = test.result$p.value,
            adjusted.P.value  = test.result$p.adjust,
            stringsAsFactors  = FALSE
        )
        pathway.output <- subset(
            pathway.result,
            adjusted.P.value <= threshold &
            Background.number != 0 &
            Input.number != 0
        )
        return(pathway.output[order(pathway.output$adjusted.P.value),])
    }
    
###}}}
}


#' @title KEGG pathway enrichment tests.
#' 
#' \code{enrichKEGGPathway} will do the hypergeometric tests to get the p value and
#' adjusted p value of KEGG pathways.
#'
#' @param entrez.gene.list character vector, entrez gene IDs of DE genes.,
#' @param pathway.list character vector, KEGG pathway IDs.
#' If NA, the function will use all the pathways in KEGG website automatically.
#' @param whole.gene.number numeric, the total backgroud number of all genes.
#' @param threshold numeric, output tests' adjusted p value cut off, default 0.05.
#' @param adjust.p.method character, method used in \code{p.adjust}.
#' @param organism character, 3 characters' KEGG organism code, default hsa (human).
#' 
#' @return data.frame with columns,
#' Term (pathway names),
#' Type (always KEGG pathway),
#' ID (KEGG pathway ID),
#' Input.number (number of input genes in the pathway),
#' Background.number (number of all genes in the pathway),
#' P.value,
#' adjusted.P.value. 
#'  
#' @details \code{enrichKEGGPathway} is used for pathway enrichment.
#' The pathway information is fetched from KEGG website.
#' \code{phyper} is used to do the hypergeometric tests.
#' \code{p.adjust} is used to calculate the p adjustes.
#' 
#' @seealso
#' \code{\link{viewKEGGPathwayInfo}}
#' \code{\link{p.adjust}}
#' \code{\link{phyper}} 
#'
#' @examples
#' data("deg")
#' 
#' de.entrez <- as.character(na.omit(deg$entrezgene))
#' 
#' enrich.result <- enrichKEGGPathway(
#'     entrez.gene.list  = de.entrez,
#'     pathway.list      = NA,
#'     whole.gene.number = 20000,
#'     threshold         = 0.05,
#'     adjust.p.method   = "BH",
#'     organism          = "hsa"
#' )
#' 
#' enrich.result
#' 
#' @export    
enrichKEGGPathway <- enrichKEGGPathwayParent()
## Calculate the p value of pathway enrichment.


################################################################################
##                                 EOF                                        ##
################################################################################


