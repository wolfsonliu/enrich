# -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title view pathway information of KEGG website.
#' 
#' \code{viewKEGGPathwayInfo} returns information about KEGG pathway fetched from
#' KEGG website.
#'
#' @param pathway.id character, KEGG pathway ID,
#' namely with three characters and several numbers, such as map01100.
#' 
#' @return list of KEGG pathways information.
#' If the pathway.id only have one pathway ID,
#' the result is a list contain the information of that pathway.
#' If the pathway.id contains many pathway,
#' the result is a list contains many pathway information lists.
#'  
#' @details \code{viewKEEGGPathwayInfo} will return the information about a certain
#' pathway fetched from KEGG website.
#' The result is a list contains the information of one or more pathways.
#' The function just fetchs the information of pathway from KEGG website,
#' using KEGG website API \url("http://rest.kegg.jp/get/map01100").
#' 
#' @seealso \code{\link{listKEGGPathway}} \code{\link{listKEGGOrganism}} 
#'
#' @examples
#' map01100 <- viewKEGGPathwayInfo("map01100")
#'
#' map01100
#' 
#' @export
viewKEGGPathwayInfo <- function(pathway.id) {
###{{{

    if (length(pathway.id) == 1) {
        return(fetchPathwayInfo(pathway.id))
    } else if (length(pathway.id) > 1) {
        result <- list()
        for (i in seq_along(pathway.id)) {
            cat("Fetching pathway information", sep = "")
            cat(" * ", sep = "")
            cat(switch(i%%3 + 1, "/", "\\", "-"), sep = "")
            cat(" * \r", sep = "")
            result[[pathway.id[i]]] <- fetchPathwayInfo(
                pathway.id[i]
            )
        }
        names(result) <- pathway.id
        return(result)
    } else {}

###}}}
}

################################################################################
##                                 EOF                                        ##
################################################################################
