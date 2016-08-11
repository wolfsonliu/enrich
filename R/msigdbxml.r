# -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title MSigDB XML file processing
#'
#' These functions are used to deal with the xml file downloaded from MSigDB.
#'
#' @param file xml file downloaded from MSigDB (\url{http://software.broadinstitute.org/gsea/msigdb}).
#' 
#' @return a list of processed XML or list of "MSigDBSet" objects.
#'
#' @details \code{readMSigDBxml} is a wrapped function of xmlToList.
#' \code{makeMSigDBList} returns a list of "MSigDBSet" objects.
#' 
#' @import XML
#' @export
readMSigDBxml <- function(file) {
    dat <- xmlToList(xmlParse(file))
    dat
}


#' @rdname readMSigDBxml
#' 
#' @export
makeMSigDBList <- function(file) {
    msigdbdata <- readMSigDBxml(file)
    msigdb <- lapply(
        msigdbdata[1:length(msigdbdata) - 1],
        function(x) {
            new("MSigDBSet", value.vector = x)
        }
    )
    names(msigdb) <- unlist(
        lapply(
            msigdb,
            slot,
            "STANDARD_NAME"
        )
    )
    attr(msigdb, "version") <- msigdbdata[[length(msigdbdata)]]
    return(msigdb)
}

################################################################################
##                                 EOF                                        ##
################################################################################
