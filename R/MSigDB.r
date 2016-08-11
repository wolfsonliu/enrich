# -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title MSigDB data
#'
#' Data of the MSigDB sets, stored in list. using \code{data("msigdb")} to load data.
#'
#' @format a list with 13311 objects of class "MSigDBSet".
#'
#' @source \url{http://software.broadinstitute.org/gsea/msigdb}
#'
"MSigDB"

#' @title subset MSigDB data
#'
#' subsetMSigDB is used to subset the MSigDB data (list of "MSigDBSet" objects)
#' with slot names and values of "MSigDBSet" in MSigDB data.
#'
#' @param msigdb the list of "MSigDBSet" objects.
#' @param slot.type the name of slot of "MSigDBSet".
#' @param slot.value the value of slot selected.
#'
#' @return a sublist of the data inputed that match the value gaven.
#'
#' @details
#'
#' @seealso \code{\link{MSigDB}}
#'
#' @examples
#'
#' data("msigdb")
#'
#' mysublist <- subsetMSigDB(
#'     msigdb     = MSigDB,
#'     slot.type  = "ORGANISM",
#'     slot.value = "Homo sapiens"
#' )
#'     
#' 
#' @import methods
#' @export
subsetMSigDB <- function(msigdb,
                         slot.type,
                         slot.value) {
###{{{
    
    store.call <- match.call()
    if (class(msigdb) != "list") {
        stop(
            paste0(
                as.list(store.call)[[1]],
                ": please enter a list contains MSigDBSets"
            )
        )
    } else if (any(unlist(lapply(msigdb, function(x) class(x) != "MSigDBSet")))) {
        stop(
            paste0(
                as.list(store.call)[[1]],
                ": items in input lists should be class MSigDBSet."
            )
        )
    } else if (!any(slot.type %in% slotNames("MSigDBSet"))) {
        stop(
            paste0(
                as.list(store.call)[[1]],
                ": slot.type should be one of MSigDBSet slot names."
            )
        )
    } else {}
    sets <- lapply(
        msigdb,
        function(x) {
            if (any(slot(x, slot.type) %in% slot.value)) {
                return(x)
            } else {
                NA
            }
        }
    )
    sets <- sets[!is.na(sets)]
    names(sets) <- lapply(
        sets,
        slot,
        name = "STANDARD_NAME"
    )                     
    sets
    
###}}}
}

################################################################################
##                                 EOF                                        ##
################################################################################
