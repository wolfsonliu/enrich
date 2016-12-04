# -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title List available pathways of KEGG website.
#' 
#' \code{listKEGGPathway} returns a data.frame contains the information about
#' pathways for certain organism in KEGG website.
#'
#' @param organism character, 3 characters KEGG organism code.
#' @param origin bool, if TRUE, return the file fetched from KEGG website.
#' If FALSE, the file contents will be reordered.
#'
#' @return data.frame with KEGG pathway IDs and names. 
#'  
#' @details \code{listKEGGOrganism} using API of KEGG website
#' (\url{"http://rest.kegg.jp/list/pathway/"}) to fetch the lists of available
#' pathways of certain organism. The parameter, organism, could be KEGG organism
#' code, normal English name, scientific name. Organisms in KEGG website could be
#' abtained from \code{listKEGGOrganism}.
#' 
#' @seealso \code{\link{listKEGGOrganism}} 
#' 
#' @examples
#' pathways <- listKEGGPathway()
#' 
#' pathways
#'
#' @import memoise
#' @export
listKEGGPathway <- memoise(function(organism,
                                    origin = FALSE) {
###{{{ Get pathway list for specific organism from KEGG database.

    list.url.prefix <- "http://rest.kegg.jp/list/pathway/"
    organism.list   <- listKEGGOrganism()
    if (nchar(organism) == 3) {
        ## Check organism code
        if (organism %in% organism.list$Code) {
            list.url <- paste0(
                list.url.prefix,
                organism)
            tmp.organism <- with(
                organism.list,
                Scientific[Code == organism])
        } else {
            stop(paste0("Organism code not found: ", organism))
        }
    } else if (grepl(organism, organism.list$Scientific, ignore.case = TRUE)) {
        list.url <- paste0(
            list.url.prefix,
            organism.list$Code[grep(
                organism,
                organism.list$Scientific,
                ignore.case = TRUE)])
        tmp.organism <- with(
            organism.list,
            Scientific[grep(
                organism,
                organism.list$Scientific,
                ignore.case = TRUE)])
    } else if (grepl(organism, organism.list$English, ignore.case = TRUE)) {
        list.url <- paste0(
            list.url.prefix,
            organism.list$Code[grep(
                organism,
                organism.list$English,
                ignore.case = TRUE)])
        tmp.organism <- with(
            organism.list,
            Scientific[grep(
                organism,
                organism.list$English,
                ignore.case = TRUE)])
    } else {
        stop(paste0("Organism code or name not found: ", organism))
    }
    ## Read file from url
    origin.file        <- read.delim(
        file             = url(list.url),
        header           = FALSE,
        stringsAsFactors = FALSE)
    names(origin.file) <- c("ID", "Name") # Rename origin.file columns
    pathway            <- data.frame(
        ID = sub("path:", replacement = "", origin.file$ID),
        Name = sub(
            paste0(" - ",
                   tmp.organism,
                   ".*$"),
            replacement = "",
            origin.file$Name)
    )                        # Make the data better for human reading.
    if (origin) {
        return(origin.file)
    } else {
        return(pathway)
    }
    
###}}}
})

################################################################################
##                                 EOF                                        ##
################################################################################
