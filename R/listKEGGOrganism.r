# -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title List available organisms of KEGG website.
#'
#' \code{listKEGGOrganism} returns a data.frame contains the information about
#' available organisms in KEGG website.
#' 
#' @param origin bool, if TRUE, return the file fetched from KEGG website.
#' If FALSE, the file contents will be reordered.
#'
#' @return the returned result will be a data.frame, containing the information of
#' organisms in KEGG website. The data.frame has organisms' ID,
#' organisms' three characters code, and organisms' scientific and English names.
#'  
#' @details \code{listKEGGOrganism} using API of KEGG website
#' (\url{"http://rest.kegg.jp/list/organism"}) to fetch the lists of organisms.
#' The three-character KEGG organism code is used as the start 3 characters
#' of KEGG pathway ID.
#' 
#' @seealso \code{\link{listKEGGPathway}} 
#'
#' @examples
#' organism <- listKEGGOrganism()
#' 
#' organism
#'
#' @import memoise
#' @export
listKEGGOrganism <- memoise(function(origin   = FALSE) {
###{{{ Get organism list from KEGG database.

    list.url            <- "http://rest.kegg.jp/list/organism"
    kegg.list.url      <- url(list.url)
                                    # url connection.
    origin.file        <- read.delim(
        file             = kegg.list.url,
        header           = FALSE,
        stringsAsFactors = FALSE)
                                    # read origin file
    names(origin.file) <- c("ID", "Code", "Scientific", "Classification")
    organism           <- data.frame(
        ## make the result more clear.
        ID            = origin.file$ID,
        Code          = origin.file$Code,
        Scientific    = sapply(
            strsplit(
                as.character(origin.file$Scientific),
                split = "[()]"),
            FUN = "[",
            1),
        English = sapply(
            strsplit(
                as.character(origin.file$Scientific),
                split = "[()]"),
            FUN = "[",
            2),
        Classification = origin.file$Classification)
    if (origin) {
        return(origin.file)
    } else {
        return(organism)
    }

###}}}
})

################################################################################
##                                 EOF                                        ##
################################################################################
