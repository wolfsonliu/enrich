# -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title Fetch single pathway information from KEGG website
#'
#' \code{.hypergemetricTestVector} helps calculate the p value and adjusted p value
#' in the hypergeometric tests of intersection of one set to defined sets.
#'
#' @param pathway.id kegg id  of pathway, length should be 1.
#'
#' @return the returned result will be a list,
#' contain the information of certain pathway fetched from KEGG website API.
#' 
#' @details This function is used to do many hypergeometric tests,
#' and calculate both the p value and adjusted p value.
#' 
#' @seealso \code{\link{phyper}} \code{\link{p.adjust}}
#'
#' @import memoise
fetchPathwayInfo <- memoise(function(pathway.id) {
###{{{ Get the information for specific pathway.

    pathway.info.item <- c(
        "ENTRY",       "NAME",
        "DESCRIPTION", "CLASS",
        "PATHWAY_MAP", "DISEASE",
        "ORGANISM",    "GENE",
        "COMPOUND",    "REFERENCE",
        "AUTHORS",     "TITLE",
        "JOURNAL",     "KO_PATHWAY",
        "MODULE",      "DRUG")
    list.url          <- paste0(
        "http://rest.kegg.jp/get/",
        as.character(pathway.id)
    )
    pathway.info.file <- url(list.url, open = "rt")
    pathway.info      <- list()
    myline            <- readLines(pathway.info.file, n = 1)
    while (length(myline) != 0) {
        tmp.item <- unlist(strsplit(myline, split = "[ ]+"))[1]
        if (nchar(tmp.item) == 0) {
            ## nchar is 0 means it's not the first time to meet the item.
            ## so, get the last item name.
            tmp.item       <- tmp.item.cache
        } else {
            ## the first time to meet the item, make item name cache.
            tmp.item.cache <- tmp.item
        }
        if (tmp.item == "ENTRY") {
            pathway.info[[tmp.item]]        <- unlist(      
                strsplit(myline, split = "[ ]+"))[2]
                                    # get the pathway id.
            names(pathway.info[[tmp.item]]) <- unlist(
                strsplit(myline, split = "[ ]+"))[3]
                                    # name the vector with pathway.
        } else if (tmp.item == "NAME") {
                                    # get the name of the pathway.
            pathway.info[[tmp.item]] <- sub(
                pattern     = " - .*$",
                replacement = "",
                x           = sub(
                    pattern     = "NAME[ ]+",
                    replacement = "",
                    x           = myline))
        } else if (tmp.item == "DESCRIPTION") {
            pathway.info[[tmp.item]] <- sub(
                                    # get the DESCRIPTION text.
                pattern     = "DESCRIPTION[ ]+",
                replacement = "",
                x           = myline)
        } else if (tmp.item == "CLASS") {
            pathway.info[[tmp.item]] <- unlist(
                                    # get the CLASS.
                strsplit(
                    sub("CLASS[ ]+", "", myline),
                    split = "; "))
        } else if (tmp.item == "PATHWAY_MAP") {
            tmp.pathway.id              <- unlist(
                strsplit(
                    sub("PATHWAY_MAP[ ]+", "", myline),
                    split = " "))[1]
                                    # get the pathway id.
            tmp.pathway.name            <- sub(
                                    # get the pathway name.
                paste0("PATHWAY_MAP[ ]+",
                       tmp.pathway.id,
                       "[ ]+"),
                "",
                myline)
            pathway.info[[tmp.item]]        <- c(
                                    # store the pathway information.
                tmp.pathway.id,
                tmp.pathway.name)
            names(pathway.info[[tmp.item]]) <- c("ID", "NAME")
            rm(tmp.pathway.id,      # remove temporary variables.
               tmp.pathway.name)
        } else if (tmp.item == "DISEASE") {
            tmp.disease.id   <- unlist(
                                    # get the disease id.
                strsplit(
                    sub("(DISEASE[ ]+|[ ]+)", "", myline),
                    split = " "))[1]
            tmp.disease.name <- sub(
                                    # get the disease name.
                paste0("(DISEASE[ ]+|[ ]+)",
                       tmp.disease.id,
                       "[ ]+"),
                "",
                myline)
            if (is.null(pathway.info[[tmp.item]])) {
                ## meet the item for the first time, create data.frame.
                pathway.info[[tmp.item]]        <- data.frame(
                    ID   = tmp.disease.id,
                    Name = tmp.disease.name,
                    stringsAsFactors = FALSE)
            } else {
                ## meet the item again, rbind data.frame.
                pathway.info[[tmp.item]]         <- rbind(
                    pathway.info[[tmp.item]],
                    data.frame(
                        ID   = tmp.disease.id,
                        Name = tmp.disease.name,
                        stringsAsFactors = FALSE))
            }
            rm(tmp.disease.id,   # remove the temporary variables.
               tmp.disease.name)
        } else if (tmp.item == "ORGANISM") {
            tmp.org.english <- gsub(
                                    # gsub remove the parentheses.
                pattern     = "[()]",
                replacement = "",
                x           = grep(
                                    # grep return the english name.
                    pattern = "\\(.*\\)",
                    x       = unlist(
                        strsplit(
                            x     = sub(
                                pattern     = "ORGANISM[ ]+",
                                replacement = "",
                                x           = myline),
                            split = " ")),
                    value = TRUE))
            tmp.org.code    <- substr(
                                    # get the organism code.
                x = grep(
                    pattern = "\\[.*\\]",
                    x       = unlist(
                        strsplit(
                            x     = sub(
                                pattern     = "ORGANISM[ ]+",
                                replacement = "",
                                x           = myline),
                            split = " ")),
                    value = TRUE),
                5, 7)
            tmp.org.scientific <- gsub(
                                    # get the scientific name.
                pattern     = "[ ]+(\\(.*\\)|\\[.*\\])",
                replacement = "",
                x           = sub("ORGANISM[ ]+", "", myline))               
            pathway.info[[tmp.item]]        <- c(
                                    # store the information
                tmp.org.code,
                tmp.org.scientific,
                tmp.org.english)
            names(pathway.info[[tmp.item]]) <- c(
                "Code",
                "Scientific",
                "English")
            rm(tmp.org.code,        # remove temporary variables.
               tmp.org.scientific,
               tmp.org.english) 
        } else if (tmp.item == "GENE") {
            tmp.gene.line <- sub(   # cut line.
                pattern     = "(GENE[ ]+|[ ]+)",
                replacement = "",
                x           = myline)
            tmp.gene.entrez <- unlist(
                strsplit(           # get gene entrez id.
                    x     = tmp.gene.line,
                    split = "[ ]+"))[1]
            tmp.gene.line <- sub(   # cut line.
                pattern     = paste0(tmp.gene.entrez, "[ ]+"),
                replacement = "",
                x           = tmp.gene.line)
            tmp.gene.symbol <- unlist(
                strsplit(           # get gene symbol.
                    x     = tmp.gene.line,
                    split = ";[ ]+"))[1]
            if (!length(tmp.gene.symbol)) {
                tmp.gene.symbol <- ""
            } else {
                if (length(tmp.gene.symbol) & !nzchar(tmp.gene.symbol)) {
                    tmp.gene.symbol <- ""
                }
            }
            if (!is.na(tmp.gene.symbol)) {
                tmp.gene.line <- sub(   # cut line.
                    pattern     = paste0(tmp.gene.symbol, ";[ ]+"),
                    replacement = "",
                    x           = tmp.gene.line)
            }
            tmp.gene.name <- unlist(
                strsplit(           # get gene name.
                    x     = tmp.gene.line,
                    split = " [[]"))[1]
            if (!length(tmp.gene.name)) {
                tmp.gene.name <- ""
            } else {
                if (length(tmp.gene.name) & !nzchar(tmp.gene.name)) {
                    tmp.gene.name <- ""
                }
            }
            if (!is.na(tmp.gene.name)) {
                tmp.gene.line <- unlist(
                    strsplit(           # cut line.
                        x     = tmp.gene.line,
                        split = " "))
            }
            tmp.gene.code <- "0"
            tmp.gene.code <- grep(  # get gene code.
                pattern = "\\[KO:.*\\]",
                x       = unlist(
                    strsplit(
                        x     = tmp.gene.line,
                        split = "[ ]+")),
                value = TRUE)
            ## print(!is.null(tmp.gene.code) & !nzchar(tmp.gene.code))
            ## print(tmp.gene.entrez)
            ## print(tmp.gene.symbol)
            ## print(tmp.gene.name)
            ## print(tmp.gene.code)
            if (!length(tmp.gene.code)) {
                tmp.gene.code <- ""
            } else {
                if (length(tmp.gene.code) & !nzchar(tmp.gene.code)) {
                    tmp.gene.code <- ""
                }
            }                
            if (is.null(pathway.info[[tmp.item]])) {
                ## the first time read GENE, create data.frame.
                pathway.info[[tmp.item]] <- data.frame(
                    Entrez_id = tmp.gene.entrez,
                    Symbol    = tmp.gene.symbol,
                    Name      = tmp.gene.name,
                    Code      = substr(
                        tmp.gene.code,
                        5, nchar(tmp.gene.code) - 1),
                    stringsAsFactors = FALSE)
            } else {
                ## not the first time to read GENE, rbind data.frame.
                pathway.info[[tmp.item]] <- rbind(
                    pathway.info[[tmp.item]],
                    data.frame(
                        Entrez_id = tmp.gene.entrez,
                        Symbol    = tmp.gene.symbol,
                        Name      = tmp.gene.name,
                        Code      = substr(tmp.gene.code,
                            5, nchar(tmp.gene.code) - 1),
                        stringsAsFactors = FALSE))
            }
            rm(tmp.gene.line,       # remove temporary variables.
               tmp.gene.entrez,
               tmp.gene.symbol,
               tmp.gene.name,
               tmp.gene.code)
        } else if (tmp.item  == "COMPOUND") {
            tmp.compound.id   <- unlist(
                                    # get the compound id.
                strsplit(
                    sub("(COMPOUND[ ]+|[ ]+)", "", myline),
                    split = " "))[1]
            tmp.compound.name <- sub(
                                    # get the compound name.
                paste0("(COMPOUND[ ]+|[ ]+)",
                       tmp.compound.id,
                       "[ ]+"),
                "",
                myline)
            if (is.null(pathway.info[[tmp.item]])) {
                ## meet the item for the first time, create data.frame.
                pathway.info[[tmp.item]]        <- data.frame(
                    ID   = tmp.compound.id,
                    Name = tmp.compound.name,
                    stringsAsFactors = FALSE)
            } else {
                ## meet the item again, rbind data.frame.
                pathway.info[[tmp.item]]         <- rbind(
                    pathway.info[[tmp.item]],
                    data.frame(
                        ID   = tmp.compound.id,
                        Name = tmp.compound.name,
                        stringsAsFactors = FALSE))
            }
            rm(tmp.compound.id,  # remove the temporary variables.
               tmp.compound.name)      
        } else if (tmp.item  == "MODULE") {
            tmp.module.id   <- unlist(
                strsplit(           # get the compound id.
                    sub("(MODULE[ ]+|[ ]+)", "", myline),
                    split = " "))[1]
            tmp.module.name <- sub(
                                    # get the compound name.
                paste0("(MODULE[ ]+|[ ]+)",
                       tmp.module.id,
                       "[ ]+"),
                "",
                myline)
            if (is.null(pathway.info[[tmp.item]])) {
                ## meet the item for the first time, create data.frame.
                pathway.info[[tmp.item]]        <- data.frame(
                    ID   = tmp.module.id,
                    Name = tmp.module.name,
                    stringsAsFactors = FALSE)
            } else {
                ## meet the item again, rbind data.frame.
                pathway.info[[tmp.item]]         <- rbind(
                    pathway.info[[tmp.item]],
                    data.frame(
                        ID   = tmp.module.id,
                        Name = tmp.module.name,
                        stringsAsFactors = FALSE))
            }
            rm(tmp.module.id,  # remove the temporary variables.
               tmp.module.name)      
        } else if (tmp.item  == "DRUG") {
            tmp.drug.id   <- unlist(
                strsplit(           # get the compound id.
                    sub("(DRUG[ ]+|[ ]+)", "", myline),
                    split = " "))[1]
            tmp.drug.name <- sub(
                                    # get the compound name.
                paste0("(DRUG[ ]+|[ ]+)",
                       tmp.drug.id,
                       "[ ]+"),
                "",
                myline)
            if (is.null(pathway.info[[tmp.item]])) {
                ## meet the item for the first time, create data.frame.
                pathway.info[[tmp.item]]        <- data.frame(
                    ID   = tmp.drug.id,
                    Name = tmp.drug.name,
                    stringsAsFactors = FALSE)
            } else {
                ## meet the item again, rbind data.frame.
                pathway.info[[tmp.item]]         <- rbind(
                    pathway.info[[tmp.item]],
                    data.frame(
                        ID   = tmp.drug.id,
                        Name = tmp.drug.name,
                        stringsAsFactors = FALSE))
            }
            rm(tmp.drug.id,  # remove the temporary variables.
               tmp.drug.name)      
        } else if (tmp.item == "KO_PATHWAY") {
            pathway.info[[tmp.item]] <- sub(
                                    # get the name of the pathway.
                pattern     = "KO_PATHWAY[ ]+",
                replacement = "",
                x           = myline)
        } else if (tmp.item == "REFERENCE") {
            tmp.ref.id      <- sub( # get PMID.
                pattern     = "REFERENCE[ ]+",
                replacement = "",
                x           = myline)
            tmp.ref.author  <- gsub( # get authors
                pattern     = "([ ]+AUTHORS[ ]+|\\.)",
                replacement = "",
                x           = readLines(pathway.info.file, n = 1))
            tmp.ref.title   <- sub( # get title.
                pattern     = "[ ]+TITLE[ ]+",
                replacement = "",
                x           = readLines(pathway.info.file, n = 1))
            tmp.ref.journal <- sub( # get journal information.
                pattern     = "[ ]+JOURNAL[ ]+",
                replacement = "",
                x           = readLines(pathway.info.file, n = 1))
            if (is.null(pathway.info[[tmp.item]])) {
                pathway.info[[tmp.item]] <- data.frame(
                    PMID    = tmp.ref.id,
                    Author  = tmp.ref.author,
                    Title   = tmp.ref.title,
                    Journal = tmp.ref.journal,
                    stringsAsFactors = FALSE)
            } else {
                pathway.info[[tmp.item]] <- rbind(
                    pathway.info[[tmp.item]],
                    data.frame(
                        PMID    = tmp.ref.id,
                        Author  = tmp.ref.author,
                        Title   = tmp.ref.title,
                        Journal = tmp.ref.journal,
                        stringsAsFactors = FALSE))
            }
            rm(tmp.ref.id,          # remove temporary variables.
               tmp.ref.author,
               tmp.ref.title,
               tmp.ref.journal)
        }
        myline   <- readLines(pathway.info.file, n = 1)
    }
    close(pathway.info.file)
    rm(myline,
       pathway.info.item,
       pathway.info.file,
       list.url,
       tmp.item,
       tmp.item.cache)        
    return(pathway.info)

###}}}
})

################################################################################
##                                 EOF                                        ##
################################################################################
