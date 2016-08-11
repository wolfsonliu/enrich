# -*- coding: utf-8 -*-
################################################################################
##                                                                            ##
################################################################################

#' @title MSigDB data set class
#' 
#' The Class "MSigDBSet" is used to store set from MSigDB.
#' The original data is downloaded from MSigDB webset with XML format.
#' 
#' @slot STANDARD_NAME        character, standard name of the set recently.
#' @slot SYSTEMATIC_NAME      character, systematic name of the sets,
#' unique and rarely changed.
#' @slot HISTORICAL_NAMES     character, names of the set used in the past.
#' @slot ORGANISM             character, organism of the set.
#' @slot PMID                 character, PMID of the set.
#' @slot AUTHORS              character.
#' @slot GEOID                character.
#' @slot EXACT_SOURCE         character.
#' @slot GENESET_LISTING_URL  character, url of the set in MSigDB.
#' @slot EXTERNAL_DETAILS_URL character, url of the set in MSigDB.
#' @slot CHIP                 character.
#' @slot CATEGORY_CODE        character, category code of set.
#' H: hallmark gene sets. 
#' C1: positional gene sets. 
#' C2: curated gene sets, with subcategories (CGP, CP, CP:BIOCARTA, CP:KEGG, CP:REACTOME). 
#' C3: motif gene sets, with subcategories (MIR, TFT). 
#' C4: computational gene sets, with subcategories (CGN, CM). 
#' C5: GO gene sets, with subcategories (BP, CC, MF).
#' C6: oncogenic signatures.
#' C7: immunologic signatures. 
#' @slot SUB_CATEGORY_CODE    character, sub category code of set. 
#' C2: 
#' CGP (chemical and genetic perturbations), 
#' CP (Canonical pathways), 
#' CP:BIOCARTA (BioCarta gene sets), 
#' CP:KEGG (KEGG gene sets), 
#' CP:REACTOME (Reactome gene sets). 
#' C3:
#' MIR (microRNA targets), 
#' TFT (transcription factor targets).
#' C4:
#' CGN (cancer gene neighborhoods),
#' CM (cancer modules).
#' C5: 
#' BP (GO biological process),
#' CC (GO cellular component).
#' @slot CONTRIBUTOR          character.
#' @slot CONTRIBUTOR_ORG      character.
#' @slot DESCRIPTION_BRIEF    character, brif description of the set.
#' @slot DESCRIPTION_FULL     character, detailed description of the set.
#' @slot TAGS                 character.
#' @slot MEMBERS              character, gene members with label of MSigDB.
#' @slot MEMBERS_SYMBOLIZED   character, gene members with gene symbol.
#' @slot MEMBERS_EZID         character, gene members with entrez id.
#' @slot MEMBERS_MAPPING      character, gene members with mapping of three labels.
#' @slot FOUNDER_NAMES        character.
#' @slot REFINEMENT_DATASETS  character.
#' @slot VALIDATION_DATASETS  character.
#'
#' 
#' @import methods
#' @export
setClass(
    Class = "MSigDBSet",
    slots = c(
        STANDARD_NAME        = "character",
        SYSTEMATIC_NAME      = "character",
        HISTORICAL_NAMES     = "character",
        ORGANISM             = "character",
        PMID                 = "character",
        AUTHORS              = "character",             
        GEOID                = "character",
        EXACT_SOURCE         = "character",
        GENESET_LISTING_URL  = "character",
        EXTERNAL_DETAILS_URL = "character",
        CHIP                 = "character",
        CATEGORY_CODE        = "character",       
        SUB_CATEGORY_CODE    = "character",
        CONTRIBUTOR          = "character",
        CONTRIBUTOR_ORG      = "character",
        DESCRIPTION_BRIEF    = "character",
        DESCRIPTION_FULL     = "character",
        TAGS                 = "character",
        MEMBERS              = "character",
        MEMBERS_SYMBOLIZED   = "character",
        MEMBERS_EZID         = "character",
        MEMBERS_MAPPING      = "character",
        FOUNDER_NAMES        = "character",
        REFINEMENT_DATASETS  = "character",
        VALIDATION_DATASETS  = "character"
    ),
    prototype = list(
        STANDARD_NAME        = "",
        SYSTEMATIC_NAME      = "",
        HISTORICAL_NAMES     = "",
        ORGANISM             = "",
        PMID                 = "",
        AUTHORS              = "",             
        GEOID                = "",
        EXACT_SOURCE         = "",
        GENESET_LISTING_URL  = "",
        EXTERNAL_DETAILS_URL = "",
        CHIP                 = "",
        CATEGORY_CODE        = "",       
        SUB_CATEGORY_CODE    = "",
        CONTRIBUTOR          = "",
        CONTRIBUTOR_ORG      = "",
        DESCRIPTION_BRIEF    = "",
        DESCRIPTION_FULL     = "",
        TAGS                 = "",
        MEMBERS              = "",
        MEMBERS_SYMBOLIZED   = "",
        MEMBERS_EZID         = "",
        MEMBERS_MAPPING      = "",
        FOUNDER_NAMES        = "",
        REFINEMENT_DATASETS  = "",
        VALIDATION_DATASETS  = ""
    )
)


#' @describeIn MSigDBSet initialize the MSigDBSet object.
#' @import methods
#' @export
setMethod(
    f = "initialize",
    signature = "MSigDBSet",
    definition = function(.Object,
                          value.vector = NULL,
                          STANDARD_NAME        = "",
                          SYSTEMATIC_NAME      = "",
                          HISTORICAL_NAMES     = "",
                          ORGANISM             = "",
                          PMID                 = "",
                          AUTHORS              = "",             
                          GEOID                = "",
                          EXACT_SOURCE         = "",
                          GENESET_LISTING_URL  = "",
                          EXTERNAL_DETAILS_URL = "",
                          CHIP                 = "",
                          CATEGORY_CODE        = "",       
                          SUB_CATEGORY_CODE    = "",
                          CONTRIBUTOR          = "",
                          CONTRIBUTOR_ORG      = "",
                          DESCRIPTION_BRIEF    = "",
                          DESCRIPTION_FULL     = "",
                          TAGS                 = "",
                          MEMBERS              = "",
                          MEMBERS_SYMBOLIZED   = "",
                          MEMBERS_EZID         = "",
                          MEMBERS_MAPPING      = "",
                          FOUNDER_NAMES        = "",
                          REFINEMENT_DATASETS  = "",
                          VALIDATION_DATASETS  = "",
                          ...) {
        store.call <- match.call()
        vect       <- vector()
        if (length(value.vector) != 0) {
            if (!is.atomic(value.vector)) {
                stop(
                    paste0(
                        as.list(store.call)[[1]],
                        ": value.vector should be a vector."
                    )
                )
            } else if (is.null(names(value.vector))) {
                stop(
                    paste0(
                        as.list(store.call)[[1]],
                        ": value.vector should have name attributes."
                    )
                )
            } else {
                vect <- value.vector
            }
        } else {
            vect <- c(
                STANDARD_NAME        = STANDARD_NAME,
                SYSTEMATIC_NAME      = SYSTEMATIC_NAME,
                HISTORICAL_NAMES     = HISTORICAL_NAMES,
                ORGANISM             = ORGANISM,
                PMID                 = PMID,
                AUTHORS              = AUTHORS,             
                GEOID                = GEOID,
                EXACT_SOURCE         = EXACT_SOURCE,
                GENESET_LISTING_URL  = GENESET_LISTING_URL,
                EXTERNAL_DETAILS_URL = EXTERNAL_DETAILS_URL,
                CHIP                 = CHIP,
                CATEGORY_CODE        = CATEGORY_CODE,       
                SUB_CATEGORY_CODE    = SUB_CATEGORY_CODE,
                CONTRIBUTOR          = CONTRIBUTOR,
                CONTRIBUTOR_ORG      = CONTRIBUTOR_ORG,
                DESCRIPTION_BRIEF    = DESCRIPTION_BRIEF,
                DESCRIPTION_FULL     = DESCRIPTION_FULL,
                TAGS                 = TAGS,
                MEMBERS              = MEMBERS,
                MEMBERS_SYMBOLIZED   = MEMBERS_SYMBOLIZED,
                MEMBERS_EZID         = MEMBERS_EZID,
                MEMBERS_MAPPING      = MEMBERS_MAPPING,
                FOUNDER_NAMES        = FOUNDER_NAMES,
                REFINEMENT_DATASETS  = REFINEMENT_DATASETS,
                VALIDATION_DATASETS  = VALIDATION_DATASETS
            )   
        }
        for (slot.name in slotNames(.Object)) {
            slot(.Object, slot.name) <- vect[slot.name]
        }
        return(.Object)
    }
)


setGeneric(
    name = "getMembers",
    def  = function(.Object, type = c("member", "symbol", "entrez"), ...) {
        standardGeneric("getMembers")
    }
)


#' @describeIn MSigDBSet return slot values of MSigDBSet object.
#' @examples
#'
#' data("msigdb")
#'
#' genes <- getMembers(
#'     MSigDB[[1]],
#'     type = "symbol"
#' )
#'
#' genes 
#' 
#' @export
setMethod(
    f         = "getMembers",
    signature = "MSigDBSet",
    def       = function(.Object, type = c("member", "symbol", "entrez"), ...) {
        store.call <- match.call()
        if (length(type) == 0) {
            stop(
                paste0(
                    as.list(store.call)[[1]],
                    ": type should be the subset of 'member', 'symbol', 'entrez'."
                )
            )
        } else if (any(is.na(match(type, c("member", "symbol", "entrez"))))) {
            stop(
                paste0(
                    as.list(store.call)[[1]],
                    ": type should be the subset of 'member', 'symbol', 'entrez'."
                )
            )
        }        
        mapping <- as.data.frame(
            t(
                as.data.frame(
                    lapply(
                        unname(unlist(strsplit(.Object@MEMBERS_MAPPING,"[|]"))),
                        strsplit,
                        split = ","
                    )
                )
            ),
            stringsAsFactors = FALSE
        )
        rownames(mapping) <- mapping[, 1]
        colnames(mapping) <- c("member", "symbol", "entrez")
        return(mapping[,type])
    }
)

################################################################################
##                                 EOF                                        ##
################################################################################
