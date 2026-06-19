
##############################################################################################
#' @title Get NEON Biorepository sample type information

#' @author
#' Kelsey Yule \email{kmyule@asu.edu}

#' @description
#' Get information on available NEON Biorepository sample types
#'
#
#' @return A table of Biorepository sample type information
#' 
#' @details Provides detailed NEON Biorepository sample type metadata
#' 
#' @examples	
#' # Get 
#' \dontrun{
#' sampleTypes <- getBiorepositorySampleTypes()
#' }

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Kelsey Yule (2026-06-18): original creation

##############################################################################################

getBiorepositorySampleTypes <- function() {
  
    library(httr)
    library(jsonlite)
    library(dplyr)
    
    url <- "https://biorepo.neonscience.org/portal/api/v2/collection"
    
    res <- GET(
      url,
      add_headers(
        accept = "application/json"
      )
    )
    
    stop_for_status(res)
    
    sampleTypes <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    
    sampleTypes <- sampleTypes$results
    
    sampleTypes <- sampleTypes %>% filter(available == 'TRUE')
    
    sampleTypes <- sampleTypes %>% select(collectionCode,
                                          collectionName,
                                          publicName,
                                          productID,
                                          higherTaxon,
                                          lowerTaxon,
                                          sampleType,
                                          collID,
                                          datasetID,
                                          datasetname,
                                          bibliographicCitation,
                                          rights,
                                          dwcaUrl)
    
    names(sampleTypes) <- c('sampleTypeCode','sampleType','displayName','dataProductIDs',
                            'sampleGroup','sampleSubGroup','preservationType','homepage',
                            'sampleClasses','description','citation',
                            'rights', 'darwinCoreArchiveUrl')
    
    sampleTypes$homepage <- paste0('https://biorepo.neonscience.org/portal/collections/misc/neoncollprofiles.php?collid=',sampleTypes$homepage)
      
    sampleTypes$citation <- paste0('NEON (National Ecological Observatory Network) Biorepository. ',
                                   sampleTypes$sampleType,". Data accessed from ",sampleTypes$homepage,
                                   " on ", as.Date(Sys.time()),".")
    
      
    return(sampleTypes)
    
}

a<-getBiorepositorySampleTypes()

