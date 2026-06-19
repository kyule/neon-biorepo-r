
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
#' @details Provides detailed NEON Biorepository sample type metadata. The sampleTypeCode is used to filter records in the getBiorepositoryRecords function. 
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
    
    protocol_lookup <- data.frame(
      collID = c(
        47,110,46,49,111,98,
        67,5,68,6,
        50,105,9,107,73,106,7,108,8,109,
        92,
        63,39,11,13,12,14,16,17,15,
        10,
        18,
        54,40,
        66,20,
        21,61,57,103,48,22,100,52,101,53,102,
        23,
        24,71,25,26,27,85,91,90,28,19,
        56,65,59,29,
        42,41,
        4,81,
        30,31,69,
        116,75,
        76,
        60,62,45,104,
        70
      ),
      protocol = c(
        rep("ALG – Periphyton And Phytoplankton Sampling", 6),
        rep("AMC – Aquatic Microbial Sampling", 4),
        rep("APL – Aquatic Plant, Bryophyte, Lichen And Macroalgae Clip Harvest Sampling", 10),
        "ASC – Sediment Sampling For Chemical And Physical Properties",
        rep("BET – Ground Beetle Sampling", 9),
        "BBC – Plant Belowground Biomass Sampling",
        "CFC – Canopy Foliage Sampling / HBP – Measurement Of Herbaceous Biomass",
        rep("DIV – Plant Diversity Sampling", 2),
        rep("FSL – Fish Sampling In Lakes / FSS - Fish Sampling In Wadeable Streams", 2),
        rep("INV – Aquatic Macroinvertebrate Sampling", 11),
        "LTR – Litterfall and Fine Woody Debris",
        rep("MAM – Small Mammal Sampling", 10),
        rep("MOS – Mosquito Sampling", 4),
        rep("Neon Tower/Sensor Protocols", 2),
        rep("Other", 2),
        rep("SLS – Soil Biogeochemical and Microbial Sampling", 3),
        rep("TCK – Tick and Tick-Borne Pathogen Sampling", 2),
        "TIS Soil Pit Sampling Protocol",
        rep("ZOO – Zooplankton Sampling In Lakes", 4),
        "FSL – Fish Sampling In Lakes / FSS - Fish Sampling In Wadeable Streams; MAM – Small Mammal Sampling"
      ),
      stringsAsFactors = FALSE
    )
    
    sampleTypes <- sampleTypes %>%
      left_join(protocol_lookup, by = "collID")
    
    sampleTypes <- sampleTypes %>% filter(available == 'TRUE')
    
    sampleTypes <- sampleTypes %>% select(collectionCode,
                                          collectionName,
                                          publicName,
                                          protocol,
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
                            'protocol','sampleGroup','sampleSubGroup','preservationType',
                            'homepage','sampleClasses','description','citation',
                            'rights', 'darwinCoreArchiveUrl')
    
    sampleTypes$homepage <- paste0('https://biorepo.neonscience.org/portal/collections/misc/neoncollprofiles.php?collid=',sampleTypes$homepage)
      
    sampleTypes$citation <- paste0('NEON (National Ecological Observatory Network) Biorepository. ',
                                   sampleTypes$sampleType,". Data accessed from ",sampleTypes$homepage,
                                   " on ", as.Date(Sys.time()),".")
    
      
    return(sampleTypes)
    
}

a<-getBiorepositorySampleTypes()

