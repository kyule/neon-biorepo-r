
##############################################################################################
#' @title Get Darwin Core occurrence record data for NEON Biorepository samples

#' @author
#' Kelsey Yule \email{kmyule@asu.edu}

#' @description
#' Get NEON Biorepository sample data based on sample types, sites, and months
#'
#' @param sampleTypeIDs Either NA, meaning all available sampleTypes, or a character vector of sampleTypeIDs. Defaults to NA [character]
#' @param sites Either NA, meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to NA.
#' @param startdate Start date of a range for filtering collecting events in "YYYY-MM" format. Note that this is a filter by the start of the collection period. The date the sample was collected from the field may be in the next month. Defaults to NA, but required if enddate is supplied.
#' @param enddate End date of a range for filtering collecting events in "YYYY-MM" format. Note that this is a filter by the start of the collection period. The date the sample was collected from the field may be in the next month. Defaults to NA, but required if startdate is supplied.
#' @param TODO after versioning: release Versioned NEON Biorepository data release. Defaults to "provisional" for current data. [character]
#' @param TODO after token implementation: NEONtoken NEON token for access to NEON data. Required [character]
#' @return A table of NEON Biorepository sample records
#' 
#' @details Related NEON samples can be connected to each other in a parent-child hierarchy. Parents can have one or many children, and children can have one or many parents. Sample hierarchies can be simple or complex - for example, particulate mass samples (dust filters) have no parents or children, whereas water chemistry samples can be subsampled for dissolved gas, isotope, and microbial measurements. This function finds all ancestors and descendants of the focal sample (the sampleNode), and all of their relatives, and so on recursively, to provide the entire hierarchy. See documentation for each data product for more specific information.
#' 
#' @examples	
#' # Find mammal blood samples collected at at the Santa Rita Experimental Range in 2021
#' \dontrun{
#'2021_SRER_mammal_blood <- getBiorepositoryRecords(sampleTypeIDs = "MAMC-BL", sites = "SRER", startdate = "2021-01", enddate = "2021-12", release = "provisional", token = NeonToken)
#' }

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Kelsey Yule (2026-06-19): original creation

##############################################################################################

getBiorepositoryRecords <- function(sampleTypeIDs = NA, sites = NA, startdate = NA, enddate= NA, release = "provisional", token = NA) {
  
  # TOKEN and versioning must be added
  # date sanity checks
    if (length(startdate) > 1 || length(enddate) > 1) {
      stop('Only one start and/or end date can be supplied.')
    } else if (!is.na(startdate) && is.na(enddate)) {
      stop('End date is required if start date supplied.')
    } else if(is.na(startdate) && !is.na(enddate)) {
      stop('Start date is required if end date supplied.')
    } else if (!is.na(startdate) && grepl("^\\d{4}-\\d{2}$", startdate) == FALSE) {
      stop('Start date not formatted as YYYY-MM.')
    } else if (!is.na(enddate) && grepl("^\\d{4}-\\d{2}$", enddate) == FALSE) {
      stop('End date not formatted as YYYY-MM.')
    } else if(!is.na(startdate) && !is.na(enddate) && enddate < startdate) {
      stop('End date must be later than start date.')
    }
    
  # find collections based on sampleTypeIDs

    collectionurl <- "https://biorepo.neonscience.org/portal/api/v2/collection"
      
    collectioncall <- GET(
      collectionurl,
      add_headers(
      accept = "application/json"
      )
    )
      
    stop_for_status(collectioncall)
      
    allcollections <- fromJSON(content(collectioncall, "text", encoding = "UTF-8"), flatten = TRUE)
      
    allcollections <- allcollections$results
      
    if (any(!is.na(sampleTypeIDs))) {
      
      validcollections <- allcollections[allcollections$collectionCode %in% sampleTypeIDs,]
      sampleTypesNotFound <- sampleTypeIDs[!sampleTypeIDs %in% validcollections$collectionCode]
      
      if (nrow(validcollections) == 0) {
        stop('None of the sampleTypeIDs supplied are valid.')
      } else if(length(sampleTypesNotFound) > 0){
        warning(paste0("The following sampleTypeIDs are invalid: ", paste(sampleTypesNotFound, collapse = ", ")))
      }
      
      collections <- validcollections %>% select(collID,collectionCode)
      
    } else collections <-allcollections %>% select(collID,collectionCode)
    
  
    # find datasets based on site codes

    dataseturl <- "https://biorepo.neonscience.org/portal/api/v2/occurrence/dataset?limit=1000&offset=0" 

    datasetcall <- GET(
      dataseturl,
      add_headers(
        accept = "application/json"
      )
    )
    
    stop_for_status(datasetcall)
    
    alldatasets <- fromJSON(content(datasetcall, "text", encoding = "UTF-8"), flatten = TRUE)
    
    alldatasets <- alldatasets$results
    alldatasets <- alldatasets[alldatasets$datasetid %in% 21:131,] 
    
    if (any(!is.na(sites))) {
      
      validsites <- alldatasets[alldatasets$name %in% sites,]
      sitesNotFound <- sites[!sites %in% validsites$name]
      
      if (nrow(validsites) == 0) {
        stop('None of the site codes supplied are valid.')
      } else if(length(sitesNotFound) > 0){
        warning(paste0("The following site codes are invalid: ", paste(sitesNotFound, collapse = ", ")))
      }
      
      sitelist <- validsites %>% select(datasetid,name)
      
    } else sitelist <-alldatasets %>% select(datasetid,name)
    

    # create occurrences
    occurrencecalls <- merge(sitelist, collections, by = NULL)

    #occurrencecalls$url <- "https://biorepo.neonscience.org/portal/api/v2/occurrence?limit=300"
    
    ### FOR TESTING ON 3.3 ONLY
    
    occurrencecalls$url <- "https://biorepo.neonscience.org/portal/api/v2/occurrence/search?limit=300"
    
    
    if (!is.na(startdate)) {
      occurrencecalls$url <- paste0(occurrencecalls$url, "&eventDateMin=", paste0(startdate,"-01"), "&eventDateMax=", paste0(enddate,"-31"))
    }
    
    #occurrencecalls$url <- paste0(occurrencecalls$url, "&collid=", occurrencecalls$collID, "&datasetID=", occurrencecalls$datasetid)
    
    ### FOR TESTING ON 3.3 ONLY
    
    occurrencecalls$url <- paste0(occurrencecalls$url, "&collid=", occurrencecalls$collID)
    
    
    # find occurrences associated with provided parameters

    occur_list <- vector("list", nrow(occurrencecalls))
    
    for (i in seq_len(nrow(occurrencecalls))){
      
      occurrencecall <- GET(
        occurrencecalls$url[i],
        add_headers(
          accept = "application/json"
        )
      )
      
      stop_for_status(occurrencecall)
      
      occurrenceresult <- fromJSON(content(occurrencecall, "text", encoding = "UTF-8"), flatten = TRUE)
      
      count <- occurrenceresult$count
      
      occurrencesByTypeAndSite <- occurrenceresult$results
      
      message(paste0("Found ", count, " matching sample records of sampleTypeID ", occurrencecalls$collectionCode[i], " at site ", occurrencecalls$name[i], " to be retrieved."))
      
      if (count > 300){
        
        offset <- 300
        
        while (offset < count) {
          
          occurrencecall <- GET(
            paste0(occurrencecalls$url[i], "&offset=", offset),
            add_headers(
              accept = "application/json"
            )
          )
          
          stop_for_status(occurrencecall)
          
          occurrenceresults <- fromJSON(content(occurrencecall, "text", encoding = "UTF-8"), flatten = TRUE)
          
          occurrencesByTypeAndSite <-rbind(occurrencesByTypeAndSite,occurrenceresults$results)
          
          if (offset %% 1000 < 300) {
            message(paste0(min(offset, count)," of ", count," ", occurrencecalls$collectionCode[i]," records from ", occurrencecalls$name[i]," retrieved."))
          }
          
          offset <- offset + 300
          
        }
        
      }
      
      if (count > 0) {
        occur_list[[i]] <- occurrencesByTypeAndSite
      }
      
    }
    
    
    occur_list <- Filter(Negate(is.null), occur_list)
    
    if (length(occur_list) == 0) {
      message("No matching sample records for the supplied parameters.")
      return(data.frame())
    }
    
    occur <- do.call(rbind, occur_list)
    occur <- occur[!duplicated(occur$occid),]

    message(paste0("Successfully retrieved ", nrow(occur), " matching sample records."))
    
    return(occur)
  
}
