#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                                                #
#              FUNCTIONS: PhD Chapter 2, David A Clarke, (date)                                  #
#                                                                                                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~#~# Function loads / installing packages required libraries----
loadLibrary <- function(packageList){
  
  packLoaded <- list.files(.libPaths())
  
  lapply(packageList,function(x){
    
    if(!(x %in% packLoaded)){install.packages(x)}
    
    require(x,character.only = TRUE)
    
  })
  
}

#~#~# Function imports GBIF occurrence data----

GetOcc <- function(sppList) {
  
  res.out <- lapply(sppList[1:length(sppList)], function(i) {
    
    print(i)
    OccS <- occ_search(scientificName = i, hasCoordinate = T, hasGeospatialIssue = F, fields = c("scientificName", "decimalLatitude", "decimalLongitude","coordinateUncertaintyInMeters","coordinatePrecision","geodeticDatum","basisOfRecord", "country"), limit = 200000, return = "data")
  })
  res.out <- do.call(rbind,res.out)
  
  return(res.out)
}

#~#~# Function converting NA to factor level----
NA2fctlvl <- function(df) {
  for(j in 1:ncol(df)) {
    levels(df[[j]]) <- c(levels(df[[j]]),"NA")
    df[[j]][is.na(df[[j]])] <- "NA"
  }
  return(df)
}

#~#~# Getting colour codes for specific input values #~#~#
#x = vector of data values
#breaks = vector of values related to where the data value cutoff points should be
spec_col <- function(x, breaks) {
       colours <- vector(length = length(x))
       
             if (x == breaks[1]) {
               colours <- '#313695'
             } 
             else if (x >= breaks[2] & x <= breaks[3]) {
               colours <- '#d73027'
             } 
             else if (x > breaks[3]) {
               colours <- '#72001a'
             }
        return(colours)
        
   }
