### Mammals ###

## Package Loading:
library(tidyverse) #package used for data cleaning,contains lots of common packages
library(tidyr)
library(dplyr) #package used for data cleaning and organization

## Mammal data cleaning ##

  MammalAll <- read.csv("/Users/macbookpro/Documents/Hybridization/MDD_v2.0_6759species.csv")
  #View(MammalAll)
  
  MammalIntroduced <- read.csv("/Users/macbookpro/Documents/Hybridization/DAMA_GADM.csv")
  #View(MammalIntroduced)
  
  # Correcting weird typo in data set:
  
  MammalIntroduced[144, "GID_1"] <- "BES.3_1"
  MammalIntroduced[144, "NAME_1"] <- "Sint Eustasias and Saba"
  
  # Making scientific binomial normally formatted
  MammalAll <- MammalAll %>% mutate(sciName = gsub("_", " ", sciName))
  
  # Splitting the Introduced Mammals species binomial
  MammalIntroduced$Binomial2 <- MammalIntroduced$Binomial
  MammalIntroduced$Binomial <- MammalIntroduced %>%
    separate(Binomial, into = c("Genus", "Species"), sep = " ")
  
  # Fixing accent issue on place names in the MammalIntroduced csv
  
    # Function to decode and remove accents from the names
    standardize_name <- function(name) {
      
      contains_apostrophe <- grepl("'", name)
      contains_slant <- grepl("`", name)
      
      # First, convert to proper characters (removes encoding issues)
      decoded_loc <- iconv(name, from = "latin1", to = "UTF-8")
      decoded_loc <- iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT")
      
      # taking out unnecessary apostrophes
      if(contains_apostrophe == FALSE){
        decoded_loc <- gsub("'", "", decoded_loc)
      }
      if(contains_slant == FALSE){
        decoded_loc <- gsub("`", "", decoded_loc)
      }
      # taking out other weird characters
      decoded_loc <- gsub("~", "", decoded_loc)
      decoded_loc <- gsub("\"", "", decoded_loc)
      decoded_loc <- gsub("\\^", "", decoded_loc)
      
      return(decoded_loc)
    }
    
  # Apply the function to the vector of place names
  MammalIntroduced$standard_locs <- sapply(MammalIntroduced$NAME_1, standardize_name)
    
  #Locality reformatting
  MammalIntroduced$Locality <- paste0("[' ", MammalIntroduced$NAME_0," - ", MammalIntroduced$standard_locs, "']")
    
  # Combine values in new Localities column by species
  Localities <- MammalIntroduced %>%
    group_by(Binomial2) %>%
    summarize(concatenated_values = paste(Locality, collapse = "", .groups = ''))
    print(Localities) #this is the new Introduced Mammals data frame with each unique species name 
    #in the first column and the list of localities in the second column
    
### Tested to here ^^^ ###
    
    Localities$Binomial3 <- Localities %>%
      separate(Binomial2, into = c("Genus", "Species"), sep = " ")
    
    ## Finding list of all congeners that exist for the Introduced Species
    congenermatch <- function(introspp){
     
      congenerlist <- MammalAll %>% 
        filter(genus == introspp) %>%
        pull(sciName)  # Pull out the sciName column from the subset
      
      if(length(congenerlist) > 0) {
        return(paste(congenerlist, collapse = "; "))  # Concatenate the names
      } else {
        return(NA)  # Return NA if no matches are found
      }

    }
    
    Localities$allcongeners <- sapply(Localities$Binomial3$Genus, congenermatch)

#Make it into a nice data frame all together
    
  introduced_binomial <- Localities$Binomial2
  introduced_localities <- Localities$concatenated_values
  all_congeners <- Localities$allcongeners
  
  OutputData <- data.frame(
    IntroducedBinomial = Localities$Binomial2,
    IntroducedLocalities = Localities$concatenated_values,
    AllCongeners = Localities$allcongeners)

  view(OutputData)
  write.csv(OutputData,"MammalInitialOutput.csv")



