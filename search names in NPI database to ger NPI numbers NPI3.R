#Created by Muffly and Surkov
# I have a list of names that I would like to search for their NPI number using the NPI API (https://npiregistry.cms.hhs.gov/registry/help-api). NPI number is a unique identifier number. There is documentation of the API listed above and this is also helpful (https://npiregistry.cms.hhs.gov/api/demo?version=2.1). My goal is to get the correct NPI and all data as possible from the API. The example API call would be: https://npiregistry.cms.hhs.gov/api/?number=&enumeration_type=NPI-1&taxonomy_description=&first_name=kale&use_first_name_alias=&last_name=turner&organization_name=&address_purpose=&city=&state=&postal_code=&country_code=&limit=&skip=&version=2.1.


# use library so R can make an API call
require("httr")
require("jsonlite")
require("dplyr")
library("magrittr") 
library("memoise")
library("tidyverse")


#create function trim that removes white spaces before and after value. One of the standard procedures in data preparation
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# optional. set working directory as Desktop where the output file will be saved. for Mac
setwd("~/Dropbox/npi_search")

# provide the full path or URL to the input file
input_file_path = "https://www.dropbox.com/s/3myst0596aqn96e/Physicians_total_drop_na_29.csv?raw=1"

output <- "output.csv"

# read the input CSV file into R's data frame object
input = read.csv(input_file_path)

# ONLY FOR QUICK TESTING. ONLY TAKES FIRST 10,000 ROWS FOR QUICK RESULT. 
# YOU CAN DELETE THIS LINE SO THE SCRIPT WILL CHECK ENTIRE INPUT
input<- head(input, 10000)

# Loop through each row (from 1 to the total number of rows) in the dataframe with users
for(i in 1:nrow(input)){

  # grab firstname, lastname, state from each row/person
  # write grabbed values into variables to be passed onto API call
  first_name <- input$firstname[i]
  middle_name <- input$middlename[i]
  last_name <- input$lastname[i]
  state <- input$state[i]
  
  # make API call for each row by dynamically populating API's querry with firstname, lastname, state for each row/person
  api_call <-  GET("https://npiregistry.cms.hhs.gov/api/", query = list(enumeration_type = "NPI-1", first_name=first_name, last_name=last_name,state=state, version=2.1))
  # convert API call resulting object into R's list object
  api_content <-  content(api_call)
  # grab the value of the total return result per search and write into variable
  api_content_count <-  api_content$result_count
  
  
  # if API call returns more than 0 results, add columns to the output:
  if(api_content_count>0){
  # reset the iteratorso we don't go out of bounds when selecting elements later 
  n = 1  
    # if we get more than 1 result per search
    if (api_content_count > 1){
      #loop though all results per search
      for (j in 1:length(api_content$results)){
        # if middle name returned in any result per search AND it is equal to the middle name in the input, PULL the NPI info for that user with matching middlename, otherwise pick the first result
        # functions around are just for more precise results like making sure to handle NA's, case sensitivity, no white spaces
        if(!is.null(api_content$results[[j]]$basic$middle_name) && trim(tolower(api_content$results[[j]]$basic$middle_name)) == trim(tolower(ifelse(is.na(middle_name), '', as.character(middle_name))))){
          # assigns the position of the middlename match to n
          n = j
          # optional, sets MatchCount to 1 since now the search is narrowed down
          api_content_count <- 1
          #breaking the search for matching middlename loop if successfully found the match so the n selector points to correct value
          break
        }
        else{
          # if not middlename searches matched, set the default selector value to 1 to pick the first API result for search parameters
          n = 1
        }
        #cat(j, trim(tolower(api_content$results[[j]]$basic$middle_name)), "\n")
      }
    }
    
    # RESULT_NPI column with NPI number from API for that row/person
    input$RESULT_NPI[i] <- api_content$results[[n]]$number
    # RESULT_Address1 column with Address1 value from API for that row/person
    input$RESULT_Address1[i] <- api_content$results[[n]]$addresses[[1]]$address_1
    # RESULT_City column with City value from API for that row/person
    input$RESULT_City[i] <- api_content$results[[n]]$addresses[[1]]$city
    # RESULT_IsPresent column with TRUE/FALSE flag where input row/user has been found in API
    input$RESULT_IsPresent[i] <- 'True'
    # IMPORTANT: RESULT_MatchCount column shows how many people were found in NPI with the SAME firsname, lastname living in the same state
    input$RESULT_MatchCount[i] <- api_content_count
    
    # Adding all possible result columns from API
    input$RESULT_enumeration_type[i] <- api_content$results[[n]]$enumeration_type
    input$RESULT_last_updated_epoch[i] <- api_content$results[[n]]$last_updated_epoch
    input$RESULT_created_epoch[i] <- api_content$results[[n]]$created_epoch
    input$RESULT_first_name[i] <- api_content$results[[n]]$basic$first_name
    input$RESULT_last_name[i] <- api_content$results[[n]]$basic$last_name
    input$RESULT_sole_proprietor[i] <- api_content$results[[n]]$basic$sole_proprietor
    input$RESULT_gender[i] <- api_content$results[[n]]$basic$gender
    input$RESULT_enumeration_date[i] <- api_content$results[[n]]$basic$enumeration_date
    input$RESULT_last_updated[i] <- api_content$results[[n]]$basic$last_updated
    input$RESULT_status[i] <- api_content$results[[n]]$basic$status
    input$RESULT_name[i] <- api_content$results[[n]]$basic$name
    input$RESULT_country_code[i] <- api_content$results[[n]]$addresses[[1]]$country_code
    input$RESULT_country_name[i] <- api_content$results[[n]]$addresses[[1]]$country_name
    input$RESULT_address_purpose[i] <- api_content$results[[n]]$addresses[[1]]$address_purpose
    input$RESULT_address_type[i] <- api_content$results[[n]]$addresses[[1]]$address_type
    input$RESULT_address_2[i] <- api_content$results[[n]]$addresses[[1]]$address_2
    input$RESULT_state[i] <- api_content$results[[n]]$addresses[[1]]$state
    input$RESULT_postal_code[i] <- api_content$results[[n]]$addresses[[1]]$postal_code
    input$RESULT_telephone_number[i] <- api_content$results[[n]]$addresses[[1]]$telephone_number
    input$RESULT_code[i] <- api_content$results[[n]]$taxonomies[[1]]$code
    input$RESULT_desc[i] <- api_content$results[[n]]$taxonomies[[1]]$desc
    input$RESULT_primary[i] <- api_content$results[[n]]$taxonomies[[1]]$primary
    input$RESULT_taxonomystate[i] <- api_content$results[[n]]$taxonomies[[1]]$state
    input$RESULT_license[i] <- api_content$results[[n]]$taxonomies[[1]]$license
    
    

  }
  # if API call returns 0 results, add columns to the output with respective values instead of above
  else{
    input$RESULT_NPI[i] = 'NA'
    input$RESULT_Address1[i] <- 'NA'
    input$RESULT_City[i] <- 'NA'
    input$RESULT_IsPresent[i] <- 'False'
    input$RESULT_MatchCount[i] <- api_content_count
    
    input$RESULT_enumeration_type[i] <- 'NA'
    input$RESULT_last_updated_epoch[i] <- 'NA'
    input$RESULT_created_epoch[i] <- 'NA'
    input$RESULT_first_name[i] <- 'NA'
    input$RESULT_last_name[i] <- 'NA'
    input$RESULT_sole_proprietor[i] <- 'NA'
    input$RESULT_gender[i] <- 'NA'
    input$RESULT_enumeration_date[i] <- 'NA'
    input$RESULT_last_updated[i] <- 'NA'
    input$RESULT_status[i] <- 'NA'
    input$RESULT_name[i] <- 'NA'
    input$RESULT_country_code[i] <- 'NA'
    input$RESULT_country_name[i] <- 'NA'
    input$RESULT_address_purpose[i] <- 'NA'
    input$RESULT_address_type[i] <- 'NA'
    input$RESULT_address_2[i] <- 'NA'
    input$RESULT_state[i] <- 'NA'
    input$RESULT_postal_code[i] <- 'NA'
    input$RESULT_telephone_number[i] <- 'NA'
    input$RESULT_code[i] <- 'NA'
    input$RESULT_desc[i] <- 'NA'
    input$RESULT_primary[i] <- 'NA'
    input$RESULT_taxonomystate[i] <- 'NA'
    input$RESULT_license[i] <- 'NA'
  }
  
  # optional line of code. Shows progress. Countdown of rows
  cat("\r", " remaining: ", nrow(input) - i, "\r")
}

# write output into output.csv file stored on the desktop
write.csv(input, output, row.names=FALSE)

# optional. Show output in R window. Only works if run from R Studio
View(input)

