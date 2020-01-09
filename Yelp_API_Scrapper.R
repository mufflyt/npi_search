#Muffly and Bouhestine
# Objective: I want a list of every Chinese restaurant in the United States from the yelp fusion API using R. There are estimates of about 40,000 Chinese restaurants in the United States. My goal is then to map the restaurants using the given latitude and longitude. The Yelp fusion API allows for searching of "chinese food". Below is some example code that I put together. The limit on the number of returned results per search is 50.

library(yelpr)
library(data.table)
library(tidyverse)
library(jsonlite)

yelp_api_key <- #see YelpAPILogin.rtf

getwd()
counties = fread("~/Dropbox/npi_search/Counties/US_Countiess.csv", sep = ",")
names_counties = counties[["X4"]]

for(county in names_counties){
  
  my_offset = 0
  num_output = 1
  
  while(my_offset<950){
    
    cat(paste0("current county: ",num_output,"\n"))
    cat(paste0("current output: ",num_output,"\n"))
    cat(paste0("current offset: ",my_offset,"\n"))
    cat("######################################\n")
    
    tryCatch({
      
      result = business_search(api_key = yelp_api_key,
                               location = county,
                               term = "chinese",
                               limit = 50,
                               offset = my_offset)
      
      my_table = result$businesses
      
      my_table = my_table[, c("id","name","categories","coordinates","location")]
      
      my_table = flatten(my_table)
      
      my_table = my_table[, c("id","name","categories","coordinates.latitude","coordinates.longitude","location.zip_code")]
      
      my_table = my_table %>% mutate(categories = as.character(categories))
      
      fwrite(my_table,
             paste0("~/Dropbox/npi_search/YelpOutput/Output_",county,"_",num_output,".csv"),
             sep = ";")

    }, error = function(e) {
      
      cat("######################################\n")
      cat("This County does not have any more chinese restaurants \n")
      cat("######################################\n")
    })

    num_output = num_output+1
    my_offset = my_offset+50
  }
}

files = list.files("~/Dropbox/npi_search/YelpOutput/", full.names = TRUE)

myfiles = lapply(files, fread, sep = ";", colClasses = "character")

final_output = rbindlist(myfiles)
final_output = unique(final_output)
final_output = final_output[!duplicated(id)]

fwrite(final_output,
       "~/Dropbox/npi_search/YelpOutput/chinese_restaurants_usa.csv",
       sep=";")
