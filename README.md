# npi_search
Takes names and searches them against the NPPES database using the NPPES API function.  

I have a list of names that I would like to search for their NPI number using the NPI API (https://npiregistry.cms.hhs.gov/registry/help-api). NPI number is a unique identifier number. There are about 45,000 names that I want to see if there is a match in the csv file to the API. There is documentation of the API listed above and this is also helpful (https://npiregistry.cms.hhs.gov/api/demo?version=2.1). My goal is to get the correct NPI and all data as possible from the API. The example API call would be: https://npiregistry.cms.hhs.gov/api/?number=&enumeration_type=NPI-1&taxonomy_description=&first_name=kale&use_first_name_alias=&last_name=turner&organization_name=&address_purpose=&city=&state=&postal_code=&country_code=&limit=&skip=&version=2.1. Ultimately I want to use the location data for geocoding and to create a map.


#Yelp_API_Search
Objective: I want a list of every Chinese restaurant in the United States from the yelp fusion API using R. There are estimates of about 40,000 Chinese restaurants in the United States. My goal is then to map the restaurants using the given latitude and longitude. The Yelp fusion API allows for searching of "chinese food". Below is some example code that I put together. The limit on the number of returned results per search is 50.