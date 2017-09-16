library(jsonlite)
library(dplyr)

url_list <- 'https://openapi.etsy.com/v2/listings/active?api_key=gxx1r5uo1zpe5c55jf003xn5&category=art/painting&limit=100'

#list1 <- fromJSON(url_list)
#list2a <- fromJSON(paste0(url_list,'&page=2'))

pages1 <- list()

for(i in 1:500){
  mydata <- fromJSON(paste0(url_list,'&page=', i))
  message("Retrieving page ", i)
  pages1[[i]] <- mydata$results
  message(object.size(pages1))
  if(i == 100){
    Sys.sleep(60)
    message("Waiting 1 min at", i)
  } else if(i == 200){
    Sys.sleep(60)
    message("Waiting 1 min at", i)
  } else if(i == 300){
    Sys.sleep(60)
    message("Waiting 1 min at", i)
  } else if(i == 400){
    Sys.sleep(60)
    message("Waiting 1 min at", i)
  }
}

new_data <- rbind_pages(pages1)

## also need maj_data

uniq_data <- dplyr::anti_join(new_data, maj_data, by = "listing_id")


big_data <- rbind(maj_data,big_data)




