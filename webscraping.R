packrat::init()
library(rvest)
library(stringr)
########################################### 1 #####################
etsy_listing <- read_html("https://www.etsy.com/listing/271284798/large-abstract-painting-original-oil?ref=shop_home_active_16")

 
  
title <- etsy_listing %>% html_nodes('h1 span') %>% html_text()

price.str <- etsy_listing%>% html_nodes(xpath = '//*[(@id = "listing-price")]')%>% 
  html_text(trim = T)


price <- as.numeric(str_match_all(price.str, '[0-9]+.[0-9]+'))

materials <- etsy_listing %>% html_nodes(xpath = '//*[(@id = "overview-materials")]') %>%
  html_text()


fav_list <- etsy_listing%>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "properties", " " ))]') %>%
  html_text()
  
#str_match_all(fav_list[10], ' (\\d+)\\ ')
fav_vec <- unlist(str_split(unlist(str_match_all(fav_list, '[0-9]+ people')), " "))

num_favorers <- as.numeric(str_match(fav_vec, '[0-9]+')[1])

quantity <- 1

who_made <- 'i_did'

when_made <- '2010_2017'

is_customizable <- FALSE

has_variations <- FALSE


test_df <- data.frame(price,quantity,num_favorers,who_made,when_made,
                      is_customizable, has_variations)
test_df$art_type <- character(length = nrow(test_df))

if(grepl('oil', materials, ignore.case = T)){
  test_df$art_type <- 'oil'
} else if(grepl('acrylic', materials, ignore.case = T)){
  test_df$art_type <- 'acrylic'
} else if(grepl('watercolor', materials, ignore.case = T)){
  test_df$art_type <- 'watercolor'
} else {
  test_df$art_type <- 'prints'
}

test_df$raw_mat <- character(length = nrow(test_df))

if(grepl('canvas', materials, ignore.case = T)){
  test_df$raw_mat <- 'canvas'
} else if(grepl('watercolor paper', materials, ignore.case = T)){
  test_df$raw_mat <- 'wat.paper'
}

test_df

########################################2 #######################
new_url <- read_html('https://www.etsy.com/listing/479418975/abstract-colorful-original-oil-painting?ref=listing-shop-header-0')

title <- new_url %>% html_nodes('h1 span') %>% html_text()

price.str <- new_url%>% html_nodes(xpath = '//*[(@id = "listing-price")]')%>% 
  html_text(trim = T)

price <- as.numeric(str_match_all(price.str, '[0-9]+.[0-9]+'))

materials <- new_url %>% html_nodes(xpath = '//*[(@id = "overview-materials")]') %>%
  html_text()


fav_list <- new_url%>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "properties", " " ))]') %>%
  html_text()

#str_match_all(fav_list[10], ' (\\d+)\\ ')
fav_vec <- unlist(str_split(unlist(str_match_all(fav_list, '[0-9]+ people')), " "))

num_favorers <- as.numeric(str_match(fav_vec, '[0-9]+')[1])

quantity <- 1

who_made <- 'i_did'

when_made <- '2010_2017'

is_customizable <- FALSE

has_variations <- FALSE


test_df <- data.frame(price,quantity,num_favorers,who_made,when_made,
                      is_customizable, has_variations)
test_df$art_type <- character(length = nrow(test_df))

if(grepl('oil', materials, ignore.case = T)){
  test_df$art_type <- 'oil'
} else if(grepl('acrylic', materials, ignore.case = T)){
  test_df$art_type <- 'acrylic'
} else if(grepl('watercolor', materials, ignore.case = T)){
  test_df$art_type <- 'watercolor'
} else {
  test_df$art_type <- 'prints'
}

test_df$raw_mat <- character(length = nrow(test_df))

if(grepl('canvas', materials, ignore.case = T)){
  test_df$raw_mat <- 'canvas'
} else if(grepl('watercolor paper', materials, ignore.case = T)){
  test_df$raw_mat <- 'wat.paper'
}

test_df
######################################## 3 #################
new_url2 <- read_html('https://www.etsy.com/listing/278092804/horse-print-equestrian-equine-art?ga_order=most_relevant&ga_search_type=all&ga_view_type=gallery&ga_search_query=Horse%20print,%20equestrian,%20equine%20art%20,%20abstract%20horse%20painting,%20equine%20watercolor%20expressions,%20horse%20lover,%20decor,%20wild%20horse%20gifts,%20dressage&ref=sr_gallery_1')

title <- new_url2 %>% html_nodes('h1 span') %>% html_text()

price.str <- new_url2%>% html_nodes(xpath = '//*[(@id = "listing-price")]')%>% 
  html_text(trim = T)

price <- as.numeric(str_match_all(price.str, '[0-9]+.[0-9]+'))

materials <- new_url2 %>% html_nodes(xpath = '//*[(@id = "overview-materials")]') %>%
  html_text()


fav_list <- new_url2%>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "properties", " " ))]') %>%
  html_text()

#str_match_all(fav_list[10], ' (\\d+)\\ ')
fav_vec <- unlist(str_split(unlist(str_match_all(fav_list, '[0-9]+ people')), " "))

num_favorers <- as.numeric(str_match(fav_vec, '[0-9]+')[1])

quantity_toggle <- new_url2 %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), 
             concat( " ", "mb-lg-2", " " ))]')%>%
  html_text()

quantity <- as.numeric(str_match_all(quantity_toggle, '[0-9]+'))

who_made <- 'i_did'

when_made <- '2010_2017'

is_customizable <- FALSE


variation_toggle <- new_url2 %>%
  html_nodes(xpath = '//*[(@id = "inventory-variation-select-0")]')%>%
  html_text()

has_variations <- FALSE
if(length(variation_toggle) > 0){
  has_variations <- TRUE
} else {
  has_variations <- FALSE
}


test_df <- data.frame(price,quantity,num_favorers,who_made,when_made,
                      is_customizable, has_variations)
test_df$art_type <- character(length = nrow(test_df))

if(grepl('oil', materials, ignore.case = T)){
  test_df$art_type <- 'oil'
} else if(grepl('acrylic', materials, ignore.case = T)){
  test_df$art_type <- 'acrylic'
} else if(grepl('watercolor', materials, ignore.case = T)){
  test_df$art_type <- 'watercolor'
} else {
  test_df$art_type <- 'prints'
}

test_df$raw_mat <- character(length = nrow(test_df))

if(grepl('canvas', materials, ignore.case = T)){
  test_df$raw_mat <- 'canvas'
} else if(grepl('watercolor paper', materials, ignore.case = T)){
  test_df$raw_mat <- 'wat.paper'
}

test_df

################################## 4 ###################

new_url3 <- read_html('https://www.etsy.com/listing/456860846/custom-natural-tone-painted-pet-portrait?ga_order=most_relevant&ga_search_type=all&ga_view_type=gallery&ga_search_query=&ref=sr_gallery_22')

title <- new_url3 %>% html_nodes('h1 span') %>% html_text()

price.str <- new_url3%>% html_nodes(xpath = '//*[(@id = "listing-price")]')%>% 
  html_text(trim = T)

price <- as.numeric(str_match_all(price.str, '[0-9]+.[0-9]+'))

materials <- new_url3 %>% html_nodes(xpath = '//*[(@id = "overview-materials")]') %>%
  html_text()


fav_list <- new_url3%>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "properties", " " ))]') %>%
  html_text()

#str_match_all(fav_list[10], ' (\\d+)\\ ')
fav_vec <- unlist(str_split(unlist(str_match_all(fav_list, '[0-9]+ people')), " "))

num_favorers <- as.numeric(str_match(fav_vec, '[0-9]+')[1])

quantity_toggle <- new_url3 %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), 
             concat( " ", "mb-lg-2", " " ))]')%>%
  html_text()

quantity <- as.numeric(str_match_all(quantity_toggle, '[0-9]+'))


who_made <- 'i_did'

when_made <- '2010_2017'

is_customizable <- FALSE


variation_toggle <- new_url3 %>%
  html_nodes(xpath = '//*[(@id = "inventory-variation-select-0")]')%>%
  html_text()

has_variations <- FALSE
if(length(variation_toggle) > 0){
  has_variations <- TRUE
} else {
  has_variations <- FALSE
}


test_df <- data.frame(price,quantity,num_favorers,who_made,when_made,
                      is_customizable, has_variations)
test_df$art_type <- character(length = nrow(test_df))

if(length(materials) > 0){
  if(grepl('oil', materials, ignore.case = T)){
    test_df$art_type <- 'oil'
  } else if(grepl('acrylic', materials, ignore.case = T)){
    test_df$art_type <- 'acrylic'
  } else if(grepl('watercolor', materials, ignore.case = T)){
    test_df$art_type <- 'watercolor'
  } else {
    test_df$art_type <- 'prints'
  }
} else if(grepl('oil', title, ignore.case = T)){
  test_df$art_type <- 'oil'
  } else if(grepl('acrylic', title, ignore.case = T)){
    test_df$art_type <- 'acrylic'
  } else if(grepl('watercolor', title, ignore.case = T)){
    test_df$art_type <- 'watercolor'
  } else {
    test_df$art_type <- 'prints'
}
  


test_df$raw_mat <- character(length = nrow(test_df))

if(length(materials) > 0){
  if(grepl('canvas', materials, ignore.case = T)){
    test_df$raw_mat <- 'canvas'
  } else if(grepl('watercolor | paper', materials, ignore.case = T)){
    test_df$raw_mat <- 'wat.paper'
  } 
} else if(grepl('canvas', description, ignore.case = T)){
  test_df$raw_mat <- 'canvas'
  } else if(grepl('watercolor | paper', description, ignore.case = T)){
    test_df$raw_mat <- 'wat.paper'
}


test_df

####################################### 5 ###########################
new_url4 <- read_html('https://www.etsy.com/listing/151997088/elk-original-watercolor-painting?ga_order=most_relevant&ga_search_type=all&ga_view_type=gallery&ga_search_query=&ref=sr_gallery_42')

title <- new_url4 %>% html_nodes('h1 span') %>% html_text()

description <- new_url4 %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "break-word", " " ))]') %>%
  html_text()

price.str <- new_url4%>% html_nodes(xpath = '//*[(@id = "listing-price")]')%>% 
  html_text(trim = T)

price <- as.numeric(str_match_all(price.str, '[0-9]+.[0-9]+'))

materials <- new_url4 %>% html_nodes(xpath = '//*[(@id = "overview-materials")]') %>%
  html_text()


fav_list <- new_url4%>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "properties", " " ))]') %>%
  html_text()

#str_match_all(fav_list[10], ' (\\d+)\\ ')
fav_vec <- unlist(str_split(unlist(str_match_all(fav_list, '[0-9]+ people')), " "))

num_favorers <- as.numeric(str_match(fav_vec, '[0-9]+')[1])

quantity_toggle <- new_url4 %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), 
             concat( " ", "mb-lg-2", " " ))]')%>%
  html_text()

quantity <- as.numeric(str_match_all(quantity_toggle, '[0-9]+'))

who_made <- 'i_did'

when_made <- '2010_2017'

is_customizable <- FALSE


variation_toggle <- new_url4 %>%
  html_nodes(xpath = '//*[(@id = "inventory-variation-select-0")]')%>%
  html_text()

has_variations <- FALSE
if(length(variation_toggle) > 0){
  has_variations <- TRUE
} else {
  has_variations <- FALSE
}


test_df <- data.frame(price,quantity,num_favorers,who_made,when_made,
                      is_customizable, has_variations)
test_df$art_type <- character(length = nrow(test_df))

if(length(materials) > 0){
  if(grepl('oil', materials, ignore.case = T)){
    test_df$art_type <- 'oil'
  } else if(grepl('acrylic', materials, ignore.case = T)){
    test_df$art_type <- 'acrylic'
  } else if(grepl('watercolor', materials, ignore.case = T)){
    test_df$art_type <- 'watercolor'
  } else {
    test_df$art_type <- 'prints'
  }
} else if(grepl('oil', title, ignore.case = T)){
  test_df$art_type <- 'oil'
} else if(grepl('acrylic', title, ignore.case = T)){
  test_df$art_type <- 'acrylic'
} else if(grepl('watercolor', title, ignore.case = T)){
  test_df$art_type <- 'watercolor'
} else {
  test_df$art_type <- 'prints'
}



test_df$raw_mat <- character(length = nrow(test_df))

if(length(materials) > 0){
  if(grepl('canvas', materials, ignore.case = T)){
    test_df$raw_mat <- 'canvas'
  } else if(grepl('watercolor | paper', materials, ignore.case = T)){
    test_df$raw_mat <- 'wat.paper'
  } 
} else if(grepl('canvas', description, ignore.case = T)){
  test_df$raw_mat <- 'canvas'
} else if(grepl('watercolor | paper', description, ignore.case = T)){
  test_df$raw_mat <- 'wat.paper'
}

test_df <- as.data.frame(test_df, stringsAsFactors = T)
str(test_df)

test_df$art_type <- factor(test_df$art_type)
test_df$raw_mat <- factor(test_df$raw_mat)

levels(test_df$who_made) <- levels(data_train4$who_made)
levels(test_df$when_made) <- levels(data_train4$when_made)
levels(test_df$art_type) <- levels(data_train4$art_type)
levels(test_df$raw_mat) <- levels(data_train4$raw_mat)

str(test_df)
str(data_train4)

test.pred <- predict(rfm1_d4, test_df[-1])

log(test_df$price)

########### function building #########################
test_url <- "https://www.etsy.com/listing/271284798/large-abstract-painting-original-oil?ref=shop_home_active_16"

scrape_from_url <- function(url){
  library(rvest)
  new_url <- read_html(url)
  title <- new_url %>% html_nodes('h1 span') %>% html_text()
  description <- new_url %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "break-word", " " ))]') %>%
    html_text()
  price.str <- new_url%>% html_nodes(xpath = '//*[(@id = "listing-price")]')%>% 
    html_text(trim = T)
  price <- as.numeric(str_match_all(price.str, '[0-9]+.[0-9]+'))
  materials <- new_url %>% html_nodes(xpath = '//*[(@id = "overview-materials")]') %>%
    html_text()
  fav_list <- new_url%>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "properties", " " ))]') %>%
    html_text()
  fav_vec <- unlist(str_split(unlist(str_match_all(fav_list, '[0-9]+ people')), " "))
  num_favorers <- as.numeric(str_match(fav_vec, '[0-9]+')[1])
  quantity <- 1
  who_made <- 'i_did'
  when_made <- '2010_2017'
  is_customizable <- FALSE
  variation_toggle <- new_url %>%
    html_nodes(xpath = '//*[(@id = "inventory-variation-select-0")]')%>%
    html_text()
  variation_toggle
  has_variations <- FALSE
  if(length(variation_toggle) > 0){
    has_variations <- TRUE
  } else {
    has_variations <- FALSE
  }
  test_df <- data.frame(price,quantity,num_favorers,who_made,when_made,
                        is_customizable, has_variations)
  test_df$art_type <- character(length = nrow(test_df))
  if(length(materials) > 0){
    if(grepl('oil', materials, ignore.case = T)){
      test_df$art_type <- 'oil'
    } else if(grepl('acrylic', materials, ignore.case = T)){
      test_df$art_type <- 'acrylic'
    } else if(grepl('watercolor', materials, ignore.case = T)){
      test_df$art_type <- 'watercolor'
    } else {
      test_df$art_type <- 'prints'
    }
  } else if(grepl('oil', title, ignore.case = T)){
    test_df$art_type <- 'oil'
  } else if(grepl('acrylic', title, ignore.case = T)){
    test_df$art_type <- 'acrylic'
  } else if(grepl('watercolor', title, ignore.case = T)){
    test_df$art_type <- 'watercolor'
  } else {
    test_df$art_type <- 'prints'
  }
  test_df$raw_mat <- character(length = nrow(test_df))
  
  if(length(materials) > 0){
    if(grepl('canvas', materials, ignore.case = T)){
      test_df$raw_mat <- 'canvas'
    } else if(grepl('watercolor | paper', materials, ignore.case = T)){
      test_df$raw_mat <- 'wat.paper'
    } 
  } else if(grepl('canvas', description, ignore.case = T)){
    test_df$raw_mat <- 'canvas'
  } else if(grepl('watercolor | paper', description, ignore.case = T)){
    test_df$raw_mat <- 'wat.paper'
  }
  test_df <- as.data.frame(test_df, stringsAsFactors = T)
  test_df$art_type <- factor(test_df$art_type)
  test_df$raw_mat <- factor(test_df$raw_mat)
  levels(test_df$who_made) <- levels(data_train4$who_made)
  levels(test_df$when_made) <- levels(data_train4$when_made)
  levels(test_df$art_type) <- levels(data_train4$art_type)
  levels(test_df$raw_mat) <- levels(data_train4$raw_mat)
  test_df
}

scrape_from_url(test_url)



  
    
  
  
  
  
  
    
  
 
  
  
  
  
  
  
  
  
  
  
  
  
    
  
  
  



