packrat::init()

####### libraries required ############

library(jsonlite)
library(dplyr)
library(ggplot2)
library(stringr)

#####  downloading data from the Etsy API #####

#listings <- fromJSON('https://openapi.etsy.com/v2/listings/active?api_key=gxx1r5uo1zpe5c55jf003xn5&category=art/painting')

#shops <- fromJSON('https://openapi.etsy.com/v2/shops?api_key=gxx1r5uo1zpe5c55jf003xn5', flatten = T)

url_list <- 'https://openapi.etsy.com/v2/listings/active?api_key=gxx1r5uo1zpe5c55jf003xn5&category=art/painting&limit=100'

#list1 <- fromJSON(url_list)
#list2a <- fromJSON(paste0(url_list,'&page=2'))

pages1 <- list()

for(i in 1:501){
  mydata <- fromJSON(paste0(url_list,'&page=', i))
  message("Retrieving page ", i)
  pages1[[i]] <- mydata$results
  message(object.size(pages1))
  if(i == 200){
    Sys.sleep(60)
    message("Waiting 1 min at", i)
  } else if(i == 350){
    Sys.sleep(60)
    message("Waiting 1 min at", i)
  }
  
}

maj_data <- rbind_pages(pages1) # final masterfile with all listings

sink("maj_data.txt")
for(i in 1:nrow(maj_data)){
  print(maj_data[i,])
  
}
sink()


 
####### cleaning masterfile to remove redundant columns ########
names(maj_data)
unnecessary_names <- c('category_id', 'category_path', 'category_path_ids', 'url')

min_data <- maj_data[,!names(maj_data) %in% unnecessary_names] # working masterfile

typeof(min_data) # this object is a list of several vectors. Need to simplify

table(min_data$currency_code) ### several foreign currencies: remove not USD
table(min_data$language) ### several foreing languages: difficult to use in NLP
min_data[which(min_data$language == 'MACHINE_en'), ]

min_data <- subset(min_data, min_data$currency_code == 'USD' & 
                     (min_data$language == 'en-US' | min_data$language == 'MACHINE_en'))

min_data <- min_data[, !names(min_data) %in% c("currency_code", "language", "state")]

dim(min_data)

########## playing with the data #########
names(min_data)

tail(table(min_data$price))

str(min_data$price) ## is a character vector so change to numeric function
table(is.na(min_data$price)) ## no missing entries luckily
min_data$price <- as.numeric(min_data$price)
max(min_data$price) ## max is 250K

hist(min_data$price) ## most paintings are below 25000

hist(subset(min_data$price, min_data$price <= 1000), breaks = 200)
length(subset(min_data$price, min_data$price <= 1000))

table(min_data$item_length)
table(!is.na(min_data$item_length)) # item dimensions are often missing
table(!is.na(min_data$item_width))
table(!is.na(min_data$item_height))
table(!is.na(min_data$item_weight))

table(min_data$is_supply)
table(min_data$recipient)
table(min_data$occasion)
table(min_data$non_taxable)

###### subsetting cheap paintings ########

cheap_art <- subset(min_data,min_data$price <= 1000)
p = 541078062 ###this is the listing which has only watercolor pigments
dim(cheap_art)
cheap_art <- cheap_art [cheap_art$listing_id != p, ]

###### converting list columns into a simple vector ##########
cheap_art$tag_col <- sapply(cheap_art$tags, paste0, collapse = ',')
str(cheap_art$tag_col)
cheap_art$mat_col <- sapply(cheap_art$materials, paste0, collapse = ',')
str(cheap_art$mat_col)
cheap_art$tax_col <- sapply(cheap_art$taxonomy_path, paste0, collapse = ',')
str(cheap_art$tax_col)

unrel.res <- grepl('christmas ball', cheap_art$mat_col, ignore.case = T)
table(unrel.res)

cheap_art <- cheap_art[!(unrel.res), ]

#ggplot(data = cheap_art, aes(x = non_taxable, y = price))+
#  geom_boxplot()
#hist(cheap_art$quantity[cheap_art$quantity <= 1000])
#quantile(cheap_art$quantity[cheap_art$quantity <= 1000])
#cheap_art$tag_col <- character(length = nrow(cheap_art))
#for(i in 1:length(cheap_art$tag_col)){
#  cheap_art$tag_col[i] <- paste(cheap_art$tags[[i]], collapse = ",")
#} found a shorter method above

#### which variables might be of interest? ###############
# response variable: price
# predictors: type of painting (oil, water, acrylic, ink, sketch, pastel),
# content of painting (abstract, nature, real-life, potrait)
# size of painting (continuous variable, lots of missing data, might need to
# intrapolate or pulled from description)
# is_customizable
# who made it? (can this be used as a method to distinguish between galleries and
# individual artisits?)
# whether it is original or a print
# how many views and how many favorers
# when_made : can give indication of age of painting ##########

table(cheap_art$is_digital)
table(cheap_art$is_customizable)

cheap_art$description[1:5] 

table(cheap_art$when_made)

### simple pattern checking

ggplot(data = cheap_art[cheap_art$views<= 2e+05, ], aes(x = views, y = price))+
  geom_point()

cheap_art[cheap_art$num_favorers <= 2000, ] %>%
  ggplot(aes(x = num_favorers, y = price))+
  geom_point()cheap_art$tag_col <- character(length = nrow(cheap_art))

###### classify paintings based on taxonomy path ###########

### initialize a column for painting type
cheap_art$art_type <- NA
#### classifying the acrylic paintings
acr.res <- grepl('acrylic', cheap_art$tax_col, ignore.case = T) 
table(acr.res)

for (i in 1:nrow(cheap_art)){
  if ( acr.res[i] ){
    cheap_art$art_type[i] <- 'acrylic'
  } else{
    cheap_art$art_type[i] <- NA
  }
}

table(cheap_art$art_type)

### watercolors 
wat.res <- grepl('watercolor', cheap_art$tax_col, ignore.case = T) 
table(wat.res)  

for (i in 1:nrow(cheap_art)){
  if ( wat.res[i] ){
    cheap_art$art_type[i] <- 'watercolor'
  } 
}
table(cheap_art$art_type)

#### oil paintings
oil.res <- grepl('oil', cheap_art$tax_col, ignore.case = T) 
table(oil.res)  
  
for (i in 1:nrow(cheap_art)){
  if ( oil.res[i] ){
    cheap_art$art_type[i] <- 'oil'
  } 
}
table(cheap_art$art_type)

### also classifying the prints
pri.res <- grepl('prints', cheap_art$tax_col, ignore.case = T)
table(pri.res)

for (i in 1:nrow(cheap_art)){
  if ( pri.res[i] ){
    cheap_art$art_type[i] <- 'prints'
  } 
}
table(cheap_art$art_type)

mix.res <- grepl('mixed medi', cheap_art$mat_col, ignore.case = T)
table(mix.res)

for (i in 1:length(mix.res)){
  if ( mix.res[i] )  {
    cheap_art$art_type[i] <- 'mixed'
  } 
}

table(cheap_art$art_type)

#### nearly 27000 paintings have been classified correctly
table(!is.na(cheap_art$art_type))

ggplot(cheap_art, aes(art_type, price))+
  geom_boxplot()

#### classifying based on materials #########
cheap_art[, 'raw_mat'] <- NA

## pre-classify paintings to photo paper

for(i in 1:nrow(cheap_art)){
  if(is.na(cheap_art$art_type[i])){
    cheap_art$raw_mat[i] <- ''
  } else if(cheap_art$art_type[i] == 'prints'){
    cheap_art$raw_mat[i] <- 'photo paper'
  }
  
}

### classifying watercolor papers

pap.res <- grepl('paper', cheap_art$mat_col, ignore.case = T)

table(pap.res)
length(pap.res)

for(i in 1:nrow(cheap_art)){
  if(is.na(cheap_art$art_type[i])){
    cheap_art$raw_mat[i] <- NA
  } else if ( ( cheap_art$art_type[i] == 'watercolor' ) & 
              ( pap.res[i] == T ) ) {
                cheap_art$raw_mat[i] <- 'wat.paper'
              }
}


pri.res <- grepl('print', cheap_art$mat_col, ignore.case = T)

table(pri.res)

for (i in 1:nrow(cheap_art)){
  if ( ( pri.res[i] ) ){
    cheap_art$raw_mat[i] <- 'photo paper'
  }
}

table(cheap_art$raw_mat)

can.res <- grepl('canvas', cheap_art$mat_col, ignore.case = T)
table(can.res)

for(i in 1:nrow(cheap_art)){
  if( can.res [i]) {
    cheap_art$raw_mat[i] <- 'canvas'
      
  }
}

#### more cleaning using description tags
wat.res <- grepl('watercolor', cheap_art$description, ignore.case = T)
table(wat.res)
for(i in 1:nrow(cheap_art)){
  if( ( is.na(cheap_art$raw_mat[i] ) ) & ( wat.res[i] ) ){
    cheap_art$raw_mat[i] <- 'wat.paper'
  }
}

table(cheap_art$raw_mat)

###### classifying dimensions of painting: area ########
cheap_art$dimen_item <- as.numeric(cheap_art$item_length) * 
  as.numeric(cheap_art$item_height)
hist(cheap_art$dimen_item)
table(is.na(cheap_art$dimen_item))
table(!is.na(cheap_art$dimen_item))

###### classifying content of painting using tags ######

pri.res.2 <- grepl('print', cheap_art$tag_col, ignore.case = T) 

for (i in 1:nrow(cheap_art)){
  if ( ( pri.res.2[i] ) ){
    cheap_art$raw_mat[i] <- 'photo paper'
    cheap_art$art_type[i] <- 'prints'
  }
}

### classifying digital prints
table(cheap_art$is_digital)

for ( i in 1:nrow(cheap_art)){
  if ( cheap_art$is_digital[i] ){
    cheap_art$raw_mat[i] <- 'digital'
    cheap_art$art_type[i] <- 'dig.art'
  }
}

table(cheap_art$art_type)
table(!is.na(cheap_art$art_type)) ## filled in most of data
table(cheap_art$raw_mat)
table(!is.na(cheap_art$raw_mat))

### starting with a few variables at the moment ###
names(cheap_art)
hist(cheap_art$quantity)
quantile(cheap_art$quantity)
dim(cheap_art[which(cheap_art$quantity < 1000), ])
cheap_art_use <- cheap_art[!(cheap_art$quantity == 7992), ]
names(cheap_art_use)
relfeat <- c('title', 'description', 'price', 'quantity', 'views','num_favorers',
             'who_made', 'when_made', 'is_customizable', 'has_variations', 'tag_col',
             'mat_col', 'tax_col', 'art_type', 'raw_mat', 'dimen_item')

mod_data <- cheap_art_use[, relfeat]

#### plugging in more holes in the art_type as it is my most important predictor
oil.res2 <- grepl('oil color | oil painting', mod_data$mat_col)
table(oil.res2)

for(plugs in 1:nrow(mod_data)){
  if( is.na(mod_data$art_type[plugs] ) & ( oil.res2[plugs] )){
    mod_data$art_type[plugs] <- 'oil'
    
  }
}

oil.res3 <- grepl('oil color | oil painting', mod_data$tag_col, ignore.case = T)
table(oil.res3)


for(plugs in 1:nrow(mod_data)){
  if( is.na(mod_data$art_type[plugs] ) & ( oil.res3[plugs] )){
    mod_data$art_type[plugs] <- 'oil'
    
  }
}





table(mod_data$art_type)
table(cheap_art_use$art_type) # compare above table with this one
table(!is.na(cheap_art$art_type))
table(!is.na(mod_data$art_type))

table(mod_data$art_type,mod_data$raw_mat)

table(mod_data$raw_mat, mod_data$is_customizable)

table(mod_data$art_type, mod_data$is_customizable)

hist(mod_data$price)
hist(mod_data$dimen_item)
mod_data$dimen_item
