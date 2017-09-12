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

cheap_art <- subset(min_data,min_data$price <= 1000)

ggplot(data = cheap_art, aes(x = non_taxable, y = price))+
  geom_boxplot()

hist(cheap_art$quantity[cheap_art$quantity <= 1000])

quantile(cheap_art$quantity[cheap_art$quantity <= 1000])

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
  geom_point()


###### classify paintings based on taxonomy path ###########

### initialize a column for painting type
cheap_art$art_type <- NA
#### classifying the acrylic paintings
acr.res <- lapply(cheap_art$taxonomy_path, function(ch) grep("acrylic", 
                                                             ch, ignore.case = T))
acr_indx <- sapply(acr.res, function(x) length(x) > 0)
table(acr_indx)

for (i in 1:length(acr_indx)){
  if (acr_indx[i]){
    cheap_art$art_type[i] <- 'acrylic'
  } else{
    cheap_art$art_type[i] <- NA
  }
}
table(cheap_art$art_type)

### watercolors 
wat.res <- lapply(cheap_art$taxonomy_path, function(ch) grep("watercolor", 
                                                             ch, ignore.case = T))
wat_indx <- sapply(wat.res, function(x) length(x) > 0)
table(wat_indx)

for (i in 1:length(wat_indx)){
  if (wat_indx[i]){
    cheap_art$art_type[i] <- 'watercolor'
  } 
}
table(cheap_art$art_type)

#### oil paintings
oil.res <- lapply(cheap_art$taxonomy_path, function(ch) grep("oil", 
                                                             ch, ignore.case = T))
oil_indx <- sapply(oil.res, function(x) length(x) > 0)
table(oil_indx)

for (i in 1:length(oil_indx)){
  if (oil_indx[i]){
    cheap_art$art_type[i] <- 'oil'
  } 
}
table(cheap_art$art_type)

### also classifying the prints
pri.res <- lapply(cheap_art$taxonomy_path, function(ch) grep("prints", 
                                                             ch, ignore.case = T))
pri_indx <- sapply(pri.res, function(x) length(x) > 0)
table(pri_indx)

for (i in 1:length(pri_indx)){
  if (pri_indx[i]){
    cheap_art$art_type[i] <- 'prints'
  } 
}
table(cheap_art$art_type)

#### nearly 27000 paintings have been classified correctly
table(!is.na(cheap_art$art_type))

ggplot(cheap_art, aes(art_type, price))+
  geom_boxplot()

#### classifying based on materials #########
cheap_art$raw_mat <- character(length = nrow(cheap_art))

can.res <- lapply(cheap_art$materials, function(ch) grep("canvas", 
                                                             ch, ignore.case = T))
can_indx <- sapply(can.res, function(x) length(x) > 0)
table(can_indx)

for (i in 1:length(can_indx)){
  if (can_indx[i]){
    cheap_art$raw_mat[i] <- 'canvas'
  } 
}
table(cheap_art$raw_mat)

pap.res <- lapply(cheap_art$materials, function(ch) grep("paper", 
                                                         ch, ignore.case = T))
pap_indx <- sapply(pap.res, function(x) length(x) > 0)
table(pap_indx)


for (i in 1:length(pap_indx)){
  if (pap_indx[i]){
    cheap_art$raw_mat[i] <- 'paper'
  } 
}
table(cheap_art$raw_mat)

## converting materials into a simple vector

cheap_art$materials_col <- character(length = nrow(cheap_art))

for(i in 1:length(cheap_art$materials_col)){
  cheap_art$materials_col[i] <- paste(cheap_art$materials[[i]], collapse = ",")
  
}

table(cheap_art$materials_col)

can.glas <- grepl('canvas', cheap_art$materials_col, ignore.case = T) & 
  grepl('glass', cheap_art$materials_col, ignore.case = T)

table(can.glas)
cheap_art$materials[which(can.glas)]


pap.can <- grepl('canvas', cheap_art$materials_col, ignore.case = T) &
  grepl('paper', cheap_art$materials_col, ignore.case = T)
table(pap.can)
cheap_art$materials[which(pap.can)]

if(cheap_art$art_type == 'prints'){
  cheap_art$materials_col <- 'paper'
}

