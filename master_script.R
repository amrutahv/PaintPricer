packrat::init()

####### libraries required ############
library(jsonlite)
library(dplyr)
library(ggplot2)
library(stringr)
library(randomForest)
library(data.table)
library(dplyr)
########## next big updated file: get from auto_dload.Rdata ###################
names(big_data)

table(duplicated(big_data$listing_id))
big_data <- big_data[!duplicated(big_data$listing_id), ]


####### cleaning masterfile to remove redundant columns ########
unnecessary_names <- c('category_id', 'category_path', 'category_path_ids')

min_data <- big_data[,!names(big_data) %in% unnecessary_names] # working masterfile

typeof(min_data) # this object is a list of several vectors. Need to simplify

table(min_data$currency_code) ### several foreign currencies: remove not USD
table(min_data$language) ### several foreign languages: difficult to use in NLP

min_data <- subset(min_data, min_data$currency_code == 'USD' & 
                     (min_data$language == 'en-US' | min_data$language == 'MACHINE_en'))

min_data <- min_data[min_data$state != 'sold_out', ]
min_data <- min_data[, !names(min_data) %in% c("currency_code", "language", "state")]
dim(min_data)


########## playing with the data #########

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

cheap_art <- cheap_art [cheap_art$listing_id != p, ]

cheap_art <- cheap_art[cheap_art$listing_id != 465247738, ] ## this contains a paper doll
dim(cheap_art)
###### converting list columns into a simple vector ##########

simplify_lists <- function(col_name, separator){
  new_vec <- sapply(col_name, paste0, collapse = separator)
  return(new_vec)
}

complex_cols <- c()

for(i in 1:ncol(cheap_art)){
  new_col <- class(cheap_art[, i])
  if(new_col == 'list'){
    complex_cols <- c(complex_cols, names(cheap_art)[i])
  }
}
complex_cols

for(i in 1:ncol(cheap_art)){
  new_col <- class(cheap_art[, i])
  if(new_col == 'list'){
    print(names(cheap_art)[i])
  }
}

cheap_art$tag_col <- simplify_lists(cheap_art$tags, ',')
str(cheap_art$tag_col)
cheap_art$mat_col <- simplify_lists(cheap_art$materials, ',')
str(cheap_art$mat_col)
cheap_art$sty <- simplify_lists(cheap_art$style, ',')
str(cheap_art$sty)
cheap_art$tax_col <- simplify_lists(cheap_art$taxonomy_path, ',')
str(cheap_art$tax_col)

cheap_art <- cheap_art[ ,!(names(cheap_art) %in% complex_cols)]

unrel.res1 <- grepl('christmas ball', cheap_art$mat_col, ignore.case = T)
table(unrel.res1)
unrel.res2 <- grepl('dolls', cheap_art$title, ignore.case = T) |
  grepl('tray' ,cheap_art$title, ignore.case = T) |
  grepl('cabinet', cheap_art$title, ignore.case = T) ### remove christmas balls & dolls
table(unrel.res2)

cheap_art <- cheap_art[!(unrel.res1), ]
cheap_art <- cheap_art[!(unrel.res2), ]
vid.res <- grepl('video lesson|on-line|powerpoint', cheap_art$title, ignore.case = T)
table(vid.res)
cheap_art <- cheap_art[!(vid.res), ]

empty_tags <- c()
for(i in 1:nrow(cheap_art)){
  if( (cheap_art$tag_col[i] == "") & (cheap_art$mat_col[i] == "") ){
    empty_tags <- c(empty_tags, cheap_art$listing_id[i])
  }
}

length(empty_tags)

cheap_art <- cheap_art[!(cheap_art$listing_id %in% empty_tags), ]

cheap_art <- cheap_art[!(cheap_art$listing_id %in% empty_tags), ]

cheap_art <- data.table(cheap_art)

row.names(cheap_art) <- cheap_art$listing_id

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

#### mixed paintings
mix.res <- grepl('mixed medi', cheap_art$mat_col, ignore.case = T)
table(mix.res)
mix.res2 <- grepl('mixed medi', cheap_art$tag_col, ignore.case = T)
table(mix.res2)
mix.res3 <- grepl('mixed', cheap_art$tax_col, ignore.case = T)
table(mix.res3)
mix.res4 <- grepl('acrylic', cheap_art$tag_col, ignore.case = T) &
  grepl('oil', cheap_art$tag_col, ignore.case = T)
table(mix.res4)
mix.res5 <- grepl('acrylic', cheap_art$mat_col, ignore.case = T) &
  grepl('oil', cheap_art$mat_col, ignore.case = T)
table(mix.res5)

for (i in 1:nrow(cheap_art)){
  if ( mix.res[i] | mix.res2[i] | mix.res3[i] | mix.res4[i] | mix.res5[i])  {
    cheap_art$art_type[i] <- 'mixed'
  } 
}

table(cheap_art$art_type)

table(!is.na(cheap_art$art_type))

### also classifying the prints
pri.res <- grepl('prints', cheap_art$tax_col, ignore.case = T)
table(pri.res)
pri.res.2 <- grepl('print', cheap_art$tag_col, ignore.case = T)
table(pri.res.2)

for (i in 1:nrow(cheap_art)){
  if (( pri.res[i] | pri.res.2[i] )){
    cheap_art$art_type[i] <- 'prints'
  } 
}

table(cheap_art$art_type)

table(!is.na(cheap_art$art_type))

## finding giclee prints
gic.res <- grepl('giclee', cheap_art$title, ignore.case = T)
table(gic.res)

for(i in 1:length(gic.res)){
  if ( gic.res [i] ){
    cheap_art$art_type[i] <- 'prints'
  }
}

table(cheap_art$art_type)


##### looking for digital prints
dig.res <- grepl("digital art|clip art|clipart", cheap_art$title, ignore.case = T)
table(dig.res)
dig.res2 <- grepl('digital art|clip art|clipart', cheap_art$mat_col, ignore.case = T)
table(dig.res2)
dig.res3 <- grepl('digital art|e-pattern|clip art|clipart', cheap_art$description, ignore.case = T)
table(dig.res3)
dig.res4 <- grepl('digital art|clip art|digital', cheap_art$tag_col, ignore.case = T)
table(dig.res4)

for (i in 1:nrow(cheap_art)){
  if( dig.res[i] | dig.res2[i] | dig.res3[i] | dig.res4[i] ){
    cheap_art$art_type[i] <- 'dig.art'
  }
}

table(cheap_art$art_type)

### other types: spray paint, ink
oth.res <- grepl('spray|ink', cheap_art$tax_col, ignore.case = T) | 
  grepl('pastel|ink|pencil', cheap_art$mat_col, ignore.case = T) &
  !grepl('print', cheap_art$mat_col, ignore.case = T)
table(oth.res)

for(i in 1:nrow(cheap_art)){
  if( oth.res [i] & is.na(cheap_art$art_type[i] ) ){
    cheap_art$art_type [i] <- 'other'
  }
}
table(cheap_art$art_type)
table(!is.na(cheap_art$art_type))


#### classifying the acrylic paintings
acr.res <- cheap_art$listing_id[grepl('acrylic', cheap_art$tax_col, ignore.case = T)]
### watercolors 
wat.res <- cheap_art$listing_id[grepl('watercolor', cheap_art$tax_col, ignore.case = T)]
table(acr.res %in% wat.res) ### sanity check
#### oil paintings
oil.res <- cheap_art$listing_id[grepl('oil', cheap_art$tax_col, ignore.case = T) ]
table(oil.res %in% wat.res) ### sanity check
table(acr.res %in% oil.res) ### sanity check

# re-writing above vectors
acr.res <- grepl('acrylic', cheap_art$tax_col, ignore.case = T)
wat.res <- grepl('watercolor', cheap_art$tax_col, ignore.case = T)
oil.res <- grepl('oil', cheap_art$tax_col, ignore.case = T)

for (i in 1:nrow(cheap_art)){
  if ( oil.res[i] & is.na(cheap_art$art_type [i] )){
    cheap_art$art_type[i] <- 'oil'
  } 
}

for(i in 1:nrow(cheap_art)){
  if( is.na(cheap_art$art_type[i] ) & acr.res[i] ){
    cheap_art$art_type[i] <- 'acrylic'
  }
}

for(i in 1:nrow(cheap_art)){
  if( is.na(cheap_art$art_type[i] ) & wat.res[i] ){
    cheap_art$art_type[i] <- 'watercolor'
  }
}

table(cheap_art$art_type)
table(!is.na(cheap_art$art_type))


##### looking for digital prints
dig.res <- grepl("digital art|clip art|clipart|download", cheap_art$title, ignore.case = T)
table(dig.res)
dig.res2 <- grepl('digital art|clip art|digital', cheap_art$mat_col, ignore.case = T)
table(dig.res2)
dig.res3 <- grepl('digital download|instant download|download|digital|e-pattern|electronic', cheap_art$description, ignore.case = T)
table(dig.res3)
dig.res4 <- grepl('digital art|clip art|digital', cheap_art$tag_col, ignore.case = T)
table(dig.res4)

for (i in 1:nrow(cheap_art)){
  if( dig.res[i] | dig.res2[i] | dig.res3[i] | dig.res4[i] ){
    cheap_art$art_type[i] <- 'dig.art'
  }
}

### other types: spray paint, ink
oth.res <- grepl('spray|ink', cheap_art$tax_col, ignore.case = T)
table(oth.res)

for(i in 1:nrow(cheap_art)){
  if( oth.res [i] & is.na(cheap_art$art_type[i] ) ){
    cheap_art$art_type [i] <- 'other'
  }
}
table(cheap_art$art_type)
table(!is.na(cheap_art$art_type))


acr.res2 <- grepl('acrylic', cheap_art$tag_col, ignore.case = T)
table(acr.res2)

cheap_art[listing_id == 252390285, ]$art_type <- 'watercolor'

### usually watercolors and acrylics are not painted together, so classify this last
mix.res5 <- grepl('acrylic', cheap_art$tag_col, ignore.case = T) &
  grepl('watercolor', cheap_art$tag_col, ignore.case = T)
table(mix.res5)

mixed_paintings <- c()
for (i in 1:nrow(cheap_art)){
  if (mix.res5[i] & is.na(cheap_art$art_type[i])){
    mixed_paintings[i] <- cheap_art$listing_id[i]
  }
}

table(!is.na(mixed_paintings))
mixed_paintings <- mixed_paintings[!is.na(mixed_paintings)]
View(cheap_art[ cheap_art$listing_id %in% mixed_paintings, ])

for(i in 1:nrow(cheap_art)){
  if (is.na(cheap_art$art_type[i]) & mix.res5[i]){
    cheap_art$art_type[i] <- 'mixed'
  }
}

table(cheap_art$art_type)
table(!is.na(cheap_art$art_type))
#### nearly 57000 paintings have been classified correctly

ggplot(cheap_art, aes(art_type, price))+
  geom_boxplot()

#### classifying based on materials #########
cheap_art[, 'raw_mat'] <- NA

## pre-classify prints to photo paper and digital art to digital
for(i in 1:nrow(cheap_art)){
  if(is.na(cheap_art$art_type[i])){
    cheap_art$raw_mat[i] <- NA
  } else if(cheap_art$art_type[i] == 'prints'){
    cheap_art$raw_mat[i] <- 'photo paper'
  } else if(cheap_art$art_type[i] == 'dig.art'){
    cheap_art$raw_mat[i] <- 'digital'
  }
  
}
table(cheap_art$raw_mat)

### classifying watercolor papers

wat.paper <- c('watercolor', 'watercolor paper', 'paper')
not.wat <- c('print', 'acrylic', 'oil', 'ink','mixed medi', 'board')

wat_pap_res <- grepl(paste0(wat.paper, collapse = '|'), cheap_art$mat_col, ignore.case = T) &
  !grepl(paste0(not.wat, collapse = '|'), cheap_art$mat_col, ignore.case = T)

table(wat_pap_res)

for(i in 1:nrow(cheap_art)){
  if(is.na(cheap_art$art_type[i]) & is.na(cheap_art$raw_mat[i]) & wat_pap_res[i] ){
    cheap_art$art_type[i] <- 'watercolor'
  }
}

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


table(cheap_art$art_type)

wat_pap_res2 <- cheap_art[art_type == 'watercolor', 
                          .(listing_id, grepl('paper', raw_mat, ignore.case = T)), ] ## only works if it is data.table
  
counter = 0
for(i in 1:nrow(wat_pap_res2)){
  if( wat_pap_res2$V2[i] ){
    print(cheap_art[listing_id == wat_pap_res2$listing_id[i], listing_id])
    counter = counter + 1
    print(counter)
  }
}

### classifying acrylic and canvas
acr.canv <- c('acrylic', 'canvas')
not.acr <- c('print', 'watercolor', 'oil', 'ink','mixed medi', 'board')

acr_can_res <- grepl(paste0(acr.canv, collapse = '|'), cheap_art$mat_col, ignore.case = T) &
  !grepl(paste0(not.acr, collapse = '|'), cheap_art$mat_col, ignore.case = T)

table(acr_can_res)

for(i in 1:nrow(cheap_art)){
  if(is.na(cheap_art$art_type[i]) & is.na(cheap_art$raw_mat[i]) & acr_can_res[i] ){
    cheap_art$art_type[i] <- 'acrylic'
    cheap_art$raw_mat[i] <- 'canvas'
  }
}
table(cheap_art$art_type)
table(cheap_art$raw_mat)

### classifying oil and canvas
oil.canv <- c('oil','canvas')
not.oil <- c('print','watercolor','acrylic','ink','mixed medi','board')

oil_can_res <- grepl(paste0(oil.canv, collapse = '|'), cheap_art$mat_col, ignore.case = T) &
  !grepl(paste0(not.oil, collapse = '|'), cheap_art$mat_col, ignore.case = T)
table(oil_can_res)

for(i in 1:nrow(cheap_art)){
  if(is.na(cheap_art$art_type[i]) & is.na(cheap_art$raw_mat[i]) & oil_can_res[i] ){
    cheap_art$art_type[i] <- 'oil'
    cheap_art$raw_mat[i] <- 'canvas'
  }
}

table(cheap_art$art_type)
table(cheap_art$raw_mat)

# pri.res <- grepl('print', cheap_art$mat_col, ignore.case = T)
# 
# table(pri.res)
# 
# for (i in 1:nrow(cheap_art)){
#   if ( ( pri.res[i] ) ){
#     cheap_art$raw_mat[i] <- 'photo paper'
#   }
# }

table(cheap_art$raw_mat)

can.res <- grepl('canvas', cheap_art$mat_col, ignore.case = T)
table(can.res)
can.res2 <- grepl('canvas', cheap_art$tag_col, ignore.case = T)
table(can.res2)

for(i in 1:nrow(cheap_art)){
  if( can.res [i] | can.res2 [i] ){
    cheap_art$raw_mat[i] <- 'canvas'
    
  }
}

table(cheap_art$raw_mat)
table(!is.na(cheap_art$raw_mat))
#### more cleaning using description tags
wat.res <- grepl('watercolor', cheap_art$description, ignore.case = T)
table(wat.res)
for(i in 1:nrow(cheap_art)){
  if( ( is.na(cheap_art$raw_mat[i] ) ) & ( wat.res[i] ) ){
    cheap_art$raw_mat[i] <- 'wat.paper'
  } 
}

table(cheap_art$raw_mat)

### digital arts 


###### classifying dimensions of painting: area ########
cheap_art$dimen_item <- as.numeric(cheap_art$item_length) * 
  as.numeric(cheap_art$item_height)
hist(cheap_art$dimen_item)
table(is.na(cheap_art$dimen_item))
table(!is.na(cheap_art$dimen_item))

input_text_vec <- c('listing_id', 'title', 'description', 'item_length', 
                    'item_height', 'item_width')
write.table(cheap_art[,input_text_vec], file = 'text_data.txt', 
            sep='mycustdelim', eol = 'mycusteol')

system('perl extractDimensions.pl text_data.txt')  #creates final_text_data.txt with extracted dimensions.
temp1 <- read.delim('final_text_data.txt', sep = '~', na.strings = 'NA', stringsAsFactors = F, header = F)

names(temp1) <- c("listing_id", "length", "breadth", "thickness")
cheap_art <- merge(cheap_art, temp1)


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

####### starting with a few variables at the moment ### #########
names(cheap_art)
hist(cheap_art$quantity)
quantile(cheap_art$quantity)
dim(cheap_art[which(cheap_art$quantity < 1000), ])
cheap_art_use <- cheap_art[!(cheap_art$quantity == 7992), ]
names(cheap_art_use)
relfeat <- c('listing_id', 'title', 'description', 'price', 'quantity', 'views','num_favorers',
             'who_made', 'when_made', 'is_customizable', 'has_variations', 'tag_col',
             'mat_col', 'tax_col', 'art_type', 'raw_mat', 'dimen_item')

mod_df <- cheap_art_use[, relfeat]
dim(mod_df)

#### converting to a datatable
mod_data <- data.table(mod_df)
mod_data

setkeyv(mod_data, c("art_type", "price", "listing_id"))
table(mod_data$art_type)

mod_data$raw_mat

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
table(cheap_art_use$art_type) # compare above table with this one: above should be more
table(!is.na(cheap_art_use$art_type))
table(!is.na(mod_data$art_type))

table(mod_data$art_type,mod_data$raw_mat)

table(mod_data$raw_mat, mod_data$is_customizable)

table(mod_data$art_type, mod_data$is_customizable)

hist(mod_data$price)
hist(mod_data$dimen_item)
mod_data$dimen_item

names(mod_data)

############ random forest !!! ############

rf_data1 <- mod_data[, c('price', 'quantity', 'views', 'num_favorers', 'who_made',
                         'when_made', 'is_customizable', 'has_variations', 'art_type',
                         'raw_mat')]
rf_data1 <- data.table(rf_data1, stringsAsFactors = T)
rf_data1 <- rf_data1[complete.cases(rf_data1), ]
setkeyv(rf_data1, c('price', 'art_type'))
str(rf_data1)
names(rf_data1)
dim(rf_data1)
train1 <- sample(nrow(rf_data1), nrow(rf_data1) * 0.8)
data_train1 <- rf_data1[train1, ]
data_test1 <- rf_data1[-train1, ]

rfm1 <- randomForest::randomForest(price ~., data_train1, ntree = 500, 
                                   na.action = na.omit)
rfm1
importance(rfm1)

rfm2 <- randomForest::randomForest(price ~., data_train1, ntree = 500, 
                                   na.action = na.omit, mtry = 4)
rfm2

rfm3 <- randomForest::randomForest(price ~., data_train1, ntree = 5000, 
                                   na.action = na.omit, sampsize = 5000)
rfm3

rfm4 <- randomForest::randomForest(price ~., data_train1, ntree = 5000, 
                                   na.action = na.omit, sampsize = 7000)
rfm4


hist(rf_data1$price)

################# Linear regression #########################

glm1 <- glm(price ~., data_train1, family = 'gaussian')
round(mean(glm1$residuals))

glm2 <- glm(log(price) ~., data_train4, family = 'gaussian') ## using data_train4 which doesnt have views

glm2

round(mean(glm2$residuals))

par(mfrow = c(2,2))
plot(glm2)

glm2.pred <- predict(glm2, data_test4)

exp(RMSE(glm2.pred, log(data_test4$price))) ### worse than RF

exp(RMSE(predperf_rfm1d4$predicted, predperf_rfm1d4$price))


mean(abs(glm2.pred-log(data_test4$price))) ### worse than RF

mean(abs(predperf_rfm1d4$predicted - predperf_rfm1d4$price))

############## glm vs RF predicted values comparison ##########3
predperf_glm2 <- data.frame(price = log(data_test4$price), glm.pred = glm2.pred)
predperf_rfm1d4

all.predictions <- merge(predperf_glm2,predperf_rfm1d4)

all.pred <- melt(all.predictions, id = "price", value.name = 'predictions', variable.name = "model")
names(all.pred)

ggplot(data = all.pred,aes(x = price, y = predictions)) + 
  geom_point(colour = "blue") +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  facet_wrap(~ model,ncol = 2) 
coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
  ggtitle("Predicted vs. Actual, by model")

ggplot(data = all.predictions, aes(x = price, y = glm2.pred))+
  geom_point()


#### check autocorrelateion of variables #
acf(data_train4)

ggplot() + geom_density(aes(residuals(glm2)))

par(mfrow = c(1,1))
spreadLevelPlot(glm2)

####### best model till now ######

rfm5_d1 <- randomForest(log(price) ~., data_train1)
rfm5_d1 #### best model till now :)

rfm5_d1

rfm5_d1$importance 

plot(rfm5_d1)
varImpPlot(rfm5_d1)


predict_rfm5d1 <- predict(rfm5_d1, data_test1)
predict_rfm5d1

predperf_rfm5d1 <- data.frame(price = log(data_test1$price), predicted = predict_rfm5d1)
caret::RMSE(predperf_rfm5d1$predicted, predperf_rfm5d1$price)

sqrt(mean((predperf_rfm5d1$price - predperf_rfm5d1$predicted)^2))



ggplot(predperf_rfm5d1, aes(price, predicted))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)###### pretty good!!!!!!!!!!!!!

qqnorm((predperf_rfm5d1$predicted - predperf_rfm5d1$price)/
         sd(predperf_rfm5d1$predicted - predperf_rfm5d1$price))

qqline((predperf_rfm5d1$predicted - predperf_rfm5d1$price)/
         sd(predperf_rfm5d1$predicted - predperf_rfm5d1$price))


########## Learning curve ###########################
data_train1
data_test1
learnCurve <- learing_curve_dat(data_train1 [],
                                outcome = 'Price',
                                test_prop = 1/4,
                                verbose = T,
                                method = 'rf')

chunks <- seq(1000, nrow(data_train1), 1000)

rmse <- function(actual, predicted) sqrt( mean( (actual - predicted)^2 ))
curve_data <- data.frame(size = integer(length(chunks)),
                         train_error = integer(length(chunks)),
                         valid_error = integer(length(chunks)))

for(i in 1:length(chunks)){
  
  size <- chunks[i]
  
  message("Running model for size", size)
  
  sampleidx <- sample(nrow(data_train1), size)
  train_d <- data_train1[sampleidx, ]
  test_d <- data_train1[-sampleidx, ]
  rfm <- randomForest::randomForest(log(price) ~., train_d)
  train.pred <- predict(rfm, train_d)
  training_error <- rmse(log(train_d$price), train.pred)
  valid.pred <- predict(rfm, test_d)
  validation_error <- rmse(log(train_d$price), valid.pred)
  curve_data[i, ] <-  c(chunks[i], train.pred, valid.pred)
  
}

ggplot(curve_data, aes(size, train_error))+
  geom_line()+
  geom_line(aes(size, valid_error), color = 'red')


library(caret)

learnCurve <- learing_curve_dat(data_train1, test_prop = 0.75, verbose = T,
                                method = 'rf', outcome = 'price')
head(learnCurve)


ggplot(learnCurve, aes(Training_Size, y = RMSE, col = Data))+
  geom_smooth(method = 'loess')

ggplot(learnCurve, aes(Training_Size, y = Rsquared, col = Data))+
  geom_smooth(method = 'loess')
names(rf_data1)



##### improving the model ##########
######### editing quantity column

hist(data_train2$quantity, breaks = 500)
?hist

hist(data_train1$quantity, xlim = c(0,20), breaks = 1000)

length(which(data_train2$quantity > 100))
data_train2[quantity >= 100, ]%>%
  ggplot(aes(quantity,price,col = art_type))+
  geom_point()

data_train2[quantity == 1, ]%>%
  ggplot(aes(art_type,price))+
  geom_boxplot()

hist(sqrt(data_train2$quantity))

hist(1/data_train2$quantity)

ggplot(data = data_train2, aes(x = log(quantity), y = log(price))) +
  geom_point()

plot(cut(data_train2$quantity, breaks = 70))

plot(cut(data_train1$quantity, breaks = 70))

range(data_train1$quantity)

mean(data_train1$quantity)

temp1 <- data_train1$quantity - mean(data_train1$quantity)
hist(temp1)

rf_data1$morethan1 <- logical(nrow(rf_data1))

for(item in 1:nrow(rf_data1)){
  if(rf_data1$quantity[item] == 1){
    rf_data1$morethan1[item] <- F
  } else{
    rf_data1$morethan1[item] <- T
  }
}

train4 <- sample(nrow(rf_data1), nrow(rf_data1) * 0.8)
names(rf_data1)
data_train4 <- rf_data1[train4, -2] ## not using quantity, but morethanone col
data_test4 <- rf_data1[-train4, ]
rfm6_d1 <- randomForest(log(price) ~., data = data_train4) 
rfm6_d1
rfm6_d1$importance



######## PCA ....... just coz....... ###############
pca <- prcomp(~ views + num_favorers + quantity, data = data_train1, scale = T)

plot(pca)
head(pca$x)

ggplot(data = as.data.frame(pca$x), aes(PC1, PC2))+
  geom_point()


biplot(pca)
summary(pca)

## looking at correlation between variables

ggplot(data = rf_data1, aes(x = quantity, y = price, col = art_type))+
  geom_point()

ggplot(data = rf_data1, aes(x = views, y = price, col = art_type))+
  geom_point()

ggplot(data = rf_data1, aes(x = views, y = num_favorers))+
  geom_point()

rf_data1[art_type == 'dig.art', ]%>%
  ggplot(aes(quantity,price))+
  geom_point()

rf_data1[(art_type == 'dig.art' & price >= 300), ]



####### reclassify expensive digital art ######

exp.dig <- mod_data[(art_type == 'dig.art' & price >= 60), ] ## too expensive for digital art


exp.dig[grepl('acrylic', tax_col, ignore.case = T),]$art_type <- 'acrylic'
exp.dig[grepl('acrylic', tax_col, ignore.case = T),]$raw_mat <- 'canvas'

exp.dig[grepl('mixed', tax_col, ignore.case = T),]$art_type <- 'mixed'
exp.dig[grepl('canvas', mat_col, ignore.case = T),]$mat_col <- 'canvas'

exp.dig[grepl('oil', tax_col, ignore.case = T),]$art_type <- 'oil'

exp.dig[listing_id == 488073168]$art_type <- 'acrylic'
exp.dig[listing_id == 488073168]$raw_mat <- 'wat.paper'

exp.dig[grepl('canvas', description, ignore.case = T) & raw_mat == 'digital']$raw_mat <- 'canvas'
exp.dig[art_type == 'oil']$raw_mat <- 'canvas'

exp.dig[listing_id == 204903984]$art_type <- 'mixed'
exp.dig[listing_id == 204903984]$raw_mat <- 'canvas'

mod_data[listing_id %in% exp.dig$listing_id, ]$art_type <- exp.dig$art_type

###### can come back to this late, at the moment, just remove all digital art instances

######### removing all digital art ##################
rf_data2 <- rf_data1[art_type != 'dig.art']


ggplot(data =rf_data2, aes(x = quantity, y = price, col = art_type))+
  geom_point()

ggplot(data = rf_data2, aes(x = views, y = num_favorers))+
  geom_point()
hist(rf_data2$num_favorers/rf_data2$views)

for(i in 1:nrow(rf_data2)){
  if(rf_data2$views[i] != 0){
    rf_data2$rev[i] <- rf_data2$num_favorers[i]/rf_data2$views[i]
  } else {
    rf_data2$rev[i] <- 0
  }
}

table(is.na(rf_data2$rev))

ggplot(data = rf_data2, aes(x = rev, y = price, col = art_type))+
  geom_point()

names(rf_data2)

ggplot(data = rf_data2, aes(x = has_variations, y = price))+
  geom_boxplot()

ggplot(data = rf_data2, aes(x = is_customizable, y = price))+
  geom_boxplot()
ggplot(data = rf_data2, aes(x = when_made, y = price))+
  geom_boxplot()
ggplot(data = rf_data2, aes(x = who_made, y = price))+
  geom_boxplot()
ggplot(data = rf_data2, aes(x = raw_mat, y = price, col = art_type))+
  geom_boxplot()
table(rf_data2$raw_mat,rf_data2$art_type)

####### RF again with rf_data2 (which doesn't have digital art) #######

train2 <- sample(nrow(rf_data2), nrow(rf_data2) * 0.8)
data_train2 <- rf_data2[train2, ]
data_test2 <- rf_data2[-train2, ]

names(data_train2)
data_train2 <- data_train2[, c(1,2,5,6,7,8,9,10,11)]
dim(data_train2)
table(is.na(data_train2))

rfm1_d2 <- randomForest::randomForest(price ~., data_train2)
rfm1_d2
rfm1_d2$importance
rfm1_d2$rsq
plot(rfm1_d2)

rfm2_d2 <- randomForest::randomForest(price ~., data_train2, 
                                      sampsize = 0.6 * nrow(data_train2))
rfm2_d2

rfm3_d2 <- randomForest::randomForest(log(price) ~., data_train2)
rfm3_d2
rfm3_d2$importance
names(rf_data2)


rfm4_d2 <- randomForest(log2(price) ~., data = data_train2)
rfm4_d2


######### checking model again with digital art and without 'rev' column ##########
names(rf_data2)

train3 <- sample(nrow(rf_data2), nrow(rf_data2) * 0.8)
data_train3 <- rf_data2[train3, -11]
data_test3 <- rf_data2[-train3, -11]

rfm1_d3 <- randomForest(log(price) ~., data_train3)
rfm1_d3 ### this performs slightly worse than rfm5_d1 so continuing with that for now

####################### NLP ideas ###############################

#### word2vec
#### tf-idf :
#### named entity recognition
#### hard code tags column: classification to see which words occur together


hist(data_train2$quantity/data_train2$price)

################### Gradient Boosted Regression tree ###############
install.packages('gbm')
library(gbm)
names(data_train1)
str(data_traingb)
# remove logical vectors: doesnt work with GBM
data_traingb <- data_train1

data_traingb$is_customizable <- as.factor(data_traingb$is_customizable)
data_traingb$has_variations <- as.factor(data_traingb$has_variations)

gbm1_d1 <- gbm::gbm(log(price) ~., data = data_traingb, distribution = 'gaussian',
                    n.trees = 2000)

summary(gbm1_d1)
gbmWithCrossValidation = gbm(formula = log(price) ~ .,
                             distribution = "gaussian",
                             data = data_traingb,
                             n.trees = 2000,
                             shrinkage = .1,
                             n.minobsinnode = 200, 
                             cv.folds = 5,
                             n.cores = 1)
bestTreeForPrediction = gbm.perf(gbmWithCrossValidation)

#################### getting a new model without number of views ##################
rf_data4 <- rf_data1[, -"views"]

write.csv(rf_data4, "training_data.csv")

train4 <- sample(nrow(rf_data3), nrow(rf_data3) * 0.8)
data_train4 <- rf_data3[train4, ]
data_test4 <- rf_data3[-train4, ]


rfm1_d4 <- randomForest(log(price) ~., data_train4)
rfm1_d4

rfm1_d4$importance 

plot(rfm1_d4)
varImpPlot(rfm1_d4)

saveRDS(rfm1_d4, file = "rfm1_d4.rds")

predict_rfm1d4 <- predict(rfm1_d4, data_test4)
predict_rfm1d4

predperf_rfm1d4 <- data.frame(price = log(data_test4$price), RF.pred = predict_rfm1d4)
rmse_rfm1d4 <- caret::RMSE(predperf_rfm1d4$RF.pred, predperf_rfm1d4$price)

sqrt(mean((predperf_rfm1d4$price - predperf_rfm1d4$RF.pred)^2)) ## same as above

exp(rmse_rfm1d4) #### average error in Dollar value

ggplot(predperf_rfm1d4, aes(price, RF.pred))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0, size = 1.5, col = 'blue')+ ###### pretty good!!!!!!!!!!!!!
  theme_bw(base_size = 16)+
  ylim(0,6)+
  xlab('Ln(Actual)')+
  ylab('Ln(Predicted)')


qqnorm((predperf_rfm1d4$predicted - predperf_rfm1d4$price)/
         sd(predperf_rfm1d4$predicted - predperf_rfm1d4$price))

qqline((predperf_rfm1d4$predicted - predperf_rfm1d4$price)/
         sd(predperf_rfm1d4$predicted - predperf_rfm1d4$price))


plot(rfm1_d4$y, rfm1_d4$y - rfm1_d4$predicted)


################### learning curve for rf_data4 ###########################
table(!is.na(rf_data4)) #### no NA columns
library(caret)
library(doParallel)
registerDoParallel(cores = 3)
print(Sys.time())
learnCurve_rfm1d4 <- learing_curve_dat(rf_data4, test_prop = 1/4, verbose = T,
                                       method = 'rf', outcome = 'price')
print(Sys.time())




###### image processsing ?
###### clustering
###### history of the seller 
###### shopping

######### plots for Demo Day: week 2 ###############################
varImpPlot(rfm1_d4)
ggplot(mod_data, aes(price))+
  geom_histogram(bins = 200)+
  theme_bw(base_size = 16)+
  xlab('Price in USD')+
  ylab('Count')

ggplot(data = mod_data, aes(x = has_variations, y = price))+
  geom_boxplot()

mod_data[!(art_type %in% c('dig.art', 'prints'))]%>%
  ggplot(aes(x = when_made, y = price))+
  geom_boxplot()

ggplot(data = mod_data, aes(x = num_favorers, y = price))+
  geom_point()

ggplot(data = mod_data, aes(x = raw_mat, y = price))+
  geom_boxplot()+
  theme_bw(base_size = 16)+
  xlab('Base type')+
  ylab('Price in USD')

mod_data$raw_mat[mod_data$raw_mat == "unknown"] <- 'unclassified'


ggplot(mod_data, aes(art_type, price))+
  geom_boxplot()+
  theme_bw(base_size = 16)+
  xlab('Medium type')+
  ylab('Price in USD')


########################## painting style ################################################
table(cheap_art$sty)

length(table(cheap_art$sty))

cheap_art$description[grepl('size', cheap_art$description, ignore.case = T)]


##### new_model with dimensions #############
names(mod_data)
table(complete.cases(mod_data))
View(mod_data[complete.cases(mod_data), ])

table(!is.na(mod_data$dimen_item))

for(i in 1:ncol(mod_data)){
  print(names(mod_data)[i])
  print(table(!is.na(mod_data[, i])))
  
}

hist(cheap_art$item_length)
hist(cheap_art$item_height)
hist(cheap_art$item_width)

cheap_art$len1 <- NA
cheap_art$brd1 <- NA
cheap_art$thi1 <- NA

for(i in 1:nrow(cheap_art)){
  if(!is.na(cheap_art$item_length[i]) & !is.na(cheap_art$item_width[i]) &
     !is.na(cheap_art$item_height[i])){
    dimens <- c(cheap_art$item_length[i], cheap_art$item_width[i], cheap_art$item_height[i])
    cheap_art$len1[i] <- sort(dimens, decreasing = T)[1]
    cheap_art$brd1[i] <- sort(dimens, decreasing = T)[2]
    cheap_art$thi1[i] <- sort(dimens, decreasing = T)[3]
  }
  
}

table(!is.na(cheap_art$len1))

####### new mod data ########
names(cheap_art)
## better to use cheap_art_use which has pruned some outliers
cheap_art_use <- cheap_art[!(cheap_art$quantity == 7992), ]
names(cheap_art_use)
relfeat2 <- c('listing_id', 'title', 'description', 'price', 'quantity', 'views','num_favorers',
              'who_made', 'when_made', 'is_customizable', 'has_variations', 'tag_col',
              'mat_col', 'tax_col', 'art_type', 'raw_mat', 'len1', 'brd1', 'thi1')

mod_df2 <- cheap_art_use[ , relfeat2]
mod_data2 <- data.table(mod_df2)
setkeyv(mod_data2, c("art_type", "price", "listing_id"))

oil.res2 <- grepl('oil color | oil painting', mod_data2$mat_col)
table(oil.res2)

for(plugs in 1:nrow(mod_data2)){
  if( is.na(mod_data2$art_type[plugs] ) & ( oil.res2[plugs] )){
    mod_data2$art_type[plugs] <- 'oil'
    
  }
}

oil.res3 <- grepl('oil color | oil painting', mod_data2$tag_col, ignore.case = T)
table(oil.res3)


for(plugs in 1:nrow(mod_data2)){
  if( is.na(mod_data2$art_type[plugs] ) & ( oil.res3[plugs] )){
    mod_data2$art_type[plugs] <- 'oil'
    
  }
}

table(mod_data2$art_type)

names(mod_data2)
modelfeat <- c('price', 'quantity', 'views', 'num_favorers', 'who_made',
               'when_made', 'is_customizable', 'has_variations', 'art_type',
               'raw_mat', 'len1', 'brd1', 'thi1')

rf_data3 <- mod_data2[,..modelfeat]
rf_data3 <- data.table(rf_data3, stringsAsFactors = T)
rf_data3 <- rf_data3[complete.cases(rf_data3), ]
setkeyv(rf_data1, c('price', 'art_type'))
str(rf_data1)
names(rf_data1)

ggplot(rf_data3, aes(len1, price, col = art_type))+
  geom_point()

ggplot(rf_data3, aes(len1*brd1, log(price), col = art_type))+
  geom_point()

######### running a learning curve created by myself ############
train1 <- sample(nrow(rf_data3), nrow(rf_data3) * 0.7)
training <- rf_data3[train1, ]
validat1 <- rf_data3[-train1, ]

test1 <- sample(nrow(validat1), nrow(validat1) * 0.3)
testing1 <- validat1[test1, ]
validat1 <- validat1[-test1, ]

rfm1_d3 <- randomForest::randomForest(log(price) ~., training)

rfm1_d3

prediction1 <- predict(rfm1_d3, testing1)

modcheck1 <- data.frame(actual = log(testing1$price),
                        prediction = prediction1)

ggplot(modcheck1, aes(actual, prediction))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0, col = 'blue')

exp(caret::RMSE(modcheck1$prediction, modcheck1$actual))

### square footage
training

ggplot(rf_data3, aes(views))+
  geom_histogram()

ggplot(rf_data3, aes(len1 * brd1))+
  geom_histogram()

ggplot(rf_data3, aes(len1 * brd1, quantity))+
  geom_point()

ggplot(rf_data3, aes(len1 * brd1, views))+
  geom_point()

ggplot(rf_data3, aes(len1 * brd1, num_favorers))+
  geom_point()

names(rf_data3)
trans = caret::preProcess(rf_data3[ ,-1], method = c('BoxCox','center','scale'))

trans.predictors = data.frame(trans = predict(trans, rf_data3))

ggplot(trans.predictors, aes(trans.len1 * trans.brd1, trans.num_favorers))+
  geom_point()


chunks <- seq(ceiling(nrow(rf_data3) * 0.1), nrow(rf_data3), 1000)

rmse <- function(actual, predicted) sqrt( mean( (actual - predicted)^2 ))
curve_data <- data.frame(size = integer(length(chunks)),
                         train_error = integer(length(chunks)),
                         valid_error = integer(length(chunks)))

for(i in 1:length(chunks)){
  ### establish training size
  size <- chunks[i]
  ### output message for logging progress
  message("Running model for size", size)
  ### random sampling input data for training size
  sampleidx <- sample(nrow(rf_data3), size)
  train_d <- rf_data3[sampleidx, ]
  test_d <- rf_data3[-sampleidx, ]
  rfm <- randomForest::randomForest(log(price) ~., train_d)
  train.pred <- predict(rfm, train_d)
  training_error <- rmse(log(train_d$price), train.pred)
  valid.pred <- predict(rfm, test_d)
  validation_error <- rmse(log(train_d$price), valid.pred)
  curve_data[i, ] <-  c(chunks[i], train.pred, valid.pred)
}

ggplot(curve_data, aes(size, train_error))+
  geom_line()+
  geom_line(aes(size, valid_error), color = 'red')

# library(caret)

# Sys.time()
# learncurve1 <- learing_curve_dat(rf_data3, outcome = 'price', test_prop = 1/4,
#                                  verbose = T, method = 'rf')
# 
# Sys.time()

################ generate style of painting ##################

cheap_art$sty_col <- sapply(cheap_art$style, paste0, collapse = ',')
table(cheap_art$sty_col)

for(i in 1:length(cheap_art$sty_col)){
  if(cheap_art$sty_col[i] == ""){
    cheap_art$sty_col[i] <- NA
  }
}
table(!is.na(cheap_art$sty_col))

levels(as.factor(cheap_art$sty_col))
?str_split_fixed

str_split_fixed(cheap_art$sty_col, ',')

library(stringr)

data_text <- maj_data[ , c('listing_id', 'title', 'description', 'item_length',
                           'item_height', 'item_width')]
write.csv(data_text, 'text_data.csv')

new_text_dat <- read.delim('final_text_data.txt', sep = '~')



for(i in 1:nrow(data_text)){
  print(nchar(data_text[i, 3]))
}















