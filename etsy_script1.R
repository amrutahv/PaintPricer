packrat::init()

####### libraries required ############

library(jsonlite)
library(dplyr)
library(ggplot2)
library(stringr)
library(randomForest)
library(data.table)
library(dplyr)
#####  downloading data from the Etsy API #####

#listings <- fromJSON('https://openapi.etsy.com/v2/listings/active?api_key=gxx1r5uo1zpe5c55jf003xn5&category=art/painting')

#shops <- fromJSON('https://openapi.etsy.com/v2/shops?api_key=gxx1r5uo1zpe5c55jf003xn5', flatten = T)

url_list <- 'https://openapi.etsy.com/v2/listings/active?api_key=gxx1r5uo1zpe5c55jf003xn5&category=art/painting&limit=100'

#list1 <- fromJSON(url_list)
#list2a <- fromJSON(paste0(url_list,'&page=2'))

# pages1 <- list()
# 
# for(i in 1:501){
#   mydata <- fromJSON(paste0(url_list,'&page=', i))
#   message("Retrieving page ", i)
#   pages1[[i]] <- mydata$results
#   message(object.size(pages1))
#   if(i == 200){
#     Sys.sleep(60)
#     message("Waiting 1 min at", i)
#   } else if(i == 350){
#     Sys.sleep(60)
#     message("Waiting 1 min at", i)
#   }
#   
# }
# 
# maj_data <- rbind_pages(pages1) # final masterfile with all listings

# sink("maj_data.txt")
# for(i in 1:nrow(maj_data)){
#   print(maj_data[i,])
#   
# }
# sink()

########## next big updated file: get from auto_dload.Rdata ###################
names(big_data)

####### cleaning masterfile to remove redundant columns ########
unnecessary_names <- c('category_id', 'category_path', 'category_path_ids')

min_data <- big_data[,!names(big_data) %in% unnecessary_names] # working masterfile
 
typeof(min_data) # this object is a list of several vectors. Need to simplify

table(min_data$currency_code) ### several foreign currencies: remove not USD
table(min_data$language) ### several foreign languages: difficult to use in NLP

min_data <- subset(min_data, min_data$currency_code == 'USD' & 
                     (min_data$language == 'en-US' | min_data$language == 'MACHINE_en'))

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
dim(cheap_art)
cheap_art <- cheap_art [cheap_art$listing_id != p, ]

cheap_art <- cheap_art[cheap_art$listing_id != 465247738, ] ## this contains a paper doll

### remove duplicated listings

cheap_art <- cheap_art[!duplicated(cheap_art$listing_id), ] 
row.names(cheap_art) <- cheap_art$listing_id

###### converting list columns into a simple vector ##########

simplify_lists <- function(col_name, separator){
  new_vec <- sapply(col_name, paste0, collapse = separator)
  return(new_vec)
}

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


unrel.res <- grepl('christmas ball', cheap_art$mat_col, ignore.case = T)|
  grepl('dolls', cheap_art$title, ignore.case = T) ### remove christmas balls & dolls
table(unrel.res)

cheap_art <- cheap_art[!(unrel.res), ]

vid.res <- grepl('video lesson', cheap_art$title, ignore.case = T)
table(vid.res)
View(cheap_art[vid.res, ])
cheap_art <- cheap_art[!(vid.res), ]


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
mix.res5 <- grepl('acrylic', cheap_art$tag_col, ignore.case = T) &
  grepl('watercolor', cheap_art$tag_col, ignore.case = T)
table(mix.res5)

for (i in 1:length(mix.res)){
  if ( mix.res[i] | mix.res2[i] | mix.res3[i] | mix.res4 [i])  {
    cheap_art$art_type[i] <- 'mixed'
  } 
}

mixed_paintings <- c()
for (i in 1:length(mix.res5)){
  if (mix.res5[i]){
    mixed_paintings[i] <- cheap_art$listing_id[i]
  }
}

for (i in 1:length(mix.res2)){
  if (mix.res2[i] & oil.res [i]){
    mixed_paintings[i] <- cheap_art$listing_id[i]
  }
}

for (i in 1:length(mix.res)){
  if (mix.res2[i] & acr.res [i]){
    mixed_paintings[i] <- cheap_art$listing_id[i]
  }
}

for (i in 1:length(mix.res)){
  if (mix.res[i] & wat.res [i]){
    mixed_paintings[i] <- cheap_art$listing_id[i]
  }
}

table(!is.na(mixed_paintings))
mixed_paintings <- mixed_paintings[!is.na(mixed_paintings)]


View(cheap_art[ cheap_art$listing_id %in% mixed_paintings, ])

table(cheap_art$art_type)

table(!is.na(cheap_art$art_type))

#### classifying the acrylic paintings
acr.res <- grepl('acrylic', cheap_art$tax_col, ignore.case = T) 
table(acr.res)
acr.res2 <- grepl('acrylic', cheap_art$tag_col, ignore.case = T)
table(acr.res2)

for (i in 1:nrow(cheap_art)){
  if ( acr.res[i] ){
    cheap_art$art_type[i] <- 'acrylic'
  }
}

table(cheap_art$art_type)

for(i in 1:nrow(cheap_art)){
  if (is.na(cheap_art$art_type [i]) & acr.res2[i]){ # to prevent re-coding of mixed paintings
    cheap_art$art_type[i] <- 'acrylic'
  }
}
### watercolors 
wat.res <- grepl('watercolor', cheap_art$tax_col, ignore.case = T) 
table(wat.res)
wat.res

for (i in 1:nrow(cheap_art)){
  if ( ( wat.res[i] ) &  is.na(cheap_art$art_type [i] ) ){
    cheap_art$art_type[i] <- 'watercolor'
  } 
}
table(cheap_art$art_type)

#### oil paintings
oil.res <- grepl('oil', cheap_art$tax_col, ignore.case = T) 
table(oil.res)  
  
for (i in 1:nrow(cheap_art)){
  if ( oil.res[i] & is.na(cheap_art$art_type [i] )){
    cheap_art$art_type[i] <- 'oil'
  } 
}


table(cheap_art$art_type)

### also classifying the prints
pri.res <- grepl('prints', cheap_art$tax_col, ignore.case = T)
table(pri.res)
pri.res.2 <- grepl('print', cheap_art$tag_col, ignore.case = T)
table(pri.res.2)

for (i in 1:nrow(cheap_art)){
  if ( pri.res.2[i] & is.na(cheap_art$art_type [i] )){
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

View(cheap_art[cheap_art$is_digital == T, ])
table(cheap_art$is_digital)

dig.res <- grepl("digital art|clip art|clipart", cheap_art$title, ignore.case = T)
table(dig.res)

dig.res2 <- grepl('digital art|clip art|digital', cheap_art$mat_col, ignore.case = T)
table(dig.res2)
dig.res3 <- grepl('digital download|instant download|download|digital|e-pattern', cheap_art$description, ignore.case = T)
table(dig.res3)
dig.res4 <- grepl('digital art|clip art', cheap_art$tag_col, ignore.case = T)
table(dig.res4)

for (i in 1:nrow(cheap_art)){
  if( dig.res[i] | dig.res2[i] | dig.res3[i] | dig.res4[i] ){
    cheap_art$art_type[i] <- 'dig.art'
  }
}

View(cheap_art[cheap_art$is_digital & 
                 cheap_art$art_type != 'dig.art', ])

table(cheap_art$art_type)
table(!is.na(cheap_art$art_type))

### other types: spray paint, ink
oth.res <- grepl('spray|ink', cheap_art$tax_col, ignore.case = T)
table(oth.res)

for(i in 1:nrow(cheap_art)){
  if( oth.res [i] ){
    cheap_art$art_type [i] <- 'other'
  }
}
table(cheap_art$art_type)

#### nearly 27000 paintings have been classified correctly
table(!is.na(cheap_art$art_type))

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
can.res2 <- grepl('canvas', cheap_art$tag_col, ignore.case = T)
table(can.res2)

for(i in 1:nrow(cheap_art)){
  if( can.res [i] | can.res2 [i] ){
    cheap_art$raw_mat[i] <- 'canvas'
    
  }
}


table(cheap_art$raw_mat)

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

test_str <- cheap_art$description[3]

library(stringr)
m1 <- str_match(cheap_art$description[1:10], "size.*?([0-9.]{1,3}.?)")
m2 <- str_match(x, "([0-9.]{1,3}.year[s]?).*?block")
sapply(seq_along(x), function(i) {
  if (is.na(m1[i, 1])) m2[i, 2]
  else if (is.na(m2[i, 1])) m1[i, 2]
  else if (str_length(m1[i, 1]) < str_length(m2[i, 1])) m1[i, 2]
  else m2[i, 2]
})
dim.res <- grepl('dimension', cheap_art$description, ignore.case = T)
table(dim.res)
View(cheap_art[dim.res, ])

t.dim <- cheap_art[dim.res, ]

str_match_all(t.dim$description, '[0-9]+&quot')[1:10]

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

ggplot(learnCurve, aes(Training_Size, y = RMSE, col = Data))+
  geom_smooth(method = 'loess')

ggplot(learnCurve_rfm1d4, aes(Training_Size, y = Rsquared, col = Data))+
  geom_smooth(method = 'loess')
names(rf_data1)




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








