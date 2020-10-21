
library(e1071)
library(randomForest)
library(dplyr)

#TODO:
#+ beef this up to multipredictor mode
#+ debug nash sutcliffe
#+ use real data!
#+ try it with sub-daily data

#setup ####

# number of models to run
n = 50

#--Nash Sutcliffe-- (just a goodness-of-fit measure)
NashSutcliffe = function(predicted, measured){
  NS = 1-sum((predicted-measured)^2, na.rm=T)/sum((measured-mean(measured, na.rm=T))^2, na.rm=T)
  return(data.frame(NS))
}

# datetime = seq(as.POSIXct('2020-01-01 00:00:00',
#                           tz = 'UTC'),
#                as.POSIXct('2020-01-05 23:45:00',
#                           tz = 'UTC'),
#                by = '15 min')
dates = seq(as.Date('2020-01-01'),
            as.Date('2020-03-30'),
            by = 'day')

solute_dates = sort(sample(dates, 15))

# a data frame of all sensor data
y = tibble(FDOM = rnorm(length(dates), 0, 1),
           turb = rnorm(length(dates), 5, 1),
           pH = rlnorm(length(dates), 0, 1),
           SC = runif(length(dates), 0, 1),
           DO = rt(length(dates), 5, 1),
           doy = as.integer(strftime(dates, format = '%j')))

# a data frame of sensor data matched to measured solutes (sol1, sol2)
x1 = y %>%
    filter(doy %in% !!as.integer(strftime(solute_dates,
                                          format = '%j'))) %>%
    # rename_with(.fn = function(x) substr(x, 1, nchar(x) - 1)) %>%
    mutate(
        sol1 = rnorm(length(solute_dates), 0, 1),
        sol2 = rlnorm(length(solute_dates), 4, 3)) %>%
    select(sol1, sol2, everything())

#single solute mode ####

# make the containers to store intermediate results
preds = matrix(NA, nrow(y), n)
preds99 = matrix(NA, nrow(x1), n)

# loop through the n models
for(i in 1:n){
    #train a model with a random subset
    a = sample(1:nrow(x1), as.integer(nrow(x1)/1.5))
    x = x1[a,]
    model = svm(sol1~FDOM+turb+pH+SC+DO+doy, data=x) # can also be a randomForest model

    # make the prediction of the rest of the data
    # this prediction is for assessing the model goodness-of-fit
    a2 = setdiff(1:nrow(x1), a)
    x = x1[a2,2:7]
    preds99[a2,i] = predict(model, newdata=x)

    # predict the solute concentration for each sensor measurment
    x = y
    preds[,i] = predict(model, newdata=x)
}

X.p = apply(preds99, 1, mean, na.rm=T) # predicted
X.a = x1[,1] # measured
NashSutcliffe(X.p, X.a)

#estimate your actual shit
solute_at_daily_interval = apply(preds, 1, mean, na.rm = TRUE)
#equivalent to:
solute_at_daily_interval = rowMeans(preds, na.rm = TRUE)

plot(y$doy, solute_at_daily_interval, type='l')
points(x1$doy, x1$sol1, col='red')

#multiple solute mode (outline) ####

# make the containers to store intermediate results
preds = array(NA, dim=c(nrow(y), n, 2))
preds99 = array(NA, dim=c(nrow(x1), n, 2))

# loop through the n models
for(i in 1:n){
    #train a model with a random subset
    a = sample(1:nrow(x1), as.integer(nrow(x1)/1.5))
    x = x1[a,]
    model = svm(sol1+sol2 ~ FDOM+turb+pH+SC+DO+doy, data=x) # can also be a randomForest model

    # make the prediction of the rest of the data
    # this prediction is for assessing the model goodness-of-fit
    a2 = setdiff(1:nrow(x1), a)
    x = x1 %>%
        select(-starts_with('sol')) %>%
        slice(a2)
    preds99[a2,i,] = predict(model, newdata=x)

    # predict the solute concentration for each sensor measurment
    x = y
    preds[,i,] = predict(model, newdata=x) #NOPE: even though there are two
        #independent variables, this only generates a single vector
}

#GOF calculation here

#estimate your actual shit
solutes_at_daily_interval = apply(preds, c(1, 3), mean, na.rm = TRUE) %>%
    as_tibble() %>%
    rename(sol1 = V1, sol2 = V2)

plot(y$doy, solutes_at_daily_interval$sol1, type='l', col='blue')
lines(y$doy, solutes_at_daily_interval$sol2, col='green')
points(x1$doy, x1$sol1, col='darkblue')
points(x1$doy, x1$sol1, col='darkgreen')
