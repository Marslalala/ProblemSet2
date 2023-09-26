###Problem 2


## Part a

#  Load the data
car_data <- read.csv('cars.csv')

#  Rename the data
col_names <- c('dim_hei', 'dim_len', 'dim_wid', 'eng_driveline', 'eng_type', 'eng_hyb', 'eng_gearnum', 
               'eng_trans', 'fuel_city', 'fuel_type', 'fuel_highway', 'id_class', 'uid', 'id_make', 
               'id_myear', 'id_ryear', 'enstat_horsep', 'enstat_tor')
colnames(car_data) <- col_names
head(car_data)


## Part b

#  Modify the data to restrict the fuel type
car_gasoline <- car_data[car_data$fuel_type == 'Gasoline',]


## Part c

#  Fit the model by instructions
car_gasoline$id_ryear <- as.factor(car_gasoline$id_ryear)
car_model <- lm(fuel_highway ~ enstat_horsep + enstat_tor + dim_hei + dim_len + dim_wid + 
                  id_ryear, data = car_gasoline)

#  Generate a summary of the model
summary(car_model)
#  When all other variables in the model held constant, we can see from the summary that the predictor
#  horsepower is positively related with the MPG on the highway as the estimate of the coefficient is
#  positive. To be more explicitly, as horsepower increases, MPG on the highway is expected to increase.
#  The estimated coefficient is 0.016, which means when horsepower increases for 1 unit, the MPG on the 
#  highway would increase for 0.016. An important thing should be noticed, which is the value is for cars 
#  released in 2009(with this estimated intercept). When consider other releasing years, the estimated 
#  intercept might change in this model. The p-value for the estimated coefficient of horsepower is 
#  7.96e-13 which is very small. This implies that this relationship between MPG on the highway and
#  horsepower is statistically significant. They are very likely to have this positive relationship.


## Part d

#  Generate a new model with interactions between horsepower and torque.
car_model2 <- lm(fuel_highway ~ enstat_horsep + enstat_tor + enstat_horsep:enstat_tor + dim_hei + 
                   dim_len + dim_wid + id_ryear, data = car_gasoline)
#  Give a summary of this new model
summary(car_model2)

#  Firstly, plot histograms of horsepower and torque to determine which values we should choose for the 
#  interaction plot.
hist(car_gasoline$enstat_horsep)
hist(car_gasoline$enstat_tor)
#  From the graphs above , we know that both horsepower and torque are gathered from 100 to 450, so we can
#  take values 100, 275 and 450.
chosen_value <- c(100, 275, 450)

#  From the summary we can tell that there does have an interaction between horsepower and torque. Now
#  generate an interaction plot to see the relationship.
library(interactions)
interact_plot(car_model2, pred = enstat_horsep, modx = enstat_tor, modx.values = chosen_value, 
              at = list(car_gasoline$id_ryear == 2010), x.label = 'horsepower', y.label = 'highway mpg', 
              legend.main = 'torque', data = car_gasoline)


## Part e

#  Design a matrix to calculate beta^hat.
X <- model.matrix(fuel_highway ~ enstat_horsep * enstat_tor + enstat_horsep + enstat_tor + dim_len + 
                  dim_wid + dim_hei + as.factor(id_ryear), data = car_gasoline)
#  Create response matrix
Y <- car_gasoline$fuel_highway

#  Calculate the coefficient estimates manually
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_hat)
#  We can see the results are exactly the same as in the lm() given.