
       
purchase_behaviour <- read.csv("data_purchase_behaviour.csv")
View(purchase_behaviour) # View the data


x <- purchase_behaviour$Age_num 
y <- purchase_behaviour$Purchase 
x_bar  <- mean(x) # 
y_bar  <- mean(y) # 
n      <- length(x)  

slope_hat         <- sum( (x-x_bar)*y ) / sum((x-x_bar)^2)
print(slope_hat)
# The fitted slope of the model is 6.6806                   

intercept_hat     <- y_bar - slope_hat*x_bar                
print(intercept_hat)
# The fitted intercept of the model is 9037.936      

sigma_hat         <- sqrt(  sum( (y-(intercept_hat + slope_hat*x))^2   ) / (n-2) ) # 
print(sigma_hat)            

std_intercept_hat  <- sigma_hat / sqrt( sum( (x-x_bar)^2 ) )        
print(std_intercept_hat )
# The standard error around the fitted intercept is 1.071577          

std_slope_hat      <- sigma_hat  *  sqrt( sum(x^2) / (n*sum((x-x_bar)^2)) ) 
print(std_slope_hat)

