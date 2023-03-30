#Avi Milan Jani
#ALY6050-MOD4
#PART 1
#1. Defining the data, uncontrollable inputs, model parameters, and the decision variables that
#influence the total inventory cost
##Data given
annual_demand<-15000
per_unit_cost<-75
order_multiple <- 2.0
opportunity_cost_percent<-0.165

##uncontrollable inputs
holding_cost_per_unit <- per_unit_cost*0.165
holding_cost_per_unit
ordering_cost_per_order <- 180

#--------------------------------------------------------------------------------------------------------------------------------

#2.Develop mathematical functions
annual_demand
class(annual_demand)

#Economical order Quantity(EOQ)
EOQ <- sqrt(2 * annual_demand * ordering_cost_per_order / holding_cost_per_unit)
EOQ
print(paste("EOQ:", round(EOQ, digits = 0)))

# Compute the number of orders
number_of_orders <- annual_demand / EOQ
number_of_orders
print(paste("Number of orders:", round(number_of_orders, digits = 0)))

# Compute the annual ordering cost
annual_ordering_cost <- number_of_orders * ordering_cost_per_order
annual_ordering_cost
print(paste("Annual ordering cost:", round(annual_ordering_cost, digits = 0)))

# Compute the average inventory
average_inventory <- EOQ / 2
average_inventory
print(paste("Average Inventory:", round(average_inventory, digits = 0)))

# Compute the annual holding cost total
annual_holding_cost_total <- average_inventory * holding_cost_per_unit * number_of_orders
annual_holding_cost_total
print(paste("Total average holding cost:", round(annual_holding_cost_total, digits = 2)))


# Compute the annual holding cost
annual_holding_cost <- (330) * 12.375
annual_holding_cost
print(paste("Average holding cost:", round(annual_holding_cost, digits = 0)))


# Compute the total inventory cost
total_inventory_cost <- annual_ordering_cost + annual_holding_cost
total_inventory_cost
print(paste("Total inventory cost:", round(total_inventory_cost, digits = 0)))


# Print the results
print(paste("EOQ:", round(EOQ, digits = 0)))
print(paste("Number of orders:", round(number_of_orders, digits = 0)))
print(paste("Annual ordering cost:", round(annual_ordering_cost, digits = 0)))
print(paste("Average Inventory:", round(average_inventory, digits = 0)))
print(paste("Total average holding cost:", round(annual_holding_cost_total, digits = 2)))
print(paste("Average holding cost:", round(annual_holding_cost, digits = 0)))
print(paste("Total inventory cost:", round(total_inventory_cost, digits = 0)))


#--------------------------------------------------------------------------------------------------------------------------------


#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#3.find an approximate order quantity that results in the smallest total cost
# Define the optimization function to minimize the total inventory cost
eoq_optimization_fn <- function(order_quantity) {
  annual_ordering_cost <- ordering_cost_per_order * (annual_demand / order_quantity)
  annual_holding_cost <- (order_quantity / 2) * holding_cost_per_unit * (annual_demand / order_quantity)
  total_inventory_cost <- annual_ordering_cost + annual_holding_cost
  return(total_inventory_cost)
}

# Use the optimize function to find the order quantity that minimizes the total inventory cost
result <- optimize(eoq_optimization_fn, c(1000, 15000), tol=0.001)

# Extract the optimal order quantity
optimal_order_quantity <- result$minimum

# Compute the corresponding annual ordering cost, annual holding cost, and total inventory cost
annual_ordering_cost <- ordering_cost_per_order * (annual_demand / optimal_order_quantity)
annual_holding_cost <- (optimal_order_quantity / 2) * holding_cost_per_unit * (annual_demand / optimal_order_quantity)
total_inventory_cost <- annual_ordering_cost + annual_holding_cost

# Print the results
cat("Optimal order quantity: ", optimal_order_quantity, "\n")
cat("Annual ordering cost: ", annual_ordering_cost, "\n")
cat("Annual holding cost: ", annual_holding_cost, "\n")
cat("Total inventory cost: ", total_inventory_cost, "\n")

#
# Calculate the annual ordering and holding costs for different order quantities
order_quantities <- seq(from = annual_demand / order_multiple, to = annual_demand * 2, by = 100)
annual_ordering_costs <- ordering_cost_per_order * (annual_demand / order_quantities)
annual_holding_costs <- holding_cost_per_unit * (annual_demand / 2) * (order_quantities / annual_demand)
total_costs <- annual_ordering_costs + annual_holding_costs

# Create a data frame with order quantities, annual ordering costs, annual holding costs, and total costs
data <- data.frame(Order_Quantity = order_quantities, 
                   Annual_Ordering_Cost = annual_ordering_cost, 
                   Annual_Holding_Cost = annual_holding_cost, 
                   Total_Cost = total_inventory_cost)
data

# Find the order quantity with the smallest total cost
min_cost_row <- data[which.min(data$Total_Cost), ]
min_cost_row
optimal_order_quantity <- min_cost_row$Order_Quantity
optimal_order_quantity

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#3.find an approximate order quantity that results in the smallest total cost
# Define the range of order quantities to analyze
order_quantities <- seq(from = EOQ - 500, to = EOQ + 500, by = 25)

# Compute the total inventory cost for each order quantity
total_costs <- c()
for (order_quantity in order_quantities) {
  # Compute the number of orders
  number_of_orders <- annual_demand / order_quantity
  
  # Compute the annual ordering cost
  annual_ordering_cost <- number_of_orders * ordering_cost_per_order
  
  # Compute the average inventory
  average_inventory <- order_quantity / 2
  
  # Compute the annual holding cost
  annual_holding_cost <- average_inventory * holding_cost_per_unit * number_of_orders
  
  # Compute the total inventory cost
  total_inventory_cost <- annual_ordering_cost + annual_holding_cost
  
  # Store the result
  total_costs <- c(total_costs, total_inventory_cost)
}


#---------------------------------------------------------------------------------------------------------------------------------
##4. Plot the results
plot(order_quantities, total_costs, type = "l", xlab = "Order Quantity", ylab = "Total Cost")

# Find the order quantity that results in the smallest total cost
min_index <- which.min(total_costs)
optimal_order_quantity <- order_quantities[min_index]
min_total_cost <- total_costs[min_index]
min_total_cost

# Print the results
cat("Optimal order quantity:", round(optimal_order_quantity, digits = 0), "\n")
cat("Minimum total cost:", round(min_total_cost, digits = 0), "\n")


#_______________________________________________________________________________________________________________________________________________________

#PART2--------------------------------------------------------------------------------------------------------------------------------------------------
# Define the annual demand range and mode
lower_bound <- 10000
upper_bound <- 18000
mode <- 15000

# Define the number of simulations
num_simulations <- 10000

# Define the data, uncontrollable inputs, model parameters, and decision variables
annual_demand <- 15000
per_unit_cost <- 75
order_multiple <- 2.0
opportunity_cost_percent <- 0.165
holding_cost_per_unit <- per_unit_cost * opportunity_cost_percent
ordering_cost_per_order <- 180

# Initialize a vector to store the total costs
total_costs <- numeric(num_simulations)

# Loop over the number of simulations
for (i in 1:num_simulations) {
  # Simulate the annual demand
  annual_demand <- runif(1, lower_bound, upper_bound)
  if (annual_demand > mode) {
    annual_demand <- lower_bound + upper_bound - annual_demand
  }
  
  # Compute the EOQ
  EOQ <- sqrt(2 * annual_demand * ordering_cost_per_order / holding_cost_per_unit)
  
  # Compute the number of orders
  number_of_orders <- annual_demand / EOQ
  
  # Compute the annual ordering cost
  annual_ordering_cost <- number_of_orders * ordering_cost_per_order
  
  # Compute the average inventory
  average_inventory <- EOQ / 2
  
  # Compute the annual holding cost
  annual_holding_cost <- average_inventory * holding_cost_per_unit * number_of_orders
  
  # Compute the total inventory cost
  total_inventory_cost <- annual_ordering_cost + annual_holding_cost
  
  # Store the total cost
  total_costs[i] <- total_inventory_cost
}

# Plot the distribution of total costs
hist(total_costs, main = "Distribution of Total Inventory Cost", xlab = "Total Cost")

# Compute the mean and standard deviation of total costs
mean_total_cost <- mean(total_costs)
sd_total_cost <- sd(total_costs)

# Print the results
cat("Mean total cost:", round(mean_total_cost, digits = 0), "\n")
cat("Standard deviation of total cost:", round(sd_total_cost, digits = 0), "\n")

install.packages(MASS)
library(MASS)
#---------------------------------------------------------------------------------------------------------------------------------
#i)#Compute the 95% confidence interval for the expected minimum total cost
alpha <- 0.05
lower_limit <- mean_total_cost - qt(1 - alpha / 2, df = num_simulations - 1) * sd_total_cost / sqrt(num_simulations)
upper_limit <- mean_total_cost + qt(1 - alpha / 2, df = num_simulations - 1) * sd_total_cost / sqrt(num_simulations)
cat("95% confidence interval for expected minimum total cost: [", round(lower_limit, digits = 0), ",", round(upper_limit, digits = 0), "]\n")

#Determine the probability distribution that best fits the distribution of total costs
fit <- fitdistr(total_costs, "normal")
fit

#Verify the validity of the choice of normal distribution
ks.test(total_costs, "pnorm", mean = fit$estimate[1], sd = fit$estimate[2])


#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#ii)Estimate expected order quantity

#Initialize a vector to store the order quantities
order_quantities <- numeric(num_simulations)

#Loop over the number of simulations
for (i in 1:num_simulations) {
  # Simulate the annual demand
  annual_demand <- runif(1, lower_bound, upper_bound)
  if (annual_demand > mode) {
    annual_demand <- lower_bound + upper_bound - annual_demand
  }
  #Compute the EOQ
  EOQ <- sqrt(2 * annual_demand * ordering_cost_per_order / holding_cost_per_unit)
  
  #Store the order quantity
  order_quantities[i] <- EOQ
}

#Compute the mean and standard deviation of order quantities
mean_order_quantity <- mean(order_quantities)
sd_order_quantity <- sd(order_quantities)

#Print the results
cat("Mean order quantity:", round(mean_order_quantity, digits = 0), "\n")
cat("Standard deviation of order quantity:", round(sd_order_quantity, digits = 0), "\n")

#Compute the 95% confidence interval for the expected order quantity
lower_limit <- mean_order_quantity - qt(1 - alpha / 2, df = num_simulations - 1) * sd_order_quantity / sqrt(num_simulations)
upper_limit <- mean_order_quantity + qt(1 - alpha / 2, df = num_simulations - 1) * sd_order_quantity / sqrt(num_simulations)
cat("95% confidence interval for expected order quantity: [", round(lower_limit, digits = 0), ",", round(upper_limit, digits = 0), "]\n")

#Determine the probability distribution that best fits the distribution of order quantities
fit <- fitdistr(order_quantities, "normal")

#Verify the validity of the choice of normal distribution
ks.test(order_quantities, "pnorm", mean = fit$estimate[1], sd = fit$estimate[2])

#---------------------------------------------------------------------------------------------------------------------------
#iii)
#Compute the annual number of orders in each simulation
number_of_orders <- 15000 / 639
number_of_orders

#Estimating the Expected Annual Number of Orders
#Initialize a vector to store the number of orders
number_of_orders <- numeric(num_simulations)


#Loop over the number of simulations
for (i in 1:num_simulations) {
  
  #Simulate the annual demand
  annual_demand <- runif(1, lower_bound, upper_bound)
  if (annual_demand > mode) {
    annual_demand <- lower_bound + upper_bound - annual_demand
  }
  
  #Compute the EOQ
  EOQ <- sqrt(2 * annual_demand * ordering_cost_per_order / holding_cost_per_unit)
  
  #Compute the number of orders
  number_of_orders[i] <- annual_demand / EOQ
}

#Plot the distribution of number of orders
hist(number_of_orders, main = "Distribution of Number of Orders", xlab = "Number of Orders")

#Compute the mean and standard deviation of number of orders
mean_number_of_orders <- mean(number_of_orders)
sd_number_of_orders <- sd(number_of_orders)

#Print the results
cat("Mean number of orders:", round(mean_number_of_orders, digits = 2), "\n")
cat("Standard deviation of number of orders:", round(sd_number_of_orders, digits = 2), "\n")

#Compute the 95% confidence interval for the expected annual number of orders
lower_limit <- mean_number_of_orders - qt(1 - alpha / 2, df = num_simulations - 1) * sd_number_of_orders / sqrt(num_simulations)
upper_limit <- mean_number_of_orders + qt(1 - alpha / 2, df = num_simulations - 1) * sd_number_of_orders / sqrt(num_simulations)
cat("95% confidence interval for expected annual number of orders: [", round(lower_limit, digits = 2), ",", round(upper_limit, digits = 2), "]\n")

#Determine the probability distribution that best fits the distribution of number of orders
fit <- fitdistr(number_of_orders, "normal")

#Verify the validity of the choice of normal distribution
ks.test(number_of_orders, "pnorm", mean = fit$estimate[1], sd = fit$estimate[2])

#_________________________________________________________________________________________________________________________________________________________________________


