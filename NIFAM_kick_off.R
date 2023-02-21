library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)

####TO DO List for installation####
#devtools::install_github("eikeluedeling/decisionSupport", 
#ref = "fix_plot_evpi")
#install.packages("Rtools 4.0")
#library("Rtools 4.0")

####Get data####

input_estimates <-read.csv2("./Session-week2-example-csv.csv", dec = ",")

input_estimates <- input_estimates %>% 
  mutate(Description = as.character(Description),
         label = as.character(label),
         variable = as.character(variable),
         distribution = as.character(distribution),
         lower = as.numeric(lower),
         median = as.numeric(median),
         upper = as.numeric(upper))

####
#test if input table is loaded:

input_estimates
###

decision_function <- function(x, varnames){
  
  #Risk
  Risk <- chance_event((1-Risk_UI_fail), 1, 0,
                       n = (paying_years + receiving_years))
############  
  #Knowledge generation
  
  Knowledge_generating_cost <- c(vv(var_mean = Knowldge_generating_cost, 
                                    var_CV = var_slight, 
                                    n = paying_years), rep(0,receiving_years))
###########
  #Knowledge spreading
  
  Knowledge_spreading_benefit <- c(rep(0,investment_months), 
                                   vv(var_mean = Knowledge_spreading_benefit, 
                                      var_CV = var_slight, 
                                      n = receiving_years))
  
  
  Knowledge_spreading_benefit <- Knowledge_spreading_benefit * Risk
  
  
  Knowledge_spreading_cost <- c(vv(var_mean = Knowledge_spreading_cost, 
                                   var_CV = var_slight, 
                                   n = paying_years), rep(0, receiving_years))
  
  Knowledge_spreading_cost <- Knowledge_spreading_cost * Risk
  ######
  
  #Health_benefit_medical_costs
  Medical_costs <- c(vv(var_mean = Medical_costs, 
                                             var_CV = var_slight, 
                                             n = receiving_years))
  #Health_benefit_higher_ability_to_work
  
  Ability_to_work_health_benefit <- c(rep(0,investment_months), 
                            vv(var_mean = Ability_to_work_health_benefit, 
                                                        var_CV = var_slight, 
                                                        n = receiving_years))
    
  #cheap population food choices
  
  #theoretically also these....maybe...
  
  ### Explanation of the value varier function (vv())###
  
  #The value varier function (vv()) is used to vary the variables depending
  #on the years of investment and payouts, leading to a variable time series.
  #var_mean is set as the initial variable that differs by var_CV, the coefficient
  #of variation, which is set within the input parameters.
  #n is the number of produced values, meaning the length of time for the
  #initial variable var_mean.
  #This is because the variable should vary around the years
  #of the outcome and not the year of investment.
  
  # Here, the coefficient of variation is set to 1 (var_cv = var_slight).
  
  ###Explanation of the chance_event function###
  
  #The chance_event function (chance_event()) models a risk.
  #It is based on the binomial distribution 0 and 1, and randomly 
  #assigns values to each of the 10.000 model runs.
  #Two parts of the risk are present: One for the investment and one 
  #for the payout. This is considered because "n" is the number
  #of risk simulations and should differ between the three-month phase of 
  #investments and the nine-month phase of payouts.
  #the "change" is the input parameter that defines the percentage of the
  #risk to occur.
  
  
  ####Decision Option calculations####  
  ###Computing the empowerment and status quo decision options###
  #Status quo decision option compared to empowerment decision option#
  
  ##Status quo decision option##
  
  Profit_SQ <- (Ability_to_work_health_benefit-Medical_costs)
  
  
  #Computing the Status Quo NPV (Net present value)#
  
  NPV_no_intervention <- discount(Profit_SQ,
                            discount_rate = discount_rate, calculate_NPV = TRUE) 
  
  ##Empowerment decision option##
  
  PartA <- Knowledge_spreading_benefit + Ability_to_work_health_benefit
    
  
  PartB <- Knowledge_generating_cost
           +  Knowledge_spreading_cost
           + Medical_costs
  
  
  Empowerment_profit <-  (PartA - PartB)
  
  
  
  #Computing the Empowerment NPV (Net present value)#
  
  
  NPV_Empowerment_profit <- discount(Empowerment_profit,
                                     discount_rate = discount_rate,
                                     calculate_NPV = TRUE)
  NPV_decision_profit_with_Empowerment <- NPV_Empowerment_profit - 
  NPV_no_intervention
  
  
  ####Return list####
  
  return(list(NPV_no_intervention=  NPV_no_intervention,
              NPV_Empowerment_profit = NPV_Empowerment_profit, 
    NPV_decision_profit_with_Empowerment = NPV_decision_profit_with_Empowerment,
              Cashflow_decision_empowerment =  Empowerment_profit
              
  )) 
  
}

####Monte Carlo Simulation####
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_estimates),
  model_function = decision_function,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)

#The Monte-Carlo simulation is run with 10,000 repetitions.

####Plot NPV distributions####

#Plot Net Present Value (NPV) distributions
#NPVs show the overall economic output value of a decision option.
#By using the plot_distributions() function, both decisions or
#each separately can be plotted.
#The expected NPV for one decision option
#represents an overlay of the full results of the Monte Carlo simulation.
#The x-axis shows the monetary range farm women can expect for either option.

#Plot empowerment decision option
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                              vars = c("NPV_decision_profit_with_Empowerment"),
                              method = 'smooth_simple_overlay', 
                               base_size = 7)
#plot both
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                vars = c("NPV_decision_profit_with_Empowerment",
                                         "NPV_no_empowerment_branch"),
                                method = 'smooth_simple_overlay', 
                                base_size = 7)

#Plot distributions one by one
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_no_empowerment_branch",
                                    method = 'boxplot_density')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                  vars = "NPV_decision_profit_with_Empowerment",
                                  method = 'boxplot_density')

####Boxplots####

#By using the plot_distributions() function,
#also the decision's boxplots can be plotted.
#Boxplots show the median (central line), 
#the 25th and 75th percentiles (sides of boxes) and any outliers 
#(light circles outside of boxes).

#'boxplot' empowerment decision option

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                               vars = c("NPV_decision_profit_with_Empowerment",
                                        "NPV_no_empowerment_branch"
                                    ),
                                    method = 'boxplot', 
                                    base_size = 7)


####Cashflow analysis####

#Here, the plot_cashflow() function is used with the outputs from the
#mcSimulation() function.
#The cash flow represents the history of the simulated intervention period. 
#The cashflow plot visualizes the time structure of investments and payouts
#During three months of investments into education,
#economic progress, and resource 
#allocation, nine months of paybacks are expected. 
#Of course, this is only a simulation of a possible scenario. 
#Scientists can and should adapt the period to the local situation
#by setting different input estimates.
#Looking at these input estimates, a substantial gain by choosing the 
#decision to empower is estimated. A few months of investment should lead to 
# a larger economic gain. For poor women with less money, it always needs to be 
#considered how long an investment period can be without negative impacts 
#on her and her childrens' lives. 
#So, this example includes, a monthly measurement
#(n_months),  but depending on the local situation,
#a yearly measurement (n_years) could also be useful.


Cashflow <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                          cashflow_var_name = "Cashflow_decision_empowerment",
                          x_axis_name = "Month",
                          y_axis_name = "Cashflow in Dollar",
                          color_25_75 = "green4",
                          color_5_95 = "green1",
                          color_median = "red")

Cashflow

####PLS####

#In this section, a post-hoc analysis is applied to the mcSimulation() outputs,
#using a partial least square regression (PLS)
#with the plsr.mcSimulation() function. 
#This function uses the outputs of the mcSimulation() selecting
#all the input variables from the decision analysis function 
#in the parameter object.
#The PLS is run by defining the outcome variable in the parameter resultName,
#which in this case is "NPV_decision_profit_with_Empowerment".
#If a programmer is unsure how to find this input, 
#names(mcSimulation_results$x) and names(mcSimulation_results$y)
#are useful code chunks to get an overview over the parameters, providing
#a legend of the objects.
#Then,
#names(mcSimulation_results$y)[n] can be used to select the correct resultName.


#The output is the determined Variable 
#Importance in the Projection (VIP) score, shown in a bar graph, 
#and coefficients of a Projection to Latent Structures (PLS) regression model.
#The plots show the variables to which the model is more sensitive. These
#variables have the most exaggerated impact on the decision outcome. 
#Furthermore, they are the
#most correlated with the outcome.
#The VIP shows the importance of the variables with a correlation to the output.
#In the plot, positive and negative values
#are visualized compared to the baseline option.
#Red colors indicate negative values and green colors positive ones.
#A positive value is not 
#only positive compared to the baseline, but also 
#is positively related to the outcome. Same is valid for a negative value:
#It is negative compared to the baseline and negatively related to the outcome.

names(mcSimulation_results$x)
names(mcSimulation_results$y)

#By using the existing input estimates,
#Empowerment workforce payout (Dollar/Month),
#the monetary value achieved by gathering money from the last step
#of the empowerment decision option, the investments into health care and food
#which then lead to more working hours per month compared to doable
#working hours with having less food and health care.
#This is the most important variable in this decision scenario,
#leading to higher payouts than investments. This variable has
#positive importance to the outcome.
#Concluding, 
#the empowerment decision option in this scenario would be very beneficial.
#No negative value is shown in this plot.


#	Pls of	"NPV_decision_profit_with_Own_business_branch_1"

#pls_result_1 <- plsr.mcSimulation(object = mcSimulation_results,
#                                resultName = names(mcSimulation_results$y)[3],
#                                ncomp = 1)

#or use the variable name directly:

pls_result_1 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = "NPV_decision_profit_with_Empowerment",
                                  ncomp = 1)

#Plot PLS
#The colors of the bars represent the positive or negative coefficient 
#of the given input variable with the output variable.

plot_pls(pls_result_1, threshold = 0.8, input_table = input_estimates)


####EVPI####

#This last part of this code is meant to guide further research.
#The Expected Value of Perfect Information analysis (EVPI) visualizes variables.
#Further research could help make better-informed decisions in the
#future. Farm women might benefit from reduced uncertainties of the plotted
#variables since more perfect information benefits
#more informed decision-making.
#Without perfect information of the plotted variables, the farm women
#might suffer from opportunity losses.

#The EVPI is calculated as the amount of money a decision-maker should pay to
#receive better information about highly uncertain variables and 
#still profit.
#If upper and lower input estimate ranges are not broad,
#it is unlikely that an EVPI is
#plotted since enough information is given. With broader ranges,
#the higher the insecurity of the forecast, leading to the plotting of
#a positive EVPI.

mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, 
                   first_out_var = "NPV_Empowerment_profit")
plot_evpi<-plot_evpi(evpi,
                     decision_vars = "NPV_decision_profit_with_Empowerment")
plot_evpi

#Check
names(mcSimulation_results$x)
names(mcSimulation_results$y[1:3])

colnames(mcSimulation_results$y)