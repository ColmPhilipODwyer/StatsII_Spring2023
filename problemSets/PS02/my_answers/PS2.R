#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))

head(climateSupport)
str(climateSupport)

# First, I recode "choice" as a binary outcome variable:
climateSupport$choice_binary <- ifelse(climateSupport$choice == "Supported", 1, 0)

# I fit a generalised linear model with 'choice' as the outcome variable, and
# 'countries' and 'sanctions' as the predictor variable.

likelihood_model <-glm(choice_binary ~ countries + sanctions,
                       data = climateSupport)

summary(likelihood_model)


#Conducting the GLobal Null Hypothesis test
#First, I create the null model which I will test my model against:
null_model <- glm(choice_binary ~ 1, data = climateSupport)



null_test <- anova(null_model, likelihood_model, test = "Chisq")

print(null_test)
# p = 2.2e-16

# Calculating likelihood of success for intercept
exp(0.498607) / (1 + exp(0.498607))


# Q2 (a) calculating effect of increasing sanctions on odds

# 5% sanctions
0.498607 -0.067615 -0.002386
[1] 0.428606 # log odds

# 15% sanctions
0.498607 -0.044187 -0.002386
[1] 0.452034 #log odds

difference_in_odds <- exp(0.428606) - exp(0.452034)
difference_in_odds
exp(0.428606)

#Q2 (b) calculating log odds if 80 countries participating, no sanctions
0.498607 + 0.112700
# [1] 0.611307
probability <- 1/(1 + exp(1)^(-0.611307))
probability

#Q2 (c) conducting likelihood ratio test
# firstly, I create a model with the interaction effect included:
likelihood_model_interaction <- glm(choice_binary ~ countries * sanctions,
                                    data = climateSupport)

# Next I calculate the deviance of both models:
deviance_original_model <- deviance(likelihood_model)
deviance_interaction_model <- deviance(likelihood_model_interaction)

# I calculate the likelihood ratio statistic:
lr_stat <- abs(deviance_interaction_model - deviance_original_model)
lr_stat
# [1] 1.598523

# I calculate the difference in the degrees of freedom:
df_diff <- df.residual(likelihood_model) - df.residual(likelihood_model_interaction)

# Finally, I calculate the p-value:
p_value <- pchisq(test_statistic, df_diff, lower.tail = FALSE)
p_value
# [1] 0.9526835
#Because p>0.05, I fail to reject the null hypothesis, that the model without
# including interaction is sufficient, and that it is unnecessary to include
# an interaction effect.