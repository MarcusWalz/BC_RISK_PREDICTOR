# Hide output of Gail89 for now.
source("GailAlgorithm.R")


cutpoints = c(  20,    25,   30,    35,    40,     45,   50,    55,     60,     65,     70,     75,     80,      85)
h1_star   = c( 2.0,   8.2, 22.7,  49.3,  87.1, 136.7, 176.2, 212.4,  226.2,  267.4,  286.8,  298.3,  289.5,   265.3) / 10^5
h2        = c(74.4, 101.7,145.9, 215.9, 315.1, 448.8, 632.3, 963.0, 1471.8, 2116.3, 3266.0, 4564.1, 6835.2, 13271.3) / 10^5
width     = rep(5,14)
F = 1 #       = ifelse(cutpoints < 50, 0.7295, 0.7440)

CAREGail_params = list( hazards = data.frame(cutpoints, h1_star, h2, F, width)
               , cof     = c(0, 0.0815, 0.185, 0.0014, 0.424, 0.0264, -0.114, 0.0485)
               )

print(CAREGail_params)

CAREGail = function(avatars, time) gail_algorithms(avatars, time, CAREGail_params)

# this calculates 20 year absolute risk for a 30yo with a relative risk of 10
# according to the CAREGail algorithm.
# gail_relative_risk_to_absolute_risk(30, 20, 10, 10, CAREGail_params$hazards)
