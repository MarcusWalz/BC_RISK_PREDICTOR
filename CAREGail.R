# Hide output of Gail89 for now.
source("GailAlgorithm.R")

# h1_star and h2 copied from output of CAREGail source code found @
# http://dceg.cancer.gov/tools/risk-assessment/care


h2=c(0.0007435356, 0.0010169822, 0.0014593653,
0.0021593303, 0.0031507691, 0.0044877870, 0.0063228053,
0.0096303749, 0.0147181842, 0.0211630353, 0.0326603535,
0.0456408661, 0.0683518455, 0.1327126155)

h1_star = c(0.0000269606, 0.0001129479, 0.0003109446, 0.0006763867,
0.0011944398, 0.0018739409, 0.0024150359, 0.0029111196,
0.0031012723, 0.0036655962, 0.0039313153, 0.0040895125,
0.0039679252, 0.0036371174)

cutpoints = c(  20,    25,   30,    35,    40,     45,   50,    55,     60,     65,     70,     75,     80,      85)
# h1_star   = c( 2.0,   8.2, 22.7,  49.3,  87.1, 136.7, 176.2, 212.4,  226.2,  267.4,  286.8,  298.3,  289.5,   265.3) / 10^5
# h2        = c(74.4, 101.7,145.9, 215.9, 315.1, 448.8, 632.3, 963.0, 1471.8, 2116.3, 3266.0, 4564.1, 6835.2, 13271.3) / 10^5
width     = rep(5,14)
F         = ifelse(cutpoints < 50, 0.7294988, 0.7439137)

CAREGail_params = list( hazards = data.frame(cutpoints, h1_star, h2, F, width)
               , cof     = c(0, 0.0815, 0.185, 0.0014, 0.424, 0.0264, -0.114, 0.0485)
               )

# print(CAREGail_params)

CAREGail = function(population, time) {
  # CAREGail has two categories for AGEMEN, not 3 like Gail89, hack to fix:
  population$AGE_AT_MENARCHE = ifelse(population$AGE_AT_MENARCHE <= 13,  13,14)  
  gail_algorithm(population, time, CAREGail_params)
}

register_algorithm("CAREGail", CAREGail, T, T, gail_fields)

test=data.frame(AGE=30, AGE_AT_MENARCHE=14, FIRST_DEGREE_RELATIVES=1, BIOPSY=1, PARITY=1, AGE_AT_FIRST_BIRTH=19)
CAREGail(test, c(5,30))
# this calculates 20 year absolute risk for a 30yo with a relative risk of 10

# according to the CAREGail algorithm.
# gail_relative_risk_to_absolute_risk(30, 20, 10, 10, CAREGail_params$hazards)
