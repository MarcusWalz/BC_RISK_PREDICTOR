#####################################################################################
# Gail89_LMP1.R is the coded Gail breast cancer risk algorithm published in 1989.
#                
# The LPM version 0 was the first effort to code the algorithm 
# Specific: algorithmic coding practical details include:   
#		Inputs:
#		RACE				# Woman's Race/Ethnicity (Controlled Dictionary, White, Black or African American, Hispanic, Asian)*
#		AGE					# Woman's Current Age (numeric)
#		FIRST_DEGREE_RELATIVES		# Woman's Family History of Breast Cancer in 1st Degree Relatives (integer)
#		BIOPSY				# Woman's History of Previous Breast Biopsies ( 0, 1 , >=2 )
#		AGE_AT_MENARCHE 	#Woman's Age which she reached menarche (numeric)
#		AGE_AT_BIRTHS		#Woman's Age at which she had birth (numeric)
#This model will estimate a woman's absolute risk of developing breast cancer within 5 years
#                               
#Change published algorithm to include: 
#	replaced verbose if/else statements with switches
#	changed variable names to meet LPM naming convention
#	changed input to data.frame rather than file path, for use in library
#	changed code to calculate risk for whole data.frame, rather than a single patient	
# Added a 'file-select' feature
#See #debug comments in code.
#Citation:		 Gail MH, Brinton LA, Byar DP, et al. Projecting individualized probabilities of developing breast cancer for white females who are being examined annually. J Natl Cancer Inst. 1989;81(24):1879-86.
#####################################################################################

source("GailAlgorithm.R")

# Exact Numbers used in the Gail89 paper

# Mortality rates taken from National Center For Health Stat. Vital Statistics in the United States
# Mortality 1979, vol 2A
cutpoints          = c(20  ,   25,   30,    35,    40,    45,    50,    55,     60,     65,     70,     75) #-80
width              = rep(5,12)
# Death Rate
death_rate_1979    = c(58.8, 60.8, 73.1, 106.7, 175.5, 286.6, 459.7, 701.9, 1105.4, 1598.9, 2527.2, 4451.8) / 10^5
# Breast Cancer only Death Rate
death_rate_1979_bc = c( 0.2,  1.3,  5.1,  11.6,  23.6,  37.4,  58.8,  74.7,   87.7,   98.3,  107.4,  124.9) / 10^5
                            
bcddp              = c( 2.7, 16.8, 60.3, 114.6, 203.7, 280.8, 320.9, 293.8,  369.4,  356.1,  307.8,  301.3) / 10^5 
# SEER cancer rate (not used)
seer               = c( 1.3,  8.0, 28.8,  54.7, 109.2, 173.3, 198.8, 221.5,  278.3,  315.3,  331.3,  364.0) / 10^5

F                  = append(rep(0.5229, 6), rep(0.5264, 6))

gail89_hazards = data.frame(cutpoints, h1_star=bcddp, h2=(death_rate_1979 - death_rate_1979_bc), F, width)

gail89 = 
  list( hazards = gail89_hazards
      , cof     = c(-0.74948, 0.09401, 0.52926, 0.21863, 0.95830, 0.01081, -0.28804, -0.19081)
      )

Gail89 = function(population, years) { gail_algorithm(population, years, gail89) } 

register_algorithm("Gail89", Gail89, T, T, gail_fields)
