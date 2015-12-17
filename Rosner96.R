#####################################################################################
# Rosner96_LMP1.R is the coded Rosner breast cancer risk algorithm published in 1996.
#                
# The LPM version 0 was the first effort to code the algorithm by Anji Tang
# Specific algorithmic coding practical details include:   
#		Inputs:
#		AGE					        # Woman's Current Age (Integer)
#		AGE_OF_FIRST_BIRTH 	# Woman's Age at first birth (Integer)
#		AGE_AT_MENARCHE 	  # Woman's Age which she reached menarche (Integer)
#		AGE_AT_MENOPUASE 	  # Woman's Age which she reached menopuase,
                        # or 0 if menopause has not been reached. (integer)
#		PARITY				      # Woman's Parity (1 for yes and 0 for no)
#		MENOPAUSE_STATUS	  # Woman's menopause status (1 for post-menopausal and
#                       # 0 for pre-menopausal)
#
# This model will estimate a woman's absolute risk of a women developing breast 
# cancer within 5 years.
#Citation:		# Rosner B, Colditz GA. Nurses' health study: log-incidence mathematical model of breast cancer incidence. J Natl Cancer Inst. #				1996;88(6):359-64.
#
#####################################################################################
#Reference: Code is based on rosner2.R, found on the LPM Github

source("AlgorithmUtil.R")


Rosner96 <- function(population, years = 5){
    avatars = population

	  alpha  = -9.687
	  beta_0 =  0.048
	  beta_1 =  0.081
	  beta_2 =  0.050
	  beta_3 =  0.013
	  beta_4 = -0.0036
	  beta_5 = -0.00020

    # t is current age 
    t = sapply(avatars$AGE, function(t) t:(t+max(years)-1))

    t_0    = avatars$AGE_AT_MENARCHE
    m      = avatars$MENOPAUSE_STATUS
    t_m    = avatars$AGE_AT_MENOPAUSE
    t_star = pmin( t, t_m ) # pairwise min on vecotrs
  
    # birth index = b
    # TODO support multiple births
    b_1    = avatars$PARITY # of 1st birth
    t_1    = avatars$AGE_AT_FIRST_BIRTH
    b      = ( t_star - t_1 ) * b_1
 
 
    # Absolute Risk of Cancer by Year
    ar_risk_by_year=exp(
         alpha 
       + beta_0 * t_0
       + beta_1 * ( t_star - t_0)
       + beta_2 * ( t - t_m ) * m
       + beta_3 * ( t_1 - t_0) * b_1
       + beta_4 * b
       + beta_5 * b * ( t - t_m) * m
    )

    # combine absolute risk by year

    out = as.matrix(t(apply(ar_risk_by_year,2,cumsum))[,years,drop=FALSE])
    colnames(out) = paste("AR", years, sep="_")

    out
}

rosner_fields = c("AGE", "MENOPAUSE_STATUS", "AGE_AT_MENOPAUSE", "PARITY", "AGE_AT_FIRST_BIRTH")
register_algorithm("Rosner96", Rosner96, T, F, rosner_fields)

# Calculate the n year absolute risk using bayes rule
cum_ar = function(ar) {
  if(!length(ar)) { return(0) }
  ar[1] + (1 - ar[1]) * cum_ar(ar[-1])
}
  
# TODO Move to test dir
test_data = data.frame(
    AGE = c(45,60,70,110)
  , AGE_AT_FIRST_BIRTH = 35
  , AGE_AT_MENARCHE = 10
  , AGE_AT_MENOPAUSE = 40 
  , PARITY = T
  , MENOPAUSE_STATUS = T
)

# print(cbind(test_data,Rosner96(test_data, years=c(5,10))))

