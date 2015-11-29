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

# options(warn=2)

cutpts = c(20,25,30,35,40,45,50,55,60,65,70,75,80,85,90)

h1_2 = c(1.22, 7.41, 22.97, 56.49, 116.45, 195.25, 261.54, 302.79
        ,367.57, 420.29, 473.08, 494.25, 479.76, 401.06) / 10^5
h1 = h1_2


h2_2 = c(44.12,52.54,67.46,90.62,125.34,195.7,329.84,546.22,910.35
        ,1418.54,2259.35,3611.46,6136.26,14206.63) / 10^5 
h2 = h2_2


# Mortality rates taken from National Center For Health Stat. Vital Statistics in the United States
# Mortality 1979, vol 2A


cutpoints          = c(20  ,   25,   30,    35,    40,    45,    50,    55,     60,     65,     70,     75) #-80
width              = rep(5,12)
death_rate_1979    = c(58.8, 60.8, 73.1, 106.7, 175.5, 286.6, 459.7, 701.9, 1105.4, 1598.9, 2527.2, 4451.8) / 10^5
death_rate_1979_bc = c( 0.2,  1.3,  5.1,  11.6,  23.6,  37.4,  58.8,  74.7,   87.7,   98.3,  107.4,  124.9) / 10^5
                            
bcddp              = c( 2.7, 16.8, 60.3, 114.6, 203.7, 280.8, 320.9, 293.8,  369.4,  356.1,  307.8,  301.3) / 10^5 
seer               = c( 1.3,  8.0, 28.8,  54.7, 109.2, 173.3, 198.8, 221.5,  278.3,  315.3,  331.3,  364.0) / 10^5

F                  = append(rep(0.5229, 6), rep(0.5264, 6))

gail89 = data.frame(cutpoints, h1_star=bcddp, h2=(death_rate_1979 - death_rate_1979_bc), F, width)

Gail89 = 
  list( hazards = gail89
       , cof     = c(-0.74948, 0.09401, 0.52926, 0.21863, 0.95830, 0.01081, -0.28804, -0.19081)
       )

gail_algorithm = function(avatars, years=c(5,10,15), fit = Gail89, aux_rr=NULL) {
  if(!is.null(aux_rr)) {
    if(is.function(aux_rr)) {
      aux_rr = aux_rr(avatars)
    } else if(length(aux_rr) == nrow(avatars)) {
      aux_rr = aux_rr
    } else {
      stop("aux_rr invalid")
    }
  } else {
    aux_rr = 1;
  }
  # calculate the relative risks 
  relative_risks = gail_rr(avatars, fit) * aux_rr 

  absolute_risks=t(apply(cbind(avatars, relative_risks), 1,
                         function(avatar) {
                           avatar = data.frame(t(avatar))
                           my_fit = ifelse(is.function(fit), fit(avatar), fit)
      my_fit = fit
      relative_risk_to_absolute_risk(
        avatar$AGE
      , years
      , avatar$RR_LT_50
      , avatar$RR_GTE_50
      , my_fit$hazards) 
    }
  ))
  colnames(absolute_risks) = paste(years, "year AR")

  cbind(relative_risks, absolute_risks)

}

print(gail89)

# Data is published to fit several races.
# This function fetches the most specific based on the avatar.
get_constants_by_demographic = function(avatar, original_gail=T) {

  if(original_gail) {
    # deaths for white females only!
    # bcddp and seer are taken from Gail89 

  }
  # hazard rates, h1 and h2 should use the most specific
  # data available. For small n reasons, we'll only use
  # the paramaters fit to Black and White data, and use
  # the "general population" fit paramaters for every 
  # other ethnicity.

  # logistic regression cofficients used in gail 99 and 07

  if( avatar$RACE == "Black" ) {
    cofs = c()
  } else if( avatar$RACE == "White" ) {
    # q.v. rlan2[*,0]
    h1 = c(1,7.6,26.6,66.1,126.5,186.6,221.1,272.1,334.8,392.3,417.8,443.9,442.1,410.9) / 10^5
    # q.v. rmu2[*,0] from BCPT source code# 
    h2 = c(49.3, 53.1, 62.5, 82.5, 130.7, 218.1, 365.5, 585.2, 943.9, 1502.8, 2383.9, 3883.2, 6682.8, 14490.8) / 10^5
  } else {
    cons = c()
  }
}

gail_avatars = function(avatars) {
  # Convert to gail format. 

  # required fields 
  req_fields =c("AGE", "AGE_AT_MENARCHE", "BIOPSY","PARITY", "AGE_AT_BIRTHS", "FIRST_DEGREE_RELATIVES")

  has_fields = req_fields %in% colnames(avatars)
  if(!all(has_fields)) {
    stop(paste("Can not use Gail, missing fields:", req_fields[!has_fields]))
  }


  AGECAT = (avatars$AGE >= 50) + 0
  AGEMEN = ifelse(avatars$AGE_AT_MENARCHE >= 14, 0,
                  ifelse(avatars$AGE_AT_MENARCHE < 12, 2, 1) 
                  )
  NBIOPS = ifelse(avatars$BIOPSY == 0, 0,
                  ifelse(avatars$BIOPSY == 1, 1, 2))
  AGEFLB = ifelse(!avatars$PARITY, 2, 
                  ifelse(avatars$AGE_AT_BIRTHS < 20, 0,
                         ifelse(avatars$AGE_AT_BIRTHS >= 20 &
                                avatars$AGE_AT_BIRTHS <  25, 1,
                                ifelse(avatars$AGE_AT_BIRTHS >= 25 &
                                       avatars$AGE_AT_BIRTHS <  30, 2, 3
                                       )))) 

  NUMREL = ifelse(avatars$FIRST_DEGREE_RELATIVES == 0, 0,
                  ifelse(avatars$FIRST_DEGREE_RELATIVES == 1, 1, 2))

  # which are multiplied by these vars (AGE = 1)
  data.frame(AGECAT, AGEMEN, NBIOPS, AGEFLB, NUMREL)
}

# Takes avatars and a fit (or fit finding function) and returns a three column matrix with:
#  RR: avatars relative risk at current age
#  RR_LT_50: avatars relative relative risk for ages younger than 50
#  RR_GTE_50: avatars relative relative risk for ages 50 or older
gail_rr = function(avatars, fit = Gail89){
  # if avatar's are not in Gail format, convert it
  if(!all(c("AGECAT", "AGEMEN", "NBIOPS","AGEFLB", "NUMREL") %in% colnames(avatars))) {
    avatars = gail_avatars(avatars)
  }

  # if we have fit finder, run avatars one-by-one
  if(is.function(fit)) {
    return(apply(avatars, 1, function(avatar) {
      gail_rr(avatar, fit(avatar))
    }))
  }

  # AGECAT = 0 

  var = rbind(
           0 # intercept
         , avatars$AGEMEN
         , avatars$NBIOPS
         , avatars$AGEFLB
         , avatars$NUMREL
         , 1 # AGECAT
         , avatars$NBIOPS # * AGECAT
         , avatars$AGEFLB * avatars$NUMREL
         )

  RR_LT_50  = exp(apply((fit$cof * c(0,1,1,1,1,0,0,1) * var), 2, sum))
  # AGECAT = 1 
  RR_GTE_50 = exp(apply((fit$cof * c(0,1,1,1,1,1,1,1) * var), 2, sum))
  print(RR_GTE_50)

  RR = ifelse(avatars$AGECAT, RR_GTE_50, RR_LT_50)

  # Project RR risk a population into the future
  time = 15 


  cbind(RR, RR_LT_50, RR_GTE_50)
}


# Modifies hazard matrix for cutpoints at intervals corresponding to target
# age. This way we can calculate hazards for patients whose age is not a 
# multiple of 5.
hazard_splice = function(hazards, years) {
  cuts = hazards$cutpoints
  total_width = cuts[length(cuts)] + hazards$width[nrow(hazards)]

  cuts = append(cuts, total_width)

  if(any(years > total_width)) {
    warning("Projecting risk too far into future.")
    years = years[years <= total_width]
  }

  new_cuts = unique(sort(append(hazards$cutpoints, years)))
  trans = cut(new_cuts, breaks=cuts, right=FALSE, include.lowest=FALSE)

  new_h = hazards[as.numeric(trans),]
  new_h$cutpoints = new_cuts
  new_widths = append(new_cuts[-1], total_width) - new_cuts
  new_h$width = new_widths

  new_h
}

relative_risk_to_absolute_risk = function( age, years, rr_lt_50, rr_gte_50, hazards) {
  hazards = hazard_splice(hazards, append(age,age+years))
#  print(hazards)
  h1 = hazards$h1_star * hazards$F
  h2 = hazards$h2
  cutpts = hazards$cutpoints
  widths = hazards$width
  rr = ifelse(cutpts < 50, rr_lt_50, rr_gte_50)

  cuts = hazards$cutpoints
  total_width = cuts[length(cuts)] + hazards$width[nrow(hazards)]
  cuts = unique(append(cuts, total_width))

  cut_f = function(x) {
    cut(x, breaks=cuts, right=FALSE, include.lowest=FALSE)
  }

  age_index = as.numeric(cut_f(age))

  # tbl = data.frame(levels(age_index), rr, h1, h2, widths)
  # print(tbl)


  S1_t = cumprod(exp(-1 * h1 * rr * widths))
  S2_t = cumprod(exp(-1 * h2      * widths))


  S1_t=append(1,S1_t)[1:length(h1)]
  S2_t=append(1,S2_t)[1:length(h1)]

  S1_a = S1_t[age_index]
  S2_a = S2_t[age_index]

  five_year_risks = cumsum(
       (1:(length(h1)) >= (age_index))
     * (h1 * rr) / (h1 * rr + h2)
     * (S1_t / S1_a)
     * (S2_t / S2_a)
     * (1 - exp( -1 * widths * (h1 * rr + h2)))
     )

#  print(five_year_risks)
  # returns list of 5-year risks

  five_year_risks[as.numeric(cut_f(age+years-1))]
}




test = data.frame( cbind(AGE    = c(54,60,30)
                 , BIOPSY = c(1,3,0)
                 , FIRST_DEGREE_RELATIVES = c(1,3,0)
                 , AGE_AT_MENARCHE = c(15,10,15) 
                 , AGE_AT_BIRTHS	 = c(0,30,20)
                 , PARITY = c(0,1,1)
                 ))
print(test)
print(gail_algorithm(test))
warnings()

valid_names =list(year=c(10,20,30), rr=c(1,2,5,10,20,30), age=c(20,30,40,50))
comp = array(0, dim=c(3,6,4), dimnames = valid_names)

for(age in dimnames(comp)[[3]]) {
  for(year in dimnames(comp)[[1]]) {
      for(rr in dimnames(comp)[[2]]) {
  comp[,rr,age] = relative_risk_to_absolute_risk(as.numeric(age), c(10,20,30), as.numeric(rr), as.numeric(rr), gail89)
    }
  }
}
warnings()

# print(comp)
comp  = round(comp*100, digits=1)

reported = 
  cbind( "1"  = c(0,0.5,1.7)
       , "2"  = c(0.1,1.0,3.4)
       , "5"  = c(0.2,2.5,8.3)
       , "10" = c(0.5,4.9, 15.9)
       , "20" = c(1.0,9.5,29.3)
       , "30" = c(1.4,14.0,40.5)
       )
reported_2 = 
  cbind( "1"  = c(0.5,1.7,3.2)
       , "2"  = c(0.9,3.3,6.3)
       , "5"  = c(2.3,8.1,14.9)
       , "10" = c(4.4,15.6,27.6)
       , "20" = c(8.7,28.8,47.4)
       , "30" = c(12.8,39.9,61.7)
       )
reported_3 = 
  cbind( "1"  = c(1.2,2.8,4.4)
       , "2"  = c(2.5,5.5,8.6)
       , "5"  = c(6.1,13.1,20.0)
       , "10" = c(11.8,24.4,35.9)
       , "20" = c(22.2,42.7,58.5)
       , "30" = c(31.3,56.6,72.8)
       )

reported_4 = 
  cbind( "1"  = c(1.6, 3.2, 4.4)
       , "2"  = c(3.1,6.4,8.5)
       , "5"  = c(7.6,15.1,19.9)
       , "10" = c(14.6,27.9,35.5)
       , "20" = c(27.1,47.8,57.8)
       , "30" = c(37.7,61.9,71.7)
       )

gail_vals = (simplify2array(list(reported, reported_2, reported_3, reported_4)))
print(gail_vals)
dim(gail_vals) = c(3,6,4)
dimnames(gail_vals)=valid_names
reported = gail_vals
rownames(reported_2) = c("10","20","30")
rownames(reported) = c("10","20","30")

print("Reported in Gail89 (percent)")
print(reported)
print("Algorithm reported")
print(comp)
print("output - reported")
print(comp-reported)

print("mean error")
print(mean(comp-reported))
print("max error")
print(max(comp-reported))
