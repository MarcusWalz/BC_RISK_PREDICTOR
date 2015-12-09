source("../CAREGail.R", chdir=T)

valid_names = list(year = c(5, 10, 20, 30), rr = c(1, 2, 5, 10), age = c(20,30,40,50))
comp = array(0, dim = c(4,4,4), dimnames = valid_names)

for(age in dimnames(comp)[[3]]) {
  for(year in dimnames(comp)[[1]]) {
    for(rr in dimnames(comp)[[2]]) {
      comp[,rr,age] = gail_relative_risk_to_absolute_risk(
        as.numeric(age)
        , c(5,10,20,30)
        , as.numeric(rr)
        , as.numeric(rr)
        , CAREGail_params$hazards
      )
    }
  }
}

comp  = round(comp*100, digits=2)

reported = 
  cbind( "1"  = c(0.01, 0.05, 0.40, 1.46)
         , "2"  = c(0.02, 0.1, 0.81, 2.9)
         , "5"  = c(0.05, 0.25, 2, 7.09)
         , "10" = c(0.10, 0.51, 3.96, 13.7)
  )

reported_2 = 
  cbind( "1"  = c(0.11, 0.36, 1.42, 3.18)
         , "2"  = c(0.23, 0.71, 2.83, 6.26)
         , "5"  = c(0.56, 1.77, 6.92, 14.9)
         , "10" = c(1.12, 3.50, 13.3, 27.5)
  )

reported_3 = 
  cbind( "1"  = c(0.43, 1.09, 2.89, 4.86)
         , "2"  = c(0.86, 2.17, 5.69, 9.47)
         , "5"  = c(2.14, 5.34, 13.6, 21.9)
         , "10" = c(4.23, 10.4, 25.3, 38.7)
  )

reported_4 = 
  cbind( "1"  = c(0.88, 1.89, 3.96, 5.78)
         , "2"  = c(1.75, 3.74, 7.75, 11.2)
         , "5"  = c(4.23, 9.09, 18.2, 25.4)
         , "10" = c(8.46, 17.3, 32.9, 43.7)
  )

gail_vals = (simplify2array(list(reported, reported_2, reported_3, reported_4)))
print(gail_vals)
dim(gail_vals) = c(4, 4, 4)
dimnames(gail_vals)=valid_names

reported = gail_vals
rownames(reported) = c("5", "10", "20", "30")

print("Reported in Gail89 (percent)")
print(reported)
print("Algorithm reported")
print(comp)
print("output - reported")
print(comp-reported)

print("mean error")
print(mean(abs(comp-reported)))
print("max error")
print(max(abs(comp-reported)))


