source("../Gail89.R", chdir=T)

valid_names =list(year=c(10,20,30), rr=c(1,2,5,10,20,30), age=c(20,30,40,50))
comp = array(0, dim=c(3,6,4), dimnames = valid_names)

for(age in dimnames(comp)[[3]]) {
  for(year in dimnames(comp)[[1]]) {
      for(rr in dimnames(comp)[[2]]) {
        comp[,rr,age] = gail_relative_risk_to_absolute_risk(
          as.numeric(age)
        , c(10,20,30)
        , as.numeric(rr)
        , as.numeric(rr)
        , gail89$hazards
        )
    }
  }
}

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
