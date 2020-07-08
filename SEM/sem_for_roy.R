### for ROY  SEM

rawd<-read.csv("www/FINAL_SWEDEN.csv", header=TRUE)


## need to build a SEM with the following ' big model'
#install.packages("piecewiseSEM")
library(piecewiseSEM)
library(nlme)
#
#
#  The story is mesopredator release (top down)  versus habitat release (bottom up) for 
#  red fox
#
#  as the lynx and wolf are hunted out of existence their impact on the red fox declines as predators
#  as the human populationgrows and industrialisation improves more land is turned arable (seed sown)
#  the lynx and wold dontlike arable land and decline ~ habitat loss
#  the red fox increases  they like arable land (mice and thinsg to eat)
#
#  simple ecology will not work ~ it is not a glm as everything interacts
#  we use sem as in the origina paper to identify relatve contribution of each to each
#
#  so can we asnwer the question are foxes released from predation or rleased form a carrying capacity

#
#  I think that this is an exciting idea for studying as it presses all thebuttons with wolf and lynx 
#
#



names(rawd)


#  [1] "X"             "name"          "year"          "county"        "time"          "pop"           "seed"         
#  [8] "sheep"         "lynx"          "wolf"          "top_predators" "popst"         "red_fox"       "north"        
# [15] "east"          "habitat1"      "habitat2" 


#  habitat1 and 2 are a back ground descriptor of the ecosystems aan RDA of ecoregions in Sweden
#  wolf  killings of wiolf 
#  lynx  killings of lynx
#  red_fox  killings of red fox
#  top_predators is sum of lynx and wolf
#  pop is human population
#  seed is sown seed in each county
#
#


### create the model and run

big_sweden.mod<-psem(
 lme(humans~time+habitat1,random=~1|name,rawd),
 lme(seed~humans+habitat1,random=~1|name, rawd),
 lme(wolf~humans+seed+habitat1,random=~1|name,rawd),
 lme(lynx~humans+seed+habitat1,random=~1|name,rawd),
 lme(red_fox~humans+seed+wolf+lynx+habitat1, random=~1|name, rawd))
 

# this plot is a nightmare it is uncontrollable and comes out as a web thing 
# not a jpg


plot(big_sweden.mod, show="std",node_attrs = list(
  shape = "circle", color = "black",
  fillcolor = "orange"))


######################### the output

summary(big_sweden.mod)

#   |                                                                                                                   
#   |                                                                                                             |   0%
#   |                                                                                                                   
#   |======================                                                                                       |  20%
#   |                                                                                                                   
#   |============================================                                                                 |  40%
#   |                                                                                                                   
#   |=================================================================                                            |  60%
#   |                                                                                                                   
#   |=======================================================================================                      |  80%
#   |                                                                                                                   
#   |=============================================================================================================| 100%
# 
# Structural Equation Model of big_sweden.mod 
# 
# Call:
#   humans ~ time + habitat1
#   seed ~ humans + habitat1
#   wolf ~ humans + seed + habitat1
#   lynx ~ humans + seed + habitat1
#   red_fox ~ humans + seed + wolf + lynx + habitat1
# 
#     AIC      BIC
#  1828.354   1990.189
# 
# ---
# Tests of directed separation:
# 
#         Independ.Claim Test.Type   DF Crit.Value P.Value    
#      seed ~ time + ...      coef 1602    24.9758       0 ***
#      wolf ~ time + ...      coef 1601   -18.0517       0 ***
#      lynx ~ time + ...      coef 1601   -18.0833       0 ***
#   red_fox ~ time + ...      coef 1599     9.3484       0 ***
#      lynx ~ wolf + ...      coef 1601    25.1241       0 ***
# 
# Global goodness-of-fit:
# 
#   Fisher's C = 1768.354 with P-value = 0 and on 10 degrees of freedom
# 
# ---
# Coefficients:
# 
#   Response Predictor Estimate Std.Error   DF Crit.Value P.Value Std.Estimate    
#     humans      time   0.1603    0.0051 1603    31.4299  0.0000       0.2297 ***
#     humans  habitat1   1.6045    0.9472   21     1.6940  0.1050       0.3213    
#       seed    humans 490.7419   14.4009 1603    34.0771  0.0000       0.6673 ***
#       seed  habitat1  51.7684  457.3391   21     0.1132  0.9110       0.0141    
#       wolf    humans  -0.0053    0.0040 1602    -1.3110  0.1900      -0.0945    
#       wolf      seed  -0.0001    0.0000 1602   -17.2491  0.0000      -1.2026 ***
#       wolf  habitat1   0.1250    0.0696   21     1.7951  0.0870       0.4475    
#       lynx    humans  -0.0122    0.0026 1602    -4.6455  0.0000      -0.3354 ***
#       lynx      seed   0.0000    0.0000 1602   -10.1475  0.0000      -0.7088 ***
#       lynx  habitat1   0.0617    0.0334   21     1.8442  0.0793       0.3389    
#    red_fox    humans   0.2935    0.1369 1600     2.1432  0.0323       0.1082   *
#    red_fox      seed   0.0026    0.0002 1600    13.5238  0.0000       0.7086 ***
#    red_fox      wolf  -2.9165    0.9977 1600    -2.9233  0.0035      -0.0601  **
#    red_fox      lynx  -5.9275    1.5249 1600    -3.8873  0.0001      -0.0796 ***
#    red_fox  habitat1   4.8689    1.3407   21     3.6317  0.0016       0.3595  **
# 
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
# 
# ---
# Individual R-squared:
# 
#   Response method Marginal Conditional
#     humans   none     0.15        0.92
#       seed   none     0.51        0.90
#       wolf   none     0.42        0.81
#       lynx   none     0.38        0.70
#    red_fox   none     0.63        0.77
# > 
# > plot(big_sweden.mod, show="std",node_attrs = list(
# +   shape = "circle", color = "black",
# +   fillcolor = "orange"))


####### now need to remove non significant pathways
## these are the habitats for all except the fox
# here I woudl expect the students to identify the NS vars and remove fro the equations

# modified model run


big_sweden.mod2<-psem(
 lme(humans~time,random=~1|name,rawd),
 lme(seed~humans,random=~1|name, rawd),
 lme(wolf~humans+seed,random=~1|name,rawd),
 lme(lynx~humans+seed,random=~1|name,rawd),
 lme(red_fox~humans+seed+wolf+lynx+habitat1, random=~1|name, rawd))
 
summary(big_sweden.mod2)
plot(big_sweden.mod2, show="std",node_attrs = list(
   shape = "circle", color = "black",
   fillcolor = "orange"))


# -
# Coefficients:
# 
#   Response Predictor Estimate Std.Error   DF Crit.Value P.Value Std.Estimate    
#     humans      time   0.1603    0.0051 1603    31.4304  0.0000       0.2297 ***
#       seed    humans 490.9280   14.3749 1603    34.1516  0.0000       0.6676 ***
#       wolf    humans  -0.0050    0.0040 1602    -1.2407  0.2149      -0.0894    
#       wolf      seed  -0.0001    0.0000 1602   -17.2739  0.0000      -1.2057 ***
#       lynx    humans  -0.0120    0.0026 1602    -4.5690  0.0000      -0.3299 ***
#       lynx      seed   0.0000    0.0000 1602   -10.1658  0.0000      -0.7117 ***
#    red_fox    humans   0.2935    0.1369 1600     2.1432  0.0323       0.1082   *
#    red_fox      seed   0.0026    0.0002 1600    13.5238  0.0000       0.7086 ***
#    red_fox      wolf  -2.9165    0.9977 1600    -2.9233  0.0035      -0.0601  **
#    red_fox      lynx  -5.9275    1.5249 1600    -3.8873  0.0001      -0.0796 ***
#    red_fox  habitat1   4.8689    1.3407   21     3.6317  0.0016       0.3595  **
# 
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
# 
# ---
# Individual R-squared:
# 
#   Response method Marginal Conditional
#     humans   none     0.05        0.92
#       seed   none     0.51        0.90
#       wolf   none     0.42        0.82
#       lynx   none     0.39        0.72
#    red_fox   none     0.63        0.77
# > 
# 
# some more become non-sig



big_sweden.mod3<-psem(
 lme(humans~time,random=~1|name,rawd),
 lme(seed~humans,random=~1|name, rawd),
 lme(wolf~seed,random=~1|name,rawd),
 lme(lynx~humans+seed,random=~1|name,rawd),
 lme(red_fox~humans+seed+wolf+lynx+habitat1, random=~1|name, rawd))
 
summary(big_sweden.mod3)
# -
# Coefficients:
# 
#   Response Predictor Estimate Std.Error   DF Crit.Value P.Value Std.Estimate    
#     humans      time   0.1603    0.0051 1603    31.4304  0.0000       0.2297 ***
#       seed    humans 490.9280   14.3749 1603    34.1516  0.0000       0.6676 ***
#       wolf      seed  -0.0001    0.0000 1603   -23.7399  0.0000      -1.2606 ***
#       lynx    humans  -0.0120    0.0026 1602    -4.5690  0.0000      -0.3299 ***
#       lynx      seed   0.0000    0.0000 1602   -10.1658  0.0000      -0.7117 ***
#    red_fox    humans   0.2935    0.1369 1600     2.1432  0.0323       0.1082   *
#    red_fox      seed   0.0026    0.0002 1600    13.5238  0.0000       0.7086 ***
#    red_fox      wolf  -2.9165    0.9977 1600    -2.9233  0.0035      -0.0601  **
#    red_fox      lynx  -5.9275    1.5249 1600    -3.8873  0.0001      -0.0796 ***
#    red_fox  habitat1   4.8689    1.3407   21     3.6317  0.0016       0.3595  **
# 
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05
# 
# ---
# Individual R-squared:
# 
#   Response method Marginal Conditional
#     humans   none     0.05        0.92
#       seed   none     0.51        0.90
#       wolf   none     0.42        0.82
#       lynx   none     0.39        0.72
#    red_fox   none     0.63        0.77

plot(big_sweden.mod3)
plot(big_sweden.mod3, show="std",node_attrs = list(
  shape = "circle", color = "black",
  fillcolor = "orange"))


  # thislast picture is decipherable ~ but I am not sure how one would achieve
# this for themore complicated earlier runs
# i) designing you equations ~ perhaps reactive function
# ii) getting the picture AND the means to modify it (ie the summary list) 


