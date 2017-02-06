##########################################
#                                        #
# SABR101x: Sabermetrics 101 on the      #
# edX.org platform for Boston University #
# Andy Andres, Ethan Bein, & Eric Smiley #
#                                        #
##########################################


fname = file.choose()
Teams = read.csv(fname)
summary(Teams)

# When exclusively using just one data frame, this command can 
# save you time and make formulas look better
attach(Teams)

# Calculate Runs Estimators
TB = (H + X2B + 2*X3B + 3*HR)
SLG = TB / AB
OBP = (H + BB + HBP) / (AB + BB + HBP + SF)
OPS = SLG + OBP
BRA = SLG * OBP
ADJ_OPS = OBP * 1.8 + SLG
RCB = (TB*(H + BB)) / (AB + BB)

XRR = (.5*(H-HR-X3B-X2B))+(.72*X2B)+(1.04*X3B)+(1.44*HR)+.33*(HBP+BB)+.18*SB-.32*CS-.098*(AB-H)

LWTs = (.46*(H-HR-X3B-X2B))+(.8*X2B)+(1.02*X3B)+(1.4*HR)+.33*(HBP+BB)+.3*SB-.6*CS-.25*(AB-H)+701.2
  
Teams["RCB"] = RCB
Teams["XRR"] = XRR
Teams["LWT"] = LWTs
  
# The columns we want to plot in scatterplot matrix, 
# making a smaller data frame
Teams_Runs_est = Teams[c("R", "RCB", "XRR", "LWT")]
View(Teams_Runs_est)


# Create a scatterplot matrix using ggplot2
install.packages("ggplot2")
require(ggplot2)

pairs(Teams_Runs_est)


RvRCB = lm(R~RCB)
#summary(RvRCB)
plot(RCB,R)
abline(RvRCB)
summary(RvRCB)$r.squared
cor(R,RCB)

RvXRR = lm(R~XRR)
#summary(RvXRR)
plot(XRR,R)
abline(RvXRR)
summary(RvXRR)$r.squared
cor(R,XRR)


# Get rid of attach data frame from above
detach(Teams)

RvLWT = lm(Teams$R~Teams$LWT)
#summary(RvLWT)
plot(Teams$LWT,Teams$R)
abline(RvLWT)
summary(RvLWT)$r.squared
cor(Teams$R,Teams$LWT)






# BaseRuns from David Smyth
#BRA = H + BB - HR
#BRB = (1.4*TB - .6*H - 3*HR + .1*BB)*1.02
#BRC = AB - H
#BRD = HR
#BRuns_Basic = BRA*BRB/(BRB+BRC) + BRD

#Teams["BRB"] = BRuns_Basic

#RvBRB = lm(R~BRB)
#plot(R,BRB)
#abline(RvBRB)
#summary(RvBRB)$r.squared
#cor(R,BRB)

