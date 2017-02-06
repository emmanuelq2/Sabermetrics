
OPS2013 = read.csv("OPS2013.csv")
qplot(OPS, data=OPS2013, geom="histogram", binwidth=0.05, col="red")
