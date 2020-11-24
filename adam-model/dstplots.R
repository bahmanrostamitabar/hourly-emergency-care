devtools::install_github("config-i1/smooth", upgrade="never", dependencies=FALSE, ref="Profile-with-DST")
library(smooth)

#### Examples with shifts via DST ####

x <- zoo(h2$n_attendance, order.by=h2$arrival_1h)
testDST1 <- adam(x, "ANA", lags=c(1,24,24*7), silent=F, initial="back", persistence=rep(0,3))
testDST2 <- adam(as.vector(x), "ANA", lags=c(1,24,24*7), silent=F, initial="back", persistence=rep(0,3))

mean((actuals(testDST1)-fitted(testDST1))^2)
mean((actuals(testDST2)-fitted(testDST2))^2)

pdf("/home/config/Dropbox/R/dstPlots.pdf",width=10,height=7)
# End of DST in 2014
plot(actuals(testDST1)[1:(24*7)+4994-24*3],type="b")
lines(fitted(testDST1)[1:(24*7)+4994-24*3],col="red")
points(fitted(testDST1)[1:(24*7)+4994-24*3],col="red")
abline(v=time(actuals(testDST1))[4994],col="red",lwd=2)

plot(actuals(testDST2)[1:(24*7)+4994-24*3],type="b")
lines(fitted(testDST2)[1:(24*7)+4994-24*3],col="blue")
points(fitted(testDST2)[1:(24*7)+4994-24*3],col="blue")
abline(v=24*3,col="red",lwd=2)


# Start of DST in 2015
plot(actuals(testDST1)[1:(24*7)+8690-24*3],type="b")
lines(fitted(testDST1)[1:(24*7)+8690-24*3],col="red")
points(fitted(testDST1)[1:(24*7)+8690-24*3],col="red")
abline(v=time(actuals(testDST1))[8690],col="red",lwd=2)

plot(actuals(testDST2)[1:(24*7)+8690-24*3],type="b")
lines(fitted(testDST2)[1:(24*7)+8690-24*3],col="blue")
points(fitted(testDST2)[1:(24*7)+8690-24*3],col="blue")
abline(v=24*3,col="red",lwd=2)


# Start of DST in 2017
plot(actuals(testDST1)[1:(24*7)+26162-24*3],type="l")
lines(fitted(testDST1)[1:(24*7)+26162-24*3],col="red")
abline(v=time(actuals(testDST1))[26162],col="red",lwd=2)

plot(actuals(testDST2)[1:(24*7)+26162-24*3],type="l")
lines(fitted(testDST2)[1:(24*7)+26162-24*3],col="blue")
abline(v=24*3,col="red",lwd=2)
dev.off()
