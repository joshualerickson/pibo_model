library(pwr)
pwr.anova.test(k=2, n = 119, f = .04, sig.level = 0.05)
# range of correlations
r <- seq(.1,.75,.01)
nr <- length(r)

# power values
p <- seq(.4,.9,.1)
np <- length(p)
n <- seq(10,200, 5)
np <- length(n)
# obtain sample sizes
samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.t.test(n = n[i], d = r[j],
                         sig.level = .05, power = NULL,
                         alternative = "two.sided")
    samsize[j,i] <-result$power
  }
}

# set up graph
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(n))
plot(xrange, yrange, type="n",
     xlab="Effect Size (d)",
     ylab="Power" )

# add power curves
for (i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Correlation Studies\n
  Sig=0.05 (Two-tailed)")
legend("topright", title="Sample Size", as.character(n),
       fill=colors)


m1 <- seq(5,9, 0.01)
m2 <- seq(5,9, 0.01)
sd <- seq(2,4, 0.01)

