x <- matrix( scan( 'bloodfat.dat', comment.char = '#' ), ncol = 2, byrow = TRUE, dimnames = list( NULL, c( 'Cholesterol', 'Triglycerides' ) ) )
x1 <- x[1:51,]
x2 <- x[52:371,]
xx1 <- x1[,1]
xx2 <- x2[,1]
n1 <- length(xx1)
n2 <- length(xx2)
th.hat <- mean(xx2) - mean(xx1)
alpha <- 0.05
B <- 10000
v1<-var(xx1)
v2<-var(xx2)
vx = v1/n1
vy = v2/ n2
var_th.hat <- vx + vy
se.hat <- sqrt(var_th.hat)
Cn = th.hat + c(-1, 1) * se.hat * qnorm(1-alpha/2)

Normal <-     th.hat + c( -1, +1 ) * qnorm( 1 - 0.5 * alpha ) * se.hat
Pivotal <-     2 * th.hat - c(quantile(Th_Conf_Int, c( 1 - 0.5 * alpha, 0.5 * alpha ) ))
Percentile <-      quantile( Th_Conf_Int, c( 0.5 * alpha, 1 - 0.5 * alpha ) )
Estimator <- function(xx1,xx2){
  # Number of data points 
  n1 <- length(xx1)
  n2 <- length(xx2)
  
  # Estimator for theta_hat
  th.hat <- mean(xx2) - mean(xx1)
  
  v1<-var(xx1)
  v2<-var(xx2)
  vx = v1/n1
  vy = v2/ n2
  var_th.hat <- vx + vy
  se.hat <- sqrt(var_th.hat)
  Cn = th.hat + c(-1, 1) * se.hat * qnorm(1-alpha/2)
  return(c(th.hat, Cn))
}
Th_Conf_Int = replicate(100000, Estimator(sample(xx1, length(xx1), TRUE), sample(xx2, length(xx2), TRUE)))

## Plotting Histogram NORMAL,PERCENTILE,PIVOTAL

a <- hist( Th_Conf_Int,'FD' ,col = 'grey', border = 'black', xlab = expression( widehat( theta )[Boot] ), xlim = range( Th_Conf_Int, Normal, Percentile, Pivotal ), main = 'Bootstrap' )
abline( v = th.hat, col = 'red', lty = 2)
axis( 3, th.hat, expression( widehat( theta ) ) )

x <- 0.5 * max( a$counts )
lines( Normal, rep( x, 2 ), lty = 3 )
text( max( Normal ), x, 'Normal', pos = 4 )

x <- 0.4 * max( a$counts )
lines( Percentile, rep( x, 2 ), lty = 3 )
text( max( Percentile ), x, 'Percentile', pos = 4 )

x <- 0.6 * max( a$counts )
lines( Pivotal, rep( x, 2 ), lty = 3 )
text( max( Pivotal ), x, 'Pivotal', pos = 4 )

#Plottig the Confidence interval
hist(c(Th_Conf_Int[2,], Th_Conf_Int[3,]), freq = FALSE, 
     main = 'Histogram for Confidence Interval limits', xlab = ''
     , breaks = 60, xlim = c(-10, 50), ylim = c(0,0.04))
