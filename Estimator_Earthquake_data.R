
x<-read.table("earthquake.txt",header=FALSE)

mag<-x[[5]]
mag<-sort(mag)

n<-length(mag)
a<-0.05
ep<-sqrt((log(2/a))/(2*n))


plot(ecdf(mag),xlab="Earthquake magnitude",ylab="Fn(mag)")
U<-NULL
L<-NULL
for(i in 1:length(mag)){

U[i]<-min(ecdf(mag)(mag[i])+ep,1)
L[i]<-max(ecdf(mag)(mag[i])-ep,0)}

lines(mag, U, col="blue")

lines(mag, L, col="red")
