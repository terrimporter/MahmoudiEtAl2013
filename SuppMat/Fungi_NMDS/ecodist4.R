#Written by Terri Porter, McMaster University, Biology
#edited Feb.11,2013 to fit environmental variables

pdf(file="NMDS_class_both_NG.pdf")
par(mfrow=c(2,2))

a<-read.csv("NGoodall.csv",head=TRUE,row.names=1)
a
b<-as.matrix(a)
b
library(Matrix)
c<-tril(b)
c
d<-as.matrix(c)
d
e<-as.dist(d)
e

library(ecodist)
#adjust dims for scree (nits=10, maxdim=10)
f<-nmds(e, nits=1000, maxdim=3)
f
g<-nmds.min(f) #best overall (dims=0)

#pdf(file="NMDS_scree.pdf")

#plot scree
#stress<-f$stress
#axis.seq <- c(seq(1, 1, length = 10), seq(2, 2, length = 10),seq(3, 3, length = 10), seq(4, 4, length = 10), seq(5, 5,length = 10), seq(6, 6, length = 10), seq(7, 7, length = 10),seq(8, 8, length = 10), seq(9, 9, length = 10), seq(10, 10,length = 10))
#plot(stress ~ factor(axis.seq), main="Scree")

#dev.off()

#=pod

plot(g$X1, g$X2, main="Normalized Goodall", xlab="NMDS1", ylab="NMDS2", pch=c(17,17,15,0,15,0,15,2,2,0,17,17,15,2,2,0), col=c("firebrick","darkorange","steelblue","olivedrab4","olivedrab4","olivedrab4","olivedrab4","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue"))

g	
env<-read.csv(file="sedimentallsamples.csv")
vars<-env[,2:4]
vars

df12<-cbind(g$X1,g$X2)
df12
vf12<-vf(df12, vars, nperm=1000)
vf12
plot(vf12, col="black")

plot(g$X2, g$X3, main="Normalized Goodall", xlab="NMDS2", ylab="NMDS3", pch=c(17,17,15,0,15,0,15,2,2,0,17,17,15,2,2,0), col=c("firebrick","darkorange","steelblue","olivedrab4","olivedrab4","olivedrab4","olivedrab4","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue"))

df23<-cbind(g$X2,g$X3)
df23
vf23<-vf(df23, vars, nperm=1000)
vf23
plot(vf23, col="black")

plot(g$X1, g$X3, main="Normalized Goodall", xlab="NMDS1", ylab="NMDS3", pch=c(17,17,15,0,15,0,15,2,2,0,17,17,15,2,2,0), col=c("firebrick","darkorange","steelblue","olivedrab4","olivedrab4","olivedrab4","olivedrab4","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue"))

df13<-cbind(g$X1,g$X3)
df13
vf13<-vf(df13, vars, nperm=1000)
vf13
plot(vf13, col="black")

#print out lowest stress and highest r2
#min(f$stress)
#max(f$r2)

dev.off()

pdf(file="NMDS_class_both_CS.pdf")
par(mfrow=c(2,2))

A<-read.csv("ChiSquared.csv",header=TRUE,row.names=1)
A
B<-as.matrix(A)
B
library(Matrix)
C<-tril(B)
C
D<-as.matrix(C)
E<-as.dist(D)
library(ecodist)
F<-nmds(E,nits=1000,maxdim=3)
G<-nmds.min(F)

plot(G$X1, G$X2, xlab="NMDS1", ylab="NMDS2", main="Chi Squared", pch=c(17,17,15,0,15,0,15,2,2,0,17,17,15,2,2,0), col=c("firebrick","darkorange","steelblue","olivedrab4","olivedrab4","olivedrab4","olivedrab4","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue"))

G
vars
DF12<-cbind(G$X1,G$X2)
DF12
VF12<-vf(DF12, vars, nperm=1000)
VF12
plot(VF12, col="black")

plot(G$X2, G$X3, xlab="NMDS2", ylab="NMDS3", main="Chi Squared", pch=c(17,17,15,0,15,0,15,2,2,0,17,17,15,2,2,0), col=c("firebrick","darkorange","steelblue","olivedrab4","olivedrab4","olivedrab4","olivedrab4","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue"))

DF23<-cbind(G$X2,G$X3)
DF23
VF23<-vf(DF23, vars, nperm=1000)
VF23
plot(VF23, col="black")

plot(G$X1, G$X3, xlab="NMDS1", ylab="NMDS3", main="Chi Squared", pch=c(17,17,15,0,15,0,15,2,2,0,17,17,15,2,2,0), col=c("firebrick","darkorange","steelblue","olivedrab4","olivedrab4","olivedrab4","olivedrab4","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue","firebrick","darkorange","steelblue"))

DF13<-cbind(G$X1,G$X3)
DF13
VF13<-vf(DF13, vars, nperm=1000)
VF13
plot(VF13, col="black")

#print out lowest stress and highest r2
#min(F$stress)
#max(F$r2)

dev.off()

save.image(file="NMDS_both_class.RData")

#=cut

quit()
