x=rnorm(1000,5,2)

y=0.8*x+rnorm(1000)

plot(x,y,pch=16)

n=length(x)

bstart=c(-2,-2)
bdev=c(0,0)
tol=1e-06


# Difs=y-bstart[1]-bstart[2]*x ; L=sum(Difs^2)
ind=c(40,100,150,200,500,700,1000,1200,2000,2500)
Hist=matrix(0,nrow=length(ind),ncol=2)
k=1

for (i in 1:5000) {
  bdev[1]=-(1/n)*sum(y-bstart[1]-bstart[2]*x)
  bdev[2]=-(1/n)*sum(x*(y-bstart[1]-bstart[2]*x))
  temp=bstart 
  bstart=bstart-0.02*bdev 
  if ( sum(abs(temp-bstart) ) <tol ) {
    stop("OK")
  }
  if (sum(i==ind)==1 ) {
   Hist[k,]=bstart 
   k=k+1
  }
}
i
ind=c(ind,i)
Hist=rbind(Hist,bstart)
bstart


lm(y~x)
