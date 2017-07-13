# stuff

# Stirling2 is (n,m) where m is upstairs in brackets
# ie first parameter greater than second

Stirling2(4,2)
choose(4,2)

clji(6,5,6)

ans=data.frame()
for (l in 1:6) {
  for (j in 1:l) {
    for (i in j:l) {
      ans=rbind(ans,c(l=l,j=j,i=i,clji=clji(l,j,i)))
    }
  }
}
ans


kolmo(c(2,3),c(0.2,0.1),5,0.1)
kolmo2(c(2,3),c(0.2,0.1),5,0.1)
kolmogorov(c(2,3),c(0.2,0.1))
# examples in paper

# example 1
n=rep(5,5)
p=seq(0.02,0.10,0.02)
n
p
kolmogorov(n,p)
# check
# example 2
n=rep(100,5)
p=seq(0.01,0.03,0.005)
n
p
kk=kolmogorov(n,p)
kk %>% filter(s==19)
# check; checked several

# example 3
n=seq(500,100,-100)
p=1/n
n
p
kk=kolmogorov(n,p)
kk %>% filter(s==5)
kk %>% filter(s==8)
kk %>% filter(s==10)
kk %>% filter(s==14)

# example 4
n=seq(50,250,50)
p=seq(0.1,0.5,0.1)
n
p
kk=kolmogorov(n,p)
kk %>% filter(s==275)
kk %>% filter(s==296)
kk %>% filter(s==305)
kk %>% filter(s==320)

# example 5
n=c(3,6,2,7)
p=c(0.016,0.071,0.093,0.035)
n
p
kk=kolmogorov(n,p)
kk %>% filter(s==0)
kk %>% filter(s==3)
kk %>% filter(s==5)

# example 5
n=c(12,14,4,2,20,17,11,1,8,11)
p=c(0.074,0.039,0.095,0.039,0.053,0.043,0.067,0.018,0.099,0.045)
n
p
kk=kolmogorov(n,p)
kk %>% filter(s==0)
kk %>% filter(s==5)
kk %>% filter(s==8)
