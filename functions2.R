### actual doing-stuff functions
library(tidyverse)

cumulants=function(n,p) {
  k=numeric(6)
  npq=n*p*(1-p)
  k[1]=n*p
  k[2]=npq
  k[3]=npq*(1-2*p)
  k[4]=npq*(1-6*p*(1-p))
  k[5]=npq*(1-2*p)*poly(p,c(12,-12,1))
  k[6]=npq*poly(p,c(120,-240,150,-30,1))
  the_names=paste0("K",1:6)
  data.frame(which=the_names,cumulant=k)
}


cumulant_sum=function(n,p) {
  # n and p are now vectors (also works if they are scalars)
  map2(n,p,cumulants) %>% bind_rows() %>% 
    group_by(which) %>% summarize(cumulant=sum(cumulant))
}

moment_0=function(kk) {
  k = kk %>% pull(cumulant)
  m0=numeric(6)
  m0[1]=k[1]
  m0[2]=k[2]+k[1]^2
  m0[3]=k[3]+3*k[2]*k[1]+k[1]^3
  m0[4]=k[4]+4*k[3]*k[1]+3*k[2]^2+6*k[2]*k[1]^2+k[1]^4
  m0[5]=k[5]+5*k[4]*k[1]+10*k[3]*k[2]+10*k[3]*k[1]^2+15*k[2]^2*k[1]+10*k[2]*k[1]^3+k[1]^5
  m0[6]=k[6]+6*k[5]*k[1]+15*k[4]*k[2]+15*k[4]*k[1]^2+10*k[3]^2+60*k[3]*k[2]*k[1]+20*k[3]*k[1]^3+
      15*k[2]^3+45*k[2]^2*k[1]^2+15*k[2]*k[1]^4+k[1]^6
  data.frame(which=1:6,moment=m0)
}

kolmo=function(n,p,n00,p00) {
  # n and p are vector inputs; n00 and p00 are scalar n and p for initial approx
  # step 1
  p0=dbinom(0:n00,n00,p00)
  difference=diff(c(0,p0))
  # step 2
  true_cumulants=cumulant_sum(n,p)
  nu=moment_0(true_cumulants) # a data frame
  initial_cumulants=cumulants(n00,p00)
  mu_0=moment_0(initial_cumulants) # of initial approximant
  # step 3
  a=numeric(6)
  a[1]=-(nu$moment[1]-mu_0$moment[1])
  # step 4
  p=matrix(-1,length(p0),6)
  p[,1]=p0+a[1]*difference
  # step 5 (the loop)
  for (k in 2:6) {
    # step (a)
    difference=diff(c(0,difference))
    # (b): beware, Fortran programming ahead!
    sum_j=0
    for (j in 1:(k-1)) {
      sum_i=0
      for (i in j:(k-1)) {
        sum_i=sum_i+clji(k,j,i)*mu_0$moment[k-i]
      }
      # add on k term, taking mu_0[0]=1
      sum_i=sum_i+clji(k,j,k)*1
      sum_j=sum_j+a[j]*sum_i
    }
    mu_km1=mu_0$moment[k]+sum_j
    # (c)
    a[k]=(nu$moment[k]-mu_km1)/factorial(k)
    if (remainder(k,2)==1) a[k]=-a[k]
    # (d)
    p[,k]=p[,k-1]+a[k]*difference
  }
  # return df of actual probabilities
  data.frame(s=0:(length(p0)-1),P.0=p0,P=p)
}

kolmo2=function(n,p,n00,p00) {
  kolmo(n,p,n00,p00) %>% 
    mutate_at(vars(starts_with("P")),funs(cumsum))
}

kolmogorov=function(n,p) {
  n00=sum(n)
  p00=sum(n*p)/n00
  kolmo2(n,p,n00,p00)
}
