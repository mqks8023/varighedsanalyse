andersen.plot=function(surv){
  #surv is survfit object based on stratified Cox ph model
  
  nstrata=surv$strata
  
  plot(c(0,0),c(0,0),xlim=c(min(surv$cumhaz),max(surv$cumhaz)),ylim=c(min(surv$cumhaz),max(surv$cumhaz)),xlab="H1",ylab="H2",type="n")
  count=1
  for (i in 1:length(nstrata))
    for (j in i:length(nstrata))
      if (i != j){
        
        
        if (i==1) times1=surv$time[1:nstrata[1]] else times1=surv$time[sum(nstrata[1:(i-1)]+1):sum(nstrata[1:i])]
        
        if (j==1) times2=surv$time[1:nstrata[1]] else times2=surv$time[sum(nstrata[1:(j-1)]+1):sum(nstrata[1:j])]
        
        
        #print(times1)
        #print(times2)
        mintime=min(c(max(times1),max(times2)))
        times=sort(unique(c(times1,times2)))
        times=times[times<=mintime]
        curves=summary(surv,times=times)
        ntimes=length(times)
        #print(times)
        
        temp1=curves$cumhaz[c(1:ntimes)+(i-1)*ntimes]
        temp2=curves$cumhaz[c(1:ntimes)+(j-1)*ntimes]
        #print(ntimes)
        #print(summary(curves))
        #print(curves$time)
        print(curves$cumhaz)
        #print(temp1)
        #print(temp2)
        #include=temp1 > 0 & temp2 >0
        #temp1=temp1[include]
        #temp2=temp2[include]
        points(temp1,temp2,xlab="",ylab="",col=count)
        fit=lm(temp2~-1+temp1)
        abline(fit,col=count)
        count=count+1
      }
}