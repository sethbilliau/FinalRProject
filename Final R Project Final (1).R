#All functions not from class came from rdocumentation.org
#I have marked one place for each of the ways that points can be earned, some of the points have been earned in multiple places, but I did not mark them all
library(MASS) #needed for mvrnorm #new package
library(rmarkdown)#needed for LaTeX integration
opdist=5 #setup number set manunally for ease of changing between setups
op1=c(runif(1,0,1),runif(1,0,1)) #selects a random point for each voting option
op2=c(runif(1,0,1),runif(1,0,1))
op3=c(runif(1,0,1),runif(1,0,1))
op4=c(runif(1,0,1),runif(1,0,1))
op=cbind(op1,op2,op3,op4) #puts all voting options into one matrix
num=dim(op)[2] #counts the number of voting options
votes=matrix(0,num,num) #sets up a matrix of the number of each vote ordering for each option
dist1=numeric(num) #sets up a blank dist1, to be described later
step1=.02 #on a scale of zero to one, sets the size of each voting median square
xx <- seq(0,1,step1) #creates a list of all of the coordinates to be tested
win=array(NA,c(length(xx),length(xx),3)) # sets up a blank win matrix
percent1=array(NA,c(length(xx),length(xx),3)) #sets up blank percent matrices
percent2=array(NA,c(length(xx),length(xx),3))
percent3=array(NA,c(length(xx),length(xx),3))
percent4=array(NA,c(length(xx),length(xx),3))
points1=c(1,0,0,0) #defines points systems
points2=c(9,2,1,0)
points3=c(3,2,1,0)
samples=500 #sets the number of voters to be simulated at each median
voters=matrix(0,samples,2) #sets up a blank voters matrix
for(pol in seq(0,.5,.25)){ #tests 3 different levels of polarization
  for(sigma in seq(.25,.5,.25)){ #tests two levels of spread
    covmat=rbind(c(sigma,0),c(0,sigma)) #defines the covariance matrix #I learned how to create the proper covariance matrix in STAT 110
    for(ii in seq(xx)){ #runs through the medians over the x axis
      for(jj in seq(xx)){ #runs through the medians over the y axis
        votes<-matrix(0,num,num) #zeros the votes matrix
        x1 <- xx[ii] #defines the x coordinate for ease of writing
        y1 <- xx[jj] #defines the y coordinate for ease of writing
        ang<-atan((y1-.5)/(x1-.5)) #determines the polarization angle (the radius)
        if(y1>.5&x1<.5){ang <- pi/2-ang} #corrects for values not in the image of arctan
        if(y1<.5&x1<.5){ang <- pi/2+ang} #corrects for values not in the image of arctan
        if(x1==.5&y1==.5){ang=0} #corrects for values not in the domain of arctan
        for(m in 1:samples){ #generates the set number of samples #for loop #this uses the statistical technique of simulation, as the integration required would be almost impossible
          med1<-sample(1:2,1) #selects a random one or 2 #random number generation
          if(med1==1){ #sets the distribution to be the one further from the center if one is generated
            voters[m,]<-mvrnorm(1,c(x1+pol*cos(ang)/2,y1+pol*sin(ang)/2),covmat) #generates one voter from a bivariate normal centered pol/2 along the radius away from the median being tested
          }else{ #sets the distribution to the one closer to the center if two is generated
            voters[m,]<-mvrnorm(1,c(x1-pol*cos(ang)/2,y1-pol*sin(ang)/2),covmat)}}#generates one voter from a bivariate normal centered pol/2 along the radius away from the median being tested
        for(k in 1:samples){ #runs through the voters
          for(i in 1:num){ #runs through the options
            dist1[i]<-(op[1,i]-voters[k,1])^2+(op[2,i]-voters[k,2])^2} #calulates the squared distance to each option
          dist2<-order(dist1) #finds the ordering of distance to each option #this function was not taught in Math 23a
          for(j in 1:num){ #runs through all of the places
            votes[j,dist2[j]]<-votes[j,dist2[j]]+1}} #adds one vote to the correct place in each option 
        for(q in 1:3){ #runs through the points systems
          total<-get(paste("points",q,sep=""))%*%votes #finds the total number of points for each option #linear algebra (matrix multiplication)
          order1<-order(total) #finds the ordering of the options
          win[ii,jj,q]<-order1[num] #selects the winning option
          percent1[ii,jj,q]<-total[1]/sum(total) #calculates the percent of the votes for each option
          percent2[ii,jj,q]<-total[2]/sum(total)
          percent3[ii,jj,q]<-total[3]/sum(total)
          percent4[ii,jj,q]<-total[4]/sum(total)}}}
    for(p in 1:3){
      png(paste("opdist=",opdist,"num=",num,"pol=",pol,"sigma=",sigma,"samples=",samples,"points=",p,".png",sep="")) #names the output file #graphic
      plot(0:1,0:1,type = "n",axes=FALSE,ylab="",xlab="",asp=1) #sets up a clear plot
      if(sum(win[,,p]==1)==0){ #tests whether the first voting option won anywhere
        if(sum(win[,,p]==2)==0){ #tests whether the second voting option won anywhere
          image(win[,,p],col=rainbow(2,s=1,v=1,start=2/num,end=(num-1)/num,alpha=1),axes=FALSE,add=TRUE) #creates a colored plot of the winning options (for this one I started at the third color to account for the fact that the first two options were missing in order to keep the coloring consistent)
          points(op1[1],op1[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[1],pch=21) #shows the options' locations with a point of the same color as their winning area
          points(op2[1],op2[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[2],pch=21)
          points(op3[1],op3[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[3],pch=21)
          points(op4[1],op4[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[4],pch=21)
        }else{
          image(win[,,p],col=rainbow(3,s=1,v=1,start=1/num,end=(num-1)/num,alpha=1),axes=FALSE,add=TRUE)
          points(op1[1],op1[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[1],pch=21)
          points(op2[1],op2[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[2],pch=21)
          points(op3[1],op3[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[3],pch=21)
          points(op4[1],op4[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[4],pch=21)
        }
      }else{
        image(win[,,p],col=rainbow(4,s=1,v=1,start=0,end=(num-1)/num,alpha=1),axes=FALSE,add=TRUE)
        points(op1[1],op1[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[1],pch=21)
        points(op2[1],op2[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[2],pch=21)
        points(op3[1],op3[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[3],pch=21)
        points(op4[1],op4[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[4],pch=21)}
      dev.off() #clears plot
      for(pp in 1:num){ #runs through the voting options
        png(paste("opdist=",opdist,"num=",num,"pol=",pol,"sigma=",sigma,"samples=",samples,"points=",p,"op=",pp,".png",sep="")) #names the output file
        plot(0:1,0:1,type = "n",axes=FALSE,ylab="",xlab="",asp=1) #sets up a clear plot
        map<-get(paste("percent",pp,sep=""))[,,p] #creates a new varible so that it can be edited in a for loop
        map[1,1]<-1 #sets a corner to one
        map[length(xx),length(xx)]<-0 #and another to zero to ensure that all gray scales are the same for easy comparison
        image(map,col=gray(1:10/10),axes=FALSE,add=TRUE) #creates a grayscale map of vote percentages
        points(get(paste("op",pp,sep=""))[1],get(paste("op",pp,sep=""))[2],col="black",bg=rainbow(num,s=1,v=1,start=0,end=3/num,alpha=1)[pp],pch=21) #places a properly colored point at the option's location
        dev.off()}}}} #clears plot
