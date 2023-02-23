
output_lod <- matrix(data = NA, nrow = 100, ncol = 100) #where nrow represents the number of simulation whereas 
#ncol represents the number of fathers

  for (i in 1:100) { #iterates across the number of simulations
  
   dads<-matrix(NA,nrow=10,ncol=100) #nrows represents the number of loci whereas ncol represents the number of potential fathers
  
   for (d in 1:100){ #iterates across the possible number of fathers
     
    dads[,d]<-sample(c(0,1,2),size=10,replace=T,prob=c(.25,.5,.25))}# assigns genotypes of the potential fathers in accordance to Hardy Weinberg equilibrium
   
  true<- 1 # Assigns the first father to be true father
  kid<-rep(NA,10)# offsprings genotype matrix
  
  for(k in 1:10){   # Iterates across the no. of loci
    if(dads[k,true]==0){   # if the dad is o then assign the offspring as either 0 or 1 with a probability of 0.5 coming from the allele frequency
      kid[k]<-sample(c(0,1),size=1,prob=c(.5,.5))
    }
    if(dads[k,true]==1){
      kid[k]<-sample(c(0,1,2),size=1,prob = c(.25,.5,.25))
    }
    if(dads[k,true]==2){
      kid[k]<-sample(c(1,2),size=1,prob=c(.5,.5))
    }
  }
  #Defining a p.like function uses these probabilities to calculate a likelihood that individual A is the father
  #given the following 
  like<-matrix(NA,nrow=9,ncol=3)
  like<-as.data.frame(like)
  like[,1]<-c(0,1,2,0,1,2,0,1,2)
  like[,2]<-c(0,0,0,1,1,1,2,2,2)
  colnames(like)<-c("Potential father","Offspring","probability")
  like
  like[2,3]<-.25
  like[3,3]<-0
  like[4,3]<-.5
  like[5,3]<-.5
  like[6,3]<-.5
  like[7,3]<-0
  like[8,3]<-.25
  like[9,3]<-.5
  like
  
  p.like<-function(parent,kid,af=NA){
    #Remove missing data
    keep<-which((!is.na(parent)) & (!is.na(kid)))
    parent<-parent[keep]
    kid<-kid[keep]
    
    if(is.na(af)){  #Right now we are leaving allele frequencies at 0.5 
      af<-rep(.5,length=length(parent))}
    
    #Make an empty string for us
    dif<-rep(NA,length=length(parent))
    like[like[,1]==0 & like[,2]==0,3]
    
    for (f in 1:length(dif)){
      dif[f]<-like[like[,1]==parent[f] & like[,2]==kid[f],3]
    }
    
    l1<-prod(dif)
    
    dif2<-rep(NA,length=length(parent))
    dif2[kid==0]<-af[kid==0]^2
    dif2[kid==1]<-2*af[kid==1]*(1-af[kid==1])
    dif2[kid==2]<-(1-af[kid==2])^2
    
    l2<-prod(dif2)
    
    #And we return the natural log ratio of the two likelihoods
    lod<-log(l1/l2)
    return(lod)
  }
  lod<-rep(NA,100)# pro
  for(s in 1:100){ #iterating across the possible fathers
    lod[s]<-p.like(dads[,s],kid)
  }
  output_lod[,i]<-lod
output_lod
  }
output_lod

