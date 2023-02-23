#Intro to paternity likelihood calculation
#Topics covered in this code include:
#Paternity likelihood calculation according to Marshall et al. 1998
#This includes paternity likelihood if we don't know the mothers genotype (p.like), and 
#paternity likelihood if we DO know the mothers genotype (p.like.mom)
#This simplistic intro is assuming allele frequencies are .5 (but can be easily adapted otherwise).

#A brief intro about what we mean when we talk about likelihoods.
#Think about a coin flip that has a 50% probability of heads and tails outcomes
#You can simulate this easily using rbinom function -- AKA a (r)andom draw from the (binom)ial distribution.
#We can simulate a bunch of tests where we flip a coin 6 times
coins<-rbinom(n = 1000,size=6,prob=c(.5,.5))

#On average we'd expect about 3 heads and tails. 
#For my random draw I got 3.01 on average, but it will be different each time
mean(coins)

#An alternative way to think about coin flips is in the reverse direction. 
#Say, for example, we find a random coin on the ground and we flip it 10 times.
#Surprisingly, we get 10 heads. 
#We would then ask: What is the LIKELIHOOD of observing 10 head flips in a row, if the coin were a fair coin?
#In simple terms, this breaks down to:
#The odds of flipping a heads on the 1st flip X the odds of flipping a heads on the 2nd flip... all the way to the 10th flip
#Well if the coin is fair, the odds of flipping a head are always 1/2
#So we can simply write the likelihood as:
(1/2)^10

#Or, 1 in 1024
1/((1/2)^10)

#So maybe the coin isn't fair? Maybe instead the coin is a trick coin and flips heads 90% of the time?
#If that were the case, we can recalculate the likelihood as:
(.9)^10

#Or about 1 in 3
1/((.9)^10)

rm(coins)
#In other words, given this observation of coin flips, the results are much more likely to be observed if the coin is a fake coin (1 in 3),
#versus if the coin is a fair coin (1 in 1024). As a result, we might conclude that the coin is fake. 

#Similarly, here we are started with an observation. 
#The observation are the offspring's genotype values, and the possible parent's genotype values.
#And now we're basically asking:
#What is the LIKELIHOOD of observing the offpsrings genotypes, GIVEN that Individual 1 IS THE FATHER?
#What is the LIKELIHOOD of observing the offpsrings genotypes, GIVEN that Individual 2 IS THE FATHER?
#Etc.. for all possible fathers.
#In addition to this, at the same time, we are asking:
#Is the likelihood of observing the offpsrings genotypes, GIVEN that Individual N IS THE FATHER greater than what we'd expect by chance?

#This is the foundation of paternity likelihood calculations. Hopefully as we code these likelihoods, this will become even more clear.



#First, lets write a function that calculates the likelihood that two individuals are parent/offsprings
#The individual probabilities come straight from Marshall et al. Table 1.

like<-matrix(NA,nrow=9,ncol=3)
like<-as.data.frame(like)
like[,1]<-c(0,1,2,0,1,2,0,1,2)
like[,2]<-c(0,0,0,1,1,1,2,2,2)
colnames(like)<-c("parent","kid","probability")

#For this matrix column 1 is the potential parent's genotype
#Column 2 is the offspring's genotype
#Column 3 is the probability of observing the offsprings genotype, given this individual is the true father
#Note here that these probabilities are all written assuming an allele frequency of .5
#I.e. in some cases Marshall says the probability is (b). But here we're assuming b=c=.5 
#So I just wrote the numeric equivalent instead. 

like[1,3]<-.5
like[2,3]<-.25
like[3,3]<-0
like[4,3]<-.5
like[5,3]<-.5
like[6,3]<-.5
like[7,3]<-0
like[8,3]<-.25
like[9,3]<-.5



#Now lets write a function that uses these probabilities to calculate a likelihood that individual A is the father
p.like<-function(parent,kid,af=NA){
  #Remove missing data
  keep<-which((!is.na(parent)) & (!is.na(kid)))
  parent<-parent[keep]
  kid<-kid[keep]
  #Right now we are leaving allele frequencies at .5, but this can be later modified along with the matrix above to be more flexible.
  if(is.na(af)){
    af<-rep(.5,length=length(parent))}
  
  #Make an empty string for us
  dif<-rep(NA,length=length(parent))
  
  #Lets get the individual probabilities at each locus from the matrix we created above
  #This loops across each locus (f), and for each value it
  #stores the probability of observing from the matrix we made above.
  
  #I.e. if parent genotype=0 and offspring=0, this value returns 0.5 which is 
  #the probability of having an offspring with a 0 genotype if you yourself have a 0 genotype
  like[like[,1]==0 & like[,2]==0,3]
  
  for (f in 1:length(dif)){
    dif[f]<-like[like[,1]==parent[f] & like[,2]==kid[f],3]
  }
  
  #Once we have the individual probabilities at each locus, we simply multiply them together to get the likelihood
  l1<-prod(dif)
  
  #Next we want to compare this result to what the likelihood of observing the offspring genotype values are,
  #given a RANDOM male is the father (i.e. not this individual).
  #These probability values come straight from Marshall et al. Table 1 also. 
  #In reality these are just the probabilities according to hardy-weinberg equilibrium.
  #Make an empty string
  dif2<-rep(NA,length=length(parent))
  
  #If the offsprings genotype is a 0, the probability of observing that at random is (p^2), where p is the allele frequency of the reference allele 
  dif2[kid==0]<-af[kid==0]^2
  
  #If the offsprings genotype is a 1, the probability of observing that at random is (2*p*q), where p is the allele frequency of the reference allele and where q is the allele frequency of the alternate allele 
  dif2[kid==1]<-2*af[kid==1]*(1-af[kid==1])
  
  #If the offsprings genotype is a 2, the probability of observing that at random is (q^2), where q is the allele frequency of the alternate allele 
  dif2[kid==2]<-(1-af[kid==2])^2
  
  #Again we calculate the likelihood by multiplying these individual probabilities
  l2<-prod(dif2)
  
  #And we return the natural log ratio of the two likelihoods
  lod<-log(l1/l2)
  return(lod)
}


#Now, what if I sumulate 100 possible fathers (1 of which is the true father), and corresponding offspring genotypes
#At 10 loci
dads<-matrix(NA,nrow=10,ncol=100)

#For 100 possible fathers, simulate 10 genotype values (ref allele frequency for each locus is .5)
for(f in 1:100){
  dads[,f]<-sample(c(0,1,2),size=10,replace=T,prob=c(.25,.5,.25))
  
}

#Randomly select one as the father
true<-sample(1:100,size = 1)

#Given the true father, simulate the genotype values for the offspring at each locus accordingly
kid<-rep(NA,10)

for(f in 1:10){
  if(dads[f,true]==0){
    kid[f]<-sample(c(0,1),size=1,prob=c(.5,.5))
  }
  if(dads[f,true]==1){
    kid[f]<-sample(c(0,1,2),size=1,prob = c(.25,.5,.25))
  }
  if(dads[f,true]==2){
    kid[f]<-sample(c(1,2),size=1,prob=c(.5,.5))
  }
}

#Now, for each possible father, lets calculate the paternity likelihood (lod) from our function p.like from before
lod<-rep(NA,100)
for(f in 1:100){
  lod[f]<-p.like(dads[,f],kid)
}

#One of the first things we find is that, for many possible fathers, the lod score is negative infinite
#This is because, some parent-offspring genotypes are IMPOSSIBLE
#I.e. If the parent at a locus is AA (genotype value of 2), the offspring CANT BE TT (genotype value of 0)
table(lod)

#But this assumes we have absoutely perfect genotyping. 
#Marshall et al. appendix introduces a modification to account for imperfect genotyping
#We havent incorporated that here, but likely will have to in the future. 
hist(lod,breaks=100)

#Still we see the LOD score for the true father is positive, but may not necessarily be the highest value
lod[true]

#This is because we used a relatively low number of loci (10 here).
#You can expand this number to high #s of loci to see how many genotypes we need to confidently call parentage in a 
#pool of possible fathers.




