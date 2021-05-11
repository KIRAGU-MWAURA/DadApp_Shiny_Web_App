
# Define server logic required to draw a histogram
server <- function(input, output){
  
  output$lodScoreplot <- renderPlot({
    i2<-input$fathers
    i3<-input$loci
    i4<-input$allele
    
    for(i in 1:i2){
      
      ### Defining the p.like functions in calling for paternity ###
      like<-matrix(NA,nrow=9,ncol=3)
      like<-as.data.frame(like)
      like[,1]<-c(0,1,2,0,1,2,0,1,2)
      like[,2]<-c(0,0,0,1,1,1,2,2,2)
      colnames(like)<-c("potential father","offspring","probability")
      
      #For this matrix column 1 is the potential father's genotype
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
      like
      
      #Now lets write a function that uses these probabilities to calculate a 
      #likelihood that individual A is the father
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
      
      
      ############# 
      #Simulations of calling for paternity model
      #############
      dads<-matrix(NA,nrow=10,ncol=100) #nrows represents the number of loci whereas 
      #ncol represents the number of potential fathers
      
      for (d in 1:i2){ #iterates across the possible number of fathers
        
        dads[,d]<-sample(c(0,1,2),size=10,replace=T,prob=c(i4^2, i4,i4^2))}# assigns genotypes of the potential fathers 
      #in accordance to Hardy Weinberg equilibrium
      
      true<- 1 # Assigns the first father to be true father
      kid<-rep(NA,10)# offsprings genotype matrix
      
      for(k in 1:i3){   # Iterates across the no. of loci
        if(dads[k,true]==0){   
          # if the dad is o then assign the offspring as either 0 or 1 with a probability of 0.5 coming from the allele frequency
          kid[k]<-sample(c(0,1),size=1,prob=c(i4,(1-i4)))
        }
        if(dads[k,true]==1){
          kid[k]<-sample(c(0,1,2),size=1,prob = c(i4^2,i4,i4^2))
        }
        if(dads[k,true]==2){
          kid[k]<-sample(c(1,2),size=1,prob=c(i4, (1-i4)))
        }
      }
      lod<-rep(NA,10)# pro
      for(s in 1:100){ #iterating across the possible fathers
        lod[s]<-p.like(dads[,s],kid)
      }
      # generate bins based on input$bins from ui.R
      # draw the histogram with the specified number of bins
      hist(lod, col = 'steelblue3', border = 'grey30', 
           main = "Histrogram of LOD Scores", xlab = "LOD SCORES", ylab = "FREQUENCY")
    }
  })
}
shinyApp(ui = ui, server = server)