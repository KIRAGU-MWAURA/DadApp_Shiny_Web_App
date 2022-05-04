library(tidyr)
data_dads <- read.csv('fathers101.csv', header = FALSE)
data_offspring <- read.csv('Offspring.csv', header = FALSE)
data_mothers<-read.csv('mothers101.csv', header = FALSE)


dads <- as.matrix(data_dads)
dads <-  replace(dads, !(dads %in% c(0, 1, 2)), NA)
offspring <- as.matrix(data_offspring)
mothers<-as.matrix(data_mothers)


allelefreq_output <-vector("numeric", length = ncol(dads))
for (i in 1:ncol(dads)){
  
  homozygous_reference<-dads[which(dads[,i]==0),i] 
  
  homozygous_alternate<-dads[which(dads[,i]==2),i]
  
  heterozygous<-dads[which(dads[,i]==1),i]
  
  present<-length(homozygous_alternate)*2+ length(homozygous_reference)*2+length(heterozygous)*2
  
  allelefre_reference<-(length(homozygous_reference)*2 + length(heterozygous))/present
  
  allelefreq_output[i]<-allelefre_reference
  
}
allelefreq_output

#Calculating probability based on Hardy-Weinberg equilibrium
hardy_loci <- vector("numeric", length = ncol(offspring)) # create an empty string that stores the likelihood of observing the offspring genotype values are,
#given a RANDOM male is the father.According to Hardy-Weinberg equilibrium

for(f in 1:length(offspring)){   # Iterates across the no. of loci of the offspring
  #If the offspring's genotype is a 0, the probability of observing the genotype at random is (p^2), 
  #where p is the allele frequency of the reference allele 
  if(offspring[f]== 0){
    hardy_loci[f]<- allelefreq_output[f]^2
  }
  #If the offspring's genotype is a 1, the probability of observing that at random is (2*p*q), 
  #where p is the allele frequency of the reference allele and where q is the allele frequency of the alternate allele 
  if(offspring[f]== 1) {
    hardy_loci[f]<- 2*allelefreq_output[f]*(1 - allelefreq_output[f])
  }
  #If the offspring's genotype is a 2, 
  #the probability of observing that at random is (q^2), where q is the allele frequency of the alternate allele 
  if(offspring[f]== 2){
    hardy_loci[f]<- (1-allelefreq_output[f])^2 
  }
}
hardy_loci
hardy_likelihood <- prod(hardy_loci) #multiply the probability to get the likelihood in accordance to Hardy-Weinberg equilibrium
hardy_likelihood

#Calculating background probability.
trio_like<-matrix(NA,nrow=27,ncol=4)
trio_like<-as.data.frame(trio_like)

trio_like[,1]<-c(0,0,0,1,1,1,2,2,2,0,0,0,1,1,1,2,2,2,0,0,0,1,1,1,2,2,2)
trio_like[,2]<-c(0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2)
trio_like[,3]<-c(0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2)
colnames(trio_like)<-c("potential_father","offspring_interest","real_mother","probabilities")

trio_like[1,4]<-1
trio_like[2,4]<-.25
trio_like[3,4]<-0
trio_like[4,4]<-.5
trio_like[5,4]<-.25
trio_like[6,4]<-0
trio_like[7,4]<-0
trio_like[8,4]<-0
trio_like[9,4]<-0
trio_like[10,4]<-0
trio_like[11,4]<-.5
trio_like[12,4]<-1
trio_like[13,4]<-.5
trio_like[14,4]<-.5
trio_like[15,4]<-.5
trio_like[16,4]<-1
trio_like[17,4]<-.5
trio_like[18,4]<-0
trio_like[19,4]<-0
trio_like[20,4]<-0
trio_like[21,4]<-0
trio_like[22,4]<-.0
trio_like[23,4]<-.25
trio_like[24,4]<-.5
trio_like[25,4]<-0
trio_like[26,4]<-.5
trio_like[27,4]<-1
trio_like

trio_like <- trio_like %>%
  unite('trio_combo', potential_father:real_mother, sep = '_')
trio_like

comb_genotype <- function(dads, offspring, mothers=matrix(NA,nrow = 1, ncol = ncol(dads))){
  
  offspring_mothers <- rbind(mothers, offspring)#combine rows of both offspring and mothers
  m1 <- lapply(1:nrow(dads), function(x) rbind(dads[x,],offspring_mothers))#create a list of matrices with trio combinations
  
  combine <- matrix(NA, nrow = (dim(dads)[1]), ncol = (dim(dads))[2])#Create an empty matrix that will store collapsed trio combinations
  for (a in 1:length(m1)){ #loop in the list of matrices
    for (b in 1:ncol(m1[[a]])){#loop in the column of every matrices
      combine[a,b] <- paste(c(m1[[a]][,b]), collapse = '_') #combine every column of every matrix and store in combine matrix
    }
  }
 return(combine)#Give the combine matrix as an output 
}
combine
combined_genotype <- comb_genotype(dads = dads, offspring = offspring, mothers = mothers)
combined_genotype
#combined_genotype<- comb_genotype(dads = dads, offspring = offspring)

matched_genotype <- matrix((match(combined_genotype, trio_like[,1])), #create a variable that stores the position of matched genotype
                           nrow= nrow(combined_genotype),        #after comparing combined genotype of trio scenario to the 'like' matrix
                           ncol= ncol(combined_genotype), byrow = F)
matched_genotype

background_probability <- function(x){ # a function that assigns the probability from the matrix 'like' 
  prob_loci <- matrix(NA, nrow = nrow(combined_genotype), ncol = ncol(combined_genotype))
  likelihood_loci<-vector(mode = "numeric", length = nrow(combined_genotype))
  for (c in 1:nrow(matched_genotype)){
    for (d in 1:ncol(matched_genotype)){
      prob_loci[c,d] <- trio_like[matched_genotype[[c,d]],2]
     likelihood <- apply(prob_loci,1,prod) 
    }
  }
 return(likelihood) 
}

likelihood_loci <- background_probability(matched_genotype)

lod <-vector("numeric", length = length(likelihood_loci))
for (g in 1:length(likelihood_loci)){ #divides 
  lod[g]<- log(likelihood_loci[g]/hardy_likelihood)
}
lod
data_lod<- data.frame(lod)
data_lod
row.names(data_lod)<- paste("Father", rownames(data_lod), sep = "_")
data_lod
table(lod)

lod[is.infinite(lod)]=-1
hist(lod, breaks = 100, col = 'steelblue3', border = 'grey30', 
     main = "Histrogram of LOD Scores", xlab = "LOD SCORES", ylab = "FREQUENCY")

