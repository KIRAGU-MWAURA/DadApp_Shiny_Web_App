server<-function(input, output, session){
    observeEvent(input$submit,{
      #Uploading whole population, dads' and offspring dataset
      whole_popn<-as.matrix(read.csv(input$popn_infile4$datapath, header = FALSE))
      dads<-read.csv(input$fathers_infile1$datapath, header = FALSE)
      names_dads<-rownames(dads)
      dads<-as.matrix(dads)
      offspring<-as.matrix(read.csv(input$offspring_infile2$datapath, header = FALSE))

      #Change loci that are not 0,1,2 into NAs
      whole_popn<-replace(whole_popn, !(whole_popn %in% c(0, 1, 2)), NA)
      dads<-replace(dads, !(dads %in% c(0, 1, 2)), NA)
      offspring<-replace(offspring, !(offspring %in% c(0, 1, 2)), NA)
      
      
      #Calculating the number of NAs of every potential father
      NAs_loci<- rowSums(is.na(dads))
      data_NAs <- data.frame(Potential_Fathers = paste("Father", rownames(data.frame(NAs_loci)), sep = "_"), NAs_loci)
      
      #Calculating the allele frequency of every loci and storing in the variable called allelefreq_output
      allelefreq_output<-apply(whole_popn,2,function(whole_popn){((length(whole_popn[which(whole_popn==0)]))*2 + length(whole_popn[which(whole_popn==1)]))/(2*length(whole_popn[!is.na(whole_popn)]))})
      
      maf_loci<-vector("numeric", length = length(allelefreq_output)) #calculate MAF of loci
      for(a in 1:length(allelefreq_output)){
        if (a<0.5){
          maf_loci[a]<-allelefreq_output[a]
        } 
        else {maf_loci[a]<-(1-allelefreq_output[a])}
        
      }
      
      #First filter with respect to offspring's genotype calls. Removing loci from fathers' data set that is/are NAs in offspring loci
      #Second filter- Removing loci that have allele frequency equals to 0 or more than 1
      #Third filter- Removes loci that have MAF that is less than 0.05
      
      keep_allele<-which((!is.na(offspring) & (allelefreq_output !=0) & (allelefreq_output <1) & (maf_loci>0.05)))
      allelefreq_output<-allelefreq_output[keep_allele]
      offspring_loci<-t(as.matrix(offspring[,keep_allele]))
      dads_loci<-as.matrix(dads[,keep_allele])

      
      hardy_loci <- vector("numeric", length = ncol(offspring)) # create an empty string that stores the likelihood of observing the offspring genotype values are,
      #given a RANDOM male is the father.According to Hardy-Weinberg equilibrium
      
      for(f in 1:length(offspring_loci)){ # Iterates across the no. of loci of the offspring
        #If the offsprings genotype is a 0, the probability of observing the genotype at random is (p^2), where p is the allele frequency of the reference allele 
        if(offspring_loci[f]== 0){
          hardy_loci[f]<- allelefreq_output[f]^2
        }
        #If the offsprings genotype is a 1, the probability of observing that at random is (2*p*q), where p is the allele frequency of the reference allele and where q is the allele frequency of the alternate allele 
        if(offspring_loci[f]== 1) {
          hardy_loci[f]<- 2*allelefreq_output[f]*(1 - allelefreq_output[f])
        }
        #If the offsprings genotype is a 2, the probability of observing that at random is (q^2), where q is the allele frequency of the alternate allele 
        if(offspring_loci[f]== 2){
          hardy_loci[f]<- (1-allelefreq_output[f])^2 
        }
      }
      hardy_loci
      hardy_likelihood <- prod(hardy_loci) #multiply the probability to get the likelihood in accordance to Hardy-Weinberg equilibrium
      
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
      
      trio_like <- trio_like %>% unite('trio_combo', potential_father:real_mother, sep = '_')
      
      #No maternal genotype calls
      #Possible combinations of genotype calls of both potential father and offspring 
      #with their probabilty in accordance to Marshall et al
      duo_like<-matrix(NA,nrow=9,ncol=3)
      duo_like<-as.data.frame(duo_like)
      duo_like[,1]<-c(0,1,2,0,1,2,0,1,2)
      duo_like[,2]<-c(0,0,0,1,1,1,2,2,2)
      colnames(duo_like)<-c("potential_father","offspring_interest","probabilities")
      
      duo_like[1,3]<-.5
      duo_like[2,3]<-.25
      duo_like[3,3]<-0
      duo_like[4,3]<-.5
      duo_like[5,3]<-.5
      duo_like[6,3]<-.5
      duo_like[7,3]<-0
      duo_like[8,3]<-.25
      duo_like[9,3]<-.5
      duo_like <- duo_like %>% unite('duo_combo', potential_father:offspring_interest, sep = '_')

      considered_loci<- possible_loci<-impossible_loci<-likelihood_loci<-vector(mode = "numeric", length = nrow(dads_loci))

      if (input$mothers_data == 2){
        data_mothers<-read.csv(input$mother_infile3$datapath, header = FALSE)
        mothers <- as.matrix(data_mothers)
        mothers<-replace(mothers, !(mothers %in% c(0, 1, 2)), NA)
        mothers_loci<- t(as.matrix(mothers[keep_allele]))
        
        for (a in 1:nrow(dads_loci)){ #Iterates through all potential fathers
          kid<-rep(offspring_loci)# Offsprings genotype matrix
          mum<-rep(mothers_loci)#mothers genotype matrix
          keep<-which((!is.na(dads_loci[a,])) & (!is.na(mum))& (!is.na(kid)))
          focal_dads<-as.matrix(dads[a,keep])
          #focal_dads_matrix<-as.matrix(focal_dads)
          focal_kid<-as.matrix(kid[keep])
          #focal_kid_matrix<-as.matrix(focal_kid)
          focal_mum<-as.matrix(mum[keep])
          
          m1 <- t(cbind(focal_dads, focal_kid, focal_mum))
          combine <-matrix(NA, nrow = 1, ncol = (dim(m1))[2])
          matched_genotype <- matrix(NA, nrow = 1, ncol = (dim(m1))[2])
          prob_loci <- matrix(NA, nrow = 1, ncol = ncol(m1))
          
          for (b in 1:ncol(m1)){
            combine[,b]<-paste(c(m1[,b]), collapse = '_')      
            matched_genotype[,b] <- match(combine[,b], trio_like[,1])
            prob_loci[,b]<- trio_like[matched_genotype[[1,b]],2]
            prob_loci[prob_loci==0]<-0.01
          }
          
          considered_loci[a]<-length(keep)
          impossible_loci[a]<-sum(prob_loci==0.01)
          possible_loci[a]<-sum(prob_loci!=0.01)
          likelihood_loci[a]<-prod(prob_loci, na.rm = FALSE)
        }
        
      }
      if (input$mothers_data == 1){
        for (a in 1:nrow(dads_loci)){ #Iterates through all potential fathers
          kid<-rep(offspring)# Offsprings genotype matrix
          keep<-which((!is.na(dads_loci[a,])) & (!is.na(kid)))
          focal_dads<-as.matrix(dads_loci[a,keep])
          focal_kid<-as.matrix(kid[keep])
          
          
          m1 <- t(cbind(focal_dads, focal_kid))
          combine <-matrix(NA, nrow = 1, ncol = (dim(m1))[2])
          matched_genotype <- matrix(NA, nrow = 1, ncol = (dim(m1))[2])
          prob_loci <- matrix(NA, nrow = 1, ncol = ncol(m1))
          
          for (b in 1:ncol(m1)){
            combine[,b]<-paste(c(m1[,b]), collapse = '_')      
            matched_genotype[,b] <- match(combine[,b], duo_like[,1])
            prob_loci[,b]<- duo_like[matched_genotype[[1,b]],2]
            prob_loci[prob_loci==0]<-0.01
          }
          
          considered_loci[a]<-length(keep)
          impossible_loci[a]<-sum(prob_loci==0.01)
          possible_loci[a]<-sum(prob_loci!=0.01)
          likelihood_loci[a]<-prod(prob_loci, na.rm = FALSE)
        }
        
      }
      
      data_considered_loci<-data.frame(considered_loci)
      data_considered_loci["Father's Position"]<- paste("Father", rownames(data_considered_loci), sep = "_")
      data_considered_loci<- data_considered_loci[, c("Father's Position", 'considered_loci')]
      
      data_impossible_loci<-data.frame(impossible_loci)
      data_impossible_loci["Father's Position"]<- paste("Father", rownames(data_impossible_loci), sep = "_")
      data_impossible_loci<- data_impossible_loci[, c("Father's Position", 'impossible_loci')]
      
      data_possible_loci<-data.frame(possible_loci)
      data_possible_loci["Father's Position"]<- paste("Father", rownames(data_possible_loci), sep = "_")
      data_possible_loci<- data_possible_loci[, c("Father's Position", 'possible_loci')]
      
      lod <-vector("numeric", length = length(likelihood_loci))
      for (g in 1:length(likelihood_loci)){ #divides 
        lod[g]<- log(likelihood_loci[g]/hardy_likelihood)
      }
      data_lod<- data.frame(lod)
      data_lod["Father's Position"]<- paste("Father", rownames(data_lod), sep = "_")
      data_lod<- data_lod[, c("Father's Position", 'lod')]
      data_lodscore<-cbind(names_dads, data_lod)
      output$table<-renderTable(data_lodscore[order(-lod),])
      
      lod[is.infinite(lod)]=0
      data_lod<- data.frame(lod)
      row.names(data_lod)<- paste("Father", rownames(data_lod), sep = "_")
      output$plot<-renderPlot(barplot(t(as.matrix(data_lod)), beside = TRUE, xlab = 'LOD Scores', ylab = 'Potential Fathers', main = 'A Representation of LOD Scores against POtential Fathers', horiz = T, col = '#69b3a2'))
      complete_data<-cbind(data_lodscore, data_considered_loci$considered_loci, data_possible_loci$possible_loci, data_impossible_loci$impossible_loci, data_NAs$NAs_loci)
      
      colnames(complete_data)<- c('Real Names',"Father's Position", "Lod Scores", 'Total loci', 'Compatible genotypes', 'Incompatible genotypes', 'Number of NAs')
      output$summary_table<-renderTable(complete_data[order(-lod),])
      
      sorted_complete_data<- complete_data[order(-lod),]
      to_plot <- pivot_longer(sorted_complete_data, cols = c('Compatible genotypes', 'Incompatible genotypes', 'Number of NAs'), names_to = "Genotypes", values_to = "Count")
      output$summary_chart<-renderPlot(ggplot(to_plot, aes(x = `Real Names`, y = Count, fill = Genotypes)) + geom_bar(position="dodge", stat="identity"))

    })
  }
