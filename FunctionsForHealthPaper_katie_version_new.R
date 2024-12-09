###Population generation

# popsize, ncomms = number communities, propbelA= prop of pop in belief A

require(boot)

g_pop_gen  <-function(pop_size=2000,ncomms=10,prop_belA=0.5,prop_old=0.3,prop_young=0.5, prop_child=0.2,g_id_1=0.25,g_id_2=0.25,
                      g_id_3=0.25,g_id_def=0.25){
  
  g_pop<-pop_size 
  n_group<-4
  
  #Work out number of people of each political belief in each age category (CODE WITH GROUP ID)
  group_soc_infl<-c(g_id_1,g_id_2,g_id_3,g_id_def)
  n_Ac<-c(rep(0,n_group))
  n_Ay<-c(rep(0,n_group))
  n_Ao<-c(rep(0,n_group))
  n_Bc<-c(rep(0,n_group))
  n_By<-c(rep(0,n_group))
  n_Bo<-c(rep(0,n_group))
  
  for (g in c(1,2,3,4)){
  n_Ac[g]  <- as.integer(round(prop_child*group_soc_infl[g]*prop_belA*g_pop))
  n_Ay[g]   <-as.integer(round(prop_young*group_soc_infl[g]*prop_belA*g_pop))
  n_Ao[g]   <-as.integer(round(prop_old*group_soc_infl[g]*prop_belA*g_pop))
  n_Bc[g]   <-as.integer(round(prop_child*group_soc_infl[g]*(1-prop_belA)*g_pop))
  n_By[g]   <-as.integer(round(prop_young*group_soc_infl[g]*(1-prop_belA)*g_pop))
  n_Bo[g]   <-as.integer(round(prop_old*group_soc_infl[g]*(1-prop_belA)*g_pop))
 }
  
  #recalculate population size in case it has changed slightly (due to rounding) (CODE WITH GROUPD ID)
  g_pop  <-sum(n_Ac[1:4]+n_Ay[1:4]+n_Ao[1:4]+n_Bc[1:4]+n_By[1:4]+n_Bo[1:4])
  
  #Vector to identify each individual's community membership (WITH GROUPD id)
  g_comms <-rep(1:ncomms,g_pop/ncomms)
  
  #Create useful vectors used in the network generation and models

  #Information on political belief of each individual (WITH GROUP ID)
  
  groupsum<-matrix(0,nrow=1,ncol=4)
  AorB<-c(rep(0,g_pop_info$g_pop))
  child<-c(rep(0,g_pop_info$g_pop))
  adult<-c(rep(0,g_pop_info$g_pop))
  old<-c(rep(0,g_pop_info$g_pop))
  
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    
    if (g==1){
      AorB[1:groupsum[g]]  <-c(rep("A",n_Ac[[g]]),rep("B",n_Bc[[g]]),rep("A",n_Ay[[g]]),rep("B",n_By[[g]]),rep("A",n_Ao[[g]]),rep("B",n_Bo[[g]]))
      child[1:groupsum[g]] <-c(rep(1,n_Ac[[g]]+n_Bc[[g]]),rep(0,n_Ay[[g]]+n_By[[g]]+n_Ao[[g]]+n_Bo[[g]]))   #Indicator of child or not
      adult[1:groupsum[g]] <-c(rep(0,n_Ac[[g]]+n_Bc[[g]]),rep(1,n_Ay[[g]]+n_By[[g]]+n_Ao[[g]]+n_Bo[[g]]))   #Indicator of adult or not
      old[1:groupsum[g]]   <-c(rep(0,n_Ac[[g]]+n_Bc[[g]]+n_Ay[[g]]+n_By[[g]]),rep(1,n_Ao[[g]]+n_Bo[[g]]))   #Indicator if old or not
      
      }
    if (g==2){
      groupsumtemp<-groupsum[1]
      AorB[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])]  <-c(rep("A",n_Ac[[g]]),rep("B",n_Bc[[g]]),rep("A",n_Ay[[g]]),rep("B",n_By[[g]]),rep("A",n_Ao[[g]]),rep("B",n_Bo[[g]]))
      child[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])] <-c(rep(1,n_Ac[[g]]+n_Bc[[g]]),rep(0,n_Ay[[g]]+n_By[[g]]+n_Ao[[g]]+n_Bo[[g]]))   #Indicator of child or not
      adult[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])] <-c(rep(0,n_Ac[[g]]+n_Bc[[g]]),rep(1,n_Ay[[g]]+n_By[[g]]+n_Ao[[g]]+n_Bo[[g]]))   #Indicator of adult or not
      old[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])]   <-c(rep(0,n_Ac[[g]]+n_Bc[[g]]+n_Ay[[g]]+n_By[[g]]),rep(1,n_Ao[[g]]+n_Bo[[g]]))   #Indicator if old or not
      
      }
    if (g==3){
      groupsumtemp<-groupsum[g-1]+groupsum[g-2]
      AorB[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])]  <-c(rep("A",n_Ac[[g]]),rep("B",n_Bc[[g]]),rep("A",n_Ay[[g]]),rep("B",n_By[[g]]),rep("A",n_Ao[[g]]),rep("B",n_Bo[[g]]))
      child[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])] <-c(rep(1,n_Ac[[g]]+n_Bc[[g]]),rep(0,n_Ay[[g]]+n_By[[g]]+n_Ao[[g]]+n_Bo[[g]]))   #Indicator of child or not
      adult[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])] <-c(rep(0,n_Ac[[g]]+n_Bc[[g]]),rep(1,n_Ay[[g]]+n_By[[g]]+n_Ao[[g]]+n_Bo[[g]]))   #Indicator of adult or not
      old[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])]   <-c(rep(0,n_Ac[[g]]+n_Bc[[g]]+n_Ay[[g]]+n_By[[g]]),rep(1,n_Ao[[g]]+n_Bo[[g]]))   #Indicator if old or not
      
      }
    if (g==4){
      groupsumtemp<-groupsum[g-1]+groupsum[g-2]+groupsum[g-3]
      AorB[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])]  <-c(rep("A",n_Ac[[g]]),rep("B",n_Bc[[g]]),rep("A",n_Ay[[g]]),rep("B",n_By[[g]]),rep("A",n_Ao[[g]]),rep("B",n_Bo[[g]]))
      child[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])] <-c(rep(1,n_Ac[[g]]+n_Bc[[g]]),rep(0,n_Ay[[g]]+n_By[[g]]+n_Ao[[g]]+n_Bo[[g]]))   #Indicator of child or not
      adult[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])] <-c(rep(0,n_Ac[[g]]+n_Bc[[g]]),rep(1,n_Ay[[g]]+n_By[[g]]+n_Ao[[g]]+n_Bo[[g]]))   #Indicator of adult or not
      old[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])]   <-c(rep(0,n_Ac[[g]]+n_Bc[[g]]+n_Ay[[g]]+n_By[[g]]),rep(1,n_Ao[[g]]+n_Bo[[g]]))   #Indicator if old or not
      
      }
    #belief <- rbind(belief, data.frame(belief=belief1))
  }
  
  g_pop_info <- list(g_pop,g_comms,n_Ac,n_Bc,n_Ay,n_By,n_Ao,n_Bo,AorB,child,adult,old)

    names(g_pop_info)<-c("g_pop","comms","n_Ac","n_Bc","n_Ay","n_By","n_Ao","n_Bo",
                       "AorB","child","adult","old")

  return(g_pop_info)
  
}
  
##################################################
##################################################

##Parent Exchange

par_ex<-function(g_pop_info,parents,dis_mat){

#creating the empty matrices

Ap1<-rep(0,sum(g_pop_info$n_Ac))
Ap2<-rep(0,sum(g_pop_info$n_Ac))
Bp1<-rep(0,sum(g_pop_info$n_Bc))
Bp2<-rep(0,sum(g_pop_info$n_Bc))
  
Acs<-rep(0,sum(g_pop_info$n_Ac))
Bcs<-rep(0,sum(g_pop_info$n_Bc))

for (g in c(1,2,3,4)){
  if (g==1){
 Ap1[1:g_pop_info$n_Ac[[g]]]  <-parents[1:g_pop_info$n_Ac[[g]],1]
 Ap2[1:g_pop_info$n_Ac[[g]]]  <-parents[1:g_pop_info$n_Ac[[g]],2]
 Bp1[1:g_pop_info$n_Bc[[g]]]  <-parents[(sum(g_pop_info$n_Ac[[g]])+1):(sum(g_pop_info$n_Ac[[g]])+g_pop_info$n_Bc[[g]]),1]
 Bp2[1:g_pop_info$n_Bc[[g]]]  <-parents[(sum(g_pop_info$n_Ac[[g]])+1):(sum(g_pop_info$n_Ac[[g]])+g_pop_info$n_Bc[[g]]),2]
 
  }
   if (g==2) {
    A1<- g_pop_info$n_Ac[[g-1]]+1
    A2<- g_pop_info$n_Ac[[g]]+ g_pop_info$n_Ac[[g-1]]
    templagA2 <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+1
    tempA2    <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g]]

    Ap1[A1:A2]  <-parents[templagA2:tempA2,1]
    Ap2[A1:A2]  <-parents[templagA2:tempA2,2]

    B1<- g_pop_info$n_Bc[[g-1]]+1
    B2<- g_pop_info$n_Bc[[g]]+ g_pop_info$n_Bc[[g-1]]
    templagB2  <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g]]+1
    tempB2     <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
    
    Bp1[B1:B2]  <-parents[templagB2:tempB2,1]
    Bp2[B1:B2]  <-parents[templagB2:tempB2,2]

   }
  if (g==3) {
    A2<- g_pop_info$n_Ac[[g-2]]+ g_pop_info$n_Ac[[g-1]]+1
    A3<- g_pop_info$n_Ac[[g]]+ g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Ac[[g-2]]
    
    templagA3  <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g-2]]+g_pop_info$n_Bc[[g-2]]+1
    tempA3     <-g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g-2]]+g_pop_info$n_Bc[[g-2]]

    Ap1[A2:A3]  <-parents[templagA3:tempA3,1]
    Ap2[A2:A3]  <-parents[templagA3:tempA3,2]

    B2<- g_pop_info$n_Bc[[g-2]]+ g_pop_info$n_Bc[[g-1]]+1
    B3<- g_pop_info$n_Bc[[g]]+ g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Bc[[g-2]]
    
    templagB3  <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g-2]]+g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Ac[[g]]+1
    tempB3     <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g-2]]+g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
    
    Bp1[B2:B3]  <-parents[templagB3:tempB3,1]
    Bp2[B2:B3]  <-parents[templagB3:tempB3,2]
  }
  if (g==4) {
    A3<- g_pop_info$n_Ac[[g-3]]+g_pop_info$n_Ac[[g-2]]+ g_pop_info$n_Ac[[g-1]]+1
    A4<- g_pop_info$n_Ac[[g]]+ g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Ac[[g-2]]+g_pop_info$n_Ac[[g-3]]
    
    templagA4   <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g-2]]+g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Ac[[g-3]]+g_pop_info$n_Bc[[g-3]]+1
    tempA4      <-g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g-2]]+g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Ac[[g-3]]+g_pop_info$n_Bc[[g-3]]

    Ap1[A3:A4]  <-parents[templagA4:tempA4,1]
    Ap2[A3:A4]  <-parents[templagA4:tempA4,2]

    B3<- g_pop_info$n_Bc[[g-3]]+g_pop_info$n_Bc[[g-2]]+ g_pop_info$n_Bc[[g-1]]+1
    B4<- g_pop_info$n_Bc[[g]]+ g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Bc[[g-3]]
    
    templagB4   <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g-2]]+g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Ac[[g-3]]+g_pop_info$n_Bc[[g-3]]+g_pop_info$n_Ac[[g]]+1
    tempB4      <-g_pop_info$n_Ac[[g-1]]+g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Ac[[g-2]]+g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Ac[[g-3]]+g_pop_info$n_Bc[[g-3]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
    
    Bp1[B3:B4]  <-parents[templagB4:tempB4,1]
    Bp2[B3:B4]  <-parents[templagB4:tempB4,2]
  }
}

groupsum<-matrix(0,nrow=1,ncol=4)
 for (g in c(1,2,3,4)){
   groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
   if (g==1){
  dis_mat[(1:(sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]]))),(sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1)):groupsum[g]]<-
  dis_mat[(sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1)):groupsum[g],(1:(sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])))]<-0
   }
   if (g==2){
     dis_mat[(groupsum[g-1]:(sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]]))),(sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1)):sum(groupsum[g-1],groupsum[g])]<-
       dis_mat[(sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1)):sum(groupsum[g-1],groupsum[g]),(groupsum[g-1]:(sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])))]<-0
   }
   if (g==3){
     dis_mat[(sum(groupsum[g-1],groupsum[g-2]):(sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]]))),(sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1)):sum(groupsum[g-1],groupsum[g-2],groupsum[g])]<-
       dis_mat[(sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1)):sum(groupsum[g-1],groupsum[g-2],groupsum[g]),(sum(groupsum[g-1],groupsum[g-2]):(sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])))]<-0
   }
   if (g==4){
     dis_mat[(sum(groupsum[g-1],groupsum[g-2],groupsum[g-3]):(sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]]))),(sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1)):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g])]<-
       dis_mat[(sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1)):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g]),(sum(groupsum[g-1],groupsum[g-2],groupsum[g-3]):(sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])))]<-0
   }
 }

Acs  <-seq(1,sum(g_pop_info$n_Ac),1)
Bcs  <-seq(1,sum(g_pop_info$n_Bc),1)

#We now fill in the child-parent edges into the new supra-adjacency matrix for both political beliefs

for (g in c(1,2,3,4)){
  groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
  
  if (g==1){
    tempA1<-g_pop_info$n_Ac[[g]]
    tempB1<-g_pop_info$n_Bc[[g]]
    tempAy1<-g_pop_info$n_Ay[[g]]
    
    for(i in 1:tempA1){
    dis_mat[Acs[i],Ap1[i]+tempA1+tempB1] <-dis_mat[Ap1[i]+tempA1+tempB1,Acs[i]]<-1
    dis_mat[Acs[i],Ap2[i]+tempA1+tempB1] <-dis_mat[Ap2[i]+tempA1+tempB1,Acs[i]]<-1
    }
    
    for(i in 1:tempB1){
      dis_mat[tempA1+Bcs[i],Bp1[i]+tempAy1+tempA1+tempB1]<-dis_mat[Bp1[i]+tempAy1+tempA1+tempB1,tempA1+Bcs[i]]<-1
      dis_mat[tempA1+Bcs[i],Bp2[i]+tempAy1+tempA1+tempB1]<-dis_mat[Bp2[i]+tempAy1+tempA1+tempB1,tempA1+Bcs[i]]<-1
    }
    
  }
  if (g==2){
    tempA1<-g_pop_info$n_Ac[[g-1]]
    tempB1<-g_pop_info$n_Bc[[g-1]]
    tempA2<-g_pop_info$n_Ac[[g]]
    tempB2<-g_pop_info$n_Bc[[g]]
    tempAy2<-g_pop_info$n_Ay[[g]]
    
    for(i in (tempA1+1):(tempA1+tempA2)){
    dis_mat[Acs[i],Ap1[i]+groupsum[g-1]+tempA2+tempB2] <-dis_mat[Ap1[i]+groupsum[g-1]+tempA2+tempB2,Acs[i]]<-1
    dis_mat[Acs[i],Ap2[i]+groupsum[g-1]+tempA2+tempB2] <-dis_mat[Ap2[i]+groupsum[g-1]+tempA2+tempB2,Acs[i]]<-1
    }
    
    B1<- g_pop_info$n_Bc[[g-1]]+1
    B2<- g_pop_info$n_Bc[[g]]+ g_pop_info$n_Bc[[g-1]]
    
    for(i in (tempB1+1):(tempB1+tempB2)){
      dis_mat[groupsum[g-1]+tempA2+Bcs[i],Bp1[i]+groupsum[g-1]+tempAy2+tempA2+tempB2]<-dis_mat[Bp1[i]+groupsum[g-1]+tempAy2+tempA2+tempB2,tempA2+groupsum[g-1]+Bcs[i]]<-1
      dis_mat[groupsum[g-1]+tempA2+Bcs[i],Bp2[i]+groupsum[g-1]+tempAy2+tempA2+tempB2]<-dis_mat[Bp2[i]+groupsum[g-1]+tempAy2+tempA2+tempB2,tempA2+groupsum[g-1]+Bcs[i]]<-1
    }
    
  }
  if (g==3){
    tempA1<-g_pop_info$n_Ac[[g-2]]
    tempB1<-g_pop_info$n_Bc[[g-2]]
    tempA2<-g_pop_info$n_Ac[[g-1]]
    tempB2<-g_pop_info$n_Bc[[g-1]]
    tempA3<-g_pop_info$n_Ac[[g]]
    tempB3<-g_pop_info$n_Bc[[g]]
    tempAy3<-g_pop_info$n_Ay[[g]]
    temp3<-groupsum[g-1]+groupsum[g-2]
    
  
    for(i in (tempA1+tempA2+1):(tempA1+tempA2+tempA3)){
      dis_mat[Acs[i],Ap1[i]+groupsum[g-2]+tempA3+tempB3] <-dis_mat[Ap1[i]+groupsum[g-2]+tempA3+tempB3,Acs[i]]<-1
      dis_mat[Acs[i],Ap2[i]+groupsum[g-2]+tempA3+tempB3] <-dis_mat[Ap2[i]+groupsum[g-2]+tempA3+tempB3,Acs[i]]<-1
    }
    
    B2<- g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Bc[[g-1]]+1
    B3<- g_pop_info$n_Bc[[g]]+ g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Bc[[g-2]]
    
    for(i in (tempB1+tempB2+1):(tempB1+tempB2+tempB3)){
      dis_mat[groupsum[g-2]+tempA3+Bcs[i],Bp1[i]+groupsum[g-2]+tempAy3+tempA3+tempB3]<-dis_mat[Bp1[i]+groupsum[g-2]+tempAy3+tempA3+tempB3,groupsum[g-2]+tempA3+Bcs[i]]<-1
      dis_mat[groupsum[g-2]+tempA3+Bcs[i],Bp2[i]+groupsum[g-2]+tempAy3+tempA3+tempB3]<-dis_mat[Bp2[i]+groupsum[g-2]+tempAy3+tempA3+tempB3,groupsum[g-2]+tempA3+Bcs[i]]<-1
    }
    
  }
  if (g==4){
    tempA1<-g_pop_info$n_Ac[[g-3]]
    tempB1<-g_pop_info$n_Bc[[g-3]]
    tempA2<-g_pop_info$n_Ac[[g-2]]
    tempB2<-g_pop_info$n_Bc[[g-2]]
    tempA3<-g_pop_info$n_Ac[[g-1]]
    tempB3<-g_pop_info$n_Bc[[g-1]]    
    tempA4<-g_pop_info$n_Ac[[g]]
    tempB4<-g_pop_info$n_Bc[[g]]
    tempAy4<-g_pop_info$n_Ay[[g]]
    temp4<-groupsum[g-1]+groupsum[g-2]+groupsum[g-3]
    
    for(i in (tempA1+tempA2+tempA3+1):(tempA1+tempA2+tempA3+tempA4)){
      dis_mat[Acs[i],Ap1[i]+groupsum[g-3]+tempA4+tempB4] <-dis_mat[Ap1[i]+groupsum[g-3]+tempA4+tempB4,Acs[i]]<-1
      dis_mat[Acs[i],Ap2[i]+groupsum[g-3]+tempA4+tempB4] <-dis_mat[Ap2[i]+groupsum[g-3]+tempA4+tempB4,Acs[i]]<-1
    }
    
    B3<- g_pop_info$n_Bc[[g-3]]+g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Bc[[g-1]]+1
    B4<- g_pop_info$n_Bc[[g]]+ g_pop_info$n_Bc[[g-1]]+g_pop_info$n_Bc[[g-2]]+g_pop_info$n_Bc[[g-3]]
    
    for(i in (tempB1+tempB2+tempB3+1):(tempB1+tempB2+tempB3+tempB4)){
      dis_mat[groupsum[g-3]+tempA4+Bcs[i],Bp1[i]+groupsum[g-3]+tempAy4+tempA4+tempB4]<-dis_mat[Bp1[i]+groupsum[g-3]+tempAy4+tempA4+tempB4,tempA4+Bcs[i]]<-1
      dis_mat[groupsum[g-3]+tempA4+Bcs[i],Bp2[i]+groupsum[g-3]+tempAy4+tempA4+tempB4]<-dis_mat[Bp2[i]+groupsum[g-3]+tempAy4+tempA4+tempB4,tempA4+Bcs[i]]<-1
    }
    
    }
}

    #Add connections from children to the same old adults as their parents
 #For political belief A
  groupsum<-matrix(0,nrow=1,ncol=4)
  
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    
    if (g==1){
    for(i in 1:length(g_pop_info$n_Ac[[g]])){
        t_ycon     <-which(dis_mat[i,(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]])]==1)
        t_ycon2    <-t_ycon+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
        for(j in 1:length(t_ycon2)){
          t_ycon3  <-which(dis_mat[t_ycon2[j],(groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]]+1):groupsum[g]]==1)
          t_ycon4  <-sort(t_ycon3+groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]])
          dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
        }
    }
    }
      if (g==2){
        for(i in groupsum[g-1]+1:sum(groupsum[g-1],g_pop_info$n_Ac[[g]])){
        t_ycon     <-which(dis_mat[i,(sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1)):sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],g_pop_info$n_Ay[[g]])]==1)
        t_ycon2    <-t_ycon+groupsum[g-1]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
        for(j in 1:length(t_ycon2)){
          t_ycon3  <-which(dis_mat[t_ycon2[j],(sum(groupsum[g-1],groupsum[g])-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]]+1):(sum(groupsum[g-1],groupsum[g]))]==1)
          t_ycon4  <-sort(t_ycon3+groupsum[g-1]+groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]])
          dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
          }      
        }
      }
      if (g==3){
        for(i in sum(groupsum[g-2],groupsum[g-1],1):sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]])){
        t_ycon     <-which(dis_mat[i,sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],g_pop_info$n_Ay[[g]])]==1)
        t_ycon2    <-t_ycon+groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
        for(j in 1:length(t_ycon2)){
          t_ycon3  <-which(dis_mat[t_ycon2[j],(sum(groupsum[g-1],groupsum[g-2],groupsum[g])-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]]+1):sum(groupsum[g-1],groupsum[g-2],groupsum[g])]==1)
          t_ycon4  <-sort(t_ycon3+groupsum[g-1]+groupsum[g-2]+groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]])
          dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
          }  
        }
      }
      if (g==4){
        for(i in sum(groupsum[g-3],groupsum[g-2],groupsum[g-1],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]])){
        t_ycon     <-which(dis_mat[i,sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],g_pop_info$n_Ay[[g]])]==1)
        t_ycon2    <-t_ycon+groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
        for(j in 1:length(t_ycon2)){
          t_ycon3  <-which(dis_mat[t_ycon2[j],(sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g])-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]]+1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g])]==1)
          t_ycon4  <-sort(t_ycon3+groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]])
          dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
        } 
        } 
      } 
    }

#For political belief B
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    if (g==1){
    for(i in (g_pop_info$n_Ac[[g]]+1):(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])){
        t_ycon<-which(dis_mat[i,(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+1):(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
                                                                                                         +g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]])]==1)
        t_ycon2<- t_ycon+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]
        for(j in 1:length(t_ycon2)){
          t_ycon3  <-which(dis_mat[t_ycon2[j],(groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]]+1):(groupsum[g])]==1)
          t_ycon4  <-sort(t_ycon3+groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]])
          dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
        }
    }
    }
      if (g==2){
        for(i in sum(groupsum[g-1],g_pop_info$n_Ac[[g]],1):sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])){
        t_ycon<-which(dis_mat[i,(groupsum[g-1]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+1):(groupsum[g-1]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
                                                                                                    +g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]])]==1)
        t_ycon2<- t_ycon+groupsum[g-1]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]
        for(j in 1:length(t_ycon2)){
          t_ycon3  <-which(dis_mat[t_ycon2[j],(sum(groupsum[g-1],groupsum[g])-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]]+1):sum(groupsum[g-1],groupsum[g])]==1)
          t_ycon4  <-sort(t_ycon3+groupsum[g-1]+groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]])
          dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
        }
        }
      }
      if (g==3){
        for(i in sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],1):sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])){
        t_ycon<-which(dis_mat[i,(groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+1):(groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
                                                                                                                 +g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]])]==1)
        t_ycon2<- t_ycon+groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]
        for(j in 1:length(t_ycon2)){
          t_ycon3  <-which(dis_mat[t_ycon2[j],(sum(groupsum[g-1],groupsum[g-2],groupsum[g])-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]]+1):sum(groupsum[g-1],groupsum[g-2],groupsum[g])]==1)
          t_ycon4  <-sort(t_ycon3+groupsum[g-1]+groupsum[g-2]+groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]])
          dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
        }
        }
      }
      if (g==4){
        for(i in sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])){
        t_ycon<-which(dis_mat[i,(groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+1):(groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]
                                                                                                                 +g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]])]==1)
        t_ycon2<-t_ycon+groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]
        for(j in 1:length(t_ycon2)){
          t_ycon3  <-which(dis_mat[t_ycon2[j],(sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g])-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]]+1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g])]==1)
          t_ycon4  <-sort(t_ycon3+groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+groupsum[g]-g_pop_info$n_Ao[[g]]-g_pop_info$n_Bo[[g]])
          dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1    
      } 
    }
  }
}
 return(dis_mat)

} #end function

###############################################
###############################################

concern_setup<-function(A_concern_y,B_concern_y,A_concern_o,B_concern_o,g_pop_info,mat_cyo,g_con_1,g_con_2,g_con_3,g_con_def){
  
  #Calculate versions of initial belief on logit scale (ORIGINAL CODE WITHOUT GROUP)
  ## options for group id initial concern
  # additive to individual initial belief (ADDITIVE CONCERN WITH GROUP ID)
  
  group_con<-c(g_con_1,g_con_2,g_con_3,g_con_def)
  
  lA_concern_y <-vector()
  lB_concern_y <-vector()
  lA_concern_o <-vector()
  lB_concern_o <-vector()
  

  for (g in 1:length(group_con)){
  lA_concern_y[g]  <-logit(A_concern_y+group_con[g])
  lB_concern_y[g]  <-logit(B_concern_y+group_con[g])
  lA_concern_o[g]  <-logit(A_concern_y+A_concern_o+group_con[g])
  lB_concern_o[g]  <-logit(B_concern_y+B_concern_o+group_con[g])
  }
  
  #Calculate starting beliefs of each person depending on the parameters defined above
  groupsum<-matrix(0,nrow=1,ncol=4)
  belief<-c(rep(NA,2000))
for (g in c(1,2,3,4)){
  groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
  
  if (g==1){
    belief[1:groupsum[g]]  <-c(rep(NA,g_pop_info$n_Ac[[g]]),rep(NA,g_pop_info$n_Bc[[g]]),rep(lA_concern_y[[g]],g_pop_info$n_Ay[[g]]),
                                     rep(lB_concern_y[[g]],g_pop_info$n_By[[g]]),rep(lA_concern_o[[g]],g_pop_info$n_Ao[[g]]),rep(lB_concern_o[[g]],g_pop_info$n_Bo[[g]]))
    
  }
  if (g==2){
    belief[sum(groupsum[g-1],1):sum(groupsum[g-1],groupsum[g])] <-c(rep(NA,g_pop_info$n_Ac[[g]]),rep(NA,g_pop_info$n_Bc[[g]]),rep(lA_concern_y[[g]],g_pop_info$n_Ay[[g]]),
                                                                  rep(lB_concern_y[[g]],g_pop_info$n_By[[g]]),rep(lA_concern_o[[g]],g_pop_info$n_Ao[[g]]),rep(lB_concern_o[[g]],g_pop_info$n_Bo[[g]]))
  }
  if (g==3){
    belief[sum(groupsum[g-1],groupsum[g-2],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g])] <-c(rep(NA,g_pop_info$n_Ac[[g]]),rep(NA,g_pop_info$n_Bc[[g]]),rep(lA_concern_y[[g]],g_pop_info$n_Ay[[g]]),
                                                                  rep(lB_concern_y[[g]],g_pop_info$n_By[[g]]),rep(lA_concern_o[[g]],g_pop_info$n_Ao[[g]]),rep(lB_concern_o[[g]],g_pop_info$n_Bo[[g]]))
  }
  if (g==4){
    belief[sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g])] <-c(rep(NA,g_pop_info$n_Ac[[g]]),rep(NA,g_pop_info$n_Bc[[g]]),rep(lA_concern_y[[g]],g_pop_info$n_Ay[[g]]),
                                                                  rep(lB_concern_y[[g]],g_pop_info$n_By[[g]]),rep(lA_concern_o[[g]],g_pop_info$n_Ao[[g]]),rep(lB_concern_o[[g]],g_pop_info$n_Bo[[g]]))
}
  #belief <- rbind(belief, data.frame(belief=belief1))
}
  #Use these starting beliefs to calculate an initial indication of concern (using a bernoulli draw)
groupsum<-matrix(0,nrow=1,ncol=4)
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    if (g==1){
  initial_conc[1:groupsum[g]]  <-c(rep(NA,sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])),
                                   rbinom((groupsum[g]-g_pop_info$n_Ac[[g]]-g_pop_info$n_Bc[[g]]),1,inv.logit(belief[((g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):groupsum[g])])))
    }
  if (g==2){
    groupsumtemp<-groupsum[g-1]
    initial_conc[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])] <-c(rep(NA,sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])),
                                                                        rbinom(groupsum[g]-g_pop_info$n_Ac[[g]]-g_pop_info$n_Bc[[g]],1,inv.logit(belief[sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsumtemp,groupsum[g])])))
  }
    if (g==3){
      groupsumtemp<-groupsum[g-1]+groupsum[g-2]
      initial_conc[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])] <-c(rep(NA,sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])),
                                                                          rbinom(groupsum[g]-g_pop_info$n_Ac[[g]]-g_pop_info$n_Bc[[g]],1,inv.logit(belief[sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsumtemp,groupsum[g])])))
    }
  if (g==4){
    groupsumtemp<-groupsum[g-1]+groupsum[g-2]+groupsum[g-3]
    initial_conc[sum(groupsumtemp,1):sum(groupsumtemp,groupsum[g])] <-c(rep(NA,sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])),rbinom(groupsum[g]-g_pop_info$n_Ac[[g]]-g_pop_info$n_Bc[[g]],1,inv.logit(belief[sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsumtemp,groupsum[g])])))
  }
  }
initial_conc
group1mean<-mean(initial_conc[sum(g_pop_info$n_Ac[[1]],g_pop_info$n_Bc[[1]]+1):groupsum[1]])
group2mean<-mean(initial_conc[sum(groupsum[1],g_pop_info$n_Ac[[2]],g_pop_info$n_Bc[[2]]+1):sum(groupsum[1],groupsum[2])])
group3mean<-mean(initial_conc[sum(groupsum[1],groupsum[2],g_pop_info$n_Ac[[3]],g_pop_info$n_Bc[[3]]+1):sum(groupsum[1],groupsum[2],groupsum[3])])
groupDmean<-mean(initial_conc[sum(groupsum[1],groupsum[2],groupsum[3],g_pop_info$n_Ac[[4]],g_pop_info$n_Bc[[4]]+1):sum(groupsum[1],groupsum[2],groupsum[3],groupsum[4])])

  #Here we set children as taking on the concern-level of their most concerned parent
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    
    if (g==1){
  for(i in 1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])){
    t_par       <-which(mat_cyo[i,(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]])]==1)
    initial_conc[i] <-max(initial_conc[t_par+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
    #print(t_par)
  }
    }
    
    if (g==2){
      for(i in sum(groupsum[1],1):sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])){
        t_par       <-which(mat_cyo[i,sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],g_pop_info$n_Ay[[g]],g_pop_info$n_By[[g]])]==1)
        initial_conc[i] <-max(initial_conc[t_par+groupsum[1]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
      }
    }
    if (g==3){
      for(i in sum(groupsum[g-1],groupsum[g-2],1):sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])){
        t_par       <-which(mat_cyo[i,sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],g_pop_info$n_Ay[[g]],g_pop_info$n_By[[g]])]==1)
        #print(initial_conc[t_par+groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
        initial_conc[i] <-max(initial_conc[t_par+groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
        #print(max(initial_conc[t_par+groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]]))
      }
    }
    if (g==4){
      for(i in sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])){
        t_par       <-which(mat_cyo[i,sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],g_pop_info$n_Ay[[g]],g_pop_info$n_By[[g]])]==1)
        print(initial_conc[t_par+groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
        initial_conc[i] <-max(initial_conc[t_par+groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
        print(max(initial_conc[t_par+groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]]))
        #if (initial_conc[i]==-Inf){
         # initial_conc[i] <- rbinom(1,1,0.5)
       # }
    }
    }
    }
  
  #And here is the fully calculated measure of current concern about the virus
  conc           <-initial_conc
  output         <-list(belief,conc,group1mean,group2mean,group3mean,groupDmean)
  names(output)  <-c("belief","concern","group1mean","group2mean","group3mean","groupDmean")
  return(output)

}



concern_timestep  <-function(g_pop_info,net_b,net_d,belief,concern,inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o){
  
  net1  <-net_b
  net2  <-net_d
  conc  <-concern
  
  
  #Calculate new belief scores based on response to extrinsic factors
  belief[g_pop_info$AorB=="A"]  <-belief[g_pop_info$AorB=="A"]+g_pop_info$adult[g_pop_info$AorB=="A"]*lA_ex
  belief[g_pop_info$AorB=="B"]  <-belief[g_pop_info$AorB=="B"]+g_pop_info$adult[g_pop_info$AorB=="B"]*lB_ex

  
  #Calculate changes in belief caused by concern of network connections in belief sharing network
  groupsum<-matrix(0,nrow=1,ncol=4)
  
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    if (g==1){
  for(i in (g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):(groupsum[g])){
    t_con        <-which(net1[i,]==1)
    t_con        <-t_con[t_con>(groupsum[g]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]
    
    if(length(t_con)>0){
      t_prop     <-sum(conc[t_con])/length(t_con)
      belief[i]  <-belief[i]+t_prop*l_conc+t_prop*l_conc_o*g_pop_info$old[i]
    }
  }
    }
    if (g==2){
      for(i in sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g])){
        t_con        <-which(net1[i,]==1)
        t_con        <-t_con[t_con>(groupsum[g-1]+groupsum[g]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]
        
        if(length(t_con)>0){
          t_prop     <-sum(conc[t_con])/length(t_con)
          belief[i]  <-belief[i]+t_prop*l_conc+t_prop*l_conc_o*g_pop_info$old[i]
        }
      }
    }
    if (g==3){
      for(i in sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g])){
        t_con        <-which(net1[i,]==1)
        t_con        <-t_con[t_con>(groupsum[g-1]+groupsum[g-2]+groupsum[g]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]
        
        if(length(t_con)>0){
          t_prop     <-sum(conc[t_con])/length(t_con)
          belief[i]  <-belief[i]+t_prop*l_conc+t_prop*l_conc_o*g_pop_info$old[i]
        }
      }
    }
    if (g==4){
      for(i in sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g])){
        t_con        <-which(net1[i,]==1)
        t_con        <-t_con[t_con>(groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]
        
        if(length(t_con)>0){
          t_prop     <-sum(conc[t_con])/length(t_con)
          belief[i]  <-belief[i]+t_prop*l_conc+t_prop*l_conc_o*g_pop_info$old[i]
        }
      }
    }
  }
  
 
  
  ######### SHOULD THIS BE WHERE WE CALCULATE THE SWITCH IN GROUPS? NEW BELIEF-PREVIOUS BELIEF? #######
  ############################# IF > SOME AMOUNT => UPDATE GROUP NUMBERS? #############################
  
  # #Calculate changes in belief caused infection status of group connections
  
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    if (g==1){
      for(i in (g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):(groupsum[g])){
        t_con        <-which(net1[i,]==1)
        t_con        <-t_con[t_con>(groupsum[g]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]
        
        if(length(t_con)>0){
          t_prop     <-sum(conc[t_con])/length(t_con)
          belief[i]  <-belief[i]+t_prop*g_con_1+t_prop*l_conc_o*g_pop_info$old[i]
        }
      }
    }
    if (g==2){
      for(i in sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g])){
        t_con        <-which(net1[i,]==1)
        t_con        <-t_con[t_con>(groupsum[g-1]+groupsum[g]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]
        
        if(length(t_con)>0){
          t_prop     <-sum(conc[t_con])/length(t_con)
          belief[i]  <-belief[i]+t_prop*g_con_2+t_prop*l_conc_o*g_pop_info$old[i]
        }
      }
    }
    if (g==3){
      for(i in sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g])){
        t_con        <-which(net1[i,]==1)
        t_con        <-t_con[t_con>(groupsum[g-1]+groupsum[g-2]+groupsum[g]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]
        
        if(length(t_con)>0){
          t_prop     <-sum(conc[t_con])/length(t_con)
          belief[i]  <-belief[i]+t_prop*g_con_3+t_prop*l_conc_o*g_pop_info$old[i]
        }
      }
    }
    if (g==4){
      for(i in sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g])){
        t_con        <-which(net1[i,]==1)
        t_con        <-t_con[t_con>(groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]
        
        if(length(t_con)>0){
          t_prop     <-sum(conc[t_con])/length(t_con)
          belief[i]  <-belief[i]+t_prop*g_con_def+t_prop*l_conc_o*g_pop_info$old[i]
        }
      }
    }
  }

  #Calculate changes in belief caused healthiness of all network connections
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    
    if (g==1){
      for(i in (g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):(groupsum[g])){
        t_con     <-which(net1[i,]==1)
        #t_con<-t_con[t_con>(pop_info$n_Ac+pop_info$n_Bc)]
        
        if(length(t_con)>0){
          t_inf   <-sum(inf[t_con,1])
          
          if(t_inf==0){
            belief[i]  <-belief[i]+l_hea+t_inf*l_hea_o*g_pop_info$old[i]
          }
        }
      }
    }
    if (g==2){
      for(i in sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g])){
        t_con     <-which(net1[i,]==1)
        #t_con<-t_con[t_con>(pop_info$n_Ac+pop_info$n_Bc)]
        
        if(length(t_con)>0){
          t_inf   <-sum(inf[t_con,1])
          
          if(t_inf==0){
            belief[i]  <-belief[i]+l_hea+t_inf*l_hea_o*g_pop_info$old[i]
          }
        }
      }
    }
    if (g==3){
      for(i in sum(groupsum[g-1],groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g])){
        t_con     <-which(net1[i,]==1)
        #t_con<-t_con[t_con>(pop_info$n_Ac+pop_info$n_Bc)]
        
        if(length(t_con)>0){
          t_inf   <-sum(inf[t_con,1])
          
          if(t_inf==0){
            belief[i]  <-belief[i]+l_hea+t_inf*l_hea_o*g_pop_info$old[i]
          }
        }
      }
    }
    if (g==4){
      for(i in sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g-2],groupsum[g-3],groupsum[g])){
        t_con     <-which(net1[i,]==1)
        #t_con<-t_con[t_con>(pop_info$n_Ac+pop_info$n_Bc)]
        
        if(length(t_con)>0){
          t_inf   <-sum(inf[t_con,1])
          
          if(t_inf==0){
            belief[i]  <-belief[i]+l_hea+t_inf*l_hea_o*g_pop_info$old[i]
          }
        }
      }
    }
  }
  
  #Work out new states of concern (minus children) 
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    if (g==1){
      conc[1:groupsum[g]]  <-c(rep(NA,sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])),rbinom(groupsum[g]-g_pop_info$n_Ac[[g]]-g_pop_info$n_Bc[[g]],1,inv.logit(belief[(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):groupsum[g]])))
    }
    if (g==2){
      conc[(groupsum[g-1]+1):(groupsum[g-1]+groupsum[g])] <-c(rep(NA,sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])),rbinom(groupsum[g]-g_pop_info$n_Ac[[g]]-g_pop_info$n_Bc[[g]],1,inv.logit(belief[sum(groupsum[g-1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1],groupsum[g])])))
    }
    if (g==3){
      conc[(groupsum[g-1]+groupsum[g-2]+1):(groupsum[g-1]+groupsum[g-2]+groupsum[g])] <-c(rep(NA,sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])),rbinom(groupsum[g]-g_pop_info$n_Ac[[g]]-g_pop_info$n_Bc[[g]],1,inv.logit(belief[sum(groupsum[g-1]+groupsum[g-2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1]+groupsum[g-2],groupsum[g])])))
    }
    if (g==4){
      conc[(groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+1):(groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+groupsum[g])] <-c(rep(NA,sum(g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])),rbinom(groupsum[g]-g_pop_info$n_Ac[[g]]-g_pop_info$n_Bc[[g]],1,inv.logit(belief[sum(groupsum[g-1]+groupsum[g-2]+groupsum[g-3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]],1):sum(groupsum[g-1]+groupsum[g-2]+groupsum[g-3],groupsum[g])])))
    }
  }
  
  #Add in concern of children (based on their most concerned parent)
  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    
    if (g==1){
      for(i in 1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])){
        t_par       <-which(net1[i,(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]])]==1)
        conc[i] <-max(conc[t_par+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
      }
    }
    if (g==2){
      for(i in groupsum[g-1]+1:(groupsum[g-1]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])){
        t_par       <-which(net1[i,(groupsum[g-1]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):(groupsum[g-1]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]])]==1)
        conc[i] <-max(conc[t_par+groupsum[g-1]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
        conc[696]<-1
      }
    }
    if (g==3){
      for(i in (groupsum[g-1]+groupsum[g-2]+1):(groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])){
        t_par       <-which(net1[i,(groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):(groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]])]==1)
        conc[i] <-max(conc[t_par+groupsum[g-1]+groupsum[g-2]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
      }
    }
    if (g==4){
      for(i in (groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+1):(groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])){
        t_par       <-which(net1[i,(groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+1):(groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]])]==1)
        conc[i] <-max(conc[t_par+groupsum[g-1]+groupsum[g-2]+groupsum[g-3]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]]])
        conc[1502]<-1
      }
    }
  }
  
  #Compare mean concern of political belief's A and B
  #print(aggregate(conc,list(AorB),mean))
  
  output          <-list(belief,conc)
  names(output)   <-c("belief","concern")
  
  return(output)
  
} #end of function

###############################################
###############################################


infection_timestep<-function(g_pop_info,status,net,d_exp,d_inf1,d_inf2,d_inf3,S_E,E_I1,yI1_I2,oI1_I2,yI2_I3,oI2_I3,yI3_D,oI3_D,yI1_R,oI1_R,yI2_R,oI2_R,yI3_R,oI3_R){
  
  mat_cyo   <-net
  old       <-g_pop_info$old
  child     <-g_pop_info$child
  young     <-sign(1-old-child)
  
  #this will block individuals changing twice in the same time-step
  f2c                          <-rep(1,g_pop_info$g_pop)
  inf                          <-sign(status$I1+status$I2+status$I3)
  risk                         <-rep(0,g_pop_info$g_pop)
  risk[as.numeric(names(table(which(inf*mat_cyo==1,arr.ind=TRUE)[,2])))]  <-table(which(inf*mat_cyo==1,arr.ind=TRUE)[,2])
  prob                         <-(1-S_E)^risk
  infect                       <-rbinom(g_pop_info$g_pop,1,1-prob)*status$S
  status$S[which(infect==1)]   <-0
  status$E[which(infect==1)]   <-1
  d_exp[!is.na(d_exp)&d_exp>0] <-d_exp[!is.na(d_exp)&d_exp>0]-1
  d_exp[which(infect==1)]      <-rpois(length(which(infect==1)),E_I1)
  f2c[which(infect==1)]        <-0
  EI1                          <-which(status$E==1&!is.na(d_exp)&d_exp==0)
  status$E[EI1]                <-0
  status$I1[EI1]               <-1
  f2c[EI1]                     <-0
  I1I2                         <-which(status$I1==1&!is.na(d_inf1)&d_inf1==0)
  status$I1[I1I2]              <-0
  status$I2[I1I2]              <-1
  f2c[I1I2]                    <-0
  I2I3                         <-rbinom(g_pop_info$g_pop,1,child*0+young*yI2_I3+old*oI2_I3)*status$I2*f2c
  status$I2[which(I2I3==1)]    <-0
  status$I3[which(I2I3==1)]    <-1  
  f2c[which(I2I3==1)]          <-0
  I3D                          <-rbinom(g_pop_info$g_pop,1,young*yI3_D+old*oI3_D)*status$I3*f2c
  status$I3[which(I3D==1)]     <-0
  status$D[which(I3D==1)]      <-1
  f2c[which(I3D==1)]           <-0
  EI1y                         <-EI1[EI1%in%which(young==1|child==1)]
  EI1o                         <-EI1[EI1%in%which(old==1)]
  d_inf1[!is.na(d_inf1)&d_inf1>0]  <-d_inf1[!is.na(d_inf1)&d_inf1>0]-1
  if(length(EI1y)>0){
    d_inf1[EI1y]  <-rpois(length(EI1y),yI1_R)
  }
  if(length(EI1o)>0){
    d_inf1[EI1o]  <-rpois(length(EI1o),oI1_R)
  }
  
  I1I2y   <-I1I2[I1I2%in%which(young==1|child==1)]
  I1I2o   <-I1I2[I1I2%in%which(old==1)]
  d_inf2[!is.na(d_inf2)&d_inf2>0]  <-d_inf2[!is.na(d_inf2)&d_inf2>0]-1
  #d_inf2[which(I1I2==1&young==1)]<-rpois(length(which(I1I2==1&young==1)),yI2_R)
  #d_inf2[which(I1I2==1&old==1)]<-rpois(length(which(I1I2==1&old==1)),oI2_R)
  
  if(length(I1I2y)>0){
    d_inf2[I1I2y]  <-rpois(length(I1I2y),yI2_R)
  }
  if(length(I1I2o)>0){
    d_inf2[I1I2o]  <-rpois(length(I1I2o),oI2_R)
  }
  
  d_inf3[!is.na(d_inf3)&d_inf3>0]  <-d_inf3[!is.na(d_inf3)&d_inf3>0]-1
  d_inf3[which(I2I3==1&young==1)]  <-rpois(length(which(I2I3==1&young==1)),yI3_R)
  d_inf3[which(I2I3==1&old==1)]    <-rpois(length(which(I2I3==1&old==1)),oI3_R)
  
  #I1R<-which(status$I1==1&!is.na(d_inf1)&d_inf1==0)
  #status$I1[I1R]<-0
  #status$R[I1R]<-1
  #f2c[I1R]<-0
  I2R             <-which(status$I2==1&!is.na(d_inf2)&d_inf2==0)
  status$I2[I2R]  <-0
  status$R[I2R]   <-1
  f2c[I2R]        <-0
  I3R             <-which(status$I3==1&!is.na(d_inf3)&d_inf3==0)
  status$I3[I3R]  <-0
  status$R[I3R]   <-1
  f2c[I3R]        <-0
  
  output          <-list(status,d_exp,d_inf1,d_inf2,d_inf3)
  names(output)   <-c("status","d_exp","d_inf1","d_inf2","d_inf3")
  return(output)
  
} #end of function  

###############################################
###############################################


network_rewire_concern   <-function(net,concern,concern.prev,g_pop_info,prop_cut,prop_cutg,cut_to,group1mean,group2mean,group3mean,groupDmean){
  g1mean<-group1mean
  g2mean<-group2mean
  g3mean<-group3mean
  gDmean<-groupDmean
  
  groupsum<-matrix(0,nrow=1,ncol=4)

  for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
  # within group cutting
    if (g==1){
      for(i in 1:groupsum[g]){
    if(concern[i]-concern.prev[i]==-1){
      if(g_pop_info$child[i]==1){
        t_con  <-which(net[i,which(g_pop_info$child==1)]==cut_to)
        lt_con <-length(t_con)
        t_cut  <-rbinom(lt_con,1,1)
        net[i,t_con[t_cut==1]] <-net[i,t_con[t_cut==1]]<-1
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,1)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==1&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-1
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,1)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
        t_y<-which(net[i,]==1&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-1
          }
        }
        if(length(t_y)>1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-1
          }
        }
      }  
    }
    if(concern[i]-concern.prev[i]==1){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cut)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cut)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,prop_cut)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
  }
  }
    if (g==2){
      groupsumtemp=groupsum[g-1]
      
      for(i in groupsumtemp:sum(groupsumtemp,groupsum[g])){
    if(concern[i]-concern.prev[i]==-1){
      if(g_pop_info$child[i]==1){
        t_con  <-which(net[i,which(g_pop_info$child==1)]==cut_to)
        lt_con <-length(t_con)
        t_cut  <-rbinom(lt_con,1,1)
        net[i,t_con[t_cut==1]] <-net[i,t_con[t_cut==1]]<-1
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,1)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==1&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-1
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,1)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
        t_y<-which(net[i,]==1&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-1
          }
        }
        if(length(t_y)>1){
          t_con2  <-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac,g_pop_info$n_Bc)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-1
          }
        }
      }  
    }
    if(concern[i]-concern.prev[i]==1){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cut)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cut)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,prop_cut)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
  }
    }
    if (g==3){
      groupsumtemp=groupsum[g-1]+groupsum[g-2]
      for(i in groupsumtemp:sum(groupsumtemp,groupsum[g])){
        if(concern[i]-concern.prev[i]==-1){
          if(g_pop_info$child[i]==1){
            t_con  <-which(net[i,which(g_pop_info$child==1)]==cut_to)
            lt_con <-length(t_con)
            t_cut  <-rbinom(lt_con,1,1)
            net[i,t_con[t_cut==1]] <-net[i,t_con[t_cut==1]]<-1
          }
          if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
            t_con   <-which(net[i,]>0&g_pop_info$child==0)
            lt_con  <-length(t_con)
            t_cut   <-rbinom(lt_con,1,1)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
            t_par   <-which(net[i,]>0&g_pop_info$child==1)
            t_con2  <-which(net[i,]==1&g_pop_info$old==1)
            if(length(t_par)>0){
              net[t_par,t_con2]<-net[t_con2,t_par]<-1
            }
          }
          if(g_pop_info$old[i]==1){
            t_con    <-which(net[i,]>0&g_pop_info$child==0)
            lt_con   <-length(t_con)
            t_cut    <-rbinom(lt_con,1,1)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
            t_y<-which(net[i,]==1&g_pop_info$child==0&g_pop_info$old==0)
            if(length(t_y)==1){
              t_con2  <-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0))
              if(length(t_con2)){
                net[i,t_con2]  <-net[t_con2,i]<-1
              }
            }
            if(length(t_y)>1){
              t_con2  <-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
              if(length(t_con2)){
                net[i,t_con2]  <-net[t_con2,i]<-1
              }
            }
          }  
        }
        if(concern[i]-concern.prev[i]==1){
          if(g_pop_info$child[i]==1){
            t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
            lt_con  <-length(t_con)
            t_cut   <-rbinom(lt_con,1,prop_cut)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          }
          if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
            t_con   <-which(net[i,]>0&g_pop_info$child==0)
            lt_con  <-length(t_con)
            t_cut   <-rbinom(lt_con,1,prop_cut)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
            t_par   <-which(net[i,]>0&g_pop_info$child==1)
            t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
            if(length(t_par)>0){
              net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
            }
          }
          if(g_pop_info$old[i]==1){
            t_con    <-which(net[i,]>0&g_pop_info$child==0)
            lt_con   <-length(t_con)
            t_cut    <-rbinom(lt_con,1,prop_cut)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
            t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
            if(length(t_y)==1){
              t_con2  <-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0))
              if(length(t_con2)){
                net[i,t_con2]  <-net[t_con2,i]<-cut_to
              }
            }
            if(length(t_y)>1){
              t_con2<-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
              if(length(t_con2)){
                net[i,t_con2]<-net[t_con2,i]<-cut_to
              }
            }
          }
        }
      }
    }
    if (g==4){
      groupsumtemp=groupsum[g-1]+groupsum[g-2]+groupsum[g-3]
      for(i in groupsumtemp:sum(groupsumtemp,groupsum[g])){
        if(concern[i]-concern.prev[i]==-1){
          if(g_pop_info$child[i]==1){
            t_con  <-which(net[i,which(g_pop_info$child==1)]==cut_to)
            lt_con <-length(t_con)
            t_cut  <-rbinom(lt_con,1,1)
            net[i,t_con[t_cut==1]] <-net[i,t_con[t_cut==1]]<-1
          }
          if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
            t_con   <-which(net[i,]>0&g_pop_info$child==0)
            lt_con  <-length(t_con)
            t_cut   <-rbinom(lt_con,1,1)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
            t_par   <-which(net[i,]>0&g_pop_info$child==1)
            t_con2  <-which(net[i,]==1&g_pop_info$old==1)
            if(length(t_par)>0){
              net[t_par,t_con2]<-net[t_con2,t_par]<-1
            }
          }
          if(g_pop_info$old[i]==1){
            t_con    <-which(net[i,]>0&g_pop_info$child==0)
            lt_con   <-length(t_con)
            t_cut    <-rbinom(lt_con,1,1)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-1
            t_y<-which(net[i,]==1&g_pop_info$child==0&g_pop_info$old==0)
            if(length(t_y)==1){
              t_con2  <-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0))
              if(length(t_con2)){
                net[i,t_con2]  <-net[t_con2,i]<-1
              }
            }
            if(length(t_y)>1){
              t_con2  <-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
              if(length(t_con2)){
                net[i,t_con2]  <-net[t_con2,i]<-1
              }
            }
          }  
        }
        if(concern[i]-concern.prev[i]==1){
          if(g_pop_info$child[i]==1){
            t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
            lt_con  <-length(t_con)
            t_cut   <-rbinom(lt_con,1,prop_cut)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          }
          if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
            t_con   <-which(net[i,]>0&g_pop_info$child==0)
            lt_con  <-length(t_con)
            t_cut   <-rbinom(lt_con,1,prop_cut)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
            t_par   <-which(net[i,]>0&g_pop_info$child==1)
            t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
            if(length(t_par)>0){
              net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
            }
          }
          if(g_pop_info$old[i]==1){
            t_con    <-which(net[i,]>0&g_pop_info$child==0)
            lt_con   <-length(t_con)
            t_cut    <-rbinom(lt_con,1,prop_cut)
            net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
            t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
            if(length(t_y)==1){
              t_con2  <-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0))
              if(length(t_con2)){
                net[i,t_con2]  <-net[t_con2,i]<-cut_to
              }
            }
            if(length(t_y)>1){
              t_con2<-unique(which(net[t_y,groupsumtemp:sum(groupsumtemp,g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
              if(length(t_con2)){
                net[i,t_con2]<-net[t_con2,i]<-cut_to
              }
            }
          }
        }
      }
    }
  }
  
  # between group cutting for small differences aka groups cut a proportion of connection
    for (g in c(1,2,3,4)){
    groupsum[g]<-sum(g_pop_info$n_Ac[[g]]+g_pop_info$n_Ac[[g]]+g_pop_info$n_Ay[[g]]+g_pop_info$n_By[[g]]+g_pop_info$n_Ao[[g]]+g_pop_info$n_Bo[[g]])
    }
    
    # difference between group 1 and 2 (small and large, repel)
    for (i in 1:sum(groupsum[1],groupsum[2])){
    if(abs(g1mean-g2mean)<0.01){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          t_con3<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
          if(length(t_con3)){
            net[i,t_con3]<-net[t_con3,i]<-cut_to
          }
        }
      }
    }
    if(abs(g1mean-g2mean)>0.25){
        if(g_pop_info$child[i]==1){
          t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        }
        if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
          t_con   <-which(net[i,]>0&g_pop_info$child==0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_par   <-which(net[i,]>0&g_pop_info$child==1)
          t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
          if(length(t_par)>0){
            net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
          }
        }
        if(g_pop_info$old[i]==1){
          t_con    <-which(net[i,]>0&g_pop_info$child==0)
          lt_con   <-length(t_con)
          t_cut    <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
          if(length(t_y)==1){
            t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
            if(length(t_con2)){
              net[i,t_con2]  <-net[t_con2,i]<-cut_to
            }
          }
          if(length(t_y)>1){
            t_con2<-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            t_con3<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            if(length(t_con2)){
              net[i,t_con2]<-net[t_con2,i]<-cut_to
            }
            if(length(t_con3)){
              net[i,t_con3]<-net[t_con3,i]<-cut_to
            }
          }
        }
      }
      
    }
  
  set1 <- 1:500
  set2 <- 1000:1500
  
  # Concatenate the sets
  combined_set <- c(set1, set2)
    
  for (i in combined_set){
    if(abs(g1mean-g3mean)<0.01){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          t_con3<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
          if(length(t_con3)){
            net[i,t_con3]<-net[t_con3,i]<-cut_to
          }
        }
      }
    }
    if(abs(g1mean-g3mean)>0.25){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          t_con3<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
          if(length(t_con3)){
            net[i,t_con3]<-net[t_con3,i]<-cut_to
          }
        }
      }
    }
    
  }
  
  set1 <- 1:500
  set2 <- 1500:2000
  
  # Concatenate the sets
  combined_set <- c(set1, set2)
  
  for (i in combined_set){
    if(abs(g1mean-gDmean)<0.01){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          t_con3<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
          if(length(t_con3)){
            net[i,t_con3]<-net[t_con3,i]<-cut_to
          }
        }
      }
    }
    if(abs(g1mean-gDmean)>0.25){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          t_con3<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
          if(length(t_con3)){
            net[i,t_con3]<-net[t_con3,i]<-cut_to
          }
        }
      }
    }
    
  }
  
    # difference between 2 and 3 (small and large, repel)
    for (i in sum(groupsum[1],1):sum(groupsum[1],groupsum[2],groupsum[3])){
      if(abs(g2mean-g3mean)<0.01){
        if(g_pop_info$child[i]==1){
          t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        }
        if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
          t_con   <-which(net[i,]>0&g_pop_info$child==0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_par   <-which(net[i,]>0&g_pop_info$child==1)
          t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
          if(length(t_par)>0){
            net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
          }
        }
        if(g_pop_info$old[i]==1){
          t_con    <-which(net[i,]>0&g_pop_info$child==0)
          lt_con   <-length(t_con)
          t_cut    <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
          if(length(t_y)==1){
            t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
            if(length(t_con2)){
              net[i,t_con2]  <-net[t_con2,i]<-cut_to
            }
          }
          if(length(t_y)>1){
            t_con2<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            t_con3<-unique(which(net[t_y,sum(groupsum[1],groupsum[2]+1):sum(groupsum[1],groupsum[2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            if(length(t_con2)){
              net[i,t_con2]<-net[t_con2,i]<-cut_to
            }
            if(length(t_con3)){
              net[i,t_con3]<-net[t_con3,i]<-cut_to
            }
          }
        }
      }
      if(abs(g2mean-g3mean)>0.25){
        if(g_pop_info$child[i]==1){
          t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        }
        if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
          t_con   <-which(net[i,]>0&g_pop_info$child==0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_par   <-which(net[i,]>0&g_pop_info$child==1)
          t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
          if(length(t_par)>0){
            net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
          }
        }
        if(g_pop_info$old[i]==1){
          t_con    <-which(net[i,]>0&g_pop_info$child==0)
          lt_con   <-length(t_con)
          t_cut    <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
          if(length(t_y)==1){
            t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
            if(length(t_con2)){
              net[i,t_con2]  <-net[t_con2,i]<-cut_to
            }
          }
          if(length(t_y)>1){
            t_con2<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            t_con3<-unique(which(net[t_y,sum(groupsum[1],groupsum[2]+1):sum(groupsum[1],groupsum[2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            if(length(t_con2)){
              net[i,t_con2]<-net[t_con2,i]<-cut_to
            }
            if(length(t_con3)){
              net[i,t_con3]<-net[t_con3,i]<-cut_to
            }
          }
        }
      }
      
    }
    
  set1 <- 500:1000
  set2 <- 1500:2000
  
  # Concatenate the sets
  combined_set <- c(set1, set2)
  
  for (i in combined_set){
    if(abs(g2mean-gDmean)<0.01){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          t_con3<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
          if(length(t_con3)){
            net[i,t_con3]<-net[t_con3,i]<-cut_to
          }
        }
      }
    }
    if(abs(g2mean-gDmean)>0.25){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_par   <-which(net[i,]>0&g_pop_info$child==1)
        t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
        if(length(t_par)>0){
          net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
        }
      }
      if(g_pop_info$old[i]==1){
        t_con    <-which(net[i,]>0&g_pop_info$child==0)
        lt_con   <-length(t_con)
        t_cut    <-rbinom(lt_con,1,prop_cutg)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          t_con3<-unique(which(net[t_y,sum(groupsum[1],1):sum(groupsum[1],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
          if(length(t_con3)){
            net[i,t_con3]<-net[t_con3,i]<-cut_to
          }
        }
      }
    }
    
  }
  
    # difference between group 3 and defect (small and large, repel)
    for (i in sum(groupsum[1],groupsum[2],1):sum(groupsum[1],groupsum[2],groupsum[3],groupsum[4])){
      if(abs(g3mean-gDmean)<0.01){
        if(g_pop_info$child[i]==1){
          t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        }
        if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
          t_con   <-which(net[i,]>0&g_pop_info$child==0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_par   <-which(net[i,]>0&g_pop_info$child==1)
          t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
          if(length(t_par)>0){
            net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
          }
        }
        if(g_pop_info$old[i]==1){
          t_con    <-which(net[i,]>0&g_pop_info$child==0)
          lt_con   <-length(t_con)
          t_cut    <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
          if(length(t_y)==1){
            t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
            if(length(t_con2)){
              net[i,t_con2]  <-net[t_con2,i]<-cut_to
            }
          }
          if(length(t_y)>1){
            t_con2<-unique(which(net[t_y,sum(groupsum[1],groupsum[2],1):sum(groupsum[1],groupsum[2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            t_con3<-unique(which(net[t_y,sum(groupsum[1],groupsum[2],groupsum[3],1):sum(groupsum[1],groupsum[2],groupsum[3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            if(length(t_con2)){
              net[i,t_con2]<-net[t_con2,i]<-cut_to
            }
            if(length(t_con3)){
              net[i,t_con3]<-net[t_con3,i]<-cut_to
            }
          }
        }
      }
      if(abs(g3mean-gDmean)>0.25){
        if(g_pop_info$child[i]==1){
          t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        }
        if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
          t_con   <-which(net[i,]>0&g_pop_info$child==0)
          lt_con  <-length(t_con)
          t_cut   <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_par   <-which(net[i,]>0&g_pop_info$child==1)
          t_con2  <-which(net[i,]==cut_to&g_pop_info$old==1)
          if(length(t_par)>0){
            net[t_par,t_con2]<-net[t_con2,t_par]<-cut_to
          }
        }
        if(g_pop_info$old[i]==1){
          t_con    <-which(net[i,]>0&g_pop_info$child==0)
          lt_con   <-length(t_con)
          t_cut    <-rbinom(lt_con,1,prop_cutg)
          net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
          t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
          if(length(t_y)==1){
            t_con2  <-unique(which(net[t_y,1:(g_pop_info$n_Ac[[g]]+g_pop_info$n_Bc[[g]])]>0))
            if(length(t_con2)){
              net[i,t_con2]  <-net[t_con2,i]<-cut_to
            }
          }
          if(length(t_y)>1){
            t_con2<-unique(which(net[t_y,sum(groupsum[1],groupsum[2],1):sum(groupsum[1],groupsum[2],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            t_con3<-unique(which(net[t_y,sum(groupsum[1],groupsum[2],groupsum[3],1):sum(groupsum[1],groupsum[2],groupsum[3],g_pop_info$n_Ac[[g]],g_pop_info$n_Bc[[g]])]>0,arr.ind=TRUE)[,2])
            if(length(t_con2)){
              net[i,t_con2]<-net[t_con2,i]<-cut_to
            }
            if(length(t_con3)){
              net[i,t_con3]<-net[t_con3,i]<-cut_to
            }
          }
        }
      }
      
    }
    
  return(net)
}

network_rewire_infectionS<-function(net,status,g_pop_info,cut_to){
  to_cut<-which(status$I2+status$I3>0)
  if(length(to_cut)>0){
    for(i in 1:length(to_cut)){
      t_c<-which(net[to_cut[i],]==1)
      net[to_cut[i],t_c]<-net[t_c,to_cut[i]]<-cut_to
    }
  }
  return(net)
}

network_rewire_infectionM<-function(net,status,g_pop_info,cut_to){
  to_cut<-which(status$I1>0)
  if(length(to_cut)>0){
    for(i in 1:length(to_cut)){
      t_c<-which(net[to_cut[i],]==1)
      if(g_pop_info$child[to_cut[i]]==1){
        t_c<-t_c[which(t_c%in%which(g_pop_info$child==0&g_pop_info$old==0)==FALSE)]
      }
      if(g_pop_info$child[to_cut[i]]==0&g_pop_info$old[to_cut[i]]==0){
        t_c<-t_c[which(t_c%in%which(g_pop_info$child==1)==FALSE)]
      }
      net[to_cut[i],t_c]<-net[t_c,to_cut[i]]<-cut_to
    }
  }
  return(net)
}

network_rewire_infectionR<-function(net,status,g_pop_info,cut_to){
  to_add<-which(status$R>0)
  if(length(to_add)>0){
    for(i in 1:length(to_add)){
      t_c<-which(net[to_add[i],]==cut_to)
      net[to_add[i],t_c]<-net[t_c,to_add[i]]<-1
    }
  }
  return(net)
}


###############################
#############################

ep_start<-function(input,thresh=5){
  return(min(which(input>(thresh-1))))
}
