###Population generation

# popsize, ncomms = number communities, propbelA= prop of pop in belief A

require(boot) 

g_pop_gen  <-function(pop_size=2000,ncomms=10,prop_belA=0.5,prop_old=0.3,prop_young=0.5, prop_child=0.2,g_id_1=0.4,g_id_2=0.3,
                      g_id_3=0.2,g_id_def=0.1){
  
  g_pop<-pop_size 
  n_group<-4
  
  #Work out number of people of each political belief in each age category (CODE WITH GROUP ID)
  group_soc_infl<-c(g_id_1,g_id_2,g_id_3,g_id_def)

  #group 1
  n_Ac1  <- as.integer(round(prop_child*g_id_1*prop_belA*g_pop))
  n_Ay1   <-as.integer(round(prop_young*g_id_1*prop_belA*g_pop))
  n_Ao1   <-as.integer(round(prop_old*g_id_1*prop_belA*g_pop))
  n_Bc1   <-as.integer(round(prop_child*g_id_1*(1-prop_belA)*g_pop))
  n_By1   <-as.integer(round(prop_young*g_id_1*(1-prop_belA)*g_pop))
  n_Bo1   <-as.integer(round(prop_old*g_id_1*(1-prop_belA)*g_pop))
  
  g1_tot<-sum(n_Ac1,n_Ay1,n_Bc1,n_By1,n_Ao1,n_Bo1)

  #group 2
  n_Ac2  <- as.integer(round(prop_child*g_id_2*prop_belA*g_pop))
  n_Ay2   <-as.integer(round(prop_young*g_id_2*prop_belA*g_pop))
  n_Ao2   <-as.integer(round(prop_old*g_id_2*prop_belA*g_pop))
  n_Bc2   <-as.integer(round(prop_child*g_id_2*(1-prop_belA)*g_pop))
  n_By2   <-as.integer(round(prop_young*g_id_2*(1-prop_belA)*g_pop))
  n_Bo2   <-as.integer(round(prop_old*g_id_2*(1-prop_belA)*g_pop))
  
  g2_tot<-sum(n_Ac2,n_Ay2,n_Bc2,n_By2,n_Ao2,n_Bo2)
  
  #group 3
  n_Ac3  <- as.integer(round(prop_child*g_id_3*prop_belA*g_pop))
  n_Ay3   <-as.integer(round(prop_young*g_id_3*prop_belA*g_pop))
  n_Ao3   <-as.integer(round(prop_old*g_id_3*prop_belA*g_pop))
  n_Bc3   <-as.integer(round(prop_child*g_id_3*(1-prop_belA)*g_pop))
  n_By3   <-as.integer(round(prop_young*g_id_3*(1-prop_belA)*g_pop))
  n_Bo3   <-as.integer(round(prop_old*g_id_3*(1-prop_belA)*g_pop))
  
  g3_tot<-sum(n_Ac3,n_Ay3,n_Bc3,n_By3,n_Ao3,n_Bo3)
  
  #group 4
  n_Ac4  <- as.integer(round(prop_child*g_id_def*prop_belA*g_pop))
  n_Ay4   <-as.integer(round(prop_young*g_id_def*prop_belA*g_pop))
  n_Ao4   <-as.integer(round(prop_old*g_id_def*prop_belA*g_pop))
  n_Bc4   <-as.integer(round(prop_child*g_id_def*(1-prop_belA)*g_pop))
  n_By4   <-as.integer(round(prop_young*g_id_def*(1-prop_belA)*g_pop))
  n_Bo4   <-as.integer(round(prop_old*g_id_def*(1-prop_belA)*g_pop))
  
  g4_tot<-sum(n_Ac4,n_Ay4,n_Bc4,n_By4,n_Ao4,n_Bo4)
  
  #recalculate population size in case it has changed slightly (due to rounding) (CODE WITH GROUPD ID)
  g_pop  <-sum(g1_tot,g2_tot,g3_tot,g4_tot)
  
  #Vector to identify each individual's community membership 
  g_comms <-rep(1:ncomms,g_pop/ncomms)
  
  #Create useful vectors used in the network generation and models

  #Information on political belief of each individual (WITH GROUP ID)
  
  AorB  <-c(rep("A",sum(n_Ac1+n_Ac2+n_Ac3+n_Ac4)),rep("B",sum(n_Bc1+n_Bc2+n_Bc3+n_Bc4)),
            rep("A",sum(n_Ay1+n_Ay2+n_Ay3+n_Ay4)),rep("B",sum(n_By1+n_By2+n_By3+n_By4)),
            rep("A",sum(n_Ao1+n_Ao2+n_Ao3+n_Ao4)),rep("B",sum(n_Bo1+n_Bo2+n_Bo3+n_Bo4)))
  
  group_num <- c(rep(1,sum(n_Ac1)),rep(2,sum(n_Ac2)),rep(3,sum(n_Ac3)),rep(4,sum(n_Ac4)),
                rep(1,sum(n_Bc1)),rep(2,sum(n_Bc2)),rep(3,sum(n_Bc3)),rep(4,sum(n_Bc4)),
                rep(1,sum(n_Ay1)),rep(2,sum(n_Ay2)),rep(3,sum(n_Ay3)),rep(4,sum(n_Ay4)),
                rep(1,sum(n_By1)),rep(2,sum(n_By2)),rep(3,sum(n_By3)),rep(4,sum(n_By4)),
                rep(1,sum(n_Ao1)), rep(2,sum(n_Ao2)), rep(3,sum(n_Ao3)), rep(4,sum(n_Ao4)),
                rep(1,sum(n_Bo1)), rep(2,sum(n_Bo2)), rep(3,sum(n_Bo3)),rep(4,sum(n_Bo4)))

  child <- c(rep(1,sum(n_Ac1+n_Ac2+n_Ac3+n_Ac4+n_Bc1+n_Bc2+n_Bc3+n_Bc4)), rep(0,sum(n_Ay1+n_Ay2+
            n_Ay3+n_Ay4+n_By1+n_By2+n_By3+n_By4+n_Ao1+n_Ao2+n_Ao3+n_Ao4+n_Bo1+n_Bo2+n_Bo3+n_Bo4))) #Indicator of child or not
  
  adult <- c(rep(0,sum(n_Ac1+n_Ac2+n_Ac3+n_Ac4+n_Bc1+n_Bc2+n_Bc3+n_Bc4)), rep(1,sum(n_Ay1+n_Ay2+
          n_Ay3+n_Ay4+n_By1+n_By2+n_By3+n_By4+n_Ao1+n_Ao2+n_Ao3+n_Ao4+n_Bo1+n_Bo2+n_Bo3+n_Bo4)))   #Indicator of adult or not
  
  old  <-c(rep(0,sum(n_Ac1+n_Ac2+n_Ac3+n_Ac4+n_Bc1+n_Bc2+n_Bc3+n_Bc4+n_Ay1+n_Ay2+n_Ay3+n_Ay4+
          n_By1+n_By2+n_By3+n_By4)), rep(1,sum(n_Ao1+n_Ao2+n_Ao3+n_Ao4+n_Bo1+n_Bo2+n_Bo3+n_Bo4)))  #Indicator if old or not
  
  g_pop_info         <-list(g_pop,g_comms,n_Ac1,n_Ac2,n_Ac3,n_Ac4,n_Bc1,n_Bc2,n_Bc3,n_Bc4,
                          n_Ay1, n_Ay2, n_Ay3, n_Ay4,n_By1,n_By2,n_By3,n_By4,
                          n_Ao1,n_Ao2,n_Ao3,n_Ao4,n_Bo1,n_Bo2,n_Bo3,n_Bo4, AorB, group_num,child,adult,old)
  names(g_pop_info)  <-c("g_pop","g_comms","n_Ac1","n_Ac2","n_Ac3","n_Ac4", "n_Bc1","n_Bc2","n_Bc3","n_Bc4",
                       "n_Ay1","n_Ay2","n_Ay3","n_Ay4","n_By1","n_By2","n_By3","n_By4",
                       "n_Ao1","n_Ao2","n_Ao3","n_Ao4","n_Bo1","n_Bo2","n_Bo3","n_Bo4",
                       "AorB","group_num","child","adult","old")
  
  return(g_pop_info)
  
}

  
##################################################
##################################################

##Parent Exchange

par_ex<-function(g_pop_info,parents,dis_mat){

  # political belief A
  # group 1
  Ap11  <-parents[1:g_pop_info$n_Ac1,1]
  Ap12  <-parents[1:g_pop_info$n_Ac1,2]

  # group 2
  Ap21  <-parents[g_pop_info$n_Ac1+1:g_pop_info$n_Ac2+g_pop_info$n_Ac1,1]
  Ap22  <-parents[g_pop_info$n_Ac1+1:g_pop_info$n_Ac2+g_pop_info$n_Ac1,2]
  
  #group 3
  Ap31  <-parents[g_pop_info$n_Ac2+g_pop_info$n_Ac1+1:g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1,1]
  Ap32  <-parents[g_pop_info$n_Ac2+g_pop_info$n_Ac1+1:g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1,2]
  
  #group 4
  Ap41  <-parents[g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1+1:g_pop_info$n_Ac4+g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1,1]
  Ap42  <-parents[g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1+1:g_pop_info$n_Ac4+g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1,2]
  
  #Political belief B
  n_Act<-g_pop_info$n_Ac4+g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1
  
  #group 1
  Bp11  <-parents[(n_Act+1):n_Act+g_pop_info$n_Bct1,1]
  Bp12  <-parents[(n_Act+1):n_Act+g_pop_info$n_Bct1,1]
  
  #group 2
  Bp21  <-parents[n_Act+g_pop_info$n_Bct1+1:n_Act+g_pop_info$n_Bct2+g_pop_info$n_Bct1,1]
  Bp22  <-parents[n_Act+g_pop_info$n_Bct1+1:n_Act+g_pop_info$n_Bct2+g_pop_info$n_Bct1,2]
  
  #group 3
  Bp31  <-parents[n_Act+g_pop_info$n_Bct2+g_pop_info$n_Bct1+1:n_Act+g_pop_info$n_Bct3+g_pop_info$n_Bct2+g_pop_info$n_Bct1,1]
  Bp32  <-parents[n_Act+g_pop_info$n_Bct2+g_pop_info$n_Bct1+1:n_Act+g_pop_info$n_Bct3+g_pop_info$n_Bct2+g_pop_info$n_Bct1,2]
  
  #group 4
  Bp41  <-parents[n_Act+g_pop_info$n_Bct3+g_pop_info$n_Bct2+g_pop_info$n_Bct1+1:n_Act+g_pop_info$n_Bct4+g_pop_info$n_Bct3+g_pop_info$n_Bct2+g_pop_info$n_Bct1,1]
  Bp42  <-parents[n_Act+g_pop_info$n_Bct3+g_pop_info$n_Bct2+g_pop_info$n_Bct1+1:n_Act+g_pop_info$n_Bct4+g_pop_info$n_Bct3+g_pop_info$n_Bct2+g_pop_info$n_Bct1,2]
  
  n_Bct<-g_pop_info$n_Bc4+g_pop_info$n_Bc3+g_pop_info$n_Bc2+g_pop_info$n_Bc1
  
  dis_mat[1:(n_Act+n_Bct),(n_Act+n_Bct+1):(g_pop_info$g_pop)]<-dis_mat[(n_Act+n_Bct+1):(g_pop_info$g_pop),1:(n_Act+n_Bct)]<-0
  
  Acs1  <-seq(1,g_pop_info$n_Ac1,1)
  Acs2  <-seq(1,g_pop_info$n_Ac2,1)
  Acs3  <-seq(1,g_pop_info$n_Ac3,1)
  Acs4  <-seq(1,g_pop_info$n_Ac4,1)
  
  Bcs1  <-seq(1,g_pop_info$n_Bc1,1)
  Bcs2  <-seq(1,g_pop_info$n_Bc2,1)
  Bcs3  <-seq(1,g_pop_info$n_Bc3,1)
  Bcs4  <-seq(1,g_pop_info$n_Bc4,1)
  
  #We now fill in the child-parent edges into the new supra-adjacency matrix for both 
  # political beliefs and groups
  
  for(i in 1:length(Acs1)){
    dis_mat[Acs1[i],Ap11[i]+n_Act+n_Bct] <-dis_mat[Ap11[i]+n_Act+n_Bct,Acs1[i]]<-1
    dis_mat[Acs1[i],Ap12[i]+n_Act+n_Bct] <-dis_mat[Ap12[i]+n_Act+n_Bct,Acs1[i]]<-1
  }

  for(i in 1:length(Acs2)){
    dis_mat[g_pop_info$n_Ac1+Acs2[i],Ap21[i]+g_pop_info$n_Ay1+n_Act+n_Bct] <-dis_mat[Ap21[i]+g_pop_info$n_Ay1+n_Act+n_Bct,g_pop_info$n_Ac1+Acs2[i]]<-1
    dis_mat[g_pop_info$n_Ac1+Acs2[i],Ap22[i]+g_pop_info$n_Ay1+n_Act+n_Bct] <-dis_mat[Ap22[i]+g_pop_info$n_Ay1+n_Act+n_Bct,g_pop_info$n_Ac1+Acs2[i]]<-1
  }
  
  for(i in 1:length(Acs3)){
    dis_mat[g_pop_info$n_Ac2+g_pop_info$n_Ac1+Acs3[i],Ap31[i]+g_pop_info$n_Ay1+g_pop_info$n_Ay2+n_Act+n_Bct] <-dis_mat[Ap31[i]+g_pop_info$n_Ay2+g_pop_info$n_Ay1+n_Act+n_Bct,g_pop_info$n_Ac2+g_pop_info$n_Ac1+Acs3[i]]<-1
    dis_mat[g_pop_info$n_Ac2+g_pop_info$n_Ac1+Acs3[i],Ap32[i]+g_pop_info$n_Ay1+g_pop_info$n_Ay2+n_Act+n_Bct] <-dis_mat[Ap32[i]+g_pop_info$n_Ay2+g_pop_info$n_Ay1+n_Act+n_Bct,g_pop_info$n_Ac2+g_pop_info$n_Ac1+Acs3[i]]<-1
  }
  
  for(i in 1:length(Acs4)){
    dis_mat[g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1+Acs4[i],Ap41[i]+g_pop_info$n_Ay1+g_pop_info$n_Ay2+g_pop_info$n_Ay3+n_Act+n_Bct] <-dis_mat[Ap41[i]+g_pop_info$n_Ay3+g_pop_info$n_Ay2+g_pop_info$n_Ay1+n_Act+n_Bct,g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1+Acs4[i]]<-1
    dis_mat[g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1+Acs4[i],Ap42[i]+g_pop_info$n_Ay1+g_pop_info$n_Ay2+g_pop_info$n_Ay3+n_Act+n_Bct] <-dis_mat[Ap42[i]+g_pop_info$n_Ay3+g_pop_info$n_Ay2+g_pop_info$n_Ay1+n_Act+n_Bct,g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1+Acs4[i]]<-1
  }
  
  n_Ayt<-g_pop_info$n_Ay4+g_pop_info$n_Ay3+g_pop_info$n_Ay2+g_pop_info$n_Ay1
  n_Byt<-g_pop_info$n_By4+g_pop_info$n_By3+g_pop_info$n_By2+g_pop_info$n_By1
  
  
  for(i in 1:length(Bcs1)){
    dis_mat[n_Act+Bcs1[i],Bp11[i]+n_Ayt+n_Act+n_Bct] <-dis_mat[Bp11[i]+n_Ayt+n_Act+n_Bct,n_Act+Bcs1[i]]<-1
    dis_mat[n_Act+Bcs1[i],Bp12[i]+n_Ayt+n_Act+n_Bct] <-dis_mat[Bp12[i]+n_Ayt+n_Act+n_Bct,n_Act+Bcs1[i]]<-1
  }
  
  for(i in 1:length(Bcs2)){
    dis_mat[g_pop_info$n_Bc1+n_Act+Bcs2[i],Bp21[i]+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct] <-dis_mat[Bp21[i]+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct,g_pop_info$n_Bc1+n_Act+Bcs2[i]]<-1
    dis_mat[g_pop_info$n_Bc1+n_Act+Bcs2[i],Bp22[i]+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct] <-dis_mat[Bp22[i]+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct,g_pop_info$n_Bc1+n_Act+Bcs2[i]]<-1
  }
  
  for(i in 1:length(Bcs3)){
    dis_mat[g_pop_info$n_Bc2+g_pop_info$n_Bc1+n_Act+Bcs3[i],Bp31[i]+g_pop_info$n_By2+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct] <-dis_mat[Bp31[i]+g_pop_info$n_By2+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct,g_pop_info$n_Bc2+g_pop_info$n_Bc1+n_Act+Bcs3[i]]<-1
    dis_mat[g_pop_info$n_Bc2+g_pop_info$n_Bc1+n_Act+Bcs3[i],Bp32[i]+g_pop_info$n_By2+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct] <-dis_mat[Bp32[i]+g_pop_info$n_By2+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct,g_pop_info$n_Bc2+g_pop_info$n_Bc1+n_Act+Bcs3[i]]<-1
  }
  
  for(i in 1:length(Bcs4)){
    dis_mat[Bcs4[i],Bp41[i]+g_pop_info$n_By3+g_pop_info$n_By2+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct] <-dis_mat[Bp41[i]+g_pop_info$n_By3+g_pop_info$n_By2+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct,g_pop_info$n_Bc3+g_pop_info$n_Bc2+g_pop_info$n_Bc1+n_Act+Bcs4[i]]<-1
    dis_mat[g_pop_info$n_Bc3+g_pop_info$n_Bc2+g_pop_info$n_Bc1+n_Act+Bcs4[i],Bp42[i]+g_pop_info$n_By3+g_pop_info$n_By2+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct] <-dis_mat[Bp42[i]+g_pop_info$n_By3+g_pop_info$n_By2+g_pop_info$n_By1+n_Ayt+n_Act+n_Bct,g_pop_info$n_Bc3+g_pop_info$n_Bc2+g_pop_info$n_Bc1+n_Act+Bcs4[i]]<-1
  }
  
  #Add connections from children to the same old adults as their parents
  #For political belief A
  
  n_Aot<-g_pop_info$n_Ao4+g_pop_info$n_Ao3+g_pop_info$n_Ao2+g_pop_info$n_Ao1
  n_Bot<-g_pop_info$n_Bo4+g_pop_info$n_Bo3+g_pop_info$n_Bo2+g_pop_info$n_Bo1
  
  for(i in 1:g_pop_info$n_Ac1){
    t_ycon     <-which(dis_mat[i, (n_Act+n_Bct+1) : (n_Act+n_Bct+g_pop_info$n_Ay1)]==1)
    t_ycon2    <-t_ycon+n_Act+n_Bct
    
    for(j in 1:length(t_ycon2)){
      t_ycon3  <-which(dis_mat[t_ycon2[j], (g_pop_info$g_pop-n_Aot-n_Bot+1) : (g_pop_info$g_pop)]==1)
      t_ycon4  <-sort(t_ycon3+g_pop_info$g_pop-n_Aot-n_Bot)
      dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
    }
  }

  for(i in g_pop_info$n_Ac1+1 : g_pop_info$n_Ac1+g_pop_info$n_Ac2){
    t_ycon     <-which(dis_mat[i, (n_Act+n_Bct+g_pop_info$n_Ay1) : (n_Act+n_Bct+g_pop_info$n_Ay1+g_pop_info$n_Ay2)]==1)
    t_ycon2    <-t_ycon+n_Act+n_Bct+g_pop_info$n_Ay1
    
    for(j in 1:length(t_ycon2)){
      t_ycon3  <-which(dis_mat[t_ycon2[j], (g_pop_info$g_pop-n_Aot-n_Bot+1):(g_pop_info$g_pop)]==1)
      t_ycon4  <-sort(t_ycon3+g_pop_info$g_pop-n_Aot-n_Bot)
      dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
    }
  }
  
  for(i in g_pop_info$n_Ac2+g_pop_info$n_Ac1+1 : g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1){
    t_ycon     <-which(dis_mat[i, (n_Act+n_Bct+g_pop_info$n_Ay1+g_pop_info$n_Ay2) : (n_Act+n_Bct+g_pop_info$n_Ay1+g_pop_info$n_Ay2+g_pop_info$n_Ay3)]==1)
    t_ycon2    <-t_ycon+n_Act+n_Bct+g_pop_info$n_Ay1+g_pop_info$n_Ay2
    
    for(j in 1:length(t_ycon2)){
      t_ycon3  <-which(dis_mat[t_ycon2[j],(g_pop_info$g_pop-n_Aot-n_Bot+1):(g_pop_info$g_pop)]==1)
      t_ycon4  <-sort(t_ycon3+g_pop_info$g_pop-n_Aot-n_Bot)
      #print(t_ycon4)
      dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
    }
  }
  
  for(i in g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1+1 : n_Act){
    t_ycon     <-which(dis_mat[i, (n_Act+n_Bct+g_pop_info$n_Ay1+g_pop_info$n_Ay2+g_pop_info$n_Ay3) : (n_Act+n_Bct+n_Ayt)]==1)
    t_ycon2    <-t_ycon+n_Act+n_Bct+g_pop_info$n_Ay1+g_pop_info$n_Ay2+g_pop_info$n_Ay3
    
    for(j in 1:length(t_ycon2)){
      t_ycon3  <-which(dis_mat[t_ycon2[j],(g_pop_info$g_pop-n_Aot-n_Bot+1):(g_pop_info$g_pop)]==1)
      t_ycon4  <-sort(t_ycon3+g_pop_info$g_pop-n_Aot-n_Bot)
      #print(t_ycon4)
      dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
    }
  }
  
  for(i in n_Act+1 : n_Act+g_pop_info$n_Bc1){
    t_ycon     <-which(dis_mat[i, (n_Act+n_Bct+n_Ayt) : (n_Act+n_Bct+n_Ayt+g_pop_info$n_By1)]==1)
    t_ycon2    <-t_ycon+n_Act+n_Bct+n_Ayt
    
    for(j in 1:length(t_ycon2)){
      t_ycon3  <-which(dis_mat[t_ycon2[j],(g_pop_info$g_pop-n_Aot-n_Bot+1):(g_pop_info$g_pop)]==1)
      t_ycon4  <-sort(t_ycon3+g_pop_info$g_pop-n_Aot-n_Bot)
      #print(t_ycon4)
      dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
    }
  }
  
  for(i in n_Act+g_pop_info$n_Bc1+1 : n_Act+g_pop_info$n_Bc1+g_pop_info$n_Bc2){
    t_ycon     <-which(dis_mat[i, (n_Act+n_Bct+n_Ayt+g_pop_info$n_By1) : (n_Act+n_Bct+n_Ayt+g_pop_info$n_By1+g_pop_info$n_By2)]==1)
    t_ycon2    <-t_ycon+n_Act+n_Bct+n_Ayt+g_pop_info$n_By1
    
    for(j in 1:length(t_ycon2)){
      t_ycon3  <-which(dis_mat[t_ycon2[j], (g_pop_info$g_pop-n_Aot-n_Bot+1) : (g_pop_info$g_pop)]==1)
      t_ycon4  <-sort(t_ycon3+g_pop_info$g_pop-n_Aot-n_Bot)
      #print(t_ycon4)
      dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
    }
  }
  
  for(i in n_Act+g_pop_info$n_Bc1+g_pop_info$n_Bc2+1 : n_Act+g_pop_info$n_Bc1+g_pop_info$n_Bc2+g_pop_info$n_Bc3){
    t_ycon     <-which(dis_mat[i, (n_Act+n_Bct+n_Ayt+g_pop_info$n_By1+g_pop_info$n_By2) : (n_Act+n_Bct+n_Ayt+g_pop_info$n_By1+g_pop_info$n_By2+g_pop_info$n_By3)]==1)
    t_ycon2    <-t_ycon+n_Act+n_Bct+n_Ayt+g_pop_info$n_By1+g_pop_info$n_By2
    
    for(j in 1:length(t_ycon2)){
      t_ycon3  <-which(dis_mat[t_ycon2[j],(g_pop_info$g_pop-n_Aot-n_Bot+1):(g_pop_info$g_pop)]==1)
      t_ycon4  <-sort(t_ycon3+g_pop_info$g_pop-n_Aot-n_Bot)
      #print(t_ycon4)
      dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
    }
  }
  
  for(i in n_Act+g_pop_info$n_Bc1+g_pop_info$n_Bc2+g_pop_info$n_Bc3+1 : n_Act+n_Bct){
    t_ycon     <-which(dis_mat[i, (n_Act+n_Bct+n_Ayt+g_pop_info$n_By1+g_pop_info$n_By2+g_pop_info$n_By3) : (n_Act+n_Bct+n_Ayt+n_Byt)]==1)
    t_ycon2    <-t_ycon+n_Act+n_Bct+n_Ayt+g_pop_info$n_By1+g_pop_info$n_By2+g_pop_info$n_By3
    
    for(j in 1:length(t_ycon2)){
      t_ycon3  <-which(dis_mat[t_ycon2[j], (g_pop_info$g_pop-n_Aot-n_Bot+1):(g_pop_info$g_pop)]==1)
      t_ycon4  <-sort(t_ycon3+g_pop_info$g_pop-n_Aot-n_Bot)
      #print(t_ycon4)
      dis_mat[i,t_ycon4]<-dis_mat[t_ycon4,i]<-1
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

  #group 1
  lA1_concern_y  <-logit(A_concern_y+g_con_1)
  lB1_concern_y  <-logit(B_concern_y+g_con_1)
  lA1_concern_o  <-logit(A_concern_y+A_concern_o+g_con_1)
  lB1_concern_o  <-logit(B_concern_y+B_concern_o+g_con_1)
  
  #group 2
  lA2_concern_y  <-logit(A_concern_y+g_con_2)
  lB2_concern_y  <-logit(B_concern_y+g_con_2)
  lA2_concern_o  <-logit(A_concern_y+A_concern_o+g_con_2)
  lB2_concern_o  <-logit(B_concern_y+B_concern_o+g_con_2)
  
  #group 3
  lA3_concern_y  <-logit(A_concern_y+g_con_3)
  lB3_concern_y  <-logit(B_concern_y+g_con_3)
  lA3_concern_o  <-logit(A_concern_y+A_concern_o+g_con_3)
  lB3_concern_o  <-logit(B_concern_y+B_concern_o+g_con_3)
  
  #group 4
  lA4_concern_y  <-logit(A_concern_y+g_con_def)
  lB4_concern_y  <-logit(B_concern_y+g_con_def)
  lA4_concern_o  <-logit(A_concern_y+A_concern_o+g_con_def)
  lB4_concern_o  <-logit(B_concern_y+B_concern_o+g_con_def)
  
  n_Act<-g_pop_info$n_Ac4+g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1
  n_Bct<-g_pop_info$n_Bc4+g_pop_info$n_Bc3+g_pop_info$n_Bc2+g_pop_info$n_Bc1
  n_Ayt<-g_pop_info$n_Ay4+g_pop_info$n_Ay3+g_pop_info$n_Ay2+g_pop_info$n_Ay1
  n_Byt<-g_pop_info$n_By4+g_pop_info$n_By3+g_pop_info$n_By2+g_pop_info$n_By1
  n_Aot<-g_pop_info$n_Ao4+g_pop_info$n_Ao3+g_pop_info$n_Ao2+g_pop_info$n_Ao1
  n_Bot<-g_pop_info$n_Bo4+g_pop_info$n_Bo3+g_pop_info$n_Bo2+g_pop_info$n_Bo1
  
  
  #Calculate starting beliefs of each person depending on the parameters defined above
  belief  <-c(rep(NA,n_Act),rep(NA,n_Bct),
              rep(lA1_concern_y,g_pop_info$n_Ay1),rep(lA2_concern_y,g_pop_info$n_Ay2),rep(lA3_concern_y,g_pop_info$n_Ay3),rep(lA4_concern_y,g_pop_info$n_Ay4),
              rep(lB1_concern_y,g_pop_info$n_By1),rep(lB2_concern_y,g_pop_info$n_By2),rep(lB3_concern_y,g_pop_info$n_By3),rep(lB4_concern_y,g_pop_info$n_By4),
              rep(lA1_concern_o,g_pop_info$n_Ao1),rep(lA2_concern_o,g_pop_info$n_Ao2),rep(lA3_concern_o,g_pop_info$n_Ao3),rep(lA4_concern_o,g_pop_info$n_Ao4),
              rep(lB1_concern_o,g_pop_info$n_Bo1),rep(lB2_concern_o,g_pop_info$n_Bo2),rep(lB3_concern_o,g_pop_info$n_Bo3),rep(lB4_concern_o,g_pop_info$n_Bo4))
  belief
  
  #Use these starting beliefs to calculate an initial indication of concern (using a bernoulli draw)
  initial_conc  <-c(rep(NA, (n_Act+n_Bct)),
                    rbinom(g_pop_info$g_pop-n_Act-n_Bct,1,inv.logit(belief[n_Act+n_Bct+1:g_pop_info$g_pop])))
  
  initial_conc
  
#Here we set children as taking on the concern-level of their most concerned parent
for(i in 1 : (n_Act+n_Bct)){
  t_par       <-which(mat_cyo[12, (n_Act+n_Bct+1) : (n_Act+n_Bct+n_Ayt+n_Byt)]==1)
  initial_conc[i] <-max(initial_conc[t_par+n_Act+n_Bct])
  if (initial_conc[i]==-Inf){
    initial_conc[i]<-1
  }
}
initial_conc

#calculating the mean belief of each group
group1<-which(g_pop_info$group_num=="1")
group2<-which(g_pop_info$group_num=="2")
group3<-which(g_pop_info$group_num=="3")
group4<-which(g_pop_info$group_num=="4")

belief1<-matrix()
for (i in 1:length(group1)){
  if (group1[i]>480){
  g <- as.integer(group1[i])
  belief1[i] <- belief[g]
  }
  else {belief1[i]<- NA}
}

group1mean<-mean(belief1,na.rm = TRUE)

belief2<-matrix()
for (i in 1:length(group2)){
  if (group2[i]>480){
    g <- as.integer(group2[i])
    belief2[i] <- belief[g]
  }
  else {belief2[i]<- NA}
}

group2mean<-mean(belief2,na.rm = TRUE)

belief3<-matrix()
for (i in 1:length(group3)){
  if (group3[i]>480){
    g <- as.integer(group3[i])
    belief3[i] <- belief[g]
  }
  else {belief3[i]<- NA}
}

group3mean<-mean(belief1,na.rm = TRUE)

belief4<-matrix()
for (i in 1:length(group4)){
  if (group4[i]>480){
    g <- as.integer(group4[i])
    belief4[i] <- belief[g]
  }
  else {belief4[i]<- NA}
}

group4mean<-mean(belief4,na.rm = TRUE)

  #And here is the fully calculated measure of current concern about the virus
  conc           <-initial_conc
  output         <-list(belief,conc,group1mean,group2mean,group3mean,group4mean)
  names(output)  <-c("belief","concern","group1mean","group2mean","group3mean","group4mean")
  return(output)

}



concern_timestep  <-function(g_pop_info,net_b,net_d,belief,concern,inf,g_con_1,g_con_2,g_con_3,g_con_def,
                             lA_ex,lB_ex,l_conc,l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o){
  
  net1  <-net_b
  net2  <-net_d
  conc  <-concern
  n_Act<-g_pop_info$n_Ac4+g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1
  n_Bct<-g_pop_info$n_Bc4+g_pop_info$n_Bc3+g_pop_info$n_Bc2+g_pop_info$n_Bc1
  n_Ayt<-g_pop_info$n_Ay4+g_pop_info$n_Ay3+g_pop_info$n_Ay2+g_pop_info$n_Ay1
  n_Byt<-g_pop_info$n_By4+g_pop_info$n_By3+g_pop_info$n_By2+g_pop_info$n_By1
  n_Aot<-g_pop_info$n_Ao4+g_pop_info$n_Ao3+g_pop_info$n_Ao2+g_pop_info$n_Ao1
  n_Bot<-g_pop_info$n_Bo4+g_pop_info$n_Bo3+g_pop_info$n_Bo2+g_pop_info$n_Bo1
  
  #Calculate new belief scores based on response to extrinsic factors
  belief[g_pop_info$AorB=="A"]  <-belief[g_pop_info$AorB=="A"]+g_pop_info$adult[g_pop_info$AorB=="A"]*lA_ex
  belief[g_pop_info$AorB=="B"]  <-belief[g_pop_info$AorB=="B"]+g_pop_info$adult[g_pop_info$AorB=="B"]*lB_ex
  # 
  # belief[g_pop_info$group_num=="1"]  <-belief[g_pop_info$group_num=="1"]+g_pop_info$adult[g_pop_info$group_num=="1"]*lA_ex
  # belief[g_pop_info$group_num=="2"]  <-belief[g_pop_info$group_num=="2"]+g_pop_info$adult[g_pop_info$group_num=="2"]*lA_ex
  # belief[g_pop_info$group_num=="3"]  <-belief[g_pop_info$group_num=="3"]+g_pop_info$adult[g_pop_info$group_num=="3"]*lA_ex
  # belief[g_pop_info$group_num=="4"]  <-belief[g_pop_info$group_num=="4"]+g_pop_info$adult[g_pop_info$group_num=="4"]*lA_ex
  # 
  #Calculate changes in belief caused by difference in group belief
  
  group1<-which(g_pop_info$group_num=="1")
  group2<-which(g_pop_info$group_num=="2")
  group3<-which(g_pop_info$group_num=="3")
  group4<-which(g_pop_info$group_num=="4")
  
  
  for(i in 1:length(group1)){
    g<-as.integer(group1[i])
    t_con        <-which(net1[g,]==1)
    t_con        <-t_con[t_con> n_Act+n_Bct]
    
    if(length(t_con)>0){
      t_prop     <-sum(conc[t_con])/length(t_con)
      belief[g]  <-belief[g]+t_prop*g_con_1+t_prop*l_conc_o*g_pop_info$old[i]
    }
  }
  
  for(i in 1:length(group2)){
    g            <-as.integer(group2[i])
    t_con        <-which(net1[g,]==1)
    t_con        <-t_con[t_con> n_Act+n_Bct]
    
    if(length(t_con)>0){
      t_prop     <-sum(conc[t_con])/length(t_con)
      belief[g]  <-belief[g]+t_prop*g_con_2+t_prop*l_conc_o*g_pop_info$old[i]
    }
  }
  
  for(i in 1:length(group3)){
    g            <-as.integer(group3[i])
    t_con        <-which(net1[g,]==1)
    t_con        <-t_con[t_con> n_Act+n_Bct]
    
    if(length(t_con)>0){
      t_prop     <-sum(conc[t_con])/length(t_con)
      belief[g]  <-belief[g]+t_prop*g_con_3+t_prop*l_conc_o*g_pop_info$old[i]
    }
  }
  
  for(i in 1:length(group4)){
    g            <-as.integer(group4[i])
    t_con        <-which(net1[g,]==1)
    t_con        <-t_con[t_con> n_Act+n_Bct]
    
    if(length(t_con)>0){
      t_prop     <-sum(conc[t_con])/length(t_con)
      belief[g]  <-belief[g]+t_prop*g_con_def+t_prop*l_conc_o*g_pop_info$old[i]
    }
  }
  

  #Calculate changes in belief caused by concern of network connections in belief sharing network

  for(i in (n_Act+n_Bct+1) : (g_pop_info$g_pop)){
    t_con        <-which(net1[i,]==1)
    t_con        <-t_con[t_con> n_Act+n_Bct]

    if(length(t_con)>0){
      t_prop     <-sum(conc[t_con])/length(t_con)
      belief[i]  <-belief[i]+t_prop*l_conc+t_prop*l_conc_o*g_pop_info$old[i]
    }
  }

  
  #Calculate changes in belief caused infection status of network connections
  for(i in  (n_Act+n_Bct+1) : (g_pop_info$g_pop)){
    t_con        <-which(net1[i,]==1)
    #t_con<-t_con[t_con>(pop_info$n_Ac+pop_info$n_Bc)]
    
    if(length(t_con)>0){
      t_inf      <-sum(inf[t_con,2])
      belief[i]  <-belief[i]+t_inf*l_inf+t_inf*l_inf_o*g_pop_info$old[i]
    }
  }
  
  #Calculate changes in belief caused healthiness of all network connections
  for(i in  (n_Act+n_Bct+1) : (g_pop_info$g_pop)){
    t_con     <-which(net1[i,]==1)
    #t_con<-t_con[t_con>(pop_info$n_Ac+pop_info$n_Bc)]
    
    if(length(t_con)>0){
      t_inf   <-sum(inf[t_con,1])
      
      if(t_inf==0){
        belief[i]  <-belief[i]+l_hea+t_inf*l_hea_o*g_pop_info$old[i]
      }
    }
  }
  
  #Work out new states of concern (minus children)
  conc       <-c(rep(NA,n_Act+n_Bct),rbinom(g_pop_info$g_pop-n_Act-n_Bct,1,
                 inv.logit(belief[n_Act+n_Bct+1:g_pop_info$g_pop])))
  
  #Add in concern of children (based on their most conerned parent)
  for(i in 1 : (n_Act+n_Bct)){
    t_par    <-which(net1[i, (n_Act+n_Bct+1) :(n_Act+n_Bct+n_Ayt+n_Byt)]==1)
    conc[i]  <-max(conc[t_par+n_Act+n_Bct])
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


network_rewire_concern   <-function(net,concern,concern.prev,g_pop_info,prop_cut,cut_to,group1mean,group2mean,
                                    group3mean,group4mean,group1,group2,group3,group4){
  group1<-which(g_pop_info$group_num=="1")
  group2<-which(g_pop_info$group_num=="2")
  group3<-which(g_pop_info$group_num=="3")
  group4<-which(g_pop_info$group_num=="4")
  n_Act<-g_pop_info$n_Ac4+g_pop_info$n_Ac3+g_pop_info$n_Ac2+g_pop_info$n_Ac1
  n_Bct<-g_pop_info$n_Bc4+g_pop_info$n_Bc3+g_pop_info$n_Bc2+g_pop_info$n_Bc1
  n_Ayt<-g_pop_info$n_Ay4+g_pop_info$n_Ay3+g_pop_info$n_Ay2+g_pop_info$n_Ay1
  n_Byt<-g_pop_info$n_By4+g_pop_info$n_By3+g_pop_info$n_By2+g_pop_info$n_By1
  n_Aot<-g_pop_info$n_Ao4+g_pop_info$n_Ao3+g_pop_info$n_Ao2+g_pop_info$n_Ao1
  n_Bot<-g_pop_info$n_Bo4+g_pop_info$n_Bo3+g_pop_info$n_Bo2+g_pop_info$n_Bo1
  
  for(g in 1:length(group1)){
    i <- as.integer(group1[g])
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
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-1
          }
        }
        if(length(t_y)>1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
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
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group1mean-group2mean) < 0.05 | abs(group1mean-group2mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group1mean-group3mean) < 0.05 | abs(group1mean-group3mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group1mean-group4mean) < 0.05 | abs(group1mean-group4mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
  }
  
  for(g in 1:length(group2)){
    i <- as.integer(group2[g])
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
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-1
          }
        }
        if(length(t_y)>1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
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
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group2mean-group1mean) < 0.05 | abs(group2mean-group1mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group2mean-group3mean) < 0.05 | abs(group2mean-group3mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group2mean-group4mean) < 0.05 | abs(group2mean-group4mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
  }
  
  for(g in 1:length(group3)){
    i <- as.integer(group3[g])
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
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-1
          }
        }
        if(length(t_y)>1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
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
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group3mean-group1mean) < 0.05 | abs(group3mean-group1mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group3mean-group2mean) < 0.05 | abs(group3mean-group2mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group3mean-group4mean) < 0.05 | abs(group3mean-group4mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
  }
  
  for(g in 1:length(group3)){
    i <- as.integer(group3[g])
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
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-1
          }
        }
        if(length(t_y)>1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
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
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group4mean-group1mean) < 0.05 | abs(group4mean-group1mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group4mean-group2mean) < 0.05 | abs(group4mean-group2mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
          }
        }
      }
    }
    if(abs(group4mean-group3mean) < 0.05 | abs(group4mean-group3mean) > 0.5){
      if(g_pop_info$child[i]==1){
        t_con   <-which(net[i,which(g_pop_info$child==1)]>0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
      }
      if(g_pop_info$child[i]==0&g_pop_info$old[i]==0){
        t_con   <-which(net[i,]>0&g_pop_info$child==0)
        lt_con  <-length(t_con)
        t_cut   <-rbinom(lt_con,1,0.25)
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
        t_cut    <-rbinom(lt_con,1,0.25)
        net[i,t_con[t_cut==1]]<-net[i,t_con[t_cut==1]]<-cut_to
        t_y<-which(net[i,]==cut_to&g_pop_info$child==0&g_pop_info$old==0)
        if(length(t_y)==1){
          t_con2  <-unique(which(net[t_y,1:(n_Act+n_Bct)]>0))
          if(length(t_con2)){
            net[i,t_con2]  <-net[t_con2,i]<-cut_to
          }
        }
        if(length(t_y)>1){
          t_con2<-unique(which(net[t_y,1:(n_Act+n_Bct)]>0,arr.ind=TRUE)[,2])
          if(length(t_con2)){
            net[i,t_con2]<-net[t_con2,i]<-cut_to
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
