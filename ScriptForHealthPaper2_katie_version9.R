
library(rlist)
library(igraph)
library(boot)

#where functions and parameter csvs are located
sourcefolder <- "."
paramsfolder <- "params"


#where the networks are located
networksfolder <-file.path(sourcefolder,paramsfolder,"networks2")

#where you want to save the results 
outputfolder<-file.path("output59_new","results59_new")
if(!dir.exists(outputfolder)) dir.create(outputfolder,recursive = T) #if output folder doesn't exists, create it.
    
source(file.path(sourcefolder,"FunctionsForHealthPaper_katie_version.R"))

params1   <-read.csv(file.path(sourcefolder,paramsfolder,"model_params.csv"))
params2   <-read.csv(file.path(sourcefolder,paramsfolder,"model_params2.csv"))
params3   <-read.csv(file.path(sourcefolder,paramsfolder,"he_params3.csv")) # params3 has the larger list #
#params3   <-params3[,2:4]
params4   <-read.csv(file.path(sourcefolder,paramsfolder,"group_id_param9.csv"))

net_params <-read.csv(file.path(sourcefolder,paramsfolder,"network_params.csv"))

#h_che<-1

#start loop over networks
for(nt in 1:9){
  for (g in 1){
    # GROUP ID
    g_id_1<-params4[g,2]
    g_id_2<-params4[g,3]
    g_id_3<-params4[g,4]
    g_id_def<-params4[g,5]

par_id <-params1[nt,2]

pop_size   =2000
ncomms     =10
prop_belA  =net_params$prop_belA[par_id]
prop_old   =0.13
prop_young =0.63
prop_child =0.24

g_pop_info <-g_pop_gen(pop_size,ncomms,prop_belA,prop_old,prop_young,prop_child,g_id_1,g_id_2,g_id_3,g_id_def)

############################################

dis_input  <-readRDS(file.path(networksfolder,paste0(params1[nt,3],"net_and_parents.RDS")))
info_input <-readRDS(file.path(networksfolder,paste0(params1[nt,2],"net_and_parents.RDS")))

parents    <-info_input[[2]]
dis_mat    <-dis_input[[1]]
info_mat   <-info_input[[1]]
dis_mat    <-par_ex(g_pop_info=g_pop_info,parents=parents,dis_mat=dis_mat)

############################################

# GROUP LEVEL OF CONCERN ASSOCIATED WITH EACH GROUP TO ALLOW FOR DIFFERENT LEVELS OF CONCERN WITHIN GROUPS
g_con_1<-params4[g,6]
g_con_2<-params4[g,7]
g_con_3<-params4[g,8]
g_con_def<-params4[g,9] 

for(md in 3:77){
# Here we define the prior beliefs of young adults (which will be used as probabilities in a bernoulli draw)
# e.g. currently there is a 50% chance a young adult of political belief A is concerned about the virus
A_concern_y  <-params2[md,2]
B_concern_y  <-params2[md,3]

#Here we define an additive effect of being old (to accommodate the fact they may be more likely to start concerned)
# e.g. There will be a 70% chance of an old adult of political belief A being concerned about the virus
A_concern_o  <-params2[md,4]
B_concern_o  <-params2[md,5]

#Here we define a daily extrinsic input into the belief of each political belief (I figured this would suffice to represent exposure to politicians/news/wider social media)

#N.B. These numbers are already defined on a logit scale. But our starting assumption is that concern of political belief A gets pushed up and political belief B gets pushed down by these extrinsic factors
#(can obviously set to zero if preferred)
lA_ex  <-params2[md,6]
lB_ex  <-params2[md,7]

#Define a linear relationship between proportion of connections concerned (at the previous time-step) and concern levels in young adults
l_conc    <-params2[md,8]
l_conc_o  <-params2[md,9]  #and an additive effect used to calculate the same parameter for old adults

#Define a linear relationship between number of connections infected (at the previous time-step) and concern levels in young adults
l_inf    <-params2[md,10] 
l_inf_o  <-params2[md,11]  #and an additive effect used to calculate the same parameter for old adults

for(r in 1:5){

#Define a threshold relationship whereby concern decreases while all immediate network connections are fully healthy (at the previous time-step) and concern levels in young adults
l_hea    <-params3[27720+(nt-1)*4*77*5+(g-1)*77*5+(md-1)*5+r,3]
l_hea_o  <-params2[md,13] #and an additive effect used to calculate the same parameter for old adults

start    <-concern_setup(A_concern_y,B_concern_y,A_concern_o,B_concern_o,g_pop_info,info_mat,g_con_1,g_con_2,g_con_3,g_con_def)

concern      <-list()
belief       <-list()
belief[[1]]  <-start[[1]]
concern[[1]] <-start[[2]]
group1mean   <-start[[3]]
group2mean   <-start[[4]]
group3mean   <-start[[5]]
group4mean   <-start[[6]]

############################################

#Need to work out R0 based on other parameters
S_E     <- 0.46/mean(colSums(dis_mat)) #Probability of becoming exposed having contacted an infectious                                              individual (daily)
E_I1    <- 4    # lambda for a Poisson draw for the length of this period
yI1_I2  <-0.01    # probability of transitioning to serious case for young (daily)
oI1_I2  <-0.05    # and same for old
yI2_I3  <-0.0125  # prob of transition to critical (HOSPITALISED) case for young (daily)
oI2_I3  <-0.025   # and same for old
yI3_D   <-0.012   # probability of death for young (daily)
oI3_D   <-0.092   # and same for old
yI1_R   <-4     #lambda for a Poisson draw for duration of a pre-symptomatic/mild                                             infection - this is now misnamed as impossible to recover - simply transition to I2.
oI1_R   <-4     #and same for old
yI2_R   <-7.8      #lambda for a Poisson draw for duration of a serious infection
oI2_R   <-7.8     #and same for old
yI3_R   <-4.2     #lambda for a Poisson draw for duration of critical/hospital infection
oI3_R   <-4.2     #and same for old

############################################

#start with 2 infected individuals
exp  <-sample(1:g_pop_info$g_pop,5,replace=FALSE)

#create dataframe to store disease state
S    <-rep(1,g_pop_info$g_pop)
E    <-rep(0,g_pop_info$g_pop)
I1   <-rep(0,g_pop_info$g_pop)
I2   <-rep(0,g_pop_info$g_pop)
I3   <-rep(0,g_pop_info$g_pop)
R    <-rep(0,g_pop_info$g_pop)
D    <-rep(0,g_pop_info$g_pop)
status <-data.frame(S,E,I1,I2,I3,R,D)

status$S[exp]  <-0
status$E[exp]  <-1

d_exp   <-rep(NA,g_pop_info$g_pop)
d_inf1  <-rep(NA,g_pop_info$g_pop)
d_inf2  <-rep(NA,g_pop_info$g_pop)
d_inf3  <-rep(NA,g_pop_info$g_pop)

d_exp[status$E==1]  <-rpois(sum(status$E),E_I1)

time           <-300
statuses       <-list()
statuses[[1]]  <-status

progression      <-matrix(0,nr=time+1,nc=ncol(status))
progression[1,]  <-colSums(status)


############################################

#be ready to change the network in this
for(t in 2:time){
  
  if(t==2){
    
    dis_mat  <-network_rewire_concern(net=dis_mat,concern=start[[2]],concern.prev=rep(0,g_pop_info$g_pop),g_pop_info=g_pop_info,prop_cutg=0.25,prop_cut=0.5,
                                      cut_to=0.001,group1mean=start[[3]],group2mean=start[[4]],group3mean=start[[5]],group4mean=start[[6]])
    
    dis_mat  <-network_rewire_infectionS(net=dis_mat,status=statuses[[t-1]],
                                       g_pop_info=g_pop_info,cut_to=0.001)
    #dis_mat<-network_rewire_infectionM(net=dis_mat,status=statuses[[t-1]],
    #                                   g_pop_info=g_pop_info,cut_to=0.001)
    dis_mat  <-network_rewire_infectionR(net=dis_mat,status=statuses[[t-1]],
                                       g_pop_info=g_pop_info,cut_to=0.001)
    
    inf      <-cbind(sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3),
                  sign(statuses[[t-1]]$I2  +statuses[[t-1]]$I3))
    
    current  <-concern_timestep(g_pop_info=g_pop_info,net_b=info_mat,net_d=dis_mat,
                                 belief=start[[1]],concern=start[[2]],inf=inf,g_con_1,g_con_2,g_con_3,g_con_def,
                                 lA_ex,lB_ex,l_conc,l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o)
    
    belief[[2]]  <-current[[1]]
    concern[[2]] <-current[[2]]
    
    dis  <-infection_timestep(g_pop_info=g_pop_info,status=statuses[[t-1]],net=dis_mat,
                              d_exp=d_exp,d_inf1=d_inf1,d_inf2=d_inf2,d_inf3=d_inf3,
                              S_E=S_E,E_I1=E_I1,yI1_I2=yI1_I2,oI1_I2=oI1_I2,
                              yI2_I3=yI2_I3,oI2_I3=oI2_I3,yI3_D=yI3_D,oI3_D=oI3_D,
                              yI1_R=yI1_R,oI1_R=oI1_R,yI2_R=yI2_R,oI2_R=oI2_R,
                              yI3_R=yI3_R,oI3_R=oI3_R)
    
  }
  if(t>2){
    
    dis_mat  <-network_rewire_concern(net=dis_mat,concern=current[[2]],
                                      concern.prev=concern[[t-2]],
                                      g_pop_info=g_pop_info,prop_cut=0.5,prop_cutg=0.25,,cut_to=0.001,
                                      group1mean=start[[3]],group2mean=start[[4]],group3mean=start[[5]],group4mean=start[[6]])
    dis_mat  <-network_rewire_infectionS(net=dis_mat,status=statuses[[t-1]],
                                       g_pop_info=g_pop_info,cut_to=0.001)
    #dis_mat<-network_rewire_infectionM(net=dis_mat,status=statuses[[t-1]],
    #                                   g_pop_info=g_pop_info,cut_to=0.001)
    dis_mat  <-network_rewire_infectionR(net=dis_mat,status=statuses[[t-1]],
                                       g_pop_info=g_pop_info,cut_to=0.001)
    
    inf      <-cbind(sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3),
                     sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3))
    current  <-concern_timestep(g_pop_info=g_pop_info,net_b=info_mat,net_d=dis_mat,
                                 belief=current[[1]],concern=current[[2]],inf=inf,
                                 g_con_1,g_con_2,g_con_3,g_con_def,lA_ex,lB_ex,l_conc,
                                 l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o)
    
    belief[[t]]  <-current[[1]]
    concern[[t]] <-current[[2]]  
    
    dis       <-infection_timestep(g_pop_info=g_pop_info,status=statuses[[t-1]],
                                   net=dis_mat,d_exp=dis[[2]],d_inf1=dis[[3]],
                                   d_inf2=dis[[4]],d_inf3=dis[[5]],S_E=S_E,E_I1=E_I1,
                                   yI1_I2=yI1_I2,oI1_I2=oI1_I2,yI2_I3=yI2_I3,
                                   oI2_I3=oI2_I3,yI3_D=yI3_D,oI3_D=oI3_D,yI1_R=yI1_R,
                                   oI1_R=oI1_R,yI2_R=yI2_R,oI2_R=oI2_R,yI3_R=yI3_R,
                                   oI3_R=oI3_R)
  }
  #print(colSums(dis$status))
  progression[t,]  <-colSums(dis$status)
  statuses[[t]]    <-dis$status
  
  if(sum(colSums(statuses[[t]])[c(2,3,4,5)])==0){break()}
  
}


########################################
########################################

mod_concerns  <-matrix(0,nr=10,nc=length(concern))
mod_exps      <-matrix(0,nr=10,nc=length(statuses))
mod_infs      <-matrix(0,nr=10,nc=length(statuses))
mod_hosps     <-matrix(0,nr=10,nc=length(statuses))

for(i in 1:length(concern)){
  mod_concerns[,i]  <-aggregate(concern[[i]],by=list(g_pop_info$g_comms),mean)[,2]
}

for(i in 1:length(statuses)){
  mod_exps[,i]      <-aggregate(statuses[[i]][,2],by=list(g_pop_info$g_comms),sum)[,2]
}

for(i in 1:length(statuses)){
  mod_infs[,i]      <-aggregate(statuses[[i]][,4],by=list(g_pop_info$g_comms),sum)[,2]
}

for(i in 1:length(statuses)){
  mod_hosps[,i]     <-aggregate(statuses[[i]][,5],by=list(g_pop_info$g_comms),sum)[,2]
}

OUT          <-list(mod_concerns,mod_exps,mod_infs,mod_hosps,l_hea)
names(OUT)   <-c("concern","exps","infs","hosps","he")

saveRDS(OUT, file.path(outputfolder,paste0("nets",params1$NetSelect[nt],"mods",params2$ModSelect[md],"group",9,"rep",r,".RDS")))


mod_group_concerns  <-matrix(0,nr=4,nc=length(concern))
mod_group_exps      <-matrix(0,nr=4,nc=length(statuses))
mod_group_infs      <-matrix(0,nr=4,nc=length(statuses))
mod_group_hosps     <-matrix(0,nr=4,nc=length(statuses))

for(i in 1:length(concern)){
  mod_group_concerns[,i]  <-aggregate(concern[[i]],by=list(g_pop_info$group_num),mean)[,2]
}

for(i in 1:length(statuses)){
  mod_group_exps[,i]      <-aggregate(statuses[[i]][,2],by=list(g_pop_info$group_num),sum)[,2]
}

for(i in 1:length(statuses)){
  mod_group_infs[,i]      <-aggregate(statuses[[i]][,4],by=list(g_pop_info$group_num),sum)[,2]
}

for(i in 1:length(statuses)){
  mod_group_hosps[,i]     <-aggregate(statuses[[i]][,5],by=list(g_pop_info$group_num),sum)[,2]
}

OUT_G          <-list(mod_group_concerns,mod_group_exps,mod_group_infs,mod_group_hosps,l_hea)
names(OUT_G)   <-c("concern","exps","infs","hosps","he")

saveRDS(OUT_G, file.path(outputfolder,paste0("group_nets",params1$NetSelect[nt],"mods",params2$ModSelect[md],"group",9,"rep",r,".RDS")))

###################################
###################################

cols=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
# 
# plot(NULL,xlim=c(0,250),ylim=c(0,1))
# for(i in 1:10){
#   lines(x=seq(1,length(concern)),y=mod_concerns[i,],col=cols[i],lwd=3)
# }
# 
# plot(NULL,xlim=c(0,250),ylim=c(0,50))
# for(i in 1:10){
#   lines(x=seq(1,length(statuses)),y=mod_infs[i,],col=cols[i],lwd=3)
# }
# 
# plot(NULL,xlim=c(0,250),ylim=c(0,1))
# for(i in 1:4){
#   lines(x=seq(1,length(concern)),y=mod_group_concerns[i,],col=cols[i],lwd=3)
# }
# 
# plot(NULL,xlim=c(0,250),ylim=c(0,50))
# for(i in 1:4){
#   lines(x=seq(1,length(statuses)),y=mod_group_infs[i,],col=cols[i],lwd=3)
# }


print(r)
} #end r loop

print(md)
} #end md loop

} #end g loop

} #end nt loop 






