

##Load packages
library(lme4)
library(viridis)
library(car)
library(boot)
library(igraph)
library(scatterplot3d)
library(sjPlot)
library(TDAmapper)
library(tibble)
library(tidyr)
library(dplyr)
#install.packages("sjPlot")

##Define functions used in generation of dataframe

min_finder<-function(object){
  return(apply(object,1,min))
}

whichmin_finder<-function(object){
  return(apply(object,1,which.min))
}

start_finder<-function(object){
  return(as.numeric(object[,1]>0))
}

max_finder<-function(object){
  return(apply(object,1,max))
}

whichmax_finder<-function(object){
  return(apply(object,1,which.max))
}

ep_start<-function(input,thresh=5){
  return(min(which(input>(thresh-1))))
}

es_finder<-function(object){
  return(apply(object,1,ep_start))
}

##################################################################################

##Set path to parameter files - needs to be done by user
sourcefolder <- "."
paramsfolder <- "params"

setwd("C:/Users/katie/Dropbox/Katie B/PIP paper/CoupledDynamicsNetworkPaper-master")
source(file.path(sourcefolder,"FunctionsForHealthPaper_katie_version.R"))

##read in parameter files
params1<-read.csv(file.path(paramsfolder,"model_params.csv"))
params2<-read.csv(file.path(paramsfolder,"model_params2.csv"))
params4<-read.csv(file.path(paramsfolder,"group_id_param.csv"))

##set path to results files - needs to be done by user
outputfolder1<-file.path("output","results4")
outputfolder2<-file.path("output59","results59")
outputfolder3<-file.path("output1014","results1014")

##################################################################################

##read in results : community
conc_res  <-list()
exp_res   <-list()
inf_res   <-list()
hosp_res  <-list()
he        <-list()
network   <-list()
mod       <-list()
group     <-list()
rep       <-list()
c<-1
for(nt in c(1,2,3,4,5,6,7,8,9)){
  for(md in seq(3,77,1)){
    for(r in 1:5){
      for (g in 1:9){
        if (g %in% c(1,2,3,4)){
          tmp_in<-readRDS(paste0(outputfolder1,"/nets",params1$NetSelect[nt],"mods",params2$ModSelect[md],"rep",r,g,".RDS"))
        }
        if (g %in% c(5,6,7,8,9)){
          tmp_in<-readRDS(paste0(outputfolder2,"/nets",params1$NetSelect[nt],"mods",params2$ModSelect[md],"group",g,"rep",r,".RDS"))
        }
        # if (g %in% c(10,11,12,13,14)) {
        #   tmp_in<-readRDS(paste0(outputfolder3,"/nets",params1$NetSelect[nt],"mods",params2$ModSelect[md],"group",g,"ep",r,".RDS"))
        #   }
      network[[c]]<-paste0("nets",params1$NetSelect[nt])
      mod[[c]]<-paste0("mods",params2$ModSelect[md])
      group[[c]]<-paste0("group",g)
      rep[[c]]<-paste0("rep",r)
      conc_res[[c]]<-tmp_in[[1]]
      exp_res[[c]]<-tmp_in[[2]]
      inf_res[[c]]<-tmp_in[[3]]
      hosp_res[[c]]<-tmp_in[[4]]
      he[[c]]<-tmp_in[[5]]
      c<-c+1
      }
    }
  }
}

##read in results : group
group_conc_res  <-list()
group_exp_res   <-list()
group_inf_res   <-list()
group_hosp_res  <-list()
group_he        <-list()
group_network   <-list()
group_mod       <-list()
group_group     <-list()
group_rep       <-list()

c<-1
for(nt in c(1,2,3,4,5,6,7,8,9)){
  for(md in seq(3,77,1)){
    for(r in 1:5){
      for (g in 1:9){
        if (g %in% c(1,2,3,4)){
          tmp_in<-readRDS(paste0(outputfolder1,"/group_nets",params1$NetSelect[nt],"mods",params2$ModSelect[md],"rep",r,g,".RDS"))
        }
        if (g %in% c(5,6,7,8,9)){
          tmp_in<-readRDS(paste0(outputfolder2,"/group_nets",params1$NetSelect[nt],"mods",params2$ModSelect[md],"group",g,"rep",r,".RDS"))
        }
        # if (g %in% c(10,11,12,13,14)) {
        #   tmp_in<-readRDS(paste0(outputfolder3,"/group_nets",params1$NetSelect[nt],"mods",params2$ModSelect[md],"group",g,"ep",r,".RDS"))
        # }
      group_network[[c]]<-paste0("nets",params1$NetSelect[nt])
      group_mod[[c]]<-paste0("mods",params2$ModSelect[md])
      group_group[[c]]<-paste0("group",g)
      group_rep[[c]]<-paste0("rep",r)
      group_conc_res[[c]]<-tmp_in[[1]]
      group_exp_res[[c]]<-tmp_in[[2]]
      group_inf_res[[c]]<-tmp_in[[3]]
      group_hosp_res[[c]]<-tmp_in[[4]]
      group_he[[c]]<-tmp_in[[5]]
      c<-c+1
      }
    }
  }
}

##################################################################################

#set up dataframe of summary results 
net    <-seq(1,9,1)
params <-seq(3,77,1)
reps   <-seq(1,5,1)
mods   <-seq(1,10,1)
groups <- seq(1,9,1)

net2     <-rep(net,each=length(params)*length(reps)*length(mods)*length(groups))
params2  <-rep(rep(params,each=length(reps)*length(mods)*length(groups)),length(net))
reps2    <-rep(rep(reps,each=length(mods)*length(groups)),length(params)*length(net))
groups2  <-rep(rep(groups,each=length(mods)),length(params)*length(reps)*length(net))
mods2    <-rep(mods,length(groups)*length(params)*length(reps)*length(net))

  cdat<-data.frame(net2,params2,reps2,groups2,mods2)
  
net    <-seq(1,9,1)
params <-seq(3,77,1)
reps   <-seq(1,5,1)
mods   <-seq(1,4,1)
groups <- seq(1,9,1)
  
net2     <-rep(net,each=length(params)*length(reps)*length(mods)*length(groups))
params2  <-rep(rep(params,each=length(reps)*length(mods)*length(groups)),length(net))
reps2    <-rep(rep(reps,each=length(mods)*length(groups)),length(params)*length(net))
groups2  <-rep(rep(groups,each=length(mods)),length(params)*length(reps)*length(net))
mods2    <-rep(mods,length(groups)*length(params)*length(reps)*length(net))
gdat     <-data.frame(net2,params2,reps2,groups2,mods2)

# community
cdat$MinConc        <-unlist(lapply(conc_res, min_finder))
cdat$WhichMinConc   <-unlist(lapply(conc_res, whichmin_finder))
cdat$Seeded         <-unlist(lapply(exp_res, start_finder))
cdat$EpStartExp     <-unlist(lapply(exp_res, es_finder))
cdat$MaxExp         <-unlist(lapply(exp_res, max_finder))
cdat$WhichMaxExp    <-unlist(lapply(exp_res, whichmax_finder))
cdat$EpStartInf     <-unlist(lapply(inf_res, es_finder))
cdat$MaxInf         <-unlist(lapply(inf_res, max_finder))
cdat$WhichMaxInf    <-unlist(lapply(inf_res, whichmax_finder))
cdat$MaxHosp        <-unlist(lapply(hosp_res, max_finder))
cdat$WhichMaxHosp   <-unlist(lapply(hosp_res, whichmax_finder))

# group
gdat$MinConc        <-unlist(lapply(group_conc_res, min_finder))
gdat$WhichMinConc   <-unlist(lapply(group_conc_res, whichmin_finder))
gdat$Seeded         <-unlist(lapply(group_exp_res, start_finder))
gdat$EpStartExp     <-unlist(lapply(group_exp_res, es_finder))
gdat$MaxExp         <-unlist(lapply(group_exp_res, max_finder))
gdat$WhichMaxExp    <-unlist(lapply(group_exp_res, whichmax_finder))
gdat$EpStartInf     <-unlist(lapply(group_inf_res, es_finder))
gdat$MaxInf         <-unlist(lapply(group_inf_res, max_finder))
gdat$WhichMaxInf    <-unlist(lapply(group_inf_res, whichmax_finder))
gdat$MaxHosp        <-unlist(lapply(group_hosp_res, max_finder))
gdat$WhichMaxHosp   <-unlist(lapply(group_hosp_res, whichmax_finder))

##names of the first 4 dataframe columns

names(cdat)[1:5] <-c("NetworkID","ParameterID","Rep","Group_Struct","Community")
names(gdat)[1:5] <-c("NetworkID","ParameterID","Rep","Group_Struct","Community")

##Add reassurance effect to the dataframe
cdat$he <-rep(unlist(he),each=10)
gdat$he <-rep(unlist(group_he),each=4)

##Set situations where epidemics failed to start to NA
cdat$EpStartExp[cdat$EpStartExp==Inf]<-NA
cdat$EpStartInf[cdat$EpStartInf==Inf]<-NA

gdat$EpStartExp[gdat$EpStartExp==Inf]<-NA
gdat$EpStartInf[gdat$EpStartInf==Inf]<-NA

##Add column to use an identifier of each simulation run
cdat$r_eff <-paste0("N",cdat$NetworkID,"P",cdat$ParameterID,"R",cdat$Rep)
gdat$r_eff <-paste0("N",gdat$NetworkID,"P",gdat$ParameterID,"R",gdat$Rep)

##Add column and related information to have colours associated with the reassurance effect 
cdat$he_col<-rep(NA,nrow(cdat))
he_pal<-plasma(20)
he_qs<-quantile(cdat$he,probs=seq(0,1,length.out=21))
for(i in 1:nrow(cdat)){
  cdat$he_col[i]<-sum(cdat$he[i]<=he_qs)
}

gdat$he_col<-rep(NA,nrow(gdat))
he_pal<-plasma(20)
he_qs<-quantile(gdat$he,probs=seq(0,1,length.out=21))
for(i in 1:nrow(gdat)){
  gdat$he_col[i]<-sum(gdat$he[i]<=he_qs)
}


#Add columns for mean-centered reassurance effect and epidemic start day
cdat$mche<-(cdat$he-mean(cdat$he))/sd(cdat$he)
cdat$mcEpStartInf<-cdat$EpStartInf-mean(cdat$EpStartInf,na.rm=T)

gdat$mche<-(gdat$he-mean(gdat$he))/sd(gdat$he)
gdat$mcEpStartInf<-gdat$EpStartInf-mean(gdat$EpStartInf,na.rm=T)

##################################################################################

##need to read in one of the parameter files again due to naming issues
##I need both because of how I coded stuff below
p2<-read.csv(paste0(paramsfolder,"/model_params2.csv"))
params2<-read.csv(paste0(paramsfolder,"/model_params2.csv"))

##################################################################################

##Create separate dataframes for high, intermediate and low starting levels of concern/adherence
cdat_high<-cdat[p2[cdat$ParameterID,2]==0.5,]
cdat_mid<-cdat[p2[cdat$ParameterID,2]==0.2,]
cdat_low<-cdat[p2[cdat$ParameterID,2]==0.05,]

gdat_high<-gdat[p2[gdat$ParameterID,2]==0.5,]
gdat_mid<-gdat[p2[gdat$ParameterID,2]==0.2,]
gdat_low<-gdat[p2[gdat$ParameterID,2]==0.05,]

##Create separate dataframes for high, intermediate and low group concern/adherence
cdat_grouphigh<-cdat[params4[cdat$Group_Struct,6]&params4[cdat$Group_Struct,7]==0.35,]
cdat_groupmid<-cdat[params4[cdat$Group_Struct,6]&params4[cdat$Group_Struct,7]==0.2,]
cdat_grouplow<-cdat[params4[cdat$Group_Struct,6]&params4[cdat$Group_Struct,7]==0.05,]
cdat_grouphighmix<-cdat[params4[cdat$Group_Struct,6]==0.35&params4[cdat$Group_Struct,7]==0.2,]
cdat_grouplowmix<-cdat[params4[cdat$Group_Struct,6]==0&params4[cdat$Group_Struct,7]==0.05,]

gdat_grouphigh<-gdat[params4[gdat$Group_Struct,6]&params4[gdat$Group_Struct,7]==0.35,]
gdat_groupmid<-gdat[params4[gdat$Group_Struct,6]&params4[gdat$Group_Struct,7]==0.2,]
gdat_grouplow<-gdat[params4[gdat$Group_Struct,6]&params4[gdat$Group_Struct,7]==0.05,]
gdat_grouphighmix<-gdat[params4[gdat$Group_Struct,6]==0.35&params4[gdat$Group_Struct,7]==0.2,]
gdat_grouplowmix<-gdat[params4[gdat$Group_Struct,6]==0&params4[gdat$Group_Struct,7]==0.05,]

##################################################################################

########### for initial high concern ################

##Add columns for severity and logit-transformed severity to the high dataset
cdat_high$sev<-(cdat_high$MaxInf/max(cdat_high$MaxInf))*(1-(cdat_high$WhichMaxInf-cdat_high$EpStartInf)/max(cdat_high$WhichMaxInf-cdat_high$EpStartInf,na.rm=TRUE))
cdat_high$lt_sev<-car::logit(cdat_high$sev)

#Fit statistical model to logit severity as used in the main text
cmod_nohe<-lmer(lt_sev~as.factor(NetworkID)*EpStartInf+as.factor(params4[Group_Struct,6])*as.factor(p2[ParameterID,8])*as.factor(p2[ParameterID,10])*EpStartInf+
                  (EpStartInf|r_eff),data=cdat_high[is.na(cdat_high$sev)==FALSE,])

##Add columns for severity and logit-transformed severity to the high dataset 
gdat_high$sev<-(gdat_high$MaxInf/max(gdat_high$MaxInf))*(1-(gdat_high$WhichMaxInf-gdat_high$EpStartInf)/max(gdat_high$WhichMaxInf-gdat_high$EpStartInf,na.rm=TRUE))
gdat_high$lt_sev<-car::logit(gdat_high$sev)

#Fit statistical model to logit severity as used in the main text
gmod_nohe<-lmer(lt_sev~as.factor(NetworkID)*EpStartInf+as.factor(params4[Group_Struct,6])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                  (EpStartInf|r_eff),data=gdat_high[is.na(gdat_high$sev)==FALSE,])

#############################

############ for group high concern ################ p2[ParameterID,8] is social concern and p2[ParameterID,10] is infected concern
# using this way allows us to "choose" low and high starting concern to compare with low and high group concern ##

##Add columns for severity and logit-transformed severity to the high dataset
cdat_grouphigh$sev<-(cdat_grouphigh$MaxInf/max(cdat_grouphigh$MaxInf))*(1-(cdat_grouphigh$WhichMaxInf-cdat_grouphigh$EpStartInf)/max(cdat_grouphigh$WhichMaxInf-cdat_grouphigh$EpStartInf,na.rm=TRUE))
cdat_grouphigh$lt_sev<-car::logit(cdat_grouphigh$sev)

#Fit statistical model to logit severity as used in the main text
cmod_nohe_grouphigh<-lmer(lt_sev~as.factor(NetworkID)*EpStartInf+as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                                       (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE,])

##Add columns for severity and logit-transformed severity to the high dataset 
gdat_grouphigh$sev<-(gdat_grouphigh$MaxInf/max(gdat_grouphigh$MaxInf))*(1-(gdat_grouphigh$WhichMaxInf-gdat_grouphigh$EpStartInf)/max(gdat_grouphigh$WhichMaxInf-gdat_grouphigh$EpStartInf,na.rm=TRUE))
gdat_grouphigh$lt_sev<-car::logit(gdat_grouphigh$sev)

#Fit statistical model to logit severity as used in the main text
gmod_nohe_grouphigh<-lmer(lt_sev~as.factor(NetworkID)*EpStartInf+as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                            (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE,])

############ for group low concern ################ p2[ParameterID,8] is social concern and p2[ParameterID,10] is infected concern


##Add columns for severity and logit-transformed severity to the high dataset
cdat_grouplow$sev<-(cdat_grouplow$MaxInf/max(cdat_grouplow$MaxInf))*(1-(cdat_grouplow$WhichMaxInf-cdat_grouplow$EpStartInf)/max(cdat_grouplow$WhichMaxInf-cdat_grouplow$EpStartInf,na.rm=TRUE))
cdat_grouplow$lt_sev<-car::logit(cdat_grouplow$sev)

#Fit statistical model to logit severity as used in the main text
cmod_nohe_grouplow<-lmer(lt_sev~as.factor(NetworkID)*EpStartInf+as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                            (EpStartInf|r_eff),data=cdat_grouplow[is.na(cdat_grouplow$sev)==FALSE,])

##Add columns for severity and logit-transformed severity to the high dataset 
gdat_grouplow$sev<-(gdat_grouplow$MaxInf/max(gdat_grouplow$MaxInf))*(1-(gdat_grouplow$WhichMaxInf-gdat_grouplow$EpStartInf)/max(gdat_grouplow$WhichMaxInf-gdat_grouplow$EpStartInf,na.rm=TRUE))
gdat_grouplow$lt_sev<-car::logit(gdat_grouplow$sev)

#Fit statistical model to logit severity as used in the main text
gmod_nohe_grouplow<-lmer(lt_sev~as.factor(NetworkID)*EpStartInf+ as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                            (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouplow$sev)==FALSE,])

##################################################################################



############## TDA mapper ################
cdat1<-cdat_grouphigh %>% filter(NetworkID %in% c("1","2","3")) %>% drop_na()

# Define two filter functions
filter_function <- cdat1[,6]  # min concern
variable1 <- cdat1[,22]   # severity
variable2 <- cdat1[,17]   # reassurance
variable3 <- cdat1[,12]   # max infected
variable4 <- cdat1[,1]    # network

# Compute Mapper output using mapper2D
mapper_output <- mapper1D(
  distance_matrix = dist(cdat1),        # Distance matrix
  filter_values = filter_function, # Two filter functions
  num_intervals = 12,             # Number of intervals for each filter
  percent_overlap = 20,         # Overlap percentage for each filter
  num_bins_when_clustering = 12         # Number of bins for clustering
)

# Optional: Calculate node attributes (e.g., mean values for nodes)
node_means <- sapply(mapper_output$points_in_vertex, function(indices) {
  mean(filter_function[indices], na.rm = TRUE)  # Mean of the first filter function
})

# Calculate the mean of the variable for each node (for size)
node_means_variable <- sapply(mapper_output$points_in_vertex, function(indices) {
  if (length(indices) == 0) return(NA)  # Handle empty nodes
  mean(variable1[indices], na.rm = TRUE)  # Mean of variable
})

#Determine the dominant value of the binary variable for each node (for shape)
# node_binary_shape <- sapply(mapper_output$points_in_vertex, function(indices) {
#   if (length(indices) == 0) return(NA)  # Handle empty nodes
#   round(mean(variable4[indices], na.rm = TRUE))  # Binary outcome (0 or 1)
# })


# Generate a color palette for the filter function mean
library(RColorBrewer)
palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(node_means_filter))

# Map the filter function means to colors
node_colors <- palette[rank(node_means_variable, na.last = "keep")]

# Scale the node sizes based on the variable mean
node_sizes <- scales::rescale(node_means, to = c(5, 15))  # Adjust size range

# Map the binary variable to node shapes
#node_shapes <- ifelse(node_binary_shape == 1, "square", "circle")

# Create the graph
library(igraph)
graph <- graph.adjacency(mapper_output$adjacency, mode = "undirected")

# Plot the graph with custom shapes
plot(
  graph,
  vertex.size = node_sizes,
  vertex.color = node_colors,
  vertex.label = NA,
  main = "Mapper Graph with Minimum Concern and Epidemic Severity"
)

# Add a legend for the node color, size, and shape
legend(
  "topright", 
  legend = round(seq(min(node_means_variable, na.rm = TRUE), 
                     max(node_means_variable, na.rm = TRUE), length.out = 5), 2),
  fill = colorRampPalette(brewer.pal(9, "YlOrRd"))(5),
  title = "Severity",
  bty = "n",
  cex=1.25
)
legend(
  "right", 
  legend = c("Low", "High"),
  pt.cex = c(1, 2),  # Adjust based on size scale
  pch = 21, 
  col = "black",
  pt.bg = "gray",
  title = "Mean Concern",
  bty = "n",
  cex=1.25
)
legend(
  "topleft",
  legend = c("Circle (0)", "Square (1)"),
  pch = c(21, 15),  # Circle and square
  col = "black",
  title = "Node Shape (Binary Variable)"
)


###### 2d mapper ##########
cdat1<-cdat_grouphigh %>% filter(NetworkID %in% c("1","2","3")) %>% drop_na()

# Define two filter functions
filter_function <- cdat1[,6]  # min concern
variable1 <- cdat1[,22]   # severity
variable2 <- cdat1[,17]   # reassurance
variable3 <- cdat1[,12]   # max infected
variable4 <- cdat1[,1]    # network

# Compute Mapper output using mapper2D
mapper_output <- mapper2D(
  distance_matrix = dist(cdat1),        # Distance matrix
  filter_values = list(filter_function,variable3), # Two filter functions
  num_intervals = c(12,12),             # Number of intervals for each filter
  percent_overlap = c(20,20),         # Overlap percentage for each filter
  num_bins_when_clustering = 12         # Number of bins for clustering
)

# Optional: Calculate node attributes (e.g., mean values for nodes)
node_means <- sapply(mapper_output$points_in_vertex, function(indices) {
  mean(filter_function[indices], na.rm = TRUE)  # Mean of the first filter function
})

# Calculate the mean of the variable for each node (for size)
node_means_variable <- sapply(mapper_output$points_in_vertex, function(indices) {
  if (length(indices) == 0) return(NA)  # Handle empty nodes
  mean(variable2[indices], na.rm = TRUE)  # Mean of variable
})

# Generate a color palette for the filter function mean
library(RColorBrewer)
palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(node_means))

# Map the filter function means to colors
node_colors <- palette[rank(node_means, na.last = "keep")]

# Scale the node sizes based on the variable mean
node_sizes <- scales::rescale(node_means_variable, to = c(5, 15))  # Adjust size range

# Create the graph
library(igraph)
graph <- graph.adjacency(mapper_output$adjacency, mode = "undirected")

# Plot the graph with custom shapes
plot(
  graph,
  vertex.size = node_sizes,
  vertex.color = node_colors,
  vertex.label = NA,
  main = "Mapper Graph with Node Color, Size, and Shape"
)
legend(
  "topright", 
  legend = round(seq(min(node_means, na.rm = TRUE), 
                     max(node_means, na.rm = TRUE), length.out = 5), 2),
  fill = colorRampPalette(brewer.pal(9, "YlOrRd"))(5),
  title = "Minimum Concern Mean",
  bty = "n"
)
legend(
  "bottomright", 
  legend = c("Low", "High"),
  pt.cex = c(1, 2),  # Adjust based on size scale
  pch = 21, 
  col = "black",
  pt.bg = "gray",
  title = "Severity",
  bty = "n"
)
legend(
  "topleft",
  legend = c("Circle (0)", "Square (1)"),
  pch = c(21, 15),  # Circle and square
  col = "black",
  title = "Node Shape (Binary Variable)"
)







######## Example ##############

data<-read.csv("breast-cancer.csv")
data<-data[1:12]

for (i in 1:nrow(data)){
  if(data[i,2]=="M"){
    data[i,2]<-1
  }
  else(data[i,2]<-0)
}

# Define two filter functions
filter_function <- data[,6]  # min concern
variable1       <- as.numeric(data[,2])

# Compute Mapper output using mapper2D
mapper_output <- mapper1D(
  distance_matrix = dist(data),        # Distance matrix
  filter_values = filter_function, # Two filter functions
  num_intervals = 10,             # Number of intervals for each filter
  percent_overlap = 30,         # Overlap percentage for each filter
  num_bins_when_clustering = 10         # Number of bins for clustering
)

# Calculate the mean of the filter function for each node (for color)
node_means_filter <- sapply(mapper_output$points_in_vertex, function(indices) {
  if (length(indices) == 0) return(NA)  # Handle empty nodes
  mean(filter_function[indices], na.rm = TRUE)  # Mean of filter function
})

# Determine the dominant value of the binary variable for each node (for shape)
node_binary_shape <- sapply(mapper_output$points_in_vertex, function(indices) {
  if (length(indices) == 0) return(NA)  # Handle empty nodes
  round(mean(variable1[indices], na.rm = TRUE))  # Binary outcome (0 or 1)
})

# Generate a color palette for the filter function mean
library(RColorBrewer)
palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(node_means_filter))

# Map the filter function means to colors
node_colors <- ifelse(node_binary_shape == 1, "red", "blue")

# Map the binary variable to node shapes
#node_shapes <- ifelse(node_binary_shape == 1, "square", "circle")

node_sizes <- scales::rescale(node_means_filter, to = c(5, 15)) 

# Create the graph
library(igraph)
graph <- graph.adjacency(mapper_output$adjacency, mode = "undirected")

# Plot the graph with custom shapes
plot(
  graph,
  vertex.color = node_colors,
  vertex.label= NA,
  vertex.size=node_sizes,
  main = "Mapper Graph with Node Color, Size, and Shape"
)

# Add a legend for the node color, size, and shape
legend(
  "topright", 
  legend = c("Malignant","Benign"),
  fill = c("red","blue"),
  title = "Diagnosis",
  bty = "n"
)
# legend(
#   "topleft",
#   legend = c("Circle (0)", "Square (1)"),
#   pch = c(21, 15),  # Circle and square
#   col = "black",
#   title = "Node Shape (Binary Variable)"
# )
legend(
  "bottomright", 
  legend = c("Small", "Large"),
  pt.cex = c(1, 2),  # Adjust based on size scale
  pch = 21, 
  col = "black",
  pt.bg = "gray",
  title = " Area of Tumor"
)




















###Code to generate Figure 5


########################
# Community : high starting level of concern

c_tdat<-cdat_high[is.na(cdat_high$sev)==FALSE,]
c_the<-aggregate(c_tdat$he,by=list(c_tdat$r_eff),mean)

par(mfrow=c(1,2),mar=c(5,5,2,2)) #this puts two graphs in one picture 
plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

randoms<-ranef(cmod_nohe)$r_eff

# here A and B refers to the waek and strong social construction of concern in the paper not predisposition
randomsA<-randoms[rownames(randoms)%in%cdat_high$r_eff[p2[cdat_high$ParameterID,8]<0.5],]
randomsB<-randoms[rownames(randoms)%in%cdat_high$r_eff[p2[cdat_high$ParameterID,8]>0.49],]

theA<-c_the[rownames(randoms)%in%cdat_high$r_eff[p2[cdat_high$ParameterID,8]<0.5],]
theB<-c_the[rownames(randoms)%in%cdat_high$r_eff[p2[cdat_high$ParameterID,8]>0.49],]

x<-seq(1,300,0.01)

for(i in 1:nrow(randomsA)){
  x<-seq(min(cdat_high$EpStartInf[cdat_high$r_eff==rownames(randomsA)[i]],na.rm=T),max(cdat_high$EpStartInf[cdat_high$r_eff==rownames(randomsA)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(cmod_nohe)[1]+fixef(cmod_nohe)[11]+randomsA[i,1]+(fixef(cmod_nohe)[10]+fixef(cmod_nohe)[27]+randomsA[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[cdat$he_col[cdat$he==theA[i,2]]])
}


plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

for(i in 1:nrow(randomsB)){
  x<-seq(min(cdat_high$EpStartInf[cdat_high$r_eff==rownames(randomsB)[i]],na.rm=T),max(cdat_high$EpStartInf[cdat_high$r_eff==rownames(randomsB)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(cmod_nohe)[1]+fixef(cmod_nohe)[14]+randomsB[i,1]+(fixef(cmod_nohe)[10]+fixef(cmod_nohe)[30]+randomsB[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[cdat$he_col[cdat$he==theB[i,2]]])
}

# Group : high level of inital concern
g_tdat<-gdat_high[is.na(gdat_high$sev)==FALSE,]
g_the<-aggregate(g_tdat$he,by=list(g_tdat$r_eff),mean)

########################

par(mfrow=c(1,2),mar=c(5,5,2,2)) #this puts two graphs in one picture 
plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

randoms<-ranef(gmod_nohe)$r_eff

# here A and B refers to the waek and strong social construction of concern in the paper not predisposition
randomsA<-randoms[rownames(randoms)%in%gdat_high$r_eff[p2[gdat_high$ParameterID,8]>0.5],]
randomsB<-randoms[rownames(randoms)%in%gdat_high$r_eff[p2[gdat_high$ParameterID,8]<0.49],]

theA<-g_the[rownames(randoms)%in%gdat_high$r_eff[p2[gdat_high$ParameterID,8]>0.5],]
theB<-g_the[rownames(randoms)%in%gdat_high$r_eff[p2[gdat_high$ParameterID,8]<0.49],]

x<-seq(1,300,0.01)

for(i in 1:nrow(randomsA)){
  x<-seq(min(gdat_high$EpStartInf[gdat_high$r_eff==rownames(randomsA)[i]],na.rm=T),max(gdat_high$EpStartInf[gdat_high$r_eff==rownames(randomsA)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(gmod_nohe)[1]+fixef(gmod_nohe)[11]+randomsA[i,1]+(fixef(gmod_nohe)[10]+fixef(gmod_nohe)[27]+randomsA[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[gdat$he_col[gdat$he==theA[i,2]]])
}


plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

for(i in 1:nrow(randomsB)){
  x<-seq(min(gdat_high$EpStartInf[gdat_high$r_eff==rownames(randomsB)[i]],na.rm=T),max(gdat_high$EpStartInf[gdat_high$r_eff==rownames(randomsB)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(gmod_nohe)[1]+fixef(gmod_nohe)[14]+randomsB[i,1]+(fixef(gmod_nohe)[10]+fixef(gmod_nohe)[30]+randomsB[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[gdat$he_col[gdat$he==theB[i,2]]])
}

########################

# Community : high group level of concern 

cg_tdat<-cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE,]
cg_the<-aggregate(cg_tdat$he,by=list(cg_tdat$r_eff),mean)



par(mfrow=c(1,2),mar=c(5,5,2,2)) #this puts two graphs in one picture 
plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

randoms<-ranef(cmod_nohe_grouphigh)$r_eff

# here A and B refers to the waek and strong social construction of concern in the paper not predisposition
randomsA<-randoms[rownames(randoms)%in%cdat_grouphigh$r_eff[p2[cdat_grouphigh$ParameterID,8]>0.5],]
randomsB<-randoms[rownames(randoms)%in%cdat_grouphigh$r_eff[p2[cdat_grouphigh$ParameterID,8]<0.49],]

theA<-cg_the[rownames(randoms)%in%cdat_grouphigh$r_eff[p2[cdat_grouphigh$ParameterID,8]>0.5],]
theB<-cg_the[rownames(randoms)%in%cdat_grouphigh$r_eff[p2[cdat_grouphigh$ParameterID,8]<0.49],]

x<-seq(1,300,0.01)

for(i in 1:nrow(randomsA)){
  x<-seq(min(cdat_grouphigh$EpStartInf[cdat_grouphigh$r_eff==rownames(randomsA)[i]],na.rm=T),max(cdat_grouphigh$EpStartInf[cdat_grouphigh$r_eff==rownames(randomsA)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(cmod_nohe_grouphigh)[1]+fixef(cmod_nohe_grouphigh)[11]+randomsA[i,1]+(fixef(cmod_nohe_grouphigh)[10]+fixef(cmod_nohe_grouphigh)[27]+randomsA[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[cdat_grouphigh$he_col[cdat_grouphigh$he==theA[i,2]]])
}


plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

for(i in 1:nrow(randomsB)){
  x<-seq(min(cdat_grouphigh$EpStartInf[cdat_grouphigh$r_eff==rownames(randomsB)[i]],na.rm=T),max(cdat_grouphigh$EpStartInf[cdat_grouphigh$r_eff==rownames(randomsB)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(cmod_nohe_grouphigh)[1]+fixef(cmod_nohe_grouphigh)[14]+randomsB[i,1]+(fixef(cmod_nohe_grouphigh)[10]+fixef(cmod_nohe_grouphigh)[30]+randomsB[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[cdat_grouphigh$he_col[cdat_grouphigh$he==theB[i,2]]])
}

# Group : high level of group concern
gg_tdat<-gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE,]
gg_the<-aggregate(gg_tdat$he,by=list(gg_tdat$r_eff),mean)

########################

par(mfrow=c(1,2),mar=c(5,5,2,2)) #this puts two graphs in one picture
plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

randoms<-ranef(gmod_nohe_grouphigh)$r_eff

# here A and B refers to the waek and strong social construction of concern in the paper not predisposition
randomsA<-randoms[rownames(randoms)%in%gdat_grouphigh$r_eff[p2[gdat_grouphigh$ParameterID,8]<0.5],]
randomsB<-randoms[rownames(randoms)%in%gdat_grouphigh$r_eff[p2[gdat_grouphigh$ParameterID,8]>0.49],]

theA<-gg_the[rownames(randoms)%in%gdat_grouphigh$r_eff[p2[gdat_grouphigh$ParameterID,8]<0.5],]
theB<-gg_the[rownames(randoms)%in%gdat_grouphigh$r_eff[p2[gdat_grouphigh$ParameterID,8]>0.49],]

x<-seq(1,300,0.01)

for(i in 1:nrow(randomsA)){
    x<-seq(min(gdat_grouphigh$EpStartInf[gdat_grouphigh$r_eff==rownames(randomsA)[i]],na.rm=T),max(gdat_grouphigh$EpStartInf[gdat_grouphigh$r_eff==rownames(randomsA)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(gmod_nohe_grouphigh)[1]+fixef(gmod_nohe_grouphigh)[11]+randomsA[i,1]+(fixef(gmod_nohe_grouphigh)[4]+fixef(gmod_nohe_grouphigh)[27]+randomsA[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[gdat_grouphigh$he_col[gdat_grouphigh$he==theA[i,2]]])
}


plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

for(i in 1:nrow(randomsB)){
  x<-seq(min(gdat_grouphigh$EpStartInf[gdat_grouphigh$r_eff==rownames(randomsB)[i]],na.rm=T),max(gdat_grouphigh$EpStartInf[gdat_grouphigh$r_eff==rownames(randomsB)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(gmod_nohe_grouphigh)[1]+fixef(gmod_nohe_grouphigh)[14]+randomsB[i,1]+(fixef(gmod_nohe_grouphigh)[4]+fixef(gmod_nohe_grouphigh)[30]+randomsB[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[gdat$he_col[gdat$he==theB[i,2]]])
}


####### low group concern #######
cg_tdat<-cdat_grouplow[is.na(cdat_grouplow$sev)==FALSE,]
cg_the<-aggregate(cg_tdat$he,by=list(cg_tdat$r_eff),mean)

par(mfrow=c(1,2),mar=c(5,5,2,2)) #this puts two graphs in one picture 
plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

randoms<-ranef(cmod_nohe_grouplow)$r_eff

# here A and B refers to the waek and strong social construction of concern in the paper not predisposition
randomsA<-randoms[rownames(randoms)%in%cdat_grouplow$r_eff[p2[cdat_grouplow$ParameterID,8]>0.5],]
randomsB<-randoms[rownames(randoms)%in%cdat_grouplow$r_eff[p2[cdat_grouplow$ParameterID,8]<0.49],]

theA<-cg_the[rownames(randoms)%in%cdat_grouplow$r_eff[p2[cdat_grouplow$ParameterID,8]>0.5],]
theB<-cg_the[rownames(randoms)%in%cdat_grouplow$r_eff[p2[cdat_grouplow$ParameterID,8]<0.49],]

x<-seq(1,300,0.01)

for(i in 1:nrow(randomsA)){
  x<-seq(min(cdat_grouplow$EpStartInf[cdat_grouplow$r_eff==rownames(randomsA)[i]],na.rm=T),max(cdat_grouplow$EpStartInf[cdat_grouplow$r_eff==rownames(randomsA)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(cmod_nohe_grouplow)[1]+fixef(cmod_nohe_grouplow)[11]+randomsA[i,1]+(fixef(cmod_nohe_grouplow)[10]+fixef(cmod_nohe_grouplow)[27]+randomsA[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[cdat_grouplow$he_col[cdat_grouplow$he==theA[i,2]]])
}


plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

for(i in 1:nrow(randomsB)){
  x<-seq(min(cdat_grouplow$EpStartInf[cdat_grouplow$r_eff==rownames(randomsB)[i]],na.rm=T),max(cdat_grouplow$EpStartInf[cdat_grouplow$r_eff==rownames(randomsB)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(cmod_nohe_grouplow)[1]+fixef(cmod_nohe_grouplow)[14]+randomsB[i,1]+(fixef(cmod_nohe_grouplow)[10]+fixef(cmod_nohe_grouplow)[30]+randomsB[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[cdat_grouplow$he_col[cdat_grouplow$he==theB[i,2]]])
}

# Group : low level of group concern
gg_tdat<-gdat_grouplow[is.na(gdat_grouplow$sev)==FALSE,]
gg_the<-aggregate(gg_tdat$he,by=list(gg_tdat$r_eff),mean)

########################

par(mfrow=c(1,2),mar=c(5,5,2,2)) #this puts two graphs in one picture
plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

randoms<-ranef(gmod_nohe_grouplow)$r_eff

# here A and B refers to the waek and strong social construction of concern in the paper not predisposition
randomsA<-randoms[rownames(randoms)%in%gdat_grouplow$r_eff[p2[gdat_grouplow$ParameterID,8]<0.5],]
randomsB<-randoms[rownames(randoms)%in%gdat_grouplow$r_eff[p2[gdat_grouplow$ParameterID,8]>0.49],]

theA<-gg_the[rownames(randoms)%in%gdat_grouplow$r_eff[p2[gdat_grouplow$ParameterID,8]<0.5],]
theB<-gg_the[rownames(randoms)%in%gdat_grouplow$r_eff[p2[gdat_grouplow$ParameterID,8]>0.49],]

x<-seq(1,300,0.01)

for(i in 1:nrow(randomsA)){
  x<-seq(min(gdat_grouplow$EpStartInf[gdat_grouplow$r_eff==rownames(randomsA)[i]],na.rm=T),max(gdat_grouplow$EpStartInf[gdat_grouplow$r_eff==rownames(randomsA)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(gmod_nohe_grouplow)[1]+fixef(gmod_nohe_grouplow)[11]+randomsA[i,1]+(fixef(gmod_nohe_grouplow)[4]+fixef(gmod_nohe_grouplow)[27]+randomsA[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[gdat_grouplow$he_col[gdat_grouplow$he==theA[i,2]]])
}


plot(NULL,xlim=c(0,300),ylim=c(0,1),xlab="Time that epidemic starts",ylab="Predicted severity",las=1,cex.lab=1.8,cex.axis=1.3)

for(i in 1:nrow(randomsB)){
  x<-seq(min(gdat_grouplow$EpStartInf[gdat_grouplow$r_eff==rownames(randomsB)[i]],na.rm=T),max(gdat_grouplow$EpStartInf[gdat_grouplow$r_eff==rownames(randomsB)[i]],na.rm=T),length.out=200)
  y_n<-(fixef(gmod_nohe_grouplow)[1]+fixef(gmod_nohe_grouplow)[14]+randomsB[i,1]+(fixef(gmod_nohe_grouplow)[4]+fixef(gmod_nohe_grouplow)[30]+randomsB[i,2])*x)
  lines(x,boot::inv.logit(y_n),col=he_pal[gdat$he_col[gdat$he==theB[i,2]]])
}

##################################################################################
#5x5 plots for supplement

socs<-c(0,0.1,0.2,0.5,1)
infs<-c(0,0.2,0.4,0.8,1.6)

#Code for Figure S1 (looks at max inf)
par(mfrow=c(5,5),mar=c(2.5,2.5,1,1))

# community mean : high initial concern
for(i in socs){
  for(j in infs){
    plot(cdat_high$MaxInf[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$WhichMaxInf[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],
         col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,150),las=1,legend=TRUE)
  }
}

# group mean : high initial concern
for(i in socs){
  for(j in infs){
    plot(gdat_high$MaxInf[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$WhichMaxInf[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],
         col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,850),las=1)
  }
}

# community mean : high group concern
for(i in socs){
  for(j in infs){
    plot(cdat_grouphigh$MaxInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$WhichMaxInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],
         col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,100),las=1)
  }
}

# group mean : high group concern
for(i in socs){
  for(j in infs){
    plot(gdat_grouphigh$MaxInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$WhichMaxInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],
         col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,650),las=1)
  }
}


# similar to figure 4 in paper : epidemic severity
# mixed with (awareness,social) (low,low),(low,high), (high,low), (high,high), (middle,middle) for four combos
socs<-c(0,0.2,1)
infs<-c(0,0.4,1.6)
par(mfrow=c(3,3),mar=c(2.5,2.5,1,1))

for(i in socs){
  for(j in infs){
    #community con mean
    plot(cdat_high$MaxInf[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$WhichMaxInf[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],
         col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,150),las=1)
    }
}

for(i in socs){
  for(j in infs){
    #group structure con mean
    plot(gdat_high$MaxInf[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$WhichMaxInf[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],
         col=adjustcolor(he_pal[cdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,800),las=1)
  }
}

for(i in socs){
  for(j in infs){
#community group mean
    plot(cdat_grouphigh$MaxInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$WhichMaxInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],
         col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,100),las=1)
    }
}

for(i in socs){
  for(j in infs){
# group group mean
    plot(gdat_grouphigh$MaxInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$WhichMaxInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],
         col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,650),las=1)
    }
}

#Code for Figure S2 (looks at epidemic start)
socs<-c(0,0.1,0.2,0.5,1)
infs<-c(0,0.2,0.4,0.8,1.6)
par(mfrow=c(5,5),mar=c(1,2,0,0))

#community high concern
for(i in socs){
  for(j in infs){
    if(j!=0&i!=1){
      par(mar=c(0.75,0.5,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$EpStartInf[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i==1){
      par(mar=c(3,3,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$EpStartInf[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),las=2,cex.axis=1.25)
    }
    if(j!=0&i==1){
      par(mar=c(3,0.5,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$EpStartInf[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i!=1){
      par(mar=c(0.75,3,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$EpStartInf[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.25)
    }
  }
}

#group high concern
for(i in socs){
  for(j in infs){
    if(j!=0&i!=1){
      par(mar=c(0.75,0.5,0,0))
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$EpStartInf[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i==1){
      par(mar=c(3,3,0,0))
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$EpStartInf[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),las=2,cex.axis=1.25)
    }
    if(j!=0&i==1){
      par(mar=c(3,0.5,0,0))
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$EpStartInf[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i!=1){
      par(mar=c(0.75,3,0,0))
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$EpStartInf[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.25)
    }
  }
}

#community high group concern
for(i in socs){
  for(j in infs){
    if(j!=0&i!=1){
      par(mar=c(0.75,0.5,0,0))
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$EpStartInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i==1){
      par(mar=c(3,3,0,0))
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$EpStartInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),las=2,cex.axis=1.25)
    }
    if(j!=0&i==1){
      par(mar=c(3,0.5,0,0))
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$EpStartInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i!=1){
      par(mar=c(0.75,3,0,0))
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$EpStartInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.25)
    }
  }
}

#group high group concern
for(i in socs){
  for(j in infs){
    if(j!=0&i!=1){
      par(mar=c(0.75,0.5,0,0))
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$EpStartInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i==1){
      par(mar=c(3,3,0,0))
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$EpStartInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),las=2,cex.axis=1.25)
    }
    if(j!=0&i==1){
      par(mar=c(3,0.5,0,0))
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$EpStartInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i!=1){
      par(mar=c(0.75,3,0,0))
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$EpStartInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,300),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.25)
    }
  }
}

#Code for Figure S5 (looks at the minimum proportion of those adherent and the severity of the epidemic)

# community mean starting concern
socs<-c(0,0.1,0.2,0.5,1)
infs<-c(0,0.2,0.4,0.8,1.6)
par(mfrow=c(5,5),mar=c(0,0,0,0))
for(i in socs){
  for(j in infs){
    if(j!=0&i!=1){
      par(mar=c(0.75,0.5,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$MinConc[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i==1){
      par(mar=c(3,3,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$MinConc[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),las=2,cex.axis=1.25)
    }
    if(i==1&j!=0){
      par(mar=c(3,0.5,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$MinConc[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i!=1){
      par(mar=c(0.75,3,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$MinConc[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.25)
    }
  }
}

# group mean starting concern
par(mfrow=c(5,5),mar=c(0,0,0,0))
for(i in socs){
  for(j in infs){
    if(j!=0&i!=1){
      par(mar=c(0.75,0.5,0,0))
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$MinConc[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i==1){
      par(mar=c(3,3,0,0))
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$MinConc[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),las=2,cex.axis=1.25)
    }
    if(i==1&j!=0){
      par(mar=c(3,0.5,0,0))
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$MinConc[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i!=1){
      par(mar=c(0.75,3,0,0))
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$MinConc[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.25)
    }
  }
}

# community mean group concern
par(mfrow=c(5,5),mar=c(0,0,0,0))
for(i in socs){
  for(j in infs){
    if(j!=0&i!=1){
      par(mar=c(0.75,0.5,0,0))
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i==1){
      par(mar=c(3,3,0,0))
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),las=2,cex.axis=1.25)
    }
    if(i==1&j!=0){
      par(mar=c(3,0.5,0,0))
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i!=1){
      par(mar=c(0.75,3,0,0))
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.25)
    }
  }
}

# group mean group concern
par(mfrow=c(5,5),mar=c(0,0,0,0))
for(i in socs){
  for(j in infs){
    if(j!=0&i!=1){
      par(mar=c(0.75,0.5,0,0))
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i==1){
      par(mar=c(3,3,0,0))
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),las=2,cex.axis=1.25)
    }
    if(i==1&j!=0){
      par(mar=c(3,0.5,0,0))
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.25)
    }
    if(j==0&i!=1){
      par(mar=c(0.75,3,0,0))
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.25)
    }
  }
}


# mixed graph for S5

socs<-c(0,1)
infs<-c(0,1.6)
par(mfrow=c(4,4),mar=c(3,3,0.75,0.5)) #this puts two graphs in one picture
for(i in socs){
  for(j in infs){
    #first 2x2
    if(j==0&i==0){
      par(mar=c(0.75,3,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$MinConc[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.5)
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$MinConc[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.5)
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.5)
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.5)
    }
    if(j==1.6&i==0){
      par(mar=c(0.75,3,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$MinConc[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.5)
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$MinConc[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.5)
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.5)
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.5)
    }
    if(j==0&i==1){
      par(mar=c(0.75,3,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$MinConc[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),xaxt="n",las=2,cex.axis=1.5)
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$MinConc[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.5)
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.5)
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",las=2,cex.axis=1.5)
    }
    if(j==1.6&i==1){
      par(mar=c(3,3,0,0))
      plot(cdat_high$sev[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]~cdat_high$MinConc[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_high$he_col[p2[cdat_high$ParameterID,8]==i&p2[cdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),las=2,cex.axis=1.5)
      plot(gdat_high$sev[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]~gdat_high$MinConc[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_high$he_col[p2[gdat_high$ParameterID,8]==i&p2[gdat_high$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.5)
      plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.5)
      plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j],col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),yaxt="n",las=2,cex.axis=1.5)
    }

  }
}


##################################################################################

##Code for Figure 4 (3x3 version of S2) epidemic severity

socs2<-c(0,0.2,1)
infs2<-c(0,0.4,1.6)

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.05]~cdat_grouphigh$EpStartInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.05],
         col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.05]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Time that epidemic starts",""),
         cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.5]~cdat_grouphigh$EpStartInf[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.5],
         col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.5]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Time that epidemic starts",""),
         cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(cdat_grouplow$sev[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.05]~cdat_grouplow$EpStartInf[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.05],
         col=adjustcolor(he_pal[cdat_grouplow$he_col[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.05]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Time that epidemic starts",""),
         cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(cdat_grouplow$sev[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.5]~cdat_grouplow$EpStartInf[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.5],
         col=adjustcolor(he_pal[cdat_grouplow$he_col[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.5]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Time that epidemic starts",""),
         cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

# group mean #

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.05]~gdat_grouphigh$EpStartInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.05],
         col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.05]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Time that epidemic starts",""),
         cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.5]~gdat_grouphigh$EpStartInf[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.5],
         col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.5]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Time that epidemic starts",""),
         cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(gdat_grouplow$sev[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.05]~gdat_grouplow$EpStartInf[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.05],
         col=adjustcolor(he_pal[gdat_grouplow$he_col[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.05]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Time that epidemic starts",""),
         cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(gdat_grouplow$sev[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.5]~gdat_grouplow$EpStartInf[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.5],
         col=adjustcolor(he_pal[gdat_grouplow$he_col[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.5]],0.4),
         pch=16,xlim=c(0,300),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Time that epidemic starts",""),
         cex.lab=1.8,cex.axis=1.1,las=1)
  }
}


##################################################################################

##Code for Figure 6 (3x3 version of S5) minimum proportion of people adherent

socs2<-c(0,0.2,1)
infs2<-c(0,0.4,1.6)

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.05]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.05],
         col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.05]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Minimum concern",""),cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(cdat_grouphigh$sev[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.5]~cdat_grouphigh$MinConc[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.5],
         col=adjustcolor(he_pal[cdat_grouphigh$he_col[p2[cdat_grouphigh$ParameterID,8]==i&p2[cdat_grouphigh$ParameterID,10]==j&p2[cdat_grouphigh$ParameterID,2]==0.5]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Minimum concern",""),cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(cdat_grouplow$sev[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.05]~cdat_grouplow$MinConc[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.05],
         col=adjustcolor(he_pal[cdat_grouplow$he_col[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.05]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Minimum concern",""),cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(cdat_grouplow$sev[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.5]~cdat_grouplow$MinConc[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.5],
         col=adjustcolor(he_pal[cdat_grouplow$he_col[p2[cdat_grouplow$ParameterID,8]==i&p2[cdat_grouplow$ParameterID,10]==j&p2[cdat_grouplow$ParameterID,2]==0.5]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Minimum concern",""),cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

# group mean ##

socs2<-c(0,0.2,1)
infs2<-c(0,0.4,1.6)

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.05]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.05],
         col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.05]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Minimum concern",""),cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(gdat_grouphigh$sev[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.5]~gdat_grouphigh$MinConc[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.5],
         col=adjustcolor(he_pal[gdat_grouphigh$he_col[p2[gdat_grouphigh$ParameterID,8]==i&p2[gdat_grouphigh$ParameterID,10]==j&p2[gdat_grouphigh$ParameterID,2]==0.5]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Minimum concern",""),cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(gdat_grouplow$sev[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.05]~gdat_grouplow$MinConc[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.05],
         col=adjustcolor(he_pal[gdat_grouplow$he_col[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.05]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Minimum concern",""),cex.lab=1.8,cex.axis=1.1,las=1)
  }
}

par(mfrow=c(3,3),mar=c(5,5,2,2))
for(i in socs2){
  for(j in infs2){
    ms<-c(5,5,2,2)
    par(mar=ms)
    if(j==0){
      ms[c(2,4)]<-c(5,0)
      par(mar=ms)
    }
    if(j==0.4){
      ms[c(2,4)]<-c(4,1)
      par(mar=ms)
    }
    if(j==1.6){
      ms[c(2,4)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0){
      ms[c(1,3)]<-c(3,2)
      par(mar=ms)
    }
    if(i==0.2){
      ms[c(1,3)]<-c(4,1)
      par(mar=ms)
    }
    if(i==1){
      ms[c(1,3)]<-c(5,0)
      par(mar=ms)
    }
    plot(gdat_grouplow$sev[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.5]~gdat_grouplow$MinConc[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.5],
         col=adjustcolor(he_pal[gdat_grouplow$he_col[p2[gdat_grouplow$ParameterID,8]==i&p2[gdat_grouplow$ParameterID,10]==j&p2[gdat_grouplow$ParameterID,2]==0.5]],0.4),pch=16,xlim=c(0,1),ylim=c(0,1),ylab=ifelse(j==0,"Epidemic Severity",""),xlab=ifelse(i==1,"Minimum concern",""),cex.lab=1.8,cex.axis=1.1,las=1)
  }
}



##################################################################################

##Code for Figure 1c and d
# the index 609 refers to a specific reassurance effect. It is arbitrarily picked and used for plot purposes.

# Community version of plot c and d in the paper
par(mfrow=c(2,1),mar=c(4.5,5,1,2))
col10<-c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")

plot(NULL,xlim=c(0,150),ylim=c(0,150),xlab="Days",ylab="# Infected",cex.lab=1.3,cex.axis=1.3,las=1)
for(i in 1:nrow(inf_res[[760]])){
  lines(inf_res[[760]][i,],col=col10[i],lwd=3)
}

plot(NULL,xlim=c(0,150),ylim=c(0,1),xlab="Days",ylab="% Concerned",cex.lab=1.3,cex.axis=1.3,las=1)
for(i in 1:nrow(conc_res[[760]])){
  lines(conc_res[[760]][i,],col=col10[i],lwd=3)
}

mtext(paste0("Reassurance effect is ",round(he[[760]],3)),side=1,adj=0.95,line=-1.5,cex=1.2)
mod[[760]] # gives the correct row for params 2 dataframe , column 8 refers to the initial level of concern
mtext(paste0("Individual Concern Effect is ",params2[13,8]),side=1,adj=0.95,line=-2.5,cex=1)
group[[760]] #gives the correct row for groupid dataframe 
mtext(paste0("Group High/Low Concern is H(",params4[4,6],"), L(",params4[10,9],")"),side=1,adj=0.95,line=-3.5,cex=1)


#group version of the plot c and d in the paper
par(mfrow=c(2,1))
col10<-c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")

plot(NULL,xlim=c(0,150),ylim=c(0,200),xlab="Days",ylab="# infected",cex.lab=1.3,cex.axis=1.3,las=1)
handles <- list()
for(i in 1:nrow(group_inf_res[[760]])){
  handles[[i]] <- lines(group_inf_res[[760]][i,], col=col10[i], lwd=3)
}
legend("topright", legend=paste("Group", 1:nrow(group_inf_res[[680]])), col=col10[1:nrow(group_inf_res[[680]])], lty=1, lwd=3, cex=1,bty="n")

plot(NULL,xlim=c(0,150),ylim=c(0,1),xlab="Days",ylab="% concerned",cex.lab=1.6,cex.axis=1.3,las=1)
for(i in 1:nrow(group_inf_res[[760]])){
  lines(group_conc_res[[760]][i,],col=col10[i],lwd=3)
}

mtext(paste0("Reassurance effect is ",round(he[[760]],3)),side=1,adj=0.95,line=-1.5,cex=1.2)
group_mod[[760]] # gives the correct row for params 2 dataframe , column 8 refers to the initial level of concern
mtext(paste0("Individual Concern Effect is ",params2[16,8]),side=1,adj=0.95,line=-2.5,cex=1)
group_group[[760]] #gives the correct row for groupid dataframe 
mtext(paste0("Group High/Low Concern is H(",params4[10,6],"), L(",params4[10,9],")"),side=1,adj=0.95,line=-3.5,cex=1)

##################################################################################

##Code for Figure 1a and b

#read in network and filter so that it is only young adults of one predisposition
sourcefolder <- "."
paramsfolder <- "params"
networksfolder <-file.path(".",paramsfolder,"networks2")

n_plot<-readRDS(paste0(networksfolder,"/88net_and_parents.RDS"))
n_plotB<-n_plot[[1]][c(1:240,481:1110,1741:1870),c(1:240,481:1110,1741:1870)]
n_plotBy<-n_plot[[1]][481:1110,481:1110]

n_plot2<-graph.adjacency(n_plotBy,mode="undirected")


#Plot the network
par(mfrow=c(1,1),mar=c(0,0,0,0))                
plot(n_plot2,vertex.label=NA,vertex.size=6,vertex.color=rep(col10,100),edge.color="grey")


###Plot multiplex structure for dark blue community

#Communication layer
n_plotBy1<-n_plot[[1]][seq(481,1101,10),seq(481,1101,10)]
n_plot3<-graph.adjacency(n_plotBy1,mode="undirected")
lo<-layout.fruchterman.reingold(n_plot3)
plot(n_plot3,layout=lo,vertex.label=NA,vertex.size=6,vertex.color=col10[1],edge.color="grey")

#Infection layer
m_plot<-readRDS(paste0(networksfolder,"/8net_and_parents.RDS"))
m_plotBy1<-m_plot[[1]][seq(481,1101,10),seq(481,1101,10)]
m_plot3<-graph.adjacency(m_plotBy1,mode="undirected")
plot(m_plot3,layout=lo,vertex.label=NA,vertex.size=6,vertex.color=col10[1],edge.color="grey")

#3D scatterplot
Dheight<-1
Iheight<-9

cD<-cbind(lo,rep(Dheight,63))
cI<-cbind(lo,rep(Iheight,63))

C<-scatterplot3d(cD,xlim=c(-6,6),ylim=c(-6,6),zlim=c(0,12),color=col10[1],pch=16,box=F,grid=F,cex.symbols=1.5,angle=70,axis=F,scale.y=0.5)
C$points3d(cI[,1],cI[,2],cI[,3],col=col10[1],pch=18,cex=1.5)

theta<-seq(0,2*pi,length=1000)
alpha<-pi/10
ell.top.x<- -0.25+7*cos(theta)*cos(alpha)-7*sin(theta)*sin(alpha)
ell.top.y<- +0.25+7*cos(theta)*sin(alpha)+7*sin(theta)*cos(alpha)
C$points3d(ell.top.x,ell.top.y,rep(Dheight,length(ell.top.x)),type="l",col=adjustcolor(col10[1],alpha=1))
C$points3d(ell.top.x,ell.top.y,rep(Iheight,length(ell.top.x)),type="l",col=adjustcolor(col10[1],alpha=1))
polygon(C$xyz.convert(ell.top.x,ell.top.y,rep(Dheight,length(ell.top.x)))$x,C$xyz.convert(ell.top.x,ell.top.y,rep(Dheight,length(ell.top.x)))$y,col=adjustcolor(col10[1],0.08),border=NA)
polygon(C$xyz.convert(ell.top.x,ell.top.y,rep(Iheight,length(ell.top.x)))$x,C$xyz.convert(ell.top.x,ell.top.y,rep(Iheight,length(ell.top.x)))$y,col=adjustcolor(col10[1],0.08),border=NA)

for(i in 1:63){
  C$points3d(c(cI[i,1],cD[i,1]),c(cI[i,2],cD[i,2]),c(cI[i,3],cD[i,3]),type="l",col="grey",lty=2)
}

for(i in 1:(ncol(n_plotBy1)-1)){
  for(j in (i+1):ncol(n_plotBy1)){
    if(n_plotBy1[i,j]>0){
      C$points3d(c(cI[i,1],cI[j,1]),c(cI[i,2],cI[j,2]),c(cI[i,3],cI[j,3]),type="l",col="dark grey",lwd=1,lty=1)
    }
  }
}

for(i in 1:(ncol(m_plotBy1)-1)){
  for(j in (i+1):ncol(m_plotBy1)){
    if(m_plotBy1[i,j]>0){
      C$points3d(c(cD[i,1],cD[j,1]),c(cD[i,2],cD[j,2]),c(cD[i,3],cD[j,3]),type="l",col="dark grey",lwd=1,lty=1)
    }
  }
}

C$points3d(cI[,1],cI[,2],cI[,3],col=col10[1],pch=18,cex=1.5)
C$points3d(cD[,1],cD[,2],cD[,3],col=col10[1],pch=16,cex=1.5)

##################################################################################

##Separate models for each multiplex network

# community mean
cmod_nohe1<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                              (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==1,])
cmod_nohe2<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==2,])
cmod_nohe3<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==3,])
cmod_nohe4<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==4,])
cmod_nohe5<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==5,])
cmod_nohe6<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==6,])
cmod_nohe7<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==7,])
cmod_nohe8<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==8,])
cmod_nohe9<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==9,])
# group mean

gmod_nohe1<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==1,])
gmod_nohe2<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==2,])
gmod_nohe3<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==3,])
gmod_nohe4<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==4,])
gmod_nohe5<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==5,])
gmod_nohe6<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==6,])
gmod_nohe7<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==7,])
gmod_nohe8<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==8,])
gmod_nohe9<-lmer(lt_sev~as.factor(p2[ParameterID,2])*as.factor(p2[ParameterID,8])*EpStartInf*as.factor(p2[ParameterID,10])+
                   (EpStartInf|r_eff),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==9,])

##################################################################################

##Code for Figures S6 and S7

crandoms1<-ranef(cmod_nohe1)$r_eff
cthe1<-aggregate(cdat_grouphigh$he[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==1],by=list(cdat_grouphigh$r_eff[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==1]),mean)

crandoms2<-ranef(cmod_nohe2)$r_eff
cthe2<-aggregate(cdat_grouphigh$he[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==2],by=list(cdat_grouphigh$r_eff[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==2]),mean)

crandoms3<-ranef(cmod_nohe3)$r_eff
cthe3<-aggregate(cdat_grouphigh$he[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==3],by=list(cdat_grouphigh$r_eff[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==3]),mean)

crandoms4<-ranef(cmod_nohe4)$r_eff
cthe4<-aggregate(cdat_grouphigh$he[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==4],by=list(cdat_grouphigh$r_eff[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==4]),mean)

crandoms5<-ranef(cmod_nohe5)$r_eff
cthe5<-aggregate(cdat_grouphigh$he[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==5],by=list(cdat_grouphigh$r_eff[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==5]),mean)

crandoms6<-ranef(cmod_nohe6)$r_eff
cthe6<-aggregate(cdat_grouphigh$he[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==6],by=list(cdat_grouphigh$r_eff[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==6]),mean)

crandoms7<-ranef(cmod_nohe7)$r_eff
cthe7<-aggregate(cdat_grouphigh$he[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==7],by=list(cdat_grouphigh$r_eff[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==7]),mean)

crandoms8<-ranef(cmod_nohe8)$r_eff
cthe8<-aggregate(cdat_grouphigh$he[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==8],by=list(cdat_grouphigh$r_eff[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==8]),mean)

crandoms9<-ranef(cmod_nohe9)$r_eff
cthe9<-aggregate(cdat_grouphigh$he[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==9],by=list(cdat_grouphigh$r_eff[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==9]),mean)

par(mar=c(5,5,2,2))
par(mfrow=c(3,3))

for(i in 1:9){
  xt<-get(paste0("cthe",i))
  yt<-get(paste0("crandoms",i))
  plot(yt[,2]~abs(xt$x),pch=16,ylab="Random Slope Estimates",xlab="Strength of Reassurance Effect",las=1,cex.axis=0.8,cex.lab=1.2)
  mtext(text=paste("NetworkID",i),side=3,adj=0.5,line=0.5)
  mtext(text=round(cor.test(abs(xt$x),yt[,2])$estimate,3),side=1,adj=1,line=-1)
}

for(i in 1:9){
  xt<-get(paste0("cthe",i))
  yt<-get(paste0("crandoms",i))
  plot(yt[,1]~abs(xt$x),pch=16,ylab="Random Intercept Estimates",xlab="Strength of Reassurance Effect",las=1,cex.axis=0.8,cex.lab=1.2)
  mtext(text=paste("NetworkID",i),side=3,adj=0.5,line=0.5)
  mtext(text=round(cor.test(abs(xt$x),yt[,1])$estimate,3),side=1,adj=1,line=-1)
}

grandoms1<-ranef(gmod_nohe1)$r_eff
gthe1<-aggregate(gdat_grouphigh$he[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==1],by=list(gdat_grouphigh$r_eff[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==1]),mean)

grandoms2<-ranef(gmod_nohe2)$r_eff
gthe2<-aggregate(gdat_grouphigh$he[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==2],by=list(gdat_grouphigh$r_eff[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==2]),mean)

grandoms3<-ranef(gmod_nohe3)$r_eff
gthe3<-aggregate(gdat_grouphigh$he[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==3],by=list(gdat_grouphigh$r_eff[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==3]),mean)

grandoms4<-ranef(gmod_nohe4)$r_eff
gthe4<-aggregate(gdat_grouphigh$he[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==4],by=list(gdat_grouphigh$r_eff[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==4]),mean)

grandoms5<-ranef(gmod_nohe5)$r_eff
gthe5<-aggregate(gdat_grouphigh$he[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==5],by=list(gdat_grouphigh$r_eff[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==5]),mean)

grandoms6<-ranef(gmod_nohe6)$r_eff
gthe6<-aggregate(gdat_grouphigh$he[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==6],by=list(gdat_grouphigh$r_eff[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==6]),mean)

grandoms7<-ranef(gmod_nohe7)$r_eff
gthe7<-aggregate(gdat_grouphigh$he[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==7],by=list(gdat_grouphigh$r_eff[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==7]),mean)

grandoms8<-ranef(gmod_nohe8)$r_eff
gthe8<-aggregate(gdat_grouphigh$he[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==8],by=list(gdat_grouphigh$r_eff[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==8]),mean)

grandoms9<-ranef(gmod_nohe9)$r_eff
gthe9<-aggregate(gdat_grouphigh$he[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==9],by=list(gdat_grouphigh$r_eff[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==9]),mean)

par(mar=c(5,5,2,2))
par(mfrow=c(3,3))

for(i in 1:9){
  xt<-get(paste0("gthe",i))
  yt<-get(paste0("grandoms",i))
  plot(yt[,2]~abs(xt$x),pch=16,ylab="Random Slope Estimates",xlab="Strength of Reassurance Effect",las=1,cex.axis=0.8,cex.lab=1.2)
  mtext(text=paste("NetworkID",i),side=3,adj=0.5,line=0.5)
  mtext(text=round(cor.test(abs(xt$x),yt[,2])$estimate,3),side=1,adj=1,line=-1)
}

for(i in 1:9){
  xt<-get(paste0("gthe",i))
  yt<-get(paste0("grandoms",i))
  plot(yt[,1]~abs(xt$x),pch=16,ylab="Random Intercept Estimates",xlab="Strength of Reassurance Effect",las=1,cex.axis=0.8,cex.lab=1.2)
  mtext(text=paste("NetworkID",i),side=3,adj=0.5,line=0.5)
  mtext(text=round(cor.test(abs(xt$x),yt[,1])$estimate,3),side=1,adj=1,line=-1)
}


##################################################################################

##Code for Figures S3 and S4

#community mean
peak_vars<-aggregate(cdat$WhichMaxInf,by=list(cdat$r_eff),var,na.rm=T)
ses<-aggregate(cdat$ParameterID,by=list(cdat$r_eff),unique)[,2]
soc_effs<-rep(NA,length(ses))
dis_effs<-rep(NA,length(ses))

for(i in 1:length(ses)){
  soc_effs[i]<-params2[ses[i],8]
  dis_effs[i]<-params2[ses[i],10]
}

hea_effs<-aggregate(cdat$he,by=list(cdat$r_eff),unique)[,2]
par(mar=c(5,5,2,2))
par(mfrow=c(1,1))
#Figure S3
boxplot(log(peak_vars[,2])~soc_effs,range=0,lty=1,xlab="Social Construction Effect Size",ylab="Log Variation in in Outbreak Peak Timings",cex.lab=1.5,cex.axis=1.5,las=1)
#Figure S4
plot(log(peak_vars[,2])~abs(hea_effs),las=1,cex.axis=1.5,cex.lab=1.5,ylab="Log Variation in Outbreak Peak Timings",xlab="Strength of Reasurrance Effect",main ="Community Mean",pch=16,col=adjustcolor("black",0.5))


# group mean
peak_vars<-aggregate(gdat$WhichMaxInf,by=list(gdat$r_eff),var,na.rm=T)

ses<-aggregate(gdat$ParameterID,by=list(gdat$r_eff),unique)[,2]
soc_effs<-rep(NA,length(ses))
dis_effs<-rep(NA,length(ses))

for(i in 1:length(ses)){
  soc_effs[i]<-params2[ses[i],8]
  dis_effs[i]<-params2[ses[i],10]
}

hea_effs<-aggregate(gdat$he,by=list(gdat$r_eff),unique)[,2]


par(mar=c(5,5,2,2))
par(mfrow=c(1,1))

#Figure S3
boxplot(log(peak_vars[,2])~soc_effs,range=0,lty=1,xlab="Social Construction Effect Size",ylab="Log Variation in in Outbreak Peak Timings",cex.lab=1.5,cex.axis=1.5,las=1)

#Figure S4
plot(log(peak_vars[,2])~abs(hea_effs),las=1,cex.axis=1.5,cex.lab=1.5,ylab="Log Variation in Outbreak Peak Timings",xlab="Strength of Reasurrance Effect",main ="Group Mean",pch=16,col=adjustcolor("black",0.5))

##################################################################################
##################################################################################
##################################################################################

##In this section of code we produce the Supplementary Tables
#We first make versions of the models with more informative variable names, refit them and produce table using sjPlot

## First for cdat_high
cdat_grouphigh$sev<-(cdat_grouphigh$MaxInf/max(cdat_grouphigh$MaxInf))*(1-(cdat_grouphigh$WhichMaxInf-cdat_grouphigh$EpStartInf)/max(cdat_grouphigh$WhichMaxInf-cdat_grouphigh$EpStartInf,na.rm=TRUE))
cdat_grouphigh$lt_sev<-car::logit(cdat_grouphigh$sev)

names(cdat_grouphigh)[23]<-"LogitSeverity"
names(cdat_grouphigh)[12]<-"EpidemicStartTime1"
names(cdat_grouphigh)[18]<-"SimulationNumber"

cdat_grouphigh$NetworkID<-as.factor(cdat_grouphigh$NetworkID)
cdat_grouphigh$EpidemicStartTime<-cdat_grouphigh$EpidemicStartTime1-40
cdat_grouphigh$SocialConstruction<-rep(NA,nrow(cdat_grouphigh))
cdat_grouphigh$Awareness<-rep(NA,nrow(cdat_grouphigh))

for(i in 1:nrow(cdat_grouphigh)){
  cdat_grouphigh$InitIndivCon[i]<-p2[cdat_grouphigh$ParameterID[i],2]
  cdat_grouphigh$SocialConstruction[i]<-p2[cdat_grouphigh$ParameterID[i],8]
  cdat_grouphigh$Awareness[i]<-p2[cdat_grouphigh$ParameterID[i],10]
}

cdat_grouphigh$SocialConstruction<-as.factor(cdat_grouphigh$SocialConstruction)
cdat_grouphigh$Awareness<-as.factor(cdat_grouphigh$Awareness)
cdat_grouphigh$InitIndivCon<-as.factor(cdat_grouphigh$InitIndivCon)

cMod_nohe<-lmer(LogitSeverity~NetworkID*EpidemicStartTime+InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+
                  (EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE,])

tab_model(cMod_nohe,file = "cdat_grouphigh_table.tex")

## For gdat_high
gdat_grouphigh$sev<-(gdat_grouphigh$MaxInf/max(gdat_grouphigh$MaxInf))*(1-(gdat_grouphigh$WhichMaxInf-gdat_grouphigh$EpStartInf)/max(gdat_grouphigh$WhichMaxInf-gdat_grouphigh$EpStartInf,na.rm=TRUE))
gdat_grouphigh$lt_sev<-car::logit(gdat_grouphigh$sev)

names(gdat_grouphigh)[23]<-"LogitSeverity"
names(gdat_grouphigh)[12]<-"EpidemicStartTime1"
names(gdat_grouphigh)[18]<-"SimulationNumber"

gdat_grouphigh$NetworkID<-as.factor(gdat_grouphigh$NetworkID)
gdat_grouphigh$EpidemicStartTime<-gdat_grouphigh$EpidemicStartTime1-40
gdat_grouphigh$SocialConstruction<-rep(NA,nrow(gdat_grouphigh))
gdat_grouphigh$Awareness<-rep(NA,nrow(gdat_grouphigh))

for(i in 1:nrow(gdat_grouphigh)){
  gdat_grouphigh$SocialConstruction[i]<-p2[gdat_grouphigh$ParameterID[i],8]
  gdat_grouphigh$Awareness[i]<-p2[gdat_grouphigh$ParameterID[i],10]
  gdat_grouphigh$InitIndivCon[i]<-p2[gdat_grouphigh$ParameterID[i],2]
}

gdat_grouphigh$SocialConstruction<-as.factor(gdat_grouphigh$SocialConstruction)
gdat_grouphigh$Awareness<-as.factor(gdat_grouphigh$Awareness)
gdat_grouphigh$InitIndivCon<-as.factor(gdat_grouphigh$InitIndivCon)

gMod_nohe<-lmer(LogitSeverity~NetworkID*EpidemicStartTime+InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+
                  (EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE,])

tab_model(gMod_nohe,file = "gdat_grouphigh_table.tex")

##################################################################################

##Now for cdat_mid

cdat_groupmid$sev<-(cdat_groupmid$MaxInf/max(cdat_groupmid$MaxInf))*(1-(cdat_groupmid$WhichMaxInf-cdat_groupmid$EpStartInf)/max(cdat_groupmid$WhichMaxInf-cdat_groupmid$EpStartInf,na.rm=TRUE))
cdat_groupmid$lt_sev<-car::logit(cdat_groupmid$sev)

names(cdat_groupmid)[23]<-"LogitSeverity"
names(cdat_groupmid)[12]<-"EpidemicStartTime1"
names(cdat_groupmid)[18]<-"SimulationNumber"

cdat_groupmid$NetworkID<-as.factor(cdat_groupmid$NetworkID)
cdat_groupmid$EpidemicStartTime<-cdat_groupmid$EpidemicStartTime1-40
cdat_groupmid$SocialConstruction<-rep(NA,nrow(cdat_groupmid))
cdat_groupmid$Awareness<-rep(NA,nrow(cdat_groupmid))

for(i in 1:nrow(cdat_groupmid)){
  cdat_groupmid$SocialConstruction[i]<-p2[cdat_groupmid$ParameterID[i],8]
  cdat_groupmid$Awareness[i]<-p2[cdat_groupmid$ParameterID[i],10]
  cdat_groupmid$InitIndivCon[i]<-p2[cdat_groupmid$ParameterID[i],2]
}

cdat_groupmid$SocialConstruction<-as.factor(cdat_groupmid$SocialConstruction)
cdat_groupmid$Awareness<-as.factor(cdat_groupmid$Awareness)
cdat_groupmid$InitIndivCon<-as.factor(cdat_groupmid$InitIndivCon)

cMod_noheM<-lmer(LogitSeverity~NetworkID*EpidemicStartTime+InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+
                  (EpidemicStartTime|SimulationNumber),data=cdat_groupmid[is.na(cdat_groupmid$sev)==FALSE,])
tab_model(cMod_noheM,file = "cdat_groupmid_table.tex")

##Now for gdat_mid
gdat_groupmid$sev<-(gdat_groupmid$MaxInf/max(gdat_groupmid$MaxInf))*(1-(gdat_groupmid$WhichMaxInf-gdat_groupmid$EpStartInf)/max(gdat_groupmid$WhichMaxInf-gdat_groupmid$EpStartInf,na.rm=TRUE))
gdat_groupmid$lt_sev<-car::logit(gdat_groupmid$sev)

names(gdat_groupmid)[23]<-"LogitSeverity"
names(gdat_groupmid)[12]<-"EpidemicStartTime1"
names(gdat_groupmid)[18]<-"SimulationNumber"

gdat_groupmid$NetworkID<-as.factor(gdat_groupmid$NetworkID)
gdat_groupmid$EpidemicStartTime<-gdat_groupmid$EpidemicStartTime1-40
gdat_groupmid$SocialConstruction<-rep(NA,nrow(gdat_groupmid))
gdat_groupmid$Awareness<-rep(NA,nrow(gdat_groupmid))

for(i in 1:nrow(gdat_groupmid)){
  gdat_groupmid$SocialConstruction[i]<-p2[gdat_groupmid$ParameterID[i],8]
  gdat_groupmid$Awareness[i]<-p2[gdat_groupmid$ParameterID[i],10]
  gdat_groupmid$InitIndivCon[i]<-p2[gdat_groupmid$ParameterID[i],2]
}

gdat_groupmid$SocialConstruction<-as.factor(gdat_groupmid$SocialConstruction)
gdat_groupmid$Awareness<-as.factor(gdat_groupmid$Awareness)
gdat_groupmid$InitIndivCon<-as.factor(gdat_groupmid$InitIndivCon)

gMod_noheM<-lmer(LogitSeverity~NetworkID*EpidemicStartTime+InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+
                   (EpidemicStartTime|SimulationNumber),data=gdat_groupmid[is.na(gdat_groupmid$sev)==FALSE,])

tab_model(gMod_noheM,file = "gdat_groupmid_table.tex")

##################################################################################

##Now for separate networks and cdat_high

cMod_nohe1<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==1,])
cMod_nohe2<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==2,])
cMod_nohe3<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==3,])
cMod_nohe4<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==4,])
cMod_nohe5<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==5,])
cMod_nohe6<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==6,])
cMod_nohe7<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==7,])
cMod_nohe8<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==8,])
cMod_nohe9<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=cdat_grouphigh[is.na(cdat_grouphigh$sev)==FALSE&cdat_grouphigh$NetworkID==9,])

##For these tables we aggregate by homophily
tab_model(cMod_nohe1,cMod_nohe2,cMod_nohe3,collapse.ci=TRUE,file = "cdat_grouphighnet13_table.tex")
tab_model(cMod_nohe4,cMod_nohe5,cMod_nohe6,collapse.ci=TRUE,file = "cdat_grouphighnet46_table.tex")
tab_model(cMod_nohe7,cMod_nohe8,cMod_nohe9,collapse.ci=TRUE,file = "cdat_grouphighnet79_table.tex")

##Now for separate networks and gdat_high
gMod_nohe1<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==1,])
gMod_nohe2<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==2,])
gMod_nohe3<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==3,])
gMod_nohe4<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==4,])
gMod_nohe5<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==5,])
gMod_nohe6<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==6,])
gMod_nohe7<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==7,])
gMod_nohe8<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==8,])
gMod_nohe9<-lmer(LogitSeverity~InitIndivCon*SocialConstruction*EpidemicStartTime*Awareness+(EpidemicStartTime|SimulationNumber),data=gdat_grouphigh[is.na(gdat_grouphigh$sev)==FALSE&gdat_grouphigh$NetworkID==9,])

##For these tables we aggregate by homophily
tab_model(gMod_nohe1,gMod_nohe2,gMod_nohe3,collapse.ci=TRUE)
tab_model(gMod_nohe4,gMod_nohe5,gMod_nohe6,collapse.ci=TRUE,file = "gdat_grouphighnet46_table.tex")
tab_model(gMod_nohe7,gMod_nohe8,gMod_nohe9,collapse.ci=TRUE,file = "gdat_grouphighnet79_table.tex")

################################

##Code for Figure 3 - peoduced for revisions

set.seed(16)
q1<-sample(which(cdat_grouphigh$sev<0.1),1)
q2<-sample(which(cdat_grouphigh$sev>0.1&cdat_grouphigh$sev<0.2),1)
q3<-sample(which(cdat_grouphigh$sev>0.2&cdat_grouphigh$sev<0.3),1)
q4<-sample(which(cdat_grouphigh$sev>0.3&cdat_grouphigh$sev<0.4),1)
q5<-sample(which(cdat_grouphigh$sev>0.4&cdat_grouphigh$sev<0.5),1)
q6<-sample(which(cdat_grouphigh$sev>0.5&cdat_grouphigh$sev<0.6),1)
q7<-sample(which(cdat_grouphigh$sev>0.6&cdat_grouphigh$sev<0.7),1)
q8<-sample(which(cdat_grouphigh$sev>0.7&cdat_grouphigh$sev<0.8),1)
q9<-sample(which(cdat_grouphigh$sev>0.8),1)

qs<-list(q1,q3,q5,q7,q9)

he2<-unlist(he)

line_pal<-plasma(40)

par(mfrow=c(1,1))
plot(NULL,xlim=c(0,200),ylim=c(0,75),ylab="",xlab="Time",las=1,cex.lab=1.5,cex.axis=1.2)

for(i in 1:5){
  j<-as.numeric(which(he2==cdat_grouphigh$he[qs[[i]]])[[1]])
  y_t<-inf_res[[j]][cdat_grouphigh$Concern_Mean[qs[[i]]],]
  lines(x=seq(1,length(y_t),1),y=y_t,lwd=5,col=line_pal[round(cdat_grouphigh$sev[qs[[i]]]*40)])
}

mtext("Number Infected",side=2,line=3.5,cex=1.5)

points(x=rep(165,40),y=seq(50,75,length.out=40),col=line_pal,cex=3,pch=15)
text("Severity = 1",x=170,y=75,adj=c(0,0.5),cex=1.2)
text("Severity = 0.5",x=170,y=62.5,adj=c(0,0.5),cex=1.2)
text("Severity = 0",x=170,y=50,adj=c(0,0.5),cex=1.2)

# group mean
set.seed(16)
q1<-sample(which(gdat_grouphigh$sev<0.1),1)
q2<-sample(which(gdat_grouphigh$sev>0.1&gdat_grouphigh$sev<0.2),1)
q3<-sample(which(gdat_grouphigh$sev>0.2&gdat_grouphigh$sev<0.3),1)
q4<-sample(which(gdat_grouphigh$sev>0.3&gdat_grouphigh$sev<0.4),1)
q5<-sample(which(gdat_grouphigh$sev>0.4&gdat_grouphigh$sev<0.5),1)
q6<-sample(which(gdat_grouphigh$sev>0.5&gdat_grouphigh$sev<0.6),1)
q7<-sample(which(gdat_grouphigh$sev>0.6&gdat_grouphigh$sev<0.7),1)
q8<-sample(which(gdat_grouphigh$sev>0.7&gdat_grouphigh$sev<0.8),1)
q9<-sample(which(gdat_grouphigh$sev>0.8),1)

qs<-list(q1,q3,q5,q7,q9)

he2<-unlist(he)

line_pal<-plasma(40)

par(mfrow=c(1,1))
plot(NULL,xlim=c(0,200),ylim=c(0,300),ylab="",xlab="Time",las=1,cex.lab=1.5,cex.axis=1.2)

for(i in 1:5){
  j<-as.numeric(which(he2==gdat_grouphigh$he[qs[[i]]])[[1]])
  y_t<-group_inf_res[[j]][gdat_grouphigh$Concern_Mean[qs[[i]]],]
  lines(x=seq(1,length(y_t),1),y=y_t,lwd=5,col=line_pal[round(gdat_grouphigh$sev[qs[[i]]]*40)])
}

mtext("Number Infected",side=2,line=3.5,cex=1.5)

points(x=rep(165,40),y=seq(75,100,length.out=40),col=line_pal,cex=3,pch=15)
text("Severity = 1",x=170,y=100,adj=c(0,0.5),cex=1.2)
text("Severity = 0.5",x=170,y=87.5,adj=c(0,0.5),cex=1.2)
text("Severity = 0",x=170,y=75,adj=c(0,0.5),cex=1.2)


  ###
