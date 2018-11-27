## Script to run a dynamic configuration of Marxan (http://www.uq.edu.au/marxan) for 1997-10-01.
## Written by Heather Welch 10-26-2018

# if (!require('devtools'))
#   install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
# devtools::install_github('jeffreyhanson/marxan')
library(marxan)
library(tidyverse)
library(mapdata)
library(maps)
library(fields)

# -------------------------------------------> marxan helper functions
vignette('quickstart', package='marxan')
vignette('tutorial', package='marxan')
vignette('classes', package='marxan')

run_Marxan=function(get_date,species,outdir,weightings,namesrisk){
  
  ############ 1. load required functions
  
  make_png_marxan=function(r,get_date,outdir,type,weightings,namesrisk){ ### does what it says
    
    EcoCols<-colorRampPalette(c("red","orange","white","cyan","blue"))
    ByCols<-colorRampPalette(c("red","orange","white"))
    
    if(weightings[4]!=0){
      zlimits=c(r@data@min,r@data@max)
      col=EcoCols(255)}
    
    if(weightings[4]==0){
      zlimits=c(r@data@min,r@data@max)
      col=ByCols(255)}
    
    png(paste0(outdir,"marxan_",paste0(weightings,collapse = "_"),"_",get_date,"_",type,".png"),width=960,height=1100,units='px',pointsize=20)
    par(mar=c(3,3,.5,.5),las=1,font=2)
    
    image.plot(r, col=col, ylab="", xlab="", xlim=c(-130,-115.5),ylim=c(30,47),zlim=zlimits)
    maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
    text(-122,46,format(get_date,format="%b %d %Y"),adj=c(0,0),cex=2) 
    text(-122,45,"Species weightings",adj=c(0,0),cex=1)
    text(-122,44.5,paste(namesrisk[1],' weighting = ',weightings[1],sep=''),adj=c(0,0),cex=.75)
    text(-122,44,paste(namesrisk[2],' weighting = ',weightings[2],sep=''),adj=c(0,0),cex=.75)
    text(-122,43.5,paste(namesrisk[3],' weighting = ',weightings[3],sep=''),adj=c(0,0),cex=.75)
    text(-122,43,paste(namesrisk[4],' weighting = ',weightings[4],sep=''),adj=c(0,0),cex=.75)
    text(-122,42,paste0("Marxan (",type,")"),adj=c(0,0),cex=1)
    
    box()
    dev.off() # closes device
  }
 
  #names(fullnames)=biofeats
  species[[4]]=1-species[[4]]
  
  ## costs
  cost=species[[4]]  ## costs
  a=rasterToPolygons(cost)
  a@data$id=1:nrow(a) 
  a@data$cost=1
  a@data$status=0L
  a@data=a@data[,2:4]
  
  
  if(weightings[4]!=0){
    
    targets=weightings[1:3]
    targets=unlist(lapply(targets,function(x)x*-1)) %>% lapply(.,function(x)x*100) %>% unlist() %>% lapply(.,function(x)paste0(x,"%")) %>% unlist()
    
    targets2=((1-weightings[4])*100) %>% paste0(.,"%")
    targets=list(targets,targets2) %>% unlist()
    
    spf=4
    
    ## run marxan
    print("running marxan algorithm")
    results<-marxan(a, species, targets=targets, spf=spf, NUMREPS=1000L, NCORES=2L, BLM=0, lengthFactor=1e-5)
    
    b=results@results@selections %>% as.matrix()
    c=b*1
    d=as.data.frame(c) %>% colSums()
    e=as.matrix(d) %>% as.data.frame()
    colnames(e)[1]="Freq"
    a$selection_freq=e$Freq
    aa=rasterize(a,cost,"selection_freq")
    
    print("writing out results")
    
    cc=aa*-1
    writeRaster(cc,paste0(outdir,"marxan_",paste0(weightings,collapse = "_"),"_",get_date,"_raw_unscaled"),overwrite=T)
    make_png_marxan(cc,get_date = get_date,outdir=outdir,type="raw_unscaled",namesrisk = namesrisk,weightings = weightings)
    
  }
  if(weightings[4]==0){
    
    targets=weightings[1:3]
    targets=unlist(lapply(targets,function(x)x*-1)) %>% lapply(.,function(x)x*100) %>% unlist() %>% lapply(.,function(x)paste0(x,"%")) %>% unlist()
    
    targets2=(weightings[4]*100) %>% paste0(.,"%")
    targets=list(targets,targets2) %>% unlist()
    
    ## format targets for cost
    spf=4
    
    ## run marxan
    print("running marxan algorithm")
    results<-marxan(a, species, targets=targets, spf=spf, NUMREPS=1000L, NCORES=2L, BLM=0, lengthFactor=1e-5)
    
    b=results@results@selections %>% as.matrix()
    c=b*1
    d=as.data.frame(c) %>% colSums()
    e=as.matrix(d) %>% as.data.frame()
    colnames(e)[1]="Freq"
    a$selection_freq=e$Freq
    aa=rasterize(a,cost,"selection_freq")
    
    print("writing out results")
    cc=aa*-1
    writeRaster(cc,paste0(outdir,"marxan_",paste0(weightings,collapse = "_"),"_",get_date,"_raw_unscaled"),overwrite=T)
    make_png_marxan(cc,get_date = get_date,outdir=outdir,type="raw_unscaled",namesrisk = namesrisk,weightings = weightings)
    
  }
  
}

# download species_stack.rds
species=readRDS("species_stack.rds")
namesrisk<-c("Blue sharks","Sea lions","Leatherbacks","Swordfish") # same order as in stack
weightings<-c(-0.25,-0.25,-0.25,0.25) # same order as in stack
get_date="1997-10-01" # date of species habitat suitability layers
outdir="/Volumes/SeaGate/EcoCast_EcoROMS_comparison_ms/Decision-support-tools-for-dynamic-management/sp_layers/"
run_Marxan(get_date = get_date,species = species,outdir = outdir,weightings = weightings,namesrisk = namesrisk)




