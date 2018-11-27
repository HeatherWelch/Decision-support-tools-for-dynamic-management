## Script to run EcoCast (Hazen et al 20181; Welch et al 2018) for 1997-10-01.
## Written by Heather Welch 10-26-2018

library(raster)
library(fields)
library(maps)
library(mapdata)

Run_EcoCast=function(get_date,outdir,namesrisk,weightings){ 
  
  ############ 1. load required functions
  
  ## A. EcoCalc
  EcoCalc<-function(spp=species,risk=weightings,clipTarget=TRUE){
    ecorisk<-species[[1]]*risk[1]+species[[1]]*risk[2]+species[[1]]*risk[3]+species[[1]]*risk[4]
    return(ecorisk)
  }
  
  ## B. PlotEcoROMS
  PlotEcoROMS<-function(r,get_date,outdir,risk=weightings,spp=namesrisk,addtext=TRUE){
    
    EcoCols<-colorRampPalette(c("red","orange","white","cyan","blue"))
    ByCols<-colorRampPalette(c("red","orange","white"))
    
    ####### produce png - unscaled
    png(paste(outdir,"/EcoROMS_original_unscaled_",paste(risk,collapse="_"),'_',get_date,version,'.png',sep=''),width=960,height=1100,units='px',pointsize=20)
    par(mar=c(3,3,.5,.5),las=1,font=2)
    
    if(risk[4]!=0) {
      zlimits=c(r@data@min,r@data@max)
      col=EcoCols(255)
    }
    
    if(risk[4]==0) {
      zlimits=c(r@data@min,r@data@max)
      col=ByCols(255)
    }
    
    image.plot(r,col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
    
    maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
    if (addtext) {
      text(-122,46,format(get_date,format="%b %d %Y"),adj=c(0,0),cex=2) 
      text(-122,45,"Species weightings",adj=c(0,0),cex=1)
      text(-122,44.5,paste(namesrisk[1],' weighting = ',risk[1],sep=''),adj=c(0,0),cex=.75)
      text(-122,44,paste(namesrisk[2],' weighting = ',risk[2],sep=''),adj=c(0,0),cex=.75)
      text(-122,43.5,paste(namesrisk[3],' weighting = ',risk[3],sep=''),adj=c(0,0),cex=.75)
      text(-122,43,paste(namesrisk[4],' weighting = ',risk[4],sep=''),adj=c(0,0),cex=.75)
      text(-122,42,"EcoROMS original (unscaled)",adj=c(0,0),cex=1)

    }
    
    box()
    dev.off()
    
    
    ####### produce raster
    writeRaster(r,filename=paste(outdir,'EcoROMS_original_unscaled_',paste(risk,collapse="_"),"_",get_date,'.grd',sep=''),overwrite=TRUE) 
    
  }
  
  ############ 2. Calculate EcoCast for get_date 
  print(paste("Running EcoROMS: calculating EcoROMS risk for ",get_date,sep=""))
  
  eco_surface=EcoCalc(spp = species,risk=weightings)
  
  PlotEcoROMS(eco_surface,get_date,outdir=outdir,risk=weightings) ## standard directory
  
}

## demo run
# download species_stack.rds
species=readRDS("species_stack.rds")
namesrisk<-c("Blue sharks","Sea lions","Leatherbacks","Swordfish") # same order as in stack
weightings<-c(-0.25,-0.25,-0.25,0.25) # same order as in stack
get_date="1997-10-01" # date of species habitat suitability layers
outdir="/Volumes/SeaGate/EcoCast_EcoROMS_comparison_ms/Decision-support-tools-for-dynamic-management/sp_layers/"
Run_EcoCast(get_date = get_date,outdir = outdir,namesrisk = namesrisk,weightings = weightings)


