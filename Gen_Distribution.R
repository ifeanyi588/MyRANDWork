#THIS IS A FUNCION THAT WILL TAKE UP TO 10 END POINT vmtABILITIES FOR EACH BIN AND 10 ELASTICITY VALUES AND THEN USE THEN
#ADJUST TO THE NEW vmtABILTIY DISTRIBUTION BY USING THE ELASTICITIES AS SLOPE IN STRAIGHT LINE EQUATIONS AND USING LINEAR 
#INTERPOLATION TO FIGURE OUT WHAT THE NEW vmtABILITIES WILL BE!

## environment set up
remove(list = objects()) ## clear all objects in R workspace

options(
  stringsAsFactors = F, ## tell R to treat text as text, not factors
  width = 80, ## set the maximum width of outputs as 80 characters
  scipen = 6, ## discourage R from displaying numbers in scientific notation
  mc.cores = 6, ## set number of (mac) cores used in parallel processing
  start.time= Sys.time()
)
#THIS IS HIGHLY INEFFICIENT PROGRAMMING FROM A HIGHLY INEFFICIENT PROGRAMMER!
find.distribution <- function(e.age1725.lmi, e.age1725.hi, e.age.2640.lmi, e.age.4165.lmi, e.title) {
  prob1 <- c(0.19, 0.46, 0.24, 0.04, 0.02, 0.03, 0.02, 0, 0, 0)
  prob2 <- c(0, 0, 0.05, 0.41, 0.32, 0.08, 0.04, 0.06, 0.04, 0)
  prob3 <- c(0, 0.07, 0.24, 0.35, 0.17, 0.07, 0.04, 0.02, 0.04, 0)
  prob4 <- c(0, 0.06, 0.22, 0.37, 0.18, 0.06, 0.03, 0.02, 0.02, 0.04)
  
  initial.prob.set <- cbind(prob1, prob2, prob3, prob4)
  dist.check <- apply(X = initial.prob.set, MARGIN = 2, FUN = sum)
  
  if (suppressWarnings(all(dist.check) == 1)) {
    cat("\nProbability Distribution is verified!\n\n") 
  } else {
    stop("\nINCORRECT PROBABILITY DISTRIBUTION FUNCTION\n\n")
  }
  
  vmt.bin <- seq(from = 10, to = 32.5, by = 2.5) #setting the x-axis with the 10 daily vmt/capita bins
  
  vmt.bin.new <- cbind((1+e.age1725.lmi)*vmt.bin, (1+e.age1725.hi)*vmt.bin, (1+e.age.2640.lmi)*vmt.bin, 
                        (1 + e.age.4165.lmi)*vmt.bin) #the new set of probabilities based on elasticity changes
  
  prob.set <- data.frame()
  
  for(i in (1:(length(vmt.bin)-1))){
    #first we calculate the slope for each successive pair of data points
    slope.set <- (initial.prob.set[i+1,]-initial.prob.set[i,])/(vmt.bin[i+1]-vmt.bin[i])
    intercept.set <- initial.prob.set[i+1,] - slope.set*vmt.bin[i+1]
    
    yhat <- slope.set*(vmt.bin.new[i,]) + intercept.set
    
    prob.set <- rbind(prob.set, yhat)
    colnames(prob.set) <- c("LMI1725","HI1725","LM2640","LMI4165")
  }
  
  yhat.last <- slope.set*(vmt.bin.new[i+1,]) + intercept.set #since we only run the iteration n-1 times, 
                                                          #we need to interpolate the last point as well

  prob.set <- rbind(prob.set, yhat.last) #include the results of the last interpolation to the prob.set matrix

  prob.set[(prob.set < 0)] <- 0 #changing all values less than 0 to be zero

  dist.check <- apply(X = prob.set, MARGIN = 2, FUN = sum) #finding the sum of all the probabilities
  prob.set <- prob.set/dist.check
  dist.check <- apply(X = prob.set, MARGIN = 2, FUN = sum)

  if (suppressWarnings(all(dist.check) == 1)) {
    cat("\nProbability Distribution is verified!\n\n") 
  } else {
    stop("\nINCORRECT PROBABILITY DISTRIBUTION FUNCTION\n\n")
  }


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################                      
#  MAKING PROBABILITY #                 
#       PLOTS         #     
#######################

setwd("C:/Users/iedochie/Documents/RAND Research/SACOG Greenhouse Gas Reduction Goal Project/Plots")
  
#placing the plots in a pdf file
#coding in the titles first as appropriate
all.plots.pdf <- paste("Change in",e.title,sep = " ")
all.plots.pdf <- paste(all.plots.pdf, "pdf", sep = ".")
all.plots.pdf <- pdf(all.plots.pdf)
  
#The New Distributions are first plotted
all.plots <- matplot(x = vmt.bin.new, y = prob.set, type = "l", 
             col = c(1:4), xlab = "Daily VMT/Capita",
             ylab = "Fraction of Age-Income Cohort",
             main = "VMT/capita Distributions for Several Age-Income Cohorts")
legend <- c("Age 17-25, Low-Middle Income", "Age 17-25, High Income", 
            "Age 26-40, Low-Middle Income", "Age 41-65, Low-Middle Income")
legend(x = 20, y = 0.45, legend = legend, fill = c("black", "red", "green", "blue")) 
dev.off() #ending the file creation routine

#some house keeping for the plots and dumping it in files

main <- c("Age 17-25, Low-Middle Income", "Age 17-25, High Income", 
            "Age 26-40, Low-Middle Income", "Age 41-65, Low-Middle Income")

plots.pdf <- c("age1725.lmi", "age1725.hi", "age.2640.lmi", "age.4165.lmi")

for (i in 1:4){
  #placing the plots in a pdf file
  #coding in the titles first as appropriate
  p.pdf <- paste(e.title, plots.pdf[i], sep = " ")
  p.pdf <- paste(p.pdf,"pdf", sep = ".")
  p.pdf <- pdf(p.pdf)
  #first we reformat the data by picking up the ith column 
  #of before and after sets of probability and vmt bin
  yval.set <- cbind(initial.prob.set[,i],prob.set[,i])
  xval.set <- cbind(vmt.bin,vmt.bin.new[,i])
  
  #now we make the plots below
  matplot(x = xval.set, y = yval.set, type = "l", xlab = "Daily VMT/Capita", 
          ylab = "Fraction of Age-Income Cohort", 
          main = main[i])
  legend (x = 23, y = 0.3, legend = c("Base", "Shift+Transform"), 
          fill = c("black", "red"))
  dev.off() #ending the file creation routine
 
  }

}


