

###########Load in Master Data
TreeMaster <- read_excel("D:/00_Work/Tree Ring/Data/VanderSleenSupplementaryInformation.xlsx")
View(TreeMaster)

HKK_Numeric <- read_excel("D:/00_Work/Tree Ring/Data/HKK_NumericVariables.xlsx")

##Basic Scatterplots of all variables w/time
###function for cor plot
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}

panel.cor = function(x,y){
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
###########

pairs(HKK_Numeric[,4:10],
      lower.panel = panel.cor,
      upper.panel = upper.panel)

###################################

ggpairs(HKK_Numeric[,4:10], ggplot2::aes(colour = HKK_Numeric$Species), title = "correlogram HKK")
##########
ggcorr(HKK_Numeric[,4:10], method = c("everything", "pearson"))
