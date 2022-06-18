sessionInfo()
install.packages("readr")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("svglite")
library(readr)
  #Installing required packages
setwd("~/GitHub/rotenone-geo2r-volcano")
xp1 <- read_delim("Expression/8_dmso_vs_8_rot50_12.tsv", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
xp2 <- read_delim("Expression/8_dmso_vs_8_rot50_24.tsv", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
xp3 <- read_delim("Expression/8_dmso_vs_8_rot100_24.tsv", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
#Loading GEO2R data sets

library(ggplot2)
library(ggrepel)
volcp <- function(xp) {

xp$diffexpressed <- "Not sig"
xp$diffexpressed[xp$logFC > 0.26 & xp$P.Value < 0.05] <- "UP"
xp$diffexpressed[xp$logFC < -0.26 & xp$P.Value < 0.05] <- "DOWN"
  #Setting the labels for significantly (P-value < 0.05) up and down regulated genes

xp$xplabel <- NA
xp$xplabel[xp$diffexpressed != "Not sig"] <- xp$GENE_SYMBOL[xp$diffexpressed != "Not sig"]

ggplot(data=xp, aes(x=logFC, y=-log10(P.Value), col=diffexpressed, label=xplabel)) +
  geom_point() + 
  theme_grey() +
  scale_color_manual(values = c("blue", "dimgrey", "red")) +
    #Setting the color for the points according to their expression
  geom_vline(xintercept=c(-0.26, 0.26), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")+
  #Adding a vertical and horizontal line to indicate logFC and p-value significance
  theme(legend.title=element_blank())
  #Hiding the legend title 
  
}
  #Defining the function to generate a volcano plot from the given expression data
library(svglite)
svglite(file="Plots/rot-xp1.svg")
volcp(xp1)
dev.off()
  #Saving the plot as jpeg to current directory
