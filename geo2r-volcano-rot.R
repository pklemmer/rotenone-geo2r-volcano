install.packages("ggplot2")
install.packages("ggrepel")
install.packages("readr")
library(readr)
  #Installing required packages
setwd("~/GitHub/rotenone-geo2r-vis")
xp1 <- read_delim("expression/8_dmso_vs_8_rot50_12.tsv", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
xp2 <- read_delim("expression/8_dmso_vs_8_rot50_24.tsv", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
xp3 <- read_delim("expression/8_dmso_vs_8_rot100_24.tsv", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
xp4 <- read_delim("expression/15_dmso_vs_15_rot50_12.tsv", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
xp5<- read_delim("expression/15_dmso_vs_15_rot50_24.tsv", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
xp6 <- read_delim("expression/15_dmso_vs_15_rot100_24.tsv", 
                                    delim = "\t", escape_double = FALSE, 
                                    trim_ws = TRUE)
#Loading generated GEO2R data sets

library(ggplot2)
library(ggrepel)
volcp <- function(xp) {

xp$diffexpressed <- "NO"
xp$diffexpressed[xp$logFC > 0.6 & xp$P.Value < 0.05] <- "UP"
xp$diffexpressed[xp$logFC < -0.6 & xp$P.Value < 0.05] <- "DOWN"
  #Setting the labels for significantly (P-value < 0.05) up and down regulated genes

xp$xplabel <- NA
xp$xplabel[xp$diffexpressed != "NO"] <- xp$GENE_SYMBOL[xp$diffexpressed != "NO"]

ggplot(data=xp, aes(x=logFC, y=-log10(P.Value), col=diffexpressed, label=xplabel)) +
  geom_point() + 
  theme_minimal() +
  geom_text_repel(max.overlaps = Inf) +
  scale_color_manual(values = c("blue", "black", "red")) +
    #Setting the color for the points according to their expression
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")
  #Adding a vertical and horizontal line to indicate logFC and p-value significance
}
  #Defining the function to generate a volcano plot from the given expression data
volcp(xp1)
  #Displaying the plot
jpeg(file="rot-xp4.jpeg")
volcp(xp2)
dev.off()
  #Saving the plot as jpeg to current directory
