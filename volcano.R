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

volcano_f <- function(xp) {
xp$diffexpressed <- "NO"
xp$diffexpressed[xp$logFC > 0.6 & xp$adj.P.Val < 0.05] <- "UP"
xp$diffexpressed[xp$logFC < -0.6 & xp$adj.P.Val < 0.05] <- "DOWN"

xp$xplabel <- NA
xp$xplabel[xp$diffexpressed != "NO"] <- xp$GENE_SYMBOL[xp$diffexpressed != "NO"]

ggplot(data=xp, aes(x=logFC, y=-log10(adj.P.Val), col=diffexpressed, label=xplabel)) +
  geom_point() + 
  theme_minimal() +
  geom_text_repel(max.overlaps = Inf) +
  scale_color_manual(values = c("blue", "black", "red")) +
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")
}
volcano_f(xp1)
volcano_f(xp2)
volcano_f(xp3)

  

