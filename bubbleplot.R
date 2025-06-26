install.packages("Rtools")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("scales")

library(ggplot2)
library(reshape2)
library(scales)

setwd('G:/My Drive/Papers/Courtney May/Bubble plot')

pc <- read.csv("relative_abundance.csv")
pc[, -1] <- pc[, -1] * 100
pcm <- melt(pc, id = "Sample")

#jellyfish to full species names
pcm$Sample <- factor(pcm$Sample, levels = unique(pcm$Sample))
levels(pcm$Sample) <- c(
   "Aequorea forskalea","Aurelia solida",
  "Rhizostoma pulmo", "Cotylorhiza tuberculata"
)

#sort axis alphabetically
pcm$Sample <- factor(pcm$Sample, levels = sort(levels(pcm$Sample)))
pcm$variable <- gsub("\\.", " ", pcm$variable)
pcm$variable <- factor(pcm$variable, levels = rev(sort(unique(pcm$variable))))


# delete rows with 0 relative abundance
pcm <- subset(pcm, value > 0)

#not sure what colours to do
sample_colours <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")

p <- ggplot(pcm, aes(x = Sample, y = variable)) +
  geom_point(aes(size = value, fill = Sample),
             shape = 21, colour = "grey30", stroke = 0.3, alpha = 0.85) +
  scale_size_continuous(
    name = "Relative Abundance (%)",
    range = c(3, 16),
    breaks = pretty_breaks(4)
  ) +
  coord_cartesian(ylim=c(-1,29.5)) +
  scale_fill_manual(values = sample_colours, guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic", size = 11),
    axis.text.y = element_text(face = "italic", size = 11),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.6),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p)

ggsave("bubbleplot_final.png", plot = p, width = 9, height = 8, dpi = 600)
