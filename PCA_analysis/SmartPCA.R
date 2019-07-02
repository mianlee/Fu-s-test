library(ggplot2)

# Read data, the .evec file, and assgin column names to each column by using col.names().
# Since we calculated four components, this first column is going to be Sample ID, and followd by four columns of coordinates of four components.
# The last column is always the name of family/population group.

evecData <- read.table("WE_An_ind_transverSNPs.evec", col.names=c("SampleID", "PC1", "PC2", "PC3", "PC4","Population"))


# plot the first and second components
ggplot(evecData, aes(x = PC1,y = PC2)) +
  geom_point() +
  ggtitle("SmartPCA Plot") + 
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5))


# advance plot
ggplot(evecData, aes(x = PC1,y = PC2)) +
  geom_point(aes(shape = Population, color = Population), size = 2.5) +
  ggtitle("SmartPCA Plot") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size =15, face ="bold"),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size=10)) +
  scale_shape_manual(values = rep(c(0:3,5:7,9:12,15:19,25), len = 92)) +
  scale_colour_manual(values = rep(c("red", "blue", "green", "orange","black","lightblue"),len =92))

# "scale_shape_manual(values = rep(1:17, len = 92)" will choose shape 1 to 17 and loop through till 92.
# "scale_shape_manual(values = rep(c(1,3,4,6,7), len = 92))" will use shape 1,3,4,6,7,and loop through till 92.


#Only plot
ggplot(evecData, aes(x = PC1,y = PC2)) +
  geom_point(aes(shape = Population, color = Population), size = 2.5) +
  ggtitle("SmartPCA Plot") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size =15, face ="bold"),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size=10),
        legend.position="none"
        ) +
  scale_shape_manual(values = rep(c(0:3,5:7,9:12,15:19,25), len = 92)) +
  scale_colour_manual(values = rep(c("red", "blue", "green", "orange","black","lightblue"),len =92))




#Extract legend

library(ggplot2)
library(ggpubr)

P<- ggplot(evecData, aes(x = PC1,y = PC2)) +
  geom_point(aes(shape = Population, color = Population), size = 2.5) +
  ggtitle("SmartPCA Plot") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size =15, face ="bold"),
        axis.text.x = element_text(size =10),
        axis.text.y = element_text(size=10)
  ) +
  scale_shape_manual(values = rep(c(0:3,5:7,9:12,15:19,25), len = 92)) +
  scale_colour_manual(values = rep(c("red", "blue", "green", "orange","black","lightblue"),len =92))

# Extract the legend. Returns a gtable
leg <- get_legend(P)
# Convert to a ggplot and print
as_ggplot(leg)



#Final Version
ggplot(evecData, aes(x = PC1,y = PC2)) +
  geom_point(aes(shape = Population, color = Population), size = 2.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size =11, margin = margin (t = 9) ),
        axis.text.y = element_text(size=11, margin = margin (r = 9) ),
        legend.position = c(0.3,0.85),        #adjust legend relative poition with the plot
        legend.key.size = unit(0.20, "cm"),   #the size of legend
        legend.title = element_blank(),       #get rid of legend title
        axis.title.x = element_text (margin = margin (t = 20) ),  # X title position to the x axis
        axis.title.y = element_text (margin = margin (r = 15) )
  ) +
  scale_shape_manual(values = rep(c(0:3,5:7,9:12,15:19,25), len = 92)) +
  scale_colour_manual(values = rep(c("red", "blue", "green", "orange", "black", "lightblue"),len =92))




# To avoid visual clutter, modern individuals as grey dots, and used colored and labeledsymbols to represnet the ancient individuals.
# Color coding modern individuals and representing ancient ones as grey dots.

library(ggplot2)

#Load data
Ancient <- read.table("Ancient_PCA.txt", col.names=c("SampleID", "PC1", "PC2", "PC3", "PC4","Population"))
WE <- read.table("WestEurasians_PCA.txt", col.names=c("SampleID", "PC1", "PC2", "PC3", "PC4","Population"))


# Plot only WestEurasians
ggplot(WE, aes(x = PC1,y = PC2)) +
  geom_point(shape = 16, color = "grey", size = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size =11, margin = margin (t = 9) ),
        axis.text.y = element_text(size=11, margin = margin (r = 9) ),
        legend.position="none",
        axis.title.x = element_text (margin = margin (t = 20) ),
        axis.title.y = element_text (margin = margin (r = 15) ))

# Plot only Ancient
ggplot(Ancient, aes(x = PC1,y = PC2)) +
  geom_point(aes(shape = Population, color = Population), size = 2.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size =11, margin = margin (t = 9) ),
        axis.text.y = element_text(size=11, margin = margin (r = 9) ),
        legend.position = c(0.2,0.86),        #adjust legend relative poition with the plot
        legend.key.size = unit(0.20, "cm"),   #the size of legend
        legend.title = element_blank(),       #get rid of legend title
        axis.title.x = element_text (margin = margin (t = 20) ),  # X title position to the x axis
        axis.title.y = element_text (margin = margin (r = 15) )
  ) +
  scale_shape_manual(values = rep(c(10,11,12,15,19,23,25), len = 33)) +
  scale_colour_manual(values = rep(c("red", "blue", "green", "orange", "black", "lightblue"),len = 33))


#Ancient color
ggplot(Ancient, aes(x = PC1,y = PC2)) +
  geom_point(data = WE, aes(x = PC1,y = PC2), shape = 16, color = "grey", size = 2) +
  geom_point(aes(shape = Population, color = Population), size = 2.6) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size =11, margin = margin (t = 9) ),
        axis.text.y = element_text(size=11, margin = margin (r = 9) ),
        legend.position = c(0.2,0.85),        #adjust legend relative poition with the plot
        legend.key.size = unit(0.30, "cm"),   #the size of legend
        legend.title = element_blank(),       #get rid of legend title
        axis.title.x = element_text (margin = margin (t = 20) ),  # X title position to the x axis
        axis.title.y = element_text (margin = margin (r = 15) )
  ) +
  scale_shape_manual(values = rep(c(10,11,12,15,19,23,25), len = 33)) +
  scale_colour_manual(values = rep(c("red", "blue", "green", "orange", "black", "lightblue"),len = 33))


#WE color
ggplot(WE, aes(x = PC1,y = PC2)) +
  geom_point(data = Ancient, aes(x = PC1,y = PC2), shape = 16, color = "grey", size = 2) +
  geom_point(aes(shape = Population, color = Population), size = 2.4) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size =11, margin = margin (t = 9) ),
        axis.text.y = element_text(size=11, margin = margin (r = 9) ),
        legend.position = c(0.2,0.84),        #adjust legend relative poition with the plot
        legend.key.size = unit(0.2, "cm"),   #the size of legend
        legend.title = element_blank(),       #get rid of legend title
        axis.title.x = element_text (margin = margin (t = 20) ),  # X title position to the x axis
        axis.title.y = element_text (margin = margin (r = 15) )
  ) +
  scale_shape_manual(values = rep(c(0,1,8,10,11,12,15,19,23,25), len = 59)) +
  scale_colour_manual(values = rep(c("red", "blue", "green", "orange", "black", "lightblue"),len = 59))



