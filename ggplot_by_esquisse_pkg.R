library(ggplot2)

mydata <- iris
head(mydata)
unique(mydata$Species)
dim(mydata)


library(ggplot2)

ggplot(mydata) +
 aes(x = Petal.Length, y = Petal.Width) +
 geom_point(size = 2L, colour = "#d8576b") +
 labs(x = "Length", y = "Width", title = "Visualization of Iris dataset ", subtitle = "Petal distribution of iris by species", caption = "Figure 1. This shows the distribution of the Iris petal length and width by species") +
 theme_bw() +
 facet_grid(vars(), vars(Species))
