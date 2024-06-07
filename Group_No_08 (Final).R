install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("beanplot")
install.packages("treemap")
install.packages("corrplot")
install.packages("fmsb")
library(corrplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(beanplot)
library(treemap)
library(fmsb)

ds<-read.csv("D:/finallab.csv",header=TRUE,sep=",")
ds
str(ds)
breaks<-c(0, 3, 5, 7, 10)
labels<-c("Bad", "Average", "Good", "Excellent")
ds$quality <- cut(ds$quality, breaks = breaks, labels = labels, include.lowest = TRUE)
ds$quality <- as.character(ds$quality)
head(ds)
str(ds)

numeric_attributes <- names(ds)[sapply(ds, is.numeric)]
numeric_attributes

for (attr in numeric_attributes) {
  p <- ggplot(data = ds, aes(x = !!sym(attr))) +
    geom_histogram(bins = 20, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram Plot of", attr),
         x = attr, y = "Count") +
    theme_minimal()
  print(p)
}


for (attr in numeric_attributes) {
  p <- ggplot(data = ds, aes(x = !!sym(attr))) +
    geom_density(fill = "skyblue", color = "blue", alpha = 0.7) +
    labs(title = paste("Density Plot of", attr),
         x = attr, y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  print(p)
}

for (attr in numeric_attributes){
  dotchart(ds[[attr]], col = "skyblue", pch = 19,
           labels = ds$group,
           main = paste("dot plot of", attr),
           xlab = attr,
           ylab = "Group",
           xlim = range(ds[[attr]])
  )
}

for (attr in numeric_attributes) {
  p <- ggplot(data = ds, aes(y = !!sym(attr))) +
    geom_boxplot( color = "black") +
    labs(title = paste("Box Plot of", attr), y = attr) +
    theme_minimal()
  
  print(p)
}

for (attr in names(ds)) {
  p <- ggplot(data = ds, aes(x = "", y = .data[[attr]])) +
    geom_violin(fill = "skyblue", color = "black") +
    stat_summary(fun.y = median, geom = "point", shape = 18, size = 3, color = "red") +
    stat_summary(fun.ymin = function(x) quantile(x, 0.25), 
                 fun.ymax = function(x) quantile(x, 0.75), 
                 geom = "errorbar", 
                 width = 0.5, 
                 color = "black") +
    labs(title = paste("Violin Plot of", attr), x = "Attribute", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  print(p)
}

for (attr in numeric_attributes) {
  ecdf_values <- ecdf(ds[[attr]])(ds[[attr]])
  ecdf_data <- data.frame(x = ds[[attr]], y = ecdf_values)
  
  p<-ggplot(data = ecdf_data, aes(x = x, y = y)) +
    geom_step() +
    labs(title = paste("ECDF of", attr), x = attr, y = "Cumulative Probability") +
    theme_minimal()
  print(p)
}

for (attr in numeric_attributes) {

    p <- ggplot(data = ds, aes(sample = ds[[attr]])) +
      stat_qq() +
      stat_qq_line() +
      labs(title = paste("Q-Q Plot of", attr),
      x = "Theoretical Quantiles",
      y = "Actual Quantiles") +
      theme_minimal()
      print(p)
}
ggplot(ds, aes(x = quality, fill = quality)) +
  geom_bar() +
  labs(title = " Bar Plot of Quality", x = "Quality", y = "Count") +
  theme_minimal()


pie <- ggplot(data = ds, aes(x = "", fill = quality)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Quality",
       fill = "Quality") +
  theme_void() +
  geom_text(aes(label = paste(round((..count.. / sum(..count..)) * 100), "%")), 
            vjust = -0.5, hjust = .5, size = 3)

print(pie)
 
  quality_counts <- table(ds$quality)
  quality_df <- as.data.frame(quality_counts)
  names(quality_df) <- c("Quality", "Count")
  tm <- treemap(quality_df,
                index = "Quality",    
                vSize = "Count",
                title = "Treemap of Quality",
                vColor = "Quality", 
                palette = "Set3",
                border.col = "black",
                fontsize.labels = c(8, 6),
                overlap.labels = 0.2
  )

  correlation<-cor(ds[,sapply(ds,is.numeric)])
  correlation
  plot(ds$fixed.acidity, ds$citric.acid,
       main = "Fixed Acidity vs Citric Acid",
       xlab = "Fixed Acidity",
       ylab = "Citric Acid",
       col = "skyblue",
       pch = 16,
       cex = 1.2)
  regression_model <- lm(ds$citric.acid ~ ds$fixed.acidity)
  abline(regression_model, col = "red")  
  
  plot(ds$fixed.acidity, ds$pH,
       main = "Fixed Acidity vs pH",
       xlab = "Fixed Acidity",
       ylab = "pH",
       col = "skyblue",
       pch = 16,
       cex = 1.2)
  regression_model <- lm(pH ~ fixed.acidity, data = ds)
  abline(regression_model, col = "red")
  
  plot(ds$volatile.acidity, ds$residual.sugar,
       main = "Volatile Acidity vs Residual Sugar",
       xlab = "Volatile Acidity",
       ylab = "Residual Sugar",
       col = "skyblue",
       pch = 16,
       cex = 0.8)
  regression_model <- lm(residual.sugar ~ volatile.acidity, data = ds)
  abline(regression_model, col = "red")
  
  correlation_matrix <-cor(ds[,sapply(ds,is.numeric)])
  color_palette <- colorRampPalette(c("white", "red"))(n = 100)
  corrplot(correlation_matrix, 
           corrplot(correlation_matrix, 
                    method = "color",
                    col = color_palette,
                    type = "full",
                    tl.srt = 45,
                    title = "Correlation Heatmap"
           )
  )

  ggplot(ds, aes(x = citric.acid, y = alcohol, size = fixed.acidity)) +
    geom_point(color = "#0072B2", alpha = 0.5, shape = 21, fill = "#56B4E9")+
    labs(title = "Bubble Plot for Citric Acid, Alcohol and fixed acidity",
         x = "Citric Acid",
         y = "Alcohol",
         size = "Fixed Acidity") +
    scale_size_continuous(range = c(1, 6)) + 
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))

  ggplot(ds, aes(x = ds$quality, y = ds$pH)) +
    geom_violin(fill = "#0072B2", color = "#56B4E9", alpha = 0.7) +
    geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
    labs(title = "Violin Plot of Numeric Variable by Category",
         x = "Quality",
         y = "pH") +
    theme_minimal()

  instance1 <- ds[1, ]
  instance2 <- ds[150, ]
  comparison <- rbind(instance1, instance2)
  comparison$Instance <- c("Instance 1", "Instance 2")
  comparison_long <- gather(comparison, key = "Variable", value = "Value", -Instance)
  ggplot(comparison_long, aes(x = Variable, y = Value, fill = Instance)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Comparison of Two Instances",
         x = "Variable",
         y = "Value",
         fill = "Instance") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

  
  

  