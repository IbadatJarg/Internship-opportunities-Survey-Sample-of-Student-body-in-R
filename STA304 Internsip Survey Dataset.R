#load data
getwd()
data <- read.csv("ProgressReport.csv")

#get sample size
sample_size <- nrow(data)

#compute proportion
proportion <- sum(data$CE %in% c("Y", "C")) / sample_size

#one-sample proportion test:
proportion_test <- prop.test(x = proportion * sample_size, n = sample_size, 
                             p = 0.5, alternative = "less", conf.level = 0.95, correct = TRUE)

#Pie chart for distribution of Co-op experience:
counts <- table(data$CE)
pie_percentage <- prop.table(counts) * 100
pct <- paste(names(counts), "(", pie_percentage, "%)", sep = " ")
pie(counts, labels = pct, main = "Distribution of Co-op Experience")

# Install and load the necessary packages
library(ggplot2)

# Sample data with program of study
categories <- c("GIS", "Bioinformatics", "Humanities", "Social Sciences", "Natural Sciences", "Economics/Finance/Business", "Mathematics", "Statistics", "Computer Science")
values <- c(1, 1, 2, 2, 1, 20, 13, 46, 14)

# Create a horizontal bar chart
bar_chart <- ggplot(data.frame(category = categories, value = values),
                    aes(x = value, y = category, fill = category)) +
  geom_bar(stat = "identity") +
  labs(title = "Program of Study", x = "Num of Students", y = "Category of Program") +  # Add labels
  theme_minimal()

# Display the bar chart
print(bar_chart)

# Pie chart for Domesticity
population_labels <- c("International Student", "Domestic student")
population_data <- c(35, 50 - 35) 
population_percentage <- (population_data / sum(population_data)) * 100
pie(population_percentage,
    labels=population_labels,
    col=c("blue", "gray"),
    main="Domesticity",
    cex=0.8)
label_p <- sprintf("%.1f%%", population_percentage)
legend("topright", legend = paste(population_labels, label_p), fill = c("blue", "gray"))