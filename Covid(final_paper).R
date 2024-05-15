install.packages("leaflet")
install.packages("rnaturalearth")
install.packages("sf")
install.packages("maps")
install.packages("plotly")
install.packages("fmsb")
install.packages("gridExtra")
install.packages("cowplot")

library(cowplot)
library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
library(tidyr)
library(leaflet)
library(maps)
library(rnaturalearth)
library(sf)
library(plotly)
library(fmsb)
library(gridExtra)


colSums(is.na(data))
data <- read.csv("C:/Users/ridoy/Downloads/country_wise_latest.csv")
str(data)
names(data)

#heatmap
correlation_matrix <- cor(data[, sapply(data, is.numeric)])
correlation_matrix_melted <- melt(correlation_matrix)

heatmap_correlation <- ggplot(data = correlation_matrix_melted, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(color = "black", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "maroon") +
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable")
print(heatmap_correlation)

#scatterplot
combined_scatter_plot <- ggplot() +
  geom_point(data = data, aes(x = Confirmed, y = Recovered, color = "Confirmed vs Recovered")) +
  geom_point(data = data, aes(x = Active, y = Deaths, color = "Active vs Deaths")) +
  labs(title = "Relationships between COVID-19 Cases",
       x = "Confirmed and Active Cases",
       y = "Recovered and Deaths",
       color = "Relationship") +
  scale_color_manual(values = c("Confirmed vs Recovered" = "blue", "Active vs Deaths" = "red"))
print(combined_scatter_plot)

# bar plot
who_region_count <- data %>%
  count(WHO.Region)


bar_plot_who_region <- ggplot(who_region_count, aes(x = WHO.Region, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n, y = n), vjust = -0.5, hjust = 0.5, size = 3) +  
  labs(title = "Bar Plot of WHO Regions", x = "WHO Region", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) 

confirmed_by_country <- data %>%
  group_by(Country.Region) %>%
  summarise(total_confirmed = sum(Confirmed, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total_confirmed)) %>%
  head(n = 10)

bar_chart <- ggplot(data = confirmed_by_country, aes(x = reorder(Country.Region, total_confirmed), y = total_confirmed)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = total_confirmed, y = total_confirmed), vjust = -0.5, hjust = 0.5, size = 3)+ 
  labs(title = "Top Countries with Highest Number of Confirmed Cases",
       x = "Country", y = "Total Confirmed Cases")

combined_plot <- grid.arrange(bar_plot_who_region, bar_chart, nrow = 1)

print(combined_plot)

# Histogram
hist_deaths_100_cases <- ggplot(data, aes(x = Deaths...100.Cases)) +
  geom_histogram(binwidth = 0.5, fill = "darkred", color = "black") +
  labs(title = "Distribution of Deaths...100.Cases",
       x = "Deaths...100.Cases", y = "Frequency") +
  theme_minimal()

hist_recovered_100_cases <- ggplot(data, aes(x = Recovered...100.Cases)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Recovered...100.Cases",
       x = "Recovered...100.Cases", y = "Frequency") +
  theme_minimal()

hist_deaths_100_recovered <- ggplot(data, aes(x = Deaths...100.Recovered)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Deaths...100.Recovered",
       x = "Deaths...100.Recovered", y = "Frequency") +
  theme_minimal()
combined_histogram <- plot_grid(hist_deaths_100_cases, hist_recovered_100_cases, hist_deaths_100_recovered, nrow = 3)

print(combined_histogram)



#line plot
combined_plot <- plot_grid(
  line_deaths_vs_new_deaths + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)),
  line_recovered_vs_new_recovered + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)),
  line_change_in_confirmed + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)),
  nrow = 3, align = "v")


print(combined_plot)




#pie chart 
pie_chart_who_region <- ggplot(data, aes(x = "", fill = WHO.Region)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of WHO Regions") +
  theme_void()

confirmed_by_region <- data %>%
  group_by(WHO.Region) %>%
  summarise(total_confirmed = sum(Confirmed, na.rm = TRUE))

pie_chart_who <- ggplot(data = confirmed_by_region, aes(x = "", y = total_confirmed, fill = WHO.Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  
  labs(title = "Distribution of Confirmed Cases by WHO Region") +  
  scale_fill_brewer(palette = "Set3") +  
  theme_void()

deaths_recovered_by_region <- data %>%
  group_by(WHO.Region) %>%
  summarise(total_deaths = sum(Deaths, na.rm = TRUE),
            total_recovered = sum(Recovered, na.rm = TRUE))

pie_chart_deaths <- ggplot(data = deaths_recovered_by_region, aes(x = "", y = total_deaths, fill = WHO.Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  
  labs(title = "Distribution of Deaths by WHO Region") +  
  scale_fill_brewer(palette = "Set3") +  
  theme_void()

pie_chart_recovered <- ggplot(data = deaths_recovered_by_region, aes(x = "", y = total_recovered, fill = WHO.Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  
  labs(title = "Distribution of Recovered Cases by WHO Region") +  
  scale_fill_brewer(palette = "Set3") +  
  theme_void()

deaths_recovered_by_region_per_hundred <- data %>%
  group_by(WHO.Region) %>%
  summarise(total_deaths_per_hundred = sum(Deaths...100.Cases, na.rm = TRUE),
            total_recovered_per_hundred = sum(Recovered...100.Cases, na.rm = TRUE))

pie_chart_recovered_per_hundred <- ggplot(data = deaths_recovered_by_region_per_hundred, aes(x = "", y =  total_recovered_per_hundred, fill = WHO.Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  
  labs(title = "Distribution of Recovered Cases per Hundred Cases by WHO Region") + 
  scale_fill_brewer(palette = "Set3") + 
  theme_void()

pie_chart_deaths_per_hundred <- ggplot(data = deaths_recovered_by_region_per_hundred, aes(x = "", y =  total_deaths_per_hundred, fill = WHO.Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  
  labs(title = "Distribution of Deaths per Hundred Cases by WHO Region") + 
  scale_fill_brewer(palette = "Set3") + 
  theme_void() 

combined_pie_chart <- plot_grid(pie_chart_who_region, pie_chart_who, pie_chart_deaths, pie_chart_recovered, pie_chart_recovered_per_hundred, pie_chart_deaths_per_hundred, nrow = 3)

print(combined_pie_chart)



#heatmap in who region
agg_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(total_confirmed = sum(Confirmed, na.rm = TRUE))



heatmap <- ggplot(agg_data, aes(x = "", y = WHO.Region, fill = total_confirmed)) +
  geom_tile(color = "white") +  # Add tiles
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  
  labs(title = "Heatmap of Total Confirmed Cases by WHO Region",
       x = "", y = "WHO Region", fill = "Total Confirmed Cases") +  
  theme(axis.text.x = element_blank(),  
        plot.title = element_text(hjust = 0.5))  

print(heatmap)







#3D Visualization
fig <- plot_ly(data, x = ~Confirmed, y = ~Deaths, z = ~Recovered,
               color = ~WHO.Region, colors = RColorBrewer::brewer.pal(8, "Set1"), 
               type = "scatr3d", mode = "markers")

fig

#worldmap
world_map <- map_data("world")



data_merged <- merge(world_map, data, by.x = "region", by.y = "Country.Region", all.x = TRUE)


p <- ggplot(data_merged, aes(long, lat, group = group, fill = Confirmed)) +
  geom_polygon(color = "darkred") +
  scale_fill_gradient(low = "darkgreen", high = "orange") +
  labs(fill = "Confirmed Cases", title = "Confirmed COVID-19 Cases per Country") +
  theme_void()


p <- ggplotly(p)


p <- p %>% layout(
  dragmode = "zoom", 
  hovermode = "closest",
  geo = list(
    projection = list(
      type = 'mercator'
    ),
    showcountries = TRUE  
  )
)

p


#sunburst chart
region_death_counts <- data %>%
  group_by(WHO.Region) %>%
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE))

selected_region <- "Europe"

selected_region_data <- data %>%
  filter(WHO.Region == selected_region)

hierarchical_data <- data.frame(
  ID = c(as.character(unique(selected_region_data$Country.Region)), selected_region),
  Label = c(as.character(unique(selected_region_data$Country.Region)), selected_region),
  Parent = c(rep(selected_region, nrow(selected_region_data)), NA),
  Deaths = c(selected_region_data$Deaths, NA)  
)

if (is.na(hierarchical_data$Deaths[nrow(hierarchical_data)])) {
  hierarchical_data$Deaths[nrow(hierarchical_data)] <- region_death_counts$Total_Deaths[region_death_counts$WHO.Region == selected_region]
}

plot_ly(hierarchical_data, ids = ~ID, labels = ~Label, parents = ~Parent, type = 'sunburst', hoverinfo = 'label+text', text = ~paste('Deaths: ', Deaths)) %>%
  layout(title = paste("Sunburst Chart of", selected_region, "Countries"))




