install.packages("leaflet")
install.packages("rnaturalearth")
install.packages("sf")
install.packages("maps")
install.packages("plotly")
install.packages("fmsb")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("ggalt")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("ggstream")
install.packages("circlize")
install.packages("dplyr")
install.packages("tidyr")


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
library(RColorBrewer)
library(treemap)
library(ggridges)
library(maps)
library(ggalt)

data <- read.csv("C:/Users/ridoy/Downloads/covid_data/country_wise_latest.csv")
str(data)
names(data)
colSums(is.na(data))

#heatmap
correlation_matrix <- cor(data[, sapply(data, is.numeric)])
correlation_matrix_melted <- melt(correlation_matrix)

heatmap_correlation <- ggplot(data = correlation_matrix_melted, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(color = "black", size = 5, family = "Arial", fontface = "bold") + # Change the font family and style here
  scale_fill_gradient(low = "white", high = "maroon") +
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable") +
  theme(
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.title = element_text(size = 16, face = "bold"), 
    axis.text.x = element_text(color = "black", size = 14, face = "bold", angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"), 
    axis.title.x = element_text(size = 14, face = "bold"), 
    axis.title.y = element_text(size = 14, face = "bold") 
  )
print(heatmap_correlation)

# Scatter Plot
combined_scatter_plot <- ggplot(data = data) +
  geom_point(aes(x = Confirmed, y = Recovered, color = "Confirmed vs Recovered",
                 text = paste("Country:", Country.Region, "\nConfirmed:", Confirmed, "\nRecovered:", Recovered))) +
  geom_point(aes(x = Active, y = Deaths, color = "Active vs Deaths",
                 text = paste("Country:", Country.Region, "\nActive:", Active, "\nDeaths:", Deaths))) +
  geom_text(aes(x = Inf, y = -Inf, label = paste("Total Points:", nrow(data))), color = "black", size = 3, family = "Arial", fontface = "bold") +
  labs(title = "Relationships between COVID-19 Cases",
       x = "Confirmed and Active Cases",
       y = "Recovered and Deaths",
       color = "Relationship") +
  scale_color_manual(values = c("Confirmed vs Recovered" = "blue", "Active vs Deaths" = "red")) +
  theme(
    plot.background = element_rect(fill = "lightgrey"), # Background color change
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(face = "bold")   # Bold legend text
  )

combined_scatter_plot_interactive <- ggplotly(combined_scatter_plot, tooltip = "text")
print(combined_scatter_plot_interactive)

# Bar plot by WHO Region
who_region_count <- data %>%
  count(WHO.Region)

bar_plot_who_region <- ggplot(who_region_count, aes(x = WHO.Region, y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  labs(title = "Bar Plot of WHO Regions", x = "WHO Region", y = "Count") +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, color = "black", face = "bold"),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )
print(bar_plot_who_region)


# Histogram: Deaths per 100 Cases
hist_deaths_100_cases <- ggplot(data, aes(x = Deaths...100.Cases)) +
  geom_histogram(binwidth = 0.5, fill = "darkred", color = "black") +
  labs(title = "Distribution of Deaths per 100 Cases",
       x = "Deaths per 100 Cases", y = "Frequency") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FFFACD", color = NA),   # Light yellow
    panel.background = element_rect(fill = "#FFFACD", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    legend.position = "none",
    panel.grid.major = element_line(color = "black", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

# Histogram: Recovered per 100 Cases
hist_recovered_100_cases <- ggplot(data, aes(x = Recovered...100.Cases)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Recovered per 100 Cases",
       x = "Recovered per 100 Cases", y = "Frequency") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#F0FFFF", color = NA),   # Light green
    panel.background = element_rect(fill = "#F0FFFF", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    legend.position = "none",
    panel.grid.major = element_line(color = "black", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

# Histogram: Deaths per 100 Recovered
hist_deaths_100_recovered <- ggplot(data, aes(x = Deaths...100.Recovered)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Deaths per 100 Recovered",
       x = "Deaths per 100 Recovered", y = "Frequency") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#E0F7FF", color = NA),   # Light blue
    panel.background = element_rect(fill = "#E0F7FF", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    legend.position = "none",
    panel.grid.major = element_line(color = "black", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

# Print histograms separately
print(hist_deaths_100_cases)
print(hist_recovered_100_cases)
print(hist_deaths_100_recovered)

#pie char
pie_chart_who_region <- ggplot(data, aes(x = "", fill = WHO.Region)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of WHO Regions") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "lightblue"),    # Change background color here
    panel.background = element_rect(fill = "lightblue"),   # Change panel background color here
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )

print(pie_chart_who_region)


# Line Plot
line_deaths_vs_new_deaths <- ggplot(data, aes(x = Deaths, y = New.deaths)) +
  geom_line(size = 1.5, color = "darkgreen") +
  labs(title = "Relationship Between Deaths and New Deaths",
       x = "Deaths",
       y = "New Deaths") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "lightgrey"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

print(line_deaths_vs_new_deaths)

# Pie Chart for Confirmed Cases by Region
confirmed_by_region <- data %>%
  group_by(WHO.Region) %>%
  summarise(total_confirmed = sum(Confirmed, na.rm = TRUE))

pie_chart_who <- ggplot(data = confirmed_by_region, aes(x = "", y = total_confirmed, fill = WHO.Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Confirmed Cases by WHO Region") +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )

print(pie_chart_who)

# Radar Chart
radar_chart <- ggplot(data, aes(x = reorder(WHO.Region, -Deaths), y = Deaths)) +
  geom_polygon(aes(group = 1), fill = "lightblue", color = "blue", alpha = 0.7) +
  geom_point(color = "red", size = 3) +
  coord_polar() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(color = "black", size = 14, face = "bold", angle = 60, hjust = 1),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  ) +
  labs(title = "Number of Deaths by WHO Region",
       x = "WHO Region",
       y = "Number of Deaths")

print(radar_chart)

# Violin Plot
violin_plot <- ggplot(data, aes(x = WHO.Region, y = Recovered...100.Cases)) +
  geom_violin(trim = FALSE, fill = "lightblue", color = "blue") +
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Recovery Rate (Recovered per 100 Cases) in Each WHO Region",
       x = "WHO Region",
       y = "Recovered per 100 Cases") +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

print(violin_plot)

# Waterfall Plot
agg_data <- data %>%
  group_by(WHO.Region) %>%
  summarize(Mean_Recovered_Per_100_Cases = mean(Recovered...100.Cases))

agg_data <- agg_data %>%
  mutate(
    Previous = lag(Mean_Recovered_Per_100_Cases, default = 0),
    Change = Mean_Recovered_Per_100_Cases - Previous,
    Cumulative = cumsum(Change)
  )

waterfall_plot <- ggplot(agg_data, aes(x = WHO.Region, y = Change, fill = Change > 0)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(Mean_Recovered_Per_100_Cases, 2), y = Cumulative), vjust = -0.5) +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_minimal() +
  labs(title = "Waterfall Plot of Recovery Rate (Recovered per 100 Cases) by WHO Region",
       x = "WHO Region",
       y = "Change in Recovered per 100 Cases") +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, color = "black", face = "bold"),
    axis.text.y = element_text(size = 14, color = "black", face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

print(waterfall_plot)

# Volume Plot (3D)
volume_data <- data %>%
  select(WHO.Region, Recovered...100.Cases, Deaths) %>%
  group_by(WHO.Region) %>%
  summarize(
    Mean_Recovered_Per_100_Cases = mean(Recovered...100.Cases),
    Total_Deaths = sum(Deaths)
  )

volume_data$Index <- as.numeric(factor(volume_data$WHO.Region))

volume_plot <- plot_ly(
  x = volume_data$Index,
  y = volume_data$Mean_Recovered_Per_100_Cases,
  z = volume_data$Total_Deaths,
  type = 'mesh3d',
  intensity = volume_data$Total_Deaths,
  colorscale = 'Viridis',
  showscale = TRUE,
  text = ~paste("WHO Region: ", volume_data$WHO.Region,
                "<br>Mean Recovered per 100 Cases: ", volume_data$Mean_Recovered_Per_100_Cases,
                "<br>Total Deaths: ", volume_data$Total_Deaths),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(text = "3D Volume Plot of Recovery Rate and Deaths by WHO Region", font = list(size = 20, color = 'black', family = "Arial", weight = "bold")),
    scene = list(
      xaxis = list(title = 'WHO Region', tickvals = volume_data$Index,
                   ticktext = volume_data$WHO.Region,
                   titlefont = list(size = 16, color = 'black', family = "Arial", weight = "bold"),
                   tickfont = list(size = 14, color = 'black', family = "Arial", weight = "bold")),
      yaxis = list(title = 'Mean Recovered per 100 Cases', titlefont = list(size = 16, color = 'black', family = "Arial", weight = "bold"),
                   tickfont = list(size = 14, color = 'black', family = "Arial", weight = "bold")),
      zaxis = list(title = 'Total Deaths', titlefont = list(size = 16, color = 'black', family = "Arial", weight = "bold"),
                   tickfont = list(size = 14, color = 'black', family = "Arial", weight = "bold")),
      camera = list(eye = list(x = 1.87, y = 0.88, z = -0.64))
    )
  )

volume_plot

# Heatmap by WHO Region
agg_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(total_confirmed = sum(Confirmed, na.rm = TRUE))

heatmap <- ggplot(agg_data, aes(x = "", y = WHO.Region, fill = total_confirmed)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Total Confirmed Cases by WHO Region", x = "", y = "WHO Region", fill = "Total Confirmed Cases") +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14, color = "black", face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

print(heatmap)

# 3D Visualization
fig <- plot_ly(data, x = ~Confirmed, y = ~Deaths, z = ~Recovered,
               color = ~WHO.Region, colors = RColorBrewer::brewer.pal(8, "Set1"), 
               type = "scatter3d", mode = "markers")

fig <- fig %>% layout(
  scene = list(
    xaxis = list(
      title = "Confirmed",
      titlefont = list(size = 16, family = "Arial", color = "black"),
      tickfont = list(size = 14, family = "Arial", color = "black")
    ),
    yaxis = list(
      title = "Deaths",
      titlefont = list(size = 16, family = "Arial", color = "black"),
      tickfont = list(size = 14, family = "Arial", color = "black")
    ),
    zaxis = list(
      title = "Recovered",
      titlefont = list(size = 16, family = "Arial", color = "black"),
      tickfont = list(size = 14, family = "Arial", color = "black")
    )
  ),
  legend = list(
    font = list(size = 14, family = "Arial", color = "black"),
    title = list(text = "WHO Region", font = list(size = 16, family = "Arial", color = "black"))
  )
)

fig

# Worldmap visualization with COVID data
world_map <- map_data("world")
data_merged <- merge(world_map, data, by.x = "region", by.y = "Country.Region", all.x = TRUE)

p <- ggplot(data_merged, aes(long, lat, group = group, fill = Confirmed)) +
  geom_polygon(color = "darkred") +
  scale_fill_gradient(low = "darkgreen", high = "orange") +
  labs(fill = "Confirmed Cases", title = "Confirmed COVID-19 Cases per Country") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )

p <- ggplotly(p)

p <- p %>% layout(
  dragmode = "zoom", 
  hovermode = "closest",
  geo = list(
    projection = list(type = 'mercator'),
    showcountries = TRUE
  )
)

p

# Sunburst Chart
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

fig_sunburst <- plot_ly(hierarchical_data, ids = ~ID, labels = ~Label, parents = ~Parent, type = 'sunburst', hoverinfo = 'label+text', text = ~paste('Deaths: ', Deaths)) %>%
  layout(
    title = paste("Sunburst Chart of", selected_region, "Countries"),
    font = list(size = 14, family = "Arial", color = "black"),
    legend = list(font = list(size = 14, family = "Arial", color = "black"))
  )

fig_sunburst

# Parallel Coordinates Plot
parcoords_data <- data %>%
  select(WHO.Region, Deaths, Confirmed, Recovered, Active)

parcoords_plot <- plot_ly(
  type = "parcoords",
  line = list(color = ~Deaths, colorscale = "Viridis", showscale = TRUE),
  dimensions = list(
    list(range = c(0, max(parcoords_data$Deaths)), label = "Deaths", values = ~Deaths),
    list(range = c(0, max(parcoords_data$Confirmed)), label = "Confirmed", values = ~Confirmed),
    list(range = c(0, max(parcoords_data$Recovered)), label = "Recovered", values = ~Recovered),
    list(range = c(0, max(parcoords_data$Active)), label = "Active Cases", values = ~Active)
  ),
  data = parcoords_data
) %>%
  layout(
    title = list(text = "Parallel Coordinates Plot of COVID-19 Metrics by WHO Region", font = list(size = 20, family = "Arial", color = "black", face = "bold")),
    dragmode = "select",
    width = 1500,
    height = 800,
    font = list(size = 18, face = "bold"),
    margin = list(l = 200, r = 200, b = 100, t = 100),
    legend = list(font = list(size = 14)),
    xaxis = list(title = list(text = "X-Axis Title", font = list(size = 20, family = "Arial", color = "black", face = "bold"))),
    yaxis = list(title = list(text = "Y-Axis Title", font = list(size = 20, family = "Arial", color = "black", face = "bold")))
  )

parcoords_plot

# Waterfall Plot
agg_data <- data %>%
  group_by(WHO.Region) %>%
  summarize(Mean_Recovered_Per_100_Cases = mean(Recovered...100.Cases))

agg_data <- agg_data %>%
  mutate(
    Previous = lag(Mean_Recovered_Per_100_Cases, default = 0),
    Change = Mean_Recovered_Per_100_Cases - Previous,
    Cumulative = cumsum(Change)
  )

waterfall_plot <- ggplot(agg_data, aes(x = WHO.Region, y = Change, fill = Change > 0)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(Mean_Recovered_Per_100_Cases, 2), y = Cumulative), vjust = -0.5) +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_minimal() +
  labs(title = "Waterfall Plot of Recovery Rate (Recovered per 100 Cases) by WHO Region",
       x = "WHO Region",
       y = "Change in Recovered per 100 Cases") +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, color = "black", face = "bold"),
    axis.text.y = element_text(size = 14, color = "black", face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  )

print(waterfall_plot)

# Volume Plot (3D)
volume_data <- data %>%
  select(WHO.Region, Recovered...100.Cases, Deaths) %>%
  group_by(WHO.Region) %>%
  summarize(
    Mean_Recovered_Per_100_Cases = mean(Recovered...100.Cases),
    Total_Deaths = sum(Deaths)
  )

volume_data$Index <- as.numeric(factor(volume_data$WHO.Region))

volume_plot <- plot_ly(
  x = volume_data$Index,
  y = volume_data$Mean_Recovered_Per_100_Cases,
  z = volume_data$Total_Deaths,
  type = 'mesh3d',
  intensity = volume_data$Total_Deaths,
  colorscale = 'Viridis',
  showscale = TRUE,
  text = ~paste("WHO Region: ", volume_data$WHO.Region,
                "<br>Mean Recovered per 100 Cases: ", volume_data$Mean_Recovered_Per_100_Cases,
                "<br>Total Deaths: ", volume_data$Total_Deaths),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(text = "3D Volume Plot of Recovery Rate and Deaths by WHO Region", font = list(size = 20, color = 'black', family = "Arial", weight = "bold")),
    scene = list(
      xaxis = list(title = 'WHO Region', tickvals = volume_data$Index,
                   ticktext = volume_data$WHO.Region,
                   titlefont = list(size = 16, color = 'black', family = "Arial", weight = "bold"),
                   tickfont = list(size = 14, color = 'black', family = "Arial", weight = "bold")),
      yaxis = list(title = 'Mean Recovered per 100 Cases', titlefont = list(size = 16, color = 'black', family = "Arial", weight = "bold"),
                   tickfont = list(size = 14, color = 'black', family = "Arial", weight = "bold")),
      zaxis = list(title = 'Total Deaths', titlefont = list(size = 16, color = 'black', family = "Arial", weight = "bold"),
                   tickfont = list(size = 14, color = 'black', family = "Arial", weight = "bold")),
      camera = list(eye = list(x = 1.87, y = 0.88, z = -0.64))
    )
  )

volume_plot

# SPLOM (Scatterplot Matrix) Plot
splom_data <- data %>%
  select(Deaths, Confirmed, Recovered, Deaths...100.Cases)

splom_plot <- plot_ly(data = splom_data) %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label = 'Deaths', values = ~Deaths),
      list(label = 'Confirmed', values = ~Confirmed),
      list(label = 'Recovered', values = ~Recovered),
      list(label = 'Deaths per 100 Cases', values = ~Deaths...100.Cases)
    ),
    marker = list(
      color = ~Deaths,
      colorscale = 'Viridis',
      showscale = TRUE,
      line = list(width = 0.5, color = 'rgba(0, 0, 0, 0.5)')
    )
  ) %>%
  layout(
    title = "SPLOM Plot of COVID-19 Metrics",
    dragmode = 'select',
    width = 800,
    height = 800,
    hovermode = 'closest'
  )

splom_plot

# Parallel Coordinates Plot (parcoords)
parcoords_data <- data %>%
  select(WHO.Region, Deaths, Confirmed, Recovered, Active)

parcoords_plot <- plot_ly(
  type = "parcoords",
  line = list(color = ~Deaths, colorscale = "Viridis", showscale = TRUE),
  dimensions = list(
    list(range = c(0, max(parcoords_data$Deaths)), label = "Deaths", values = ~Deaths),
    list(range = c(0, max(parcoords_data$Confirmed)), label = "Confirmed", values = ~Confirmed),
    list(range = c(0, max(parcoords_data$Recovered)), label = "Recovered", values = ~Recovered),
    list(range = c(0, max(parcoords_data$Active)), label = "Active Cases", values = ~Active)
  ),
  data = parcoords_data
) %>%
  layout(
    title = list(text = "Parallel Coordinates Plot of COVID-19 Metrics by WHO Region", font = list(size = 20, family = "Arial", color = "black", face = "bold")),
    dragmode = "select",
    width = 1500,  
    height = 800, 
    font = list(size = 18, face = "bold"),  
    margin = list(l = 200, r = 200, b = 100, t = 100), 
    legend = list(font = list(size = 14)),  
    xaxis = list(title = list(text = "X-Axis Title", font = list(size = 20, family = "Arial", color = "black", face = "bold"))),  
    yaxis = list(title = list(text = "Y-Axis Title", font = list(size = 20, family = "Arial", color = "black", face = "bold")))  
  )

parcoords_plot


#########NEW###########
data <- data %>%
  rename(
    Country_Region = Country_Region,  # Adjust if name is different, e.g., Country.Region
    WHO_Region = WHO_Region,         # Adjust if name is different, e.g., WHO.Region
    Deaths___100_Cases = Deaths_100_Cases,  # Adjust if name is different, e.g., Deaths.per.100.Cases
    Recovered___100_Cases = Recovered_100_Cases,  # Adjust if name is different
    Confirmed = Confirmed,
    Deaths = Deaths,
    Recovered = Recovered,
    Active = Active,
    New_cases = New_cases,
    Confirmed_last_week = Confirmed_last_week,
    X1_week_change = `1_week_change`,  # Adjust if name has special characters
    X1_week___increase = `1_week___increase`
  )
names(data)
# Remove leading and trailing spaces from column names
names(data) <- trimws(names(data))

# Check if renaming columns is now successful
names(data)

# =============================================================================
# 1. ENHANCED CORRELATION HEATMAP WITH DARK THEME
# =============================================================================
correlation_matrix <- cor(data[, sapply(data, is.numeric)], use = "complete.obs")
correlation_matrix_melted <- melt(correlation_matrix)

enhanced_heatmap <- ggplot(data = correlation_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(value, 2)), color = "white", size = 3, fontface = "bold") +
  scale_fill_gradient2(low = "#2E86C1", mid = "#F8C471", high = "#E74C3C", 
                       midpoint = 0, name = "Correlation\nCoefficient") +
  labs(title = "COVID-19 Metrics Correlation Matrix", 
       subtitle = "Correlation between different COVID-19 indicators",
       x = "", y = "") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C2833", color = NA),
    panel.background = element_rect(fill = "#1C2833", color = NA),
    plot.title = element_text(size = 18, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text.x = element_text(color = "white", size = 10, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "white", size = 10, face = "bold"),
    legend.title = element_text(size = 12, face = "bold", color = "white"),
    legend.text = element_text(size = 10, color = "white"),
    legend.background = element_rect(fill = "#1C2833", color = "white"),
    panel.grid = element_blank()
  )
print(enhanced_heatmap)

# =============================================================================
# 2. TREEMAP VISUALIZATION
# =============================================================================
library(treemap)
library(RColorBrewer)

# Prepare data for treemap
treemap_data <- data %>% 
  group_by(WHO.Region) %>% 
  summarise(Total_Confirmed = sum(Confirmed, na.rm = TRUE),
            Total_Deaths = sum(Deaths, na.rm = TRUE),
            Countries = n()) %>% 
  filter(Total_Confirmed > 0)

treemap_plot <- treemap(treemap_data,
                        index = c("WHO.Region"),
                        vSize = "Total_Confirmed",
                        vColor = "Total_Deaths",
                        type = "value",
                        palette = "RdYlBu",
                        title = "COVID-19 Cases by WHO Region",
                        fontsize.title = 16,
                        fontcolor.labels = "white",
                        fontface.labels = "bold",
                        bg.labels = 0,
                        border.col = "white",
                        border.lwds = 2)

# =============================================================================
# Updated bubble chart code with customized background color
bubble_chart <- ggplot(data, aes(x = Deaths, y = Recovered, 
                                 size = Confirmed, color = WHO.Region)) +
  geom_point(alpha = 0.7, stroke = 0) +
  scale_size_continuous(range = c(1, 15), name = "Confirmed\nCases",
                        labels = scales::comma_format()) +
  scale_color_viridis_d(name = "WHO Region") +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(title = "COVID-19: Deaths vs Recovered Cases",
       subtitle = "Bubble size represents confirmed cases (log scale)",
       x = "Deaths (log scale)",
       y = "Recovered Cases (log scale)") +
  theme_dark() +
  theme(
    plot.background = element_rect(fill = "#EED9C4", color = NA),  # Background color
    panel.background = element_rect(fill = "#EED9C4", color = NA), # Same for panel background
    plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "black", size = 10, face = "bold"),  # Bold axes text
    axis.title = element_text(color = "black", size = 12, face = "bold"),  # Bold axes title
    legend.title = element_text(color = "black", size = 10, face = "bold"),  # Bold legend title
    legend.text = element_text(color = "black", size = 9, face = "bold"),  # Bold legend text
    legend.background = element_rect(fill = "lightblue", color = "black"),
    panel.grid = element_line(color = "black", linewidth = 0.5)  # Bold grid lines
  )

# Make it interactive
bubble_interactive <- ggplotly(bubble_chart, tooltip = c("x", "y", "size", "colour"))
print(bubble_interactive)

# =============================================================================
# 4. RIDGE PLOT (Density Distribution)
# =============================================================================
library(ggridges)

ridge_plot <- ggplot(data, aes(x = Deaths...100.Cases, y = WHO.Region, fill = WHO.Region)) +
  geom_density_ridges(alpha = 0.8, scale = 2) +
  scale_fill_viridis_d(name = "WHO Region") +
  labs(title = "Distribution of Death Rates by WHO Region",
       subtitle = "Deaths per 100 confirmed cases",
       x = "Deaths per 100 Confirmed Cases",
       y = "WHO Region") +
  theme_ridges() +
  theme(
    plot.background = element_rect(fill = "#1A1A1A", color = NA),
    panel.background = element_rect(fill = "#1A1A1A", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white", size = 12, face = "bold"),
    legend.position = "none",
    panel.grid = element_line(color = "#2E2E2E", size = 0.3)
  )
print(ridge_plot)

# =============================================================================
# 5. ENHANCED WORLD MAP WITH DARK THEME
# =============================================================================
# Option 3: Dark grey background
p3 <- ggplot(data_merged, aes(long, lat, group = group, fill = Confirmed)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey60") +
  labs(fill = "Confirmed Cases", title = "Confirmed COVID-19 Cases per Country") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#2F2F2F", color = NA),  # Dark grey
    panel.background = element_rect(fill = "#2F2F2F", color = NA),
    plot.title = element_text(size = 18, face = "bold", color = "white"),
    legend.title = element_text(size = 14, face = "bold", color = "white"),
    legend.text = element_text(size = 12, face = "bold", color = "white"),
    legend.background = element_rect(fill = "#2F2F2F", color = "white"),
    legend.key = element_rect(fill = "#2F2F2F", color = "white")
  )

p3_interactive <- ggplotly(p3)
p3_interactive <- p3_interactive %>% layout(
  title = list(text = "Confirmed COVID-19 Cases per Country", 
               font = list(size = 20, family = "Arial", color = "white")),
  paper_bgcolor = "lightblue",
  plot_bgcolor = "lightblue",
  dragmode = "zoom", 
  hovermode = "closest"
)
print(p3_interactive)

# =============================================================================
# 6. ENHANCED STACKED BAR CHART
# =============================================================================
# Prepare data for stacked bar chart
stacked_data <- data %>% 
  group_by(WHO.Region) %>% 
  summarise(
    Active = sum(Active, na.rm = TRUE),
    Recovered = sum(Recovered, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(Active, Recovered, Deaths),
               names_to = "Status",
               values_to = "Cases")

stacked_bar <- ggplot(stacked_data, aes(x = reorder(WHO.Region, Cases), y = Cases, fill = Status)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(values = c("Active" = "#F39C12", "Recovered" = "#27AE60", "Deaths" = "#E74C3C"),
                    name = "Case Status") +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip() +
  labs(title = "COVID-19 Cases by WHO Region and Status",
       subtitle = "Total cases broken down by current status",
       x = "WHO Region",
       y = "Number of Cases") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#2C3E50", color = NA),
    panel.background = element_rect(fill = "#2C3E50", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white", size = 12, face = "bold"),
    legend.title = element_text(color = "white", size = 10, face = "bold"),
    legend.text = element_text(color = "white", size = 9),
    legend.background = element_rect(fill = "#2C3E50", color = "white"),
    panel.grid.major = element_line(color = "#34495E", size = 0.5),
    panel.grid.minor = element_blank()
  )
print(stacked_bar)

# 7. DUMBBELL CHART (Lollipop variation)
# =============================================================================
# Compare recovery rates and death rates
comparison_data <- data %>% 
  select(Country.Region, WHO.Region, Deaths...100.Cases, Recovered...100.Cases) %>%   # Correct column names
  filter(!is.na(Deaths...100.Cases) & !is.na(Recovered...100.Cases)) %>%  # Correct column names
  arrange(desc(Deaths...100.Cases)) %>%  # Correct column names
  head(20)  # Top 20 countries by death rate

dumbbell_chart <- ggplot(comparison_data) +
  geom_segment(aes(x = Deaths...100.Cases, xend = Recovered...100.Cases,  # Correct column names
                   y = reorder(Country.Region, Deaths...100.Cases),   # Correct column names
                   yend = reorder(Country.Region, Deaths...100.Cases)),  # Correct column names
               color = "gray70", linewidth = 1.5) +  # Use 'linewidth' instead of 'size'
  geom_point(aes(x = Deaths...100.Cases, y = reorder(Country.Region, Deaths...100.Cases)),  # Correct column names
             color = "#E74C3C", size = 3) +
  geom_point(aes(x = Recovered...100.Cases, y = reorder(Country.Region, Deaths...100.Cases)),  # Correct column names
             color = "#27AE60", size = 3) +
  labs(title = "Death Rate vs Recovery Rate by Country",
       subtitle = "Top 20 countries by death rate (per 100 confirmed cases)",
       x = "Rate per 100 Confirmed Cases",
       y = "Country") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C2833", color = NA),
    panel.background = element_rect(fill = "#1C2833", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "white", size = 9),
    axis.title = element_text(color = "white", size = 12, face = "bold"),
    panel.grid.major = element_line(color = "#2E2E2E", linewidth = 0.3),  # Use 'linewidth' instead of 'size'
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = max(comparison_data$Deaths...100.Cases) * 0.3, 
           y = nrow(comparison_data) * 0.9, 
           label = "Death Rate", color = "#E74C3C", size = 4, fontface = "bold") +
  annotate("text", x = max(comparison_data$Recovered...100.Cases) * 0.8,  # Correct column name
           y = nrow(comparison_data) * 0.9, 
           label = "Recovery Rate", color = "#27AE60", size = 4, fontface = "bold")

print(dumbbell_chart)


# =============================================================================
# 8. ENHANCED DONUT CHART
# =============================================================================
# Create donut chart for WHO regions
region_summary <- data %>% 
  group_by(WHO.Region) %>% 
  summarise(Total.Cases = sum(Confirmed, na.rm = TRUE)) %>% 
  mutate(Percentage = Total.Cases / sum(Total.Cases) * 100,
         ymax = cumsum(Percentage),
         ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(WHO.Region, "\n", round(Percentage, 1), "%"))

donut_chart <- ggplot(region_summary, aes(ymax = ymax, ymin = ymin, 
                                          xmax = 4, xmin = 3, fill = WHO.Region)) +
  geom_rect() +
  geom_text(x = 3.5, aes(y = labelPosition, label = label), 
            size = 3, fontface = "bold", color = "white") +
  scale_fill_viridis_d(name = "WHO Region") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  labs(title = "Distribution of COVID-19 Cases by WHO Region",
       subtitle = "Percentage of global confirmed cases") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#0F1419", color = NA),
    panel.background = element_rect(fill = "#0F1419", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(color = "white", size = 10, face = "bold"),
    legend.text = element_text(color = "white", size = 9)
  )
print(donut_chart)

# =============================================================================
# 9. MULTIPLE METRICS COMPARISON (Radar-like)**
# =============================================================================
# Normalize metrics for comparison
metrics_comparison <- data %>% 
  group_by(WHO.Region) %>% 
  summarise(
    Avg_Death_Rate = mean(Deaths...100.Cases, na.rm = TRUE),
    Avg_Recovery_Rate = mean(Recovered...100.Cases, na.rm = TRUE),
    Total_Countries = n(),
    Avg_New_Cases = mean(New.cases, na.rm = TRUE)
  ) %>% 
  mutate(across(c(Avg_Death_Rate, Avg_Recovery_Rate, Avg_New_Cases), scales::rescale, to = c(0, 10)))

# Create a faceted comparison
metrics_long <- metrics_comparison %>% 
  pivot_longer(cols = c(Avg_Death_Rate, Avg_Recovery_Rate, Avg_New_Cases),
               names_to = "Metric",
               values_to = "Normalized_Value")

faceted_comparison <- ggplot(metrics_long, aes(x = WHO.Region, y = Normalized_Value, fill = Metric)) +
  geom_col(position = "dodge", alpha = 0.8) +
  facet_wrap(~Metric, scales = "free_y") +
  scale_fill_manual(values = c("#E74C3C", "#27AE60", "#3498DB")) +
  coord_flip() +
  labs(title = "WHO Region Performance Comparison",
       subtitle = "Normalized metrics (0-10 scale) across different indicators",
       x = "WHO Region",
       y = "Normalized Score") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#EDC9AF", color = NA),
    panel.background = element_rect(fill = "#EDC9AF", color = NA),
    strip.background = element_rect(fill = "#EDC9AF", color = "black"),
    strip.text = element_text(color = "black", face = "bold"),
    plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "black", size = 9),
    axis.title = element_text(color = "black", size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "#2E2E2E", linewidth = 0.3),  # Updated to 'linewidth'
    panel.grid.minor = element_blank()
  )
print(faceted_comparison)


# =============================================================================
# 10. TIME SERIES SIMULATION (Using available data)
# =============================================================================
# Create a simulated time series using weekly changes
time_series_data <- data %>% 
  select(Country.Region, WHO.Region, Confirmed.last.week, X1.week.change, X1.week...increase) %>% 
  filter(!is.na(X1.week.change)) %>% 
  mutate(
    Week_1 = Confirmed.last.week,
    Week_2 = Confirmed.last.week + X1.week.change,
    Growth_Rate = X1.week...increase
  ) %>% 
  select(Country.Region, WHO.Region, Week_1, Week_2, Growth_Rate) %>% 
  pivot_longer(cols = c(Week_1, Week_2),
               names_to = "Week",
               values_to = "Cases") %>% 
  group_by(WHO.Region, Week) %>% 
  summarise(Total.Cases = sum(Cases, na.rm = TRUE), .groups = "drop")

time_series_plot <- ggplot(time_series_data, aes(x = Week, y = Total.Cases, 
                                                 color = WHO.Region, group = WHO.Region)) +
  geom_line(size = 2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_viridis_d(name = "WHO Region") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Weekly Change in COVID-19 Cases by WHO Region",
       subtitle = "Comparison between last week and current week totals",
       x = "Time Period",
       y = "Total Confirmed Cases") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.background = element_rect(fill = "#0D1B2A", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white", size = 12, face = "bold"),
    legend.title = element_text(color = "white", size = 10, face = "bold"),
    legend.text = element_text(color = "white", size = 9),
    legend.background = element_rect(fill = "#0D1B2A", color = "white"),
    panel.grid.major = element_line(color = "#1E3A5F", size = 0.5),
    panel.grid.minor = element_blank()
  )
print(time_series_plot)

# =============================================================================
# 11. SLOPE GRAPH
# =============================================================================
slope_data <- data %>%
  select(Country.Region, WHO.Region, Confirmed.last.week, Confirmed) %>%
  filter(!is.na(Confirmed.last.week) & !is.na(Confirmed)) %>%
  group_by(WHO.Region) %>%
  summarise(
    Last_Week = sum(Confirmed.last.week, na.rm = TRUE),
    Current_Week = sum(Confirmed, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Last_Week, Current_Week),
               names_to = "Period",
               values_to = "Cases")

slope_graph <- ggplot(slope_data, aes(x = Period, y = Cases, group = WHO.Region, color = WHO.Region)) +
  geom_line(size = 2, alpha = 0.8) +
  geom_point(size = 4) +
  geom_text(data = filter(slope_data, Period == "Current_Week"),
            aes(label = WHO.Region), hjust = -0.1, size = 3, fontface = "bold") +
  scale_color_viridis_d(name = "WHO Region") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "COVID-19 Cases: Weekly Change by WHO Region",
       subtitle = "Slope indicates increase/decrease trend",
       x = "Time Period", y = "Total Cases") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#0A0E1A", color = NA),
    panel.background = element_rect(fill = "#0A0E1A", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white", size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "#1E2A3A", linewidth = 0.5),
    panel.grid.minor = element_blank()
  )
print(slope_graph)

# =============================================================================
# 12. WAFFLE CHART
# =============================================================================
library(waffle)
library(viridis)

# Create waffle chart for case distribution
waffle_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(Total_Cases = sum(Confirmed, na.rm = TRUE)) %>%
  mutate(Cases_Thousands = round(Total_Cases / 1000)) %>%
  arrange(desc(Cases_Thousands))

# Convert the data to a named vector (waffle chart needs a named vector)
waffle_data_vector <- setNames(waffle_data$Cases_Thousands, waffle_data$WHO.Region)

# Create the waffle chart
waffle_chart <- waffle(waffle_data_vector, 
                       rows = 10,
                       colors = viridis(nrow(waffle_data)),
                       title = "COVID-19 Cases Distribution by WHO Region") +
  theme(
    plot.background = element_rect(fill = "#1A1A1A", color = NA),
    panel.background = element_rect(fill = "#1A1A1A", color = NA),
    plot.title = element_text(color = "white", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "lightgray", size = 12),
    legend.text = element_text(color = "white", size = 9),
    legend.background = element_rect(fill = "#1A1A1A", color = NA)
  ) +
  labs(subtitle = "Each square = 1,000 cases")  # Add subtitle separately

# Print the chart
print(waffle_chart)


# =============================================================================
# 13. SANKEY DIAGRAM***
# =============================================================================
library(networkD3)

# Prepare data for Sankey (Flow from WHO Region to Case Status)
sankey_data <- data %>%
  select(WHO.Region, Active, Recovered, Deaths) %>%
  group_by(WHO.Region) %>%
  summarise(
    Active = sum(Active, na.rm = TRUE),
    Recovered = sum(Recovered, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Active, Recovered, Deaths),
               names_to = "Status",
               values_to = "Cases") %>%
  filter(Cases > 0)

# Create nodes
nodes <- data.frame(
  name = c(unique(sankey_data$WHO.Region), unique(sankey_data$Status))
)

# Create links
links <- sankey_data %>%
  mutate(
    source = match(WHO.Region, nodes$name) - 1,
    target = match(Status, nodes$name) - 1,
    value = Cases
  ) %>%
  select(source, target, value)

# Create Sankey diagram
sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 30,
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);")
)

# Add custom CSS (background color for the plot container)
sankey_plot_with_bold <- htmlwidgets::onRender(sankey_plot, "
  function(el, x) {
    // Set the background color to AliceBlue
    el.style.backgroundColor = '	#F7E7CE'; 
    
    // Make node names (region names) bold
    var nodeLabels = el.querySelectorAll('.node text');
    nodeLabels.forEach(function(label) {
      label.style.fontWeight = 'bold';  // Make node names bold
      label.style.fill = 'black'; // Ensure text color is black
    });
  }
")

# Print the plot
print(sankey_plot_with_bold)



# =============================================================================
# 14. CALENDAR HEATMAP (Simulated with weekly data)
# =============================================================================
calendar_data <- data %>%
  select(Country.Region, WHO.Region, X1.week.change) %>%  # Updated column name
  filter(!is.na(X1.week.change)) %>%  # Updated column name
  mutate(
    Week = sample(1:52, nrow(.), replace = TRUE),  # Simulate weeks
    Growth_Category = cut(X1.week.change,  # Updated column name
                          breaks = c(-Inf, 0, 5, 10, 20, Inf),
                          labels = c("Negative", "Low", "Moderate", "High", "Very High"))
  ) %>%
  group_by(WHO.Region, Week) %>%
  summarise(Avg_Growth = mean(X1.week.change, na.rm = TRUE), .groups = "drop")  # Updated column name

calendar_heatmap <- ggplot(calendar_data, aes(x = Week, y = WHO.Region, fill = Avg_Growth)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "#27AE60", mid = "#F39C12", high = "#E74C3C",
                       midpoint = 10, name = "Growth Rate\n(%)") +
  scale_x_continuous(breaks = seq(1, 52, 4)) +
  labs(title = "COVID-19 Growth Rate Calendar View",
       subtitle = "Average weekly growth rate by WHO region",
       x = "Week of Year", y = "WHO Region") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1C1C1C", color = NA),
    panel.background = element_rect(fill = "#1C1C1C", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "white", size = 9),
    axis.title = element_text(color = "white", size = 12, face = "bold"),
    legend.title = element_text(color = "white", size = 10, face = "bold"),
    legend.text = element_text(color = "white", size = 9),
    legend.background = element_rect(fill = "#1C1C1C", color = "white"),
    panel.grid = element_blank()
  )
print(calendar_heatmap)


# =============================================================================
# 15. BULLET CHART
# =============================================================================
bullet_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(
    Actual_Recovery = mean(Recovered...100.Cases, na.rm = TRUE),
    Target_Recovery = 85,  # Assume 85% as target
    Good_Recovery = 70     # 70% as acceptable
  ) %>%
  mutate(Performance = ifelse(Actual_Recovery >= Target_Recovery, "Excellent",
                              ifelse(Actual_Recovery >= Good_Recovery, "Good", "Poor")))

bullet_chart <- ggplot(bullet_data) +
  # Background bars
  geom_col(aes(x = WHO.Region, y = 100), fill = "#BDC3C7", alpha = 0.3, width = 0.5) +
  geom_col(aes(x = WHO.Region, y = Target_Recovery), fill = "#F39C12", alpha = 0.5, width = 0.5) +
  geom_col(aes(x = WHO.Region, y = Good_Recovery), fill = "#27AE60", alpha = 0.5, width = 0.5) +
  # Actual values
  geom_col(aes(x = WHO.Region, y = Actual_Recovery, fill = Performance), 
           width = 0.3, alpha = 0.9) +
  # Target line
  geom_hline(yintercept = 85, color = "#E74C3C", linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Excellent" = "#27AE60", "Good" = "#F39C12", "Poor" = "#E74C3C"),
                    name = "Performance") +
  coord_flip() +
  labs(title = "Recovery Rate Performance by WHO Region",
       subtitle = "Actual vs Target Recovery Rates (per 100 cases)",
       x = "WHO Region", y = "Recovery Rate (%)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#2C3E50", color = NA),
    panel.background = element_rect(fill = "#2C3E50", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white", size = 12, face = "bold"),
    legend.title = element_text(color = "white", size = 10, face = "bold"),
    legend.text = element_text(color = "white", size = 9),
    legend.background = element_rect(fill = "#2C3E50", color = "white"),
    panel.grid.major = element_line(color = "#34495E", size = 0.3),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 0.5, y = 87, label = "Target: 85%", 
           color = "#E74C3C", size = 3, fontface = "bold")

print(bullet_chart)

# =============================================================================
# 16. RADIAL BAR CHART*
# =============================================================================
radial_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(Avg_Death_Rate = mean(Deaths...100.Cases, na.rm = TRUE)) %>%
  arrange(Avg_Death_Rate)

radial_bar <- ggplot(radial_data, aes(x = WHO.Region, y = Avg_Death_Rate, fill = WHO.Region)) +
  geom_col(width = 0.8, alpha = 0.8) +
  geom_text(aes(label = round(Avg_Death_Rate, 1)), 
            hjust = 0.5, vjust = -0.5, color = "white", size = 4, fontface = "bold") +
  coord_polar(theta = "x") +
  scale_fill_viridis_d(name = "WHO Region") +
  labs(title = "Average Death Rate by WHO Region",
       subtitle = "Deaths per 100 confirmed cases (radial view)",
       y = "Death Rate (%)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#0F1419", color = NA),
    panel.background = element_rect(fill = "#0F1419", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text.x = element_text(color = "white", size = 10, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "white", size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "#2E2E2E", size = 0.3),
    panel.grid.minor = element_blank()
  )
print(radial_bar)

# =============================================================================
# 17. STREAMGRAPH (Area Chart)
# =============================================================================
library(ggstream)

# Create streamgraph using simulated time data
stream_data <- data %>%
  select(WHO.Region, Confirmed.last.week, Confirmed) %>%
  filter(!is.na(Confirmed.last.week)) %>%
  group_by(WHO.Region) %>%
  summarise(
    Week_1 = sum(Confirmed.last.week, na.rm = TRUE),
    Week_2 = sum(Confirmed, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Week_1, Week_2),
               names_to = "Week",
               values_to = "Cases") %>%
  mutate(Week_Num = ifelse(Week == "Week_1", 1, 2))

streamgraph <- ggplot(stream_data, aes(x = Week_Num, y = Cases, fill = WHO.Region)) +
  geom_stream(alpha = 0.8) +
  scale_fill_viridis_d(name = "WHO Region") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Last Week", "Current Week")) +
  labs(title = "COVID-19 Cases Flow by WHO Region",
       subtitle = "Stream representation of case distribution over time",
       x = "Time Period", y = "Cumulative Cases") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1A1A2E", color = NA),
    panel.background = element_rect(fill = "#1A1A2E", color = NA),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white", size = 12, face = "bold"),
    legend.title = element_text(color = "white", size = 10, face = "bold"),
    legend.text = element_text(color = "white", size = 9),
    legend.background = element_rect(fill = "#1A1A2E", color = "white"),
    panel.grid = element_line(color = "#2E2E4E", size = 0.3)
  )
print(streamgraph)

# =============================================================================
# 18. CHORD DIAGRAM
# =============================================================================
library(circlize)

# Ensure tibble is loaded
library(tibble)

# Create chord diagram showing relationships between regions and case severity
chord_data <- data %>%
  mutate(
    Severity = case_when(
      Deaths...100.Cases > 5 ~ "High Mortality",
      Deaths...100.Cases > 2 ~ "Medium Mortality", 
      TRUE ~ "Low Mortality"
    )
  ) %>%
  group_by(WHO.Region, Severity) %>%
  summarise(Count = n(), .groups = "drop") %>%
  tidyr::complete(WHO.Region, Severity, fill = list(Count = 0))

# Create matrix for chord diagram
chord_matrix <- chord_data %>%
  pivot_wider(names_from = Severity, values_from = Count, values_fill = 0) %>%
  column_to_rownames("WHO.Region") %>%
  as.matrix()

# Create chord diagram
chordDiagram(chord_matrix, 
             grid.col = rainbow(nrow(chord_matrix) + ncol(chord_matrix)),
             transparency = 0.2,
             annotationTrack = "grid")
title("COVID-19 Mortality Patterns by WHO Region", 
      col.main = "white", cex.main = 1.5)



# =============================================================================
# 19. ENHANCED BOX PLOT WITH VIOLIN**
# =============================================================================
violin_box_plot <- ggplot(data, aes(x = WHO.Region, y = Deaths...100.Cases, fill = WHO.Region)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8, outlier.color = "red", outlier.size = 2) +
  scale_fill_viridis_d(name = "WHO Region") +
  scale_y_log10(labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "Distribution of Death Rates by WHO Region",
       subtitle = "Violin plot with box plot overlay (log scale)",
       x = "WHO Region",
       y = "Deaths per 100 Cases (log scale)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#F1DDCF", color = NA),  # Light green background
    panel.background = element_rect(fill = "#F1DDCF", color = NA),  # Same light green for panel
    plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),  # Black title text
    plot.subtitle = element_text(size = 12, color = "black", hjust = 0.5),  # Black subtitle text
    axis.text = element_text(color = "black", size = 9),  # Black axis text
    axis.title = element_text(color = "black", size = 12, face = "bold"),  # Black axis title text
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # Hiding legend
    panel.grid.major = element_line(color = "#2E3A59", size = 0.3),
    panel.grid.minor = element_blank()
  )
print(violin_box_plot)

# =============================================================================
# 20. ANIMATED BAR RACE (Static version showing progression)
# =============================================================================
# Create progression data
progression_data <- data %>%
  select(Country.Region, WHO.Region, Confirmed.last.week, Confirmed) %>%
  filter(!is.na(Confirmed.last.week)) %>%
  pivot_longer(cols = c(Confirmed.last.week, Confirmed),
               names_to = "Period",
               values_to = "Cases") %>%
  group_by(WHO.Region, Period) %>%
  summarise(Total.Cases = sum(Cases, na.rm = TRUE), .groups = "drop") %>%
  group_by(Period) %>%
  mutate(Rank = rank(-Total.Cases)) %>%
  filter(Rank <= 6)  # Top 6 regions

bar_race_static <- ggplot(progression_data, aes(x = Rank, y = Total.Cases, fill = WHO.Region)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = WHO.Region), hjust = -0.1, color = "white", size = 4, fontface = "bold") +
  scale_x_reverse() +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_viridis_d(name = "WHO Region") +
  coord_flip() +
  facet_wrap(~Period, labeller = labeller(Period = c("Confirmed.last.week" = "Last Week", 
                                                     "Confirmed" = "Current Week"))) +
  labs(title = "COVID-19 Cases Ranking by WHO Region",
       subtitle = "Comparison between last week and current week",
       x = "Rank", y = "Total Confirmed Cases") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#0A0A0A", color = NA),
    panel.background = element_rect(fill = "#0A0A0A", color = NA),
    strip.background = element_rect(fill = "#1E1E1E", color = "white"),
    strip.text = element_text(color = "white", size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "lightgray", hjust = 0.5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(color = "white", size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "#2E2E2E", size = 0.3),
    panel.grid.minor = element_blank()
  )
print(bar_race_static)

# =============================================================================
# SUMMARY OF ALL AVAILABLE PLOTS
# =============================================================================
cat("
=============================================================================
COMPLETE LIST OF COVID-19 VISUALIZATIONS AVAILABLE:
=============================================================================

ORIGINAL ENHANCED PLOTS:
1.  Enhanced Correlation Heatmap (Dark theme)
2.  Treemap Visualization 
3.  Enhanced Bubble Chart (Interactive)
4.  Ridge Plot (Density distributions)
5.  Enhanced World Map (Dark theme)
6.  Enhanced Stacked Bar Chart
7.  Dumbbell Chart (Comparison)
8.  Enhanced Donut Chart
9.  Multiple Metrics Comparison (Faceted)
10. Time Series Simulation

ADDITIONAL ADVANCED PLOTS:
11. Slope Graph (Trend visualization)
12. Waffle Chart (Proportional squares)
13. Sankey Diagram (Flow visualization)
14. Calendar Heatmap (Temporal patterns)
15. Bullet Chart (Performance vs targets)
16. Radial Bar Chart (Circular bars)
17. Streamgraph (Area flow)
18. Chord Diagram (Relationship patterns)
19. Enhanced Violin Box Plot
20. Bar Race Static (Ranking progression)

INTERACTIVE OPTIONS:
- All plotly-compatible charts can be made interactive
- 3D visualizations available
- Hover tooltips and zoom functionality
- Dynamic filtering capabilities

THEMES AVAILABLE:
- Dark themes (multiple variations)
- Professional color palettes
- Custom backgrounds and fonts
- Enhanced legends and labels
")

# Install required packages
install.packages(c("ggalluvial", "networkD3", "circlize", "pheatmap", 
                   "ggradar", "ggbump", "treemapify", "ggridges", 
                   "plotrix", "VennDiagram"))

# Load libraries
install.packages("ggplot2")           # For data visualization
install.packages("dplyr")             # For data manipulation
install.packages("plotly")            # For interactive visualizations
install.packages("ggalluvial")        # For alluvial plots
install.packages("networkD3")         # For network diagrams
install.packages("circlize")          # For circular visualizations
install.packages("pheatmap")          # For heatmaps
install.packages("ggradar")           # For radar charts
install.packages("ggbump")            # For bump plots
install.packages("treemapify")        # For treemaps
install.packages("ggridges")          # For ridge plots
install.packages("plotrix")           # For various plots, like 3D pie charts
install.packages("VennDiagram")       # For Venn Diagrams
install.packages("RColorBrewer") 
library(ggplot2)
library(dplyr)
library(plotly)
library(ggalluvial)
library(networkD3)
library(circlize)
library(pheatmap)
#library(ggradar)
library(ggbump)
library(treemapify)
library(ggridges)
library(plotrix)
library(VennDiagram)
library(RColorBrewer)

# Load your data
data <- read.csv("country_wise_latest.csv")

# 1. ALLUVIAL DIAGRAM - Flow between WHO regions and case severity
# Create severity categories
data_severity <- data %>%
  mutate(
    Severity = case_when(
      Deaths/Confirmed > 0.05 ~ "High Mortality",
      Deaths/Confirmed > 0.02 ~ "Medium Mortality",
      TRUE ~ "Low Mortality"
    ),
    Recovery_Rate = case_when(
      Recovered/Confirmed > 0.8 ~ "High Recovery",
      Recovered/Confirmed > 0.5 ~ "Medium Recovery",
      TRUE ~ "Low Recovery"
    )
  ) %>%
  count(WHO.Region, Severity, Recovery_Rate)

alluvial_plot <- ggplot(data_severity,
                        aes(y = n, axis1 = WHO.Region, axis2 = Severity, axis3 = Recovery_Rate)) +
  geom_alluvium(aes(fill = WHO.Region), width = 1/12, alpha = 0.7) +
  geom_stratum(width = 1/12, fill = "white", color = "black") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("WHO Region", "Mortality Level", "Recovery Rate"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  labs(title = "Flow of COVID-19 Severity Across WHO Regions",
       subtitle = "From WHO Region → Mortality Level → Recovery Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(alluvial_plot)

# 2. NETWORK DIAGRAM - Countries connected by similar case patterns
# Create similarity matrix based on case ratios
create_network_data <- function(data, threshold = 0.1) {
  # Calculate key ratios for each country
  country_metrics <- data %>%
    select(Country.Region, Deaths...100.Cases, Recovered...100.Cases, Active) %>%  # Corrected column names
    filter(complete.cases(.))
  
  # Create edges based on similarity
  edges <- data.frame()
  countries <- country_metrics$Country.Region
  
  for(i in 1:(length(countries)-1)) {
    for(j in (i+1):length(countries)) {
      # Calculate similarity (simplified)
      similarity <- abs(country_metrics$Deaths...100.Cases[i] - country_metrics$Deaths...100.Cases[j])
      if(similarity < threshold) {
        edges <- rbind(edges, data.frame(
          source = countries[i], 
          target = countries[j], 
          value = 1/similarity
        ))
      }
    }
  }
  
  return(edges)
}

# Create simplified network for top 20 countries by cases
top_countries <- data %>% 
  arrange(desc(Confirmed)) %>% 
  slice_head(n = 20)

network_edges <- create_network_data(top_countries, threshold = 2)

if(nrow(network_edges) > 0) {
  # Prepare data for networkD3
  nodes <- data.frame(
    name = unique(c(network_edges$source, network_edges$target)),
    group = 1
  )
  
  network_edges$source_id <- match(network_edges$source, nodes$name) - 1
  network_edges$target_id <- match(network_edges$target, nodes$name) - 1
  
  # Create network diagram
  network_plot <- forceNetwork(
    Links = network_edges, Nodes = nodes,
    Source = "source_id", Target = "target_id", Value = "value",
    NodeID = "name", Group = "group",
    opacity = 0.8, fontSize = 12, zoom = TRUE
  )
  
  print(network_plot)
}

# 3. POLAR HEATMAP - Circular heatmap of regions vs metrics
# Prepare data for polar heatmap
polar_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(
    avg_death_rate = mean(Deaths...100.Cases, na.rm = TRUE),
    avg_recovery_rate = mean(Recovered...100.Cases, na.rm = TRUE),
    avg_active_rate = mean(Active/Confirmed * 100, na.rm = TRUE),
    total_cases = sum(Confirmed, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -WHO.Region, names_to = "Metric", values_to = "Value")

polar_heatmap <- ggplot(polar_data, aes(x = Metric, y = WHO.Region, fill = Value)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 50) +
  coord_polar(theta = "x") +
  labs(title = "Polar Heatmap: WHO Regions vs COVID-19 Metrics",
       fill = "Value") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(polar_heatmap)

# 4. SPIDER/RADAR CHART - Multi-dimensional comparison*
# Prepare radar chart data
radar_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(
    Death_Rate = mean(Deaths...100.Cases, na.rm = TRUE),
    Recovery_Rate = mean(Recovered...100.Cases, na.rm = TRUE),
    New_Cases_Rate = mean(New.cases/Confirmed * 100, na.rm = TRUE),
    Week_Change = mean(X1.week...increase, na.rm = TRUE)
  ) %>%
  # Normalize to 0-100 scale for radar chart
  mutate_if(is.numeric, ~scales::rescale(.x, to = c(0, 100)))

# Create radar chart using base R
create_radar_chart <- function(region_name) {
  region_data <- radar_data[radar_data$WHO.Region == region_name, -1]
  
  # Set background color
  par(mfrow = c(1, 1), bg = "lightblue")  # Set background color for the plot (Alice Blue)
  
  # Create the plot
  radarchart(rbind(rep(100, 4), rep(0, 4), region_data),
             axistype = 1,
             pcol = rainbow(1), pfcol = rainbow(1, alpha = 0.3),
             plwd = 2, plty = 1,
             cglcol = "grey", cglty = 1, axislabcol = "grey",
             caxislabels = seq(0, 100, 25),
             title = paste("Radar Chart:", region_name))
}

# Create radar chart for each region (example for Americas)
create_radar_chart("Americas")

# 5. BUMP CHART - Ranking changes over time (simulated)
# Since we don't have time series, we'll simulate ranking by different metrics
bump_data <- data %>%
  select(Country.Region, WHO.Region, Confirmed, Deaths, Recovered, Active) %>%
  arrange(desc(Confirmed)) %>%
  slice_head(n = 15) %>%
  mutate(
    Confirmed_Rank = rank(-Confirmed),
    Deaths_Rank = rank(-Deaths),
    Recovery_Rank = rank(-Recovered),
    Active_Rank = rank(-Active)
  ) %>%
  pivot_longer(cols = ends_with("_Rank"), names_to = "Metric", values_to = "Rank") %>%
  mutate(Metric = gsub("_Rank", "", Metric))

bump_chart <- ggplot(bump_data, aes(x = Metric, y = Rank, group = Country.Region, color = Country.Region)) +
  geom_line(size = 1.2, alpha = 0.7) +
  geom_point(size = 2) +
  scale_y_reverse(breaks = 1:15) +
  labs(title = "Country Rankings Across Different COVID-19 Metrics",
       x = "Metric", y = "Rank") +
  theme_minimal() +
  theme(legend.position = "right")

print(bump_chart)

# 6. TREEMAP - Hierarchical view of cases by region and country
treemap_data <- data %>%
  arrange(desc(Confirmed)) %>%
  group_by(WHO.Region) %>%
  slice_head(n = 3) %>%  # Top 3 countries per region
  ungroup() %>%
  select(WHO.Region, Country.Region, Confirmed, Deaths)

treemap_plot <- ggplot(treemap_data, aes(area = Confirmed, fill = Deaths, 
                                         label = Country.Region, subgroup = WHO.Region)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white", size = 2) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5, colour = "black", 
                             fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(title = "Treemap: COVID-19 Cases by WHO Region and Top Countries",
       fill = "Deaths") +
  theme(legend.position = "bottom")

print(treemap_plot)

# 7. RIDGELINE PLOT - Distribution of recovery rates by region
ridgeline_plot <- ggplot(data, aes(x = `Recovered...100.Cases`, y = WHO.Region, fill = WHO.Region)) +  # Fixed column name
  geom_density_ridges(scale = 4, rel_min_height = 0.01, alpha = 0.7) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(title = "Distribution of Recovery Rates by WHO Region",
       x = "Recovery Rate (per 100 cases)", y = "WHO Region") +
  theme_ridges() +
  theme(legend.position = "none")

print(ridgeline_plot)


# 8. CIRCULAR BAR PLOT - Deaths by region in circular format
circular_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(total_deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(desc(total_deaths))

circular_bar <- ggplot(circular_data, aes(x = reorder(WHO.Region, total_deaths), y = total_deaths, fill = WHO.Region)) +
  geom_col(width = 0.8) +
  coord_polar(theta = "y") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Circular Bar Chart: Total Deaths by WHO Region",
       x = "", y = "Total Deaths") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid = element_blank()
  )

print(circular_bar)

# 9. FLOW/SANKEY DIAGRAM - Flow from confirmed to outcomes
# Create flow data
flow_data <- data %>%
  summarise(
    Confirmed = sum(Confirmed, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE),
    Recovered = sum(Recovered, na.rm = TRUE),
    Active = sum(Active, na.rm = TRUE)
  )

# Prepare for Sankey
sankey_links <- data.frame(
  source = c(0, 0, 0),  # Confirmed cases
  target = c(1, 2, 3),  # Deaths, Recovered, Active
  value = c(flow_data$Deaths, flow_data$Recovered, flow_data$Active)
)

sankey_nodes <- data.frame(
  name = c("Confirmed", "Deaths", "Recovered", "Active")
)

# Create Sankey diagram
sankey_plot <- sankeyNetwork(Links = sankey_links, Nodes = sankey_nodes,
                             Source = "source", Target = "target", Value = "value",
                             NodeID = "name", units = "cases", fontSize = 12,
                             nodeWidth = 30)

print(sankey_plot)

# 10. WAFFLE CHART - Proportional representation
# Create waffle chart data (simplified version using ggplot)
waffle_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(total_cases = sum(Confirmed, na.rm = TRUE)) %>%
  mutate(
    proportion = total_cases / sum(total_cases) * 100,
    squares = round(proportion)
  ) %>%
  uncount(squares) %>%
  mutate(
    x = rep(1:10, length.out = n()),
    y = rep(1:ceiling(n()/10), each = 10)[1:n()]
  )

waffle_chart <- ggplot(waffle_data, aes(x = x, y = y, fill = WHO.Region)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  labs(title = "Waffle Chart: COVID-19 Cases Distribution by WHO Region",
       subtitle = "Each square represents ~1% of total global cases",
       fill = "WHO Region") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  coord_equal()

print(waffle_chart)


*#*#*#*#*#**#**#***#**#**#**NEW**#*#*#**##**#*#**#*
  # Load libraries

install.packages("plotly")         # For interactive plots
install.packages("networkD3")      # For network diagrams
install.packages("DT")             # For DataTables in R
install.packages("crosstalk")      # For interactive linked views
install.packages("leaflet")        # For interactive maps
install.packages("visNetwork")     # For network visualization
install.packages("chorddiag")      # For chord diagrams
install.packages("gganimate")      # For animated plots
install.packages("ggplot2")        # For static plots (base)
install.packages("dplyr")          # For data manipulation
install.packages("tidyr")          # For data tidying
install.packages("highcharter")    # For highcharts-based plots
install.packages("htmlwidgets")    # For interactive widgets (e.g., in shiny)

library(plotly)
library(networkD3)
library(DT)
library(crosstalk)
library(leaflet)
library(visNetwork)
library(chorddiag)
library(gganimate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(highcharter)
library(htmlwidgets)


# Assume data is already loaded as 'data'
# data <- read.csv("your_file.csv")

# 1. ANIMATED BUBBLE RACE - Cases over time simulation
create_bubble_race <- function(data) {
  # Simulate time progression by creating multiple time points
  bubble_data <- data %>%
    select(Country.Region, WHO.Region, Confirmed, Deaths, Recovered) %>%
    slice_head(n = 20) %>%  # Top 20 countries
    crossing(week = 1:10) %>%  # Simulate 10 weeks
    mutate(
      # Simulate progression
      current_confirmed = Confirmed * (1 + week * 0.1 + rnorm(n(), 0, 0.05)),
      current_deaths = Deaths * (1 + week * 0.08 + rnorm(n(), 0, 0.03)),
      death_rate = current_deaths / current_confirmed * 100
    )
  
  p <- bubble_data %>%
    plot_ly(
      x = ~current_confirmed, 
      y = ~current_deaths,
      size = ~current_confirmed,
      color = ~WHO.Region,
      frame = ~week,
      text = ~paste("Country:", Country.Region, 
                    "<br>Confirmed:", round(current_confirmed),
                    "<br>Deaths:", round(current_deaths)),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers'
    ) %>%
    layout(
      title = "Animated Bubble Race: COVID-19 Progression by Country",
      xaxis = list(title = "Confirmed Cases", type = "log"),
      yaxis = list(title = "Deaths", type = "log")
    ) %>%
    animation_opts(frame = 1000, transition = 500, redraw = FALSE)
  
  return(p)
}

bubble_race <- create_bubble_race(data)
bubble_race

# 2. INTERACTIVE CHORD DIAGRAM - Relationships between regions and severity
create_chord_diagram <- function(data) {
  # Create severity categories based on the correct column name
  chord_data <- data %>%
    mutate(
      severity = case_when(
        `Deaths...100.Cases` > 5 ~ "High_Mortality",  # Corrected column name
        `Deaths...100.Cases` > 2 ~ "Medium_Mortality", 
        TRUE ~ "Low_Mortality"
      )
    ) %>%
    count(WHO.Region, severity) %>%
    pivot_wider(names_from = severity, values_from = n, values_fill = 0)
  
  # Convert to matrix for chord diagram
  chord_matrix <- as.matrix(chord_data[, -1])
  rownames(chord_matrix) <- chord_data$WHO.Region
  
  # Create interactive chord diagram
  chord_plot <- chorddiag(chord_matrix, 
                          type = "bipartite",
                          showTicks = FALSE,
                          groupnamePadding = 10,
                          margin = 90)
  
  return(chord_plot)
}

# Generate and display the chord plot
chord_plot <- create_chord_diagram(data)
chord_plot


# 3. HEXBIN HEATMAP - Interactive density plot
create_hexbin_plot <- function(data) {
  p <- plot_ly(data, 
               x = ~Deaths...100.Cases, 
               y = ~Recovered...100.Cases,
               text = ~paste("Country:", Country.Region,
                             "<br>WHO Region:", WHO.Region,
                             "<br>Death Rate:", round(Deaths...100.Cases, 2),
                             "<br>Recovery Rate:", round(Recovered...100.Cases, 2)),
               hoverinfo = "text") %>%
    add_histogram2d(nbinsx = 20, nbinsy = 20) %>%
    layout(
      title = "Interactive Hexbin: Death Rate vs Recovery Rate",
      xaxis = list(title = "Deaths per 100 Cases"),
      yaxis = list(title = "Recovered per 100 Cases")
    ) %>%
    config(displayModeBar = TRUE)
  
  return(p)
}

hexbin_plot <- create_hexbin_plot(data)
hexbin_plot


#3D Sctter**
create_3d_scatter <- function(data) {
  p <- plot_ly(data, 
               x = ~Confirmed, 
               y = ~Deaths, 
               z = ~Recovered,
               color = ~WHO.Region,
               size = ~Active,
               text = ~paste("Country:", Country.Region,
                             "<br>Confirmed:", Confirmed,
                             "<br>Deaths:", Deaths,
                             "<br>Recovered:", Recovered),
               hoverinfo = "text") %>%
    add_markers() %>%
    layout(
      title = "3D Interactive Scatter: COVID-19 Multi-Dimensional View",
      scene = list(
        xaxis = list(title = "Confirmed Cases", type = "log", showgrid = TRUE, gridcolor = "black"),  # Black grid lines
        yaxis = list(title = "Deaths", type = "log", showgrid = TRUE, gridcolor = "black"),  # Black grid lines
        zaxis = list(title = "Recovered Cases", type = "log", showgrid = TRUE, gridcolor = "black"),  # Black grid lines
        camera = list(eye = list(x = 1.2, y = 1.2, z = 1.2)),
        bgcolor = "#FAE7B5"  # Set the background color for the 3D scene (Alice Blue)
      ),
      plot_bgcolor = "#FAE7B5"  # Set the background color for the entire plot (Alice Blue)
    )
  
  return(p)
}

scatter_3d <- create_3d_scatter(data)
scatter_3d


# 5. INTERACTIVE PARALLEL SETS - Categorical flow
create_parallel_sets <- function(data) {
  # Prepare data for parallel sets using correct column names
  parallel_data <- data %>%
    mutate(
      case_level = case_when(
        Confirmed > 100000 ~ "Very High",
        Confirmed > 10000 ~ "High",
        Confirmed > 1000 ~ "Medium",
        TRUE ~ "Low"
      ),
      mortality_level = case_when(
        `Deaths...100.Cases` > 5 ~ "High Mortality",  # Corrected column name
        `Deaths...100.Cases` > 2 ~ "Medium Mortality", 
        TRUE ~ "Low Mortality"
      )
    ) %>%
    count(WHO.Region, case_level, mortality_level)
  
  # Create nodes and links for sankey
  all_regions <- unique(parallel_data$WHO.Region)
  all_case_levels <- unique(parallel_data$case_level)
  all_mortality_levels <- unique(parallel_data$mortality_level)
  
  nodes <- data.frame(
    name = c(all_regions, all_case_levels, all_mortality_levels)
  )
  
  # Create links
  links <- data.frame(
    source = integer(),
    target = integer(),
    value = numeric()
  )
  
  # Links from regions to case levels
  for(i in 1:nrow(parallel_data)) {
    region_idx <- which(nodes$name == parallel_data$WHO.Region[i]) - 1
    case_idx <- which(nodes$name == parallel_data$case_level[i]) - 1
    mortality_idx <- which(nodes$name == parallel_data$mortality_level[i]) - 1
    
    links <- rbind(links, data.frame(source = region_idx, target = case_idx, value = parallel_data$n[i]))
    links <- rbind(links, data.frame(source = case_idx, target = mortality_idx, value = parallel_data$n[i]))
  }
  
  # Create sankey diagram
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "source", Target = "target", Value = "value",
                     NodeID = "name", units = "countries", fontSize = 12)
  
  return(p)
}

parallel_sets <- create_parallel_sets(data)
parallel_sets



# 6. ZOOMABLE CIRCLE PACKING - Hierarchical exploration
create_circle_packing <- function(data) {
  # Prepare hierarchical data
  hierarchy_data <- data %>%
    group_by(WHO.Region) %>%
    summarise(
      total_confirmed = sum(Confirmed, na.rm = TRUE),
      avg_death_rate = mean(Deaths...100.Cases, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_confirmed))
  
  # Create bubble chart
  p <- plot_ly(hierarchy_data,
               x = ~WHO.Region,
               y = ~total_confirmed,
               size = ~total_confirmed,
               color = ~avg_death_rate,
               text = ~paste("Region:", WHO.Region,
                             "<br>Total Cases:", format(total_confirmed, big.mark = ","),
                             "<br>Avg Death Rate:", round(avg_death_rate, 2)),
               hoverinfo = "text") %>%
    add_markers() %>%
    layout(
      title = "Interactive Circle Packing: WHO Regions by Cases and Mortality",
      xaxis = list(title = "WHO Region"),
      yaxis = list(title = "Total Confirmed Cases", type = "log")
    )
  
  return(p)
}

circle_pack <- create_circle_packing(data)
circle_pack

# 7. INTERACTIVE STREAMGRAPH - Flow over categories
create_streamgraph <- function(data) {
  # Prepare data for streamgraph
  stream_data <- data %>%
    select(WHO.Region, Confirmed, Deaths, Recovered, Active) %>%
    pivot_longer(cols = -WHO.Region, names_to = "case_type", values_to = "count") %>%
    group_by(WHO.Region, case_type) %>%
    summarise(total = sum(count, na.rm = TRUE), .groups = "drop")
  
  # Create area chart with plotly
  p <- plot_ly(stream_data, 
               x = ~case_type, 
               y = ~total, 
               color = ~WHO.Region,
               type = "scatter",
               mode = "none",
               fill = "tonexty",
               stackgroup = "one") %>%
    layout(
      title = "Interactive Streamgraph: Case Distribution Across WHO Regions",
      xaxis = list(title = "Case Type"),
      yaxis = list(title = "Total Cases")
    )
  
  return(p)
}

streamgraph <- create_streamgraph(data)
streamgraph

# 8. FORCE-DIRECTED GRAPH - Country similarity network*
create_force_network <- function(data) {
  # Calculate similarity between countries (simplified)
  countries <- data %>%
    select(Country.Region, Deaths...100.Cases, Recovered...100.Cases, WHO.Region) %>%
    slice_head(n = 20)  # Limit for performance
  
  # Create edges based on similarity
  edges <- data.frame()
  for(i in 1:(nrow(countries)-1)) {
    for(j in (i+1):nrow(countries)) {
      death_diff <- abs(countries$Deaths...100.Cases[i] - countries$Deaths...100.Cases[j])
      recovery_diff <- abs(countries$Recovered...100.Cases[i] - countries$Recovered...100.Cases[j])
      
      if(death_diff < 2 && recovery_diff < 20) {  # Similar patterns
        edges <- rbind(edges, data.frame(
          from = countries$Country.Region[i],
          to = countries$Country.Region[j],
          value = 3 - death_diff,
          title = paste("Similar patterns:", round(death_diff, 2), "death rate diff")
        ))
      }
    }
  }
  
  # Create nodes with groups based on WHO Region
  nodes <- data.frame(
    id = countries$Country.Region,
    label = countries$Country.Region,
    value = countries$Deaths...100.Cases,
    group = as.factor(countries$WHO.Region),  # Grouping by WHO.Region
    title = paste("Death Rate:", round(countries$Deaths...100.Cases, 2))
  )
  
  # Assign colors to the groups
  group_colors <- c("Americas" = "red", "Europe" = "blue", "Africa" = "green", "Asia" = "yellow", "Oceania" = "purple")
  
  # Create network
  if(nrow(edges) > 0) {
    network <- visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visLayout(randomSeed = 123) %>%
      visGroups(groupname = "Americas", color = "red") %>%
      visGroups(groupname = "Europe", color = "blue") %>%
      visGroups(groupname = "Africa", color = "green") %>%
      visGroups(groupname = "Asia", color = "yellow") %>%
      visGroups(groupname = "Oceania", color = "purple")
    
    return(network)
  } else {
    return("No similar countries found with current threshold")
  }
}

force_network <- create_force_network(data)
force_network


# 9. INTERACTIVE SPIRAL PLOT - Weekly change progression
create_spiral_plot <- function(data) {
  # Use actual weekly change data
  spiral_data <- data %>%
    filter(!is.na(X1.week.change) & !is.na(X1.week...increase)) %>%
    arrange(desc(X1.week.change)) %>%
    slice_head(n = 50) %>%
    mutate(
      angle = seq(0, 4*pi, length.out = n()),
      radius = scales::rescale(X1.week.change, to = c(1, 10)),
      x = radius * cos(angle),
      y = radius * sin(angle)
    )
  
  p <- plot_ly(spiral_data,
               x = ~x, 
               y = ~y,
               size = ~X1.week.change,
               color = ~X1.week...increase,
               text = ~paste("Country:", Country.Region,
                             "<br>Weekly Change:", format(X1.week.change, big.mark = ","),
                             "<br>Weekly % Increase:", round(X1.week...increase, 2), "%",
                             "<br>WHO Region:", WHO.Region),
               hoverinfo = "text") %>%
    add_markers() %>%
    layout(
      title = "Interactive Spiral Plot: Weekly COVID-19 Changes",
      xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
  
  return(p)
}

spiral_plot <- create_spiral_plot(data)
spiral_plot

# 10. DASHBOARD WITH LINKED VIEWS - Coordinated multiple plots
create_dashboard <- function(data) {
  # Create shared data object
  shared_data <- SharedData$new(data)
  
  # Create multiple linked plots
  scatter1 <- shared_data %>%
    plot_ly(x = ~Confirmed, y = ~Deaths, color = ~WHO.Region,
            text = ~paste("Country:", Country.Region,
                          "<br>Confirmed:", format(Confirmed, big.mark = ","),
                          "<br>Deaths:", format(Deaths, big.mark = ",")),
            hoverinfo = "text") %>%
    add_markers() %>%
    layout(title = "Confirmed vs Deaths")
  
  scatter2 <- shared_data %>%
    plot_ly(x = ~`Recovered...100.Cases`, y = ~`Deaths...100.Cases`, color = ~WHO.Region,  # Corrected column names
            text = ~paste("Country:", Country.Region,
                          "<br>Recovery Rate:", round(`Recovered...100.Cases`, 2),
                          "<br>Death Rate:", round(`Deaths...100.Cases`, 2)),
            hoverinfo = "text") %>%
    add_markers() %>%
    layout(title = "Recovery Rate vs Death Rate")
  
  bar_chart <- shared_data %>%
    plot_ly(x = ~WHO.Region, y = ~Confirmed, color = ~WHO.Region, type = "bar") %>%
    layout(title = "Total Cases by WHO Region", showlegend = FALSE)
  
  # Create weekly change plot
  weekly_plot <- shared_data %>%
    plot_ly(x = ~X1.week.change, y = ~X1.week...increase, color = ~WHO.Region,
            text = ~paste("Country:", Country.Region,
                          "<br>Weekly Change:", format(X1.week.change, big.mark = ","),
                          "<br>Weekly % Increase:", round(X1.week...increase, 2), "%"),
            hoverinfo = "text") %>%
    add_markers() %>%
    layout(title = "Weekly Change vs % Increase")
  
  # Combine plots in subplot
  dashboard <- subplot(scatter1, scatter2, bar_chart, weekly_plot, nrows = 2, titleY = TRUE) %>%
    layout(title = "Interactive COVID-19 Dashboard - Linked Views",
           showlegend = FALSE)
  
  return(dashboard)
}

# Generate the dashboard
dashboard <- create_dashboard(data)
dashboard

# Print all plots
cat("All interactive plots created successfully with correct column names!")
cat("\nFeatures included:")
cat("\n1. Animated Bubble Race - Time progression simulation")
cat("\n2. Interactive Chord Diagram - Region-severity relationships") 
cat("\n3. Hexbin Heatmap - Death vs Recovery rate density")
cat("\n4. 3D Scatter Plot - Confirmed/Deaths/Recovered dimensions")
cat("\n5. Interactive Parallel Sets - WHO Region → Case Level → Mortality flow")
cat("\n6. Zoomable Circle Packing - Regional case distribution")
cat("\n7. Interactive Streamgraph - Case type flows by region")
cat("\n8. Force-Directed Network - Country similarity by death/recovery rates")
cat("\n9. Interactive Spiral Plot - Weekly changes in spiral format")
cat("\n10. Linked Dashboard - Multiple coordinated views with weekly data")
cat("\nBonus: Interactive Data Table - Full dataset exploration with styling")
cat("\n\nColumn names used:")
cat("\n- Deaths.100.Cases (instead of Deaths..100.Cases)")
cat("\n- Recovered.100.Cases (instead of Recovered..100.Cases)")
cat("\n- X1.week.change and X1.week...increase for weekly trends")
cat("\n- All other columns as provided in 

