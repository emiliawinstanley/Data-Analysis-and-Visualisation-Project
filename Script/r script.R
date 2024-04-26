

# Download the mental health and music dataset as 'dataset.csv' via this link
url = https://www.kaggle.com/datasets/catherinerasgaitis/mxmh-survey-results?select=mxmh_survey_results.csv


# DOWNLOAD PACKAGES

# Install and load tidyverse
install.packages("tidyverse")
library(tidyverse)

# Install and load tidyr
install.packages("tidyr")
library("tidyr")

# Install and load ggplot
install.packages("ggplot2")
library(ggplot2)

# Install and load plotly
install.packages("plotly")
library("plotly")

# Install and load dplyr
install.packages("dplyr")
library("dplyr")

# Install and load here
install.packages("here")
library(here)


# DATA PREPROCESSING


# Save dataset as music_mentalhealth_data.csv 
# load the dataset in the csv file into R
dataset <- read.csv("dataset.csv")

# Set working directory 
setwd(here())
data_path <- here::here("dataset", "dataset.csv")

# view the data
view(dataset)

# review the variables in the dataset
colnames(dataset)

# remove variables from dataset that are not relevant to the visualisation
filtered_data <- select(dataset, -c("Timestamp", "Age", "Primary.streaming.service", "While.working", "Instrumentalist", "Composer", "Exploratory", "Foreign.languages", "BPM", "Frequency..Classical.", "Frequency..Country.", "Frequency..EDM.", "Frequency..Folk.", "Frequency..Gospel.", "Frequency..Hip.hop.", "Frequency..Jazz.", "Frequency..K.pop.", "Frequency..Latin.", "Frequency..Metal.", "Frequency..Pop.", "Frequency..R.B.", "Frequency..Rap.", "Frequency..Rock.", "Frequency..Video.game.music.", "Music.effects", "Permissions", "Frequency..Lofi.", "Insomnia", "OCD"))

# Install two colours for the visualisation
custom_colours <- c(anxiety = "#434343", depression = "#2A5676")

# Install two colours from the colourblind palette
colourblind_colours <- c(anxiety = "#CC6677", depression = "#882255")

#label count of favourite genre on each bar
genre_counts <- table(filtered_data$Fav.genre)

#put favourite genres in decreasing order
sorted_genres <- sort(genre_counts, decreasing = TRUE)

#identify favourite genre
most_common_genre <- names(genre_counts)[which.max(genre_counts)]
print(paste("The most common music genre is:", most_common_genre))

#find top 10 favourite genres
top_10_genres <- names(sorted_genres)[1:10]

# Remove the remaining genres from the dataset
data_top_10 <- filtered_data[filtered_data$Fav.genre %in% top_10_genres, ]


# Create a new table with mean scores for anxiety and depression for hours of listening per day
mean_scores_by_hours <- data_top_10 %>%
  group_by(Hours.per.day) %>%
  summarise(
    depression = mean(Depression),
    anxiety = mean(Anxiety)
  )

# Create another table with the mean scores for anxiety and depression for each favourite genre_counts
mean_scores_by_genre <- data_top_10 %>%
  group_by(Fav.genre) %>%
  summarise(
    depression = mean(Depression),
    anxiety = mean(Anxiety)
  )



# Reshape both datasets to long format
mean_scores_long_genre <- pivot_longer(mean_scores_by_genre, 
                                 cols = c(depression, anxiety),
                                 names_to = "Mental_Health", 
                                 values_to = "Mean_Score")

mean_scores_long_hours <- pivot_longer(mean_scores_by_hours, 
                                 cols = c(depression, anxiety),
                                 names_to = "Mental_Health", 
                                 values_to = "Mean_Score")

# VISUALISATIONS

# create a scatterplot with hours of listening on the x axis and mean depression and anxiety scores on the y axis
# Add a title, subtitle and label axes
# Remove of background grid lines
# Make title bold
# Change font to Times New Roman
# Change colour, size and shape of points 

# Change colour of line of best fit


p <- ggplot(mean_scores_long_hours, aes(x = Hours.per.day, y = Mean_Score)) + geom_point(colour = "#2A5676", size = 3, shape = 8) +
  geom_smooth(method = "lm", se = FALSE, colour = "#434343") + 
  labs(x = "Hours", y = "Mental Health Score", subtitle = "The correlation between hours listening to music and mean anxiety and depression scores") +
  ggtitle("Hours Listening to Music vs. Mental Health") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", family = "Times New Roman"),  
        axis.text = element_text(family = "Times New Roman"),  
        axis.title = element_text(family = "Times New Roman"),
        plot.subtitle = element_text(family = "Times New Roman"))

plotly_p <- ggplotly(p)
plotly_p


# Ensure Times New Roman is the custom font (professional)
custom_font <- "Times New Roman"

# Before creating a bar chart, ensure the y axis ticks will ascend to the maximum score
max_score <- ceiling(max(mean_scores_long_genre$Mean_Score))

# Create a bar chart to show anxiety and depression scores by favourite music genre
# Add a title to the bar chart
# Make Title bold
# Add a subtitle to the bar chart
# Label the key
# Align the bars with the x axis
# Add integer lines on the y axis, increasing by 1
# Change colours to colourblind friendly colours
# Make the writing on the x axis slanted
# Delete grid lines
# Add bar lines
# Add ticks
# Centre the title and subtitle
# Change the font of labels and text to the custom font
# Outline the bars
ggplot(mean_scores_long_genre, aes(x = Fav.genre, y = Mean_Score, fill = Mental_Health)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = expression(bold("Mood Disorders by Favorite Music Genre")),
       subtitle = "A visualisation using mean anxiety and depression scores",
       x = expression(bold("Favorite Genre")),
       y = expression(bold("Score")),
       fill = expression(bold("Mood Disorders"))) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = custom_colours) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = custom_font),
        axis.title = element_text(face = "bold", family = custom_font),
        legend.title = element_text(face = "bold", family = custom_font),  
        legend.text = element_text(face = "bold", family = custom_font), 
        legend.position = "right",
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, family = custom_font),
        plot.subtitle = element_text(hjust = 0.5, family = custom_font) ) 

title_text <- "Mood Disorders by Favorite Music Genre"
subtitle_text <- "A visualisation using mean anxiety and depression scores"
x_label_text <- "Favorite Genre"
y_label_text <- "Score"
fill_label_text <- "Mood Disorders"

# Turn the ggplot into an object and make the formatting the same as the original plot 
# Make titles and axes labels bold
# Change font of labels and text to custom font
p1 <- ggplot(mean_scores_long_genre, aes(x = Fav.genre, y = Mean_Score, fill = Mental_Health, text = paste("Genre: ", Fav.genre, "<br>Score: ", Mean_Score, "<br>Mood Disorder: ", Mental_Health))) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = title_text,
       x = x_label_text,
       y = y_label_text,
       fill = fill_label_text) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, max_score, by = 1)) +
  scale_fill_manual(values = custom_colours) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = custom_font),
        axis.title = element_text(face = "bold", family = custom_font),  
        legend.title = element_text(face = "bold", family = custom_font), 
        legend.text = element_text(family = custom_font),
        legend.position = "right",
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, family = custom_font),
        plot.subtitle = element_text(family = custom_font) )

# Add the subtitle into the plot again as it disappears
# Make the title bold and underlined
p1 <- p1 + ggtitle(paste("<b>", title_text, "</b>", "\n", subtitle_text))

# convert ggplot to plotly
p1 <- ggplotly(p1, tooltip = "text")

# Adjust font sizes
p1 <- layout(p1, 
            title = list(font = list(size = 15)),  
            font = list(size = 6),  
            xaxis = list(title = list(font = list(size = 13))),  
            yaxis = list(title = list(font = list(size = 13))), 
            legend = list(
              title = list(text = "Mood Disorders", font = list(size = 13, face = "bold")) 
            ))

# Generate interactive plot
p1

# Save the plot
htmlwidgets::saveWidget(p1, "plot1_customcolours.html")

# Create a second, identical plot but with colourblind friendly colours
p2 <- ggplot(mean_scores_long_genre, aes(x = Fav.genre, y = Mean_Score, fill = Mental_Health, text = paste("Genre: ", Fav.genre, "<br>Score: ", Mean_Score, "<br>Mood Disorder: ", Mental_Health))) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = title_text,
       x = x_label_text,
       y = y_label_text,
       fill = fill_label_text) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, max_score, by = 1)) +
  scale_fill_manual(values = colourblind_colours) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = custom_font),
        axis.title = element_text(face = "bold", family = custom_font),  
        legend.title = element_text(face = "bold", family = custom_font), 
        legend.text = element_text(family = custom_font),
        legend.position = "right",
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, family = custom_font),
        plot.subtitle = element_text(family = custom_font) )

# Add the subtitle into the plot again as it disappears
# Make the title bold and underlined
p2 <- p2 + ggtitle(paste("<b>", title_text, "</b>", "\n", subtitle_text))

# convert ggplot to plotly
p2 <- ggplotly(p2, tooltip = "text")

# Adjust font sizes
p2 <- layout(p2, 
             title = list(font = list(size = 15)),  
             font = list(size = 6),  
             xaxis = list(title = list(font = list(size = 13))),  
             yaxis = list(title = list(font = list(size = 13))), 
             legend = list(
               title = list(text = "Mood Disorders", font = list(size = 13, face = "bold")) 
             ))

# Generate interactive plot
p2

# Save the plot
htmlwidgets::saveWidget(p2, "plot2_colourblind.html")






