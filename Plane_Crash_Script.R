#_________________PREPARE AND CLEAN_____________________
#Import datasets
planecrash<-read.csv("C:\\Users\\Usuario\\Documents\\Data Analytics\\Plane_Crash_Analysis\\Plane_Crash_Analysis\\planecrashinfo.csv")

#Install packages
install.packages("dplyr") 
install.packages("janitor") 
install.packages("lubridate") 
install.packages("tidyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("knitr")

#Load packages
library("dplyr") 
library("janitor") 
library("lubridate") 
library("tidyr")
library("tidyverse")
library("ggplot2")
library("reshape2")
library("knitr")

#Check info type
str(planecrash)

#Correct date column


# Convert the "date" column to the desired format
planecrash$date <- mdy(planecrash$date)  # mdy converts from month-day-year

# Change the date format to "yyyy/mm/dd"
planecrash$date <- format(planecrash$date, "%Y/%m/%d")

# Change the data type of the "date" column to Date
planecrash$date <- as.Date(planecrash$date)


#correct time column 

planecrash$time <- gsub("[^0-9:]", "", planecrash$time)
planecrash$time <- gsub("(\\d{2})(\\d{2})", "\\1:\\2", planecrash$time)
planecrash$time <- as.POSIXct(planecrash$time, format = "%H:%M")
planecrash$time <- format(planecrash$time, format = "%H:%M")

#correct flight type

# Remove non-numeric characters from the "flight_no" column
planecrash$flight_no <- gsub("[^0-9]", "", planecrash$flight_no)

# Convert the "flight_no" column to integers
planecrash$flight_no <- as.integer(planecrash$flight_no)

# Split the "route" column into two separate columns by "-"
planecrash <- separate(planecrash, route, into = c("origin", "destination"), sep = "-")

# Create a new column "fatalities_number" containing the first two characters from the "fatalities" column
planecrash$fatalities_number <- substr(planecrash$fatalities, 1, 2)

# Convert the "fatalities_number" column to integers
planecrash$fatalities_number <- as.integer(planecrash$fatalities_number)

str(planecrash)

#Create column "year"
planecrash$year <- year(planecrash$date)

#Verify amount of operators
num_unique_operators <- planecrash %>% 
  summarise(num_distinct_operators = n_distinct(operator))
print(num_unique_operators)
#(num_unique_operators=2821)



#_________________________ANALYSIS__________________________

#PLANECRASHes DURANTE LOS AÑOS 

crashes_per_year<- planecrash %>%
  count(year)

ggplot(crashes_per_year, aes(x = year, y = n)) +
  geom_line() +
  labs(x = "Year", y = "Count", title = "Count of Plane Crashes by Year")


#Fatalities per year

fatalities_per_year <-
  planecrash %>% 
  group_by(year) %>%
  summarise(total_fatalities= sum(fatalities_number, na.rm = TRUE))

ggplot(fatalities_per_year, aes(x = year, y = total_fatalities)) +
  geom_line() +
  labs(x = "Year", y = "Total Fatalities", title = "Total Fatalities per Year")

#Count of crashes by country


#Count all the US crashes, by summing all accidents from each state.

# Function to count occurrences of states
count_states <- function(states, data) {
  library(stringr)
  
  state_counts <- numeric(length(states))
  
  for (i in 1:length(states)) {
    state_counts[i] <- sum(str_detect(tolower(data$location), tolower(states[i])))
  }
  
  state_counts_df <- data.frame(State = states, Frequency = state_counts)
  
  return(state_counts_df)
}

# List of US states 
us_states <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
# Count occurrences of states
state_counts_df <- count_states(us_states, planecrash)

print(state_counts_df)


# Sum of the 'Frequency' column
total_sum <- sum(state_counts_df$Frequency)

print(total_sum)
#Total_sum_US=1510

#Count crashes by country

# Split each string into words
words <- unlist(strsplit(planecrash$location, "\\s+"))

# Count the frequency of each word
word_counts <- table(words)

# Sort the words by frequency
sorted_word_counts <- sort(word_counts, decreasing = TRUE)
word_counts_df <- as.data.frame(sorted_word_counts)

#Delete Rows that aren't countrys in the first 25 rows
rows_to_remove <- c(1, 2, 3, 12, 17, 18, 20, 28)

# Remove the specified rows from the dataframe
word_counts_df <- word_counts_df[-rows_to_remove, ]

# Rename the column "words" to "country"
names(word_counts_df)[names(word_counts_df) == "words"] <- "country"

# Rename the dataframe from word_counts_df to crash_count_by_country
crash_count_by_country <- word_counts_df

#Add the Values of USA

new_row <- data.frame(country = "USA", Freq = 1510)

# Add the new row to the dataframe
crash_count_by_country <- rbind(new_row, crash_count_by_country)

# Find 15 countrys with more crashes.

# Keep only the first 15 rows of the dataframe
crash_count_by_country <- crash_count_by_country[1:15, ]
row.names(crash_count_by_country) <- NULL

#Barplot Crash count by country

# Crear el gráfico de barras
bar_plot <- ggplot(crash_count_by_country, aes(x = country, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Country", y = "Frequency", title = "Plane crashes by Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar el gráfico
print(bar_plot)


#Aircrafts with the most accidents

# Count the frequency of each value in the "ac_type" column
ac_type_counts <- table(planecrash$ac_type)

# Sort the results in descending order
ac_type_counts_sorted <- sort(ac_type_counts, decreasing = TRUE)

# Show the top 10 most common values and their frequency
top_10_ac_types <- head(ac_type_counts_sorted, 5)

# Convert the result into a dataframe
top_5_ac_types_df <- as.data.frame(top_5_ac_types)

# Rename the columns
colnames(top_5_ac_types_df) <- c("ac_type", "Frequency")

# Print the dataframe
print(top_5_ac_types_df)

#Barplot
bar_plot <- ggplot(top_5_ac_types_df, aes(x = Frequency, y = ac_type)) +
  geom_col(fill = "orange") +
  labs(x = "Frequency", y = "Aircraft type", title = "Top 5 Aircraft Types with the most accidents") +
  theme_minimal()

print(bar_plot)



# Summarize the data to get counts of accidents for each year and aircraft type
summary_data <- planecrash %>%
  group_by(year, ac_type) %>%
  summarise(count = n()) %>%
  filter(ac_type %in% top_5_ac_types_df$ac_type)  

# Create the line plot
line_plot <- ggplot(summary_data, aes(x = year, y = count, color = ac_type)) +
  geom_line(size = 0.6) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +
  labs(x = "Year", y = "Count", title = "Crashes by Aircraft Type Over Time") +
  theme_minimal()

# Show the plot
print(line_plot)



