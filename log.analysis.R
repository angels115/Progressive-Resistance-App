
# List of exercises and corresponding muscle groups
exercises = list(
  Press = c("Bench Press",
            "Dumbbell Incline Press",
            "One-arm Dumbbell Arnold Press",
            "Clean and Press"),
  Back = c("Chin-Ups",
           "Dumbbell Rows"),
  Legs = c("Squats", "Deadlifts","Lunges","Stiff-Legged Deadlifts"),
  Shoulders = c("Lateral Raises", "Upright Rows"),
  Biceps = c("Barbell Curls", "Dumbbell Curls", "Hammer Curls"),
  Triceps = c("Overhead Triceps Extensions", "Bench-Dips")
)

# Create a data frame from the list
exercise_df <- data.frame(
  Muscle_Group = rep(names(exercises), lengths(exercises)),
  Exercise = unlist(exercises)
)

# Function to calculate 1RM using the Epley formula
calculate_1rm <- function(weight, reps) {
  weight * (1 + reps / 30)
}

# Function to calculate weight using the Epley formula
calculate_weight <- function(one_rep_max, reps) {
  one_rep_max / (1 + reps / 30)
}

# Get a list of workout files
workout_files <- list.files(path = "./logs", pattern = "workout-.*\\.txt$", full.names = TRUE)

# Assuming your list of workout files is named 'workout_files'

# Initialize an empty list to store the extracted data
all_workout_data <- list()

# Loop through each workout file
for (file in workout_files) {
  # Extract the date from the file name (assuming the format "workout-YYYY-MM-DD.txt")
  workout_date <- as.Date(sub("workout-", "", sub("\\.txt", "", basename(file))), format = "%Y-%m-%d")
  
  # Read the file content
  file_content <- readLines(file)
  
  # Extract exercise blocks (assuming each block starts with "Press:", "Back:", etc.)
  exercise_blocks <- strsplit(paste(file_content, collapse = "\n"), "(Press:|Back:|Legs:|Shoulders:|Biceps:|Triceps:)")[[1]][-1]
  
  # Loop through each exercise block
  for (block in exercise_blocks) {
    # Extract exercise name
    exercise_name <- trimws(strsplit(block, "\n")[[1]][1])
    exercise_name <- trimws(sub("- 3 sets", "", exercise_name))  # Remove " - 3 sets" part
    
    # Extract rest time
    rest_time <- as.numeric(sub(".*Rest: (\\d+) seconds.*", "\\1", block))
    
    # Extract sets, reps, and weight using corrected regular expressions
    sets <- as.numeric(sub(".*- (\\d+) sets.*", "\\1", block))
    reps <- unlist(regmatches(block, gregexpr("Reps: (\\d+),", block))) 
    reps <- as.numeric(gsub("[^0-9]", "", reps)) # Extract only the numbers from reps
    weight <- unlist(regmatches(block, gregexpr("Weight: (\\d+) lbs", block)))
    weight <- as.numeric(gsub("[^0-9]", "", weight)) # Extract only the numbers from weight
    
    
    # Create a data frame for the exercise
    exercise_data <- data.frame(
      Date = workout_date,
      Exercise = exercise_name,
      Set = 1:sets,
      Reps = reps,
      Weight = weight,
      Rest = rest_time,
      One_Rep_Max = calculate_1rm(weight, reps)  # Calculate 1RM here
    )
    
    # Add the exercise data to the list
    all_workout_data[[length(all_workout_data) + 1]] <- exercise_data
  }
}

# Combine all workout data into a single data frame
all_workouts_df <- do.call(rbind, all_workout_data)

# Order the data frame by Date in descending order
all_workouts_df <- all_workouts_df[order(all_workouts_df$Date, decreasing = TRUE), ]

# Initialize an empty list to store the averaged 1RM data
averaged_1rm_data <- list()

# Loop through each exercise in the exercise_df
for (i in 1:nrow(exercise_df)) {
  # Get the exercise name
  exercise_name <- exercise_df$Exercise[i]
  
  # Subset the data for the current exercise
  exercise_data <- subset(all_workouts_df, Exercise == exercise_name)
  
  # If the exercise has not appeared in the workouts yet
  if (nrow(exercise_data) == 0) {
    # Store NA for the averaged 1RM data
    averaged_1rm_data[[length(averaged_1rm_data) + 1]] <- data.frame(
      Exercise = exercise_name,
      Set = NA,
      Avg_One_Rep_Max = NA
    )
  } else { 
    # Get the unique dates for this exercise
    unique_dates <- unique(exercise_data$Date)
    
    # If there are less than three workouts, take the average of whatever data there is
    if (length(unique_dates) < 3) { 
      # Calculate the average 1RM for each set
      avg_1rm <- aggregate(One_Rep_Max ~ Set, data = exercise_data, FUN = mean)
      
      # Store the averaged 1RM data
      averaged_1rm_data[[length(averaged_1rm_data) + 1]] <- data.frame(
        Exercise = exercise_name,
        Set = avg_1rm$Set,
        Avg_One_Rep_Max = avg_1rm$One_Rep_Max
      )
    } else {
      # Get the last three workout dates
      last_three_dates <- unique_dates[1:3]
      
      # Subset the data for the last three workouts
      last_three_workouts <- subset(exercise_data, Date %in% last_three_dates)
      
      # Calculate the average 1RM for each set
      avg_1rm <- aggregate(One_Rep_Max ~ Set, data = last_three_workouts, FUN = mean)
      
      # Store the averaged 1RM data
      averaged_1rm_data[[length(averaged_1rm_data) + 1]] <- data.frame(
        Exercise = exercise_name,
        Set = avg_1rm$Set,
        Avg_One_Rep_Max = avg_1rm$One_Rep_Max
      )
    }
  }
}

# Combine all averaged 1RM data into a single data frame
averaged_1rm_df <- do.call(rbind, averaged_1rm_data)

# Generate reference table for suggested weights for each set and rep combination
Sets = c(1,2,3)
Reps = c(3:20)
exercise = as.character(exercise_df$Exercise)

exercise_table = expand.grid(Set = Sets,Reps = Reps,Exercise = exercise)

# Merge the exercise_table with averaged_1rm_df
merged_data <- merge(exercise_table, averaged_1rm_df, by = c("Exercise", "Set"))

# Calculate suggested weight and round to the nearest whole number
merged_data$Suggested_Weight <- round(calculate_weight(merged_data$Avg_One_Rep_Max, merged_data$Reps))

# to provide look-up values for suggested weight
reference.table = merged_data

# For now, choose only suggested weights for set 1
reference.table = reference.table[which(reference.table$Set == 1),]

library(readr)
write_csv(reference.table, file = "./reference.table.csv")


