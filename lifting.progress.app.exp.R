library(shiny)
library(shinyFiles)
library(ggplot2)
library(dplyr)
library(purrr)
library(zoo)

# Function to calculate 1RM
calculate_1rm <- function(weight, reps) {
  weight * (1 + reps / 30)
}

# UI for application
ui <- fluidPage(
  
  titlePanel("Performance Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("dir", "Choose directory", "Please select a folder", title = "Select directory"),
      selectInput("exerciseSelect", "Select Exercise:",
                  choices = "Please select a directory first", selected = NULL),
      dateInput("startDate", "Progress Start Date", 
                value = "2025-01-01",  # Fixed default
                format = "yyyy-mm-dd"),
      dateInput("endDate", "Target End Date", 
                value = "2025-12-31",  # Fixed default
                format = "yyyy-mm-dd"),
      numericInput("targetIncrease", "Target Weight Increase (lbs)", value = 300, min = 1)
    ),
    
    mainPanel(
      
    )
    
  ), 
  
  div(style="display: flex; justify-content: center;",
      plotOutput("oneRepMaxPlot", height = 450, width = "50%"))
)

server <- function(input, output, session) {
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  
  observeEvent(input$dir, {
    tryCatch({
      shinyDirChoose(input, "dir", roots = volumes, session = session, restrictions = system.file(package = "base"))
      print("shinyDirChoose executed successfully.")
    }, error = function(e) {
      print(paste("Error in shinyDirChoose:", e$message))
    })
  })
  
  selectedDir <- reactive({
    req(input$dir)
    parseDirPath(volumes, input$dir)
  })
  
  observeEvent(selectedDir(), {
    
    workout_files <- list.files(path = selectedDir(), pattern = "workout-.*\\.txt$", full.names = TRUE)
    print(workout_files)
    
    all_workout_data <- list()
    for (file in workout_files) {
      workout_date <- as.Date(sub("workout-", "", sub("\\.txt", "", basename(file))), format = "%Y-%m-%d")
      file_content <- readLines(file)
      exercise_blocks <- strsplit(paste(file_content, collapse = "\n"), "(Press:|Back:|Legs:|Shoulders:|Biceps:|Triceps:)")[[1]][-1]
      
      for (block in exercise_blocks) {
        exercise_name <- trimws(strsplit(block, "\n")[[1]][1])
        exercise_name <- trimws(sub("- 3 sets", "", exercise_name))
        rest_time <- as.numeric(sub(".*Rest: (\\d+) seconds.*", "\\1", block))
        sets <- as.numeric(sub(".*- (\\d+) sets.*", "\\1", block))
        reps <- unlist(regmatches(block, gregexpr("Reps: (\\d+),", block)))
        reps <- as.numeric(gsub("[^0-9]", "", reps))
        weight <- unlist(regmatches(block, gregexpr("Weight: (\\d+) lbs", block)))
        weight <- as.numeric(gsub("[^0-9]", "", weight))
        
        exercise_data <- data.frame(
          Date = workout_date,
          Exercise = exercise_name,
          Set = 1:sets,
          Reps = reps,
          Weight = weight,
          Rest = rest_time,
          One_Rep_Max = calculate_1rm(weight, reps)
        )
        
        all_workout_data[[length(all_workout_data) + 1]] <- exercise_data
      }
    }
    
    all_workouts_df <- do.call(rbind, all_workout_data)
    
    if (is.data.frame(all_workouts_df) && nrow(all_workouts_df) > 0) {
      all_workouts_df <- all_workouts_df[order(all_workouts_df$Date, decreasing = TRUE), ]
    } else {
      print("No data found in workout files or all_workouts_df is not a data frame.")
    }
    
    exercises = list(
      Press = c("Bench Press", "Dumbbell Incline Press", "One-arm Dumbbell Arnold Press", "Clean and Press"),
      Back = c("Chin-Ups", "Dumbbell Rows"),
      Legs = c("Squats", "Deadlifts","Lunges","Stiff-Legged Deadlifts"),
      Shoulders = c("Lateral Raises", "Upright Rows"),
      biceps = c("Barbell Curls", "Dumbbell Curls", "Hammer Curls"),
      triceps = c("Overhead Triceps Extensions", "Bench-Dips")
    )
    
    exercise_df <- data.frame(
      Muscle_Group = rep(names(exercises), lengths(exercises)),
      Exercise = unlist(exercises)
    )
    
    if (is.data.frame(all_workouts_df) && nrow(all_workouts_df) > 0) {
      all_workouts_df <- merge(all_workouts_df, exercise_df, by = "Exercise")
    } else {
      print("Skipping merge: all_workouts_df is empty or not a data frame.")
    }
    
    all_workouts_df_allsets = all_workouts_df
    all_workouts_df = all_workouts_df[which(all_workouts_df$Set == 1),]
    
    # Check if all_workouts_df is NULL or empty
    if (!is.null(all_workouts_df) && nrow(all_workouts_df) > 0) {
      legend_df <- all_workouts_df %>%
        select(Exercise, Muscle_Group) %>%
        distinct() %>%
        arrange(Muscle_Group)
      
      calculate_synthetic_1rm <- function(df, date_) {
        latest_1rm <- df %>%
          filter(Exercise %in% c("Bench Press", "Squats", "Deadlifts", "Chin-Ups", "Clean and Press")) %>%
          filter(Date <= date_) %>%
          group_by(Exercise) %>%
          arrange(desc(Date)) %>%
          slice_head(n = 1) %>%
          ungroup()
        
        if (sum(latest_1rm$Exercise %in% c("Bench Press", "Squats", "Deadlifts", "Chin-Ups", "Clean and Press")) < 5) {
          return(NA)
        } else {
          synthetic_1rm <- sum(latest_1rm$One_Rep_Max)
          return(synthetic_1rm)
        }
      }
      
      synthetic_1rm_data <- all_workouts_df %>%
        select(Date) %>%
        distinct() %>%
        mutate(Synthetic_1RM = map_dbl(Date, ~calculate_synthetic_1rm(all_workouts_df, .x)))
      
      synthetic_exercise_data <- synthetic_1rm_data %>%
        rename(One_Rep_Max = Synthetic_1RM) %>%
        mutate(Exercise = "Compound Lift Total") %>%
        select(Date, Exercise, One_Rep_Max)
      
      all_workouts_df <- bind_rows(all_workouts_df, synthetic_exercise_data)
      
      all_workouts_df$Week <- as.Date(cut(all_workouts_df$Date, breaks = "week", start.on.monday = FALSE))
      
      all_workouts_df <- all_workouts_df %>%
        group_by(Exercise) %>%
        arrange(Date) %>%
        mutate(Workout_Number = row_number()) %>%
        mutate(MovingAverage = rollapply(One_Rep_Max, width = 3, FUN = mean, align = "right", fill = NA)) %>%
        ungroup()
      
      all_workouts_df = all_workouts_df[-which(is.na(all_workouts_df$MovingAverage)),]
      
      weekly_sets <- all_workouts_df %>%
        group_by(Muscle_Group, Week) %>%
        summarize(TotalSets = length(Set))
      
      updateSelectInput(session, "exerciseSelect",
                        choices = unique(all_workouts_df$Exercise),
                        selected = "Bench Press")
      

      
      output$oneRepMaxPlot <- renderPlot({
        req(input$exerciseSelect, input$startDate, input$endDate, input$targetIncrease)
        
        filtered_data <- all_workouts_df %>%
          filter(Exercise == input$exerciseSelect,
                 Date >= input$startDate,
                 Date <= input$endDate)
        
        # Dynamic y-axis label
        y_label <- ifelse(input$exerciseSelect == "Compound Lift Total",
                         "Compound Lift Total (lbs)",
                         "Moving Average of 1RM (lbs)")
        
        if(nrow(filtered_data) > 0) {
          start_total <- filtered_data$MovingAverage[which.min(filtered_data$Date)]
          target_total <- start_total + input$targetIncrease
          days <- as.numeric(input$endDate - input$startDate)
          
          projected_data <- data.frame(
            Date = seq(input$startDate, input$endDate, by = "day"),
            Projected_1RM = seq(start_total, target_total, length.out = days + 1)
          )
          
          # Calculate weekly progress rates
          days_elapsed <- as.numeric(max(filtered_data$Date) - min(filtered_data$Date))
          actual_weeks_elapsed <- days_elapsed / 7
          current_total_increase <- tail(filtered_data$MovingAverage, 1) - start_total
          current_weekly_rate <- current_total_increase / actual_weeks_elapsed
          
          # Calculate required weekly increase
          total_weeks <- days / 7
          required_weekly_increase <- input$targetIncrease / total_weeks
          
          # Update subtitle
          subtitle_text <- sprintf("Required: %.1f lbs/week | Current: %.1f lbs/week | Days Tracked: %d",
                                    required_weekly_increase,
                                    current_weekly_rate,
                                    days_elapsed)
          
          ggplot(filtered_data, aes(x = Date, y = MovingAverage)) +
            geom_line(aes(color = "Actual Progress"), size = 1.2) +
            geom_line(data = projected_data, 
                      aes(x = Date, y = Projected_1RM, color = "Target Trajectory"), 
                      linetype = "dashed", size = 1.2) +
            geom_point(aes(y = MovingAverage), size = 3, color = "#2c7fb8") +
            geom_text(aes(y = MovingAverage, label = round(MovingAverage)), 
                      vjust = -1, size = 3.5) +
            scale_color_manual(values = c("Actual Progress" = "#2c7fb8", 
                                          "Target Trajectory" = "#e41a1c")) +
            labs(
              title = paste("Progress Towards", input$targetIncrease, "lbs Goal"),
              subtitle = subtitle_text,
              y = y_label,
              color = "Legend"
            ) +
            theme_minimal(base_size = 14) +
            theme(
              legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = 12)
            )
        } else {
          ggplot() + 
            geom_text(aes(x = 0.5, y = 0.5, 
                         label = "No data available for selected dates/exercise"), 
                     size = 5) + 
            theme_void()
        }
      })
      
    } else {
      print("all_workouts_df is NULL or empty. Skipping legend_df creation and plotting.")
    }
  })
  
}

shinyApp(ui, server)



