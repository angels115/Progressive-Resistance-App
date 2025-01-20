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
                  choices = "Please select a directory first", selected = NULL)
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
        req(input$exerciseSelect)
        
        filtered_data <- all_workouts_df %>%
          filter(Exercise == input$exerciseSelect)
        
        ggplot(filtered_data, aes(x = Date, y = MovingAverage, label = round(MovingAverage))) +
          geom_line(color = "#000080", size = 1.5) +
          geom_point(color = "#000080", size = 4, shape = 19) +
          geom_text(aes(label = round(MovingAverage)), vjust = -2, size = 4, family = "sans", fontface = "bold") +
          labs(
            x = "Date",
            y = "Moving Average of 1RM") +
          theme(
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 16, color = "black", face = "bold", family = "sans"),
            axis.title = element_text(size = 12, color = "black", family = "sans"),
            axis.text = element_text(color = "black"),
            legend.position = "none",
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
          ) +
          scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
          scale_y_continuous(expand = expansion(mult = c(0.25, 0.25)))
      })
      
    } else {
      print("all_workouts_df is NULL or empty. Skipping legend_df creation and plotting.")
    }
  })
  
}

shinyApp(ui, server)



