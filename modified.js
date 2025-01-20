const exercises = {
  Press: {
    exercises: [
      "Bench Press",
      "Dumbbell Incline Press", 
      "One-arm Dumbbell Arnold Press",
      "Clean and Press",
    ],
    repRanges: ["strength", "hypertrophy", "endurance"],
    restTimes: {
      strength: 120,     
      hypertrophy: 120,  
      endurance: 120      
    },
    sets: 4
  },
  
  Back: {
    exercises: [
      "Chin-Ups",
      "Dumbbell Rows",
    ],
    repRanges: ["strength", "hypertrophy", "endurance"],
    restTimes: {
      strength: 120,
      hypertrophy: 120,
      endurance: 120
    },
    sets: 3
  },
  
  Legs: {
    exercises: [
      "Squats",
      "Deadlifts", 
      "Lunges",
      "Stiff-Legged Deadlifts"
    ],
    repRanges: ["strength", "hypertrophy"],
    restTimes: {
      strength: 120,
      hypertrophy: 120
    },
    sets: 3
  },
  Shoulders: {
    exercises: ["Lateral Raises", "Upright Rows"],
    repRanges: ["hypertrophy", "endurance"],
    restTimes: {
      hypertrophy: 60,
      endurance: 60
    },
    sets: 3
  },
  Biceps: {
    exercises: ["Barbell Curls", "Dumbbell Curls", "Hammer Curls"],
    repRanges: ["hypertrophy", "endurance"],
    restTimes: {
      hypertrophy: 60,   
      endurance: 60
    },
    sets: 3
  },
  Triceps: {
    exercises: ["Overhead Triceps Extensions", "Bench-Dips"],
    repRanges: ["hypertrophy", "endurance"],
    restTimes: {
      hypertrophy: 60,
      endurance: 60
    },
    sets: 3
  }
};


const generateButton = document.getElementById("generateButton");
const pauseSessionTimerButton = document.getElementById("pauseSessionTimer");
const sessionTimerDisplay = document.getElementById("sessionTimerDisplay");
const csvFileInput = document.getElementById("csvFileInput"); // Get the file input element
let activeTimerInterval = null;

//Session Timer Variables
let sessionTimerInterval = null;
let sessionTimeElapsed = 0;
let sessionTimerRunning = false;

let referenceData = {}; // Initialize an empty object for referenceData

// Declare workout outside any function
let workout = [];

// Function to parse the CSV data and populate referenceData
function parseCSVData(csvData) {
  const lines = csvData.split("\n");
  const parsedData = {};

  

  // Assuming the first line is a header row, skip it
  for (let i = 1; i < lines.length; i++) {
    const line = lines[i].trim();
    if (line) {
      const values = line.split(",");
     const exercise = values[0].trim(); // trim spaces
      const set = parseInt(values[1]); // Not currently used but kept for potential future use
      const reps = parseInt(values[2]);
      const weight = parseFloat(values[4]);

      if (!parsedData[exercise]) {
        parsedData[exercise] = {};
      }
      parsedData[exercise][reps] = weight;
    }
  }

  return parsedData;
}

// Handle CSV file upload
csvFileInput.addEventListener("change", () => {
  const file = csvFileInput.files[0];
  if (file) {
    const reader = new FileReader();

    reader.onload = (event) => {
      const csvData = event.target.result;
      referenceData = parseCSVData(csvData);
      console.log("CSV data loaded:", referenceData);
    };

    reader.onerror = () => {
      console.error("Error reading file.");
    };

    reader.readAsText(file);
  }
});

// Modified import function
function importWorkout() {
  const importCode = document.getElementById("importCode").value;
  try {
    const workoutData = JSON.parse(atob(importCode));
    
    // Validate version compatibility
    if (!workoutData.version || workoutData.version !== "1.0") {
      throw new Error("Incompatible workout version");
    }
    
    // Initialize workout array
    workout = workoutData.exercises;
    
    // Create the workoutPlanDiv if it doesn't exist
    let workoutPlanDiv = document.getElementById("workoutPlan");
    if (!workoutPlanDiv) {
      workoutPlanDiv = document.createElement("div");
      workoutPlanDiv.id = "workoutPlan";
      document.querySelector(".container").appendChild(workoutPlanDiv);
    }
    
    // Hide the initial buttons
    generateButton.style.display = "none";
    csvFileInput.style.display = "none";
    document.querySelector(".custom-file-upload").style.display = "none";
    document.getElementById("importButton").style.display = "none";
    
    // Show workout interface
    displayWorkout(workout);
    startSessionTimer();
    
    // Close dialog
    document.getElementById("importDialog").close();
    
  } catch (e) {
    console.error(e); // Add this for debugging
    alert("Invalid workout code. Please check and try again.");
  }
}

document.getElementById("importButton").addEventListener("click", () => {
  document.getElementById("importDialog").showModal();
});

// Modify generateButton event listener to only enable buttons after CSV is loaded, and to hide the buttons after the workout is generated
generateButton.addEventListener("click", () => {
  // Check if the user wants to skip the CSV upload only if no CSV has been uploaded
  if (Object.keys(referenceData).length === 0) {
    const skipCSV = confirm("Do you want to skip uploading a CSV file and use default weights?");
    
    if (!skipCSV) {
      alert("Please upload the CSV file first.");
      return;
    }
  }
  
  const workoutPlan = generateWorkout();

  // Create the workoutPlanDiv if it doesn't exist
  let workoutPlanDiv = document.getElementById("workoutPlan");
  if (!workoutPlanDiv) {
    workoutPlanDiv = document.createElement("div");
    workoutPlanDiv.id = "workoutPlan";
    document.querySelector(".container").appendChild(workoutPlanDiv);
  }

  displayWorkout(workoutPlan);

  // Hide the buttons
  generateButton.style.display = "none";
  csvFileInput.style.display = "none"; // Hide the file input
  document.querySelector(".custom-file-upload").style.display = "none"; // Hide the custom label
  document.getElementById("importButton").style.display = "none"; // Hide import button


  startSessionTimer();
});



function generateWorkout() {

  workout = []; // Re-initialize workout inside generateWorkout

  // Get the current date in landlab-MM-DD format
  const today = new Date();
  const dateString = today.toISOString().slice(0, 10); // Format as landlab-MM-DD

  // Define rep ranges
  const repRanges = {
    strength: [3, 5], // 3-5 reps for strength
    hypertrophy: [6, 12], // 6-12 reps for muscle growth
    endurance: [15, 20], // 15-20 reps for muscular endurance
  };

  // Define exercises with restricted rep ranges
  const strengthHypertrophyExercises = [
    "Deadlifts",
    "Squats",
    "Chin-Ups",
    "Clean and Press",
  ];
  const hypertrophyEnduranceExercises = [
    "Dumbbell Rows",
    "Dumbbell Incline Press",
    "Stiff-Legged Deadlifts",
    "Lunges",
    "One-arm Dumbbell Arnold Press",
    "Barbell Curls",
    "Dumbbell Curls",
    "Hammer Curls",
    "Bench-Dips",
    "Overhead Triceps Extensions",
  ];

  // Select one exercise for each muscle group (except for arms)
  for (const muscleGroup in exercises) {
   
      const exerciseObj = exercises[muscleGroup];
      const validExercises = exerciseObj.exercises;
      const randomIndex = Math.floor(Math.random() * validExercises.length);
      const exercise = validExercises[randomIndex];

      // Choose rep range based on exercise selected
      let validRepRanges;
      if (strengthHypertrophyExercises.includes(exercise)) {
        validRepRanges = ["strength", "hypertrophy"];
      } else if (hypertrophyEnduranceExercises.includes(exercise)) {
        validRepRanges = ["hypertrophy", "endurance"];
      } else {
        validRepRanges = exerciseObj.repRanges;
      }

      // Choose a random rep range from the valid rep ranges for the exercise
      const randomRepRangeKey =
        validRepRanges[Math.floor(Math.random() * validRepRanges.length)];
      const [minReps, maxReps] = repRanges[randomRepRangeKey];

      // Randomly choose a rep number within the range
      const reps = getRandomInt(minReps, maxReps);

      workout.push({
        date: dateString, // Add the date to each exercise
        muscleGroup,
        exercise,
        rest: exerciseObj.restTimes[randomRepRangeKey], // Use the rest time based on the selected rep range
        sets: exerciseObj.sets, // Use the defined sets for this muscle group
        reps: reps,
      });
    
  }


  return workout;
}

function getRandomExercise(muscleGroup) {
  const exerciseObj = exercises[muscleGroup];
  const validExercises = exerciseObj.exercises;
  const randomIndex = Math.floor(Math.random() * validExercises.length);
  return validExercises[randomIndex];
}

// Helper function to get a random integer within a range (inclusive)
function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function changeExercise(muscleGroup, workoutIndex) {
  const currentExercise = workout[workoutIndex].exercise;
  const exerciseArray = exercises[muscleGroup].exercises;
  const otherExercises = exerciseArray.filter((ex) => ex !== currentExercise);

  if (otherExercises.length > 0) {
    const newExercise = otherExercises[Math.floor(Math.random() * otherExercises.length)];
    
    // Get the exercise object for the muscle group
    const exerciseObj = exercises[muscleGroup];
    
    // Determine valid rep ranges for the new exercise
    let validRepRanges;
    const strengthHypertrophyExercises = ["Deadlifts", "Squats", "Chin-Ups", "Clean and Press"];
    const hypertrophyEnduranceExercises = [
      "Dumbbell Rows", "Dumbbell Incline Press", "Stiff-Legged Deadlifts",
      "Lunges", "One-arm Dumbbell Arnold Press", "Barbell Curls", "Dumbbell Curls",
      "Hammer Curls", "Bench-Dips", "Overhead Triceps Extensions"
    ];

    if (strengthHypertrophyExercises.includes(newExercise)) {
      validRepRanges = ["strength", "hypertrophy"];
    } else if (hypertrophyEnduranceExercises.includes(newExercise)) {
      validRepRanges = ["hypertrophy", "endurance"];
    } else {
      validRepRanges = exerciseObj.repRanges;
    }

    // Choose a random rep range and get rep count
    const repRanges = {
      strength: [3, 5],
      hypertrophy: [6, 12],
      endurance: [15, 20]
    };
    
    const randomRepRangeKey = validRepRanges[Math.floor(Math.random() * validRepRanges.length)];
    const [minReps, maxReps] = repRanges[randomRepRangeKey];
    const newReps = getRandomInt(minReps, maxReps);

    // Update the workout array with the new exercise and reps
    workout[workoutIndex].exercise = newExercise;
    workout[workoutIndex].reps = newReps;
    workout[workoutIndex].rest = exerciseObj.restTimes[randomRepRangeKey];
    
    // Clear existing weights and actual reps
    workout[workoutIndex].weights = [];
    workout[workoutIndex].actualReps = [];
    
    // Clear saved inputs for this exercise
    const exerciseSets = workout[workoutIndex].sets;
    for (let i = 1; i <= exerciseSets; i++) {
      const repsInput = document.getElementById(`workout-${workoutIndex}-set-${i}-reps`);
      const weightInput = document.getElementById(`workout-${workoutIndex}-set-${i}-weight`);
      if (repsInput) repsInput.value = '';
      if (weightInput) weightInput.value = '';
    }

    // Re-display the updated workout plan
    displayWorkout(workout);
  } else {
    alert("No other exercises available for this muscle group.");
  }
}

function displayWorkout(workout) {
  if (!workout[0].date) {
    const today = new Date();
    const dateString = today.toISOString().split('T')[0];
    workout.forEach(item => item.date = dateString);
  }
  
  const workoutPlanDiv = document.getElementById("workoutPlan");
  
  // Create session timer display if it doesn't exist
  let sessionTimerDisplay = document.getElementById("sessionTimerDisplay");
  if (!sessionTimerDisplay) {
    sessionTimerDisplay = document.createElement("div");
    sessionTimerDisplay.id = "sessionTimerDisplay";
    document.querySelector(".container").insertBefore(sessionTimerDisplay, workoutPlanDiv);
  }
  
  // Start the session timer
  startSessionTimer();
  
  // Save existing input values before clearing the display
  const savedInputs = {};
  workout.forEach((item, workoutIndex) => {
    savedInputs[workoutIndex] = {
      reps: [],
      weights: []
    };
    
    for (let i = 1; i <= item.sets; i++) {
      const repsInput = document.getElementById(`workout-${workoutIndex}-set-${i}-reps`);
      const weightInput = document.getElementById(`workout-${workoutIndex}-set-${i}-weight`);
      
      if (repsInput) savedInputs[workoutIndex].reps[i-1] = repsInput.value;
      if (weightInput) savedInputs[workoutIndex].weights[i-1] = weightInput.value;
    }
  });

  workoutPlanDiv.innerHTML = ""; // Clear previous workout

  const workoutList = document.createElement("ul");

  workout.forEach((item, workoutIndex) => {
    item.weights = [];
    item.actualReps = [];

    const listItem = document.createElement("li");

    // Create a container for the exercise title and button
    const exerciseDiv = document.createElement("div");
    exerciseDiv.classList.add("exercise-title-container");

    const exerciseTitle = document.createElement("h3");
    exerciseTitle.textContent = `${item.muscleGroup}: ${item.exercise}`;
    exerciseDiv.appendChild(exerciseTitle);

    // Create change exercise button
    const changeExerciseButton = document.createElement("button");
    changeExerciseButton.classList.add("change-exercise-button");
    changeExerciseButton.textContent = "Change Exercise";
    changeExerciseButton.dataset.muscleGroup = item.muscleGroup;
    changeExerciseButton.dataset.workoutIndex = workoutIndex;
    exerciseDiv.appendChild(changeExerciseButton);

    listItem.appendChild(exerciseDiv); // Append the container to the list item

    // Add event listener to change exercise button
    changeExerciseButton.addEventListener("click", () => {
      const muscleGroup = changeExerciseButton.dataset.muscleGroup;
      const workoutIndex = parseInt(changeExerciseButton.dataset.workoutIndex);
      changeExercise(muscleGroup, workoutIndex);
    });

    const setInputsList = document.createElement("ul");
    for (let i = 1; i <= item.sets; i++) {
      const setInputItem = document.createElement("li");
      setInputItem.classList.add("set-input-item");
      const inputId = `workout-${workoutIndex}-set-${i}`;

      // Input field for actual reps
      const repsInput = document.createElement("input");
      repsInput.type = "number";
      repsInput.id = `${inputId}-reps`;
      repsInput.classList.add("reps-input");
      repsInput.placeholder = "Enter reps";
      setInputItem.appendChild(document.createTextNode(" Reps: "));
      // Restore saved reps value or use default
      repsInput.value = savedInputs[workoutIndex]?.reps[i-1] || item.reps;
      setInputItem.appendChild(repsInput);

      // Look up suggested weight
      let suggestedWeight = "";
      if (referenceData[item.exercise] && referenceData[item.exercise][item.reps]) {
        suggestedWeight = referenceData[item.exercise][item.reps];
      }

      const weightInput = document.createElement("input");
      weightInput.type = "number";
      weightInput.id = `${inputId}-weight`;
      weightInput.classList.add("weight-input");
      weightInput.placeholder = "Enter weight";
      // Restore saved weight value or use suggested weight
      weightInput.value = savedInputs[workoutIndex]?.weights[i-1] || suggestedWeight;
      setInputItem.appendChild(document.createTextNode(" Weight: "));
      setInputItem.appendChild(weightInput);
      setInputItem.appendChild(document.createTextNode(" lbs"));

      // Store the suggested weight as the initial value
      item.weights[i - 1] = suggestedWeight ? parseInt(suggestedWeight) : null;

      // Create timer display span
      const timerDiv = document.createElement("div");
      timerDiv.id = `timer-${inputId}`;
      timerDiv.classList.add("timer-display");
      setInputItem.appendChild(timerDiv);

      // Create the progress bar elements
      const progressBarContainer = document.createElement("div");
      progressBarContainer.classList.add("progress-bar-container");
      const progressBar = document.createElement("div");
      progressBar.classList.add("progress-bar");
      progressBarContainer.appendChild(progressBar);
      timerDiv.appendChild(progressBarContainer);

      // Create timer display span
      const timerSpan = document.createElement("span");
      timerSpan.id = `timer-text-${inputId}`;
      timerSpan.classList.add("timer-text");
      timerDiv.appendChild(timerSpan);

      // Create start timer button
      const startTimerButton = document.createElement("button");
      startTimerButton.classList.add("start-timer-button");
      startTimerButton.textContent = "Start Timer";
      startTimerButton.dataset.timerId = `timer-${inputId}`;
      startTimerButton.dataset.rest = item.rest;
      setInputItem.appendChild(startTimerButton);

      // Add event listeners to capture reps and weight input
      repsInput.addEventListener("input", () => {
        item.actualReps[i - 1] = repsInput.value ? parseInt(repsInput.value) : null;
      });

      weightInput.addEventListener("input", () => {
        item.weights[i - 1] = weightInput.value ? parseInt(weightInput.value) : null;
      });

      setInputsList.appendChild(setInputItem);

      // Capture initial rep value if not changed
      if (item.actualReps.length < item.sets) {
        item.actualReps.push(item.reps);
      }

      // Attach event listeners to start timer buttons
      startTimerButton.addEventListener("click", () => {
        const duration = parseInt(startTimerButton.dataset.rest);
        const timerId = startTimerButton.dataset.timerId;
        const timerDisplay = document.getElementById(timerId);
        startTimer(duration, timerDisplay);
      });
    }

    listItem.appendChild(setInputsList);
    workoutList.appendChild(listItem);
  });

  workoutPlanDiv.appendChild(workoutList);

  // Add a "Finish Workout" button
  const finishButton = document.createElement("button");
  finishButton.textContent = "Finish Workout";

  // Add event listener to the button
  finishButton.addEventListener("click", () => {
    // Confirm if all weights are entered
    const allWeightsEntered = workout.every((item) =>
      item.weights.every((weight) => weight !== null)
    );

    if (!allWeightsEntered) {
      alert("Please enter weights for all sets before finishing the workout.");
      return;
    }

    // Format workout data and initiate download
    const workoutText = formatWorkoutData(workout);
    const workoutDate = workout[0].date;
    downloadWorkout(workoutText, workoutDate);

    // Stop the session timer
    stopSessionTimer();
  });

  workoutPlanDiv.appendChild(finishButton);

  // Add share button next to finish button
  const shareButton = document.createElement("button");
  shareButton.textContent = "Share Workout";
  shareButton.addEventListener("click", () => {
    const shareableCode = createShareableWorkout(workout);
    
    // Create share dialog
    const dialog = document.createElement("dialog");
    dialog.innerHTML = `
      <h3>Share Your Workout</h3>
      <p>Share this code with others:</p>
      <input type="text" value="${shareableCode}" readonly />
      <button onclick="navigator.clipboard.writeText('${shareableCode}')">
        Copy Code
      </button>
      <button onclick="this.parentElement.close()">Close</button>
    `;
    document.body.appendChild(dialog);
    dialog.showModal();
  });
  workoutPlanDiv.appendChild(shareButton);

  // Add "Share via Text" button
  const textShareButton = document.createElement("button");
  textShareButton.textContent = "Share via Text";
  textShareButton.addEventListener("click", () => {
    const textContent = formatWorkoutForText(workout);
    const encodedText = encodeURIComponent(textContent);
    window.open(`sms:?&body=${encodedText}`);
  });
  workoutPlanDiv.appendChild(textShareButton);
}

function startTimer(duration, display) {
  clearInterval(activeTimerInterval); // Clear any active timer

  const timerSound = document.getElementById("timerSound");
  timerSound.pause();
  timerSound.currentTime = 0;

  let timer = duration;
  const timerText = display.querySelector(`.timer-text`); // Get the timer text span element
  const progressBar = display.querySelector(".progress-bar"); // Get the progress bar element

  // Set initial time and reset progress bar
  timerText.textContent = formatTime(timer);
  progressBar.style.width = "0%";

  activeTimerInterval = setInterval(() => {
    timer--;
    timerText.textContent = formatTime(timer);

    // Update the progress bar width
    const progress = ((duration - timer) / duration) * 100;
    progressBar.style.width = `${progress}%`;

    if (timer < 0) {
      clearInterval(activeTimerInterval);
      timerText.textContent = "Time's up!";
      timerSound.play();
    }
  }, 1000);
}

// Helper function to format time (add leading zeros)
function formatTime(seconds) {
  const minutes = Math.floor(seconds / 60);
  const remainingSeconds = seconds % 60;
  return `${minutes.toString().padStart(2, "0")}:${remainingSeconds
    .toString()
    .padStart(2, "0")}`;
}

function formatWorkoutData(workout) {
  let workoutText = "";

  // Group exercises by date
  const groupedWorkouts = {};
  for (const item of workout) {
    if (!groupedWorkouts[item.date]) {
      groupedWorkouts[item.date] = [];
    }
    groupedWorkouts[item.date].push(item);
  }

  // Format each workout
  for (const date in groupedWorkouts) {
    const workoutItems = groupedWorkouts[date];
    workoutText += `Workout Date: ${date}\n`;

    for (const item of workoutItems) {
      workoutText += `\n${item.muscleGroup}: ${item.exercise} - ${item.sets} sets\n`;
      for (let i = 0; i < item.sets; i++) {
        workoutText += `  Set ${i + 1}: Reps: ${
          item.actualReps[i] || ""
        }, Weight: ${item.weights[i] || ""} lbs\n`;
      }
      workoutText += `  Rest: ${item.rest} seconds\n`;
    }
    workoutText += "\n";
  }

  return workoutText;
}

function downloadWorkout(workoutText, dateString) {
  const blob = new Blob([workoutText], { type: "text/plain" });
  const link = document.createElement("a");
  link.href = URL.createObjectURL(blob);
  link.download = `workout-${dateString}.txt`;
  link.click();
}

//Start of new code for Session Timer
function startSessionTimer() {
    if (!sessionTimerRunning) {
        sessionTimerRunning = true;
        // Get existing timer display
        const timerDisplay = document.getElementById("sessionTimerDisplay");
        if (!timerDisplay) {
            console.error("Timer display element not found");
            return;
        }
        updateSessionTimerDisplay();
        sessionTimerInterval = setInterval(() => {
            sessionTimeElapsed++;
            updateSessionTimerDisplay();
        }, 1000);
    }
}

function stopSessionTimer() {
    clearInterval(sessionTimerInterval);
    sessionTimerRunning = false;
    sessionTimeElapsed = 0; // Reset the timer
    updateSessionTimerDisplay();
}

function pauseSessionTimer() {
    if (sessionTimerRunning) {
        clearInterval(sessionTimerInterval);
        sessionTimerRunning = false;
        pauseSessionTimerButton.textContent = "Resume";
    } else {
        startSessionTimer();
        pauseSessionTimerButton.textContent = "Pause";
    }
}

function updateSessionTimerDisplay() {
    const timerDisplay = document.getElementById("sessionTimerDisplay");
    if (!timerDisplay) return;
    
    const hours = Math.floor(sessionTimeElapsed / 3600);
    const minutes = Math.floor((sessionTimeElapsed % 3600) / 60);
    const seconds = sessionTimeElapsed % 60;
    
    timerDisplay.textContent = `Training Session Time: ${hours}h ${minutes}m ${seconds}s`;
}

function createShareableWorkout(workout) {
  const shareableData = {
    date: workout[0].date,
    exercises: workout.map(item => ({
      muscleGroup: item.muscleGroup,
      exercise: item.exercise,
      sets: item.sets,
      reps: item.reps,
      weights: item.weights,
      rest: item.rest,
      actualReps: item.actualReps
    })),
    totalTime: sessionTimeElapsed,
    version: "1.0"
  };
  
  return btoa(JSON.stringify(shareableData)); // Base64 encode for sharing
}

function formatWorkoutForText(workout) {
  let workoutText = "Today's Workout:\n\n";
  
  for (const item of workout) {
    // Add exercise header
    workoutText += `${item.exercise}\n`;
    
    // Format sets/reps/weight in a compact way
    for (let i = 0; i < item.sets; i++) {
      const weight = item.weights[i] || '?';
      const reps = item.actualReps[i] || item.reps;
      workoutText += `Set ${i + 1}: ${weight}lbs x ${reps}\n`;
    }
    workoutText += '\n';
  }
  
  // Add total workout time
  const hours = Math.floor(sessionTimeElapsed / 3600);
  const minutes = Math.floor((sessionTimeElapsed % 3600) / 60);
  workoutText += `Total time: ${hours}h ${minutes}m`;
  
  return workoutText;
}



