# Load required libraries
library(neuralnet)

# Function to generate random student grades
generate_student_grades <- function(n_students) {
  set.seed(123)
  
  # Generate random grades for quizzes, assignments, and tests
  quizzes <- round(runif(n_students, min = 0, max = 100), 1) 
  assignments <- round(runif(n_students, min = 0, max = 100), 1) 
  tests <- round(runif(n_students, min = 0, max = 100), 1) 
  
  # Combine into a dataframe
  student_grades <- data.frame(Quizzes = quizzes,
                               Assignments = assignments,
                               Tests = tests)
  return(student_grades)
}

# Generate student grades data for 100 students
student_grades <- generate_student_grades(100)

# Function to determine pass/fail based on average grade
classify_pass_fail <- function(grades, pass_threshold = 50) {
  
  # Calculate average grade
  avg_grade <- rowMeans(grades)
  
  # Classify as pass or fail based on threshold
  pass_fail <- ifelse(avg_grade >= pass_threshold, "Pass", "Fail")
  return(pass_fail)
}

# Classify pass/fail
pass_fail <- classify_pass_fail(student_grades)

# Combine grades and pass/fail status into one dataframe
student_data <- cbind(student_grades, Pass_Fail = pass_fail)

# Construct the formula for neural network model
neural_formula <- as.formula("Pass_Fail ~ .")

# Train and Plot neural network model
neural_model <- neuralnet(neural_formula, data = student_data, hidden = 10)
plot(neural_model)

# Make predictions on the training data
predictions <- predict(neural_model, student_data)

# Loop through the predictions and print the class with the highest probability for each student
for (i in 1:nrow(predictions)) {
  if (predictions[i] >= 0.5) {
    print("Pass")
  } else {
    print("Fail")
  }
}

