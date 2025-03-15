# Game Setup --------------------------------------------------------------
questions <- data.frame(
  question = c(
    "What bias occurs when data collection doesn't represent the whole population?",
    "What bias happens when we seek information that confirms our existing beliefs?",
    "What bias results from past inequalities in datasets?",
    "What bias occurs when a dataset excludes key groups, leading to misleading results?",
    "What bias happens when a model learns incorrect patterns due to biased training data?",
    "What bias occurs when a dataset reflects societal stereotypes?",
    "What bias arises when only successful cases are analyzed, ignoring failures?",
    "What bias occurs when different groups are measured inconsistently?",
    "What bias occurs when incorrect or misleading data is more likely to be published?",
    "What bias happens when a dataset reflects outdated societal norms?"
  ),
  options = I(list(
    c("Confirmation", "Sampling", "Selection"),
    c("Confirmation", "Reporting", "Historical"),
    c("Algorithmic", "Historical", "Measurement"),
    c("Exclusion", "Survivorship", "Reporting"),
    c("Algorithmic", "Sampling", "Confirmation"),
    c("Selection", "Measurement", "Reporting"),
    c("Survivorship", "Exclusion", "Algorithmic"),
    c("Measurement", "Sampling", "Selection"),
    c("Reporting", "Historical", "Exclusion"),
    c("Historical", "Algorithmic", "Confirmation")
  )),
  correct = c("Sampling", "Confirmation", "Historical", "Exclusion", "Algorithmic", 
              "Selection", "Survivorship", "Measurement", "Reporting", "Historical")
)

current_user <- NULL
scores <- list(played = 0, correct = 0)


# Core Functions ----------------------------------------------------------
show_stats <- function() {
  cat("\nPlayer:", current_user)
  cat("\nTotal Questions:", scores$played)
  cat("\nCorrect Answers:", scores$correct)
  if(scores$played > 0) {
    cat("\nAccuracy:", round(scores$correct/scores$played*100, 1), "%\n")
  } else {
    cat("\nNo games played yet!\n")
  }
}

play_round <- function(q) {
  cat("\nQuestion:", q$question, "\n")
  opts <- sample(q$options[[1]])
  for(i in seq_along(opts)) {
    cat(i, "-", opts[i], "\n")
  }
  
  
  answer <- readline("Choose (1-3) or press Enter to skip: ")
  if(answer %in% c("1","2","3")) {
    scores$played <<- scores$played + 1
    if(opts[as.numeric(answer)] == q$correct) {
      cat("Correct!\n")
      scores$correct <<- scores$correct + 1
    } else {
      cat("Wrong. Correct answer:", q$correct, "\n")
    }
  } else {
    cat("Skipped!\n")
  }
}


# Main Game Loop ----------------------------------------------------------
while(TRUE) {
  cat("\n=== Bias Quiz ===")
  cat("\n1. Set Name\n2. Play\n3. Stats\n4. Quit\n")
  choice <- readline("Choose: ")
  
  if(choice == "1") {
    current_user <- readline("Enter your name: ")
    cat("Welcome", current_user, "!\n")
    
  } else if(choice == "2") {
    if(is.null(current_user)) cat("Playing as Guest\n")
    
    for(q in questions[sample(nrow(questions)), ]) {
      play_round(q)
    }
    
    cat("\nGame Complete!\n")
    
  } else if(choice == "3") {
    show_stats()
    
  } else if(choice == "4") {
    cat("\nFinal Statistics:")
    show_stats()
    cat("\nRemember: Data biases can lead to unfair outcomes.")
    cat("\nAlways check your data for representation and fairness!For more info check https://www.ibm.com/think/topics/data-bias. \n")
    cat("\nThanks for playing! Goodbye.\n")
    break
    
  } else {
    cat("Invalid choice. Try again.\n")
  }
}