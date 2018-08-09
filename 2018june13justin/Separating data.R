df <- read.csv("data/final_claims_NM_August4th08_10-17.csv", header = TRUE  )
final_clam <- as.data.frame(df) 

final_clam$Pain <- ifelse(final_clam$DEPENDENCY_GROUPING == "Pain",1,0)

final_clam$Abuse <- ifelse(final_clam$DEPENDENCY_GROUPING == "Abuse",1,0)

final_clam$Diagnosis <- ifelse(final_clam$DEPENDENCY_GROUPING == "Diagnosis",1,0)

