df <- read.csv("data/final_claims_NM_August4th08_10-17.csv", header = TRUE  )
final_claim <- as.data.frame(df) 

final_claim$PainandAbuseDia <-
  ifelse(
    final_claim$DEPENDENCY_GROUPING == "Pain" |
      final_claim$DEPENDENCY_GROUPING == "Abuse",
    "PainandAbuse",
    as.character(final_claim$DEPENDENCY_GROUPING)
  )

final_claim_new <- final_claim[,-4]


write.csv(final_claim_new, "./data/final_claim_parsed.csv")
