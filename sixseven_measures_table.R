library(knitr)
library(dplyr)
library(kableExtra)

measures_col_names <- c("Measure","Derived Count", "Derived Proportion")

measures <- c("Quantity","Speaker","Utterance Type", "Object Co-presence")

derived_count <- c("Noun tokens, Noun types","Nouns from Mother, Nouns from Father, Unique Speakers",
                   "Nouns in Declarative, Imperative, Question, Short-Phrase, Reading, and Singing Utterances",
                   "Nouns said when the referent was present and attended to")

derived_prop <- c("Type-token ratio","Prop. nouns by mother, Prop. nouns by father",
                  "Prop. Nouns in Declarative, Imperative, Question, Short-Phrase, Reading, and Singing Utterances",
                  "Prop. nouns said when the referent was present and attended to")

measures_table_data <- data.frame(measures,derived_count,derived_prop)

measures_table <- kable(measures_table_data, col.names = measures_col_names,
      format = "markdown", padding = 2) %>%
  kable_styling()
