# Edit data before running a Rasch analysis or calculate a D-score
#
# Dependencies
# Run first R scripts: assemble_data.R to create object `work`
# suppressWarnings(source("scripts/assemble_data.R"))

# cromoc001	gpamoc008 Clench fist
# Remove item because it identifies abnormality (Melissa 22020807)
work <- select(work, -gpamoc008)

# Responses not relevant for older children, overwrite by NA
# ddicmm030	gtolgd002	Smiles in response (M; can ask parents (6MO)
# mdtlgd002	gtolgd003	Happy vocalizing or making sounds, not crying (6MO)
# mdtlgd003	gtolgd004	Laughs/chuckles	(6M)
# denlgd007	gtolgd006	Turn to Voice (6M)
# sgrred006 gtolgd007	Vocalises when spoken to (9M)
# dmclgd002 gtolgd008 Repeats syllables in strings e.g. ma ma ma (9M)
work <- work %>%
  mutate(gtolgd002 = ifelse(age > 182, NA, gtolgd002),
         gtolgd003 = ifelse(age > 182, NA, gtolgd003),
         gtolgd004 = ifelse(age > 182, NA, gtolgd004),
         gtolgd006 = ifelse(age > 182, NA, gtolgd006),
         gtolgd007 = ifelse(age > 274, NA, gtolgd007),
         gtolgd008 = ifelse(age > 274, NA, gtolgd008),
  )
