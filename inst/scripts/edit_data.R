# Edit data before running a Rasch analysis or calculate a D-score
#
# Dependencies
# Run first R scripts: assemble_data.R to create object `work`
# suppressWarnings(source("scripts/assemble_data.R"))

# cromoc001	gpamoc008 Clench fist
# Remove item because it identifies abnormality (Melissa 22020807)
work <- select(work, -gpamoc008)

# Responses not relevant for older children, overwrite by NA

## 20221201 - NOTE: Lines with ## are outdated because of incorrect gto labels
## ddicmm030 gtolgd002	Smiles in response (M; can ask parents (6MO)
## mdtlgd002 gtolgd003	Happy vocalizing or making sounds, not crying (6MO)
## mdtlgd003 gtolgd004	Laughs/chuckles	(6M)
## denlgd007 gtolgd006	Turn to Voice (6M)
## sgrred006 gtolgd007	Vocalises when spoken to (9M)
## dmclgd002 gtolgd008  Repeats syllables in strings e.g. ma ma ma (9M)

# 20221201 Correct labels
# gtolgd002	13,22	B2. Smiles in response
# gtolgd003	 5,33 B3. Calms and quiets with caregivers
# gtolgd004	19,12	B4. Happy vocalizing or making sounds
# gtolgd006	24,62	B6. Laughs
# gtolgd007	23,47	B7. Vocalises when spoken to
# gtolgd008	35,25	B8. Repeats syllables

work <- work %>%
  mutate(gtolgd002 = ifelse(age > 182, NA, gtolgd002),
         gtolgd003 = ifelse(age > 182, NA, gtolgd003),
         gtolgd004 = ifelse(age > 182, NA, gtolgd004),
         gtolgd006 = ifelse(age > 182, NA, gtolgd006),
         gtolgd007 = ifelse(age > 274, NA, gtolgd007),
         gtolgd008 = ifelse(age > 274, NA, gtolgd008),
  )

