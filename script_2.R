df <- read.csv(file="C:/Users/petit/Dropbox/germ/English studies/2 semester/Cognitive linguistics/paper/results_survey_code.csv", header=T, sep=",")

df <- df[df$id != 37,] #Remove the Troll

names(df)

df$WHO5score <- df$WHO5.WHO51.+df$WHO5.WHO52.+df$WHO5.WHO53.+df$WHO5.WHO54.+df$WHO5.WHO55.

summary (df$WHO5score)
sd(df$WHO5score)

median_who5 <- median(df$WHO5score)
df$who5group <- FALSE
df$who5group[df$WHO5score > median_who5] <- TRUE 
sum(df$who5group)

hist(df$WHO5score, breaks=25, xlim=c(0, 25), density=20, angle=45, col="red", main = "WHO5 score")

df$relstat_rc_single <- FALSE
df$relstat_rc_single[df$RelState == 0] <- TRUE

df$gender_rc_female <- FALSE
df$gender_rc_female[df$Gender == 1] <- TRUE

#Adjective: table

df$table_new_r = -1
df$table_new_r[df$WATable1.1. == "A1"] =  1
df$table_new_r[df$WATable1.2. == "A1"] =  2
df$table_new_r[df$WATable1.3. == "A1"] =  3
df$table_new_r[df$WATable1.4. == "A1"] =  4
df$table_new_r[df$WATable1.5. == "A1"] =  5
df$table_new_r[df$WATable1.6. == "A1"] =  6
df$table_new_r[df$WATable1.7. == "A1"] =  7
df$table_new_r[df$WATable1.8. == "A1"] =  8
df$table_new_r[df$WATable1.9. == "A1"] =  9
df$table_new_r[df$WATable1.10. == "A1"] =  10
hist(df$table_new_r, breaks=10, xlim=c(1, 10), ylim=c(0, 25), density=20, angle=45, col="green", main = "'new table' score")

df$table_wooden_r = -1
df$table_wooden_r[df$WATable1.1. == "A2"] =  1
df$table_wooden_r[df$WATable1.2. == "A2"] =  2
df$table_wooden_r[df$WATable1.3. == "A2"] =  3
df$table_wooden_r[df$WATable1.4. == "A2"] =  4
df$table_wooden_r[df$WATable1.5. == "A2"] =  5
df$table_wooden_r[df$WATable1.6. == "A2"] =  6
df$table_wooden_r[df$WATable1.7. == "A2"] =  7
df$table_wooden_r[df$WATable1.8. == "A2"] =  8
df$table_wooden_r[df$WATable1.9. == "A2"] =  9
df$table_wooden_r[df$WATable1.10. == "A2"] =  10
hist(df$table_wooden_r, breaks=25, col="green")

df$table_folding_r = -1
df$table_folding_r[df$WATable1.1. == "A3"] =  1
df$table_folding_r[df$WATable1.2. == "A3"] =  2
df$table_folding_r[df$WATable1.3. == "A3"] =  3
df$table_folding_r[df$WATable1.4. == "A3"] =  4
df$table_folding_r[df$WATable1.5. == "A3"] =  5
df$table_folding_r[df$WATable1.6. == "A3"] =  6
df$table_folding_r[df$WATable1.7. == "A3"] =  7
df$table_folding_r[df$WATable1.8. == "A3"] =  8
df$table_folding_r[df$WATable1.9. == "A3"] =  9
df$table_folding_r[df$WATable1.10. == "A3"] =  10
hist(df$table_folding_r, breaks=25, ylim=c(0, 35), col="red")

df$table_black_r = -1
df$table_black_r[df$WATable1.1. == "A4"] =  1
df$table_black_r[df$WATable1.2. == "A4"] =  2
df$table_black_r[df$WATable1.3. == "A4"] =  3
df$table_black_r[df$WATable1.4. == "A4"] =  4
df$table_black_r[df$WATable1.5. == "A4"] =  5
df$table_black_r[df$WATable1.6. == "A4"] =  6
df$table_black_r[df$WATable1.7. == "A4"] =  7
df$table_black_r[df$WATable1.8. == "A4"] =  8
df$table_black_r[df$WATable1.9. == "A4"] =  9
df$table_black_r[df$WATable1.10. == "A4"] =  10
hist(df$table_black_r, breaks=25, col="blueviolet")

df$table_beautiful_r = -1
df$table_beautiful_r[df$WATable1.1. == "A5"] =  1
df$table_beautiful_r[df$WATable1.2. == "A5"] =  2
df$table_beautiful_r[df$WATable1.3. == "A5"] =  3
df$table_beautiful_r[df$WATable1.4. == "A5"] =  4
df$table_beautiful_r[df$WATable1.5. == "A5"] =  5
df$table_beautiful_r[df$WATable1.6. == "A5"] =  6
df$table_beautiful_r[df$WATable1.7. == "A5"] =  7
df$table_beautiful_r[df$WATable1.8. == "A5"] =  8
df$table_beautiful_r[df$WATable1.9. == "A5"] =  9
df$table_beautiful_r[df$WATable1.10. == "A5"] =  10
hist(df$table_beautiful_r, breaks=25, col="purple")

df$table_big_r = -1
df$table_big_r[df$WATable1.1. == "A6"] =  1
df$table_big_r[df$WATable1.2. == "A6"] =  2
df$table_big_r[df$WATable1.3. == "A6"] =  3
df$table_big_r[df$WATable1.4. == "A6"] =  4
df$table_big_r[df$WATable1.5. == "A6"] =  5
df$table_big_r[df$WATable1.6. == "A6"] =  6
df$table_big_r[df$WATable1.7. == "A6"] =  7
df$table_big_r[df$WATable1.8. == "A6"] =  8
df$table_big_r[df$WATable1.9. == "A6"] =  9
df$table_big_r[df$WATable1.10. == "A6"] =  10
hist(df$table_big_r, breaks=25, col="orange")

df$table_empty_r = -1
df$table_empty_r[df$WATable1.1. == "A7"] =  1
df$table_empty_r[df$WATable1.2. == "A7"] =  2
df$table_empty_r[df$WATable1.3. == "A7"] =  3
df$table_empty_r[df$WATable1.4. == "A7"] =  4
df$table_empty_r[df$WATable1.5. == "A7"] =  5
df$table_empty_r[df$WATable1.6. == "A7"] =  6
df$table_empty_r[df$WATable1.7. == "A7"] =  7
df$table_empty_r[df$WATable1.8. == "A7"] =  8
df$table_empty_r[df$WATable1.9. == "A7"] =  9
df$table_empty_r[df$WATable1.10. == "A7"] =  10
hist(df$table_empty_r, breaks=25, col="blue")

df$table_tall_r = -1
df$table_tall_r[df$WATable1.1. == "A8"] =  1
df$table_tall_r[df$WATable1.2. == "A8"] =  2
df$table_tall_r[df$WATable1.3. == "A8"] =  3
df$table_tall_r[df$WATable1.4. == "A8"] =  4
df$table_tall_r[df$WATable1.5. == "A8"] =  5
df$table_tall_r[df$WATable1.6. == "A8"] =  6
df$table_tall_r[df$WATable1.7. == "A8"] =  7
df$table_tall_r[df$WATable1.8. == "A8"] =  8
df$table_tall_r[df$WATable1.9. == "A8"] =  9
df$table_tall_r[df$WATable1.10. == "A8"] =  10
hist(df$table_tall_r, breaks=25, col="grey")

df$table_round_r = -1
df$table_round_r[df$WATable1.1. == "A9"] =  1
df$table_round_r[df$WATable1.2. == "A9"] =  2
df$table_round_r[df$WATable1.3. == "A9"] =  3
df$table_round_r[df$WATable1.4. == "A9"] =  4
df$table_round_r[df$WATable1.5. == "A9"] =  5
df$table_round_r[df$WATable1.6. == "A9"] =  6
df$table_round_r[df$WATable1.7. == "A9"] =  7
df$table_round_r[df$WATable1.8. == "A9"] =  8
df$table_round_r[df$WATable1.9. == "A9"] =  9
df$table_round_r[df$WATable1.10. == "A9"] =  10
hist(df$table_round_r, breaks=25, col="yellow")

df$table_spacious_r = -1
df$table_spacious_r[df$WATable1.1. == "A10"] =  1
df$table_spacious_r[df$WATable1.2. == "A10"] =  2
df$table_spacious_r[df$WATable1.3. == "A10"] =  3
df$table_spacious_r[df$WATable1.4. == "A10"] =  4
df$table_spacious_r[df$WATable1.5. == "A10"] =  5
df$table_spacious_r[df$WATable1.6. == "A10"] =  6
df$table_spacious_r[df$WATable1.7. == "A10"] =  7
df$table_spacious_r[df$WATable1.8. == "A10"] =  8
df$table_spacious_r[df$WATable1.9. == "A10"] =  9
df$table_spacious_r[df$WATable1.10. == "A10"] =  10
hist(df$table_spacious_r, breaks=25, col="aquamarine")

#Adjectives: breakup

df$breakup_tough_r = -1
df$breakup_tough_r[df$WABreakup.1. == "A1"] =  1
df$breakup_tough_r[df$WABreakup.2. == "A1"] =  2
df$breakup_tough_r[df$WABreakup.3. == "A1"] =  3
df$breakup_tough_r[df$WABreakup.4. == "A1"] =  4
df$breakup_tough_r[df$WABreakup.5. == "A1"] =  5
df$breakup_tough_r[df$WABreakup.6. == "A1"] =  6
df$breakup_tough_r[df$WABreakup.7. == "A1"] =  7
df$breakup_tough_r[df$WABreakup.8. == "A1"] =  8
df$breakup_tough_r[df$WABreakup.9. == "A1"] =  9
df$breakup_tough_r[df$WABreakup.10. == "A1"] =  10
hist(df$breakup_tough_r, breaks=10, xlim=c(1, 10), ylim=c(0, 40), density=20, angle=45, main = "'tough breakup' score",  col="blue")

df$breakup_sudden_r = -1
df$breakup_sudden_r[df$WABreakup.1. == "A2"] =  1
df$breakup_sudden_r[df$WABreakup.2. == "A2"] =  2
df$breakup_sudden_r[df$WABreakup.3. == "A2"] =  3
df$breakup_sudden_r[df$WABreakup.4. == "A2"] =  4
df$breakup_sudden_r[df$WABreakup.5. == "A2"] =  5
df$breakup_sudden_r[df$WABreakup.6. == "A2"] =  6
df$breakup_sudden_r[df$WABreakup.7. == "A2"] =  7
df$breakup_sudden_r[df$WABreakup.8. == "A2"] =  8
df$breakup_sudden_r[df$WABreakup.9. == "A2"] =  9
df$breakup_sudden_r[df$WABreakup.10. == "A2"] =  10
hist(df$breakup_sudden_r, breaks=25, col="seagreen")

df$breakup_painful_r = -1
df$breakup_painful_r[df$WABreakup.1. == "A3"] =  1
df$breakup_painful_r[df$WABreakup.2. == "A3"] =  2
df$breakup_painful_r[df$WABreakup.3. == "A3"] =  3
df$breakup_painful_r[df$WABreakup.4. == "A3"] =  4
df$breakup_painful_r[df$WABreakup.5. == "A3"] =  5
df$breakup_painful_r[df$WABreakup.6. == "A3"] =  6
df$breakup_painful_r[df$WABreakup.7. == "A3"] =  7
df$breakup_painful_r[df$WABreakup.8. == "A3"] =  8
df$breakup_painful_r[df$WABreakup.9. == "A3"] =  9
df$breakup_painful_r[df$WABreakup.10. == "A3"] =  10
hist(df$breakup_painful_r, breaks=25, ylim=c(0, 50), col="coral1")

df$breakup_crazy_r = -1
df$breakup_crazy_r[df$WABreakup.1. == "A4"] =  1
df$breakup_crazy_r[df$WABreakup.2. == "A4"] =  2
df$breakup_crazy_r[df$WABreakup.3. == "A4"] =  3
df$breakup_crazy_r[df$WABreakup.4. == "A4"] =  4
df$breakup_crazy_r[df$WABreakup.5. == "A4"] =  5
df$breakup_crazy_r[df$WABreakup.6. == "A4"] =  6
df$breakup_crazy_r[df$WABreakup.7. == "A4"] =  7
df$breakup_crazy_r[df$WABreakup.8. == "A4"] =  8
df$breakup_crazy_r[df$WABreakup.9. == "A4"] =  9
df$breakup_crazy_r[df$WABreakup.10. == "A4"] =  10
hist(df$breakup_crazy_r, breaks=25, ylim=c(0, 35), col="deeppink1")

df$breakup_complicated_r = -1
df$breakup_complicated_r[df$WABreakup.1. == "A5"] =  1
df$breakup_complicated_r[df$WABreakup.2. == "A5"] =  2
df$breakup_complicated_r[df$WABreakup.3. == "A5"] =  3
df$breakup_complicated_r[df$WABreakup.4. == "A5"] =  4
df$breakup_complicated_r[df$WABreakup.5. == "A5"] =  5
df$breakup_complicated_r[df$WABreakup.6. == "A5"] =  6
df$breakup_complicated_r[df$WABreakup.7. == "A5"] =  7
df$breakup_complicated_r[df$WABreakup.8. == "A5"] =  8
df$breakup_complicated_r[df$WABreakup.9. == "A5"] =  9
df$breakup_complicated_r[df$WABreakup.10. == "A5"] =  10
hist(df$breakup_complicated_r, breaks=25, ylim=c(0, 30), col="darkorchid1")

df$breakup_loud_r = -1
df$breakup_loud_r[df$WABreakup.1. == "A6"] =  1
df$breakup_loud_r[df$WABreakup.2. == "A6"] =  2
df$breakup_loud_r[df$WABreakup.3. == "A6"] =  3
df$breakup_loud_r[df$WABreakup.4. == "A6"] =  4
df$breakup_loud_r[df$WABreakup.5. == "A6"] =  5
df$breakup_loud_r[df$WABreakup.6. == "A6"] =  6
df$breakup_loud_r[df$WABreakup.7. == "A6"] =  7
df$breakup_loud_r[df$WABreakup.8. == "A6"] =  8
df$breakup_loud_r[df$WABreakup.9. == "A6"] =  9
df$breakup_loud_r[df$WABreakup.10. == "A6"] =  10
hist(df$breakup_loud_r, breaks=25, col="tomato1")

df$breakup_final_r = -1
df$breakup_final_r[df$WABreakup.1. == "A7"] =  1
df$breakup_final_r[df$WABreakup.2. == "A7"] =  2
df$breakup_final_r[df$WABreakup.3. == "A7"] =  3
df$breakup_final_r[df$WABreakup.4. == "A7"] =  4
df$breakup_final_r[df$WABreakup.5. == "A7"] =  5
df$breakup_final_r[df$WABreakup.6. == "A7"] =  6
df$breakup_final_r[df$WABreakup.7. == "A7"] =  7
df$breakup_final_r[df$WABreakup.8. == "A7"] =  8
df$breakup_final_r[df$WABreakup.9. == "A7"] =  9
df$breakup_final_r[df$WABreakup.10. == "A7"] =  10
hist(df$breakup_final_r, breaks=25, ylim=c(0, 20), col="forestgreen")

df$breakup_blue_r = -1
df$breakup_blue_r[df$WABreakup.1. == "A8"] =  1
df$breakup_blue_r[df$WABreakup.2. == "A8"] =  2
df$breakup_blue_r[df$WABreakup.3. == "A8"] =  3
df$breakup_blue_r[df$WABreakup.4. == "A8"] =  4
df$breakup_blue_r[df$WABreakup.5. == "A8"] =  5
df$breakup_blue_r[df$WABreakup.6. == "A8"] =  6
df$breakup_blue_r[df$WABreakup.7. == "A8"] =  7
df$breakup_blue_r[df$WABreakup.8. == "A8"] =  8
df$breakup_blue_r[df$WABreakup.9. == "A8"] =  9
df$breakup_blue_r[df$WABreakup.10. == "A8"] =  10
hist(df$breakup_blue_r, breaks=25, col="salmon")

df$breakup_free_r = -1
df$breakup_free_r[df$WABreakup.1. == "A9"] =  1
df$breakup_free_r[df$WABreakup.2. == "A9"] =  2
df$breakup_free_r[df$WABreakup.3. == "A9"] =  3
df$breakup_free_r[df$WABreakup.4. == "A9"] =  4
df$breakup_free_r[df$WABreakup.5. == "A9"] =  5
df$breakup_free_r[df$WABreakup.6. == "A9"] =  6
df$breakup_free_r[df$WABreakup.7. == "A9"] =  7
df$breakup_free_r[df$WABreakup.8. == "A9"] =  8
df$breakup_free_r[df$WABreakup.9. == "A9"] =  9
df$breakup_free_r[df$WABreakup.10. == "A9"] =  10
hist(df$breakup_free_r, breaks=25, col="springgreen3")

df$breakup_lonely_r = -1
df$breakup_lonely_r[df$WABreakup.1. == "A10"] =  1
df$breakup_lonely_r[df$WABreakup.2. == "A10"] =  2
df$breakup_lonely_r[df$WABreakup.3. == "A10"] =  3
df$breakup_lonely_r[df$WABreakup.4. == "A10"] =  4
df$breakup_lonely_r[df$WABreakup.5. == "A10"] =  5
df$breakup_lonely_r[df$WABreakup.6. == "A10"] =  6
df$breakup_lonely_r[df$WABreakup.7. == "A10"] =  7
df$breakup_lonely_r[df$WABreakup.8. == "A10"] =  8
df$breakup_lonely_r[df$WABreakup.9. == "A10"] =  9
df$breakup_lonely_r[df$WABreakup.10. == "A10"] =  10
hist(df$breakup_lonely_r, breaks=25, col="chartreuse2")

#Adjectives: party

df$party_boring_r = -1
df$party_boring_r[df$WAParty.1. == "A1"] =  1
df$party_boring_r[df$WAParty.2. == "A1"] =  2
df$party_boring_r[df$WAParty.3. == "A1"] =  3
df$party_boring_r[df$WAParty.4. == "A1"] =  4
df$party_boring_r[df$WAParty.5. == "A1"] =  5
df$party_boring_r[df$WAParty.6. == "A1"] =  6
df$party_boring_r[df$WAParty.7. == "A1"] =  7
df$party_boring_r[df$WAParty.8. == "A1"] =  8
df$party_boring_r[df$WAParty.9. == "A1"] =  9
df$party_boring_r[df$WAParty.10. == "A1"] =  10
hist(df$party_boring_r, breaks=10, xlim=c(1, 10), ylim=c(0, 50), density=20, angle=45, col="orange", main = "'boring party' score")

df$party_awesome_r = -1
df$party_awesome_r[df$WAParty.1. == "A2"] =  1
df$party_awesome_r[df$WAParty.2. == "A2"] =  2
df$party_awesome_r[df$WAParty.3. == "A2"] =  3
df$party_awesome_r[df$WAParty.4. == "A2"] =  4
df$party_awesome_r[df$WAParty.5. == "A2"] =  5
df$party_awesome_r[df$WAParty.6. == "A2"] =  6
df$party_awesome_r[df$WAParty.7. == "A2"] =  7
df$party_awesome_r[df$WAParty.8. == "A2"] =  8
df$party_awesome_r[df$WAParty.9. == "A2"] =  9
df$party_awesome_r[df$WAParty.10. == "A2"] =  10
hist(df$party_awesome_r, breaks=25, col="mediumseagreen")

df$party_weird_r = -1
df$party_weird_r[df$WAParty.1. == "A3"] =  1
df$party_weird_r[df$WAParty.2. == "A3"] =  2
df$party_weird_r[df$WAParty.3. == "A3"] =  3
df$party_weird_r[df$WAParty.4. == "A3"] =  4
df$party_weird_r[df$WAParty.5. == "A3"] =  5
df$party_weird_r[df$WAParty.6. == "A3"] =  6
df$party_weird_r[df$WAParty.7. == "A3"] =  7
df$party_weird_r[df$WAParty.8. == "A3"] =  8
df$party_weird_r[df$WAParty.9. == "A3"] =  9
df$party_weird_r[df$WAParty.10. == "A3"] =  10
hist(df$party_weird_r, breaks=25, col="indianred2")

df$party_awful_r = -1
df$party_awful_r[df$WAParty.1. == "A4"] =  1
df$party_awful_r[df$WAParty.2. == "A4"] =  2
df$party_awful_r[df$WAParty.3. == "A4"] =  3
df$party_awful_r[df$WAParty.4. == "A4"] =  4
df$party_awful_r[df$WAParty.5. == "A4"] =  5
df$party_awful_r[df$WAParty.6. == "A4"] =  6
df$party_awful_r[df$WAParty.7. == "A4"] =  7
df$party_awful_r[df$WAParty.8. == "A4"] =  8
df$party_awful_r[df$WAParty.9. == "A4"] =  9
df$party_awful_r[df$WAParty.10. == "A4"] =  10
hist(df$party_awful_r, breaks=25, col="olivedrab4")

df$party_exciting_r = -1
df$party_exciting_r[df$WAParty.1. == "A5"] =  1
df$party_exciting_r[df$WAParty.2. == "A5"] =  2
df$party_exciting_r[df$WAParty.3. == "A5"] =  3
df$party_exciting_r[df$WAParty.4. == "A5"] =  4
df$party_exciting_r[df$WAParty.5. == "A5"] =  5
df$party_exciting_r[df$WAParty.6. == "A5"] =  6
df$party_exciting_r[df$WAParty.7. == "A5"] =  7
df$party_exciting_r[df$WAParty.8. == "A5"] =  8
df$party_exciting_r[df$WAParty.9. == "A5"] =  9
df$party_exciting_r[df$WAParty.10. == "A5"] =  10
hist(df$party_exciting_r, breaks=25, col="mediumslateblue")

df$party_crowded_r = -1
df$party_crowded_r[df$WAParty.1. == "A6"] =  1
df$party_crowded_r[df$WAParty.2. == "A6"] =  2
df$party_crowded_r[df$WAParty.3. == "A6"] =  3
df$party_crowded_r[df$WAParty.4. == "A6"] =  4
df$party_crowded_r[df$WAParty.5. == "A6"] =  5
df$party_crowded_r[df$WAParty.6. == "A6"] =  6
df$party_crowded_r[df$WAParty.7. == "A6"] =  7
df$party_crowded_r[df$WAParty.8. == "A6"] =  8
df$party_crowded_r[df$WAParty.9. == "A6"] =  9
df$party_crowded_r[df$WAParty.10. == "A6"] =  10
hist(df$party_crowded_r, breaks=25, col="darkgoldenrod1")

df$party_loud_r = -1
df$party_loud_r[df$WAParty.2. == "A7"] =  2
df$party_loud_r[df$WAParty.3. == "A7"] =  3
df$party_loud_r[df$WAParty.4. == "A7"] =  4
df$party_loud_r[df$WAParty.5. == "A7"] =  5
df$party_loud_r[df$WAParty.6. == "A7"] =  6
df$party_loud_r[df$WAParty.7. == "A7"] =  7
df$party_loud_r[df$WAParty.8. == "A7"] =  8
df$party_loud_r[df$WAParty.9. == "A7"] =  9
df$party_loud_r[df$WAParty.10. == "A7"] =  10
hist(df$party_loud_r, breaks=25, col="plum")

df$party_happy_r = -1
df$party_happy_r[df$WAParty.1. == "A8"] =  1
df$party_happy_r[df$WAParty.2. == "A8"] =  2
df$party_happy_r[df$WAParty.3. == "A8"] =  3
df$party_happy_r[df$WAParty.4. == "A8"] =  4
df$party_happy_r[df$WAParty.5. == "A8"] =  5
df$party_happy_r[df$WAParty.6. == "A8"] =  6
df$party_happy_r[df$WAParty.7. == "A8"] =  7
df$party_happy_r[df$WAParty.8. == "A8"] =  8
df$party_happy_r[df$WAParty.9. == "A8"] =  9
df$party_happy_r[df$WAParty.10. == "A8"] =  10
hist(df$party_happy_r, breaks=25, col="sienna1")

df$party_late_r = -1
df$party_late_r[df$WAParty.1. == "A9"] =  1
df$party_late_r[df$WAParty.2. == "A9"] =  2
df$party_late_r[df$WAParty.3. == "A9"] =  3
df$party_late_r[df$WAParty.4. == "A9"] =  4
df$party_late_r[df$WAParty.5. == "A9"] =  5
df$party_late_r[df$WAParty.6. == "A9"] =  6
df$party_late_r[df$WAParty.7. == "A9"] =  7
df$party_late_r[df$WAParty.8. == "A9"] =  8
df$party_late_r[df$WAParty.9. == "A9"] =  9
df$party_late_r[df$WAParty.10. == "A9"] =  10
hist(df$party_late_r, breaks=25, col="plum")

df$party_crazy_r = -1
df$party_crazy_r[df$WAParty.1. == "A10"] =  1
df$party_crazy_r[df$WAParty.2. == "A10"] =  2
df$party_crazy_r[df$WAParty.3. == "A10"] =  3
df$party_crazy_r[df$WAParty.4. == "A10"] =  4
df$party_crazy_r[df$WAParty.5. == "A10"] =  5
df$party_crazy_r[df$WAParty.6. == "A10"] =  6
df$party_crazy_r[df$WAParty.7. == "A10"] =  7
df$party_crazy_r[df$WAParty.8. == "A10"] =  8
df$party_crazy_r[df$WAParty.9. == "A10"] =  9
df$party_crazy_r[df$WAParty.10. == "A10"] =  10
hist(df$party_crazy_r, breaks=25, col="limegreen")

pairwise_list <- c()

##Table
for(i in 1:(nrow(df)-1))
{
  row1 <- df[i,]
  for(j in (i+1):(nrow(df)))
  {
    if(i>=j) next
    
    row2 <- df[j,]
    
    who5_difference = abs(row1$WHO5score - row2$WHO5score)
    gender_difference = abs(row1$gender_rc_female - row2$gender_rc_female)
    relstat_difference = abs(row1$relstat_rc_single - row2$relstat_rc_single)
    age_difference = abs(row1$Age - row2$Age)
    englang_difference = abs(row1$EngLang - row2$EngLang)
    influence_difference = abs(row1$EMOINF.EMOTAB. - row2$EMOINF.EMOTAB.)
    
    rankseq_1 = c(row1$table_beautiful_r, row1$table_big_r, row1$table_black_r, row1$table_empty_r, row1$table_folding_r, row1$table_new_r, row1$table_round_r, row1$table_spacious_r, row1$table_tall_r, row1$table_wooden_r)
    rankseq_2 = c(row2$table_beautiful_r, row2$table_big_r, row2$table_black_r, row2$table_empty_r, row2$table_folding_r, row2$table_new_r, row2$table_round_r, row2$table_spacious_r, row2$table_tall_r, row2$table_wooden_r)
    
    kendall_object <- Kendall::Kendall(rankseq_1, rankseq_2)
    kendall_distance <- abs(kendall_object$tau)
    
    newpair = c(kendall_distance, who5_difference, gender_difference, relstat_difference, age_difference, englang_difference, influence_difference)
    
    pairwise_list <- rbind(pairwise_list, newpair)

  }
}

y <- pairwise_list[,1]
x_who5 <- pairwise_list[,2]
x_gender <- pairwise_list[,3]
x_relstat <- pairwise_list[,4]
x_age <- pairwise_list[,5]
x_englang <- pairwise_list[,6]
x_emoinf <- pairwise_list[,7]


fit <- lm(y ~ x_who5 + x_gender + x_relstat + x_age)
summary(fit)


#Breakup
for(i in 1:(nrow(df)-1))
{
  row1 <- df[i,]
  for(j in (i+1):(nrow(df)))
  {
    if(i>=j) next
    
    row2 <- df[j,]
    
    who5_difference = abs(row1$WHO5score - row2$WHO5score)
    gender_difference = abs(row1$gender_rc_female - row2$gender_rc_female)
    relstat_difference = abs(row1$relstat_rc_single - row2$relstat_rc_single)
    age_difference = abs(row1$Age - row2$Age)
    englang_difference = abs(row1$EngLang - row2$EngLang)
    influence_difference = abs(row1$EMOINF.EMOBRE. - row2$EMOINF.EMOBRE.)
    
    rankseq_1 = c(row1$breakup_tough_r, row1$breakup_sudden_r, row1$breakup_painful_r, row1$breakup_crazy_r, row1$breakup_complicated_r, row1$breakup_loud_r, row1$breakup_final_r, row1$breakup_blue_r, row1$breakup_free_r, row1$breakup_lonely_r)
    rankseq_2 = c(row2$breakup_tough_r, row2$breakup_sudden_r, row2$breakup_painful_r, row2$breakup_crazy_r, row2$breakup_complicated_r, row2$breakup_loud_r, row2$breakup_final_r, row2$breakup_blue_r, row2$breakup_free_r, row2$breakup_lonely_r)
    kendall_object <- Kendall::Kendall(rankseq_1, rankseq_2)
    kendall_distance <- abs(kendall_object$tau)
    
    newpair = c(kendall_distance, who5_difference, gender_difference, relstat_difference, age_difference, englang_difference, influence_difference)
    
    pairwise_list <- rbind(pairwise_list, newpair)
    
  }
}

y <- pairwise_list[,1]
x_who5 <- pairwise_list[,2]
x_gender <- pairwise_list[,3]
x_relstat <- pairwise_list[,4]
x_age <- pairwise_list[,5]
x_englang <- pairwise_list[,6]
x_emoinf <- pairwise_list[,7]


fit <- lm(y ~ x_who5 + x_gender + x_relstat + x_age)
summary(fit)

#Party

for(i in 1:(nrow(df)-1))
{
  row1 <- df[i,]
  for(j in (i+1):(nrow(df)))
  {
    if(i>=j) next
    
    row2 <- df[j,]
    
    who5_difference = abs(row1$WHO5score - row2$WHO5score)
    gender_difference = abs(row1$gender_rc_female - row2$gender_rc_female)
    relstat_difference = abs(row1$relstat_rc_single - row2$relstat_rc_single)
    age_difference = abs(row1$Age - row2$Age)
    englang_difference = abs(row1$EngLang - row2$EngLang)
    influence_difference = abs(row1$EMOINF.EMOPAR. - row2$EMOINF.EMOPAR.)
    
    rankseq_1 = c(row1$party_boring_r, row1$party_awesome_r, row1$party_weird_r, row1$party_awful_r, row1$party_exciting_r, row1$party_crowded_r, row1$party_loud_r, row1$party_happy_r, row1$party_late_r, row1$party_crazy_r)
    rankseq_2 = c(row2$party_boring_r, row2$party_awesome_r, row2$party_weird_r, row2$party_awful_r, row2$party_exciting_r, row2$party_crowded_r, row2$party_loud_r, row2$party_happy_r, row2$party_late_r, row2$party_crazy_r)
    kendall_object <- Kendall::Kendall(rankseq_1, rankseq_2)
    kendall_distance <- abs(kendall_object$tau)
    
    newpair = c(kendall_distance, who5_difference, gender_difference, relstat_difference, age_difference, englang_difference, influence_difference)
    
    pairwise_list <- rbind(pairwise_list, newpair)
    
  }
}

y <- pairwise_list[,1]
x_who5 <- pairwise_list[,2]
x_gender <- pairwise_list[,3]
x_relstat <- pairwise_list[,4]
x_age <- pairwise_list[,5]
x_englang <- pairwise_list[,6]
x_emoinf <- pairwise_list[,7]


fit <- lm(y ~ x_who5 + x_gender + x_relstat + x_age)
summary(fit)