# Kittiwake Re-pairing README
Associated with the paper: Links between personality, reproductive success and re-pairing patterns in a long-lived seabird 

GENERAL INFORMATION: 

Dataset description: Data files relating to the analyses of the paper 'Links between personality, reproductive success and re-pairing patterns in a long-lived seabird'. 

Current use: To test the effect of pair members' individual and interacting boldness on reproductive success and re-pairing probability in black-legged kittiwakes.

Geographic location of data collection: Svalbard, 78° 41' 53.1780'' N and 15° 43' 25.3812'' E.

Information about funding sources that supported the collection of the data: The long-term data collection used in this study was funded by programmes MOSJ (https://mosj.no/) and SEAPOP (https://seapop.no/). We are grateful for funding provided by NERC (grant numbers NE/S00713X/1 for FMcC and FMck and NE/L002450/1 for SH) and the Research Council of Norway’s Arctic Field Grant (310627, Research in Svalbard (RiS) ID: 11366 for FMcC, and 333121, RiS ID: 11820 for FMck). 

DATA & FILE OVERVIEW

File List: 
"absol_hist.csv" is the dataset used to test for assortative mating in kittiwakes (model 1). 

"all_kitt_data3.csv" is the dataset used to investigate the links between personality, reproductive success and re-pairing in the kittiwakes (models 2, 3 and 5). 

"next_season_repro.csv" is the dataset used for investigating the effect of re-pairing on future reproduction outcomes (model 4).

"pers_scores_with_colony.csv" and "absol_random.csv" are used to conduct the randomisations to test if re-pairing birds choose partners that are more similar to themselves than their previous mates. 

The r script for the cleaning and analysis of the data (R Markdown re-pairing script.R)

The r markdown file used to generate the pdf guide (R Markdown re-pairing Paper.Rmd)

The r markdown word.doc for the cleaning and analysis of the data (R-Markdown-re-pairing-Paper.docx)

METHODOLOGICAL INFORMATION

Description of methods used for collection/generation of data: two colonies of kittiwakes have been monitored over multple years. Data from 'Grumant' was collected between 2009-18 and data from pyramiden were collected in 2018, 2019, 2021 and 2022.

Identity of pair members were recorded each year using long-term ringing data. Relationship outcome the year following each breeding attempt was recorded.

Boldness: Responses of  birds to novel object were quantified using an ordinal scale (a) sitting on the nest, with the body resting on the nest cup; (b) body raised off nest cup, but not standing; (c) standing on the nest (legs visible and extending to the base of the nest); (d) off the nest but remaining visible on the ledge close to the nest; and (e) off the ledge (and no longer visible). these were condensed into a single score per bird using Princple Component Analysis. Higher scores = bolder birds. Observation number, observer identity, and bird ID were fitted as fixed effects in a generalized linear model. Individual parameter estimates were mean-centred at the population level (see Patrick et al., 2013 for a detailed description). This created a boldness score of each individual.

Instrument- or software-specific information needed to interpret the data: Data processed in R, packages used: dplyr, ggplot2, lme4, MuMIn, sjPlot, car, knitr

DATA-SPECIFIC INFORMATION FOR: 

"absol_hist.csv" is the dataset used to test for assortative mating in kittiwakes (model 1). 

 male_ring: male bird unique ID
 
 female_ring.x: female bird unique ID
 
 male_pers.x: male bird pers score
 
 female_pers.x: female bird pers score
 
 colony: bird location either Pyramiden or Grumant
 
 
 "all_kitt_data3.csv" is the dataset used to investigate the links between personality, reproductive success and re-pairing in the kittiwakes (models 2, 3 and 5). 
 
  male_ring: male bird unique ID
  
 female_ring: female bird unique ID
 
 colony: bird location either Pyramiden or Grumant
 
 pair_id: unique pair ID
 
 cycle_pair: unique breeding attempt ID
 
 year: year of breeding attempt (year t)

 chick_sur: outcome of breeding attempt success1/0fail in year t
 
 season_outcome: partnership outcome in year t+1
 
 ulti_outcome: re-pairing outcome in year t+n
 
 missed_seasons: number of seasons since birds were recorded last
 
 male_pers: male bird pers score
 
 female_pers: female bird pers score
 
 absol: absolute difference in boldnesss score between pair members
 

 "next_season_repro.csv" is the dataset used for investigating the effect of re-pairing on future reproduction outcomes (model 4).
 
 ring: bird unique ID
 
 sex: Male M/ Female F
 
 colony: bird location either Pyramiden or Grumant
 
 pair_id: unique pair ID
 
 cycle_pair: unique breeding attempt ID
 
 year_t: year of breeding attempt (year t)
 
 season_outcome: re-pairing outcome in year t+1
 
 next_season: reproductive outcome in year t + 1
 
 "pers_scores_with_colony.csv" and "absol_random.csv" are used to conduct the randomisations to test if re-pairing birds choose partners that are more similar to themselves than their previous mates. 
 
 "pers_scores_with_colony.csv"
 
 colony: bird location either Pyramiden or Grumant
 
 pers_scores: bird's boldness score
 
 "absol_random.csv"
 
 focal_ring: re-pairing bird's unique ID
 
 colony.x: bird location either Pyramiden or Grumant
 
 orig_absol: original partnership's absolute difference in boldness score
 
 new_absol: new partnership's absolute difference in boldness score
 
 trueDiff: true difference between original and new absolute difference scores
 
 F_pers: focal/ re-pairing bird's boldness score
 
 orig_part_pers: original partner's boldness score
 
 new_part_pers: new partner's boldness score
