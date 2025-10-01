#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# condition_codes.R
# June 2023
# Bella Tortora Brayda 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Heat related conditions codes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

alzheimers <- '^G30' # any codes that START with G30 (that is what the ^ symbol does)
dehydration <- '^E86|^X54' # any codes that start with E86 OR start with X54
cardiovascular <- '^I0[0-42]|^I5[0-1]|^I6[0-99]' # any codes that start with I0 (and are then followed by a number between 0 and 42) etc. etc.
dementia <- '^F01|^F03'
drowning <- '^W6[7-9]|^W7[0-4]'
falls <- '^W0[0-4]|^W09|^W1[0-9]'
hot_weather <- '^T67[0-9]|^X30|X32'
injuries <- '^S\\d{2}|^T0[0-9]|^T1[0-4]'
mental_health <- '^F[10-63]|^F[67-89]|^F99'
natural_forces <- '^T750|^X[33-34]|^X3[6-9]'
parkinson <- '^G70'
renal <- '^N[0-3][0-9]'
respiratory <- '^J[00-22]|^J30|^J39|^J[40-84]|^J[96-99]'
road_incidents <- '^V[0-8][0-9]'
suicide_selfharm <- '^X[60-84]|^Y[10-34]'
violence <- '^X[85-99]|^Y0[0-9]|^U50.9'


all <- paste0(c(alzheimers,dehydration,cardiovascular,dementia,drowning,falls,hot_weather,
                injuries,mental_health,natural_forces,parkinson,renal,respiratory,
                road_incidents,suicide_selfharm,violence), collapse = "|")

