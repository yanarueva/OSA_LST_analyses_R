[] doy needed for data cleaning?

[x] cooling df
    [x] full time series and 
        [x] forest lst
        [x] open land lst
        [x] cooling lst
        [x] elv
    [x] monthly 
        [x] monthly with mean across all poly_ids
        [x]monthly for each poly id

[x]climate data
    [x]precip
    [x]cooling
   ![x]solar --> Wm2 seems unrealistic, double check somehow, however relationship from Jm2 to Wm2 conversion is linear
    [x]temp
        [x] choose hottest months
            [x] hottest Tair
            [] hottest lst
            [x] hottest solar radiation
            [x] hottest min precipitation


[] stats
    [x] cooling over hottest monnths (with monthly df) El Nino vs reference years
        [x] air temp El Nino vs reference years
        [x] precip El Nino vs reference years
        [x] solar radiation El Nino vs reference years
    [] cooling over hottest LST data ?
    [] elv and cooling based on the hottest months

    [] cor for open land, forest and cooling for the hottest months

## Desk notes
[] coupling between LST and 
    - Tair
    - Precip
    is stronger in forests than in poen land? ( different than Li et al. 2015)
[]  Li et al. 2015
    -2.41 +- 0.10 C annual average
    Rueva et al 2026
    -4.76 --> warmest months
    ? --> annual average
    ?--> coldest month
    + ---> highest precip
    + ---> lowest precip

    regression for Tair and cooling, colour El Nino
    refression for precip and cooling, colour El Nino
    3D Tair vs Tlst vs precip
    
[] LST of forests is more independent than lst of open land to climate variable --> due to water infiltration and recycling capabilites
[] impact on gloabl warming on agriculture and pastures
[] I don't use clear sky cond only, which is a limitation in previous studies, as LST could be lower during clear sky (Li et al 2015)
[] UTC = local solar time - (long/15) =
    = 13:30 - (-83.2/15) = ~19:00 UTC (day) and ~07:00 UTC (night)

## Workflow


## Notes meeting with Chris 28.07.2026
[not needed] YR to write Shean and Amy for processing units
[?] forest cooling with climate variables --> should we dig into it?
[x] LST dat for night time --> actually daytime

