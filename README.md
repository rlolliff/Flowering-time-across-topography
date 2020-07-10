# Flowering-time-across-topography
 Data files and scripts related to the publication "Topographic heterogeneity lengthens the duration of pollinator resources" Authors Olliff-Yang RL*, DD Ackerly *Corresponding author: rlolliff@berkeley.edu

This readme.txt file was generated on 2020-07-09 by Rachael L. Olliff-Yang

Author information: 

	A. Principal Investigator Contact Information
		Name: Rachael L. Olliff-Yang
		Institution: University of California Berkeley
		Address: 3040 Valley Life Sciences Building #3140
		Email: rlolliff@berkeley.edu

	B. Co-investigator Contact Information
		Name: David D. Ackerly
		Institution: University of California Berkeley 
		Email: dackerly@berkeley.edu


Years of data collection: 2015-2018

Geographic location of data collection: Pepperwood Preserve (Sonoma County, California, USA 38.57˚ N, -122.68˚ W) 

Funding sources that supported the collection of the data: 

Funding was provided by a Myrtle Wolf Scholarship (East Bay CNPS), a Natalie Hopkins Award (CNPS), a Mildred E. Mathias Research grant (UC NRS), and a UC Berkeley Integrative Biology graduate student research grant. Additional support was provided by the National Science Foundation Graduate Research Fellowship Grant (#1049702) (to R.L.O.-Y.). 

Data are archived and publicly available on Dryad, DOI https://doi.org/10.6078/D1KX30

DATA & FILE OVERVIEW

1.	File List: 

BechtelMET_2014-2018.csv
MoistureData.csv
GrasslandPlotTemps2015_2018.csv 
AvMonthTemp_Full.csv
PollResDates5.csv
SPECIESPollResDates.csv
LASPollResDates.csv
SpInfo.csv


Relationship between files: 

PollResDates5.csv, SPECIESPollResDates.csv, LASPollResDates.csv
Were all calculated from SpInfo.csv raw phenology data. 

AvMonthTemp_Full.csv was calculated from GrasslandPlotTemps2015_2018.csv 
plot temperature data. 

Scripts available at https://github.com/rlolliff/Flowering-time-across-topography


METHODOLOGICAL INFORMATION

Description of methods used for collection/generation of data:
GrasslandPlotTemps2015_2018.csv
Ibutton temperature data collected at Pepperwood Preserve in 2015-2018. Paired grassland sites on North and South facing slopes. Temperatures were collected once per hour at 10cm below soil surface.
Temperature and moisture were recorded at the plots to quantify microsite differences between aspects. Temperature was recorded at each aspect with an iButton (Thermochron, N=8) placed 10 cm below the soil surface and set to record every hour. Temperature measurements were taken from March 1st through June 30th in all four years to compare aspect temperature differences during the flowering season. Additionally, temperature measurements were collected throughout the year in 2016 to capture full growing degree day accumulation curves.

Phenology data collection methods: 
(for SpInfo.csv, PollResDates5.csv,  SPECIESPollResDates.csv, LASPollResDates.csv)
In each plot, we recorded flowering throughout the spring growing season (March-June) in 2015, 2016, 2017 and 2018. Flowering phenology was observed for all pollinator resource species in the plots, including native and non-native species, annuals and perennials. Species status as a pollinator resource was identified by direct observations of animal visitation during the study, together with outside sources, including information provided by the Xerces society (Mader et al. 2011). Richness of pollinator resource species in each plot varied from 1 to 22 species over the entire season. Inflorescences in flower for each species in each plot were counted weekly to determine start, middle, and end flowering, as well as the length of the flowering season. For species with inflorescences that had more than one phenology stage present, an inflorescence was counted as flowering when at least 50% of it was in flower. Not all species were present in all sites, or on both slopes.
Mader E, Shepherd M, Vaughan M, Hoffman Black S, LeBuhn. G (2011) The Xerces Society Guide to Attracting Native Pollinators: Protecting North America’s Bees and Butterflies. Storey Publishing, North Adams MA.


Methods for data processing: 
PollResDates5.csv
Raw flowering time data was collected as the number of inflorescences in flower for each species on each census date. Data was filtered to species that were determined to be pollinator resources. 
Phenology date columns calculation: The number of inflorescences on each census date in each plot was divided by the total number of inflorescences counted in the plot for the full season to determine proportional flowering on each date. This proportion was then summed to create a cumulative sum.
            Community flowering dates for pollinator resource species were calculated based on cumulative plot flowering over the season, as follows: start date as the date when 5% of the cumulative number of flowers in a plot (summed over the season) had been reached, mid-flowering date as the date when 50% flowering was reached, and end date as the date when 95% of flowering had been reached. Flowering duration was defined as the total number of days between start and end dates (when 5% and 95% flowering had been reached, respectively) for each plot.
SPECIESPollResDates.csv
Raw flowering time data was collected as the number of inflorescences in flower for each species on each census date. Data was filtered to species that were determined to be pollinator resources.
Phenology date columns calculation: The number of inflorescences for each species on each census date in each plot was divided by the total number of inflorescences for that species counted in the plot for the full season to determine proportional flowering on each date. This proportion was then summed to create a cumulative sum for each species.
Species flowering dates were defined as: start date as the date when 5% of the cumulative number of flowers in a plot (summed over the season) had been reached, mid-flowering date as the date when 50% flowering was reached, and end date as the date when 95% of flowering had been reached for each species. When flowering was only observed for a species on one survey date, the duration of flowering was calculated as 1 day (although it is likely that flowering occurred for two days or longer depending on the species). There were instances of gaps between flowering time on north and south aspects for individual species, and these were removed when calculating site flowering duration.
LASPollResDates.csv
Experimental plots of goldfields (Lasthenia gracilis (DC.) Greene) were set up just outside of main phenology plots in 2017 and 2018, with 3 subplots per aspect at each site (n=24). These 30x30 cm subplots were planted with 30 seeds each, collected from two grassland locations on Pepperwood Preserve from 10 maternal lines (3 seeds per line per plot). Seeds were planted in the fall and marked with toothpicks to differentiate them from any other Lasthenia individuals occurring at the site. Flowering time was recorded for these subplots in the same way as the study phenology plots – with all open inflorescences from all individuals counted each week from March through June. Counts were conducted only on planted and marked Lasthenia individuals within each plot.

Flowering dates were defined as: start date as the date when 5% of the cumulative number of flowers in a plot (summed over the season) had been reached, mid-flowering date as the date when 50% flowering was reached, and end date as the date when 95% of flowering had been reached for each experimental plot. 







DATA-SPECIFIC INFORMATION FOR: BechtelMET_2014-2018.csv
Number of variables: 10
Number of cases/rows: 175053    
Variable List: 
“timestamp” - mm/dd/yyyy hh:mm
“DOY” = day of year (# of days since Jan1st)
“WDOY” – day of water year (#days since October 1st)
“YEAR” – year of collection
“the records” = record # (for reference)
“airTemp” = degrees C
“rainHun15min”  = Number of tips       1tip =0.01" of precip
“rainInchYear”  = Cummulative sum of precip for water year beginning OCT 1  (inches)
“rainInch15min” = Number of tips / 100 (inches)

Bechtel weather station and rain gauge data collected every 15 minutes. 

LIST OF OMMITTED DATA








TIMESTAMP START
TIMESTAMP END
FIELD
REASON
12/9/15 16:15
2/2/16 17:15
AirTC_Avg
 Sensor malfunction and removal for calibration
12/9/15 16:15
2/2/16 17:15
RH_Avg
 Sensor malfunction and removal for calibration. Bad values (over 100 preced this period, RH was out of calibration)
10/9/17 4:45
1/5/18 10:30
TE 525 15min Rain, ("rainHun15min", "raininchYear", "rainInch15min", "rainInch15minForm")
Tubbs Fire knocked over rain gauge, not reinstalled until after recalibration. Nearby gauge recorded 8.6 inches during this time
12/5/17 7:15
1/5/18 10:30
All
Sensors sent in for recalibrations.  
1/8/18 11:00
1/8/18 11:00
barometricPressure dropped out 
unknown but fell from ~750mmHg to 20-50mmHg for one 15minute average
4/13/18 11:00
4/13/18 11:00
barometricPressure dropped out 
unknown but fell from ~750mmHg to 20-50mmHg for one 15minute average
6/11/18 8:00
6/11/18 8:00
barometricPressure dropped out 
unknown but fell from ~750mmHg to 20-50mmHg for one 15minute average
8/14/18 9:30
8/14/18 9:30
barometricPressure dropped out 
Logger powered off as hard reboot to reestablish internet connection to DC. -RF






DATA-SPECIFIC INFORMATION FOR: MoistureData.csv
Number of variables: 8

Number of cases/rows: 163

Variable List: 
“DATE” – Date of soil moisture measurement; mm/dd/yy
“DOY” – day of year (# of days since Jan1st)
“YEAR” – year of collection
“PLOT”  - indicates site (1st 2 letters), aspect (3rd letter), and replicate (1, 2, or 3). 
“SITE” – Location of hill with paired aspects (BH = Bechtel House, DP= Double ponds, FH=Hill 1521, TP = Turtle ponds, TH = Three Tree Hill. 
“ASPECT” -  aspect of plot (N = North, S= South)
“PERCENTVWC” –Volumetric Water Content (%)
“Microsiemens” – Electrical conductivity (µS)


DATA-SPECIFIC INFORMATION FOR: GrasslandPlotTemps2015_2018.csv 

Number of variables: 9 
Number of cases/rows: 155780
Variable List: 
“PLOT”  - indicates site (1st 2 letters) and aspect (3rd letter) where temperature data was collected. This does not correspond to individual replicate “plots” from related data in this repository, only to the site and aspect (since temperature data was taken from a central point between the three plots.)
“DateTime” – date and time of temperature measurement;  mm/dd/yyyy hh:mm
“DOY” – day of year (# of days since Jan1st)
“WDOY” – day of water year (#days since October 1st)
“YEAR” – year of collection
“WYEAR” – water year of collection (October-September)
“TEMP” – temperature measurement (˚C)
“SerialNo” – Serial number of ibutton for testing accuracy and comparison 
“DataType” – Plot (temperature taken at plot), Test (temperatures taken before/after placement in plot, in same location as other ibuttons for testing serial number differences in temperature readings), PlotBAD (ibutton data taken at plot but disturbed/displaced/unearthed by animal activity)


Ibutton temperature data collected at Pepperwood Preserve in 2015-2018. Paired grassland sites on North and South facing slopes. Temperatures were collected once per hour at 10cm below soil surface. 


DATA-SPECIFIC INFORMATION FOR: AvMonthTemp_Full

Number of variables: 17
Number of cases/rows: 
Variable List: 
“YEAR” – year of data collection (2015, 2016, 2017, 2018)
“PLOT”  - indicates site (1st 2 letters) and aspect (3rd letter) where temperature data was collected. This does not correspond to individual replicate “plots” from related data in this repository, only to the site and aspect (since temperature data was taken from a central point between the three plots.)
“TempJan” – average temperature (˚C) 10cm below soil surface in January 
“TempFeb”– average temperature (˚C) 10cm below soil surface in February
“TempMar”– average temperature (˚C) 10cm below soil surface in March
“ExtrMarch” – extrapolated March temperature data (˚C)  for plots with missing data. Calculated as the average march temperature at that plot in other years and adjusted by the average difference in march air temperature data. 
“TempApril”– average temperature (˚C) 10cm below soil surface in April
“ExtrApril “– extrapolated April temperature data (˚C)  for plots with missing data. Calculated as the average April temperature at that plot in other years and adjusted by the average difference in April air temperature data. 
“TempMay”– average temperature (˚C) 10cm below soil surface in May
“ExtrMay” – extrapolated May temperature data (˚C)  for plots with missing data. Calculated as the average May temperature at that plot in other years and adjusted by the average difference in May air temperature data. 
“TempJun”– average temperature (˚C) 10cm below soil surface in June
“TempJul”– average temperature (˚C) 10cm below soil surface in July
“TempAug”– average temperature (˚C) 10cm below soil surface in August
“TempSep”– average temperature (˚C) 10cm below soil surface in Septemeber
“TempOct”– average temperature (˚C) 10cm below soil surface in October
“TempNov”– average temperature (˚C) 10cm below soil surface in November
“TempDec”– average temperature (˚C) 10cm below soil surface in December

Data was averaged from hourly measurements, catalogued in GrasslandPlotTemps2015_2018.csv



DATA-SPECIFIC INFORMATION FOR: PollResDates5.csv
Number of variables: 8
Number of cases/rows: 97
Variable List: 
“YEAR” – year of data collection (2015, 2016, 2017, 2018)
“ASPECT” -  aspect of plot (N = North, S= South)
“SITE” – Location of hill with paired aspects (BH = Bechtel House, DP= Double ponds, FH=Hill 1521, TP = Turtle ponds, TH = Three Tree Hill). TH was only included in 2016 & 2017 and was not included in ANOVA analyses. 
“PLOT” – Six per site, Three plots on each aspect. 1st two letters (e.g. BH) indicate site, 3rd (e.g. N) indicates aspect, Number at end (1, 2, or 3) indicates replicate. 
“Start” day of year of 5% flowering. 
“Mid” or “Peak” day of year of 50% flowering,
 “End” the day of year of 95% flowering. 
“length” = number of days from “start” (5% flowering) to “end” (95% flowering) dates

Raw flowering time data was collected as the number of inflorescences in flower for each species on each census date. Data for this paper was filtered to species that were determined to be pollinator resources. 
Phenology date columns calculation: The number of inflorescences on each census date in each plot was divided by the total number of inflorescences counted in the plot for the full season to determine proportional flowering on each date. This proportion was then summed to create a cumulative sum.

DATA-SPECIFIC INFORMATION FOR: SPECIESPollResDates.csv
Number of variables: 9
Number of cases/rows: 995
Variable List: 
“YEAR” – year of data collection (2015, 2016, 2017, 2018)
“ASPECT” -  aspect of plot (N = North, S= South)
“SITE” – Location of hill with paired aspects (BH = Bechtel House, DP= Double ponds, FH=Hill 1521, TP = Turtle ponds, TH = Three Tree Hill). TH was only included in 2016 & 2017 and was not included in ANOVA analyses. 
“PLOT” – Six per site, Three plots on each aspect. 1st two letters (e.g. BH) indicate site, 3rd (e.g. N) indicates aspect, Number at end (1, 2, or 3) indicates replicate. 
“NAME” –Species binomial 
““Start” day of year of 5% flowering. 
“Mid” or “Peak” day of year of 50% flowering,
 “End” the day of year of 95% flowering. 
“length” = number of days from “start” (5% flowering) to “end” (95% flowering) dates

Phenology date columns calculation: The number of inflorescences for each species on each census date in each plot was divided by the total number of inflorescences for that species counted in the plot for the full season to determine proportional flowering on each date. This proportion was then summed to create a cumulative sum for each species.

Raw flowering time data was collected as the number of inflorescences in flower for each species on each census date. Data for this paper was filtered to species that were determined to be pollinator resources. 

DATA-SPECIFIC INFORMATION FOR: LASPollResDates.csv
Number of variables: 8 
Number of cases/rows: 38
Variable List: 
“YEAR” – year of experiment data collection (2017, 2018)
“ASPECT” - aspect of plot (N = North, S= South)
“SITE” – Location of hill with paired aspects (BH = Bechtel House, DP= Double ponds, FH=Hill 1521, TP = Turtle ponds, TH = Three Tree Hill). TH was only included in 2016 & 2017 and was not included in ANOVA analyses. 
“PLOT” – Six per site, Three plots on each aspect. 1st two letters (e.g. BH) indicate site, 3rd (e.g. N) indicates aspect, Number at end (1, 2, or 3) indicates replicate. 
“Start” day of year of 5% flowering. 
“Mid” or “Peak” day of year of 50% flowering,
 “End” the day of year of 95% flowering. 
“length” = number of days from “start” (5% flowering) to “end” (95% flowering) day of year


Raw flowering time data was collected as the number of inflorescences in flower for Lasthenia gracilis in experimental plots on each census date. 

Phenology date columns calculation: The number of Lasthenia gracilis inflorescences in experimental plots on each census date in each plot was divided by the total number of inflorescences counted in the plot for the full season to determine proportional flowering on each date. This proportion was then summed to create a cumulative sum.



DATA-SPECIFIC INFORMATION FOR: SpInfo.csv

Number of variables: 22
Number of cases/rows: 17022
Variable List: 
"SP_CODE" – six (or nine) letter unique species code (1st 3 letters of genus and 1st 3 letters of specific epithet). Nine letter codes are used in cases where a six letter code would be identical to another species already in the dataset.             
"DATE"   - mm/dd/yy 
“YEAR” – year of experiment data collection (2017, 2018)
“DOY” – day of year (# of days since Jan1st)
“PLOT” – Six per site, Three plots on each aspect. 1st two letters (e.g. BH) indicate site, 3rd (e.g. N) indicates aspect, Number at end (1, 2, or 3) indicates replicate. 
“SITE” – Location of hill with paired aspects (BH = Bechtel House, DP= Double ponds, FH=Hill 1521, TP = Turtle ponds, TH = Three Tree Hill). TH was only included in 2016 & 2017 and was not included in ANOVA analyses. 
“ASPECT” - aspect of plot (N = North, S= South)      
 "PHENO"  - all reproductive phases observed            
"MAIN_PHENO"    - main reproductive phase     
"INFL"     - number of total inflorescences in bud, flower or fruit        
 "FLS"   - number of inflorescences in flower                     
"FRT"   - number of inflorescences in fruit                     
“NAME” – Species binomial 
"LIFESPAN"  - A = annual, P= perennial          
"TYPE" – F=Forb, G=Grass              
"NATIVE"  - Y = yes, N= No            
"PollinatorResource"  - pollinator resource Y = yes, N= No
"BEE"    - Bee resource Y = yes, N= No                       
"BUTTERFLY"  - butterfly resource Y = yes, N= No                    
"FLY"  = fly resource Y = yes, N= No                    
"MOTHS" – moth resource Y = yes, N= No                                 
"BEETLES"  - beetle resource Y = yes, N= No                      

Raw phenology data for all species in plots, collected as the number of inflorescences in flower and fruit. Species status as a pollinator resource was identified by direct observations of animal visitation during the study, together with outside sources, including information provided by the Xerces society (Mader et al. 2011). 

Mader E, Shepherd M, Vaughan M, Hoffman Black S, LeBuhn. G (2011) The Xerces Society Guide to Attracting Native Pollinators: Protecting North America’s Bees and Butterflies. Storey Publishing, North Adams MA.

