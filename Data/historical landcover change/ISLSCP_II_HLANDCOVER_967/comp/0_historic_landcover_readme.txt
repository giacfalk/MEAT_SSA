##############################################################################
#      Global Historical Land Cover and Land Use Estimates (1700-1990)       #
##############################################################################
The files in this directory were provided by Dr. Kees Klein Goldewijk from the 
National Institute of Public Health and the Environment (RIVM) in the 
Netherlands. These are global, gridded, coarse resolution (1/2 and 1 degree 
spatial resolutions) data sets with historical estimates of land cover and land 
use over the past 300 years. Data layers are provided for every 50 years from 
1700 to 1950 and then every 20 years to 1990. The original 0.5 degree files are 
available from RIVM at http://www.rivm.nl/env/int/hyde/

Testing against historical data is an important step for validating integrated 
models of global environmental change. Owing to long time lags in the climate 
and biogeochemical systems, these models should aim to simulate the land use 
dynamics for long periods, i.e., spanning decades to centuries. Developing such 
models requires an understanding of past and current trends and is therefore 
strongly data dependent. For this purpose, a historical database of the global 
environment has been developed: the History Database of the Global Environment 
(HYDE) (Klein Goldewijk 2001). Historical statistical inventories on 
agricultural land (census data, tax records, land surveys, etc.) and different 
spatial analysis techniques were used to create a geographically-explicit data 
set of land use change, with a regular time interval. The data set can be used 
to test integrated models of global change.

The original 0.5 degree data set has been modified by the staff of the 
International Satellite Land Surface Climatology Project (ISLSCP) Initiative II 
data collection in order to match the land/water boundaries used in the 
collection. A separate file that shows the differences between the original 0.5 
degree land/water boundaries and the ISLSCP II land/water mask is provided. 
Finally, the ISLSCP II staff has created a 1 degree version of the data set by 
aggregating the 0.5 degree data set to the coarser resolution. This 1 degree 
version was produced so that all data sets within the ISLSCP II collection 
contained 1 degree versions of all data sets and should be considered a 'browse' 
product. Users should always refer to the full data set documentation or the 
reference below for more details on data set production.

##############################################################################

File Name Convention
---------------------
The files in this data set are named "historic_landcover_XX_YYYY.asc", where XX can be 
either hd or 1d, meaning a spatial resolution of 0.5 and 1 degree in both 
latitude and longitude, respectively and YYYY is the year from 1700 to 1990. For 
each spatial resolution, 8 files with global estimates of historical land cover 
and land use are provided for every 50 years from 1700 to 1950, and then every 
20 years to 1990. An additional file called "historic_landcover_hd_chngm.asc" is also 
provided that shows the differences between the original 0.5 degree land/water 
mask and the 0.5 degree land/water mask used in this collection. Note that the 
land/water mask difference file is only available at a 0.5 degree spatial 
resolution.

##############################################################################

ASCII File Format
------------------
All of the files in the ISLSCP Initiative II data collection are in the ASCII, 
or text format. The file format consists of numerical fields of varying length, 
which are delimited by a single space and arranged in columns and rows. The 
files in this data set contain 720 columns by 360 rows for the 0.5 degree data 
sets, and 360 columns and 180 rows for the 1 degree data sets. All values are 
written as integer numbers. Land cover/use classes are assigned the values below 
(note that values 3 and 4 are non-existent):

   0  Water Bodies
   1  Cultivated land
   2  Pasture/land used for grazing
   5  Ice
   6  Tundra
   7  Wooded tundra
   8  Boreal forest
   9  Cool conifer forest
  10  Temperate mixed forest
  11  Temperate deciduous forest
  12  Warm mixed forest
  13  Grassland/Steppe
  14  Hot desert
  15  Scrubland
  16  Savanna
  17  Tropical woodland
  18  Tropical forest
  19  No data over land (e.g. Antarctica)

The values in the "historic_landcover_diffs_hd.asc" file represent the differences 
between the 0.5 degree ISLSCP II land/water mask and the original land/water mask 
and mean the following:
 -1 = ISLSCP II mask is water and original data is land (data removed)
  0 = Data sets agree over land or water (data unchanged)
  1 = ISLSCP II mask is land or water and original data is missing (fill 
      value used).

The files are gridded to a common equal-angle lat/long grid with spatial 
resolutions of 0.5 x 0.5 and 1 x 1 degree in latitude and longitude, where the 
coordinates of the upper left corner of the files are located at 180 degrees W, 
90 degrees N and the lower right corner coordinates are located at 180 degrees E, 
90 degrees S. Data in the files are ordered from North to South and from West to 
East beginning at 180 degrees West and 90 degrees North. 
