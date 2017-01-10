# HutchinHill

Sources
http://www.computerworld.com/article/3038270/data-analytics/create-maps-in-r-in-10-fairly-easy-steps.html
http://www.endmemo.com/program/R/gsub.php

http://garrettgman.github.io/tidying/ -data tidying 
http://stackoverflow.com/questions/13672781/populate-a-column-using-if-statements-in-r

https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html - state boundary files
reformatted the countries following the form, Korea, the Republic of to The Republic of Korea
Changed People's republic of china to china to keep consistent with all the data sets

Changed Georia to Georgia(Country) so that it does not get confused with state when merging datasets during the creation of the clean sets

Guam was actually printed several times in the 2010-2011, 2012-13, 2013-14 datasets, and I manually deleted those duplicate entries

Created the complete data matrix of 2000-2015,
