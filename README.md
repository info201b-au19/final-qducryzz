# Final Project Brainstorm
## Domain of Interest
We choose CO2 emission as our field of study due to our shared interest in climate change and natural environments. At the same time, we think the natural environment plays an important role in our daily life, so we want to get to know more about the correlation between humans and the environment.

Some existing projects in this domain:
#### Project 1:
>[Alaska Ocean Acidification Research: Autonomous Observations of Ocean Acidification in Alaska Coastal Seas](https://oceanacidification.noaa.gov/CurrentProjects/GulfofAlaska.aspx#)

#### Summary
>The project is run by JESSICA CROSS, NOAA ALASKA FISHERIES SCIENCE CENTER giving new insights into the seasonal progression of OA events caused by the progressive accumulation of anthropogenic CO2 into the region's coastal seas. The mooring and cruise data can also be used as an early warning system for stakeholders around the state, as well as to provide information for other types of OA research.

#### Project 2:
>[Global Carbon Project
The Science Framework
and Implementation](https://www.globalcarbonproject.org/global/pdf/GCPFrameworkFinal.pdf)

#### Summary
>The Global Carbon Project is designed to make the links
between the fundamental research on global change and
the Earth System carried out in the programs themselves and issues of vital concern for people. The project emphasizes on the carbon-climate human system, develops new methodologies for analyzing and
modeling the integrated carbon cycle addresses questions of direct policy
relevance, such as the management strategies and sustainable regional development pathways required to achieve
stabilization of carbon dioxide in the atmosphere, and seeks g to engage the industrial and energy sectors as well
as the economic development and resource management
sectors in the developing regions of the world.
#### Project 3：
>[Global Non-linear Effect of Temperature on Economic Production](https://web.stanford.edu/~mburke/climate/index.html)

#### Summary
>The project was published online in 2015 addressing how economic output around the world been affected by changes in temperature and precipitation and implication of the potential future impacts of climate change. The result shows that changes in temperature have substantially shaped economic growth in both rich and poor countries over the last half-century, and that future warming is likely to reduce global economic output, relative to a world without climate change.

**Our questions:**
#### Question 1:
>How CO2 emissions per capita is related to total economic output?

Compare the CO2 emissions level from dataset [Data on co2 emission per country per capita in from 1751 - 2014](http://cdiac.ornl.gov/ftp/ndp030/CSV-FILES/nation.1751_2014.csv) with the capita income in dataset [Human Development Index](https://data.humdata.org/dataset/human-development-index-hdi/resource/4a7fd374-7e35-4c04-b7c8-25e5943aa476), we can find the relationship of the economic output and the CO2 emission.

#### Question 2:
>How greenhouse emissions of each country is related to quality of life for the residents?

Use the country life expectancy & education data in dataset [Human Development Index](https://data.humdata.org/dataset/human-development-index-hdi/resource/4a7fd374-7e35-4c04-b7c8-25e5943aa476) combined with the CO2 emission in [Data on co2 emission per country per capita in from 1751 - 2014](http://cdiac.ornl.gov/ftp/ndp030/CSV-FILES/nation.1751_2014.csv), we can compare the quality of life for residents of a country and the total CO2 emissions over time showing how countries which have had very high emissions per capita tend to have a higher quality of life.
#### Question 3:
>How the renewable energy technology adopted in each country relates to the country economic output?

A country's renewable energy consumption shows the countries overall advancement in green energy. Our data set on renewable energy, the human development index, and emissions can be used to see how past greenhouse emission is related to present renewable energy percentage, and if sustainable energy is correlated with happier citizens.

## Finding Data

**[Data on co2 emission per country per capita in from 1751 - 2014](https://datahub.io/core/co2-fossil-by-nation#readme):** Shows the equivalent in million metric tons of CO2 greenhouse emissions per capita for each country. Will be used for Question 1.
>
>**Source:** Data comes from the Carbon Dioxide Information Analysis Center (CDIAC) a subdepartment of the United States Department of Energy. The CDIAC and DOE collect their data by calculating total industrial output, and by countries self reporting in accordance with international treaties. Data from 1751 - 2014
>
>**Number of Rows:** 17232
>
>**Number of Columns:** 10

**[Human Development Index](https://data.humdata.org/dataset/human-development-index-hdi/resource/4a7fd374-7e35-4c04-b7c8-25e5943aa476):** Shows the overall quality of life in a country. Will be used for Question 2.
>**Source:** The UNDP Human Development Reports Office (HDRO) uses a composite index of life expectancy, education, and per capita income which are used to give each country a value which shows the overall quality of life for the residents of said country. Data from 1980 to 2013.
>
>**Number of Rows:** 2122
>
>**Number of Columns:** 9

**[Renewable Energy Data](https://data.worldbank.org/indicator/eg.fec.rnew.zs):** shows the percentage of energy production in any given country. Will be used for Question 3.
>**Source:** the World Bank, the International Energy Agency, and the Energy Sector Management Assistance Program. These organizations track the number of renewable energy power plants, total investment in green energy, etc.  
>
>**Number of Rows:** 264
>
>**Number of Columns:** 30

