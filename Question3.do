** Part a

*  Import the data first
import delimited cars.csv
*  Rename the variables
rename dimensionsheight dim_hei
rename dimensionslength dim_len
rename dimensionswidth dim_wid
rename engineinformationdriveline eng_driveline
rename engineinformationenginetype eng_type
rename engineinformationhybrid eng_hyb
rename engineinformationnumberofforward eng_gearnum
rename engineinformationtransmission eng_trans
rename fuelinformationcitympg fuel_city
rename fuelinformationfueltype fuel_type
rename fuelinformationhighwaympg fuel_highway
rename identificationclassification id_class
rename identificationid uid
rename identificationmake id_make
rename identificationmodelyear id_myear
rename identificationyear id_ryear
rename engineinformationenginestatistic enstat_horsep
rename v18 enstat_tor


** Part b

*  Create another data set which restrict the fuel_type to "Gasoline"

keep if fuel_type == "Gasoline"
save cars_gasoline


** Part c

*  Fit the model by instructions
regress fuel_highway enstat_horsep enstat_tor dim_hei dim_len dim_wid i.id_ryear
*  When all other variables in the model held constant, we can see from the 
*  summary that the predictor horsepower is positively related with the MPG on
*  the highway as the estimate of the coefficient is positive. To be more 
*  explicitly, as horsepower increases, MPG on the highway is expected to 
*  increase. The estimated coefficient is 0.016, which means when horsepower 
*  increases for 1 unit, the MPG on the highway would increase for 0.016. An 
*  important thing should be noticed, which is the value is for cars released in
*  2009(with this estimated intercept). When consider other releasing years, 
*  the estimated intercept might change in this model. The p-value for the 
*  estimated coefficient of horsepower is 7.96e-13 which is very small. This 
*  implies that this relationship between MPG on the highway and horsepower is 
*  statistically significant. They are very likely to have this positive 
*  relationship.


** Part d

*  Generate a new model with interactions between horsepower and torque
regress fuel_highway c.enstat_horsep##c.enstat_tor dim_hei dim_len dim_wid i.id_ryear
*  Now choose values for horsepower and torque
global lower = 100
global median = 275
global higher = 450
*  Then draw the interaction plot
margins, at(enstat_horsep=(100(3)450) enstat_tor=($lower, $median, $higher))
marginsplot






