# fdClassify
This is a R pacakage to indentify flash droughts with different methods

Six methods are implemented:

* Mo, K. C. & Lettenmaier, D. P. Precipitation Deficit Flash Droughts over the United States Journal of Hydrometeorology, American Meteorological Society, 2016, 17, 1169-1184
* Ford, T. W. & Labosier, C. F. Meteorological conditions associated with the onset of flash drought in the Eastern United States Agricultural and Forest Meteorology, Elsevier BV, 2017, 247, 414-423
* Christian, J. I.; Basara, J. B.; Hunt, E. D.; Otkin, J. A. & Xiao, X. Flash drought development and cascading impacts associated with the 2010 Russian heatwave Environmental Research Letters, IOP Publishing, 2020, 15, 094078
* Pendergrass, A. G.; Meehl, G. A.; Pulwarty, R.; Hobbins, M.; Hoell, A.; AghaKouchak, A.; Bonfils, C. J. W.; Gallant, A. J. E.; Hoerling, M.; Hoffmann, D.; Kaatz, L.; Lehner, F.; Llewellyn, D.; Mote, P.; Neale, R. B.; Overpeck, J. T.; Sheffield, A.; Stahl, K.; Svoboda, M.; Wheeler, M. C.; Wood, A. W. & Woodhouse, C. A. Flash droughts present a new challenge for subseasonal-to-seasonal prediction Nature Climate Change, Springer Science and Business Media LLC, 2020, 10, 191-199
* Noguera, I.; Dom\inguez-Castro, F. & Vicente-Serrano, S. M. Characteristics and trends of flash droughts in Spain, 1961--2018 Annals of the New York Academy of Sciences, Wiley Online Library, 2020
* Osman, M.; Zaitchik, B. F.; Badr, H. S.; Christian, J. I.; Tadesse, T.; Otkin, J. A. & Anderson, M. C. Flash drought onset over the contiguous United States: sensitivity of inventories and trends to quantitative definitions Hydrology and Earth System Sciences, Copernicus GmbH, 2021, 25, 565-581

The functions are built to handle daily or sub-daily data with minimal pre-processing required. 
Two datasets are preloaded (de_tha_d and de_tha_h) for daily and hourly data from the Tharandt (Germany) station from the FluxNET15 project (Pastorello et al., 2020). These datasets ilustrate the format you should organize your data before using these package functions. 

More info on the preloaded datasets can be found on http://sites.fluxdata.org/DE-Tha/.

For doubts, contributions or colaborations you can reach the Author of this package in pedrohlalencar@gmail.com or pedro.alencar@campus.tu-berlin.de
