# BreathingPatterns

R code for the analysis of respiratory signals obtained by means of Optoelectronic Plethysmography.

Reference code for the research paper *A. LoMauro, A. Colli, L. Colombo, A. Aliverti,
Breathing patterns recognition: A functional data analysis approach,
Computer Methods and Programs in Biomedicine,
Volume 217,
2022,
106670,
ISSN 0169-2607,
https://doi.org/10.1016/j.cmpb.2022.106670.*


### Prerequisites

To run the code, you need to install the following R libraries:

	fda
	splus2R
	roahd
	fdakmapp

The *fdakmapp* library can be found under AlessandraColli/fdakmapp. For a smooth installation (also for
Windows users), we suggest downloading the source code in a folder called fdakmapp-master and install through

	install.packages('fdakmapp-master', type='source', repos=NULL)

#### Note
A more recent version of roahd and fdakmapp is available at https://github.com/astamm/roahd and https://github.com/astamm/fdacluster, respectively,
though current BreathingPatterns implementation depends on the older packages.

### Folder structure

 - *data* contains data files which can be used to test the code, and scripts to produce them

 - *scripts* contains example scripts to run the code: **validation.R** for the analysis of the univariate test data,
          and **complete_procedure.R** for the analysis of Optoelectronic Plethysmography (multivariate) data.

 - *src* contains source code
