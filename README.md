# BreathingPatterns

R code for the analysis of respiratory signals obtained by means of Optoelectronic Plethysmography.


### Prerequisites

To run the code, you need to install the following R libraries:

	fda
	splus2R
	roahd
        fdakmapp

The *fdakmapp* library can be found under AlessandraColli/fdakmapp. For a smooth installation (also for
Windows users), we suggest downloading the source code in a folder called fdakmapp-master and install through

	install.packages('fdakmapp-master', type='source', repos=NULL)

### Folder structure

	- *data* contains data files which can be used to test the code, and scripts to produce them
        - *scripts* contains example scripts to run the code: **validation.R** for the analysis of the univariate test data,
          and **complete_procedure.R** for the analysis of Optoelectronic Plethysmography (multivariate) data.
        - *src* contains source code
