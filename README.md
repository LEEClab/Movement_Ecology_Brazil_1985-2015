# Movement Ecology in Brazil (1985-2015)

Here we present the data and the analyses of the state of the art of scientific production of Movement Ecology in Brazil, in the period 1985-2015. Plus, it also includes the analysis of questionnaires answered by the participants that took part on the [I Movement Ecology Brazil Workshop](http://www.leec.eco.br/en/meb.html) (MEB), at Rio Claro (SP, Brazi), in 2015.

The data and code presented here is part and Supplementary Material of the article bellow. They present the same analyses presented in the article. To use and cite this, please refer to:

Alves-Eigenheer, M. et al. Challenges and perspectives of movement ecology research in Brazil. Submitted to Perspectives in Ecology and Conservation.

The repository has 3 folders:
- [Code](https://github.com/LEEClab/Movement_Ecology_Brazil_1985-2015/tree/master/Code): An R script for running analyses and plotting some figures.
- [Data](https://github.com/LEEClab/Movement_Ecology_Brazil_1985-2015/tree/master/Data): The data for the analysis. It includes:
  1. A survey of the literature on movement ecology in Brazil, in the period 1985-2005, performed in the scientific citation index [Web of Science®](https://webofknowledge.com). The data are presented in `xls` and `csv` formats.
  2. Questionnaires answered by the participants of the I Movement Ecology Brazil Workshop, in 2015. The data are presented in `xls` and `csv` formats.
  3. A metadata document, explaining in details the columns of the literature and I MEB data.
  4. A [folder](https://github.com/LEEClab/Movement_Ecology_Brazil_1985-2015/tree/master/Data/shapes) containin a map of Brazilian states, as well as the location of movement ecology research groups in Brazil.
- [Results](https://github.com/LEEClab/Movement_Ecology_Brazil_1985-2015/tree/master/Results): A folder where figures and files are saved, in case the analyses are run.

To re-run the analises, first you have to install the required packages (listed in the beggining of the script) and set the `data` and `results` directories (as well as the `map` directory) in the script, with you local directory path:

```[r]
# Set working directories

# Data folder
datadir <- "your_local_path/Data"
# Map folder
mapdir <- "your_local_path/Data/shapes"
# Results folder
resultsdir <- "your_local_path/Results"
```

Then you can normally run the script.

If you have any questions or suggestions, contact us:  
Milene A. Alves-Eigenheer <<mileneaae@gmail.com>>  
Bernardo B. S. Niebuhr <<bernardo_brandaum@yahoo.com.br>>  
Milton C. Ribeiro <<mcr@rc.unesp.br>>
