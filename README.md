# growthrateR
Shiny app for estimating growth rates from OD data

Code and functions used in my shiny app, <a href="https://vedomics.shinyapps.io/growthrateR/?_ga=2.57990767.435307508.1613752576-912579947.1611334819"> growthrateR </a>

<strong> FYI: </strong> Takes OD data from a plate reader as a .csv file with wells as headers and time as a column and estimates growth-rates in a user-dependent fashion. Users can then download their files. 

Note: The file "all_strain_GC.csv" is growth curve data from several strains of <i> S. aureus </i> generated in part for use in the Key et al. 2022 paper. You may use this data, in GrowthrateR to generate the estimated growth rates in "all_strain_rates.csv". The R script, "rate_figure_script.R" is used to directly generate extended data figure 12 referenced in the paper. 

![Lieberman lab, MIT](../main/www/lieb.png)
