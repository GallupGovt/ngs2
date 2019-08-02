###### Dockerfile #######
FROM jrnold/rstan

# Set the working directory to /app
#RUN mkdir /home/rstudio

# Copy the current directory contents into the container at /app
#COPY NGS2_WITNESS_Cycle2_Notebook.Rmd /home/rstudio/
WORKDIR /report


RUN R -e "options(repos = \
  list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-02-06/')); \
  install.packages('pacman', dependencies=TRUE); \
  install.packages('formatR', dependencies=TRUE); \
  install.packages('Hmisc', dependencies=TRUE); \
  install.packages('bridgesampling', dependencies=TRUE); \
  install.packages('recipes', dependencies=TRUE); \
  install.packages('caret', dependencies=TRUE); \
  install.packages('pROC', dependencies=TRUE); \
  install.packages('randomForest', dependencies=TRUE)"

# Set the container to run `Rscript --vanilla ` by default
ENTRYPOINT ["/usr/local/bin/Rscript", "--vanilla"]

# Set the `example_report.R data.csv` as the default script to run with ENTRYPOINT's Rscript
CMD ["ngs2.R"]
