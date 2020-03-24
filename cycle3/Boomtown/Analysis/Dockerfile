FROM jrnold/rstan

LABEL maintainer="Pablo Diego-Rosell c_Pablo_Diego-Rosell@gallup.co.uk"

RUN mkdir /app
WORKDIR /app

ADD . /app

RUN R -e "install.packages('pacman', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('formatR', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('Hmisc', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('bridgesampling', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('recipes', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('caret', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('pROC', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('pacman', repos='http://cran.us.r-project.org')"
RUN R -e "install.packages('randomForest', repos='http://cran.us.r-project.org')"

CMD Rscript --vanilla ngs2.R
