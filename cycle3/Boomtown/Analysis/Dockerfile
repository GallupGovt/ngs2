FROM continuumio/miniconda3

LABEL maintainer="Pablo Diego-Rosell c_Pablo_Diego-Rosell@gallup.co.uk"

RUN mkdir /app
WORKDIR /app

COPY environment.yaml /tmp/environment.yaml

RUN conda update -n base -c defaults conda \
 && conda env create -f /tmp/environment.yaml \
 && conda shell.posix activate ngs2 \
 && conda install -c conda-forge pandoc \
 && conda install -c anaconda libcurl \
 && conda install -c anaconda make \

ADD . /app

CMD /opt/conda/envs/ngs2/bin/Rscript --vanilla ngs2.R
