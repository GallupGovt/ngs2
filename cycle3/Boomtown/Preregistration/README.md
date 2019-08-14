# NGS2 Docker Cycle 3

[Pablo Diego-Rosell](https://github.gallup.com/Pablo-Diego-Rosell "Gallup GitHub")

[Nasser Qadri](https://github.gallup.com/nasser-qadri "Gallup GitHub")

[Taylor Bolt](https://github.gallup.com/taylor-bolt "Gallup GitHub")

[Mitchell Bregman](https://github.gallup.com/mitchell-bregman "Gallup GitHub")

-----

## Prerequisites:

- Must have Docker installed
- Must have Git installed


## 1) Clone the GitHub repository:

`git clone https://github.com/GallupGovt/ngs2.git`

This will download all projects contents and create a new folder called `ngs2`.


## 2) Navigate to proper directory where MCMC and Dockerfile live:

`cd ngs2/cycle3/Boomtown/Preregistration/`

In this directory, there will be the following files:

- `Dockerfile`
- `bridge_1.0`
- `bridge_1.1`
- `README.md`
- `analytics.R`
- `Bayes_power.R`
- `dataprep.R`
- `functions.R`
- `ngs2.R`
- `NGS2_WITNESS_Cycle3_Prereg.RMD`

The key here is the `Dockerfile` which will build and run the Docker container image.


## 3) Build the Docker image:

This `Dockerfile` contains the instructions for a machine to generate a Docker image. In order to run Docker container, we must build this image first.

To do so: `docker build --tag=ngs2 .`

We have now built the Docker image.


## 4) Running the Docker image:

Now that we have a Docker image sitting within our machine, we have a container to run our code in.

To run the code: `docker run --rm -v $PWD:/report ngs2`

In case you'd like to run this as a background process, not affecting your current terminal view use the `-d` flag: `docker run -d --rm -v $PWD:/report ngs2`

From there, you are free to edit the `.RMD` file and run container the same way.


## 5) Final output:

After running the Docker image, more files will populate into the directory.

The most important file will be: `NGS2_WITNESS_Cycle3_Prereg.html` which summarizes the output of the models!
