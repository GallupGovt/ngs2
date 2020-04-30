# NGS2

## Setup

We assume a working knowledge of your operating systems file system and terminal.

1. Install `git`

For Linux, please see the following guide: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git#_installing_on_linux

For MacOS, please see the following guide: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git#_installing_on_macos

For Windows, please see the following guide: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git#_installing_on_windows

2. Install `docker`

For CentOS, please see the following guide: https://docs.docker.com/engine/install/centos/

For Ubuntu, please see the following guide: https://docs.docker.com/engine/install/ubuntu/

For MacOS, please see the following guide: https://docs.docker.com/docker-for-mac/

For Windows, please see the following guide: https://docs.docker.com/docker-for-windows/


## Installation

The remaining parts of this guide will be tailored to Unix-like file systems, but can be easily interpretted in a Windows environment.

1. Clone over the repository

The code repository lives [here](https://github.com/GallupGovt/ngs2).  In order to retrieve using `git`, run the following command:

```bash
$ git clone https://github.com/GallupGovt/ngs2.git
```

This step will create a folder in your filesystem named `ngs2`.

2. Move to the correct working directory for Cycle 3

```bash
$ cd ngs2/cycle3/Boomtown/Analysis
```

3. Build the `docker` image

Now that we are all set up with preliminary steps, we can begin building the `docker` image.  Notice within this folder, there exists a filed named `Dockerfile`.  This file defines the way we will produce this `docker` image.  This image holds all the required environment dependencies needed to run our Cycle 3 models.  To build this image:

```bash
$ docker build -t ngs2:latest -f Dockerfile .
```

Building this `docker` image will take a bit, grab a coffee in the meantime!  To validate that we have built our image properly, we can run the following command:

```
$ docker images
```

The output should look as follows:

```
REPOSITORY               TAG                 IMAGE ID            CREATED             SIZE
ngs2                     latest              60ef15dd69db        3 minutes ago       4.23GB
continuumio/miniconda3   latest              b4adc22212f1        7 weeks ago         429MB
```

4. Create an empty directory at the root of your filesystem (or anywhere you would like)

This folder is where the outputs of our models will be placed. For example,

```bash
$ mkdir ~/.ngs2_output
```

Make sure to remember where you have created this folder, as it will be important for when we produce output.

# Usage

1. Run the `docker` image

We are ready to go!  To run a `docker` container based on the `docker` image we have built, run the following command:

```bash
$ docker run -v ~/.ngs2_output:/app/output ngs2:latest # notice that ~/.ngs2_output is the folder we created in the last step
```

If you would rather run this program in the background, you can add the `-d` flag as such:

```bash
$ docker run -d -v ~/.ngs2_output:/app/output ngs2:latest # notice that ~/.ngs2_output is the folder we created in the last step
```

2. To verify we are running

You can check the running of a `docker` container by running the following command:

```bash
$ docker ps
```

The output should look similar to the following:

```
CONTAINER ID        IMAGE               COMMAND                  CREATED             STATUS              PORTS               NAMES
d2d4bb97dca4        ngs2:latest         "/bin/sh -c '/opt/co…"   10 seconds ago      Up 9 seconds                            naughty_roentgen
```

---

You should be all set!  Once the `docker` container has completed running, you will see:

```bash
$ docker ps
CONTAINER ID        IMAGE               COMMAND                  CREATED             STATUS              PORTS               NAMES
```

and all of your output files will be stored in the output folder we created above, i.e.

```bash
$ ls -ltaX ~/.ngs2_output
```

### Thank you!