NGS2 Docker for Cycle 3 Pre-registration

1) Connecting to AWS Linux instance in terminal use:
ssh -i .ssh/id_rsa {first_last}@dc-docker.gallupaws.com where first, last are your Gallup credentials.

Upon first sign on, you will be asked about RSA fingerprint - just accept and write yes. You will then be prompted a password which is your Gallup internal credentials.

2) Getting Started with Git Repository
Navigate to your user folder on the AWS Instance:
(Note: when you log in to the instance, you will automatically be inside of your user directory.)

Exploring your current working directory:

ls -lta: Show all files and folders within relevant folder

pwd: Show current working directory path

cd {}: Change directory into a certain folder, where {} is the name of the folder

Clone the Git Repository from https://github.gallup.com/mitchell-bregman/ngs2docker
This pulls the repository from the remote webpage down to your machine (can either be AWS or local).

Run the following command on the linux terminal:

git clone https://github.gallup.com/mitchell-bregman/ngs2docker.git

This will create a new folder in your current working directory named ngs2docker. You can ensure this by checking ls -lta.
Move into this repository you have just cloned by using:

cd ngs2docker

3) Building the Docker image
Based on the above steps, within your current folder there should be a file named Dockerfile. This file contains the instructions for machine to build a Docker image. In order to run Docker container, we must "build" this image first.

To do so: docker build --tag=ngs2 .

We have now built the Docker image.

4) Running the Docker image
Now that we have a Docker image sitting within our machine, we have a container to run our code in.

To run the code: docker run --rm -v $PWD:/report ngs2

In case you'd like to run this as a background process, not affecting your current terminal view use the -d flag: docker run -d --rm -v $PWD:/report ngs2

From there, you are free to edit the .RMD file and run container the same way.

5) To run hypotheses in parallel on AWS
To run each hypothesis in parallel, we use a 'parallel for-loop' approach in R.

This requires the following packages:

library('foreach')
library('doParallel')
To set up the backend necessary to run parallel for-loop we type the two following commands in R:

registerDoParallel(cores=#) # - number of cores you want to run on
getDoParWorkers() # - get the number of cores specified above
The syntax to run a parallel for-loop in R is simple, and follows similar syntax to a traditional for-loop:

foreach(n = 1:100) %dopar% {'some code'})

To apply to the case of running all hypotheses in NGS2 is simple. We first define a vector containing the file name of each hypothesis. Then parallel for-loop through that vector. Here is some example code to do just that:

hypothesis_files = c("h1.1.R", "h1.2.R", "h1.3.R",
                 "h2.1.R", "h2.2.R", "h2.3.R", "h2.4.R", "h2.5.R", "h2.6.R", "h3.1.R", "h3.2.R", "h3.3.R",
                 "h3.4.R", "h3.5.R")
foreach(n = 1:length(hypothesis_files)) %dopar% {
  source(hypothesis_files[n])}
(This is currently configured in this repository. We default set to 7 cores. )

NGS2

(Optional) To setup git for code changes:
git Setup
To set git remote:

git remote add {name} {github.com/user/.git}
git remote -v to verify.
git fetch --all
git pull {name} master
To edit code

git fetch --all - retrieves all code that has been editied
git pull {remote} {branch} - updates your current code view to the one from whichever branch code was edited in.
Interacting with git is a whole thing in its own.

To download content:
scp {first_last}@dc-dev-docker.gallupaws.com:{full-path-to-file-on-ec2} {path-to-save-on-local}
