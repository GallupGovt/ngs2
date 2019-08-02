# NGS2 Docker Cycle 3

**Most up-to-date:**

As of August 02, 2019 - https://github.com/GallupGovt/ngs2/edit/master/cycle3/Boomtown/Preregistration

[Pablo Diego-Rosell](https://github.gallup.com/Pablo-Diego-Rosell "Gallup GitHub")
[Nasser Qadri](https://github.gallup.com/nasser-qadri "Gallup GitHub")
[Taylor Bolt](https://github.gallup.com/taylor-bolt "Gallup GitHub")
[Mitchell Bregman](https://github.gallup.com/mitchell-bregman "Gallup GitHub")

## To connect to AWS instance in terminal use: 
`ssh -i .ssh/id_rsa {first_last}@dc-dev-docker.gallupaws.com`

## (Optional) To setup `git` remote:
Instructions included below.

As of August 2, 2019 - set remote for [this repo](https://github.com/GallupGovt/ngs2/edit/master/cycle3/Boomtown/Preregistration) as `upstream`. This is required.

*To be moved to Predictive Analytics GitHub in future.*

## To build image:
I recommend creating a folder first, moving into it, then:

`docker build --tag=ngs2 .`

## To run container: 
`docker run --rm -v $PWD:/report ngs2`

From there, you are free to edit the `.RMD` file and run container the same way.

### `git` Setup
**To set `git` remote:**
1. `git remote add {name} {github.com/user/.git}`
2. `git remote -v` to verify.
3. `git fetch --all`
4. `git pull {name} master`


