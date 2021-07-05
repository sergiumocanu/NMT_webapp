# Neuro-ML-Tools

## Description

This is a machine learning web-application that can be run as an `R` project in `RStudio` or inside a `Docker` container.

## Installation
### R project run locally in RStudio
1. Simply clone the repo with `git clone https://github.com/sergiumocanu/RBioFS_app.git`. 
2. Download [R](https://www.r-project.org) and [RStudio](https://www.rstudio.com) for your machine.
3. Open the `.Rproj` file in RStudio and navigate to the `app.R` file.
4. At the top right, in the `Run App` (green play button) drop down menu select `Run External` then click `Run App`. Alternatively, in the console type `shiny::runApp('RBioFS')`.
5. To <span style="color:red">stop</span> the app, simply go back to RStudio and hit the *ESC* button on your keyboard or press the red stop button in the Console.

### Building Docker image using Dockerfile
1. Clone the repo with  `git clone https://github.com/sergiumocanu/RBioFS_app.git`.
2. Download and install [Docker](https://www.docker.com).
3. In terminal navigate to the `RBioFS` folder where the `Dockerfile` file is located. 
4. Build the image (this will take some time  the first time you run it) with: `docker build -t ml_webapp .` (don't miss the *space* and *period* at the end of the command).
5. Run the application: `docker rum --rm -p 3838:3838 --name ml ml_webapp:latest`.
6. In your browser navigate to `localhost:3838`.
7. To <span style="color:red">stop</span> the app, go back to the terminal and pres *CTRL+C* then type `docker stop ml`. 