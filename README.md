# Data Imputation for Gene Expression

An interactive desktop application built with **R Shiny** for preprocessing, transforming, imputing, and visualizing **gene expression datasets with missing values**.

This tool is designed to guide users step by step through the complete workflow, from data upload to final visualization, without requiring advanced programming knowledge.

---

## What does this application do?

This application allows you to:

- Load gene expression datasets with missing values  
- Inspect and summarize the data structure  
- Apply statistical transformations to improve data distributions  
- Automatically or manually select imputation models  
- Perform single or multiple imputation (MICE)  
- Visualize and compare original, transformed, and imputed data  

The interface is fully interactive and runs locally on your computer.

---

## Who is this tool for?

This application is intended for:

- Students and researchers working with gene expression data  
- Users interested in data imputation methods  
- People with **no prior experience in R, Git, or Shiny**  
- Anyone who prefers a graphical interface over coding  

No programming is required to use the application once it is running.

---

## Supported Operating Systems

The application runs on:

- **Windows**  
- **macOS**  
- **Linux**

The only requirement is the ability to install **R** and **RStudio**, which are free and cross-platform.

---

## How to use the application (no Git required)

### Step 1 – Download the project

1. Click the **Code** button on this GitHub page  
2. Select **Download ZIP**  
3. Extract the ZIP file to any folder on your computer  

You do **not** need a GitHub account.

---

### Step 2 – Install R

Download and install R from the official website:

https://cran.r-project.org/

---

### Step 3 – Install RStudio

Download RStudio Desktop (free version):

https://posit.co/download/rstudio-desktop/

---

### Step 4 – Open the project

1. Open **RStudio**  
2. Click **File → Open Project**  
3. Navigate to the extracted folder  
4. Open the `.Rproj` file  

---

### Step 5 – Install required packages (one-time step)

```r
install.packages(c(
  "shiny",
  "bslib",
  "shinyjs",
  "DT",
  "echarts4r",
  "mice",
  "e1071",
  "bestNormalize"
))
```

---

### Step 6 – Run the application

```r
shiny::runApp()
```

---

## Application Workflow

1. **Upload & Profiling**  
2. **Transformation**  
3. **Imputation**  
4. **Visualization**

---

## License

This project is provided for academic and educational use.
