# Computation
Project for the Data Computation course @fcds

# README

This repository contains code for performing various data analysis and image compression tasks using R. The code is organized into different sections, each performing a specific task. Below is an overview of each section and its purpose.


#Tasks:
1. Data preprocessing: Cleaning, transforming, and preparing the raw data for analysis.
2. Exploratory data analysis: Understanding the data, identifying patterns, and exploring relationships between variables.
3. Feature engineering: Creating new features or transforming existing features to improve the analysis.
4. Classification: Categorizing campaigns into different groups based on specific criteria.
5. Clustering: Grouping similar campaigns together based on their characteristics.
6. Performance evaluation

Algorithms:
1. Support Vector Machines (SVM): Building models that separate data points into different classes.
2. K-means clustering: Partitioning data into distinct groups based on similarity.
3. kNN
4. Principal Component Analysis (PCA): Dimensionality reduction technique for feature extraction.


## Data Analysis

The data analysis section focuses on cleaning and transforming the data, as well as performing clustering and visualization tasks. The code utilizes several R packages, including `corrgram`, `cluster`, `dplyr`, `ggplot2`, `factoextra`, `GGally`, `corrr`, `ggcorrplot`, `FactoMineR`, `caTools`, `class`, and `dbscan`.

To get started, make sure you have these packages installed in your R environment. If not, you can install them using the `install.packages()` function.

The code in this section performs the following tasks:

1. Loading and inspecting the data from a CSV file.
2. Handling missing values and removing duplicate rows.
3. Deriving the age and seniority of customers from the available data.
4. Converting categorical variables (`Education` and `Marital_Status`) into binary categories.
5. Analyzing customer age, education, marital status, number of children, and total expenses.
6. Performing k-means clustering and determining the optimal number of clusters using the elbow method and silhouette width.
7. Visualizing the clusters using various plots.
8. Performing agglomerative and divisive hierarchical clustering.
9. Conducting principal component analysis (PCA) and visualizing the results.

## Image Compression

The image compression section focuses on compressing an image using principal component analysis (PCA). The code utilizes the `jpeg`, `magick`, `ggplot2`, `factoextra`, and `gridExtra` packages.

To use this code, make sure you have these packages installed. If not, you can install them using the `install.packages()` function.

The code in this section performs the following tasks:

1. Reading and inspecting an input image (JPEG format).
2. Decomposing the RGB channels of the image using PCA.
3. Visualizing the eigenvalues of the PCA for each color channel.
4. Compressing the image by varying the number of PCA components and saving the compressed images.
5. Plotting the compressed images side by side for comparison.
6. Calculating the image sizes and the saved disk space for each compression level.


The code is organized into separate sections and can be executed in any R environment.

## Running the Code

To run the code, follow these steps:

1. Install the required R packages if they are not already installed.
2. Place the `marketing_campaign.csv` and `itatchi.jpg` files in the appropriate locations (as specified in the code).
3. Execute the code section by section or run the entire script.

Make sure to adjust file paths if necessary to match the location of the dataset and image file on your system.
