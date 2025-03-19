mbX
A Comprehensive R Package for Streamlined 16S rRNA Microbiome Analysis

mbX is designed to simplify and automate the analysis of 16S rRNA microbiome data.
Whether you're a bioinformatician aiming to automate your workflow or a researcher focused on generating 
reproducible, publication-quality outputs, mbX is here to help. The package provides essential functions 
to clean taxonomic data, generate dynamic visualizations, and (soon) perform statistical analyses—all with minimal manual intervention.

Key Features:
-------------
• Data Cleaning with ezclean:
  - Transforms raw taxonomic data (e.g., outputs from QIIME2) into a standardized, relative abundance format.
  - Seamlessly merges your microbiome data with metadata and outputs a clean Excel file ready for downstream analysis.

• Visualization with ezviz:
  - Generates high-resolution, publication-ready stacked bar plots displaying microbial relative abundance.
  - Allows customization by selecting taxonomic levels and grouping variables.
  - Automatically aggregates low-abundance taxa to improve clarity.

• (Upcoming) Statistical Analysis with ezstat:
  - An integrated statistical workflow for deriving deeper insights from your microbiome datasets.

• Workflow Automation & Reproducibility:
  - Automatically names output files.
  - Cleans up intermediate files.
  - Provides clear console messages throughout the process.

Installation:
-------------
From CRAN:
  install.packages("mbX")

From GitHub (Local Installation):
  Download the package source file (mbX_0.1.3.tar.gz) and run:
  install.packages("mbX_0.1.3.tar.gz", repos = NULL, type = "source")

Then, load the package in your R session:
  library(mbX)

Repository Structure:
---------------------
• mbx_CRAN/  
  Contains all materials submitted to CRAN.

• time_test_mbX/  
  Includes data files and an R script used for testing the execution time of various functions.

• Documentation-mbX.pdf  
  The complete documentation for the mbX package.

• mbX_0.1.3.tar.gz  
  The packaged source file for mbX, available for local download and installation.

Getting Started:
----------------
1. Data Preparation:
   - Prepare your raw 16S taxonomic data (preferably as a CSV file from QIIME2) with clear headers and sample IDs.
   - Ensure your metadata file is correctly formatted (TXT, CSV, XLS, or XLSX) with consistent sample identifiers.

2. Data Cleaning (ezclean):
   - Import and standardize your data.
   - Merge microbiome data with metadata.
   - Output a cleaned Excel file.

3. Data Visualization (ezviz):
   - Process the cleaned data by grouping and averaging.
   - Generate publication-quality stacked bar plots.
   - Outputs include:
     • A cleaned Excel file (e.g., mbX_cleaned_families.xlsx)
     • A visualization data file (e.g., mbX_vizualization_data_families.xlsx)
     • A high-resolution PDF plot (e.g., mbX_viz_families.pdf)

4. (Coming Soon) Statistical Analysis (ezstat):
   - An integrated statistical workflow for deeper insights.

Example Usage:
--------------
  # Load the package
  library(mbX)

  # Clean your microbiome data
  ezclean(
    microbiome_data = "path/to/your/microbiome_data.csv",
    metadata = "path/to/your/metadata.csv",
    level = "family"
  )

  # Generate a visualization
  ezviz(
    microbiome_data = "path/to/your/microbiome_data.csv",
    metadata = "path/to/your/metadata.csv",
    level = "family",
    selected_metadata = "SampleType",  
    top_taxa = 10                     
  )



License:
--------
Refer to the license file in the repository for details.

Contact:
--------
For questions or more information, please contact the Dr. Lourenco Lab at the University of Georgia.

Happy analyzing!
