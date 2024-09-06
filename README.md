# Gender Composition in Classrooms: Influences on Post-Secondary Schooling Choices

This repository contains the code and data associated with the paper titled "Gender Composition in Classrooms: Influences on Post-Secondary Schooling Choices." The paper investigates the relationship between the gender composition of classrooms and female students' university major choices in Colombia.

## Contents

* **./With_the_Boys__How__Girls__STEM_Interest.pdf:**  The PDF version of the research paper.
* **./Tables:** Directory containing LaTeX files for tables presented in the paper.
* **./Literature:** Directory containing relevant research articles and reports cited in the paper.
* **./Graph:** Directory containing graphs and figures used in the paper.
* **./Scripts:** Directory containing R scripts used for data cleaning, analysis, and visualization.
    * **./Scripts/R:** Subdirectory containing R scripts.
    * **./Scripts/SQL:** Subdirectory containing SQL scripts for data extraction.
* **./Data:** Directory containing processed data files used in the analysis (Note: The raw data is not publicly available).
* **./obtimal_distance.json:** JSON file with optimal distance calculations.

## Data Access

The raw data used in this research is not publicly available due to privacy concerns and restrictions imposed by the data providers. The processed data files in the "./Data" directory are anonymized and aggregated to protect individual privacy. If you are interested in accessing the raw data, please contact the corresponding author for information about the data access process and requirements.

## Reproducibility

To reproduce the analysis and results presented in the paper, follow these steps:

1. Clone this repository to your local machine.
2. Install the necessary R packages listed in the "genereal_settings.R" script.
3. Run the R scripts in the "./Scripts/R" directory in the following order:
    * `read_data.R`
    * `Correlatons.R`
    * `Optimal bandwith with bce.R`
    * `Marginal effect.R`
    * `Fixed effect.R`
    * `staggered.R`
    * `colombian_context.R`
4. The scripts will generate the tables, figures, and results presented in the paper.

## Citation

If you use this code or data in your own research, please cite the following paper:

```latex
@article{polancojimenez2023gender,
  title={Gender Composition in Classrooms: Influences on Post-Secondary Schooling Choices},
  author={Polanco-Jim{\'e}nez, Jaime and De Witte, Kristof and Bernal, Gloria L},
  journal={Unpublished Manuscript},
  year={2024}
}
```

## Contact

For any questions or inquiries, please contact the corresponding author:

Jaime Polanco-Jiménez
Email: jaime.polanco@javeriana.edu.co 