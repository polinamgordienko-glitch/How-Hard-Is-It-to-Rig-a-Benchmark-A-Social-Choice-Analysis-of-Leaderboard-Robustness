# How Hard Is It to Rig a Benchmark?
## A Social Choice Analysis of Leaderboard Robustness

**Accepted at NeurIPS 2026**

Polina Gordienko · Georg Schollmeyer · Frauke Kreuter · Christoph Jansen

[Paper](paper.pdf)

For a fixed suite of datasets and a fixed set of competing models,
how many tasks must a model developer include in training to make
a target model top-ranked?

![Robustness under four aggregation rules on MMLU and BBH](ecdf_figure.png)

This repository contains the code for the experiments in our paper.
We compute instance-level robustness under arithmetic mean, median,
mean win rate and pairwise majority on MMLU under HELM and BIG-Bench
Hard under the Open LLM Leaderboard.

## Contents

- `robustness_code.R`: robustness computations, statistical analyses,
  additional experiments and figures.
- `download_bbh.py`: extraction of BBH scores from the fixed
  Open LLM Leaderboard results snapshot.
- `model_size_analysis_corrected.py`: preparation and verification
  of BBH model-size data.
- `data/bbh_sizes.csv`: model parameter counts for the BBH analysis.
- `data/mmlu_costs.csv`: MMLU test-instance counts by subject.
- `data/bbh_costs.csv`: BBH test-instance counts by task.

## Requirements

The analysis uses R and the packages `jsonlite`, `dplyr`, `tibble`,
`digest`, `lpSolve`, `tidyr` and `ggplot2`.

Install them with:

```r
install.packages(
  c("jsonlite", "dplyr", "tibble", "digest", "lpSolve", "tidyr", "ggplot2"),
  repos = "https://cloud.r-project.org"
)
```

Python is used only for data preparation. It is not required to run
the R analysis when using the supplied scores and metadata.

The code was tested with R version 4.4.2 on macOS Sequoia 15.7.3.

## Data

The experiments require MMLU and BBH score data. There are two ways to obtain it:

Option A. Download the data for both suites under this link: [https://zenodo.org/records/18402602?preview=1&token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjI1MDc3YWZjLWIzYWEtNDUzYy05NzBkLTY4OTA5NmEwMjcwOSIsImRhdGEiOnt9LCJyYW5kb20iOiI4NmI4MjY1MWJkMWNiZTFmNzM5NDFiYmUyYTc2YTI0MiJ9.SfRSP6FsVUywpMI0iBcdhiRran2YRlGyeso2JPgoPxhl7KXCWtCiMcziKANBDr6V2zUqQUBR7WeeMhEdjGodAQ. ](https://zenodo.org/records/20058671?preview=1&token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjkzMTQ3YWVkLTZkY2ItNDBlMS04NGExLWUzOWM0NDFhYmE3NSIsImRhdGEiOnt9LCJyYW5kb20iOiI5M2IxNGM2NDk2ODk3MDJkMDliY2YxYzU2MTU5OWZlMiJ9.ph8cLU92Lkam82w8p44ZBge52hJy-1eSVPn3SOrvmUg9LfYXW31G1u2vApXiL2WlGq-IE--gqUV_WiGUzNHFmg)

Option B. 

1. Download the HELM MMLU raw results (v1.0.0) from the public crfm-helm-public bucket in the Google Cloud Storage (GCS) by completing the following two steps:

  1.1.	Install the Google Cloud CLI (gcloud) by following Google’s official instructions: 
https://docs.cloud.google.com/sdk/docs/install-sdk.

  1.2.	Download HELM MMLU raw results by following the official HELM instructions:
https://crfm-helm.readthedocs.io/en/latest/downloading_raw_results/.

2. Download the BBH data from the Hugging Face Open LLM Leaderboard. Note that the paper uses the data downloaded on April 18, 2026.


  2.1 First, you will require a Hugging Face token, which can be obtained, following the instructions under the following link: https://huggingface.co/docs/hub/security-tokens

  2.2 Then, run the following commands from the repository root. On macOS/Linux this can be Terminal. The extraction script `download_bbh.py` is included in this repository. It downloads the public Open LLM Leaderboard results dataset, extracts BBH subtasks using `acc_norm,none`, and writes `bbh_scores.csv`.

The R script expects this file to be located at `~/bbh_analysis/bbh_scores.csv`. Therefore, create this directory and run the download script from inside it:

```bash
repo_dir="$(pwd)"

mkdir -p ~/bbh_analysis
cd ~/bbh_analysis

python -m pip install huggingface_hub pandas pyarrow
export HF_TOKEN="YOUR_TOKEN"
python "$repo_dir/download_bbh.py"

On  Windows PowerShell, set the token with:
```text
$env:HF_TOKEN="YOUR_TOKEN"
```

After downloading via Option A or B, place the HELM MMLU run files and BBH score file in the following
locations:

```text
~/helm_mmlu/
  runs/
    v1.0.0/
      <run directory>/
        run_spec.json
        stats.json

~/bbh_analysis/
  bbh_scores.csv
```

From the repository directory, copy the supplied metadata files:

```bash
mkdir -p ~/helm_mmlu ~/bbh_analysis
cp data/mmlu_costs.csv ~/helm_mmlu/
cp data/bbh_costs.csv data/bbh_sizes.csv ~/bbh_analysis/
```

## Reproducing the results

From the repository directory, run:

```bash
Rscript --vanilla robustness_code.R
```

The script computes:

- Raw and normalized robustness under all four aggregation rules.
- Bootstrap intervals and paired Wilcoxon tests.
- The BBH analysis retaining one model per namespace.
- Comparisons of robustness between aggregation rules.
- Robustness for the quarter of models with the highest mean score.
- The BBH model-size analysis.
- Minimum costs when dataset costs equal test-instance counts.
- The empirical CDF figures.

For the strongest-quarter and model-size analyses, we summarize
the selected target models while retaining the competing models
within each analysis.

## Outputs

MMLU robustness results are saved in `~/helm_mmlu/`.
The combined summaries and additional results are saved in
`~/bbh_analysis/`.

The main output files include:

- `all_results.rds`: the original robustness and statistical results.
- `additional_results.rds`: the additional analyses.
- `norm_sum.csv`: median normalized robustness.
- `cmp_sum.csv`: counts of comparisons between aggregation rules.
- `top_sum.csv`: strongest-quarter results.
- `size_sum.csv` and `size_rho.csv`: model-size results.
- `cost_sum.csv` and `cost_pct.csv`: heterogeneous-cost results.
- `ecdf.pdf` and `bbh_dup_ecdf.pdf`: figures.
- `sessionInfo.txt`: the R version and package versions used.

## Citation

If you use this code or the results, please cite:

```bibtex
@inproceedings{gordienko2026,
  title     = {How Hard Is It to Rig a Benchmark?
               A Social Choice Analysis of Leaderboard Robustness},
  author    = {Gordienko, Polina and Schollmeyer, Georg
               and Kreuter, Frauke and Jansen, Christoph},
  booktitle = {Advances in Neural Information Processing Systems},
  year      = {2026}
}
```

## Questions

Please open a GitHub issue for questions about the code or
reproducing the results.
