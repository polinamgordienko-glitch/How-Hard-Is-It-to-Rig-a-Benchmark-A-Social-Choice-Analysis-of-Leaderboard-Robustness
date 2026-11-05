# How Hard Is It to Rig a Benchmark? A Social Choice Analysis of Leaderboard Robustness

## Introduction

This repository contains the code to reproduce all experiments in our paper titled "How Hard Is It to Rig a Benchmark? A Social Choice Analysis of Leaderboard Robustness". Starting from MMLU and BBH model scores, the script "robustness_code.R" constructs complete score matrices, computes instance-level robustness under arithmetic mean, median, mean win rate and pairwise majority and runs all sensitivity analyses. 

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

After downloading via Option A or B, the code expects the HELM MMLU results to be placed in a folder structure that looks like this (schematically):

```text
helm_mmlu/
  runs/
    v1.0.0/
      <RUN_ID_1>/
        run_spec.json
        stats.json
      ...
```
The key point is: under helm_mmlu/runs/v1.0.0/ there should be many run directories (each a run id), and each run directory should contain at least run_spec.json and stats.json.

For BBH,if you download the BBH data manually, save it as:
```text
~/bbh_analysis/bbh_scores.csv
```
