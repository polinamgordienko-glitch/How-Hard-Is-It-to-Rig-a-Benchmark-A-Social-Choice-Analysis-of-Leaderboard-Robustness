import os, json, math
import pandas as pd
from huggingface_hub import HfApi, hf_hub_download

repo = "open-llm-leaderboard/results"
rev = "aa81ecc38fdc5708254b833923368970efdf5ef5"
token = os.environ.get("HF_TOKEN")
rows = []
seen = set()

files = sorted(HfApi(token=token).list_repo_files(
    repo, repo_type="dataset", revision=rev
))

for f in files:
    if not (
        f.rsplit("/", 1)[-1].startswith("results_")
        and f.endswith(".json")
    ):
        continue

    path = hf_hub_download(
        repo, f, repo_type="dataset", revision=rev, token=token
    )

    with open(path, "r") as fh:
        ex = json.load(fh)

    model = ex.get("model_name") or ex.get("model_name_sanitized")
    if not model:
        continue

    for task, metrics in ex.get("results", {}).items():
        if (
            not task.startswith("leaderboard_bbh_")
            or not isinstance(metrics, dict)
        ):
            continue

        score = metrics.get("acc_norm,none")
        if score is None:
            continue

        score = float(score)
        if not math.isfinite(score):
            continue

        key = (task.replace("leaderboard_", ""), model)
        if key in seen:
            continue

        seen.add(key)
        rows.append((*key, score))

df = pd.DataFrame(rows, columns=["dataset_id", "model", "score"])
df = df.sort_values(["dataset_id", "model"])

assert len(df) == 108168
assert df["model"].nunique() == 4507
assert df["dataset_id"].nunique() == 24

df.to_csv("bbh_scores.csv", index=False)