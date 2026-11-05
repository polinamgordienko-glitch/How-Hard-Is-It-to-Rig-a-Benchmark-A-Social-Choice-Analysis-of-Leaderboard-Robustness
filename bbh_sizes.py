from __future__ import annotations

import argparse
import json
import math
from collections import defaultdict
from pathlib import Path
from typing import Iterable

import pandas as pd


SNAPSHOT = "aa81ecc38fdc5708254b833923368970efdf5ef5"
EXPECTED_TASKS = 24
EXPECTED_MODELS = 4507
EXPECTED_NAMESPACES = 718

RAW_COLUMNS = {
    "k_mean": "Arithmetic mean",
    "k_median": "Median",
    "k_win_global": "Mean win rate",
    "k_maj_global": "Pairwise majority",
}

NORMALIZED_COLUMNS = {
    "k_mean_norm": "Arithmetic mean",
    "k_median_norm": "Median",
    "k_win_global_norm": "Mean win rate",
    "k_maj_global_norm": "Pairwise majority",
}

SIZE_BINS = [0, 3, 8, 15, 40, float("inf")]
SIZE_LABELS = ["≤ 3B", "(3, 8]B", "(8, 15]B", "(15, 40]B", "> 40B"]


def default_paths() -> dict[str, Path]:
    here = Path(__file__).resolve().parent
    home = Path.home()
    return {
        "robustness_csv": home / "bbh_analysis" / "bbh_robustness_results.csv",
        "scores_csv": home / "bbh_analysis" / "bbh_scores.csv",
        "namespace_robustness_csv": (
            home / "bbh_analysis" / "bbh_robustness_results_model_uploader.csv"
        ),
        "results_root": (
            home
            / ".cache"
            / "huggingface"
            / "hub"
            / "datasets--open-llm-leaderboard--results"
            / "snapshots"
            / SNAPSHOT
        ),
    }


def parse_args() -> argparse.Namespace:
    paths = default_paths()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--robustness-csv",
        type=Path,
        default=paths["robustness_csv"],
        help="Full-BBH robustness results used in the paper.",
    )
    parser.add_argument(
        "--scores-csv",
        type=Path,
        default=paths["scores_csv"],
        help="Long BBH score table with dataset_id, model, and score columns.",
    )
    parser.add_argument(
        "--namespace-robustness-csv",
        type=Path,
        default=paths["namespace_robustness_csv"],
        help="Robustness results after retaining one model per namespace.",
    )
    parser.add_argument(
        "--results-root",
        type=Path,
        default=paths["results_root"],
        help="Pinned Open LLM Leaderboard results snapshot.",
    )
    return parser.parse_args()


def require_file(path: Path, description: str) -> None:
    if not path.is_file():
        raise FileNotFoundError(f"{description} not found: {path}")


def require_directory(path: Path, description: str) -> None:
    if not path.is_dir():
        raise FileNotFoundError(f"{description} not found: {path}")


def fingerprint(values: Iterable[float]) -> tuple[float, ...]:
    """Create a stable key for one complete vector of BBH scores."""
    return tuple(round(float(value), 12) for value in values)


def load_scores(scores_csv: Path) -> tuple[pd.DataFrame, list[str]]:
    scores = pd.read_csv(scores_csv)
    required = {"dataset_id", "model", "score"}
    missing = required - set(scores.columns)
    if missing:
        raise ValueError(f"Missing columns in {scores_csv}: {sorted(missing)}")

    scores = scores.loc[:, ["dataset_id", "model", "score"]].copy()
    scores["score"] = pd.to_numeric(scores["score"], errors="raise")
    if not scores["score"].map(math.isfinite).all():
        raise ValueError("The BBH score table contains non-finite values.")

    # This is the same duplicate handling used in the paper's R analysis.
    scores = (
        scores.groupby(["dataset_id", "model"], as_index=False, sort=True)["score"]
        .mean()
        .sort_values(["dataset_id", "model"])
    )
    wide = scores.pivot(index="dataset_id", columns="model", values="score").sort_index()

    if wide.shape[0] != EXPECTED_TASKS:
        raise ValueError(
            f"Expected {EXPECTED_TASKS} BBH tasks, found {wide.shape[0]}."
        )
    if wide.shape[1] != EXPECTED_MODELS:
        raise ValueError(
            f"Expected {EXPECTED_MODELS} BBH model entries, found {wide.shape[1]}."
        )
    if wide.isna().any().any():
        raise ValueError("The BBH score matrix is not complete.")
    if ((wide < 0) | (wide > 1)).any().any():
        raise ValueError("BBH scores must be between 0 and 1.")

    return wide, wide.index.tolist()


def result_score_vector(
    result: dict, dataset_ids: list[str]
) -> tuple[float, ...] | None:
    values: list[float] = []
    result_scores = result.get("results", {})
    for dataset_id in dataset_ids:
        task = result_scores.get(f"leaderboard_{dataset_id}")
        if not isinstance(task, dict):
            return None
        value = task.get("acc_norm,none")
        if not isinstance(value, (int, float)) or not math.isfinite(value):
            return None
        values.append(float(value))
    return fingerprint(values)


def load_parameter_counts(
    results_root: Path,
    dataset_ids: list[str],
    needed_fingerprints: set[tuple[float, ...]],
) -> tuple[
    dict[tuple[str, tuple[float, ...]], int],
    dict[tuple[float, ...], set[int]],
]:
    counts_by_model_and_fingerprint: dict[
        tuple[str, tuple[float, ...]], set[int]
    ] = defaultdict(set)
    counts_by_fingerprint: dict[tuple[float, ...], set[int]] = defaultdict(set)
    result_files = sorted(results_root.rglob("results_*.json"))
    if not result_files:
        raise FileNotFoundError(f"No results_*.json files found under {results_root}")

    for path in result_files:
        try:
            with path.open("r", encoding="utf-8") as stream:
                result = json.load(stream)
        except (OSError, json.JSONDecodeError):
            continue

        score_fingerprint = result_score_vector(result, dataset_ids)
        if score_fingerprint not in needed_fingerprints:
            continue

        value = result.get("config", {}).get("model_num_parameters")
        model_name = result.get("model_name")
        if (
            not isinstance(value, (int, float))
            or not math.isfinite(value)
            or value <= 0
            or not float(value).is_integer()
        ):
            continue
        if not isinstance(model_name, str) or not model_name:
            continue
        parameter_count = int(round(value))
        counts_by_model_and_fingerprint[(model_name, score_fingerprint)].add(
            parameter_count
        )
        counts_by_fingerprint[score_fingerprint].add(parameter_count)

    resolved: dict[tuple[str, tuple[float, ...]], int] = {}
    missing = needed_fingerprints - set(counts_by_fingerprint)
    if missing:
        raise ValueError(
            "No parameter count was found for "
            f"{len(missing)} of {len(needed_fingerprints)} score vectors."
        )

    conflicts = {
        key: values
        for key, values in counts_by_model_and_fingerprint.items()
        if len(values) != 1
    }
    if conflicts:
        raise ValueError(
            "Conflicting parameter counts were found for "
            f"{len(conflicts)} model and score-vector pairs."
        )

    for key, values in counts_by_model_and_fingerprint.items():
        resolved[key] = next(iter(values))
    return resolved, counts_by_fingerprint


def attach_model_size(
    robustness_csv: Path,
    score_matrix: pd.DataFrame,
    parameter_counts: dict[tuple[str, tuple[float, ...]], int],
    parameter_counts_by_fingerprint: dict[tuple[float, ...], set[int]],
    expected_rows: int,
) -> pd.DataFrame:
    robustness = pd.read_csv(robustness_csv)
    required = {"model", *RAW_COLUMNS, *NORMALIZED_COLUMNS}
    missing = required - set(robustness.columns)
    if missing:
        raise ValueError(f"Missing columns in {robustness_csv}: {sorted(missing)}")
    if len(robustness) != expected_rows:
        raise ValueError(
            f"Expected {expected_rows} rows in {robustness_csv}, "
            f"found {len(robustness)}."
        )
    if robustness["model"].duplicated().any():
        raise ValueError(f"Duplicate model identifiers in {robustness_csv}.")

    unknown = set(robustness["model"]) - set(score_matrix.columns)
    if unknown:
        raise ValueError(
            f"{len(unknown)} robustness models are missing from the BBH score table."
        )

    rows = []
    for model in robustness["model"]:
        values = score_matrix[model].to_numpy()
        score_fingerprint = fingerprint(values)
        exact_key = (model, score_fingerprint)
        if exact_key in parameter_counts:
            parameter_count = parameter_counts[exact_key]
        else:
            # If an evaluation result uses a different form of the model name,
            # the complete BBH score vector can still identify it.
            candidates = parameter_counts_by_fingerprint.get(score_fingerprint, set())
            if len(candidates) != 1:
                raise ValueError(
                    f"No unique parameter count found for {model}; "
                    f"found {len(candidates)} candidates."
                )
            parameter_count = next(iter(candidates))
        rows.append(
            {
                "model": model,
                "model_num_parameters": parameter_count,
                "default_mean_score": float(score_matrix[model].mean()),
            }
        )

    metadata = pd.DataFrame(rows)
    combined = robustness.merge(
        metadata, on="model", how="left", validate="one_to_one"
    )
    if combined["model_num_parameters"].isna().any():
        raise ValueError("Missing parameter counts after the merge.")
    if (combined["model_num_parameters"] <= 0).any():
        raise ValueError("All parameter counts must be positive.")

    combined["size_b"] = combined["model_num_parameters"] / 1_000_000_000
    combined["size_group"] = pd.cut(
        combined["size_b"],
        bins=SIZE_BINS,
        labels=SIZE_LABELS,
        right=True,
        include_lowest=True,
    )
    if combined["size_group"].isna().any():
        raise ValueError("At least one model was not assigned to a size group.")
    return combined


def format_number(value: object, decimals: int = 3) -> str:
    if pd.isna(value):
        return ""
    number = float(value)
    if number.is_integer():
        return str(int(number))
    return f"{number:.{decimals}f}".rstrip("0").rstrip(".")


def markdown_table(
    frame: pd.DataFrame,
    headers: list[str],
    decimal_columns: set[str] | None = None,
) -> str:
    decimal_columns = decimal_columns or set()
    rows = []
    for _, row in frame.iterrows():
        rendered = []
        for column in frame.columns:
            value = row[column]
            if column in decimal_columns:
                rendered.append(format_number(value))
            else:
                rendered.append(str(value))
        rows.append(rendered)

    output = [
        "| " + " | ".join(headers) + " |",
        "|" + "|".join(["---"] + ["---:" for _ in headers[1:]]) + "|",
    ]
    output.extend("| " + " | ".join(row) + " |" for row in rows)
    return "\n".join(output)


def median_table(data: pd.DataFrame, columns: dict[str, str]) -> pd.DataFrame:
    grouped = data.groupby("size_group", observed=False)
    table = grouped.agg(
        Entries=("model", "size"),
        **{label: (column, "median") for column, label in columns.items()},
    ).reindex(SIZE_LABELS)
    table = table.reset_index()
    table = table.rename(columns={"size_group": "Model size"})
    table["Model size"] = table["Model size"].astype(str)
    return table


def correlation_table(data: pd.DataFrame) -> pd.DataFrame:
    rows = [
        {
            "Aggregation rule": label,
            "Spearman’s ρ": data["size_b"].rank().corr(data[column].rank()),
        }
        for column, label in RAW_COLUMNS.items()
    ]
    return pd.DataFrame(rows)


def print_analysis(title: str, data: pd.DataFrame) -> None:
    print(f"\n## {title}\n")

    raw = median_table(data, RAW_COLUMNS)
    print("### Median robustness by model size\n")
    print(
        markdown_table(
            raw,
            list(raw.columns),
            decimal_columns=set(raw.columns) - {"Model size", "Entries"},
        )
    )

    correlations = correlation_table(data)
    print("\n### Correlation between model size and robustness\n")
    print(
        markdown_table(
            correlations,
            list(correlations.columns),
            decimal_columns={"Spearman’s ρ"},
        )
    )

    size_score_rho = data["size_b"].rank().corr(data["default_mean_score"].rank())
    print(
        "\nSpearman’s ρ between model size and the default mean score: "
        f"{size_score_rho:.3f}."
    )

    normalized = median_table(data, NORMALIZED_COLUMNS)
    print("\n### Median normalized robustness by model size\n")
    print(
        markdown_table(
            normalized,
            list(normalized.columns),
            decimal_columns=set(normalized.columns) - {"Model size", "Entries"},
        )
    )


def main() -> None:
    args = parse_args()
    require_file(args.robustness_csv, "Full-BBH robustness file")
    require_file(args.scores_csv, "BBH score file")
    require_directory(args.results_root, "Open LLM Leaderboard results snapshot")

    score_matrix, dataset_ids = load_scores(args.scores_csv)
    needed_fingerprints = {
        fingerprint(score_matrix[model].to_numpy()) for model in score_matrix.columns
    }
    parameter_counts, parameter_counts_by_fingerprint = load_parameter_counts(
        args.results_root, dataset_ids, needed_fingerprints
    )

    full = attach_model_size(
        args.robustness_csv,
        score_matrix,
        parameter_counts,
        parameter_counts_by_fingerprint,
        expected_rows=EXPECTED_MODELS,
    )

    full[["model", "model_num_parameters"]].to_csv(
        args.scores_csv.with_name("bbh_sizes.csv"),
        index=False,
    )
    
    print_analysis("Full BBH: 4507 model entries", full)

    if args.namespace_robustness_csv.is_file():
        namespace = attach_model_size(
            args.namespace_robustness_csv,
            score_matrix,
            parameter_counts,
            parameter_counts_by_fingerprint,
            expected_rows=EXPECTED_NAMESPACES,
        )
        print_analysis("BBH: one model per namespace", namespace)
    else:
        print(
            "\nThe one-model-per-namespace file was not found; "
            "the sensitivity analysis was skipped:\n"
            f"{args.namespace_robustness_csv}"
        )


if __name__ == "__main__":
    main()
