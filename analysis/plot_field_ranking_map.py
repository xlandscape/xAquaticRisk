import argparse
import os

import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd


def main():
    parser = argparse.ArgumentParser(description="Plot field ranking results on a scenario map.")
    parser.add_argument("--ranking_csv", required=True, help="Path to field_ranking.csv")
    parser.add_argument("--scenario_path", required=True, help="Path to scenario folder")
    parser.add_argument("--output_png", required=False, help="Path to output PNG")
    parser.add_argument("--top_n", type=int, default=10, help="Number of top fields to label")
    args = parser.parse_args()

    ranking_csv = os.path.normpath(args.ranking_csv)
    scenario_path = os.path.normpath(args.scenario_path)
    lulc_path = os.path.join(scenario_path, "geo", "LULC.shp")

    if not os.path.isfile(ranking_csv):
        raise FileNotFoundError(f"Ranking CSV not found: {ranking_csv}")
    if not os.path.isfile(lulc_path):
        raise FileNotFoundError(f"LULC shapefile not found: {lulc_path}")

    if args.output_png:
        output_png = os.path.normpath(args.output_png)
    else:
        output_png = os.path.join(os.path.dirname(ranking_csv), "field_ranking_map_top10.png")

    df = pd.read_csv(ranking_csv)
    gdf = gpd.read_file(lulc_path)

    # Keep arable polygons only and align key types.
    arable = gdf[gdf["LULCTypeID"] == 222].copy()
    arable["field_id"] = arable["ALVID"].astype(str)
    df["field_id"] = df["field_id"].astype(str)

    merged = arable.merge(
        df[["field_id", "total_contribution", "contribution_unit"]],
        on="field_id",
        how="left",
    )
    merged["total_contribution"] = merged["total_contribution"].fillna(0.0)

    top = (
        merged[merged["total_contribution"] > 0]
        .sort_values(["total_contribution", "field_id"], ascending=[False, True])
        .head(args.top_n)
        .copy()
    )

    fig, ax = plt.subplots(figsize=(12, 12))

    # Base layer: all arable fields in light gray.
    merged.plot(ax=ax, color="#f1f1f1", edgecolor="#c8c8c8", linewidth=0.1)

    # Choropleth for non-zero contributions.
    positive = merged[merged["total_contribution"] > 0]
    if not positive.empty:
        positive.plot(
            ax=ax,
            column="total_contribution",
            cmap="YlOrRd",
            linewidth=0.2,
            edgecolor="#8a8a8a",
            legend=True,
            legend_kwds={"label": "Total contribution (g/ha)", "shrink": 0.6},
        )

    # Highlight and label top-N fields.
    if not top.empty:
        top.plot(ax=ax, facecolor="none", edgecolor="#1f3d7a", linewidth=1.2)
        for _, row in top.iterrows():
            c = row.geometry.centroid
            ax.text(
                c.x,
                c.y,
                str(row["field_id"]),
                fontsize=8,
                color="#0b1d4d",
                ha="center",
                va="center",
                bbox={"boxstyle": "round,pad=0.15", "fc": "white", "ec": "none", "alpha": 0.8},
            )

    ax.set_title("Top Arable Fields by Spray-Drift Contribution", fontsize=14, pad=12)
    ax.set_axis_off()

    plt.tight_layout()
    os.makedirs(os.path.dirname(output_png), exist_ok=True)
    plt.savefig(output_png, dpi=220)
    plt.close(fig)

    print(f"Map saved: {output_png}")


if __name__ == "__main__":
    main()
