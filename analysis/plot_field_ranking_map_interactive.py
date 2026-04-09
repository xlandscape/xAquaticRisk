import argparse
import os

import geopandas as gpd
import pandas as pd


def to_web_mercator(gdf: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    if gdf.crs is None:
        # LULC shapefiles in this project are metric projected; assume ETRS89 / UTM 32N.
        gdf = gdf.set_crs(epsg=25832, allow_override=True)
    return gdf.to_crs(epsg=4326)


def build_popup(row: pd.Series) -> str:
    return (
        f"<b>Field ID:</b> {row['field_id']}<br>"
        f"<b>Total contribution:</b> {row['total_contribution']:.6f} {row['contribution_unit']}"
    )


def main() -> None:
    parser = argparse.ArgumentParser(description="Create an interactive field ranking map (HTML).")
    parser.add_argument("--ranking_csv", required=True, help="Path to field_ranking.csv")
    parser.add_argument("--scenario_path", required=True, help="Path to scenario folder")
    parser.add_argument("--output_html", default=None, help="Output HTML path")
    parser.add_argument("--top_n", type=int, default=10, help="Top N fields to emphasize")
    args = parser.parse_args()

    ranking_csv = os.path.normpath(args.ranking_csv)
    scenario_path = os.path.normpath(args.scenario_path)
    lulc_path = os.path.join(scenario_path, "geo", "LULC.shp")

    if args.output_html:
        output_html = os.path.normpath(args.output_html)
    else:
        output_html = os.path.join(os.path.dirname(ranking_csv), "field_ranking_map_interactive.html")

    if not os.path.isfile(ranking_csv):
        raise FileNotFoundError(f"Ranking CSV not found: {ranking_csv}")
    if not os.path.isfile(lulc_path):
        raise FileNotFoundError(f"LULC shapefile not found: {lulc_path}")

    df = pd.read_csv(ranking_csv)
    df["field_id"] = df["field_id"].astype(str)

    gdf = gpd.read_file(lulc_path)
    arable = gdf[gdf["LULCTypeID"] == 222].copy()
    arable["field_id"] = arable["ALVID"].astype(str)

    merged = arable.merge(
        df[["field_id", "total_contribution", "contribution_unit"]],
        on="field_id",
        how="left",
    )
    merged["total_contribution"] = merged["total_contribution"].fillna(0.0)
    merged["contribution_unit"] = merged["contribution_unit"].fillna("g/ha")

    merged = to_web_mercator(merged)

    top = (
        merged[merged["total_contribution"] > 0]
        .sort_values(["total_contribution", "field_id"], ascending=[False, True])
        .head(args.top_n)
        .copy()
    )

    import folium
    from branca.colormap import linear

    center = merged.unary_union.centroid
    m = folium.Map(location=[center.y, center.x], zoom_start=12, tiles="CartoDB positron")

    vmax = max(float(merged["total_contribution"].max()), 1e-12)
    cmap = linear.YlOrRd_09.scale(0, vmax)
    cmap.caption = "Total contribution (g/ha)"

    def style_fn(feature):
        value = feature["properties"].get("total_contribution", 0.0) or 0.0
        return {
            "fillColor": cmap(value),
            "color": "#444444",
            "weight": 0.3,
            "fillOpacity": 0.72 if value > 0 else 0.08,
        }

    folium.GeoJson(
        merged,
        name="Arable fields contribution",
        style_function=style_fn,
        tooltip=folium.GeoJsonTooltip(
            fields=["field_id", "total_contribution", "contribution_unit"],
            aliases=["Field ID", "Total contribution", "Unit"],
            localize=True,
            sticky=False,
            labels=True,
        ),
    ).add_to(m)

    # Emphasize top N with thicker borders and permanent labels.
    if not top.empty:
        folium.GeoJson(
            top,
            name=f"Top {args.top_n} fields",
            style_function=lambda _: {
                "fillColor": "#00000000",
                "color": "#0b2e6b",
                "weight": 2.0,
                "fillOpacity": 0.0,
            },
        ).add_to(m)

        for _, row in top.iterrows():
            c = row.geometry.centroid
            folium.Marker(
                location=[c.y, c.x],
                icon=folium.DivIcon(
                    html=(
                        "<div style='font-size:10px; font-weight:700; color:#0b2e6b; "
                        "background:rgba(255,255,255,0.85); padding:2px 4px; border-radius:3px;'>"
                        f"{row['field_id']}"
                        "</div>"
                    )
                ),
                tooltip=build_popup(row),
            ).add_to(m)

    cmap.add_to(m)
    folium.LayerControl(collapsed=False).add_to(m)

    os.makedirs(os.path.dirname(output_html), exist_ok=True)
    m.save(output_html)
    print(f"Interactive map saved: {output_html}")


if __name__ == "__main__":
    main()
