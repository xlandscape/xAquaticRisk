import argparse
import html
import os
from datetime import datetime, timezone

import geopandas as gpd
import numpy as np
import pandas as pd
from pyproj import CRS


def default_renderer_xml(geometry_type: str) -> str:
    if geometry_type == "line":
        return """
      <renderer-v2 type="singleSymbol" symbollevels="0" referencescale="-1" forceraster="0" enableorderby="0">
        <symbols>
          <symbol force_rhr="0" frame_rate="10" type="line" clip_to_extent="1" alpha="1" is_animated="0" name="0">
            <layer class="SimpleLine" enabled="1" locked="0" pass="0">
              <Option type="Map">
                <Option type="QString" name="capstyle" value="square"/>
                <Option type="QString" name="customdash" value="5;2"/>
                <Option type="QString" name="joinstyle" value="bevel"/>
                <Option type="QString" name="line_color" value="31,61,122,255"/>
                <Option type="QString" name="line_style" value="solid"/>
                <Option type="QString" name="line_width" value="0.5"/>
              </Option>
            </layer>
          </symbol>
        </symbols>
      </renderer-v2>
        """

    return """
      <renderer-v2 type="singleSymbol" symbollevels="0" referencescale="-1" forceraster="0" enableorderby="0">
        <symbols>
          <symbol force_rhr="0" frame_rate="10" type="fill" clip_to_extent="1" alpha="1" is_animated="0" name="0">
            <layer class="SimpleFill" enabled="1" locked="0" pass="0">
              <Option type="Map">
                <Option type="QString" name="border_color" value="35,35,35,255"/>
                <Option type="QString" name="border_style" value="solid"/>
                <Option type="QString" name="border_width" value="0.1"/>
                <Option type="QString" name="color" value="230,230,230,120"/>
              </Option>
            </layer>
          </symbol>
        </symbols>
      </renderer-v2>
    """


def graduated_renderer_xml(values: pd.Series) -> str:
    vals = pd.to_numeric(values, errors="coerce").fillna(0.0)
    vmax = float(vals.max())
    if vmax <= 0:
        return default_renderer_xml("polygon")

    # Quantile bins on positive values keep visual contrast for skewed distributions.
    positive = vals[vals > 0]
    quantiles = np.quantile(positive.to_numpy(), [0.0, 0.2, 0.4, 0.6, 0.8, 1.0])
    quantiles = np.maximum.accumulate(quantiles)
    if len(np.unique(quantiles)) < 2:
        quantiles = np.linspace(float(positive.min()), float(positive.max()), 6)

    # YlOrRd-like palette from light to dark.
    colors = [
        "255,255,204,220",
        "255,237,160,220",
        "254,217,118,220",
        "254,178,76,220",
        "253,141,60,220",
        "240,59,32,220",
    ]

    symbols = []
    ranges = []

    # Include a separate zero class.
    symbols.append(
        """
          <symbol force_rhr="0" frame_rate="10" type="fill" clip_to_extent="1" alpha="1" is_animated="0" name="0">
            <layer class="SimpleFill" enabled="1" locked="0" pass="0">
              <Option type="Map">
                <Option type="QString" name="border_color" value="140,140,140,255"/>
                <Option type="QString" name="border_style" value="solid"/>
                <Option type="QString" name="border_width" value="0.08"/>
                <Option type="QString" name="color" value="240,240,240,140"/>
              </Option>
            </layer>
          </symbol>
        """
    )
    ranges.append(
        "<range render=\"true\" lower=\"0\" upper=\"0\" symbol=\"0\" label=\"0\"/>"
    )

    for i in range(5):
        lower = float(quantiles[i])
        upper = float(quantiles[i + 1])
        if upper < lower:
            upper = lower
        symbol_name = str(i + 1)
        color = colors[min(i + 1, len(colors) - 1)]
        symbols.append(
            f"""
          <symbol force_rhr="0" frame_rate="10" type="fill" clip_to_extent="1" alpha="1" is_animated="0" name="{symbol_name}">
            <layer class="SimpleFill" enabled="1" locked="0" pass="0">
              <Option type="Map">
                <Option type="QString" name="border_color" value="90,90,90,255"/>
                <Option type="QString" name="border_style" value="solid"/>
                <Option type="QString" name="border_width" value="0.08"/>
                <Option type="QString" name="color" value="{color}"/>
              </Option>
            </layer>
          </symbol>
            """
        )
        ranges.append(
            f"<range render=\"true\" lower=\"{lower:.12f}\" upper=\"{upper:.12f}\" symbol=\"{symbol_name}\" label=\"{lower:.4f} - {upper:.4f}\"/>"
        )

    return f"""
      <renderer-v2 type="graduatedSymbol" attr="total_contribution" graduatedMethod="GraduatedColor" symbollevels="0" referencescale="-1" forceraster="0" enableorderby="0">
        <ranges>
          {''.join(ranges)}
        </ranges>
        <symbols>
          {''.join(symbols)}
        </symbols>
        <source-symbol>
          <symbol force_rhr="0" frame_rate="10" type="fill" clip_to_extent="1" alpha="1" is_animated="0" name="source">
            <layer class="SimpleFill" enabled="1" locked="0" pass="0">
              <Option type="Map">
                <Option type="QString" name="border_color" value="80,80,80,255"/>
                <Option type="QString" name="border_style" value="solid"/>
                <Option type="QString" name="border_width" value="0.08"/>
                <Option type="QString" name="color" value="255,255,204,220"/>
              </Option>
            </layer>
          </symbol>
        </source-symbol>
        <colorramp type="gradient" name="YlOrRd">
          <Option type="Map">
            <Option type="QString" name="color1" value="255,255,204,255"/>
            <Option type="QString" name="color2" value="240,59,32,255"/>
            <Option type="QString" name="discrete" value="0"/>
          </Option>
        </colorramp>
        <classificationMethod id="Quantile">
          <symmetricMode enabled="0" symmetryPoint="0" astride="0"/>
        </classificationMethod>
      </renderer-v2>
    """


def qgs_layer_xml(
    layer_id: str,
    name: str,
    gpkg_path: str,
    table_name: str,
    geometry_col: str,
    crs_authid: str,
    srs_xml: str,
    geometry_type: str = "polygon",
    renderer_xml: str | None = None,
) -> str:
    escaped_path = gpkg_path.replace('\\', '/')
    datasource = f"{escaped_path}|layername={table_name}"
    geom_name = "NoGeometry" if geometry_col == "" else ("Line" if geometry_type == "line" else "Polygon")
    layer_geometry_type = "4" if geometry_col == "" else ("1" if geometry_type == "line" else "2")
    geometry_type_code = "0" if geometry_col == "" else ("1" if geometry_type == "line" else "2")
    if renderer_xml is None:
        renderer_xml = default_renderer_xml(geometry_type)

    return f"""
    <maplayer geometry="{geom_name}" type="vector" simplifyDrawingHints="1" simplifyAlgorithm="0" hasScaleBasedVisibilityFlag="0" simplifyDrawingTol="1" autoRefreshEnabled="0" maxScale="0" simplifyMaxScale="1" labelsEnabled="0" refreshOnNotifyEnabled="0" styleCategories="AllStyleCategories" minScale="100000000" autoRefreshTime="0">
      <id>{layer_id}</id>
      <datasource>{datasource}</datasource>
      <layername>{name}</layername>
      <provider encoding="UTF-8">ogr</provider>
      <layerGeometryType>{layer_geometry_type}</layerGeometryType>
      <srs>
        {srs_xml}
      </srs>
      <resourceMetadata/>
      <fieldConfiguration/>
      <aliases/>
      <defaults/>
      <constraints/>
      <constraintExpressions/>
      <expressionfields/>
      <attributeactions>
        <defaultAction key="Canvas" value=""/>
      </attributeactions>
      <attributetableconfig actionWidgetStyle="dropDown">
        <columns/>
      </attributetableconfig>
      <editform tolerant="1"></editform>
      <editforminitcode><![CDATA[# -*- coding: utf-8 -*-]]></editforminitcode>
      <featformsuppress>0</featformsuppress>
      <editorlayout>generatedlayout</editorlayout>
      <editable/>
      <labelOnTop/>
      <reuseLastValue/>
      <dataDefinedFieldProperties/>
      <widgets/>
      <previewExpression></previewExpression>
      <mapTip></mapTip>
      <layerOpacity>1</layerOpacity>
      <blendMode>0</blendMode>
      <featureBlendMode>0</featureBlendMode>
      <layerFlags>2049</layerFlags>
      <geometryOptions removeDuplicateNodes="0" geometryPrecision="0" activeChecks=""/>
      <legend type="default-vector" showLabelLegend="0"/>
      <referencedLayers/>
      <fielddomains/>
      {renderer_xml}
      <customproperties>
        <property key="embeddedWidgets/count" value="0"/>
      </customproperties>
      <geometryType>{geometry_type_code}</geometryType>
    </maplayer>
    """


def qgs_spatialrefsys_xml(crs: CRS) -> str:
    auth = crs.to_authority()
    authid = f"{auth[0]}:{auth[1]}" if auth else crs.to_string()
    description = crs.name or authid
    wkt = crs.to_wkt("WKT2_2019")
    proj4 = crs.to_proj4() or ""
    is_geographic = "true" if crs.is_geographic else "false"
    return f"""
      <spatialrefsys>
        <authid>{html.escape(authid)}</authid>
        <description>{html.escape(description)}</description>
        <projectionacronym></projectionacronym>
        <ellipsoidacronym></ellipsoidacronym>
        <proj4>{html.escape(proj4)}</proj4>
        <wkt>{html.escape(wkt)}</wkt>
        <geographicflag>{is_geographic}</geographicflag>
      </spatialrefsys>
    """


def main() -> None:
    parser = argparse.ArgumentParser(description="Create GeoPackage + QGIS project for field ranking workflow")
    parser.add_argument("--run_path", required=True)
    parser.add_argument("--scenario_path", required=True)
    parser.add_argument("--ranking_csv", default=None)
    parser.add_argument("--project_name", default="field_ranking_qgis_project")
    args = parser.parse_args()

    run_path = os.path.normpath(args.run_path)
    scenario_path = os.path.normpath(args.scenario_path)
    ranking_csv = os.path.normpath(args.ranking_csv) if args.ranking_csv else os.path.join(run_path, "field_ranking.csv")

    lulc_path = os.path.join(scenario_path, "geo", "LULC.shp")
    reach_path = os.path.join(scenario_path, "geo", "Reachlist_shp.shp")

    if not os.path.isfile(ranking_csv):
        raise FileNotFoundError(f"Missing ranking_csv: {ranking_csv}")
    if not os.path.isfile(lulc_path):
        raise FileNotFoundError(f"Missing LULC shapefile: {lulc_path}")
    if not os.path.isfile(reach_path):
        raise FileNotFoundError(f"Missing stream network shapefile: {reach_path}")

    ranking_df = pd.read_csv(ranking_csv)
    ranking_df["field_id"] = ranking_df["field_id"].astype(str)

    lulc = gpd.read_file(lulc_path)
    reaches = gpd.read_file(reach_path)

    arable = lulc[lulc["LULCTypeID"] == 222].copy()
    arable["field_id"] = arable["ALVID"].astype(str)

    field_ranking = arable.merge(
        ranking_df,
        on="field_id",
        how="left",
        suffixes=("_lulc", ""),
    )

    # Keep a clean set of columns first, then all remaining ranking attributes.
    preferred_cols = [
        "field_id",
        "ALVID",
        "LULCTypeID",
        "key",
        "sd_cov",
        "total_contribution",
        "contribution_unit",
        "scenario_id",
        "run_id",
        "time_start",
        "time_end",
        "dataset_version",
        "generated_at",
        "source_run_path",
        "source_scenario_path",
        "field_type",
        "applied",
        "geometry",
    ]
    cols = [c for c in preferred_cols if c in field_ranking.columns] + [
        c for c in field_ranking.columns if c not in preferred_cols
    ]
    field_ranking = field_ranking[cols].copy()

    # Output locations
    gpkg_path = os.path.join(run_path, f"{args.project_name}.gpkg")
    qgs_path = os.path.join(run_path, f"{args.project_name}.qgs")

    # Overwrite existing outputs for reproducibility.
    if os.path.exists(gpkg_path):
        os.remove(gpkg_path)

    field_ranking.to_file(gpkg_path, layer="field_ranking", driver="GPKG")
    lulc.to_file(gpkg_path, layer="lulc", driver="GPKG")
    reaches.to_file(gpkg_path, layer="stream_network", driver="GPKG")

    if not field_ranking.crs:
      raise ValueError("field_ranking layer has no CRS; cannot build reliable QGIS project")
    pyproj_crs = CRS.from_user_input(field_ranking.crs)
    auth = pyproj_crs.to_authority()
    crs_authid = f"{auth[0]}:{auth[1]}" if auth else pyproj_crs.to_string()
    spatialrefsys_xml = qgs_spatialrefsys_xml(pyproj_crs)

    project_time = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    field_layer_id = "field_ranking_layer"
    lulc_layer_id = "lulc_layer"
    stream_layer_id = "stream_layer"
    field_renderer = graduated_renderer_xml(field_ranking.get("total_contribution", pd.Series([0.0])))

    qgs_xml = f"""<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<qgis projectname=\"Field Ranking QGIS Project\" version=\"3.34.0-Prizren\" saveDateTime=\"{project_time}\">
  <homePath path=\".\"/>
  <title>Field Ranking QGIS Project</title>
  <projectCrs>
    {spatialrefsys_xml}
  </projectCrs>
  <layer-tree-group>
    <customproperties/>
    <layer-tree-layer id=\"{field_layer_id}\" checked=\"Qt::Checked\" expanded=\"1\" source=\"{gpkg_path.replace('\\', '/')}|layername=field_ranking\" providerKey=\"ogr\" name=\"field_ranking\"/>
    <layer-tree-layer id=\"{stream_layer_id}\" checked=\"Qt::Checked\" expanded=\"1\" source=\"{gpkg_path.replace('\\', '/')}|layername=stream_network\" providerKey=\"ogr\" name=\"stream_network\"/>
    <layer-tree-layer id=\"{lulc_layer_id}\" checked=\"Qt::Unchecked\" expanded=\"1\" source=\"{gpkg_path.replace('\\', '/')}|layername=lulc\" providerKey=\"ogr\" name=\"lulc\"/>
  </layer-tree-group>
  <mapcanvas>
    <units>meters</units>
    <extent>
      <xmin>{field_ranking.total_bounds[0]}</xmin>
      <ymin>{field_ranking.total_bounds[1]}</ymin>
      <xmax>{field_ranking.total_bounds[2]}</xmax>
      <ymax>{field_ranking.total_bounds[3]}</ymax>
    </extent>
    <rotation>0</rotation>
    <destinationsrs>
      {spatialrefsys_xml}
    </destinationsrs>
  </mapcanvas>
  <projectlayers>
    {qgs_layer_xml(field_layer_id, 'field_ranking', gpkg_path, 'field_ranking', 'geometry', crs_authid, spatialrefsys_xml, geometry_type='polygon', renderer_xml=field_renderer)}
    {qgs_layer_xml(stream_layer_id, 'stream_network', gpkg_path, 'stream_network', 'geometry', crs_authid, spatialrefsys_xml, geometry_type='line')}
    {qgs_layer_xml(lulc_layer_id, 'lulc', gpkg_path, 'lulc', 'geometry', crs_authid, spatialrefsys_xml, geometry_type='polygon')}
  </projectlayers>
  <layerorder>
    <layer id=\"field_ranking_layer\"/>
    <layer id=\"stream_layer\"/>
    <layer id=\"lulc_layer\"/>
  </layerorder>
</qgis>
"""

    with open(qgs_path, "w", encoding="utf-8") as f:
        f.write(qgs_xml)

    print(f"GeoPackage created: {gpkg_path}")
    print(f"QGIS project created: {qgs_path}")


if __name__ == "__main__":
    main()
