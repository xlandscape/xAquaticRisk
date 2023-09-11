"""
Script for documenting the Landscape Model xAquaticRisk variant.
"""
import os
import base.documentation
import json
import distutils.version

root_folder = os.path.abspath(os.path.join(os.path.dirname(base.__file__)))
with open(os.path.join(root_folder, "..", "..", "..", "model.json")) as f:
    version = distutils.version.StrictVersion(json.load(f)["version"])
base.documentation.check_variant_parts(root_folder)
base.documentation.write_contribution_notes(os.path.join(root_folder, "..", "..", "..", "CONTRIBUTING.md"))
base.documentation.write_repository_info(
    os.path.join(root_folder, "..", "..", ".."),
    os.path.join(root_folder, "..", "..", "..", "repository.json"),
    os.path.join(root_folder, "..", "..", "..", "..", "versions.json"),
    "model"
)
base.documentation.write_latest_version_info(os.path.join(root_folder, "..", "..", "..", "latest_versions.json"))
