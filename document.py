"""
Script for documenting the Landscape Model xAquaticRisk variant.
"""
import os
import base.documentation

root_folder = os.path.abspath(os.path.join(os.path.dirname(base.__file__), ".."))
base.documentation.write_contribution_notes(os.path.join(root_folder, "..", "..", "CONTRIBUTING.md"))
