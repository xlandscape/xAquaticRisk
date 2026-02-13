"""
Simple web server for parameterizing and running the xAquatic model.
"""
import os
import sys
import json
import subprocess
import xml.etree.ElementTree as ET
from http.server import HTTPServer, SimpleHTTPRequestHandler
import threading
import re

PORT = 8080
BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
TEMPLATE_PATH = os.path.join(BASE_DIR, "template.xrun")
OUTPUT_DIR = BASE_DIR  # Save .xrun files in root directory (paths are relative to xrun location)
START_BAT = os.path.join(BASE_DIR, "__start__.bat")


def parse_xrun_template(template_path: str) -> dict:
    """
    Parse the template.xrun file and extract parameters with their metadata.
    """
    parameters = {}
    
    try:
        with open(template_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        tree = ET.parse(template_path)
        root = tree.getroot()
        
        # Define namespace
        ns = {'x': 'urn:xAquaticRisk'}
        
        # Extract all elements recursively
        def extract_params(element, prefix=""):
            for child in element:
                # Get local name (without namespace)
                tag = child.tag.replace('{urn:xAquaticRisk}', '')
                full_key = f"{prefix}{tag}" if prefix else tag
                
                if len(child) > 0:
                    # Has children - recurse
                    extract_params(child, f"{full_key}/")
                else:
                    # Leaf element - extract value and find comment
                    value = child.text.strip() if child.text else ""
                    
                    # Try to find the comment before this element
                    description = ""
                    values_hint = ""
                    remark = ""
                    
                    # Search for comment in content
                    pattern = rf'<!--\s*Parameter\s*:\s*{tag}\s*Description\s*:\s*(.*?)\s*Values\s*:\s*(.*?)\s*(?:Remark\s*:\s*(.*?))?\s*-->'
                    match = re.search(pattern, content, re.DOTALL | re.IGNORECASE)
                    if match:
                        description = match.group(1).strip()
                        values_hint = match.group(2).strip()
                        if match.group(3):
                            remark = match.group(3).strip()
                    
                    parameters[full_key] = {
                        "value": value,
                        "tag": tag,
                        "description": description,
                        "values_hint": values_hint,
                        "remark": remark
                    }
        
        extract_params(root)
        
    except Exception as e:
        print(f"Error parsing template: {e}")
        import traceback
        traceback.print_exc()
    
    return parameters


def create_xrun_file(parameters: dict, output_path: str, template_path: str) -> str:
    """
    Create a new .xrun file from parameters, preserving the template structure.
    """
    # Parse the template to get the structure
    tree = ET.parse(template_path)
    root = tree.getroot()
    
    # Update values in the tree
    def update_params(element, prefix=""):
        for child in element:
            tag = child.tag.replace('{urn:xAquaticRisk}', '')
            full_key = f"{prefix}{tag}" if prefix else tag
            
            if len(child) > 0:
                update_params(child, f"{full_key}/")
            else:
                if full_key in parameters:
                    child.text = parameters[full_key]
    
    update_params(root)
    
    # Write the file
    tree.write(output_path, encoding="utf-8", xml_declaration=True)
    
    return output_path


class XAquaticHandler(SimpleHTTPRequestHandler):
    """HTTP request handler for the xAquatic web interface."""
    
    def __init__(self, *args, **kwargs):
        self.webui_dir = os.path.dirname(os.path.abspath(__file__))
        super().__init__(*args, directory=self.webui_dir, **kwargs)
    
    def do_GET(self):
        if self.path == "/" or self.path == "/index.html":
            self.send_response(200)
            self.send_header("Content-type", "text/html")
            self.end_headers()
            with open(os.path.join(self.webui_dir, "index.html"), "rb") as f:
                self.wfile.write(f.read())
        elif self.path == "/api/template":
            self.send_response(200)
            self.send_header("Content-type", "application/json")
            self.end_headers()
            params = parse_xrun_template(TEMPLATE_PATH)
            self.wfile.write(json.dumps(params).encode())
        elif self.path == "/api/scenarios":
            self.send_response(200)
            self.send_header("Content-type", "application/json")
            self.end_headers()
            scenarios = self.get_available_scenarios()
            self.wfile.write(json.dumps(scenarios).encode())
        else:
            super().do_GET()
    
    def do_POST(self):
        if self.path == "/api/run":
            content_length = int(self.headers["Content-Length"])
            post_data = self.rfile.read(content_length)
            params = json.loads(post_data.decode())
            
            try:
                # Get SimID for filename
                sim_id = params.get("Scenario/SimID", params.get("SimID", "Simulation"))
                output_filename = f"{sim_id}.xrun"
                output_path = os.path.join(OUTPUT_DIR, output_filename)
                
                # Create the .xrun file
                create_xrun_file(params, output_path, TEMPLATE_PATH)
                
                # Run the model in a separate thread
                def run_model():
                    subprocess.Popen(
                        f'"{START_BAT}" "{output_path}"',
                        cwd=BASE_DIR,
                        shell=True
                    )
                
                thread = threading.Thread(target=run_model)
                thread.start()
                
                self.send_response(200)
                self.send_header("Content-type", "application/json")
                self.end_headers()
                response = {
                    "status": "success",
                    "message": f"Model started with configuration: {output_filename}",
                    "xrun_path": output_path
                }
                self.wfile.write(json.dumps(response).encode())
            except Exception as e:
                self.send_response(500)
                self.send_header("Content-type", "application/json")
                self.end_headers()
                response = {"status": "error", "message": str(e)}
                self.wfile.write(json.dumps(response).encode())
        
        elif self.path == "/api/save":
            content_length = int(self.headers["Content-Length"])
            post_data = self.rfile.read(content_length)
            params = json.loads(post_data.decode())
            
            try:
                sim_id = params.get("Scenario/SimID", params.get("SimID", "Simulation"))
                output_filename = f"{sim_id}.xrun"
                output_path = os.path.join(OUTPUT_DIR, output_filename)
                
                create_xrun_file(params, output_path, TEMPLATE_PATH)
                
                self.send_response(200)
                self.send_header("Content-type", "application/json")
                self.end_headers()
                response = {
                    "status": "success",
                    "message": f"Configuration saved to: {output_filename}",
                    "xrun_path": output_path
                }
                self.wfile.write(json.dumps(response).encode())
            except Exception as e:
                self.send_response(500)
                self.send_header("Content-type", "application/json")
                self.end_headers()
                response = {"status": "error", "message": str(e)}
                self.wfile.write(json.dumps(response).encode())
    
    def get_available_scenarios(self) -> list:
        """Get list of available scenarios."""
        scenario_dir = os.path.join(BASE_DIR, "scenario")
        scenarios = []
        if os.path.exists(scenario_dir):
            for item in os.listdir(scenario_dir):
                item_path = os.path.join(scenario_dir, item)
                if os.path.isdir(item_path):
                    scenarios.append({
                        "name": item,
                        "path": f"scenario/{item}"
                    })
        return scenarios


def main():
    """Start the web server."""
    server_address = ("", PORT)
    httpd = HTTPServer(server_address, XAquaticHandler)
    
    print("=" * 60)
    print("xAquatic Model Web Interface")
    print("=" * 60)
    print(f"Server running at: http://localhost:{PORT}")
    print(f"Template file: {TEMPLATE_PATH}")
    print(f"Output directory: {OUTPUT_DIR}")
    print("Press Ctrl+C to stop the server")
    print("=" * 60)
    
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down server...")
        httpd.shutdown()


if __name__ == "__main__":
    main()
