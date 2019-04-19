import os
import xml.etree.ElementTree as ET
from xml.dom import minidom

data_folder = 'data'

def get_data_file(file: str) -> str:
    return os.path.join(data_folder, file)

def get_libs_file() -> str:
    return os.path.join(data_folder, 'libs.xml')

def get_source_file() -> str:
    return os.path.join(data_folder, 'source.c')

def get_res_file() -> str:
    return os.path.join(data_folder, 'res.xml')

def get_bench_folder() -> str:
    return '/home/alex/Documents/VPL-Experiments/polyhedra_generator/benchmark/sphere'

def export(tree: ET.ElementTree, file: str) -> None:
    root = tree.getroot()
    xmlstr = minidom.parseString(ET.tostring(root)).toprettyxml(indent="    ")
    with open(file, "w") as f:
        f.write(xmlstr)
