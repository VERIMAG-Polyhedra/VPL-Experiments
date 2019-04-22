import os
import xml.etree.ElementTree as ET
from xml.dom import minidom
from typing import List, Dict

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

def get_bench_file(n_cstr: int, n_dim: int) -> str:
    return 'P_%i_%i_0.vpl' % (n_dim, n_cstr)

def get_polyhedra(n_cstr: int, n_dim: int) -> Dict[str, List[str]]:
    file_name = os.path.join(get_bench_folder(), get_bench_file(n_cstr, n_dim))
    file = open(file_name, 'r')
    lines = file.read().split('\n')
    d = dict()
    p = []
    name = None
    for line in lines:
        if len(line) > 0 and line[0] == 'P':
            name = line
        elif name is not None:
            if line == '':
                d[name] = p
                name = None
                p = []
            else:
                p.append(line)
    if name is not None:
        d[name] = p
    return d

def export(tree: ET.ElementTree, file: str) -> None:
    root = tree.getroot()
    xmlstr = minidom.parseString(ET.tostring(root)).toprettyxml(indent="    ")
    with open(file, "w") as f:
        f.write(xmlstr)


def get_value_from_polyname(i):
    def f(polyname: str) -> float:
        return float(polyname.split('_')[i])
    return f

def value_from_var_list(s: str) -> float:
    try:
        return float(len(s.split(',')))
    except:
        return None

def hypercube_dimension():
    def f(s: str) -> float:
        return float(len(s.split('&&')))/2.0
    return f

def assume_back_number():
    def f(s: str) -> float:
        l = [x for x in s.split('\n') if x is not '']
        return float(len(l)//2)
    return f
