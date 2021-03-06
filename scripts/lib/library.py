import os
from typing import List, Dict
from lib.utils import *

import xml.etree.ElementTree as ET

class Library:

    def __init__(self, name = 'unnamed_library', node = None):
        if node != None:
            self.import_lib(node)
        else:
            # Library name
            self.name = name
            # flags for library
            self.flags = {}
            # Library executable
            self.executable = 'undefined_executable'

    def __str__(self):
        return 'Library %s (%s)\nflags: %s' % (self.name,
            self.executable,
            self.flags)

    def set_executable(self, executable: str) -> None:
        self.executable = executable

    def set_flag(self, flag_name: str, flag_value: str) -> None:
        self.flags[flag_name] = flag_value

    def equal(self, lib: 'Library') -> bool:
        return self.flags.cmp(lib.flags) == 0 and self.name == lib.name

    def import_lib(self, node : ET.ElementTree) -> None:
        self.name = node.attrib['name']
        self.executable = node.find('exe').text
        self.flags = {}
        flags_node = node.find('flags')
        for flag_node in list(flags_node):
            self.set_flag(flag_node.attrib['type'], flag_node.text)

    def export(self):
        node = ET.Element('Lib', attrib = {'name' : self.name})
        ET.SubElement(node, 'exe').text = self.executable
        flags_node = ET.SubElement(node, 'flags')
        for (flag, flag_value) in self.flags.items():
            flag_node = ET.SubElement(flags_node, 'flag', attrib = {'type' : flag})
            flag_node.text = flag_value
        return node

    def execution_string(self):
        l = ['-%s' % flag if value is None else '-%s %s' % (flag,value) for (flag,value) in self.flags.items()]
        flags_string = ' '.join(l)
        return '%s %s' % (self.executable, flags_string)

    def run(self):
        s = '../%s -file %s -folder %s -res %s' %(
            self.execution_string(),
            get_source_file(),
            get_bench_folder(),
            get_res_file())
        print(s)
        os.system(s)

def import_libs(file: str) -> Dict[str, Library]:
    tree = ET.parse(file)
    root = tree.getroot()
    libs = {}
    for lib_node in root.iter('Lib'):
        lib = Library(node = lib_node)
        libs[lib.name] = lib
    return libs
