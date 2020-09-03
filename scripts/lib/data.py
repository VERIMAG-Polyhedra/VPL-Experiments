from lib.library import *
from lib.utils import get_source_file

class Parameters:

    def __init__(self):
        self.values = []

    def __str__(self):
        return str(self.values)

    def add(self, value) -> None:
        self.values.append(value)

    def equal(self, params: 'Parameters') -> bool:
        return self.values == params.values

    def import_parameters(self, node: ET.ElementTree) -> None:
        l = sorted(list(node), key = lambda node: int(node.attrib['i']))
        for node in l:
            self.add(node.text)

    def export(self, parent_node: ET.ElementTree) -> None:
        node = ET.SubElement(parent_node, 'parameters')
        for i in range(len(self.values)):
            parameter_node = ET.SubElement(node, 'param', attrib = {'i' : str(i)})
            parameter_node.text = str(self.values[i])
        return node

    def instantiate(self, trace):
        return trace % tuple(self.values)

class Timings:

    def __init__(self, from_res_file: bool = False):
        self.timings = dict()
        if from_res_file:
            self.import_res()

    def __str__(self):
        return '\n'.join(['%s -> %s' % (key, value) for (key, value) in self.timings.items()])

    def import_timings(self, node: ET.ElementTree) -> None:
        for child in node:
            if not ("unit" in child.attrib) or "ns" in child.attrib["unit"]:
                self.timings[child.tag] = child.text

    def import_res(self) -> None:
        tree = ET.parse(get_res_file())
        root = tree.getroot()
        self.import_timings(root)

class Instance:

    def __init__(self):
        self.parameters = Parameters()
        self.libs = {}

    def __str__(self):
        return 'Instance: \nparameters:%s\nLIBS:\n%s' % (
            self.parameters,
            '\n'.join(['%s -> %s' % (key, value) for (key, value) in self.libs.items()]))

    def import_instance(self, node : ET.ElementTree) -> None:
        self.parameters.import_parameters(node.find('parameters'))

        for lib_node in node.iter('Lib'):
            name = lib_node.attrib["name"]
            self.libs[name] = Timings()
            self.libs[name].import_timings(lib_node)

    def has_lib(self, library : Library) -> bool:
        return library.name in self.libs

    def set_parameters(self, parameters: Parameters) -> None:
        self.parameters = parameters

    def instantiate(self, trace):
        return self.parameters.instantiate(trace)

    def add_exp(self, lib: Library) -> None:
        self.libs[lib.name] = Timings(True)
        print(self)

    def export(self, parent_node: ET.ElementTree) -> None:
        self.parameters.export(parent_node)
        for (lib_name, timings) in self.libs.items():
            node = ET.SubElement(parent_node, 'Lib', attrib = {'name' : lib_name})
            for (key, value) in timings.timings.items():
                time_node = ET.SubElement(node, key)
                time_node.text = value

source_skeleton = '''typedef int abs_value;
typedef int var;

void main(){
%s
}
'''

class Data:

    def __init__(self, file = None):
        self.instances = []
        self.trace = None
        if file is not None:
            self.import_data(file)

    def __str__(self):
        instances = '\n'.join([str(i) for i in self.instances])
        return 'Data\ntrace: \n%s\ninstances:\n%s' % (self.trace, instances)

    def import_data(self, file: str) -> None:
        tree = ET.parse(file)
        root = tree.getroot()
        self.trace = root.find('Trace').text
        for instance_node in root.iter('Instance'):
            instance = Instance()
            instance.import_instance(instance_node)
            self.instances.append(instance)

    def get_instance(self, params: Parameters) -> Instance:
        for instance in self.instances:
            if params.equal(instance.parameters):
                return instance
        ins = Instance()
        ins.set_parameters(params)
        self.instances.append(ins)
        return ins

    def instance_exist(self, params: Parameters, lib: Library) -> bool:
        instance = self.get_instance(params)
        for lib_name in instance.libs:
            if lib.name == lib_name:
                return True
        return False

    def make_source_file(self, params: Parameters) -> None:
        s = source_skeleton % params.instantiate(self.trace)
        f = open(get_source_file(), 'w')
        f.write(s)
        f.close()

    def export(self, file: str) -> None:
        node = ET.Element('Data')
        trace_node = ET.SubElement(node, 'Trace')
        trace_node.text = self.trace
        for instance in self.instances:
            instance_node = ET.SubElement(node, 'Instance')
            instance.export(instance_node)
        export(ET.ElementTree(node), get_data_file(file))
