from lib.library import *

class Parameters:

    def __init__(self):
        self.values = []

    def __str__(self):
        return str(self.values)

    def add(self, value):
        self.values.append(value)

    def equal(self, params):
        return self.values ==  params.values

    def import_parameters(self, node):
        l = sorted(list(node), key = lambda node: int(node.attrib['i']))
        for node in l:
            self.add(node.text)

    def export(self):
        node = ET.Element('parameters')
        for i in range(len(self.values)):
            parameter_node = ET.SubElement(node, 'param', attrib = {'i' : i})
            parameter_node.text = self.values[i]
        return node

    def instantiate(self, trace):
        return trace % tuple(self.values)

class Timings:

    keys = [
        'widen',
        'assume',
        'project',
        'min',
        'assign',
        'join',
        'proj_incl',
        'assume_back',
        'total'
    ]

    def __init__(self):
        self.timings = {}

    def __str__(self):
        return '\n'.join(['%s -> %s' % (key, value) for (key, value) in self.timings.iteritems()])

    def import_timings(self, node: ET.ElementTree) -> None:
        for key in Timings.keys:
            self.timings[key] = node.find(key).text

class Instance:

    def __init__(self):
        self.parameters = Parameters()
        self.libs = {}

    def __str__(self):
        return 'Instance: \nparameters:%s\nLIBS:\n%s' % (
            self.parameters,
            '\n'.join(['%s -> %s' % (key, value) for (key, value) in self.libs.iteritems()]))

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

    def add(self, library, timings_node):
        lib_node = library.export()
        lib_node.append(timings)
        this.append(lib_node)

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
