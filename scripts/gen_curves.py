from lib.library import *
from lib.data import *
from lib.utils import *
from lib.make_curve import *
import os

libs_file = get_libs_file()
libs = import_libs(libs_file)

g = Graph('Dimension')
ord = Ordinate.TOTAL

data = Data(get_data_file('sas19_noab.xml'))
a = ParamChoice(1, get_value_from_polyname(1))
a.set(1, get_value_from_polyname(2), 8)
a.set(2, hypercube_dimension(), 2)
a.set(3, assume_back_number(), 2)

for (_,lib) in [(name,lib) for (name,lib) in libs.items() if name in ["Newpolka", "VPL"]]:
    c = Curve(lib, data, a, ord, color = random_color())
    print(lib.name, c.points)
    g.add_curve(c)

data = Data(get_data_file('sas19.xml'))
a = ParamChoice(1, get_value_from_polyname(1))
a.set(1, get_value_from_polyname(2), 8)
a.set(2, hypercube_dimension(), 2)
a.set(3, get_value_from_polyname(2), 2)

for (_,lib) in [(name,lib) for (name,lib) in libs.items() if name == "VPL_PROJINCL"]:
    c = Curve(lib, data, a, ord, color = random_color())
    print(lib.name, c.points)
    g.add_curve(c)

g.show()
