from lib.library import *
from lib.data import *
from lib.utils import *
from lib.make_curve import *
import os

libs_file = get_libs_file()
libs = import_libs(libs_file)

# First curve: time depending on the number of assume back
g = Graph('Number of proj-inclusions')
ord = Ordinate.TOTAL

a = ParamChoice(3, assume_back_number())
a.set(1, get_value_from_polyname(1), 5)
a.set(1, get_value_from_polyname(2), 14)
a.set(2, hypercube_dimension(), 3)
a2 = ParamChoice(3, get_value_from_polyname(2))
a2.copy_from(a)

# VPL curve
data = Data(get_data_file('sas19_noab.xml'))
for (_,lib) in [(name,lib) for (name,lib) in libs.items() if name in ["VPL"]]:
    c = Curve(lib, data, a, ord, color = random_color())
    print(lib.name, c.points)
    g.add_curve(c, linestyle = '--', label = 'Not incremental')

# VPL_proj curve
data = Data(get_data_file('sas19.xml'))
for (_,lib) in [(name,lib) for (name,lib) in libs.items() if name == "VPL_PROJINCL"]:
    c = Curve(lib, data, a2, ord, color = random_color())
    print(lib.name, c.points)
    g.add_curve(c, label = 'Incremental')
g.show()


# Second curve:
a = ParamChoice(3, get_value_from_polyname(2))
a.set(1, get_value_from_polyname(1), 5)
a.set(1, get_value_from_polyname(2), 14)
a.set(2, hypercube_dimension(), 3)
data = Data(get_data_file('sas19.xml'))

g = Histogram(xlabel = 'Number of proj-inclusions', ylabel = 'Number of regions')

ord = Ordinate.TOTAL_REGIONS
for (_,lib) in [(name,lib) for (name,lib) in libs.items() if name == "VPL_PROJINCL"]:
    c_total = Curve(lib, data, a, ord, color = random_color(), label = 'Unchanged regions')
    print(lib.name, c.points)


hatches = ['/','\\',"//",""]
for ord in [Ordinate.NEW_REGIONS, Ordinate.DELETED_REGIONS, Ordinate.UPDATED_REGIONS]:

    # VPL_proj curve
    for (_,lib) in [(name,lib) for (name,lib) in libs.items() if name == "VPL_PROJINCL"]:
        c = Curve(lib, data, a, ord, color = random_color())
        print(lib.name, c.points)

        for i in range(len(c.points)):
            y = c_total.points[i][1] - c.points[i][1]
            c_total.points[i] = (c_total.points[0], y, 0)

        g.add_curve(c, hatches[0])
        hatches.pop(0)

g.add_curve(c_total)
g.show()
