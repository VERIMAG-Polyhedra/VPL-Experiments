import matplotlib.pyplot as plt
import matplotlib.lines as mlines
from lib.utils import *
from lib.data import *
from enum import Enum
from typing import Union

class Abscissa(Enum):
    CSTR = 2
    RED = 3
    DIM = 1
    DENSITY = 5

    def label(self):
        if self == Abscissa.CSTR:
            return 'Number of constraints'
        elif self == Abscissa.RED:
            return 'Redundancy rate'
        elif self == Abscissa.DIM:
            return 'Dimension'
        elif self == Abscissa.DENSITY:
            return 'Density rate'

class Ordinate(Enum):
    TOTAL = 1
    PROJ = 2
    JOIN = 3
    ASSUME = 4
    PROJINCL = 5
    ASSUMEBACK = 6
    ASSIGN = 7
    MIN = 8
    WIDEN = 9

    def label(self) -> str:
        if self == Ordinate.TOTAL:
            return 'Total'
        elif self == Ordinate.PROJ:
            return 'projection'
        elif self == Ordinate.JOIN:
            return 'Join'
        elif self == Ordinate.ASSUME:
            return 'Assume'
        elif self == Ordinate.PROJINCL:
            return 'Proj_inclusion'
        elif self == Ordinate.ASSUMEBACK:
            return 'Assume_back'
        elif self == Ordinate.ASSIGN:
            return 'Assignment'
        elif self == Ordinate.MIN:
            return 'Minimization'
        elif self == Ordinate.WIDEN:
            return 'Widening'


    def tag(self) -> str:
        if self == Ordinate.TOTAL:
            return 'total'
        elif self == Ordinate.PROJ:
            return 'project'
        elif self == Ordinate.JOIN:
            return 'join'
        elif self == Ordinate.ASSUME:
            return 'assume'
        elif self == Ordinate.PROJINCL:
            return 'proj_incl'
        elif self == Ordinate.ASSUMEBACK:
            return 'assume_back'
        elif self == Ordinate.ASSIGN:
            return 'assign'
        elif self == Ordinate.MIN:
            return 'min'
        elif self == Ordinate.WIDEN:
            return 'widen'

class AbscissaChoice:

    def __init__(self, abs: Abscissa, i_abs: int):
        self.params = dict()
        self.abs = abs
        self.i_abs = i_abs

    def set(self, abs: Abscissa, i_param: int, value: Union[int, float]) -> None:
        if i_param in self.params:
            self.params[i_param][abs] = value
        else:
            self.params[i_param] = dict()
            self.params[i_param][abs] = value

    @staticmethod
    def value_from_polyname(polyname: str, abs: Abscissa) -> float:
        return float(polyname.split('_')[abs.value])

    def satisfy(self, ins: Instance) -> bool:
        for i in range(len(ins.parameters.values)):
            param = ins.parameters.values[i]
            if i in self.params:
                for (abs, value) in self.params[i].items():
                    if AbscissaChoice.value_from_polyname(param, abs) != value:
                        return False
        return True

    def value(self, ins: Instance):
        param = ins.parameters.values[self.i_abs]
        return AbscissaChoice.value_from_polyname(param, self.abs)

class Curve:

    def __init__(self, lib: Library, data: Data, abs: AbscissaChoice, ord: Ordinate, color = 'black'):
        self.points = []
        self.color = color
        self.lib = lib
        self.abs = abs.abs
        self.gather(data, abs, ord)

    def gather(self, data: Data, abs: AbscissaChoice, ord: Ordinate) -> None:
        for instance in data.instances:
            if abs.satisfy(instance):
                abs = abs.value(instance)
                timings = instance.libs[self.lib.name]
                ord = Curve.to_second(int(timings.timings[ord.tag()]))
                self.points.append((abs, ord))

    @staticmethod
    def to_second(time: int) -> float:
        '''Translates a time from ns to s'''
        return float(time)/float(pow(10,9))

    def sort(self) -> None:
    	self.points = sorted(self.points)

    def stack(self) -> None:
        '''Stacks all points that share the same abscissa
        Requires a sorted list of points'''
        if self.points != []:
            points = []
            (x_prec,y_stack) = self.points[0]
            n_stack = 1
            for (x,y) in curve['points'][1:]:
                if x == x_prec:
                    n_stack += 1
                    y_stack += y
                else:
                    points.append((x_prec,y_stack/n_stack))
                    n_stack = 1
                    x_prec = x
                    y_stack = y
            #last point :
            points.append((x_prec,y_stack/n_stack))
            self.points = points

class Graph:

    def __init__(self):
        plt.ylabel('Time in ms (logscale)')
        plt.yscale('log')
        plt.legend(loc = 'best')

    def add_curve(self, curve: Curve) -> None:
        xs = [p[0] for p in curve.points]
        ys = [p[1] for p in curve.points]
        plt.plot(xs, ys, color = curve.color, label = curve.lib.name)
        plt.xlabel(curve.abs.label())

    def show(self) -> None:
        plt.show()
