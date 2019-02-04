# Installation

1. __Install VPL__

    With OPAM:
```
opam repo add vpl https://raw.githubusercontent.com/VERIMAG-Polyhedra/opam-vpl/master
opam update
opam install vpl-core
```

2. __Install VPL-Experiments__
    1. Download or clone the VPL-Experiment repository, available at https://github.com/VERIMAG-Polyhedra/VPL-Experiments

    2. Compile the VPL executable:

    ```
    make
    ```

# Usage
To run a problem, you need to encode it with a C program (see the _Language_ section below).
Then, type

```
./Run_VPL.byte -file <C_file.c> -folder benchs/ -proj plp
```

# Language

1. __Syntax__

	A trace is written in a C file, following the C standard syntax. Operators are applied by function calls.

2. __Types__

	An abstract value has type `abs_value`.
	Program variables have type `int` or `float`

3. __Operators__

	```
	abs_value load(string file);
	```
	Loading an abstract value from a file name

	```
	abs_value top();
	```

	```
	bool isBottom(abs_value a)
	```

	```
	abs_value meet(abs_value a1, abs_value a2)
	```

	```
	abs_value join(abs_value a1, abs_value a2)
	```

	```
	abs_value guard(abs_value a, b_expr)
	```

	```
	abs_value assign(abs_value a, var1 = a_expr1, var2 = a_expr2, ...)
	```
	Parallel assignment

	```
	abs_value widen(abs_value a1, abs_value a2)
	```

	```
	abs_value elim(abs_value a, var1, var2, ...)
	```
	Variable elimination

	```
	abs_value minimize(abs_value a)
	```
	Minimization of representation

	```
	bool includes(abs_value a1, abs_value a2)
	```

	```
	abs_value minimize(abs_value a)
	```

	```
	itv get_itv(abs_value a)
	```

4. __Timers__

	The trace runner has a timer for each operator. If you need to see timings at some point:

	```
	void show_timers()
	```

## Examples

```
void main(){
	int x, y;
	int c = 0; // counter

	abs_value P1 = load("P_200");
	abs_value P3 = top();

	while(c <= 100 || includes(P3, P1)){
		P2 = guard(P1, x + y >= 3);
		if( includes(P2, guard(top(), x >= 0)) ){
			P2 = assign(P2, x = x + 1);
		}
		P3 = join(P1,P2);
		c++;
	}
}
```
