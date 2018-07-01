# Installation

1. __Install VPL__
    1. Download and install _Ocamlmpi_, available at https://github.com/xavierleroy/ocamlmpi

    2. Download the `dev_mpi` branch of the VPL, available at https://github.com/VERIMAG-Polyhedra/VPL/tree/dev_mpi

    3. Check the VPL dependencies, and install them if needed

    4. Compile the VPL:

    ```
    make
    ```

    5. Install the VPL:

    ```
    make install
    ```

2. __Install VPL-Experiments__
    1. Then, download the VPL-Experiment repository, available at https://github.com/VERIMAG-Polyhedra/VPL-Experiments

    2. Compile the VPL executable:

    ```
    make
    ```

# Usage
To run a problem, you need to encode it with a C program (see the _Language_ section below).
Then, type

```
mpirun -np <number_of_processes> ./Run_VPL.byte -file <C_file.c> -folder benchs/ -proj plp
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
