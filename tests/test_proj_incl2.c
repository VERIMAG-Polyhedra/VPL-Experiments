typedef int abs_value;
typedef int var;

void main(){
	var a, b, c, d, e, f, g;
	abs_value P1 = load("P_7_10_0.1554809760632");
    //abs_value P2 = load("P_5_10_0.1554809760638");
	abs_value P2 = guard(top(),
		c >= -1 && c <= 1 &&
		d >= -1 && d <= 1 &&
		e >= -1 && e <= 1 &&
		f >= -1 && f <= 1 &&
		g >= -1 && g <= 1);

	abs_value P3 = projincl(P1, P2);

	//abs_value P4 = project(P1, a, b);

	abs_value P4 = assume_back(P3,-5*b -65*c -98*e <= 2300);
	//abs_value P3 = project(P1, x, y);

	// infeasible ->
	//abs_value p2join3 = assume_back(p2join2, x <= 2);
}
