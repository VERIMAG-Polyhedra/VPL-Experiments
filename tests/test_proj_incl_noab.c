typedef int abs_value;
typedef int var;

void main(){
	var a, b, c, d, e, f, g;
	abs_value P1 = load("P_7_20_0.1555054155670");
	abs_value P2 = guard(top(),
		c >= -1 && c <= 1 &&
		d >= -1 && d <= 1);

	abs_value P3 = projincl(P1, P2);
	abs_value P4 = guard(P1, 90*b -83*c -56*f <= 13300);
	abs_value P5 = projincl(P4, P2);
	abs_value P6 = guard(P4, 91*b -62*c -81*g <= 10000);
	abs_value P7 = projincl(P6, P2);
	print(P7);
}
