typedef int abs_value;
typedef int var;

void main(){
	var a, b, c, d, e, f, g;
	abs_value P1 = load("P_7_20_0.1555054155670");
	abs_value P2 = guard(top(),
		c >= -1 && c <= 1 &&
		d >= -1 && d <= 1);

	abs_value P3 = projincl(P1, P2);
	abs_value P4 = assume_back(P3, 90*b -83*c -56*f <= 13300);
	abs_value P44 = assume_back(P4, 91*b -62*c -81*g <= 10000);
/*
	abs_value P5 = guard(P1, 90*b -83*c -56*f <= 13300);
	abs_value P6 = projincl(P5, P2);
	abs_value P66 = guard(P5, 91*b -62*c -81*g <= 10000);
	abs_value P67 = projincl(P66, P2);

	includes(P44,P67);
	includes(P67,P44);*/


}
