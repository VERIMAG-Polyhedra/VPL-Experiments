typedef int abs_value;
typedef int var;

void main(){
	var x, y, z, t;

	abs_value P1, P2; // by default associated to top;
	//P1 = load("P_4_8_0.1499094263418");
    //P1 = load("P_6_25_0.1530275387391");
    P1 = load("P_6_44_0.1530275401632");

	P2 = project(P1, x);
}
