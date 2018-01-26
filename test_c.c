typedef int abs_value;
typedef int var;

void main(){
	var x, y, z ;
	int c = 0; // counter
	float test = c + 2.15;

	abs_value P1, P2, P3; // by default associated to top;
	P1 = load("P_4_8_0.1499094263418");

	while(c <= 10 && includes(P3, P1)){
		P2 = guard(P1, x + y >= 3);
		if( includes(P2, guard(top(), x >= 0)) ){
			P2 = assign(P2, x = x + 1);
		}
		P3 = join(P1,P2);
		P3 = project(P3, x);
		c++;
	}
}
