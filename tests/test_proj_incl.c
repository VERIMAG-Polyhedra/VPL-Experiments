typedef int abs_value;
typedef int var;

void main(){
	abs_value P1 = guard(top(), x >= 0 && x <= 2);
    abs_value P2 = guard(top(), x >= y && y >= 1 && z >= 0);
	abs_value P3 = join(P1, project(P2, y));
	abs_value p1join = projincl(P3, P1);
	//abs_value p2join = projincl(P3, P2);

	//abs_value p1join2 = assume_back(p1join, x >= y);
	//abs_value p2join2 = assume_back(p2join, x >= y);

	//abs_value p1join3 = assume_back(p1join2, x <= 2);
	// infeasible ->
	//abs_value p2join3 = assume_back(p2join2, x <= 2);*/
}
