%token <Z.t> Z
%token SLASH
%token <string> PolyName
%token LE LT EQ GE GT NEQ
%token EOF EOL
%start one_matrix
%type <IOBuild.t> one_matrix
%%
one_matrix: PolyName EOL matrix EOF {$3}
;
matrix : cstrs {IOBuild.mk $1}
;
cstrs:
  cstr {[$1]}
| cstr cstrs {$1 :: $2}
;
cstr:
  nb LE nb EOL {([$1;$3], IOBuild.Le)}
| nb LT nb EOL {([$1;$3], IOBuild.Lt)}
| nb EQ nb EOL {([$1;$3], IOBuild.Eq)}
| nb cstr {let (l,s) = $2 in ($1 :: l,s)}
;
nb:
	| Z {Q.make $1 Z.one}
	| Z SLASH Z {Q.make $1 $3}
