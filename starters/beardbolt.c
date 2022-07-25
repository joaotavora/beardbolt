#include <stdio.h>

// C beardbolt starter file

// Local Variables:
// beardbolt-command: "gcc -O0"
// beardbolt-disassemble: nil
// End:

int isRMS(int a) {
	 switch (a) {
	 case 'R':
		  return 1;
	 case 'M':
		  return 2;
	 case 'S':
		  return 3;
	 default:
		  return 0;
	 }
}

int main() {
		char a = 1 + 1;
		if (isRMS(a))
			 printf("%c\n", a);
}
