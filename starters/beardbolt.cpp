#include <iostream>

// C++ beardbolt starter file

// Local Variables:
// beardbolt-command: "g++ -O3"
// rmsbolt-command: "g++ -O3"
// beardbolt-kill-symbol-re: "\\(^_Z[^0-9]*[SP]\\|__gnu\\)"
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
    std::cout << a << std::endl;
}
