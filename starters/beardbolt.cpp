#include <iostream>
#include <array>
#include <vector>

template <typename It>
void bubble(It from, It to) {
  for (auto i = from; i < to - 1; i++)
    for (auto j = to - 1; i < j; j--)
      if (*j < *(j - 1))
        std::swap(*j, *(j - 1));
}

int main() {
  std::vector v{5, 2, 1, 4, 2};
  bubble(v.begin(), v.end());

  std::cout << "Sorted array : ";
  for (const auto& e : v) std::cout << e << "\n";
  return 0;
}

// Local Variables:
// beardbolt-command: "g++ -std=c++20 -O0"
// beardbolt-demangle: t
// beardbolt-execute: t
// beardbolt-disassemble: nil
// beardbolt-preserve-library-functions: nil
// beardbolt-preserve-unused-labels: nil
// beardbolt-preserve-directives: nil
// End:
