#include <iostream>
#include <algorithm>
#include <vector>

template <typename It>
void bubble(It from, It to) {
  for (auto i = from; i < to - 1; i++)
    for (auto j = to - 1; i < j; j--)
      if (*j < *(j - 1))
        std::swap(*j, *(j - 1));
}

int main(int argc, char* argv[]) {
  std::vector<int> vi;
  std::transform(argv, argv+argc, std::back_inserter(vi),
      std::atoi);
  bubble(vi.begin(), vi.end());

  std::cout << "Sorted array : \n";
  for (auto&& e : vi) std::cout << e << "\n";
  return 0;
}

// Local Variables:
// beardbolt-command: "g++ -std=c++20 -O3"
// beardbolt-demangle: t
// beardbolt-execute: "5 4 blargh 2 1"
// beardbolt-link-flags: "-fsanitize=address"
// beardbolt-disassemble: nil
// beardbolt-preserve-library-functions: nil
// beardbolt-preserve-unused-labels: nil
// beardbolt-preserve-directives: nil
// End:
