#include <iostream>
#include <array>
#include <vector>
#include <span>

template <typename F, typename T >
void bubble(F from, T to) {
  for (auto i = from; i < to - 1; i++)
    for (auto j = to - 1; i < j; j--)
      if (*j < *(j - 1))
        std::swap(*j, *(j - 1));
}

int main() {
  std::array a{5, 2, 1, 4, 2};
  bubble(a.begin(), a.end());

  std::cout << " Sorted array : ";
  for (const auto& e : a) std::cout << e << "\n";
  return 0;
}

// Local Variables:
// beardbolt-command: "g++ -std=c++20 -O3"
// beardbolt-kill-symbol-re: "\\(^_Z[^0-9]*[SP]\\|__\\)"
// End:
