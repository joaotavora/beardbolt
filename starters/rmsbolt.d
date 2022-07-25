// Local Variables:
// beardbolt-command: "ldc2 -O0"
// End:

import std.stdio : writeln;

int isRMS(char a) {
    switch(a) {
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
    int a = 1 + 1;
    if(isRMS(cast(char) a)) {
        writeln(a);
    }
    return 0;
}
