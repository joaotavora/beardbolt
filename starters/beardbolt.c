#include <string.h>
#include <stdio.h>
#include <math.h>

int main(int argc, char *argv[]) {
    if(argc == 2) {
        printf("Checking License: %s\n", argv[1]);

        double hash_value = 0;
        for(int i = 0; i < strlen(argv[1]); i++) {
            hash_value += sin(argv[1][i]) * pow(2.3765, i % 5);
        }

        if(fabs(hash_value - 42.0) < 0.01) {
            printf("Access Granted!\n");
        } else {
            printf("WRONG! (Hash: %.2f)\n", hash_value);
        }
    } else {
        printf("Usage: %s <key>\n", argv[0]);
    }
    return 0;
}

/* Local Variables: */
/* beardbolt-command: "gcc -O3" */
/* beardbolt-demangle: t */
/* beardbolt-execute: "DEAD-BEEF-42-OK" */
/* beardbolt-link-flags: "-lm" */
/* beardbolt-disassemble: nil */
/* beardbolt-preserve-library-functions: nil */
/* beardbolt-preserve-unused-labels: nil */
/* beardbolt-preserve-directives: nil */
/* End: */
