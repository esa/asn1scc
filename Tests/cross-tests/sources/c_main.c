#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "file_utility.h"

bool proxy_decode();
void proxy_encode();
void usage();

int main(int argc, char **argv) {
    if (argc != 2) {
        usage();
        return 0;
    }
    if (strcmp(argv[1], "encode") == 0) {
        proxy_encode();
    }
    else if (strcmp(argv[1], "decode") == 0) {
        proxy_decode();
    }
    else {
        fprintf(stderr, "Unrecognized option: %s\n", argv[1]);
        usage();
    }
}

void usage() {
    printf("Descriptive help\n");
}
