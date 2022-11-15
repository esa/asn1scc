#include "file_utility.h"

#include <stdio.h>

void write_to_file(const char *filename, const char *buffer, unsigned size) {
    FILE *file = fopen(filename, "wb");

    if (file == NULL) {
        perror("Failed to open the file");
        return;
    }

    int written = fwrite(buffer, size, 1, file);
    if (written != 1) {
        perror("Failed to write to file");
        fclose(file);
        return;
    }

    fflush(file);
    fclose(file);
}

void read_from_file(const char *filename, char *buffer, unsigned size) {
    FILE *file = fopen(filename, "rb");

    if (file == NULL) {
        perror("Failed to open the file");
        return;
    }

    int read = fread(buffer, size, 1, file);
    if (read != 1) {
        perror("Failed to read from file");
        fclose(file);
        return;
    }
    fclose(file);
}
