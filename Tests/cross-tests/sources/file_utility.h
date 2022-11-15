#ifndef FILE_UTILITY_H
#define FILE_UTILITY_H

void write_to_file(const char *filename, const char *buffer, unsigned size);
void read_from_file(const char *filename, char *buffer, unsigned size);

#endif //FILE_UTILITY_H
