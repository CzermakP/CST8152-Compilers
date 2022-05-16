#include <stdio.h>

int main(int argc, char** argv) {
	char debug = 0;
	int i;
	for (i = 0; i < argc; i++) {
		fprintf(stderr, "argv[%d] = %s\n", i, argv[i]);
	}
	mainBuffer(argc, argv);
	return 0;
}