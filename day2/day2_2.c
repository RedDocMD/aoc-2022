#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int choice_scores[] = { 1, 2, 3 };
static int result_scores[] = { 0, 3, 6 };

enum result {
	Loss = 0,
	Draw = 1,
	Win = 2,
};

enum choice {
	Rock = 0,
	Paper = 1,
	Scissors = 2,
};

enum choice find_choice(enum choice opp, enum result result)
{
	if (result == Draw)
		return opp;
	if (result == Win)
		return (opp + 1) % 3;
	return (opp - 1 + 3) % 3;
}

enum choice letter_to_choice(char l)
{
	switch (l) {
	case 'A':
		return Rock;
	case 'B':
		return Paper;
	case 'C':
		return Scissors;
	default:
		fprintf(stderr, "Invalid choice: %c\n", l);
		exit(1);
	}
}

enum result letter_to_result(char l)
{
	switch (l) {
	case 'X':
		return Loss;
	case 'Y':
		return Draw;
	case 'Z':
		return Win;
	default:
		fprintf(stderr, "Invalid result: %c\n", l);
		exit(1);
	}
}

#define FILENAME "input1.txt"

int main()
{
	char opp_buf[10], result_buf[10];
	int score = 0;

	FILE *fp = fopen(FILENAME, "r");
	if (!fp) {
		fprintf(stderr, "Failed to open %s: %s\n", FILENAME,
			strerror(ferror(fp)));
		exit(1);
	}

	while (fscanf(fp, "%s %s", opp_buf, result_buf) != EOF) {
		enum choice opp = letter_to_choice(opp_buf[0]);
		enum result result = letter_to_result(result_buf[0]);
		score += result_scores[result];
		enum choice yours = find_choice(opp, result);
		score += choice_scores[yours];
	}

	printf("Total score = %d\n", score);

	fclose(fp);
	return 0;
}