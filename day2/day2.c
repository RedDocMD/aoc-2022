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

enum result evaluate_result(enum choice opp, enum choice yours)
{
	if (yours == opp)
		return Draw;
	if (yours == Paper && opp == Rock)
		return Win;
	if (yours == Scissors && opp == Paper)
		return Win;
	if (yours == Rock && opp == Scissors)
		return Win;
	return Loss;
}

enum choice letter_to_choice(char l)
{
	switch (l) {
	case 'A':
	case 'X':
		return Rock;
	case 'B':
	case 'Y':
		return Paper;
	case 'C':
	case 'Z':
		return Scissors;
	default:
		fprintf(stderr, "Invalid choice: %c\n", l);
		exit(1);
	}
}

#define FILENAME "input1.txt"

int main()
{
	char opp_buf[10], your_buf[10];
	int score = 0;

	FILE *fp = fopen(FILENAME, "r");
	if (!fp) {
		fprintf(stderr, "Failed to open %s: %s\n", FILENAME,
			strerror(ferror(fp)));
		exit(1);
	}

	while (fscanf(fp, "%s %s", opp_buf, your_buf) != EOF) {
		enum choice opp = letter_to_choice(opp_buf[0]);
		enum choice yours = letter_to_choice(your_buf[0]);
		score += choice_scores[yours];
		enum result result = evaluate_result(opp, yours);
		score += result_scores[result];
	}

	printf("Total score = %d\n", score);

	fclose(fp);
	return 0;
}