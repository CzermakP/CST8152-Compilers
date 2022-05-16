/********************************************************************
 * Filename: scanner.c
 * PURPOSE:
 *    Functions implementing a Lexical Analyzer (Scanner)
 *    as required for CST8152, Assignment #2
 *    The file is incomplete;
 *    Created by: Svillen Ranev - Paulo Sousa - Abdulah
 *    Version: 1.21
 *    Date: 1 Jan 2021
 *******************************************************************
 */

/* TODO_201: REPLACE THIS HEADER WITH YOUR HEADER */

 /* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
  * to suppress the warnings about using "unsafe" functions like fopen()
  * and standard sting library functions defined in string.h.
  * The define does not have any effect in Borland compiler projects.
  */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

  /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern bPointer stringLiteralTable;		/* String literal table */
int line;								/* current line number of the source code */
extern int errorNumber;					/* defined in platy_st.c - run-time error number */

static char debugMode = 0;				/* optional for debugging */

/* Local(file) global objects - variables */
static bPointer lexemeBuffer;			/* pointer to temporary lexeme buffer */
static bPointer sourceBuffer;			/* pointer to input source buffer */
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int nextClass(char c);			/* character class function */
static int nextState(int, char);		/* state machine function */
static int isKeyword(char* kw_lexeme);	/* keywords lookup function */

/* TODO_202: Follow the standard and adjust all function headers */

/*************************************************************
 * Intitializes scanner
 *		This function initializes the scanner using defensive programming.
 ************************************************************/
int startScanner(bPointer psc_buf) {
	if (bIsEmpty(psc_buf))
		return EXIT_FAILURE; /*1*/
	/* in case the buffer has been read previously  */
	bRewind(psc_buf);
	bClean(stringLiteralTable);
	line = 1;
	sourceBuffer = psc_buf;
	return EXIT_SUCCESS; /*0*/
}

/* TODO_203: Follow the standard and adjust all function headers */
/*************************************************************
 * Process Token
 *		Main function of buffer, responsible to classify a char (or sequence
 *		of chars). In the first part, a specific sequence is detected (reading
 *		from buffer). In the second part, a pattern (defined by Regular Expression)
 *		is recognized and the appropriate function is called (related to final states 
 *		in the Transition Diagram).
 ************************************************************/
Token tokenizer(void) {
	Token currentToken = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c;	/* input symbol */
	int state = 0;		/* initial state of the FSM */
	short lexStart;		/* start offset of a lexeme in the input char buffer (array) */
	short lexEnd;		/* end offset of a lexeme in the input char buffer (array)*/

	int lexLength;		/* token length */
	int i;				/* counter */
	unsigned char newc;	/* new char */

	while (1) { /* endless loop broken by token returns it will generate a warning */
		c = bGetCh(sourceBuffer);

		/* ------------------------------------------------------------------------
			Part 1: Implementation of token driven scanner.
			Every token is possessed by its own dedicated code
			-----------------------------------------------------------------------
		*/

		/* TODO_204: Token driven scanner implementation using switch */
		switch (c) {
		case UNKNOWN:
			// Continue the code...
			return currentToken;
		// Continue the code (other cases)...
		

		/* ------------------------------------------------------------------------
			Part 2: Implementation of Finite State Machine (DFA)
					   or Transition Table driven Scanner
					   Note: Part 2 must follow Part 1 to catch the illegal symbols
			-----------------------------------------------------------------------
		*/

		/* TODO_205: Transition driven scanner implementation inside default */

		default: // general case
			state = nextState(state, c);
			// Continue the code...
			while (stateType[state] == NOAS) {
				// Continue the code...
			}
			if (stateType[state] == ASWR)
				// Continue the code...
			// Continue the code...
			currentToken = (*finalStateTable[state])(bGetContent(lexemeBuffer, 0));
			// Continue the code...
			return currentToken;
		} // switch

	} //while

} // tokenizer


/* DO NOT MODIFY THE CODE / COMMENT OF THIS FUNCTION */
/*************************************************************
 * Get Next State
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	(*) assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:
	(*) Assertion failed: test, file filename, line linenum.
	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
 ************************************************************/

int nextState(int state, char c) {
	int col;
	int next;
	col = nextClass(c);
	next = transitionTable[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	assert(next != IS);
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/* TODO_206: Comment this function header */
/*************************************************************
 * Get Next Token Class
	* Create a function to return the column number in the transition table:
	* Considering an input char c, you can identify the "class".
	* For instance, a letter should return the column for letters, etc.
************************************************************/
int nextClass(char c) {
	int val = -1;
	/* TODO_207: Adjust the logic to return next column in TT */
	/*				[A-z](0),	[0-9](1),	.(2),	$(3),	'(4),	SEOF(5),	other(6) */
	switch (c) {
	case UNKNOWN:
		// Continue the code...
	// Continue the code (other cases)...
	}
	return val;
}

/* TODO_208: Comment this function header */
/*************************************************************
 * Acceptance State Function AVID
 *		In this function, the pattern for AVID must be recognized.
 *		Since keywords obey the same pattern, is required to test if 
 *		the current lexeme matches with KW from language.
 *	- Remember to respect the limit defined for lexemes (VID_LEN) and 
 *	  set the lexeme to the corresponding attribute (vidLexeme).
 *    Remember to end each token with the \0.
 *  - Suggestion: Use "strncpy" function.
 ************************************************************/

 /* TODO_209: Continue the code... */
Token funcAVID(char lexeme[]) {
	Token currentToken = { 0 };
	// Continue the code...
	return currentToken;
}

/* TODO_210: Comment this function header */
/*************************************************************
 * Acceptance State Function SVID
 *		In this function, the pattern for SVID must be recognized.
 *	- Remember to respect the limit defined for lexemes (VID_LEN), 
 *	  also observing the delimiters. Remember to end also with \0.
 *	  set the lexeme to the corresponding attribute (vidLexeme).
 *  - Suggestion: Use "strncpy" function.
 ************************************************************/

 /* TODO_211: Continue the code... */
Token funcSVID(char lexeme[]) {
	Token currentToken = { 0 };
	// Continue the code...
	return currentToken;
}

/* TODO_212: Comment this function header */
/*************************************************************
 * Acceptance State Function IL
 *		Function responsible to identify IL (integer literals).
 * - It is necessary respect the limit (ex: 2-byte integer in C).
 * - In the case of larger lexemes, error shoul be returned.
 * - Only first ERR_LEN characters are accepted and eventually, 
 *   additional three dots (...) should be put in the output.
 ************************************************************/

/* TODO_213: Continue the code... */
Token funcIL(char lexeme[]) {
	Token currentToken = { 0 };
	// Continue the code...
	return currentToken;
}

/* TODO_214: Comment this function header */
/*************************************************************
 * Acceptance State Function FPL
 *		Function responsible to identify FPL (float point literals).
 * - It is necessary respect the limit (ex: 4-byte integer in C).
 * - In the case of larger lexemes, error shoul be returned.
 * - Only first ERR_LEN characters are accepted and eventually, 
 *   additional three dots (...) should be put in the output.
 ************************************************************/

/* TODO_215: Continue the code... */
Token funcFPL(char lexeme[]) {
	Token currentToken = { 0 };
	// Continue the code...
	return currentToken;
}

/* TODO_216: Comment this function header */
/*************************************************************
 * Acceptance State Function SL
 *		Function responsible to identify SL (string literals).
 * - The lexeme must be stored in the String Literal Table 
 *   (stringLiteralTable). You need to include the literals in 
 *   this structure, using offsets. Remember to include \0 to
 *   separate the lexemes. Remember also to incremente the line.
 ************************************************************/

/* TODO_217: Continue the code... */
Token funcSL(char lexeme[]) {
	Token currentToken = { 0 };
	// Continue the code...
	return currentToken;
}

/* TODO_218: Comment this function header */
/*************************************************************
 * Acceptance State Function Error
 *		Function responsible to deal with ERR token.
 * - This function uses the errLexeme, respecting the limit given
 *   by ERR_LEN. If necessary, use three dots (...) to use the
 *   limit defined. The error lexeme contains line terminators,
 *   so remember to increment line.
 ************************************************************/

/* TODO_219: Continue the code... */
Token funcErr(char lexeme[]) {
	Token currentToken = { 0 };
	// Continue the code...
	return currentToken;
}

/* TODO_220: Comment this function header */
/*************************************************************
 * This function checks if one specific lexeme is a keyword.
 * - Tip: Remember to use the keywordTable to check the keywords.
 ************************************************************/
 /* TODO_221: Continue the code... */

int isKeyword(char lexeme[]) {
	int i = -1;
	// Continue the code...
	return i;
}


/*
TODO_222: (If necessary): HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS (IF ANY).
*/
