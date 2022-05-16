/*******************************************************************************************
 * Filename: scanner.c
 * Compiler: MS Visual Studio 2019
 * Author: Patrick Czermak (Partner: Iman Dirie)
 * Course: CST 8152 – Compilers, Lab Section: [013]
 * Assignment: A2
 * Date: March 19, 2021
 * Professor: Paulo Sousa(lab) / Abdulah Khadri(theory)
 * Purpose: Holds the functions required for implementing a Lexical Analyzer (aka. scanner)
 * Function List: startScanner, tokenizer, nextState, nextClass, funcAVID, funcSVID, funcIL,
 *				  funcFPL, funcSL, funcERR, isKeyword
*******************************************************************************************/

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
static int isKeyword(char lexeme[]);	/* keywords lookup function    static int isKeyword(char* kw_lexeme)   */

/******************************************************************************************
 * Funtion Name: startScanner
 * Purpose: To Intitialize the scanner
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Funtions: bIsEmpty, bRewind, bClean
 * Parameters: pointer to psc_buf - instance of the buffer used to initialize the scanner.
 * Return Value: 1-EXIT_FAILURE on fail, 0-EXIT_SUCCESS on success
 * Algorithm: If the buffer is empty return false. Rewind the buffer. Clean the string 
 *			  literal buffer. Set line to 1. Set sourceBuffer to psc_buf.
 *****************************************************************************************/
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
/*******************************************************************************************
 * Function Name: tokenizer
 * Purpose: To process and determine/classify what kind of token the char being read is.
 *			First part - a specific sequence is detected (reading from the buffer)
 *			Second part - a pattern is recognized and the appropriate function realted 
						  to the final state in the transition diagram is called. 
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Functions: bGetChar, bRetract, bGetChOffset, bSetMarkOffset, bRestore, nextState,
 *					 bCreate, bAddChar, bFinish, bFree
 * Parameters: void
 * Return Value: currentToken - the token being processed and classified
 * Algorithm: Check if there are any exceptions in the token classification. Default, loop 
 *			  through the transition table to determine the token type.
 ******************************************************************************************/
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

		/* Part 1: Implementation of token driven scanner */
		switch (c) {
		case ' ': // empty space
			break;

		case '\t': /* tab */
			break;

		case '\n': /* newline */
			newc = bGetCh(sourceBuffer);
			if (newc == '\r') { /* carriage return */
				line++;
				break;
			}
			line++;
			bRetract(sourceBuffer);
			break;

		case '\r': /* carriage return */
			line++;
			break;

		case CHARSEOF0: /* seof 0 */
			currentToken.code = SEOF_T;
			return currentToken;
			
		case CHARSEOF255: /* seof 255 */
			currentToken.code = SEOF_T;
			return currentToken;

		case ';': /* semicolon */
			currentToken.code = EOS_T;
			return currentToken;

		case ',': /* comma */
			currentToken.code = COM_T;
			return currentToken;

		case '{': /* left brace */
			currentToken.code = LBR_T;
			return currentToken;

		case '}': /* right brace */
			currentToken.code = RBR_T;
			return currentToken;

		case '(': /* left bracket */
			currentToken.code = LPR_T;
			return currentToken;

		case ')': /* right bracket */
			currentToken.code = RPR_T;
			return currentToken;

		case '+': /* string concatentation and addition operator */
			c = bGetCh(sourceBuffer);
			if (c == '+') { /* confirm if next token is a '+', as this is string concatenation operator */
				currentToken.code = SCC_OP_T;
				return currentToken;
			}
			else { /* if it's not than retract the buffer and use the single '+' which is arithmetic operator ADD */
				bRetract(sourceBuffer);
				currentToken.code = ART_OP_T;
				currentToken.attribute.arithmeticOperator = ADD;
				return currentToken;
			}

		case '-': /* minus */
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = SUB;
			return currentToken;

		case '*': /* multiply */
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = MUL;
			return currentToken;

		case '/': /* division */
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = DIV;
			return currentToken;

		case '<': /* less than */
			currentToken.code = REL_OP_T;
			currentToken.attribute.relationalOperator = LT;
			return currentToken;

		case '>': /* greater than */
			currentToken.code = REL_OP_T;
			currentToken.attribute.relationalOperator = GT;
			return currentToken;

		case '=': /* equals and assignment operator */
			c = bGetCh(sourceBuffer);
			if (c == '=') { /* if next symbold is '=' than is relational operator '==' */
				currentToken.code = REL_OP_T;
				currentToken.attribute.relationalOperator = EQ;
				return currentToken;
			}
			else {
				bRetract(sourceBuffer);
				currentToken.code = ASS_OP_T;
				return currentToken;
			}

		case '!': /* used to check if relational operator '!=' */
			newc = bGetCh(sourceBuffer); // temp comment  

			if (newc == '=') {
				currentToken.code = REL_OP_T;
				currentToken.attribute.relationalOperator = NE;
				return currentToken;
			}
			else {
				bRetract(sourceBuffer);
				currentToken.code = ERR_T;
				currentToken.attribute.errLexeme[0] = c;
				currentToken.attribute.errLexeme[1] = CHARSEOF0;
				return currentToken;
			}
			
		case '%': /* comments */
			newc = c = bGetCh(sourceBuffer);
			/* confirm if token is a comment OR error */
			if (newc != '%') {
				bRetract(sourceBuffer);
				currentToken.code = ERR_T;
				currentToken.attribute.errLexeme[0] = '%';
				currentToken.attribute.errLexeme[1] = CHARSEOF0;
				return currentToken;
			}
			/* loop until able to locate a new line */
			while (c != '\n') {
				c = bGetCh(sourceBuffer);
				/* if end of file reached */
				if (c == CHARSEOF255 || c == CHARSEOF0) {
					bRetract(sourceBuffer);
					currentToken.code = SEOF_T;
					return currentToken;
				}
			}
			line++;
			continue;

		case '.': /* start of logical operator */
			lexStart = bGetChOffset(sourceBuffer); /* '.' represents the start of a logical operator .AND., .OR., .NOT. */ 
			bSetMarkOffset(sourceBuffer, lexStart);

			newc = bGetCh(sourceBuffer);
			if (newc == 'A' && bGetCh(sourceBuffer) == 'N' && bGetCh(sourceBuffer) == 'D' && bGetCh(sourceBuffer) == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = AND;
				return currentToken;
			} 
			else if (newc == 'O' && bGetCh(sourceBuffer) == 'R' && bGetCh(sourceBuffer) == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = OR;
				return currentToken;
			}
			else if (newc == 'N' && bGetCh(sourceBuffer) == 'O' && bGetCh(sourceBuffer) == 'T' && bGetCh(sourceBuffer) == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = NOT;
				return currentToken;
			}
			else {
				bRestore(sourceBuffer);
				currentToken.code = ERR_T;
				currentToken.attribute.errLexeme[0] = c;
				currentToken.attribute.errLexeme[1] = CHARSEOF0;
				return currentToken;
			}
			break;
		

		/* Part 2: Implementation of Finite State Machine (DFA) or Transition Table driven Scanner */

		default: /* general case */
			state = nextState(state, c);
			lexStart = bGetChOffset(sourceBuffer) - 1;
			bSetMarkOffset(sourceBuffer, lexStart);

			while (stateType[state] == NOAS) {
				newc = bGetCh(sourceBuffer);
				state = nextState(state, newc);
			}
			if (stateType[state] == ASWR) {
				bRetract(sourceBuffer);
			}

			lexEnd = bGetChOffset(sourceBuffer); 
			lexLength = lexEnd - lexStart;
			lexemeBuffer = bCreate((short)lexLength, (short)state, 'f'); /* bCreate(size, increment, mode) */
			bRestore(sourceBuffer);

			for (i = 0; i < lexLength; i++) {
				newc = bGetCh(sourceBuffer); 
				bAddCh(lexemeBuffer, newc);
			}
			bFinish(lexemeBuffer, CHARSEOF0);
			currentToken = (*finalStateTable[state])(bGetContent(lexemeBuffer, 0));
			bFree(lexemeBuffer);

			return currentToken;
		} /* end switch */

	} /* end while */

} /* end tokenizer */


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

/*************************************************************************************************************
 * Function Name: nextClass
 * Purpose: To return the correct column number in the transition table
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Functions: isalpha, isalnum, 
 * Parameters: char c - the char being processed/classified
 * Return Value: int val - the specific column from the transition table
 * Algorithm: if 'c' is a letter, digit, decimal/period, dollar sign, single quote, EOF char, or anything else
**************************************************************************************************************/
int nextClass(char c) {
	int val = -1;
	/* [A-z](0),	[0-9](1),	.(2),	$(3),	'(4),	SEOF(5),	other(6)  */

	if (isalpha(c)) {
		val = 0;
	}
	else if (isalnum(c)) {
		val = 1;
	}
	else if (c == '.') {
		val = 2;
	}
	else if (c == '$') {
		val = 3;
	}
	else if (c == '\'') {   
		val = 4;
	}
	else if (c == CHARSEOF0 || c == CHARSEOF255) {
		val = 5;
	}
	else {
		val = 6;
	}
	return val;
}

/********************************************************************************************************
 * Function Name: funcAVID
 * Purpose: To recognize and accept all AVID(Arithmetic Variable Identifier) and keywords. 
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Functions: isKeyword, strlen, strcpy, 
 * Parameters: char lexeme[] - an array(lexeme) of characters 
 * Return Value: currentToken - the token being processed
 * Algorithm: Check if the lexeme is a keyword, if yes set the token attribute, if no set the AVID token.
 *			  Check that lexeme is shorter than VID_LEN, if lexeme is longer only store the VID_LEN 
 *			  characters, add '\0' at the end position to make a c-type string. 
 ********************************************************************************************************/
Token funcAVID(char lexeme[]) {
	Token currentToken = { 0 };
	int keyword = isKeyword(lexeme);
	int i, length;

	/* Confirm lexeme is a keyword */
	if (keyword != -1) { /* yes, a keyword */
		currentToken.code = KW_T;
		currentToken.attribute.keywordIndex = keyword;
		return currentToken;
	}
	else { /* not a keyword, set AVID token */
		currentToken.code = AVID_T; 
	}

	length = strlen(lexeme);
	/* if lexeme if longer than VID_LEN, only keep the chars that are within same length of VID_LEN */
	if (length > VID_LEN) {
		for (i = 0; i < VID_LEN; i++) {
			currentToken.attribute.vidLexeme[i] = lexeme[i];
		}
		currentToken.attribute.vidLexeme[VID_LEN] = CHARSEOF0;
	}
	/* copy the correct length lexeme AND end each token with '\0' */
	else { 
		strcpy(currentToken.attribute.vidLexeme, lexeme);
		currentToken.attribute.vidLexeme[length] = CHARSEOF0;
	}
	return currentToken;
}

/*********************************************************************************************************
 * Function Name: funcSVID
 * Purpose: To recognize and accept all SVID(String Variable Identifier).
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Functions: strlen, strcpy
 * Parameters: char lexeme[] - an array(lexeme) of characters
 * Return Value: currentToken - the token being processed
 * Algorithm: Set the SVID token. Get the length of the lexeme. Check that the lexeme is shorter than 
 *			  VID_LEN - 1, if longer store only chars before VID_LEN - 1, add '\0' at the end position to 
 *			  make a c-type string. 
 ********************************************************************************************************/
Token funcSVID(char lexeme[]) {
	Token currentToken = { 0 };
	int i, length;

	/* Ensure the lexeme for SVID starts with a '$' */
	currentToken.code = SVID_T;
	length = strlen(lexeme);
	/* Length is longer than VID_LEN - 1, only keep the chars within the same length ** '-1' to leave space at end for required '$' */
	if (length > VID_LEN - 1) {
		for (i = 0; i < VID_LEN - 1; i++) {
			currentToken.attribute.vidLexeme[i] = lexeme[i];
		}
		currentToken.attribute.vidLexeme[VID_LEN - 1] = '$';
		currentToken.attribute.vidLexeme[VID_LEN] = CHARSEOF0;
	}
	/* copy the correct length lexeme AND end each token with '\0' */
	else {
		strcpy(currentToken.attribute.vidLexeme, lexeme);
		currentToken.attribute.vidLexeme[length] = CHARSEOF0;
	}
	return currentToken;
}

/***********************************************************************************************
 * Function Name: funcIL
 * Purpose: To recognize and accept all IL(Integer Literals). 
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Functions: atol, funcERR
 * Parameters: char lexeme[] - an array(lexeme) of characters
 * Return Value: currentToken - the token being processed
 * Algorithm: Check if lexeme is in the range of 2-byte integer. If longer or shater than range, 
 *			  send lexeme to funcERR, if length ok set code and attribute 
 ***********************************************************************************************/
Token funcIL(char lexeme[]) {
	Token currentToken = { 0 };
	long tempNum = atol(lexeme);

	/* If lexeme is over/under in size, take lexeme to funcErr() */
	if (tempNum > SHRT_MAX || tempNum < SHRT_MIN) {
		currentToken = funcErr(lexeme);
	}
	/* If lexeme confirmed, set token and value of tempNum into it (attribute.intValue) */
	else {
		currentToken.code = INL_T;
		currentToken.attribute.intValue = tempNum;
	}
	return currentToken;
}

/* TODO_214: Comment this function header */
/*********************************************************************************************
 * Function Name: funcFPL
 * Purpose: To recognize and accept all FPL (Floating Point Literals). 
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Functions: strtof, funcERR
 * Parameters: char lexeme[] - an array(lexeme) of characters
 * Return Value: currentToken - the token being processed
 * Algorithm: Check if lexeme is in the range of 4-byte float. If longer or shater than range, 
 *			  send lexeme to funcERR, if length ok set code and attribute
 *********************************************************************************************/
Token funcFPL(char lexeme[]) {
	Token currentToken = { 0 };
	double tempNum = strtof(lexeme, NULL); 

	/* If the lexeme is less than 0 or smaller or larger than the allowed size limt for FPL, set token(lexeme) to error aka funcErr(lexeme) */
	if (((tempNum >= 0 && strlen(lexeme) > 7) && (tempNum < FLT_MIN || tempNum > FLT_MAX)) || (tempNum < 0)) {
		currentToken = funcErr(lexeme);
	}
	/* If token is confirmed as a FPL, set token and value and cast value(tempNum) to float (attribute.floatValue) */
	else {
		currentToken.code = FPL_T;
		currentToken.attribute.floatValue = (float)tempNum;
	}
	return currentToken;
}

/****************************************************************************************************************
 * Function Name: funcSL
 * Purpose: To recognize and accept all SL (String Literals).
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Functions: strlen, bGetAddChOffset, bAddCh
 * Parameters: char lexeme[] - an array(lexeme) of characters
 * Return Value: currentToken - the token being processed
 * Algorithm: Get the length of the lexeme. Get the position of where the next character will be added in the 
 *			  buffer. Set the contentString attribute and token code. Add the lexeme into the string literal table 
 *  		  while ignoring single quotes and incrementing line when a newline is found/detected and add '\0' to 
 *			  the end of the string.
 ***************************************************************************************************************/
Token funcSL(char lexeme[]) {
	Token currentToken = { 0 };
	int i;
	int length = strlen(lexeme);
	short addPosition = bGetAddChOffset(stringLiteralTable);

	currentToken.attribute.contentString = addPosition;
	currentToken.code = STR_T;

	for (i = 0; i < length; i++) {
		if (lexeme[i] == '\'') { /*  ignore " ' "  */
			continue;
		}
		if (lexeme[i] == '\n') {
			line++;
		}
		stringLiteralTable = bAddCh(stringLiteralTable, lexeme[i]);
	}
	stringLiteralTable = bAddCh(stringLiteralTable, CHARSEOF0);
	return currentToken;
}

/****************************************************************************************************************
 * Function Name: funcERR
 * Purpose: To recognize and accept all error tokens.
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Functions: strlen, 
 * Parameters: char lexeme[] - an array(lexeme) of characters
 * Return Value: currentToken - the token being processed
 * Algorithm: Get the length of the lexeme. Check that the lexeme is longer that ERR_LEN, if longer then add the 
 *			  lexeme only up to ERR_LEN - 3(17) and add '...' to the end(totaling 20), if shorter then add full 
 *			  lexeme to errLexeme attribute, incrementing line if newline found. Finally set ERR token code.  
 ****************************************************************************************************************/
Token funcErr(char lexeme[]) {
	Token currentToken = { 0 };
	int length = strlen(lexeme);
	int i;
	/* If lexeme length is longer that ERR_LEN */
	if (length > ERR_LEN) {
		/* Take only only the lexemes that match ERR_LEN -3 (=17) in order to be able to add in the '...' at the end totalling 20 */
		for (i = 0; i < ERR_LEN - 3; i++) {
			currentToken.attribute.errLexeme[i] = lexeme[i];
			if (lexeme[i] == '\n') {
				line++;
			}
		}
		currentToken.attribute.errLexeme[ERR_LEN - 3] = '.';
		currentToken.attribute.errLexeme[ERR_LEN - 2] = '.';
		currentToken.attribute.errLexeme[ERR_LEN - 1] = '.';
 	}
	else {
		/* Lexeme is smaller than ERR_LEN so simply add it */
		for (i = 0; i < length; i++) {
			currentToken.attribute.errLexeme[i] = lexeme[i];
			if (lexeme[i] == '\n') {
				line++;
			}
		}
	}
	currentToken.code = ERR_T;
	return currentToken;
}

/* TODO_220: Comment this function header */
/*************************************************************
 * Function Name: isKeyword
 * Purpose: To check if the specific lexeme is a keyword from the keyword table
 * Author: Patrick Czermak
 * History/Versions: 1
 * Called Functions: strlen, strcmp
 * Parameters: char lexeme[] - an array(lexeme) of characters
 * Return Value: RT_FAIL_1 on failure, int j - the index of the keyword in the keyword table.
 * Algorithm: Get length of lexeme. Check if the lexeme is empty. Loop through the array of keywords to see if they 
 *			  match the lexeme, if match return the index otherwise return RT_FAIL_1.
 ************************************************************/
int isKeyword(char lexeme[]) {
	//int i = -1; /* PAULO -> I changed this to RT_FAIL_1 as it's a constant used in the buffer, so to stay consistent instead of using -1 value, made more sense to me */
	int j;
	int length = strlen(lexeme);

	/* If lexeme is empty/NULL */
	if ( length == NULL) {
		return RT_FAIL_1; 
	}
	/* Go through keyword array and compare/match the keywords in the lexeme */
	for (j = 0; j < KWT_SIZE; j++) {
		if (strcmp(keywordTable[j], lexeme) == 0) { 
			return j;
		}
	}
	return RT_FAIL_1; 
}

