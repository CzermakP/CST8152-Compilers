/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
*************************************************************
* File name: parser.h
* Compiler: MS Visual Studio 2019
* Author: Svillen Ranev - Paulo Sousa - Abdulah
* Course: CST 8152 – Compilers, Lab Section: [011, 012, 013, or 014]
* Assignment: A3.
* Date: Jan 01 2021
* Purpose: This file is the main header for Parser (.h)
* Function list: (...).
*************************************************************/
/* TODO_201: Adjust the function header */

#include "parser.h"

/*************************************************************
 * Process Parser
 ************************************************************/
void startParser(void) {
	lookahead = tokenizer();
	program();
	matchToken(SEOF_T, NO_ATTR);
	printf("%s\n", "PLATY: Source file parsed");
}

/*************************************************************
 * Match Token
 ************************************************************/
/* TODO_202: Continue the development */
void matchToken(int tokenCode, int tokenAttribute) {
	int matchFlag = 1;
	switch (lookahead.code) {
	case UNKNOWN:
		;
		// Continue the code
	// Continue the code (other cases)
	}
	if (matchFlag && lookahead.code == SEOF_T)
		;
		// Continue the code
	if (matchFlag) {
		;
		// Continue the code
	}
	else
		;
		// Continue the code 
}

/*************************************************************
 * Syncronize Error Handler
 ************************************************************/
/* TODO_203: Continue the development */
void syncErrorHandler(int syncTokenCode) {
	// Continue the code
	while (lookahead.code != syncTokenCode) {
		;
	// Continue the code
	}
	if (lookahead.code != SEOF_T)
		;
		// Continue the code
}


/*************************************************************
 * Print Error
 ************************************************************/
/* TODO_204: Continue the development */
void printError() {
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case UNKNOWN:
		;
		// Continue the code
	// Continue the code (other cases)
	}
}


/*************************************************************
 * Program statement
 * BNF: <program> -> PLATYPUS { <opt_statements> }
 * FIRST(<program>)= {KW_T (MAIN)}.
 ************************************************************/
void program(void) {
	matchToken(KW_T, MAIN);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	printf("%s\n", "PLATY: Program parsed");
}

/* TODO_205: Continue the development (all non-terminal functions) */

/* Example 1 */
/*************************************************************
 * Optional statement
 * TODO: Include the grammar definition here
 * TODO: Include the first set here
 ************************************************************/
void optionalStatements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		if (lookahead.attribute.codeType == IF
			|| lookahead.attribute.codeType == WHILE
			|| lookahead.attribute.codeType == READ
			|| lookahead.attribute.codeType == WRITE) {
			statements();
			break;
		}
	default:
		;
	}
	printf("%s\n", "PLATY: Optional statements parsed");
}

/* Example 2 */
/*************************************************************
 * Input Statement
 * TODO: Include the grammar definition here
 * TODO: Include the first set here
 ************************************************************/
void inputStatement(void) {
	matchToken(KW_T, READ);
	matchToken(LPR_T, NO_ATTR);
	variableList();
	matchToken(RPR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Input statement parsed");
}

/* TODO: Continue all functions */

/*************************************************************
 * Statements
 * TODO: Include the grammar definition here
 * TODO: Include the first set here
 ************************************************************/
void statements(void) {
	// TODO
	;
}

/*************************************************************
 * Variable List
 * TODO: Include the grammar definition here
 * TODO: Include the first set here
 ************************************************************/
void variableList(void) {
	// TODO
	;
}

// TODO: Continue the code...
