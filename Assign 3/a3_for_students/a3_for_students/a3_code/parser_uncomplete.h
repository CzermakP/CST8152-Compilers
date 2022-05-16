/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
*************************************************************
* File name: parser.h
* Compiler: MS Visual Studio 2019
* Author: Svillen Ranev - Paulo Sousa - Abdulah
* Course: CST 8152 – Compilers, Lab Section: [011, 012, 013, or 014]
* Assignment: A3.
* Date: Sep 01 2020
* Purpose: This file is the main header for Parser (.h)
* Function list: (...).
*************************************************************/

/* TODO_101: Adjust the function header */

/* Inclusion section */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "token.h"
#include "buffer.h"

/* Global vars */
static Token lookahead;
int syntaxErrorNumber = 0;
extern bStructure* stringLiteralTable;
extern int line;
extern Token tokenizer();
extern char* keywordTable[];

/* Mock code: remove this UNKNOWN when variables defined */
#define UNKNOWN 0

/* TODO_102: Create ALL constants for keywords (sequence given in table.h) */
/* Constants */
#define	NO_ATTR	UNKNOWN
#define MAIN 	UNKNOWN
#define IF		UNKNOWN
#define THEN	UNKNOWN
#define	ELSE	UNKNOWN
#define WHILE	UNKNOWN
#define DO		UNKNOWN
#define READ	UNKNOWN
#define WRITE	UNKNOWN
#define TRUE	UNKNOWN
#define FALSE	UNKNOWN
// Continue the code

/* Function definitions */
void startParser(void);
void matchToken(int, int);
void syncErrorHandler(int);
void printError();
//void printMessage(char*);

/* TODO_103: Place ALL non-terminal function declarations */
void program(void);
void optionalStatements(void);
void statements(void);
void variableList(void);
// Continue the code
